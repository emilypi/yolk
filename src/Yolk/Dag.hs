{-# LANGUAGE RoleAnnotations #-}

-- |
-- Module       : Yolk.Dag
-- Description  : Immutable sorted DAGs, checked reconstruction and shared folds.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- DAG identities are local to a forest. Construction checks class membership,
-- sorts and acyclicity; a forest fold shares one memo table across every root.
module Yolk.Dag
  ( Dag, dagNode, dagNodeId, dagSize, dagToTerm
  , foldDag, foldDagM, foldDagMany, foldDagManyM
  , dagFromSelection
  ) where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Functor.Identity (Identity (..))
import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Type.Reflection (Typeable, TypeRep, SomeTypeRep (..), eqTypeRep, withTypeable, (:~~:) (HRefl))
import Yolk.Internal.Graph
import Yolk.Internal.Types

-- | A finite acyclic sorted DAG. Construction is checked; IDs and layers
-- cannot be replaced independently. All roots built together share IDs.
type ForestKey = IM.IntMap (SomeTypeRep, String, [Int])

data Dag l s where
  Dag :: !(TypeRep s) -> !Int -> !Int -> !ForestKey -> !(l s (Dag l)) -> Dag l s
type role Dag nominal nominal

dagNode :: Dag l s -> l s (Dag l)
dagNode (Dag _ _ _ _ n) = n

-- | Dense postorder identity local to one reconstructed forest. IDs from
-- different calls to an extractor or 'dagFromSelection' are unrelated.
dagNodeId :: Dag l s -> Int
dagNodeId (Dag _ ident _ _ _) = ident

data SomeDag l where
  SomeDag :: !(TypeRep s) -> !(Dag l s) -> SomeDag l

data Folded f where
  Folded :: !(TypeRep s) -> !(f s) -> Folded f

newtype One s f = One { getOne :: f s }
instance FFunctor (One s) where ffmap f (One a) = One (f a)
instance FFoldable (One s) where ffoldMap f (One a) = f a
instance FTraversable (One s) where ftraverse f (One a) = One <$> f a

-- | Fold each reachable node once, children before parents. Separate calls
-- have separate memo tables. Use 'foldDagManyM' for a jointly extracted forest.
foldDagM :: (Monad m, Language l)
  => (forall t. Typeable t => l t f -> m (f t)) -> Dag l s -> m (f s)
foldDagM algebra root = getOne <$> foldDagManyM algebra (One root)

foldDag :: Language l => (forall t. Typeable t => l t f -> f t) -> Dag l s -> f s
foldDag algebra = runIdentity . foldDagM (Identity . algebra)

-- | Fold a forest with one memo per exact structural blueprint. Roots may
-- come from different extraction calls: unrelated local IDs cannot collide.
-- Structurally identical forests may share effects; their identity includes
-- all operator payloads, result sorts and originating child identities.
foldDagManyM :: forall l m roots f. (Monad m, Language l, FTraversable roots)
  => (forall t. Typeable t => l t f -> m (f t)) -> roots (Dag l) -> m (roots f)
foldDagManyM algebra roots = evalStateT (ftraverse start roots) (M.empty, IM.empty)
  where
    start :: forall t. Dag l t
      -> StateT (M.Map ForestKey Int, IM.IntMap (IM.IntMap (Folded f))) m (f t)
    start root@(Dag _ _ _ key _) = do
      (groups, tables) <- get
      group <- case M.lookup key groups of
        Just existing -> pure existing
        Nothing -> do
          let fresh = M.size groups
          modify' (const (M.insert key fresh groups, tables))
          pure fresh
      visit group root

    visit :: forall t. Int -> Dag l t
      -> StateT (M.Map ForestKey Int, IM.IntMap (IM.IntMap (Folded f))) m (f t)
    visit group (Dag rep _ origin _ n) = do
      (_, tables) <- get
      case IM.lookup group tables >>= IM.lookup origin of
        Just (Folded actual value) | Just HRefl <- eqTypeRep rep actual -> pure value
        _ -> do
          children <- ftraverse (visit group) n
          value <- lift (withTypeable rep (algebra children))
          modify' (\(groups, done) -> (groups,
            IM.alter (Just . IM.insert origin (Folded rep value) . maybe IM.empty id) group done))
          pure value

foldDagMany :: (Language l, FTraversable roots)
  => (forall t. Typeable t => l t f -> f t) -> roots (Dag l) -> roots f
foldDagMany algebra = runIdentity . foldDagManyM (Identity . algebra)

dagToTerm :: Language l => Dag l s -> Term l s
dagToTerm = foldDag Term

dagSize :: Language l => Dag l s -> Int
dagSize root = IS.size (visit IS.empty root)
  where
    visit :: Language l => IS.IntSet -> Dag l t -> IS.IntSet
    visit seen (Dag _ ident _ _ n)
      | IS.member ident seen = seen
      | otherwise = foldl' visitSome (IS.insert ident seen)
          (ffoldMap (\child@(Dag rep _ _ _ _) -> [SomeDag rep child]) n)
    visitSome seen (SomeDag _ child) = visit seen child

-- | Reconstruct selected original node IDs, keyed by canonical class IDs,
-- from a snapshot. Reject missing/hidden members, wrong classes/sorts and
-- cycles. Unreachable selection entries are ignored. Unknown root IDs fail.
-- This checks representation validity, not semantic truth or optimality.
dagFromSelection :: forall l roots r. (Language l, FTraversable roots)
  => Snapshot r l -> IM.IntMap Int -> roots (ClassId r) -> Maybe (roots (Dag l))
dagFromSelection snap selected roots = evalStateT (ftraverse build roots) (IM.empty, IS.empty)
  where
    forestKey = IM.mapMaybe (\ident -> case IM.lookup ident (snapshotRows snap) of
      Nothing -> Nothing
      Just (Stored rep _ n _) -> Just (SomeTypeRep rep, operatorKey (erase n),
        ffoldMap (\child -> [classIdIndex child]) n)) selected

    build :: forall s. ClassId r s
      -> StateT (IM.IntMap (SomeDag l), IS.IntSet) Maybe (Dag l s)
    build requested = do
      canonical <- lift (snapshotFind requested snap)
      let rep = classIdRep canonical
          owner = classIdIndex canonical
      (memo, active) <- get
      case IM.lookup owner memo of
        Just (SomeDag actual value) -> case eqTypeRep rep actual of
          Just HRefl -> pure value
          Nothing -> lift Nothing
        Nothing -> do
          lift (guard (not (IS.member owner active)))
          ident <- lift (IM.lookup owner selected)
          Stored actual original n visible <- lift (IM.lookup ident (snapshotRows snap))
          lift (guard (visible && IM.lookup original (snapshotRoots snap) == Just owner))
          case eqTypeRep rep actual of
            Nothing -> lift Nothing
            Just HRefl -> do
              modify' (\(m, a) -> (m, IS.insert owner a))
              children <- ftraverse build n
              (completed, visiting) <- get
              let result = Dag rep (IM.size completed) owner forestKey children
              modify' (const (IM.insert owner (SomeDag rep result) completed, IS.delete owner visiting))
              pure result
