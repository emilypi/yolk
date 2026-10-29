{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module       : Yolk.Internal.Graph
-- Description  : Typed e-graph storage and deferred congruence rebuilding.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Deferred congruence rebuilding, following Willsey et al.,
-- "egg: Fast and Extensible Equality Saturation" (POPL 2021).
-- https://doi.org/10.1145/3434304
--
-- I keep intern identity and visible membership separate. A hidden node
-- remains available to congruence and hashconsing; pruning does not undo
-- an equality. This first storage implementation retains typed boxed nodes.
module Yolk.Internal.Graph
  ( ClassId
  , classIdRep
  , classIdIndex
  , EGraphT
  , Stored (..)
  , Snapshot
  , snapshotRoots
  , snapshotRows
  , snapshotFind
  , snapshotClasses
  , runEGraphT
  , runPersistent
  , runMutable
  , findClass
  , add
  , addAt
  , addTerm
  , lookupNode
  , union
  , equivalent
  , rebuild
  , classNodes
  , hideNonLeaves
  , nodeCount
  , graphRevision
  , freeze
  , snapshotNodes
  , throwGraph
  ) where

import Control.Monad (foldM, forM, unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify')
import Data.Functor.Identity (Identity, runIdentity)
import Data.HKD (FFoldable (..), FTraversable (..))
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Type.Reflection
  ( SomeTypeRep (..)
  , TypeRep
  , Typeable
  , eqTypeRep
  , typeRep
  , (:~~:) (HRefl)
  )
import Yolk.Backend
import Yolk.Internal.Types

-- | Graph-region and sort parameters are nominal: neither can be coerced.
type ClassId :: Type -> k -> Type
data ClassId r s = ClassId !(TypeRep s) !Int

type role ClassId nominal nominal

instance Eq (ClassId r s) where
  ClassId _ a == ClassId _ b = a == b

instance Ord (ClassId r s) where
  compare (ClassId _ a) (ClassId _ b) = compare a b

instance Show (ClassId r s) where
  showsPrec p (ClassId _ n) = showParen (p > 10) (showString "ClassId " . shows n)

classIdRep :: ClassId r s -> TypeRep s
classIdRep (ClassId rep _) = rep

classIdIndex :: ClassId r s -> Int
classIdIndex (ClassId _ index) = index

data Stored r l where
  Stored :: !(TypeRep s) -> !Int -> !(l s (ClassId r)) -> !Bool -> Stored r l

type NodeKey = (SomeTypeRep, String, [Int])

data Graph b r l m = Graph
  { graphUF :: !(UnionFind b m)
  , graphNodes :: !(IM.IntMap (Stored r l))
  , graphOriginals :: !(IM.IntMap (Stored r l))
  , graphMemo :: !(M.Map NodeKey Int)
  , graphDirty :: !Bool
  , graphVersion :: !Int
  }

-- | The graph region cannot escape a runner. Backend state stays inside the
-- chosen monad, including the state token for the mutable backend.
newtype EGraphT b r l m a = EGraphT
  {unEGraphT :: StateT (Graph b r l m) (ExceptT EGraphError m) a}
  deriving (Functor, Applicative, Monad)

type role EGraphT nominal nominal nominal nominal nominal

throwGraph :: Monad m => EGraphError -> EGraphT b r l m a
throwGraph = EGraphT . lift . throwE

liftGraph :: Monad m => m a -> EGraphT b r l m a
liftGraph = EGraphT . lift . lift

runEGraphT ::
  Backend b m =>
  Proxy b ->
  (forall r. EGraphT b r l m a) ->
  m (Either EGraphError a)
runEGraphT _ action = do
  uf <- ufEmpty
  runExceptT
    (evalStateT (unEGraphT action) (Graph uf IM.empty IM.empty M.empty False 0))

runPersistent ::
  (forall r. EGraphT Persistent r l Identity a) -> Either EGraphError a
runPersistent action = runIdentity (runEGraphT (Proxy :: Proxy Persistent) action)

runMutable ::
  (forall st r. EGraphT Mutable r l (ST st) a) -> Either EGraphError a
runMutable action = runST (runEGraphT (Proxy :: Proxy Mutable) action)

-- | Read-only find. This deliberately leaves the mutable parent array alone.
findClass :: Backend b m => ClassId r s -> EGraphT b r l m (ClassId r s)
findClass c@(ClassId rep i) = do
  validateId c
  uf <- EGraphT (gets graphUF)
  ClassId rep <$> liftGraph (ufFind uf i)
{-# INLINABLE findClass #-}

validateId :: Monad m => ClassId r s -> EGraphT b r l m ()
validateId (ClassId rep i) = do
  originals <- EGraphT (gets graphOriginals)
  case IM.lookup i originals of
    Nothing -> throwGraph (UnknownClass i)
    Just (Stored actual _ _ _) ->
      unless (SomeTypeRep rep == SomeTypeRep actual) (throwGraph (SortMismatch i))

requireClean :: Monad m => EGraphT b r l m ()
requireClean = do
  dirty <- EGraphT (gets graphDirty)
  when dirty (throwGraph DirtyGraph)

nodeKey :: Language l => TypeRep s -> l s (ClassId r) -> NodeKey
nodeKey rep n =
  (SomeTypeRep rep, operatorKey (erase n), ffoldMap (\(ClassId _ i) -> [i]) n)

add ::
  (Backend b m, Language l, Typeable s) =>
  l s (ClassId r) ->
  EGraphT b r l m (ClassId r s)
add = addAt typeRep
{-# INLINABLE add #-}

addAt ::
  (Backend b m, Language l) =>
  TypeRep s ->
  l s (ClassId r) ->
  EGraphT b r l m (ClassId r s)
addAt rep original = do
  dirty <- EGraphT (gets graphDirty)
  when dirty rebuild
  n <- ftraverse findClass original
  let key = nodeKey rep n
  g <- EGraphT get
  case M.lookup key (graphMemo g) of
    Just owner -> findClass (ClassId rep owner)
    Nothing -> do
      (owner, uf) <- liftGraph (ufFresh (graphUF g))
      let stored = Stored rep owner n True
      EGraphT $ modify' $ \state ->
        state
          { graphUF = uf
          , graphNodes = IM.insert owner stored (graphNodes state)
          , graphOriginals =
              IM.insert owner (Stored rep owner original True) (graphOriginals state)
          , graphMemo = M.insert key owner (graphMemo state)
          , graphVersion = graphVersion state + 1
          }
      pure (ClassId rep owner)

addTerm ::
  (Backend b m, Language l) => Term l s -> EGraphT b r l m (ClassId r s)
addTerm (Term n) = ftraverse addTerm n >>= add

lookupNode ::
  (Backend b m, Language l, Typeable s) =>
  l s (ClassId r) ->
  EGraphT b r l m (Maybe (ClassId r s))
lookupNode n = do
  requireClean
  canonical <- ftraverse findClass n
  found <- EGraphT (gets (M.lookup (nodeKey typeRep canonical) . graphMemo))
  traverse (findClass . ClassId typeRep) found

union :: Backend b m => ClassId r s -> ClassId r s -> EGraphT b r l m Bool
union a@(ClassId _ i) b@(ClassId _ j) = do
  validateId a
  validateId b
  unionRaw i j
{-# INLINABLE union #-}

unionRaw :: Backend b m => Int -> Int -> EGraphT b r l m Bool
unionRaw i j = do
  uf <- EGraphT (gets graphUF)
  (changed, uf') <- liftGraph (ufUnion uf i j)
  when changed $ EGraphT $ modify' $ \g ->
    g
      { graphUF = uf'
      , graphDirty = True
      , graphVersion = graphVersion g + 1
      }
  pure changed

equivalent :: Backend b m => ClassId r s -> ClassId r s -> EGraphT b r l m Bool
equivalent a b = (==) <$> findClass a <*> findClass b

-- | Restore congruence and canonical hashcons keys. Repeated whole-store
-- sweeps terminate because each nontrivial collision reduces the number
-- of classes. This is the correctness baseline for occurrence-queue work.
rebuild :: (Backend b m, Language l) => EGraphT b r l m ()
rebuild = do
  dirty <- EGraphT (gets graphDirty)
  when dirty sweep
 where
  sweep = do
    rows <- EGraphT (gets (IM.toAscList . graphNodes))
    canonical <- forM rows $ \(owner, Stored rep _ n visible) -> do
      n' <- ftraverse findClass n
      pure (owner, Stored rep owner n' visible)
    (nodes, memo, changed) <- foldM insertRow (IM.empty, M.empty, False) canonical
    EGraphT $ modify' $ \g -> g {graphNodes =
      nodes, graphMemo = memo, graphDirty = changed}
    when changed sweep
  insertRow (nodes, memo, changed) (owner, row@(Stored rep _ n visible)) =
    let key = nodeKey rep n
     in case M.lookup key memo of
          Nothing -> pure (IM.insert owner row nodes, M.insert key owner memo, changed)
          Just kept -> do
            united <- unionRaw owner kept
            let retain (Stored tr ident shape oldVisible) =
                  Stored tr ident shape (oldVisible || visible)
            pure (IM.adjust retain kept nodes, memo, changed || united)

classNodes :: Backend b m => ClassId r s -> EGraphT b r l m [l s (ClassId r)]
classNodes c = do
  requireClean
  root@(ClassId rep _) <- findClass c
  rows <- EGraphT (gets (IM.elems . graphNodes))
  fmap concat $ forM rows $ \(Stored actual owner n visible) ->
    case eqTypeRep rep actual of
      Just HRefl | visible -> do
        r <- findClass (ClassId actual owner)
        pure [n | r == root]
      _ -> pure []

-- | Hide compound members but keep their intern entries and equality
-- dependencies. Leaf nodes are retained. Re-add does not restore members.
hideNonLeaves :: (Backend b m, Language l) => ClassId r s -> EGraphT b r l m ()
hideNonLeaves c = do
  requireClean
  ClassId _ root <- findClass c
  g <- EGraphT get
  rows <- forM (IM.toAscList (graphNodes g)) $
    \(owner, row@(Stored rep ident n visible)) -> do
      current <- liftGraph (ufFind (graphUF g) owner)
      let hide = current == root && visible && not (null (ffoldMap (const [()]) n))
      pure (owner, if hide then Stored rep ident n False else row, hide)
  EGraphT $ modify' $ \state ->
    state
      { graphNodes = IM.fromAscList [(owner, row) | (owner, row, _) <- rows]
      , graphVersion = graphVersion state + if any (\(_, _, h) -> h) rows then 1 else 0
      }

nodeCount :: Monad m => EGraphT b r l m Int
nodeCount = EGraphT (gets (IM.size . graphNodes))

graphRevision :: Monad m => EGraphT b r l m Int
graphRevision = EGraphT (gets graphVersion)

-- | An immutable, region-scoped search view. It never references mutable
-- arrays, and remains valid when the live graph changes or prunes members.
data Snapshot r l = Snapshot !(IM.IntMap Int) !(IM.IntMap (Stored r l))

snapshotRoots :: Snapshot r l -> IM.IntMap Int
snapshotRoots (Snapshot roots _) = roots

snapshotRows :: Snapshot r l -> IM.IntMap (Stored r l)
snapshotRows (Snapshot _ rows) = rows
type role Snapshot nominal nominal

freeze :: (Backend b m, Language l) => EGraphT b r l m (Snapshot r l)
freeze = do
  rebuild
  g <- EGraphT get
  roots <- traverse (liftGraph . ufFind (graphUF g)) (IM.keys (graphOriginals g))
  pure
    ( Snapshot
        (IM.fromAscList (zip (IM.keys (graphOriginals g)) roots))
        (graphNodes g)
    )

snapshotFind :: ClassId r s -> Snapshot r l -> Maybe (ClassId r s)
snapshotFind (ClassId rep i) snap = ClassId rep <$> IM.lookup i (snapshotRoots snap)

snapshotNodes :: ClassId r s -> Snapshot r l -> [l s (ClassId r)]
snapshotNodes c@(ClassId rep _) snap = case snapshotFind c snap of
  Nothing -> []
  Just (ClassId _ root) -> mapMaybe (pick root) (IM.elems (snapshotRows snap))
 where
  pick root (Stored actual owner n visible) = case eqTypeRep rep actual of
    Just HRefl | visible && IM.lookup owner (snapshotRoots snap) == Just root -> Just n
    _ -> Nothing

snapshotClasses :: TypeRep s -> Snapshot r l -> [ClassId r s]
snapshotClasses rep snap =
  map (ClassId rep) . IS.toAscList $
    foldl' collect IS.empty (IM.elems (snapshotRows snap))
 where
  collect roots (Stored actual owner _ _) = case eqTypeRep rep actual of
    Just HRefl -> maybe roots (`IS.insert` roots) (IM.lookup owner (snapshotRoots snap))
    _ -> roots
