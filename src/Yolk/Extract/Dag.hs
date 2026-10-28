{-# LANGUAGE RoleAnnotations #-}

-- |
-- Module       : Yolk.Extract.Dag
-- Description  : Solver-free optimal DAG extraction with additive costs.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Solver-free optimal extraction of finite, shared DAGs. This is a separate
-- path from tree extraction: choose one visible member per demanded e-class,
-- close over its children, reject cycles, and charge each chosen member once.
-- Branch-and-bound is exact when it finishes, but can take exponential work.
-- A bounded search reports 'Unproven' unless its lower bound proves optimality.
module Yolk.Extract.Dag
  ( DagCost, dagSizeCost
  , DagLimits (..), defaultDagLimits, Optimality (..)
  , DagResult, dag, dagCost, lowerBound, optimality, expansions
  , Dag, dagNode, dagNodeId, dagSize, dagToTerm, foldDag, foldDagM, foldDagMany, foldDagManyM
  , extractDagOptimal, extractDagMany
  ) where

import Data.Functor.Const (Const (..))
import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Word (Word64)
import Yolk.Backend (Backend)
import Yolk.Dag
import qualified Yolk.Internal.Dag as Search
import Yolk.Internal.Graph
import Yolk.Internal.Types

-- | A nonnegative constructor/payload weight. Children are erased: each
-- selected node is charged exactly once, regardless of incoming edge count.
-- Sums saturate at @maxBound :: Word64@; larger mathematical costs tie there.
-- This additive objective differs from the general tree-cost algebra.
type DagCost l = forall s. l s (Const ()) -> Word64

-- | Minimize the number of distinct selected nodes, including leaves.
dagSizeCost :: DagCost l
dagSizeCost _ = 1

-- | A deterministic search-work cap. 'Nothing' explores until optimality is
-- proved. 'Just' zero still constructs a finite incumbent and checks the
-- initial lower bound. This is not a time or memory limit: preprocessing and
-- generating the children of one search state are outside the expansion cap.
data DagLimits = DagLimits { expansionLimit :: !(Maybe Word64) }
  deriving (Eq, Show)

-- | At most 100,000 branch expansions. This is a work cap,
-- not a performance guarantee; use @DagLimits Nothing@ for exhaustive search.
defaultDagLimits :: DagLimits
defaultDagLimits = DagLimits (Just 100000)

data Optimality = Optimal | Unproven
  deriving (Eq, Show)

-- | A feasible result with a cost certificate in the saturated Word64 domain:
-- @lowerBound <= optimum <= dagCost@. 'Optimal' means both bounds coincide.
-- Constructors are private so edits cannot invalidate the certificate.
data DagResult a = DagResult
  !a {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 !Optimality {-# UNPACK #-} !Word64

dag :: DagResult a -> a
dag (DagResult value _ _ _ _) = value

dagCost :: DagResult a -> Word64
dagCost (DagResult _ value _ _ _) = value

lowerBound :: DagResult a -> Word64
lowerBound (DagResult _ _ value _ _) = value

optimality :: DagResult a -> Optimality
optimality (DagResult _ _ _ value _) = value

expansions :: DagResult a -> Word64
expansions (DagResult _ _ _ _ value) = value

newtype One s f = One { getOne :: f s }

instance FFunctor (One s) where
  ffmap f (One x) = One (f x)
instance FFoldable (One s) where
  ffoldMap f (One x) = f x
instance FTraversable (One s) where
  ftraverse f (One x) = One <$> f x

-- | Optimize one root. 'Nothing' means there is no finite visible derivation,
-- not that the budget ran out. Even a zero budget returns a feasible incumbent
-- when one exists. Equal-cost DAGs have no promised secondary ordering.
extractDagOptimal :: (Backend b m, Language l)
                  => DagLimits -> DagCost l -> ClassId r s
                  -> EGraphT b r l m (Maybe (DagResult (Dag l s)))
extractDagOptimal limits weight root = fmap unwrap <$>
  extractDagMany limits weight (One root)
  where
    unwrap (DagResult value cost bound status count) =
      DagResult (getOne value) cost bound status count

-- | Jointly optimize an HKD collection of roots, possibly at different sorts.
-- Shared nodes across roots are charged once. Duplicate roots preserve their
-- positions; an empty collection yields an optimal empty DAG of cost zero.
-- A single representative is chosen for each reachable canonical e-class.
-- Uses a rebuilt immutable snapshot with both graph backends.
extractDagMany :: (Backend b m, Language l, FTraversable roots)
               => DagLimits -> DagCost l -> roots (ClassId r)
               -> EGraphT b r l m (Maybe (DagResult (roots (Dag l))))
extractDagMany limits weight requested = do
  snap <- freeze
  roots <- ftraverse findClass requested
  let problem = IM.fromListWith (++)
        [ (owner, [Search.Node ident (ffoldMap childSet n) (weight (erase n))])
        | (ident, Stored _ original n True) <- IM.toAscList (snapshotRows snap)
        , Just owner <- [IM.lookup original (snapshotRoots snap)]
        ]
      childSet = IS.singleton . classIdIndex
  case Search.solve (expansionLimit limits) problem (ffoldMap childSet roots) of
    Nothing -> pure Nothing
    Just solution -> case dagFromSelection snap (Search.selection solution) roots of
      Nothing -> throwGraph InvalidExtraction
      Just result -> pure (Just (DagResult result (Search.cost solution) (Search.bound solution)
        (if Search.proven solution then Optimal else Unproven) (Search.expanded solution)))
