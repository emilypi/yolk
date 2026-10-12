{-# LANGUAGE RoleAnnotations #-}

-- | Solver-free optimal extraction of finite, shared DAGs. This is a separate
-- path from tree extraction: choose one visible member per demanded e-class,
-- close over its children, reject cycles, and charge each chosen member once.
-- Branch-and-bound is exact when it finishes, but can take exponential work.
-- A bounded search reports 'Unproven' unless its lower bound proves optimality.
module Yolk.Extract.Dag
  ( DagCost, dagSizeCost
  , DagLimits (..), defaultDagLimits, Optimality (..)
  , DagResult, dag, dagCost, lowerBound, optimality, expansions
  , Dag, dagNode, dagNodeId, dagSize, dagToTerm, foldDag, foldDagM
  , extractDagOptimal, extractDagMany
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Word (Word64)
import Type.Reflection (Typeable, TypeRep, eqTypeRep, withTypeable, (:~~:) (HRefl))
import Yolk.Backend (Backend)
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

-- | An immutable, well-sorted DAG. Shared children reference the same value.
-- It can escape the graph runner. Construction is private to keep IDs unique
-- and prevent cycles or mixing nodes from different extraction results.
data Dag l s where
  Dag :: !(TypeRep s) -> !Int -> !(l s (Dag l)) -> Dag l s
type role Dag nominal nominal

-- | Inspect one layer, preserving child sorts, order, and multiplicity.
dagNode :: Dag l s -> l s (Dag l)
dagNode (Dag _ _ n) = n

-- | Dense postorder ID, local to a single extraction (all roots of
-- 'extractDagMany' share that numbering). Children have smaller IDs.
-- IDs from separate calls must not be compared as identities.
dagNodeId :: Dag l s -> Int
dagNodeId (Dag _ i _) = i

data SomeDag l where
  SomeDag :: !(TypeRep s) -> !(Dag l s) -> SomeDag l

data Folded f where
  Folded :: !(TypeRep s) -> !(f s) -> Folded f

-- | Run the algebra once per reachable node, children before parents.
-- Repeated child positions receive the memoized result. Separate calls have
-- separate memo tables; folding two roots separately repeats their overlap.
foldDagM :: forall l m f s. (Monad m, Language l)
         => (forall t. Typeable t => l t f -> m (f t)) -> Dag l s -> m (f s)
foldDagM algebra root = evalStateT (visit root) IM.empty
  where
    visit :: forall t. Dag l t -> StateT (IM.IntMap (Folded f)) m (f t)
    visit (Dag rep ident n) = do
      memo <- get
      case IM.lookup ident memo of
        Just (Folded actual value) | Just HRefl <- eqTypeRep rep actual -> pure value
        _ -> do
          children <- ftraverse visit n
          value <- lift (withTypeable rep (algebra children))
          modify' (IM.insert ident (Folded rep value))
          pure value

foldDag :: Language l => (forall t. Typeable t => l t f -> f t) -> Dag l s -> f s
foldDag algebra = runIdentity . foldDagM (Identity . algebra)

-- | Convert to the tree API. Shared values are retained in memory, but tree
-- consumers count occurrences; use 'foldDagM' to emit shared code only once.
dagToTerm :: Language l => Dag l s -> Term l s
dagToTerm = foldDag Term

-- | Number of distinct reachable nodes, including leaves.
dagSize :: Language l => Dag l s -> Int
dagSize root = IS.size (visit IS.empty root)
  where
    visit :: Language l => IS.IntSet -> Dag l t -> IS.IntSet
    visit seen (Dag _ ident n)
      | IS.member ident seen = seen
      | otherwise = foldl' visitSome (IS.insert ident seen)
          (ffoldMap (\child@(Dag rep _ _) -> [SomeDag rep child]) n)
    visitSome seen (SomeDag _ child) = visit seen child

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
      childSet (ClassId _ i) = IS.singleton i
  case Search.solve (expansionLimit limits) problem (ffoldMap childSet roots) of
    Nothing -> pure Nothing
    Just solution -> case evalStateT
        (ftraverse (buildDag snap (Search.selection solution)) roots) IM.empty of
      Nothing -> throwGraph InvalidExtraction
      Just result -> pure (Just (DagResult result (Search.cost solution) (Search.bound solution)
        (if Search.proven solution then Optimal else Unproven) (Search.expanded solution)))

-- Each selected class is built once. The search has already proved this
-- selection acyclic; postorder memo insertion assigns dense topological IDs.
buildDag :: Language l => Snapshot r l -> IM.IntMap Int -> ClassId r s
         -> StateT (IM.IntMap (SomeDag l)) Maybe (Dag l s)
buildDag snap selected (ClassId rep owner) = do
  memo <- get
  case IM.lookup owner memo of
    Just (SomeDag actual value) -> case eqTypeRep rep actual of
      Just HRefl -> pure value
      Nothing -> lift Nothing
    Nothing -> do
      ident <- lift (IM.lookup owner selected)
      Stored actual _ n _ <- lift (IM.lookup ident (snapshotRows snap))
      case eqTypeRep rep actual of
        Nothing -> lift Nothing
        Just HRefl -> do
          children <- ftraverse (buildDag snap selected) n
          next <- IM.size <$> get
          let result = Dag rep next children
          modify' (IM.insert owner (SomeDag rep result))
          pure result
