{-# LANGUAGE GADTs #-}

-- | Configurable minimum-cost tree extraction. The relaxation follows
-- the fixed-point view of extraction in Willsey et al., POPL 2021
-- (https://doi.org/10.1145/3434304), related to Knuth's generalization of
-- Dijkstra's algorithm (https://doi.org/10.1016/0020-0190(77)90002-3).
-- Cycles without a finite visible derivation yield 'Nothing'. This does
-- not perform DAG extraction; see "Yolk.Extract.Dag" for that separate path.
module Yolk.Extract
  ( TreeCost, extract, extractWith
  , astSize, astDepth, weightedSize, saturatingAdd
  ) where

import Data.Functor.Const (Const (..))
import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import Data.Semigroup (Max (..))
import Data.Word (Word64)
import Type.Reflection (TypeRep, eqTypeRep, withTypeable, (:~~:) (HRefl))
import Yolk.Backend (Backend)
import Yolk.Internal.Graph
import Yolk.Internal.Types

-- | A node algebra receiving the cost of each child in its original position.
-- The constructor and its payload remain available; child sorts are preserved
-- by the language even though their costs have a common type.
--
-- Laws required by 'extractWith':
--
-- * @Ord c@ must be a lawful total order.
-- * Decreasing any child cost must not increase the node's cost (monotonicity).
-- * A node's cost must be at least every child's cost (superiority).
-- * Reachable costs must admit no infinite strictly descending chain.
--
-- These are caller obligations, not properties that the library can check for
-- an arbitrary function. Violating them can invalidate optimality or prevent
-- termination. Nonnegative additive weights, depth, and lexicographically
-- ordered exact nonnegative additive tuples satisfy these laws. For bounded
-- lexicographic tuples, saturation of an earlier component must also absorb
-- the later components; independent wrapping or saturation can break ordering.
-- Equal parent/child
-- costs are allowed: zero-cost cycles never replace an equal-cost finite tree.
type TreeCost l c = forall s. l s (Const c) -> c

data Candidate l c where
  Candidate :: !(TypeRep s) -> !c -> !(Term l s) -> Candidate l c

data Costed l c s = Costed !c !(Term l s)

-- | Unsigned addition clamped at @maxBound@. An overflowing cost must never
-- wrap around and become cheaper. Saturation deliberately identifies all
-- values above the representable range; use an unbounded custom algebra if
-- their relative order matters.
saturatingAdd :: Word64 -> Word64 -> Word64
saturatingAdd a b
  | b > maxBound - a = maxBound
  | otherwise = a + b
{-# INLINE saturatingAdd #-}

newtype Total = Total { getTotal :: Word64 }

instance Semigroup Total where
  Total a <> Total b = Total (saturatingAdd a b)

instance Monoid Total where
  mempty = Total 0

-- | Sum a nonnegative constructor/payload weight and every child occurrence.
-- Arithmetic saturates at @maxBound :: Word64@. The weight function receives
-- the node with its children erased, so it cannot depend on their costs.
weightedSize :: Language l => (forall s. l s (Const ()) -> Word64) -> TreeCost l Word64
weightedSize weight n =
  saturatingAdd (weight (erase n)) (getTotal (ffoldMap (Total . getConst) n))

-- | One per constructor, including leaves; repeated children count repeatedly.
-- Arithmetic saturates at @maxBound :: Word64@.
astSize :: Language l => TreeCost l Word64
astSize = weightedSize (const 1)

-- | Leaf depth is one; other nodes cost one plus their deepest child.
-- Arithmetic saturates at @maxBound :: Word64@.
astDepth :: Language l => TreeCost l Word64
astDepth n = saturatingAdd 1 (getMax (ffoldMap (Max . getConst) n))

-- | Extract from a rebuilt immutable view. Each constructor costs one;
-- shared subterms are counted once per occurrence in the returned tree.
-- This compatibility wrapper uses 'astSize' and reports 'CostOverflow' when
-- the result exceeds @maxBound :: Int@. Use @extractWith astSize@ to retain
-- the full 'Word64' range.
extract :: (Backend b m, Language l) => ClassId r s -> EGraphT b r l m (Maybe (Int, Term l s))
extract requested = do
  result <- extractWith astSize requested
  case result of
    Nothing -> pure Nothing
    Just (cost, tree)
      | cost > fromIntegral (maxBound :: Int) -> throwGraph CostOverflow
      | otherwise -> pure (Just (fromIntegral cost, tree))

-- | Find the minimum finite visible tree under the supplied lawful algebra.
-- Rebuilds before reading an immutable snapshot, works with either backend,
-- and returns 'Nothing' when no finite visible derivation exists. Costs are
-- recomputed for each call, so different objectives can query the same graph.
--
-- The returned cost is exact in the model's ordered domain, including any
-- saturation defined by that model. Equal costs keep the first finite witness
-- encountered; no secondary objective or insertion-order independence of
-- tied /terms/ is promised. Sharing is counted by occurrence, not by DAG node.
extractWith :: (Backend b m, Language l, Ord c)
            => TreeCost l c -> ClassId r s -> EGraphT b r l m (Maybe (c, Term l s))
extractWith model requested = do
  root <- findClass requested
  snap <- freeze
  let candidates = solve model snap IM.empty
  case fetch snap candidates root of
    Nothing -> pure Nothing
    Just (Costed cost tree) -> pure (Just (cost, tree))

fetch :: Snapshot r l -> IM.IntMap (Candidate l c) -> ClassId r s -> Maybe (Costed l c s)
fetch snap table c@(ClassId rep _) = do
  ClassId _ root <- snapshotFind c snap
  Candidate actual cost tree <- IM.lookup root table
  case eqTypeRep rep actual of
    Just HRefl -> Just (Costed cost tree)
    Nothing -> Nothing

solve :: (Language l, Ord c)
      => TreeCost l c -> Snapshot r l -> IM.IntMap (Candidate l c) -> IM.IntMap (Candidate l c)
solve model snap initial =
  let (next, changed) = foldl' improve (initial, False) (IM.elems (snapshotRows snap))
  in if changed then solve model snap next else next
  where
    improve state@(_, _) (Stored _ _ _ False) = state
    improve state@(table, changed) (Stored rep owner n True) =
      case (IM.lookup owner (snapshotRoots snap), ftraverse (fetch snap table) n) of
        (Just root, Just children) ->
          let cost = model (ffmap (\(Costed c _) -> Const c) children)
              better = case IM.lookup root table of
                Nothing -> True
                Just (Candidate _ old _) -> cost < old
          in if better
             then let tree = withTypeable rep (Term (ffmap (\(Costed _ t) -> t) children))
                  in (IM.insert root (Candidate rep cost tree) table, True)
             else (table, changed)
        _ -> state
