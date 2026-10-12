{-# LANGUAGE GADTs #-}

-- | Minimum AST-size tree extraction. The positive-cost relaxation follows
-- the fixed-point view of extraction in Willsey et al., POPL 2021
-- (https://doi.org/10.1145/3434304), related to Knuth's generalization of
-- Dijkstra's algorithm (https://doi.org/10.1016/0020-0190(77)90002-3).
-- Cycles without a finite visible derivation yield 'Nothing'. This does
-- not perform the separately planned optimal DAG extraction.
module Yolk.Extract (extract) where

import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import Data.Monoid (Sum (..))
import Type.Reflection (TypeRep, eqTypeRep, withTypeable, (:~~:) (HRefl))
import Yolk.Backend (Backend)
import Yolk.Internal.Graph
import Yolk.Internal.Types

data Candidate l where
  Candidate :: !(TypeRep s) -> !Integer -> !(Term l s) -> Candidate l

data Costed l s = Costed !Integer !(Term l s)

-- | Extract from a rebuilt immutable view. Each constructor costs one;
-- shared subterms are counted once per occurrence in the returned tree.
-- Cost arithmetic is unbounded internally; an unrepresentable result
-- reports 'CostOverflow' rather than wrapping an 'Int'.
extract :: (Backend b m, Language l) => ClassId r s -> EGraphT b r l m (Maybe (Int, Term l s))
extract requested = do
  root <- findClass requested
  snap <- freeze
  let candidates = solve snap IM.empty
  case fetch snap candidates root of
    Nothing -> pure Nothing
    Just (Costed cost tree)
      | cost > toInteger (maxBound :: Int) -> throwGraph CostOverflow
      | otherwise -> pure (Just (fromInteger cost, tree))

fetch :: Snapshot r l -> IM.IntMap (Candidate l) -> ClassId r s -> Maybe (Costed l s)
fetch snap table c@(ClassId rep _) = do
  ClassId _ root <- snapshotFind c snap
  Candidate actual cost tree <- IM.lookup root table
  case eqTypeRep rep actual of
    Just HRefl -> Just (Costed cost tree)
    Nothing -> Nothing

solve :: Language l => Snapshot r l -> IM.IntMap (Candidate l) -> IM.IntMap (Candidate l)
solve snap initial =
  let (next, changed) = foldl' improve (initial, False) (IM.elems (snapshotRows snap))
  in if changed then solve snap next else next
  where
    improve state@(_, _) (Stored _ _ _ False) = state
    improve state@(table, changed) (Stored rep owner n True) =
      case (IM.lookup owner (snapshotRoots snap), ftraverse (fetch snap table) n) of
        (Just root, Just children) ->
          let cost = 1 + getSum (ffoldMap (\(Costed c _) -> Sum c) children)
              better = case IM.lookup root table of
                Nothing -> True
                Just (Candidate _ old _) -> cost < old
          in if better
             then let tree = withTypeable rep (Term (ffmap (\(Costed _ t) -> t) children))
                  in (IM.insert root (Candidate rep cost tree) table, True)
             else (table, changed)
        _ -> state
