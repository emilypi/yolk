{-# LANGUAGE BangPatterns #-}

-- |
-- Module       : Yolk.Internal.Pareto
-- Description  : Exact additive-vector search over shared class selections.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- A solver-free extension of Yolk's scalar branch-and-bound search. A partial
-- selection charges each selected class once. Componentwise minimum weights
-- for demanded classes bound every completion; descendants only add cost.
-- Strict dominance pruning retains equal-vector witnesses until their cap.
module Yolk.Internal.Pareto
  ( Node (..), Entry (..), Result (..), Termination (..), solve
  , nodeIdL, nodeChildrenL, nodeCostL, entrySelectionL, entryCostL
  , resultEntriesL, resultTerminationL, resultExpandedL, resultBoundsL
  , resultWitnessesCompleteL, resultVectorsCompleteL
  , _Exhausted, _WorkLimit, _ArchiveLimit
  , _Partial
  ) where

import Control.Lens (Iso', Lens', Prism', iso, lens, prism')
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl1', minimumBy, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import qualified Data.Vector as V

-- Boxed vectors retain arbitrary-precision Natural values while repeatedly
-- traversed cost coordinates use contiguous slots instead of list spines.
type Vector = V.Vector Natural
type Selection = IM.IntMap Node
type Problem = IM.IntMap [Node]

data Node = Node
  { nodeId :: !Int
  , nodeChildren :: !IS.IntSet
  , nodeCost :: !Vector
  } deriving (Eq, Show)

data Entry = Entry
  { entrySelection :: !(IM.IntMap Int)
  , entryCost :: !Vector
  } deriving (Eq, Show)

data Termination = Exhausted | WorkLimit | ArchiveLimit deriving (Eq, Show)

data Result = Result
  { resultEntries :: ![Entry]
  , resultTermination :: !Termination
  , resultExpanded :: !Word64
  , resultBounds :: ![Vector]
  , resultWitnessesComplete :: !Bool
  , resultVectorsComplete :: !Bool
  } deriving (Eq, Show)

data Partial = Partial !Selection !IS.IntSet !Vector

-- | The search-state product. This is an internal solver state, not a
-- checked typed DAG or an extraction evidence record.
_Partial :: Iso' Partial (Selection, IS.IntSet, Vector)
_Partial = iso (\(Partial selected needed spent) -> (selected, needed, spent))
  (\(selected, needed, spent) -> Partial selected needed spent)

nodeIdL :: Lens' Node Int
nodeIdL = lens nodeId (\s a -> s {nodeId = a})

nodeChildrenL :: Lens' Node IS.IntSet
nodeChildrenL = lens nodeChildren (\s a -> s {nodeChildren = a})

nodeCostL :: Lens' Node Vector
nodeCostL = lens nodeCost (\s a -> s {nodeCost = a})

entrySelectionL :: Lens' Entry (IM.IntMap Int)
entrySelectionL = lens entrySelection (\s a -> s {entrySelection = a})

entryCostL :: Lens' Entry Vector
entryCostL = lens entryCost (\s a -> s {entryCost = a})

resultEntriesL :: Lens' Result [Entry]
resultEntriesL = lens resultEntries (\s a -> s {resultEntries = a})

resultTerminationL :: Lens' Result Termination
resultTerminationL = lens resultTermination (\s a -> s {resultTermination = a})

resultExpandedL :: Lens' Result Word64
resultExpandedL = lens resultExpanded (\s a -> s {resultExpanded = a})

resultBoundsL :: Lens' Result [Vector]
resultBoundsL = lens resultBounds (\s a -> s {resultBounds = a})

resultWitnessesCompleteL :: Lens' Result Bool
resultWitnessesCompleteL = lens resultWitnessesComplete (\s a -> s {resultWitnessesComplete = a})

resultVectorsCompleteL :: Lens' Result Bool
resultVectorsCompleteL = lens resultVectorsComplete (\s a -> s {resultVectorsComplete = a})

_Exhausted, _WorkLimit, _ArchiveLimit :: Prism' Termination ()
_Exhausted = prism' (const Exhausted) $ \case Exhausted -> Just (); _ -> Nothing
_WorkLimit = prism' (const WorkLimit) $ \case WorkLimit -> Just (); _ -> Nothing
_ArchiveLimit = prism' (const ArchiveLimit) $ \case ArchiveLimit -> Just (); _ -> Nothing

plus :: Vector -> Vector -> Vector
plus = V.zipWith (+)

weaklyDominates :: Vector -> Vector -> Bool
weaklyDominates a b = V.and (V.zipWith (<=) a b)

strictlyDominates :: Vector -> Vector -> Bool
strictlyDominates a b = a /= b && weaklyDominates a b

seed :: Int -> Problem -> Selection
seed dimensions problem = IM.map fst (loop IM.empty)
  where
    loop known =
      let next = IM.foldlWithKey' discover known problem
      in if IM.size next == IM.size known then next else loop next
    discover known owner nodes
      | IM.member owner known = known
      | otherwise = case mapMaybe (available known) nodes of
          [] -> known
          choices -> IM.insert owner (minimumBy (comparing rank) choices) known
    rank (n, estimate) = (sum estimate, nodeId n)
    available known n = do
      children <- traverse (`IM.lookup` known) (IS.toList (nodeChildren n))
      pure (n, foldl' plus (V.replicate dimensions 0) (nodeCost n : map snd children))

closure :: Selection -> IS.IntSet -> Selection
closure table roots = go IM.empty (IS.toList roots)
  where
    go !seen [] = seen
    go !seen (owner : rest)
      | IM.member owner seen = go seen rest
      | otherwise = case IM.lookup owner table of
          Nothing -> go seen rest
          Just n -> go (IM.insert owner n seen) (IS.toList (nodeChildren n) ++ rest)

reachable :: Problem -> IS.IntSet -> IS.IntSet
reachable problem roots = go IS.empty (IS.toList roots)
  where
    go !seen [] = seen
    go !seen (owner : rest)
      | IS.member owner seen = go seen rest
      | otherwise = go (IS.insert owner seen)
          (IS.toList (IS.unions (map nodeChildren (IM.findWithDefault [] owner problem))) ++ rest)

closesCycle :: Selection -> Int -> Node -> Bool
closesCycle chosen owner n = go IS.empty (IS.toList (nodeChildren n))
  where
    go _ [] = False
    go seen (child : rest)
      | child == owner = True
      | IS.member child seen = go seen rest
      | otherwise = go (IS.insert child seen)
          (maybe [] (IS.toList . nodeChildren) (IM.lookup child chosen) ++ rest)

-- | Preconditions: every cost/bound has exactly the positive dimension,
-- all node IDs are unique, children use problem class IDs, and limits are
-- checked by the public adapter. No physical objective is inferred here.
solve :: Int -> Maybe Vector -> Maybe Word64 -> Int -> Int
  -> Problem -> IS.IntSet -> Result
solve dimensions bounds budget archiveLimit witnessLimit original roots
  | not (roots `IS.isSubsetOf` productive) = Result [] Exhausted 0 [] True True
  | otherwise = search 0 initial True [Partial IM.empty roots zero]
  where
    zero = V.replicate dimensions 0
    feasible cost = maybe True (weaklyDominates cost) bounds
    noSelf = IM.mapWithKey (\owner -> filter (not . IS.member owner . nodeChildren)) original
    witnesses = seed dimensions noSelf
    productive = IM.keysSet witnesses
    live = IM.map (filter ((`IS.isSubsetOf` productive) . nodeChildren))
      (IM.restrictKeys noSelf productive)
    problem = IM.restrictKeys live (reachable live roots)
    minima = IM.map (foldl1' (V.zipWith min) . map nodeCost) problem
    chosen = closure witnesses roots
    first = Entry (IM.map nodeId chosen) (foldl' plus zero (map nodeCost (IM.elems chosen)))
    initial = [first | feasible (entryCost first)]

    lower (Partial _ needed spent) = foldl' plus spent
      [IM.findWithDefault zero c minima | c <- IS.toList needed]
    ruledOut archive state =
      let bound = lower state
      in not (feasible bound) || any ((`strictlyDominates` bound) . entryCost) archive

    -- One componentwise envelope bounds every unexplored completion. This
    -- deliberately avoids an unbounded result list of pending search states.
    pendingBounds [] = []
    pendingBounds states = [foldl1' (V.zipWith min) (map lower states)]

    finish n stop complete archive pending = Result
      (sortOn (\e -> (entryCost e, IM.toAscList (entrySelection e))) archive)
      stop n (pendingBounds pending) (complete && stop == Exhausted)
      (stop /= ArchiveLimit && all (\state -> not (feasible (lower state))
        || any ((`weaklyDominates` lower state) . entryCost) archive) pending)

    search !count archive witnessesComplete pending =
      case dropWhile (ruledOut archive) pending of
        [] -> finish count Exhausted witnessesComplete archive []
        state@(Partial selection needed spent) : rest
          | IS.null needed ->
              let candidate = Entry (IM.map nodeId selection) spent
                  (next, droppedWitness, overfull) = insert candidate archive
              in if overfull
                 then finish count ArchiveLimit False (takeArchive next) (state : rest)
                 else search count next (witnessesComplete && not droppedWitness) rest
          | maybe False (count >=) budget -> finish count WorkLimit False archive (state : rest)
          | count == maxBound -> finish count WorkLimit False archive (state : rest)
          | otherwise -> search (count + 1) archive witnessesComplete (branch state ++ rest)

    insert candidate archive
      | candidate `elem` archive = (archive, False, False)
      | any ((`strictlyDominates` entryCost candidate) . entryCost) archive = (archive, False, False)
      | length same >= witnessLimit = (archive, True, False)
      | otherwise =
          let next = candidate : filter (not . (entryCost candidate `strictlyDominates`) . entryCost) archive
          in (next, False, vectorCount next > archiveLimit)
      where same = filter ((== entryCost candidate) . entryCost) archive

    vectorCount = length . distinctCosts
    distinctCosts = foldl' (\seen entry -> if entryCost entry `elem` seen then seen else entryCost entry : seen) []
    takeArchive entries =
      let retained = take archiveLimit (sortOn id (distinctCosts entries))
      in filter ((`elem` retained) . entryCost) entries

    branch (Partial selected needed spent) =
      let options candidateOwner = filter (not . closesCycle selected candidateOwner)
            (IM.findWithDefault [] candidateOwner problem)
          (owner, members) = minimumBy (comparing (\(c, ns) -> (length ns, c)))
            [(c, options c) | c <- IS.toList needed]
          remaining = IS.delete owner needed
          demanded n = (remaining `IS.union` nodeChildren n)
            `IS.difference` IS.insert owner (IM.keysSet selected)
          successor n = Partial (IM.insert owner n selected) (demanded n) (plus spent (nodeCost n))
      in sortOn lower (map successor members)
