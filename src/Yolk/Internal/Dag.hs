{-# LANGUAGE BangPatterns #-}

-- |
-- Module       : Yolk.Internal.Dag
-- Description  : Branch-and-bound search for finite DAG extraction.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Solver-free branch-and-bound over a sort-erased immutable problem.
-- Typed reconstruction lives in Yolk.Extract.Dag. No tree-cost relaxation
-- participates in the optimality proof.
module Yolk.Internal.Dag (Node (..), Solution (..), solve) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (minimumBy, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import Yolk.Extract (saturatingAdd)

data Node = Node
  { nodeId :: !Int
  , nodeChildren :: !IS.IntSet
  , nodeWeight :: {-# UNPACK #-} !Word64
  }

type Problem = IM.IntMap [Node]
type Selection = IM.IntMap Node

data Solution = Solution
  { selection :: !(IM.IntMap Int)
  , cost :: {-# UNPACK #-} !Word64
  , bound :: {-# UNPACK #-} !Word64
  , proven :: !Bool
  , expanded :: {-# UNPACK #-} !Word64
  }

data Partial = Partial !Selection !IS.IntSet {-# UNPACK #-} !Word64
data Incumbent = Incumbent !Selection {-# UNPACK #-} !Word64

total :: [Word64] -> Word64
total = foldl' saturatingAdd 0

-- | Establish finite witnesses in discovery order. Every chosen edge points
-- to a class discovered earlier, even when all weights are zero. Choices are
-- only a seed, never used to discard alternatives in the exact search.
seed :: Problem -> Selection
seed problem = IM.map fst (loop IM.empty)
 where
  loop known =
    let next = IM.foldlWithKey' discover known problem
     in if IM.size next == IM.size known then next else loop next
  discover known owner nodes
    | IM.member owner known = known
    | otherwise = case mapMaybe (available known) nodes of
        [] -> known
        choices -> IM.insert owner (minimumBy (comparing rank) choices) known
  rank (n, estimate) = (estimate, nodeId n)
  available known n = do
    children <- traverse (`IM.lookup` known) (IS.toList (nodeChildren n))
    pure (n, saturatingAdd (nodeWeight n) (total (map snd children)))

-- | Visit each demanded class once, preserving sharing. The table must
-- contain a witness for every reachable class (ensured by seed).
closure :: Selection -> IS.IntSet -> Selection
closure table roots = go IM.empty (IS.toList roots)
 where
  go !seen [] = seen
  go !seen (c : todo)
    | IM.member c seen = go seen todo
    | otherwise = case IM.lookup c table of
        Nothing -> go seen todo
        Just n -> go (IM.insert c n seen) (IS.toList (nodeChildren n) ++ todo)

reachable :: Problem -> IS.IntSet -> IS.IntSet
reachable problem roots = go IS.empty (IS.toList roots)
 where
  go !seen [] = seen
  go !seen (c : todo)
    | IS.member c seen = go seen todo
    | otherwise =
        let children = IS.unions (map nodeChildren (IM.findWithDefault [] c problem))
         in go (IS.insert c seen) (IS.toList children ++ todo)

-- Only a path back to the newly selected owner can create a cycle: all
-- previously selected edges are acyclic. Unselected classes have no edges.
closesCycle :: Selection -> Int -> Node -> Bool
closesCycle chosen owner n = go IS.empty (IS.toList (nodeChildren n))
 where
  go _ [] = False
  go seen (c : todo)
    | c == owner = True
    | IS.member c seen = go seen todo
    | otherwise =
        let children = maybe [] (IS.toList . nodeChildren) (IM.lookup c chosen)
         in go (IS.insert c seen) (children ++ todo)

-- | Nothing means the roots have no finite derivation. Costs and bounds use
-- the saturated Word64 domain. A cap counts branch expansions; preprocessing,
-- constructing successors, and reconstruction are outside that count.
solve :: Maybe Word64 -> Problem -> IS.IntSet -> Maybe Solution
solve limit original roots
  | not (roots `IS.isSubsetOf` productive) = Nothing
  | otherwise = Just (search 0 initial [Partial IM.empty roots 0])
 where
  -- A self edge cannot occur in any acyclic selection.
  noSelf =
    IM.mapWithKey (\owner -> filter (not . IS.member owner . nodeChildren)) original
  witnesses = seed noSelf
  productive = IM.keysSet witnesses
  live =
    IM.map
      (filter ((`IS.isSubsetOf` productive) . nodeChildren))
      (IM.restrictKeys noSelf productive)
  problem = IM.restrictKeys live (reachable live roots)
  minima = IM.map (minimum . map nodeWeight) problem
  initialSelection = closure witnesses roots
  initial =
    Incumbent initialSelection (total (map nodeWeight (IM.elems initialSelection)))

  -- Every distinct demanded-but-unchosen class must contribute one member.
  -- Ignoring any further descendants (and cycles) can only lower the cost.
  lower (Partial _ needed spent) =
    saturatingAdd
      spent
      (total [IM.findWithDefault 0 c minima | c <- IS.toList needed])

  finish count isProven lowerCost (Incumbent chosen best) =
    Solution (IM.map nodeId chosen) best lowerCost isProven count

  search !count incumbent@(Incumbent _ best) pending =
    case dropWhile ((>= best) . lower) pending of
      [] -> finish count True best incumbent
      state@(Partial chosen needed spent) : rest
        | IS.null needed -> search count (Incumbent chosen spent) rest
        | maybe False (count >=) limit ->
            let certificate = minimum (best : map lower (state : rest))
             in finish count False certificate incumbent
        | otherwise ->
            search (saturatingAdd count 1) incumbent (branch state ++ rest)

  branch (Partial chosen needed spent) =
    let options c =
          filter
            (not . closesCycle chosen c)
            (IM.findWithDefault [] c problem)
        choices = [(c, options c) | c <- IS.toList needed]
        priority (c, ns) = (length ns, Down (IM.findWithDefault 0 c minima), c)
        (owner, members) = minimumBy (comparing priority) choices
        remaining = IS.delete owner needed
        demanded n =
          (remaining `IS.union` nodeChildren n)
            `IS.difference` IS.insert owner (IM.keysSet chosen)
        successor n =
          Partial
            (IM.insert owner n chosen)
            (demanded n)
            (saturatingAdd spent (nodeWeight n))
     in sortOn lower (map successor members)
