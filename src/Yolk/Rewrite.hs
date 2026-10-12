{-# LANGUAGE GADTs #-}

-- |
-- Module       : Yolk.Rewrite
-- Description  : Validated typed rewrites and bounded equality saturation.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Typed rewrite patterns and a bounded search/apply runner, following
-- the equality-saturation iteration in Willsey et al. (POPL 2021):
-- https://doi.org/10.1145/3434304
--
-- Matching here enumerates visible constructor members in an immutable
-- snapshot. It deliberately has no memo-based bound-pattern optimization:
-- that optimization changes matching behavior after pruning. Search finishes
-- before apply starts. This is a backtracking baseline, not a relational VM.
module Yolk.Rewrite
  ( Pattern, var, node, Rewrite, RuleError (..), rewrite
  , Limits (..), defaultLimits, StopReason (..), RunResult (..), run
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (gets)
import Data.HKD (FFoldable (..), FTraversable (..))
import qualified Data.Map.Strict as M
import Type.Reflection (SomeTypeRep (..), TypeRep, Typeable, eqTypeRep, typeRep, (:~~:) (HRefl))
import Yolk.Backend (Backend)
import Yolk.Internal.Graph
import Yolk.Internal.Types

-- | A named variable at a known sort. Repeated names impose equality.
var :: Typeable s => String -> Pattern l s
var = PVar typeRep

node :: Typeable s => l s (Pattern l) -> Pattern l s
node = PNode typeRep

data RuleError
  = UnboundVariable !String
  | VariableSortMismatch !String
  deriving (Eq, Show)

patternRep :: Pattern l s -> TypeRep s
patternRep (PVar rep _) = rep
patternRep (PNode rep _) = rep

variables :: Language l => Pattern l s -> [(String, SomeTypeRep)]
variables (PVar rep name) = [(name, SomeTypeRep rep)]
variables (PNode _ n) = ffoldMap variables n

-- | Validate variables before a rule enters the runner. The RHS can only
-- consume bindings supplied by the LHS, at precisely the same sorts.
rewrite :: Language l => String -> Pattern l s -> Pattern l s -> Either RuleError (Rewrite l)
rewrite name lhs rhs = do
  bindings <- foldM bind M.empty (variables lhs)
  mapM_ (check bindings) (variables rhs)
  pure (Rewrite name lhs rhs)
  where
    bind env (v, rep) = case M.lookup v env of
      Nothing -> Right (M.insert v rep env)
      Just old | old == rep -> Right env
               | otherwise -> Left (VariableSortMismatch v)
    check env (v, rep) = case M.lookup v env of
      Nothing -> Left (UnboundVariable v)
      Just old | old == rep -> Right ()
               | otherwise -> Left (VariableSortMismatch v)

data SomeId r where
  SomeId :: ClassId r s -> SomeId r

data SomePattern l where
  SomePattern :: Pattern l s -> SomePattern l

type Substitution r = M.Map String (SomeId r)

match :: Language l => Snapshot r l -> Pattern l s -> ClassId r s -> Substitution r -> [Substitution r]
match snap pat requested env = case snapshotFind requested snap of
  Nothing -> []
  Just c -> case pat of
    PVar rep name -> case M.lookup name env of
      Nothing -> [M.insert name (SomeId c) env]
      Just (SomeId old@(ClassId actual _)) -> case eqTypeRep rep actual of
        Just HRefl -> [env | snapshotFind old snap == Just c]
        Nothing -> []
    PNode _ p -> concatMap (matchNode p) (snapshotNodes c snap)
  where
    matchNode p n
      | operatorKey (erase p) /= operatorKey (erase n) = []
      | length pats /= length kids = []
      | otherwise = foldM matchChild env (zip pats kids)
      where
        pats = ffoldMap (\x -> [SomePattern x]) p
        kids = ffoldMap (\x -> [SomeId x]) n
    matchChild bindings (SomePattern p, SomeId c@(ClassId rep _)) =
      case eqTypeRep (patternRep p) rep of
        Just HRefl -> match snap p c bindings
        Nothing -> []

instantiate :: (Backend b m, Language l) => Substitution r -> Pattern l s -> EGraphT b r l m (ClassId r s)
instantiate env (PVar rep name) = case M.lookup name env of
  Nothing -> throwGraph (InvalidBinding name)
  Just (SomeId c@(ClassId actual _)) -> case eqTypeRep rep actual of
    Just HRefl -> findClass c
    Nothing -> throwGraph (InvalidBinding name)
instantiate env (PNode rep n) = ftraverse (instantiate env) n >>= addAt rep

data Application r l where
  Application :: ClassId r s -> Pattern l s -> Substitution r -> Application r l

applicationsFor :: Language l => Snapshot r l -> Rewrite l -> [Application r l]
applicationsFor snap (Rewrite _ lhs rhs) =
  [Application root rhs bindings
  | root <- snapshotClasses (patternRep lhs) snap
  , bindings <- match snap lhs root M.empty]

data Limits = Limits
  { iterationLimit :: !Int
  , nodeLimit :: !Int
  , matchLimit :: !Int
  } deriving (Eq, Show)

defaultLimits :: Limits
defaultLimits = Limits 30 100000 50000

data StopReason = Saturated | IterationLimit | NodeLimit | MatchLimit
  deriving (Eq, Show)

data RunResult = RunResult
  { stopReason :: !StopReason
  , iterations :: !Int
  , applications :: !Int
  , finalNodes :: !Int
  } deriving (Eq, Show)

-- | Run bounded equality saturation. A search exceeding 'matchLimit' is
-- rejected as a whole, without applying a truncated match set. The node
-- limit is checked initially and after each atomic rewrite; a single RHS
-- may overshoot it. The returned graph is always rebuilt before reporting.
run :: (Backend b m, Language l) => Limits -> [Rewrite l] -> EGraphT b r l m RunResult
run limits rules
  | any (< 0) [iterationLimit limits, nodeLimit limits, matchLimit limits] = throwGraph InvalidLimits
  | otherwise = loop 0 0
  where
    finish reason n applied = do
      rebuild
      RunResult reason n applied <$> nodeCount
    loop n applied = do
      snap <- freeze
      count <- nodeCount
      if count > nodeLimit limits then finish NodeLimit n applied
      else if n >= iterationLimit limits then finish IterationLimit n applied
      else do
        let (work, excess) = splitAt (matchLimit limits) (concatMap (applicationsFor snap) rules)
        if not (null excess) then finish MatchLimit n applied
        else do
          version <- EGraphT (gets graphVersion)
          (performed, exceeded) <- applyBatch 0 work
          rebuild
          version' <- EGraphT (gets graphVersion)
          if exceeded then finish NodeLimit (n + 1) (applied + performed)
          else if version == version' then finish Saturated (n + 1) (applied + performed)
          else loop (n + 1) (applied + performed)
    applyBatch n [] = pure (n, False)
    applyBatch n (Application root rhs env : rest) = do
      result <- instantiate env rhs
      _ <- union root result
      count <- nodeCount
      if count > nodeLimit limits then pure (n + 1, True)
      else applyBatch (n + 1) rest
