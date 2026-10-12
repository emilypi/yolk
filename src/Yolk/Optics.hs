-- |
-- Module       : Yolk.Optics
-- Description  : Lens-compatible optics for yolk terms, rewrites, and extraction.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Optics compatible with @lens@. Import qualified to distinguish the record
-- lenses from the existing accessors:
--
-- @
-- import Control.Lens ((&), (.~), (^.))
-- import qualified Yolk.Optics as O
-- limits = defaultLimits & O.iterationLimit .~ 100
-- @
--
-- Terms and patterns retain their result sorts. Graph identities and snapshots
-- remain opaque. A validated rewrite exposes only its name for modification;
-- construct changed rule bodies through 'R.rewrite' to validate them again.
module Yolk.Optics
  ( _Term, _Var, _Node, rewriteName
  , iterationLimit, nodeLimit, matchLimit
  , stopReason, iterations, applications, finalNodes
  , _Saturated, _IterationLimit, _NodeLimit, _MatchLimit
  , _UnboundVariable, _VariableSortMismatch
  , _DirtyGraph, _UnknownClass, _SortMismatch
  , _InvalidLimits, _InvalidBinding, _CostOverflow
  , expansionLimit, dag, dagCost, lowerBound, optimality, expansions
  , dagNode, dagNodeId, _Optimal, _Unproven, _InvalidExtraction
  ) where

import Control.Lens (Getter, Iso', Lens', Prism', iso, lens, prism', to)
import Data.Word (Word64)
import Type.Reflection (Typeable, typeRep)
import qualified Yolk.Extract.Dag as D
import Yolk.Internal.Types
import qualified Yolk.Rewrite as R

-- | One layer of a finite term, at the same sort in both directions.
_Term :: Typeable s => Iso' (Term l s) (l s (Term l))
_Term = iso (\(Term n) -> n) Term

-- | Construct, inspect, or rename a pattern variable at its existing sort.
_Var :: Typeable s => Prism' (Pattern l s) String
_Var = prism' (PVar typeRep) $ \case
  PVar _ name -> Just name
  PNode {} -> Nothing

-- | One constructor layer of a pattern, preserving its result sort.
_Node :: Typeable s => Prism' (Pattern l s) (l s (Pattern l))
_Node = prism' (PNode typeRep) $ \case
  PVar {} -> Nothing
  PNode _ n -> Just n

-- | Renaming a rule leaves its validated bindings and bodies intact.
rewriteName :: Lens' (Rewrite l) String
rewriteName = lens (\(Rewrite name _ _) -> name)
  (\(Rewrite _ lhs rhs) name -> Rewrite name lhs rhs)

iterationLimit :: Lens' R.Limits Int
iterationLimit = lens R.iterationLimit (\s a -> s { R.iterationLimit = a })

nodeLimit :: Lens' R.Limits Int
nodeLimit = lens R.nodeLimit (\s a -> s { R.nodeLimit = a })

matchLimit :: Lens' R.Limits Int
matchLimit = lens R.matchLimit (\s a -> s { R.matchLimit = a })

stopReason :: Lens' R.RunResult R.StopReason
stopReason = lens R.stopReason (\s a -> s { R.stopReason = a })

iterations :: Lens' R.RunResult Int
iterations = lens R.iterations (\s a -> s { R.iterations = a })

applications :: Lens' R.RunResult Int
applications = lens R.applications (\s a -> s { R.applications = a })

finalNodes :: Lens' R.RunResult Int
finalNodes = lens R.finalNodes (\s a -> s { R.finalNodes = a })

_Saturated :: Prism' R.StopReason ()
_Saturated = prism' (const R.Saturated) $ \case
  R.Saturated -> Just ()
  _ -> Nothing

_IterationLimit :: Prism' R.StopReason ()
_IterationLimit = prism' (const R.IterationLimit) $ \case
  R.IterationLimit -> Just ()
  _ -> Nothing

_NodeLimit :: Prism' R.StopReason ()
_NodeLimit = prism' (const R.NodeLimit) $ \case
  R.NodeLimit -> Just ()
  _ -> Nothing

_MatchLimit :: Prism' R.StopReason ()
_MatchLimit = prism' (const R.MatchLimit) $ \case
  R.MatchLimit -> Just ()
  _ -> Nothing

_UnboundVariable :: Prism' R.RuleError String
_UnboundVariable = prism' R.UnboundVariable $ \case
  R.UnboundVariable name -> Just name
  _ -> Nothing

_VariableSortMismatch :: Prism' R.RuleError String
_VariableSortMismatch = prism' R.VariableSortMismatch $ \case
  R.VariableSortMismatch name -> Just name
  _ -> Nothing

_DirtyGraph :: Prism' EGraphError ()
_DirtyGraph = prism' (const DirtyGraph) $ \case
  DirtyGraph -> Just ()
  _ -> Nothing

_UnknownClass :: Prism' EGraphError Int
_UnknownClass = prism' UnknownClass $ \case
  UnknownClass n -> Just n
  _ -> Nothing

_SortMismatch :: Prism' EGraphError Int
_SortMismatch = prism' SortMismatch $ \case
  SortMismatch n -> Just n
  _ -> Nothing

_InvalidLimits :: Prism' EGraphError ()
_InvalidLimits = prism' (const InvalidLimits) $ \case
  InvalidLimits -> Just ()
  _ -> Nothing

_InvalidBinding :: Prism' EGraphError String
_InvalidBinding = prism' InvalidBinding $ \case
  InvalidBinding name -> Just name
  _ -> Nothing

_CostOverflow :: Prism' EGraphError ()
_CostOverflow = prism' (const CostOverflow) $ \case
  CostOverflow -> Just ()
  _ -> Nothing

-- | Change the deterministic DAG search budget; Nothing requests an
-- unbounded search. Result and node optics below are read-only so the
-- optimality certificate, sharing, and acyclicity cannot be invalidated.
expansionLimit :: Lens' D.DagLimits (Maybe Word64)
expansionLimit = lens D.expansionLimit (\s a -> s { D.expansionLimit = a })

dag :: Getter (D.DagResult a) a
dag = to D.dag

dagCost :: Getter (D.DagResult a) Word64
dagCost = to D.dagCost

lowerBound :: Getter (D.DagResult a) Word64
lowerBound = to D.lowerBound

optimality :: Getter (D.DagResult a) D.Optimality
optimality = to D.optimality

expansions :: Getter (D.DagResult a) Word64
expansions = to D.expansions

dagNode :: Getter (D.Dag l s) (l s (D.Dag l))
dagNode = to D.dagNode

dagNodeId :: Getter (D.Dag l s) Int
dagNodeId = to D.dagNodeId

_Optimal :: Prism' D.Optimality ()
_Optimal = prism' (const D.Optimal) $ \case
  D.Optimal -> Just ()
  _ -> Nothing

_Unproven :: Prism' D.Optimality ()
_Unproven = prism' (const D.Unproven) $ \case
  D.Unproven -> Just ()
  _ -> Nothing

_InvalidExtraction :: Prism' EGraphError ()
_InvalidExtraction = prism' (const InvalidExtraction) $ \case
  InvalidExtraction -> Just ()
  _ -> Nothing
