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
  , paretoDimensions, paretoUpperBounds, paretoExpansionLimit
  , paretoArchiveLimit, paretoWitnessLimit, paretoNodeLimit, paretoRootLimit, paretoCostBitLimit
  , candidateValue, candidateCost, paretoCandidates, paretoCoverage, paretoFeasibility
  , paretoTermination, paretoExpansions, paretoPendingBounds, paretoWitnessesComplete
  , parsedParetoOptions
  , _Complete, _Partial, _Approximate, _FeasibleFound, _NoFeasibleFoundYet, _ProvenInfeasible
  , _SearchExhausted, _ExpansionBudget, _ArchiveBudget
  , _InvalidParetoOptions, _ObjectiveDimensionMismatch, _ParetoInputLimit
  , classIdRep, classIdIndex, snapshotRoots, snapshotRows
  ) where

import Control.Lens (Getter, Iso', Lens', Prism', iso, lens, prism', to)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import Numeric.Natural (Natural)
import Type.Reflection (Typeable, TypeRep)
import qualified Yolk.Extract.Dag as D
import qualified Yolk.Extract.Pareto as P
import Yolk.Extract.Pareto (parsedParetoOptions)
import Yolk.Internal.Types
import qualified Yolk.Internal.Graph as G
import Yolk.Rewrite (_Var, _Node, rewriteName)
import qualified Yolk.Rewrite as R

-- | One layer of a finite term, at the same sort in both directions.
_Term :: Typeable s => Iso' (Term l s) (l s (Term l))
_Term = iso (\(Term n) -> n) Term

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
-- unbounded search. Result fields support ordinary edits; they do not
-- revalidate evidence. DAG node optics remain read-only to preserve sharing
-- and acyclicity.
expansionLimit :: Lens' D.DagLimits (Maybe Word64)
expansionLimit = lens D.expansionLimit (\s a -> s { D.expansionLimit = a })

dag :: Lens' (D.DagResult a) a
dag = lens D.dag (\s a -> s { D.dag = a })

dagCost :: Lens' (D.DagResult a) Word64
dagCost = lens D.dagCost (\s a -> s { D.dagCost = a })

lowerBound :: Lens' (D.DagResult a) Word64
lowerBound = lens D.lowerBound (\s a -> s { D.lowerBound = a })

optimality :: Lens' (D.DagResult a) D.Optimality
optimality = lens D.optimality (\s a -> s { D.optimality = a })

expansions :: Lens' (D.DagResult a) Word64
expansions = lens D.expansions (\s a -> s { D.expansions = a })

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

-- | Raw Pareto configuration lenses. Extraction parses edited options again.
paretoDimensions :: Lens' P.ParetoOptions Int
paretoDimensions = lens P.paretoDimensions (\s a -> s { P.paretoDimensions = a })

paretoUpperBounds :: Lens' P.ParetoOptions (Maybe [Natural])
paretoUpperBounds = lens P.paretoUpperBounds (\s a -> s { P.paretoUpperBounds = a })

paretoExpansionLimit :: Lens' P.ParetoOptions (Maybe Word64)
paretoExpansionLimit = lens P.paretoExpansionLimit (\s a -> s { P.paretoExpansionLimit = a })

paretoArchiveLimit :: Lens' P.ParetoOptions Int
paretoArchiveLimit = lens P.paretoArchiveLimit (\s a -> s { P.paretoArchiveLimit = a })

paretoWitnessLimit :: Lens' P.ParetoOptions Int
paretoWitnessLimit = lens P.paretoWitnessLimit (\s a -> s { P.paretoWitnessLimit = a })

paretoNodeLimit :: Lens' P.ParetoOptions Int
paretoNodeLimit = lens P.paretoNodeLimit (\s a -> s { P.paretoNodeLimit = a })

paretoRootLimit :: Lens' P.ParetoOptions Int
paretoRootLimit = lens P.paretoRootLimit (\s a -> s { P.paretoRootLimit = a })

paretoCostBitLimit :: Lens' P.ParetoOptions Int
paretoCostBitLimit = lens P.paretoCostBitLimit (\s a -> s { P.paretoCostBitLimit = a })

candidateValue :: Lens' (P.ParetoCandidate a) a
candidateValue = lens P.candidateValue (\s a -> s { P.candidateValue = a })

candidateCost :: Lens' (P.ParetoCandidate a) [Natural]
candidateCost = lens P.candidateCost (\s a -> s { P.candidateCost = a })

paretoCandidates :: Lens' (P.ParetoResult a) [P.ParetoCandidate a]
paretoCandidates = lens P.paretoCandidates (\s a -> s { P.paretoCandidates = a })

paretoCoverage :: Lens' (P.ParetoResult a) P.ParetoCoverage
paretoCoverage = lens P.paretoCoverage (\s a -> s { P.paretoCoverage = a })

paretoFeasibility :: Lens' (P.ParetoResult a) P.ParetoFeasibility
paretoFeasibility = lens P.paretoFeasibility (\s a -> s { P.paretoFeasibility = a })

paretoTermination :: Lens' (P.ParetoResult a) P.ParetoTermination
paretoTermination = lens P.paretoTermination (\s a -> s { P.paretoTermination = a })

paretoExpansions :: Lens' (P.ParetoResult a) Word64
paretoExpansions = lens P.paretoExpansions (\s a -> s { P.paretoExpansions = a })

paretoPendingBounds :: Lens' (P.ParetoResult a) [[Natural]]
paretoPendingBounds = lens P.paretoPendingBounds (\s a -> s { P.paretoPendingBounds = a })

paretoWitnessesComplete :: Lens' (P.ParetoResult a) Bool
paretoWitnessesComplete = lens P.paretoWitnessesComplete (\s a -> s { P.paretoWitnessesComplete = a })

_Complete, _Partial, _Approximate :: Prism' P.ParetoCoverage ()
_Complete = prism' (const P.Complete) $ \case P.Complete -> Just (); _ -> Nothing
_Partial = prism' (const P.Partial) $ \case P.Partial -> Just (); _ -> Nothing
_Approximate = prism' (const P.Approximate) $ \case P.Approximate -> Just (); _ -> Nothing

_FeasibleFound, _NoFeasibleFoundYet, _ProvenInfeasible :: Prism' P.ParetoFeasibility ()
_FeasibleFound = prism' (const P.FeasibleFound) $ \case P.FeasibleFound -> Just (); _ -> Nothing
_NoFeasibleFoundYet = prism' (const P.NoFeasibleFoundYet) $ \case P.NoFeasibleFoundYet -> Just (); _ -> Nothing
_ProvenInfeasible = prism' (const P.ProvenInfeasible) $ \case P.ProvenInfeasible -> Just (); _ -> Nothing

_SearchExhausted, _ExpansionBudget, _ArchiveBudget :: Prism' P.ParetoTermination ()
_SearchExhausted = prism' (const P.SearchExhausted) $ \case P.SearchExhausted -> Just (); _ -> Nothing
_ExpansionBudget = prism' (const P.ExpansionBudget) $ \case P.ExpansionBudget -> Just (); _ -> Nothing
_ArchiveBudget = prism' (const P.ArchiveBudget) $ \case P.ArchiveBudget -> Just (); _ -> Nothing

_InvalidParetoOptions :: Prism' P.ParetoError String
_InvalidParetoOptions = prism' P.InvalidParetoOptions $ \case P.InvalidParetoOptions message -> Just message; _ -> Nothing

_ObjectiveDimensionMismatch :: Prism' P.ParetoError (Int, Int)
_ObjectiveDimensionMismatch = prism' (uncurry P.ObjectiveDimensionMismatch) $ \case
  P.ObjectiveDimensionMismatch expected actual -> Just (expected, actual)
  _ -> Nothing

_ParetoInputLimit :: Prism' P.ParetoError String
_ParetoInputLimit = prism' P.ParetoInputLimit $ \case P.ParetoInputLimit message -> Just message; _ -> Nothing

-- | Identity and snapshot inspection remains read-only: editing these
-- representations could bypass graph-region and reconstruction checks.
classIdRep :: Getter (G.ClassId r s) (TypeRep s)
classIdRep = to G.classIdRep

classIdIndex :: Getter (G.ClassId r s) Int
classIdIndex = to G.classIdIndex

snapshotRoots :: Getter (G.Snapshot r l) (IM.IntMap Int)
snapshotRoots = to G.snapshotRoots

snapshotRows :: Getter (G.Snapshot r l) (IM.IntMap (G.Stored r l))
snapshotRows = to G.snapshotRows
