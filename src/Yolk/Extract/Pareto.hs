{-# LANGUAGE RoleAnnotations #-}

-- |
-- Module       : Yolk.Extract.Pareto
-- Description  : Bounded shared-DAG Pareto extraction with exact vector costs.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Each coordinate is minimized and charges a nonnegative node weight once
-- across all roots. Completeness concerns vectors in the frozen visible graph,
-- not all equivalent programs or physical performance. This is a distinct
-- dominance API: Pareto order is never disguised as a total 'Ord' instance.
module Yolk.Extract.Pareto
  ( ParetoCost, ParetoOptions (..), defaultParetoOptions, ParetoError (..)
  , ParsedParetoOptions, parseParetoOptions, parsedParetoOptions
  , ParetoCoverage (..), ParetoFeasibility (..), ParetoTermination (..)
  , ParetoResult, ParetoCandidate, paretoCandidates, candidateValue, candidateCost
  , paretoCoverage, paretoFeasibility, paretoTermination, paretoExpansions
  , paretoPendingBounds, paretoWitnessesComplete
  , extractParetoMany, extractPareto
  , _ParetoOne
  ) where

import Control.Lens (Getter, Iso', iso, to)
import Control.Monad (unless)
import Data.Functor.Const (Const (..))
import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Word (Word64)
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Yolk.Backend (Backend)
import Yolk.Dag (Dag, dagFromSelection)
import Yolk.Internal.Graph
import qualified Yolk.Internal.Pareto as Search
import Yolk.Internal.Types

type ParetoCost l = forall s. l s (Const ()) -> [Natural]

-- | All limits are positive except the optional expansion cap, which may be
-- zero. Dimensions are bounded by 64; input cost precision by 65536 bits.
-- There are at most one million input rows/root positions or retained
-- witnesses. The row cap counts all stored members before snapshot creation,
-- including hidden members. Expansion counts do not bound wall time or preprocessing work;
-- the caller must arrange cancellation for an untrusted/expensive language.
data ParetoOptions = ParetoOptions
  { paretoDimensions :: !Int
  , paretoUpperBounds :: !(Maybe [Natural])
  , paretoExpansionLimit :: !(Maybe Word64)
  , paretoArchiveLimit :: !Int
  , paretoWitnessLimit :: !Int
  , paretoNodeLimit :: !Int
  , paretoRootLimit :: !Int
  , paretoCostBitLimit :: !Int
  } deriving (Eq, Show)

defaultParetoOptions :: Int -> ParetoOptions
defaultParetoOptions dimensions = ParetoOptions dimensions Nothing (Just 100000)
  1024 1 100000 10000 4096

data ParetoError
  = InvalidParetoOptions !String
  | ObjectiveDimensionMismatch !Int !Int
  | ParetoInputLimit !String
  deriving (Eq, Show)

data ParetoCoverage = Complete | Partial | Approximate deriving (Eq, Show)
data ParetoFeasibility = FeasibleFound | NoFeasibleFoundYet | ProvenInfeasible deriving (Eq, Show)
data ParetoTermination = SearchExhausted | ExpansionBudget | ArchiveBudget deriving (Eq, Show)

-- | A selected forest and its exact additive vector. This vector
-- describes the supplied extraction model, not a downstream reconstructed or
-- physically mapped circuit. Record updates are ordinary user edits: evidence
-- describes the original extractor-produced value, not a modified record.
data ParetoCandidate a = ParetoCandidate
  { candidateValue :: !a
  , candidateCost :: ![Natural]
  } deriving (Eq, Show)
type role ParetoCandidate nominal

-- | Search evidence for the original extraction. Selectors permit ordinary
-- record updates; editing results does not rerun extraction or validate them.
data ParetoResult a = ParetoResult
  { paretoCandidates :: ![ParetoCandidate a]
  , paretoCoverage :: !ParetoCoverage
  , paretoFeasibility :: !ParetoFeasibility
  , paretoTermination :: !ParetoTermination
  , paretoExpansions :: !Word64
  -- | At most one componentwise lower envelope for unresolved branches.
  -- Coordinates need not be jointly attainable. No pending branches is not
  -- a certificate that every equivalent circuit has been searched.
  , paretoPendingBounds :: ![[Natural]]
  , paretoWitnessesComplete :: !Bool
  } deriving (Eq, Show)
type role ParetoResult nominal

-- | Parsed bounded configuration. Construction validates every field and the
-- archive/witness product together. Private fields keep this construction
-- boundary intact; 'parsedParetoOptions' is a read-only optic of raw options.
data ParsedParetoOptions = ParsedParetoOptions
  { parsedDimensions :: !Int
  , parsedUpperBounds :: !(Maybe (V.Vector Natural))
  , parsedExpansionLimit :: !(Maybe Word64)
  , parsedArchiveLimit :: !Int
  , parsedWitnessLimit :: !Int
  , parsedNodeLimit :: !Int
  , parsedRootLimit :: !Int
  , parsedCostBitLimit :: !Int
  } deriving (Eq, Show)

-- | Inspect a parsed configuration as editable raw options. Editing those
-- options requires parsing them again before the extractor consumes them.
parsedParetoOptions :: Getter ParsedParetoOptions ParetoOptions
parsedParetoOptions = to $ \options -> ParetoOptions
  { paretoDimensions = parsedDimensions options
  , paretoUpperBounds = fmap V.toList (parsedUpperBounds options)
  , paretoExpansionLimit = parsedExpansionLimit options
  , paretoArchiveLimit = parsedArchiveLimit options
  , paretoWitnessLimit = parsedWitnessLimit options
  , paretoNodeLimit = parsedNodeLimit options
  , paretoRootLimit = parsedRootLimit options
  , paretoCostBitLimit = parsedCostBitLimit options
  }

parseParetoOptions :: ParetoOptions -> Either ParetoError ParsedParetoOptions
parseParetoOptions options = do
  bounded "dimensions" 64 (paretoDimensions options)
  bounded "archive limit" 1000000 (paretoArchiveLimit options)
  bounded "witness limit" 1000000 (paretoWitnessLimit options)
  bounded "node limit" 1000000 (paretoNodeLimit options)
  bounded "root limit" 1000000 (paretoRootLimit options)
  bounded "cost bit limit" 65536 (paretoCostBitLimit options)
  unless (toInteger (paretoArchiveLimit options) * toInteger (paretoWitnessLimit options) <= 1000000)
    (Left (InvalidParetoOptions "archive/witness product exceeds 1000000"))
  let parsed = ParsedParetoOptions
        (paretoDimensions options) Nothing (paretoExpansionLimit options)
        (paretoArchiveLimit options) (paretoWitnessLimit options)
        (paretoNodeLimit options) (paretoRootLimit options) (paretoCostBitLimit options)
  bounds <- traverse (parseVector parsed) (paretoUpperBounds options)
  pure parsed {parsedUpperBounds = bounds}
  where
    bounded label upper value = unless (value > 0 && value <= upper)
      (Left (InvalidParetoOptions (label ++ " must be between 1 and " ++ show upper)))

parseVector :: ParsedParetoOptions -> [Natural] -> Either ParetoError (V.Vector Natural)
parseVector options raw = do
  let dimension = parsedDimensions options
      vector = take (dimension + 1) raw
  unless (length vector == dimension)
    (Left (ObjectiveDimensionMismatch dimension (length vector)))
  unless (all (< (2 ^ parsedCostBitLimit options :: Natural)) vector)
    (Left (ParetoInputLimit "objective or bound exceeds the input bit limit"))
  pure (V.fromList vector)

-- | Select one visible representative per reachable canonical e-class. Upper
-- bounds are inclusive. A work cap may still prove vector completeness from
-- pending bounds, while witness enumeration remains incomplete. At an archive
-- overflow search stops after retaining a deterministic subset, reports
-- 'Approximate', and makes no epsilon or full-front guarantee.
extractParetoMany :: (Backend b m, Language l, FTraversable roots)
  => ParetoOptions -> ParetoCost l -> roots (ClassId r)
  -> EGraphT b r l m (Either ParetoError (ParetoResult (roots (Dag l))))
extractParetoMany rawOptions model requested = case parseParetoOptions rawOptions of
  Left err -> pure (Left err)
  Right options -> do
    count <- nodeCount
    if count > parsedNodeLimit options
      then pure (Left (ParetoInputLimit "graph node limit exceeded before snapshot"))
      else if length (take (parsedRootLimit options + 1) (ffoldMap (const [()]) requested))
          > parsedRootLimit options
        then pure (Left (ParetoInputLimit "root position limit exceeded"))
        else do
          snap <- freeze
          roots <- ftraverse findClass requested
          let rows = [(ident, original, n) | (ident, n@(Stored _ original _ True)) <- IM.toAscList (snapshotRows snap)]
              convert (ident, original, Stored _ _ n _) = do
                weight <- parseVector options (model (erase n))
                case IM.lookup original (snapshotRoots snap) of
                  Nothing -> Left (ParetoInputLimit "snapshot class is missing")
                  Just owner -> Right (owner, [Search.Node ident (ffoldMap childSet n) weight])
              childSet = IS.singleton . classIdIndex
          case traverse convert rows of
            Left err -> pure (Left err)
            Right converted -> do
              let result = Search.solve (parsedDimensions options) (parsedUpperBounds options)
                    (parsedExpansionLimit options) (parsedArchiveLimit options) (parsedWitnessLimit options)
                    (IM.fromListWith (++) converted) (ffoldMap childSet roots)
                  reconstruct entry = do
                    forest <- dagFromSelection snap (Search.entrySelection entry) roots
                    pure (ParetoCandidate forest (V.toList (Search.entryCost entry)))
              case traverse reconstruct (Search.resultEntries result) of
                Nothing -> throwGraph InvalidExtraction
                Just candidates -> pure (Right (fromSearch result candidates))

fromSearch :: Search.Result -> [ParetoCandidate a] -> ParetoResult a
fromSearch result candidates = ParetoResult candidates coverage feasibility termination
  (Search.resultExpanded result) (map V.toList (Search.resultBounds result)) (Search.resultWitnessesComplete result)
  where
    termination = case Search.resultTermination result of
      Search.Exhausted -> SearchExhausted
      Search.WorkLimit -> ExpansionBudget
      Search.ArchiveLimit -> ArchiveBudget
    coverage
      | termination == ArchiveBudget = Approximate
      | Search.resultVectorsComplete result = Complete
      | otherwise = Partial
    feasibility
      | not (null candidates) = FeasibleFound
      | coverage == Complete = ProvenInfeasible
      | otherwise = NoFeasibleFoundYet

newtype One s f = One { getOne :: f s }

-- | The single-root adapter preserves the indexed payload's sort.
_ParetoOne :: Iso' (One s f) (f s)
_ParetoOne = iso getOne One

instance FFunctor (One s) where ffmap f (One a) = One (f a)
instance FFoldable (One s) where ffoldMap f (One a) = f a
instance FTraversable (One s) where ftraverse f (One a) = One <$> f a

extractPareto :: (Backend b m, Language l)
  => ParetoOptions -> ParetoCost l -> ClassId r s
  -> EGraphT b r l m (Either ParetoError (ParetoResult (Dag l s)))
extractPareto options model root = fmap unwrap <$> extractParetoMany options model (One root)
  where
    unwrap (ParetoResult candidates coverage feasibility termination expanded bounds complete) =
      ParetoResult [ParetoCandidate (getOne value) cost | ParetoCandidate value cost <- candidates]
        coverage feasibility termination expanded bounds complete
