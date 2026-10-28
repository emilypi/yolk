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
  , ParetoCoverage (..), ParetoFeasibility (..), ParetoTermination (..)
  , ParetoResult, ParetoCandidate, paretoCandidates, candidateValue, candidateCost
  , paretoCoverage, paretoFeasibility, paretoTermination, paretoExpansions
  , paretoPendingBounds, paretoWitnessesComplete
  , extractParetoMany, extractPareto
  ) where

import Control.Monad (unless)
import Data.Functor.Const (Const (..))
import Data.HKD (FFunctor (..), FFoldable (..), FTraversable (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Word (Word64)
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

-- | An immutable selected forest and its exact additive vector. This vector
-- describes the supplied extraction model, not a downstream reconstructed or
-- physically mapped circuit. There is intentionally no Functor instance.
data ParetoCandidate a = ParetoCandidate !a ![Natural]
type role ParetoCandidate nominal

candidateValue :: ParetoCandidate a -> a
candidateValue (ParetoCandidate value _) = value

candidateCost :: ParetoCandidate a -> [Natural]
candidateCost (ParetoCandidate _ cost) = cost

data ParetoResult a = ParetoResult
  ![ParetoCandidate a] !ParetoCoverage !ParetoFeasibility !ParetoTermination
  !Word64 ![[Natural]] !Bool
type role ParetoResult nominal

paretoCandidates :: ParetoResult a -> [ParetoCandidate a]
paretoCandidates (ParetoResult value _ _ _ _ _ _) = value
paretoCoverage :: ParetoResult a -> ParetoCoverage
paretoCoverage (ParetoResult _ value _ _ _ _ _) = value
paretoFeasibility :: ParetoResult a -> ParetoFeasibility
paretoFeasibility (ParetoResult _ _ value _ _ _ _) = value
paretoTermination :: ParetoResult a -> ParetoTermination
paretoTermination (ParetoResult _ _ _ value _ _ _) = value
paretoExpansions :: ParetoResult a -> Word64
paretoExpansions (ParetoResult _ _ _ _ value _ _) = value
-- | At most one componentwise lower envelope for all unresolved branches.
-- Coordinates need not be jointly attainable. An empty list means no pending
-- branches, not a certificate that every equivalent circuit was searched.
paretoPendingBounds :: ParetoResult a -> [[Natural]]
paretoPendingBounds (ParetoResult _ _ _ _ _ value _) = value
paretoWitnessesComplete :: ParetoResult a -> Bool
paretoWitnessesComplete (ParetoResult _ _ _ _ _ _ value) = value

validateOptions :: ParetoOptions -> Either ParetoError ()
validateOptions options = do
  bounded "dimensions" 64 (paretoDimensions options)
  bounded "archive limit" 1000000 (paretoArchiveLimit options)
  bounded "witness limit" 1000000 (paretoWitnessLimit options)
  bounded "node limit" 1000000 (paretoNodeLimit options)
  bounded "root limit" 1000000 (paretoRootLimit options)
  bounded "cost bit limit" 65536 (paretoCostBitLimit options)
  unless (toInteger (paretoArchiveLimit options) * toInteger (paretoWitnessLimit options) <= 1000000)
    (Left (InvalidParetoOptions "archive/witness product exceeds 1000000"))
  maybe (pure ()) (fmap (const ()) . validateVector options) (paretoUpperBounds options)
  where
    bounded label upper value = unless (value > 0 && value <= upper)
      (Left (InvalidParetoOptions (label ++ " must be between 1 and " ++ show upper)))

validateVector :: ParetoOptions -> [Natural] -> Either ParetoError [Natural]
validateVector options raw = do
  let dimension = paretoDimensions options
      vector = take (dimension + 1) raw
  unless (length vector == dimension)
    (Left (ObjectiveDimensionMismatch dimension (length vector)))
  unless (all (< (2 ^ paretoCostBitLimit options :: Natural)) vector)
    (Left (ParetoInputLimit "objective or bound exceeds the input bit limit"))
  pure vector

-- | Select one visible representative per reachable canonical e-class. Upper
-- bounds are inclusive. A work cap may still prove vector completeness from
-- pending bounds, while witness enumeration remains incomplete. At an archive
-- overflow search stops after retaining a deterministic subset, reports
-- 'Approximate', and makes no epsilon or full-front guarantee.
extractParetoMany :: (Backend b m, Language l, FTraversable roots)
  => ParetoOptions -> ParetoCost l -> roots (ClassId r)
  -> EGraphT b r l m (Either ParetoError (ParetoResult (roots (Dag l))))
extractParetoMany options model requested = case validateOptions options of
  Left err -> pure (Left err)
  Right () -> do
    count <- nodeCount
    if count > paretoNodeLimit options
      then pure (Left (ParetoInputLimit "graph node limit exceeded before snapshot"))
      else if length (take (paretoRootLimit options + 1) (ffoldMap (const [()]) requested))
          > paretoRootLimit options
        then pure (Left (ParetoInputLimit "root position limit exceeded"))
        else do
          snap <- freeze
          roots <- ftraverse findClass requested
          let rows = [(ident, original, n) | (ident, n@(Stored _ original _ True)) <- IM.toAscList (snapshotRows snap)]
              convert (ident, original, Stored _ _ n _) = do
                weight <- validateVector options (model (erase n))
                case IM.lookup original (snapshotRoots snap) of
                  Nothing -> Left (ParetoInputLimit "snapshot class is missing")
                  Just owner -> Right (owner, [Search.Node ident (ffoldMap childSet n) weight])
              childSet = IS.singleton . classIdIndex
          case traverse convert rows of
            Left err -> pure (Left err)
            Right converted -> do
              let result = Search.solve (paretoDimensions options) (paretoUpperBounds options)
                    (paretoExpansionLimit options) (paretoArchiveLimit options) (paretoWitnessLimit options)
                    (IM.fromListWith (++) converted) (ffoldMap childSet roots)
                  reconstruct entry = do
                    forest <- dagFromSelection snap (Search.entrySelection entry) roots
                    pure (ParetoCandidate forest (Search.entryCost entry))
              case traverse reconstruct (Search.resultEntries result) of
                Nothing -> throwGraph InvalidExtraction
                Just candidates -> pure (Right (fromSearch result candidates))

fromSearch :: Search.Result -> [ParetoCandidate a] -> ParetoResult a
fromSearch result candidates = ParetoResult candidates coverage feasibility termination
  (Search.resultExpanded result) (Search.resultBounds result) (Search.resultWitnessesComplete result)
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
