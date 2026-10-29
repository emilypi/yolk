{-# LANGUAGE DerivingVia #-}

-- |
-- Module       : ExampleResults
-- Description  : Named extraction observations used by the examples.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- These are presentation values. Search evidence stays in Yolk's result
-- records; rendering a candidate does not certify or rerun its extraction.
module ExampleResults
  ( CandidateSummary (..)
  , summaryCostL
  , summaryExpressionL
  , ObjectiveCosts (..)
  , _ObjectiveCosts
  , FrontierSummary (..)
  , frontierCoverageL
  , frontierCandidatesL
  , PortfolioOutcome (..)
  , portfolioOutcome
  , _GraphFailed
  , _ExtractionFailed
  , _FrontierFound
  , Simplification (..)
  , reasonL
  , numericResultL
  , booleanResultL
  , Choices (..)
  , smallestL
  , cheapestL
  , shallowestL
  , lexicographicL
  , DagChoices (..)
  , treeChoiceL
  , sharedChoiceL
  , weightedChoiceL
  , limitedChoiceL
  ) where

import Control.Lens (Iso', Lens', Prism', iso, lens, prism')
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Yolk (DagResult, EGraphError, ParetoCoverage, ParetoError, StopReason)

-- | A rendered witness paired with the cost model used to select it.
data CandidateSummary cost = CandidateSummary
  { summaryCost :: cost
  , summaryExpression :: String
  }
  deriving (Eq, Show)

summaryCostL :: Lens' (CandidateSummary cost) cost
summaryCostL = lens summaryCost (\s a -> s {summaryCost = a})

summaryExpressionL :: Lens' (CandidateSummary cost) String
summaryExpressionL = lens summaryExpression (\s a -> s {summaryExpression = a})

-- | Minimized coordinates in the extractor's objective order. This newtype
-- names their role in the display; dimension validation belongs to extraction.
-- The small presentation vector retains the public API's list representation.
newtype ObjectiveCosts = ObjectiveCosts {getObjectiveCosts :: [Natural]}
  deriving stock (Show)
  deriving (Eq) via [Natural]

_ObjectiveCosts :: Iso' ObjectiveCosts [Natural]
_ObjectiveCosts = iso getObjectiveCosts ObjectiveCosts

data FrontierSummary = FrontierSummary
  { frontierCoverage :: ParetoCoverage
  , frontierCandidates :: [CandidateSummary ObjectiveCosts]
  }
  deriving (Eq, Show)

frontierCoverageL :: Lens' FrontierSummary ParetoCoverage
frontierCoverageL = lens frontierCoverage (\s a -> s {frontierCoverage = a})

frontierCandidatesL :: Lens' FrontierSummary [CandidateSummary ObjectiveCosts]
frontierCandidatesL = lens frontierCandidates (\s a -> s {frontierCandidates = a})

-- | The two failure stages remain distinguishable at the display boundary.
data PortfolioOutcome
  = GraphFailed EGraphError
  | ExtractionFailed ParetoError
  | FrontierFound FrontierSummary
  deriving (Eq, Show)

portfolioOutcome ::
  Either EGraphError (Either ParetoError FrontierSummary) -> PortfolioOutcome
portfolioOutcome = either GraphFailed (either ExtractionFailed FrontierFound)

_GraphFailed :: Prism' PortfolioOutcome EGraphError
_GraphFailed = prism' GraphFailed $ \case
  GraphFailed err -> Just err
  _ -> Nothing

_ExtractionFailed :: Prism' PortfolioOutcome ParetoError
_ExtractionFailed = prism' ExtractionFailed $ \case
  ExtractionFailed err -> Just err
  _ -> Nothing

_FrontierFound :: Prism' PortfolioOutcome FrontierSummary
_FrontierFound = prism' FrontierFound $ \case
  FrontierFound result -> Just result
  _ -> Nothing

data Simplification = Simplification
  { reason :: StopReason
  , numericResult :: Maybe (CandidateSummary Int)
  , booleanResult :: Maybe (CandidateSummary Int)
  }
  deriving (Eq, Show)

reasonL :: Lens' Simplification StopReason
reasonL = lens reason (\s a -> s {reason = a})

numericResultL :: Lens' Simplification (Maybe (CandidateSummary Int))
numericResultL = lens numericResult (\s a -> s {numericResult = a})

booleanResultL :: Lens' Simplification (Maybe (CandidateSummary Int))
booleanResultL = lens booleanResult (\s a -> s {booleanResult = a})

data Choices = Choices
  { smallest :: Maybe (CandidateSummary Word64)
  , cheapest :: Maybe (CandidateSummary Word64)
  , shallowest :: Maybe (CandidateSummary Word64)
  , lexicographic :: Maybe (CandidateSummary (Word64, Word64))
  }
  deriving (Eq, Show)

smallestL :: Lens' Choices (Maybe (CandidateSummary Word64))
smallestL = lens smallest (\s a -> s {smallest = a})

cheapestL :: Lens' Choices (Maybe (CandidateSummary Word64))
cheapestL = lens cheapest (\s a -> s {cheapest = a})

shallowestL :: Lens' Choices (Maybe (CandidateSummary Word64))
shallowestL = lens shallowest (\s a -> s {shallowest = a})

lexicographicL :: Lens' Choices (Maybe (CandidateSummary (Word64, Word64)))
lexicographicL = lens lexicographic (\s a -> s {lexicographic = a})

data DagChoices a = DagChoices
  { treeChoice :: Maybe (CandidateSummary Word64)
  , sharedChoice :: Maybe (DagResult a)
  , weightedChoice :: Maybe (DagResult a)
  , limitedChoice :: Maybe (DagResult a)
  }
  deriving (Eq, Show)

treeChoiceL :: Lens' (DagChoices a) (Maybe (CandidateSummary Word64))
treeChoiceL = lens treeChoice (\s a -> s {treeChoice = a})

sharedChoiceL :: Lens' (DagChoices a) (Maybe (DagResult a))
sharedChoiceL = lens sharedChoice (\s a -> s {sharedChoice = a})

weightedChoiceL :: Lens' (DagChoices a) (Maybe (DagResult a))
weightedChoiceL = lens weightedChoice (\s a -> s {weightedChoice = a})

limitedChoiceL :: Lens' (DagChoices a) (Maybe (DagResult a))
limitedChoiceL = lens limitedChoice (\s a -> s {limitedChoice = a})
