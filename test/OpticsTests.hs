-- |
-- Module       : OpticsTests
-- Description  : Lens laws and API tests for yolk optics.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module OpticsTests (opticsTests) where

import Control.Lens (Lens', Prism', (&), (.~), (%~), (^.), (^?), _1, _Just, from, over, preview, prism', review, set, view)
import Control.Monad (forM_, void)
import Data.Functor.Const (Const (..))
import Data.HKD (ffmap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC
import TestLanguage
import TestSupport
import Yolk
import qualified Yolk.Optics as O
import qualified Yolk.Internal.Pareto as P

literal :: Prism' (Math 'Number f) Integer
literal = prism' Lit $ \case
  Lit n -> Just n
  _ -> Nothing

opticsTests :: TestTree
opticsTests = testGroup "lens API"
  [ QC.testProperty "all limit lenses obey get-set, set-get, and set-set" $
      \i n m x y ->
        let limits = Limits i n m
        in QC.conjoin
             [ lensLaws O.iterationLimit limits x y
             , lensLaws O.nodeLimit limits x y
             , lensLaws O.matchLimit limits x y
             ]
  , QC.testProperty "all run-result lenses obey the lens laws" $
      \i a n x y ->
        let result = RunResult Saturated i a n
        in QC.conjoin
             [ lensLaws O.iterations result x y
             , lensLaws O.applications result x y
             , lensLaws O.finalNodes result x y
             , lensLaws O.stopReason result NodeLimit MatchLimit
             ]
  , QC.testProperty "DAG budget lens obeys the lens laws" $ \limit x y ->
      lensLaws O.expansionLimit (DagLimits limit) x y
  , QC.testProperty "Pareto raw numeric option lenses obey the lens laws" $ \dimension x y ->
      let options = defaultParetoOptions dimension
      in QC.conjoin
        [ lensLaws O.paretoDimensions options x y
        , lensLaws O.paretoArchiveLimit options x y
        , lensLaws O.paretoWitnessLimit options x y
        , lensLaws O.paretoNodeLimit options x y
        , lensLaws O.paretoRootLimit options x y
        , lensLaws O.paretoCostBitLimit options x y
        ]
  , QC.testProperty "Pareto optional option lenses obey the lens laws" $ \budget x y ->
      let options = (defaultParetoOptions 2) {paretoExpansionLimit = budget}
      in QC.conjoin
        [ lensLaws O.paretoExpansionLimit options x y
        , lensLaws O.paretoUpperBounds options Nothing (Just [1, 2])
        ]
  , testCase "all Pareto status and error prisms preserve payloads" $ do
      let coverage = [Complete, Partial, Approximate]
          feasibility = [FeasibleFound, NoFeasibleFoundYet, ProvenInfeasible]
          termination = [SearchExhausted, ExpansionBudget, ArchiveBudget]
          errors = [InvalidParetoOptions "limit", ObjectiveDimensionMismatch 2 3, ParetoInputLimit "size"]
      prismLaws O._Complete [()] coverage
      prismLaws O._Partial [()] coverage
      prismLaws O._Approximate [()] coverage
      prismLaws O._FeasibleFound [()] feasibility
      prismLaws O._NoFeasibleFoundYet [()] feasibility
      prismLaws O._ProvenInfeasible [()] feasibility
      prismLaws O._SearchExhausted [()] termination
      prismLaws O._ExpansionBudget [()] termination
      prismLaws O._ArchiveBudget [()] termination
      prismLaws O._InvalidParetoOptions ["", "limit"] errors
      prismLaws O._ObjectiveDimensionMismatch [(2, 3), (1, 0)] errors
      prismLaws O._ParetoInputLimit ["", "size"] errors
  , both "result records and lenses support ordinary edits without mutating graph nodes"
      (Right (True, True, True, True)) $ Program $ do
        root <- add (Lit 7)
        extracted <- extractPareto (defaultParetoOptions 2) (const [1, 2]) root
        scalar <- extractDagOptimal defaultDagLimits dagSizeCost root
        pure $ do
          result <- extracted
          case (paretoCandidates result, scalar) of
            (candidate : _, Just dagResult) ->
              let simpleCandidate = candidate {candidateValue = 7 :: Int}
                  simpleResult = result {paretoCandidates = [simpleCandidate]}
                  simpleDag = dagResult {dag = 7 :: Int}
                  candidateLaws = and
                    [ laws O.candidateValue simpleCandidate 3 4
                    , laws O.candidateCost simpleCandidate [0, 2] [3, 4]
                    ]
                  resultLaws = and
                    [ laws O.paretoCandidates simpleResult [] [simpleCandidate]
                    , laws O.paretoCoverage simpleResult Partial Approximate
                    , laws O.paretoFeasibility simpleResult NoFeasibleFoundYet ProvenInfeasible
                    , laws O.paretoTermination simpleResult ExpansionBudget ArchiveBudget
                    , laws O.paretoExpansions simpleResult 17 23
                    , laws O.paretoPendingBounds simpleResult [] [[3, 4]]
                    , laws O.paretoWitnessesComplete simpleResult False True
                    ]
                  scalarLaws = and
                    [ laws O.dag simpleDag 3 4
                    , laws O.dagCost simpleDag 3 4
                    , laws O.lowerBound simpleDag 3 4
                    , laws O.optimality simpleDag Unproven Optimal
                    , laws O.expansions simpleDag 3 4
                    ]
                  edited = simpleResult & O.paretoCoverage .~ Partial
                  originalPreserved = pretty (dagToTerm (candidateValue candidate)) == "7"
                    && paretoCoverage result == Complete && paretoCoverage edited == Partial
              in Right (candidateLaws, resultLaws, scalarLaws, originalPreserved)
            _ -> Left (ParetoInputLimit "missing test result")
  , testCase "internal vector-search record lenses obey the lens laws" $ do
      let cost = V.fromList [1, 2]
          other = V.fromList [3, 4]
          solverNode = P.Node 1 (IS.singleton 2) cost
          entry = P.Entry (IM.singleton 1 2) cost
          result = P.Result [entry] P.Exhausted 3 [cost] True True
      assertEqual "record lenses" True (and
        [ laws P.nodeIdL solverNode 2 3
        , laws P.nodeChildrenL solverNode IS.empty (IS.singleton 3)
        , laws P.nodeCostL solverNode other cost
        , laws P.entrySelectionL entry IM.empty (IM.singleton 3 4)
        , laws P.entryCostL entry other cost
        , laws P.resultEntriesL result [] [entry]
        , laws P.resultTerminationL result P.WorkLimit P.ArchiveLimit
        , laws P.resultExpandedL result 0 7
        , laws P.resultBoundsL result [] [other]
        , laws P.resultWitnessesCompleteL result False True
        , laws P.resultVectorsCompleteL result False True
        ])
      let endings = [P.Exhausted, P.WorkLimit, P.ArchiveLimit]
      prismLaws P._Exhausted [()] endings
      prismLaws P._WorkLimit [()] endings
      prismLaws P._ArchiveLimit [()] endings
  , testCase "optimality prisms obey matching and round-trip laws" $ do
      prismLaws O._Optimal [()] [Optimal, Unproven]
      prismLaws O._Unproven [()] [Optimal, Unproven]
  , both "DAG result lenses compose with read-only node getters"
      (Just (2, 2, Optimal, True, Just (), 1, "(7 + 7)", "add")) $ Program $ do
        root <- addTerm (plus (number 7) (number 7))
        result <- extractDagOptimal (defaultDagLimits & O.expansionLimit .~ Nothing) dagSizeCost root
        pure (fmap (\r ->
          ( r ^. O.dagCost, r ^. O.lowerBound, r ^. O.optimality
          , r ^. O.expansions == expansions r, r ^? O.optimality . O._Optimal
          , r ^. O.dag . O.dagNodeId, pretty (dagToTerm (r ^. O.dag))
          , operatorKey (ffmap (const (Const ())) (r ^. O.dag . O.dagNode))
          )) result)
  , testCase "term iso composes with a language prism and round-trips" $ do
      let input = plus (number 2) (number 3)
          rebuilt = review O._Term (view O._Term input)
          layer = Lit 7 :: Math 'Number (Term Math)
      assertEqual "term round trip" (pretty input) (pretty rebuilt)
      assertEqual "layer round trip" (Just 7)
        (preview literal (view O._Term (review O._Term layer)))
      assertEqual "inverse iso" "7" (pretty (layer ^. from O._Term))
      assertEqual "composed modification" "8"
        (pretty (number 7 & O._Term . literal %~ (+ 1)))
      assertEqual "nonmatching prism leaves compound intact" (pretty input)
        (pretty (over (O._Term . literal) (+ 1) input))
  , testCase "pattern prisms match only their constructor and preserve sorts" $ do
      let numeric = review O._Var "n" :: Pattern Math 'Number
          booleanPat = review O._Var "b" :: Pattern Math 'Boolean
          branch = review O._Node (Add numeric numeric)
          isNode p = fmap (operatorKey . ffmap (const (Const ()))) (preview O._Node p)
      assertEqual "numeric review-preview" (Just "n") (numeric ^? O._Var)
      assertEqual "boolean review-preview" (Just "b") (booleanPat ^? O._Var)
      assertEqual "node review-preview" (Just "add") (isNode branch)
      assertEqual "node is not a variable" Nothing (branch ^? O._Var)
      assertEqual "variable is not a node" Nothing (isNode numeric)
      assertEqual "variable edit" (Just "renamed")
        (preview O._Var (numeric & O._Var .~ "renamed"))
      assertEqual "unmatched variable edit" (Just "add")
        (isNode (branch & O._Var .~ "renamed"))
      assertEqual "unmatched node edit" (Just "n")
        (preview O._Var (numeric & O._Node .~ Lit 99))
  , testCase "stop-reason prisms satisfy round-trip and matching laws" $ do
      let reasons = [Saturated, IterationLimit, NodeLimit, MatchLimit]
      prismLaws O._Saturated [()] reasons
      prismLaws O._IterationLimit [()] reasons
      prismLaws O._NodeLimit [()] reasons
      prismLaws O._MatchLimit [()] reasons
  , testCase "error prisms preserve payloads and unmatched errors" $ do
      let graphErrors = [DirtyGraph, UnknownClass 5, SortMismatch 7,
                         InvalidLimits, InvalidBinding "x", CostOverflow, InvalidExtraction]
          ruleErrors = [UnboundVariable "x", VariableSortMismatch "y"]
      prismLaws O._DirtyGraph [()] graphErrors
      prismLaws O._UnknownClass [0, 2, maxBound] graphErrors
      prismLaws O._SortMismatch [0, 2, maxBound] graphErrors
      prismLaws O._InvalidLimits [()] graphErrors
      prismLaws O._InvalidBinding ["", "x"] graphErrors
      prismLaws O._CostOverflow [()] graphErrors
      prismLaws O._InvalidExtraction [()] graphErrors
      prismLaws O._UnboundVariable ["", "x"] ruleErrors
      prismLaws O._VariableSortMismatch ["", "x"] ruleErrors
  , both "limit lenses configure the runner and result lenses compose with prisms"
      (IterationLimit, 0, Just (), Just 1) $ Program $ do
        root <- add (Lit 7)
        let limits = defaultLimits & O.iterationLimit .~ 0
                                   & O.nodeLimit .~ 20
                                   & O.matchLimit .~ 10
        result <- run limits []
        best <- extractWith astSize root
        pure (result ^. O.stopReason, result ^. O.iterations,
              result ^? O.stopReason . O._IterationLimit, best ^? _Just . _1)
  , testCase "editing limits retains the runner's validation" $ do
      let action :: Backend b m => EGraphT b r Math m ()
          action = void (run (defaultLimits & O.iterationLimit .~ (-1)) [])
      assertEqual "mutable" (Left InvalidLimits) (runMutable action)
      assertEqual "persistent" (Left InvalidLimits) (runPersistent action)
  , case arithmeticRules of
      Left err -> testCase "rule construction" (assertFailure (show err))
      Right [] -> testCase "rule construction" (assertFailure "expected arithmetic rules")
      Right (rule : _) -> testGroup "rewrite names"
        [ QC.testProperty "name lens obeys the lens laws" $ \x y ->
            let named = set O.rewriteName x rule
            in QC.conjoin
                 [ view O.rewriteName named QC.=== x
                 , view O.rewriteName (set O.rewriteName y named) QC.=== y
                 , view O.rewriteName (set O.rewriteName (view O.rewriteName rule) rule)
                     QC.=== view O.rewriteName rule
                 ]
        , both "renaming preserves the validated rule's behavior" (Just (1, "7")) $ Program $ do
            root <- addTerm (plus (number 7) (number 0))
            void (run defaultLimits [rule & O.rewriteName .~ "renamed-add-zero"])
            fmap (\(c, t) -> (c, pretty t)) <$> extract root
        ]
  ]

lensLaws :: (Eq s, Show s, Eq a, Show a) => Lens' s a -> s -> a -> a -> QC.Property
lensLaws optic source x y = QC.conjoin
  [ view optic (set optic x source) QC.=== x
  , set optic (view optic source) source QC.=== source
  , set optic y (set optic x source) QC.=== set optic y source
  ]

laws :: (Eq s, Eq a) => Lens' s a -> s -> a -> a -> Bool
laws optic source x y = and
  [ view optic (set optic x source) == x
  , set optic (view optic source) source == source
  , set optic y (set optic x source) == set optic y source
  ]

prismLaws :: (Eq s, Show s, Eq a, Show a) => Prism' s a -> [a] -> [s] -> Assertion
prismLaws optic values sources = do
  forM_ values $ \a -> assertEqual "review-preview" (Just a) (preview optic (review optic a))
  forM_ sources $ \s -> case preview optic s of
    Just a -> assertEqual "matched reconstruction" s (review optic a)
    Nothing -> forM_ values $ \a -> assertEqual "unmatched setter" s (set optic a s)
