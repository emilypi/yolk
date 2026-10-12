module OpticsTests (opticsTests) where

import Control.Lens (Lens', Prism', (&), (.~), (%~), (^.), (^?), _1, _Just, from, over, preview, prism', review, set, view)
import Control.Monad (forM_, void)
import Data.Functor.Const (Const (..))
import Data.HKD (ffmap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC
import TestLanguage
import TestSupport
import Yolk
import qualified Yolk.Optics as O

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
                         InvalidLimits, InvalidBinding "x", CostOverflow]
          ruleErrors = [UnboundVariable "x", VariableSortMismatch "y"]
      prismLaws O._DirtyGraph [()] graphErrors
      prismLaws O._UnknownClass [0, 2, maxBound] graphErrors
      prismLaws O._SortMismatch [0, 2, maxBound] graphErrors
      prismLaws O._InvalidLimits [()] graphErrors
      prismLaws O._InvalidBinding ["", "x"] graphErrors
      prismLaws O._CostOverflow [()] graphErrors
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

prismLaws :: (Eq s, Show s, Eq a, Show a) => Prism' s a -> [a] -> [s] -> Assertion
prismLaws optic values sources = do
  forM_ values $ \a -> assertEqual "review-preview" (Just a) (preview optic (review optic a))
  forM_ sources $ \s -> case preview optic s of
    Just a -> assertEqual "matched reconstruction" s (review optic a)
    Nothing -> forM_ values $ \a -> assertEqual "unmatched setter" s (set optic a s)
