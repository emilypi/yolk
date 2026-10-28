{-# LANGUAGE RankNTypes #-}

-- |
-- Module       : Main
-- Description  : Tests and properties for the yolk public API.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module Main (main) where

import Control.Monad (forM, forM_, void)
import Data.Functor.Const (Const (..))
import Data.HKD (ffmap)
import Data.List (nub, sort)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC
import ExtractionTests (customExtractionTests)
import DagExtractionTests (dagExtractionTests)
import ParetoExtractionTests (paretoExtractionTests)
import BenchmarkTests (benchmarkTests)
import BenchmarkReportTests (benchmarkReportTests)
import OpticsTests (opticsTests)
import TestLanguage
import TestSupport
import Yolk

main :: IO ()
main = defaultMain $ localOption (QC.QuickCheckTests 200) $ testGroup "yolk"
  [ coreTests
  , pruningTests
  , ruleTests
  , extractionTests
  , customExtractionTests
  , dagExtractionTests
  , paretoExtractionTests
  , benchmarkTests
  , benchmarkReportTests
  , opticsTests
  , propertyTests
  ]

dirtyBoth :: String -> Program () -> TestTree
dirtyBoth label (Program action) = testGroup label
  [ testCase "mutable" $ check (runMutable action)
  , testCase "persistent" $ check (runPersistent action)
  ]
  where
    check = \case
      Left DirtyGraph -> pure ()
      Left err -> assertFailure ("expected DirtyGraph, got " ++ show err)
      Right () -> assertFailure "a dirty graph was observed without rebuilding"

keys :: [Math s f] -> [String]
keys = sort . map (operatorKey . ffmap (const (Const ())))

extracted :: Maybe (Int, Term Math s) -> Maybe (Int, Int, String)
extracted = fmap (\(cost, tree) -> (cost, termSize tree, pretty tree))

coreTests :: TestTree
coreTests = testGroup "interning and congruence"
  [ both "empty graph" 0 $ Program nodeCount
  , both "payload, constructor and child order all participate in identity"
      (True, False, False, False, 5) $ Program $ do
        one <- add (Lit 1)
        two <- add (Lit 2)
        again <- add (Lit 1)
        sum12 <- add (Add one two)
        sum21 <- add (Add two one)
        product12 <- add (Mul one two)
        same <- equivalent one again
        payloadDistinct <- equivalent one two
        ordered <- equivalent sum12 sum21
        constructors <- equivalent sum12 product12
        count <- nodeCount
        pure (same, payloadDistinct, ordered, constructors, count)
  , both "addTerm shares existing recursive subterms" (True, True, 3) $ Program $ do
        zero <- add (Lit 0)
        seven <- add (Lit 7)
        manual <- add (Add seven zero)
        recursive <- addTerm (plus (number 7) (number 0))
        repeated <- addTerm (plus (number 7) (number 0))
        same <- equivalent manual recursive
        sameAgain <- equivalent recursive repeated
        count <- nodeCount
        pure (same, sameAgain, count)
  , both "identical literal keys at distinct sorts remain separate"
      (4, True, True, False, True, True) $ Program $ do
        zero <- add (Lit 0)
        one <- add (Lit 1)
        falseId <- add (BoolLit False)
        trueId <- add (BoolLit True)
        rebuild
        count <- nodeCount
        numeric <- classNodes one
        booleanNodes <- classNodes trueId
        void (union zero one)
        rebuild
        booleansMerged <- equivalent falseId trueId
        numericLookup <- lookupNode (Lit 1) >>= maybe (pure False) (equivalent zero)
        boolLookup <- lookupNode (BoolLit True) >>= maybe (pure False) (equivalent trueId)
        pure (count, all isOne numeric && not (null numeric),
              all isTrue booleanNodes && not (null booleanNodes),
              booleansMerged, numericLookup, boolLookup)
  , both "union reports change once and recognizes stale handles"
      (True, False, False, True) $ Program $ do
        a <- add (Lit 8)
        b <- add (Lit 9)
        changed <- union a b
        unchanged <- union b a
        reflexive <- union b b
        same <- equivalent a b
        pure (changed, unchanged, reflexive, same)
  , both "canonical interning follows stale children after rebuild"
      (True, True, True) $ Program $ do
        a <- add (Lit 2)
        b <- add (Lit 3)
        c <- add (Lit 4)
        parent <- add (Add b c)
        void (union a b)
        rebuild
        before <- nodeCount
        canonical <- add (Add a c)
        stale <- add (Add b c)
        rebuild
        after <- nodeCount
        same <- equivalent parent canonical
        staleSame <- equivalent parent stale
        pure (same, staleSame, before == after)
  , both "rebuild propagates through several levels and across result sorts"
      (True, True, True, False) $ Program $ do
        a <- add (Lit 10)
        b <- add (Lit 20)
        c <- add (Lit 30)
        pa <- add (Add a c)
        pb <- add (Add b c)
        ga <- add (Mul pa c)
        gb <- add (Mul pb c)
        ba <- add (Equal ga c)
        bb <- add (Equal gb c)
        unrelated <- add (Equal c c)
        void (union a b)
        rebuild
        (,,,) <$> equivalent pa pb <*> equivalent ga gb
          <*> equivalent ba bb <*> equivalent ba unrelated
  , both "rebuilding twice preserves observable members" True $ Program $ do
        a <- add (Lit 10)
        b <- add (Lit 20)
        pa <- add (Add a a)
        pb <- add (Add b b)
        void (union a b)
        rebuild
        before <- keys <$> classNodes pa
        countBefore <- nodeCount
        rebuild
        after <- keys <$> classNodes pb
        countAfter <- nodeCount
        pure (before == after && countBefore == countAfter)
  , both "lookup returns Nothing for an absent node" True $ Program $ do
        void (add (Lit 1))
        rebuild
        result <- lookupNode (Lit 99)
        pure $ case result of
          Nothing -> True
          Just _ -> False
  , dirtyBoth "classNodes rejects observations after union" $ Program $ do
        a <- add (Lit 1)
        b <- add (Lit 2)
        void (union a b)
        void (classNodes a)
  , dirtyBoth "lookupNode rejects observations after union" $ Program $ do
        a <- add (Lit 1)
        b <- add (Lit 2)
        void (union a b)
        void (lookupNode (Lit 1))
  ]
  where
    isOne :: Math 'Number f -> Bool
    isOne (Lit 1) = True
    isOne _ = False
    isTrue :: Math 'Boolean f -> Bool
    isTrue (BoolLit True) = True
    isTrue _ = False

pruningTests :: TestTree
pruningTests = testGroup "pruning and immutable snapshots"
  [ both "exact reinterning finds a hidden node without resurrecting it"
      (True, True, ["literal:7"], ["literal:7"], True) $ Program $ do
        seven <- add (Lit 7)
        zero <- add (Lit 0)
        expression <- add (Add seven zero)
        void (union seven expression)
        rebuild
        hideNonLeaves expression
        rebuild
        before <- keys <$> classNodes expression
        countBefore <- nodeCount
        interned <- add (Add seven zero)
        rebuild
        found <- lookupNode (Add seven zero) >>= maybe (pure False) (equivalent seven)
        after <- keys <$> classNodes expression
        countAfter <- nodeCount
        same <- equivalent interned expression
        pure (same, found, before, after, countBefore == countAfter)
  , both "a hidden parent still participates in later congruence"
      (True, True) $ Program $ do
        a <- add (Lit 2)
        b <- add (Lit 3)
        zero <- add (Lit 0)
        hidden <- add (Add a zero)
        visible <- add (Add b zero)
        rebuild
        hideNonLeaves hidden
        void (union a b)
        rebuild
        same <- equivalent hidden visible
        members <- keys <$> classNodes visible
        -- A visible collision partner must survive until pruning runs again.
        pure (same, "add" `elem` members)
  , both "snapshot retains separate classes and members after union and prune"
      (["add"], ["literal:7"], ["literal:7"], ["add"], ["literal:7"]) $ Program $ do
        seven <- add (Lit 7)
        zero <- add (Lit 0)
        expression <- add (Add seven zero)
        rebuild
        saved <- freeze
        void (union expression seven)
        rebuild
        hideNonLeaves expression
        rebuild
        current <- keys <$> classNodes expression
        later <- freeze
        void (add (Lit 100))
        rebuild
        -- Inspect saved snapshots only after all mutations, also checking that
        -- lazy observations do not close over mutable backend storage.
        pure (keys (snapshotNodes expression saved),
              keys (snapshotNodes seven saved), current,
              keys (snapshotNodes expression saved),
              keys (snapshotNodes expression later))
  ]

ruleTests :: TestTree
ruleTests = testGroup "rewrite validation and saturation"
  [ testCase "reject an unbound right-hand-side variable" $
      rejected (UnboundVariable "absent")
        (rewrite "unbound" (node (Add x (node (Lit 0)))) (var "absent"))
  , testCase "reject a variable used at two sorts on the left" $
      rejected (VariableSortMismatch "x")
        (rewrite "conflict-left" (node (If (var "x") x (node (Lit 0)))) x)
  , testCase "reject a variable whose sort changes on the right" $
      rejected (VariableSortMismatch "x")
        (rewrite "conflict-right" x (node (If (var "x") x (node (Lit 0)))))
  , testCase "accept a well-sorted repeated variable" $
      case rewrite "same-operands" (node (Add x x)) x of
        Left err -> assertFailure (show err)
        Right _ -> pure ()
  , withRule "equal-reflexive" (node (Equal x x)) (node (BoolLit True)) $ \rule ->
      both "repeated variables require equal classes and notice later unions"
        (Saturated, True, False, Saturated, True) $ Program $ do
          one <- add (Lit 1)
          two <- add (Lit 2)
          sameOperands <- add (Equal one one)
          differentOperands <- add (Equal one two)
          trueId <- add (BoolLit True)
          first <- run defaultLimits [rule]
          matchedSame <- equivalent sameOperands trueId
          matchedDifferent <- equivalent differentOperands trueId
          void (union one two)
          second <- run defaultLimits [rule]
          matchedAfterUnion <- equivalent differentOperands trueId
          pure (stopReason first, matchedSame, matchedDifferent,
                stopReason second, matchedAfterUnion)
  , withRule "one-to-two" (node (Lit 1)) (node (Lit 2)) $ \rule ->
      both "a numeric literal rule cannot match the same key at Boolean sort"
        (True, ["literal:1"], Just (1, 1, "true")) $ Program $ do
          one <- add (Lit 1)
          two <- add (Lit 2)
          trueId <- add (BoolLit True)
          void (run defaultLimits [rule])
          changed <- equivalent one two
          boolMembers <- keys <$> classNodes trueId
          witness <- extracted <$> extract trueId
          pure (changed, boolMembers, witness)
  , withRule "detect-hidden-add" (node (Add x (node (Lit 0)))) (node (Lit 99)) $ \rule ->
      both "constructor matching does not consult hidden intern entries"
        (Saturated, False, ["literal:7"]) $ Program $ do
          seven <- add (Lit 7)
          zero <- add (Lit 0)
          sentinel <- add (Lit 99)
          compound <- add (Add seven zero)
          void (union seven compound)
          rebuild
          hideNonLeaves seven
          result <- run defaultLimits [rule]
          matched <- equivalent seven sentinel
          members <- keys <$> classNodes seven
          pure (stopReason result, matched, members)
  , withRules $ \rules -> both "arithmetic rules converge through a Boolean parent"
      (Saturated, True, Just (1, 1, "7"), Just (3, 3, "(7 == 7)"), Saturated, True) $ Program $ do
        expression <- addTerm (times (plus (number 7) (number 0)) (number 1))
        comparison <- addTerm (equal (times (plus (number 7) (number 0)) (number 1)) (number 7))
        seven <- add (Lit 7)
        expectedComparison <- add (Equal seven seven)
        result <- run defaultLimits rules
        same <- equivalent expression seven
        numericCost <- extracted <$> extract expression
        booleanCost <- extracted <$> extract comparison
        comparisonSame <- equivalent comparison expectedComparison
        again <- run defaultLimits rules
        pure (stopReason result, same, numericCost, booleanCost,
              stopReason again, comparisonSame)
  , withRule "if-true" (node (If (node (BoolLit True)) x (var "else"))) x $ \chooseTrue ->
      withRules $ \rules -> both "conditional matching visits Boolean and numeric children"
        (Saturated, True, Just 1) $ Program $ do
          root <- addTerm (ifNumber (boolean True) (plus (number 7) (number 0)) (number 99))
          seven <- add (Lit 7)
          result <- run defaultLimits (chooseTrue : rules)
          same <- equivalent root seven
          cost <- fmap fst <$> extract root
          pure (stopReason result, same, cost)
  , withRules $ \rules -> both "zero iterations reports the iteration limit"
      IterationLimit $ Program $ do
        void (addTerm (plus (number 7) (number 0)))
        stopReason <$> run defaultLimits {iterationLimit = 0} rules
  , withRules $ \rules -> both "an existing graph beyond budget reports the node limit"
      NodeLimit $ Program $ do
        void (addTerm (plus (number 7) (number 0)))
        stopReason <$> run defaultLimits {nodeLimit = 1} rules
  , withRules $ \rules -> both "zero match budget reports the match limit"
      MatchLimit $ Program $ do
        void (addTerm (plus (number 7) (number 0)))
        stopReason <$> run defaultLimits {matchLimit = 0} rules
  , both "an empty rule set saturates" Saturated $ Program $ do
        void (add (Lit 7))
        stopReason <$> run defaultLimits []
  ]
  where
    x :: Pattern Math 'Number
    x = var "x"
    rejected :: RuleError -> Either RuleError (Rewrite Math) -> Assertion
    rejected expected (Left actual) = assertEqual "rule validation error" expected actual
    rejected _ (Right _) = assertFailure "an invalid rewrite was accepted"

withRules :: ([Rewrite Math] -> TestTree) -> TestTree
withRules use = case arithmeticRules of
  Left err -> testCase "arithmetic rule construction" (assertFailure (show err))
  Right rules -> use rules

withRule :: String -> Pattern Math s -> Pattern Math s -> (Rewrite Math -> TestTree) -> TestTree
withRule name lhs rhs use = case rewrite name lhs rhs of
  Left err -> testCase (name ++ " rule construction") (assertFailure (show err))
  Right rule -> use rule

extractionTests :: TestTree
extractionTests = testGroup "minimum AST-size extraction"
  [ both "shared children are counted once per AST occurrence"
      (Just (7, 7, "((2 + 3) * (2 + 3))")) $ Program $ do
        root <- addTerm (times (plus (number 2) (number 3)) (plus (number 2) (number 3)))
        rebuild
        extracted <$> extract root
  , both "a cyclic class chooses its finite leaf witness" (Just (1, 1, "7"), True) $ Program $ do
        seven <- add (Lit 7)
        zero <- add (Lit 0)
        cycleNode <- add (Add seven zero)
        void (union seven cycleNode)
        rebuild
        best <- extract cycleNode
        case best of
          Nothing -> pure (Nothing, False)
          Just (cost, witness) -> do
            witnessId <- addTerm witness
            same <- equivalent seven witnessId
            pure (extracted (Just (cost, witness)), same)
  , both "mutually recursive classes retain their minimum finite witnesses"
      (Just 1, Just 3) $ Program $ do
        seven <- add (Lit 7)
        one <- add (Lit 1)
        a <- add (Add seven one)
        b <- add (Mul a one)
        void (union seven b)
        rebuild
        (,) <$> (fmap fst <$> extract b) <*> (fmap fst <$> extract a)
  , both "a class with only hidden members has no extractable term" Nothing $ Program $ do
        root <- addTerm (plus (number 2) (number 3))
        rebuild
        hideNonLeaves root
        rebuild
        fmap fst <$> extract root
  , both "a visible cycle without a finite derivation returns Nothing" Nothing $ Program $ do
        base <- addTerm (plus (number 2) (number 3))
        one <- add (Lit 1)
        recursive <- add (Mul base one)
        rebuild
        -- Remove the finite compound witness before closing the cycle. The
        -- remaining visible member is Mul self one, with no finite tree.
        hideNonLeaves base
        void (union base recursive)
        fmap fst <$> extract recursive
  , both "an unavailable child makes its visible ancestor unextractable" Nothing $ Program $ do
        child <- addTerm (plus (number 2) (number 3))
        one <- add (Lit 1)
        parent <- add (Mul child one)
        rebuild
        hideNonLeaves child
        rebuild
        fmap fst <$> extract parent
  ]

-- A small undirected-equation oracle. Reachability is computed directly;
-- it contains no e-graph, union-find, canonical representative or rebuild.
newtype Equations = Equations [(Int, Int)]
  deriving stock Show

instance QC.Arbitrary Equations where
  arbitrary = do
    count <- QC.chooseInt (0, 10)
    Equations <$> QC.vectorOf count ((,) <$> QC.chooseInt (0, 5) <*> QC.chooseInt (0, 5))
  shrink (Equations equations) =
    [ Equations smaller
    | smaller <- QC.shrinkList QC.shrink equations
    , all (\(a, b) -> a >= 0 && a <= 5 && b >= 0 && b <= 5) smaller
    ]

reachable :: [(Int, Int)] -> Int -> Int -> Bool
reachable equations start target = visit [] [start]
  where
    visit _ [] = False
    visit seen (current : pending)
      | current == target = True
      | current `elem` seen = visit seen pending
      | otherwise = visit (current : seen) (neighbors current ++ pending)
    neighbors current =
      [ b | (a, b) <- equations, a == current ] ++
      [ a | (a, b) <- equations, b == current ]

congruenceProgram :: Equations -> Program [Bool]
congruenceProgram (Equations equations) = Program $ do
  leaves <- forM ([0 .. 5] :: [Int]) $ \n -> add (Lit (toInteger n))
  zero <- add (Lit 0)
  one <- add (Lit 1)
  parents <- traverse (\child -> add (Add child zero)) leaves
  grandparents <- traverse (\child -> add (Mul child one)) parents
  predicates <- traverse (\child -> add (Equal child one)) grandparents
  let indexed = zip [0 ..] leaves
  forM_ equations $ \(a, b) ->
    forM_ [(left, right) | (i, left) <- indexed, (j, right) <- indexed, i == a, j == b] $
      \(left, right) -> void (union left right)
  rebuild
  leafResults <- matrix leaves
  parentResults <- matrix parents
  grandparentResults <- matrix grandparents
  predicateResults <- matrix predicates
  pure (leafResults ++ parentResults ++ grandparentResults ++ predicateResults)
  where
    matrix :: Backend b m => [ClassId r s] -> EGraphT b r Math m [Bool]
    matrix classes = sequence [equivalent a b | a <- classes, b <- classes]

propertyTests :: TestTree
propertyTests = testGroup "properties against independent observations"
  [ QC.testProperty "both backends agree with equation reachability through typed contexts" $
      \equations@(Equations pairs) ->
        let expected = concat (replicate 4 [reachable pairs a b | a <- [0 .. 5], b <- [0 .. 5]])
            Program action = congruenceProgram equations
         in QC.conjoin
              [ observeProperty "mutable" expected (runMutable action)
              , observeProperty "persistent" expected (runPersistent action)
              ]
  , QC.testProperty "literal insertion preserves exactly the distinct payloads" $
      \(payloads :: [QC.Small Int]) ->
        let values = map (toInteger . QC.getSmall) payloads
            expected = length (nub values)
            action :: Backend b m => EGraphT b r Math m Int
            action = do
              forM_ (values ++ reverse values) (void . add . Lit)
              rebuild
              nodeCount
         in QC.conjoin
              [ observeProperty "mutable" expected (runMutable action)
              , observeProperty "persistent" expected (runPersistent action)
              ]
  , testCase "reference reachability distinguishes disconnected equations" $ do
      assertBool "transitive path" (reachable [(0, 1), (2, 1)] 0 2)
      assertBool "disconnected node" (not (reachable [(0, 1), (2, 1)] 0 3))
  ]
