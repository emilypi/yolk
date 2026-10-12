{-# LANGUAGE RankNTypes #-}

module TestSupport (Program (..), both, assertRight, observeProperty) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC
import TestLanguage (Math)
import Yolk

-- Interpret the same public graph program with both actual library backends.
newtype Program a = Program
  (forall b r m. Backend b m => EGraphT b r Math m a)

both :: (Eq a, Show a) => String -> a -> Program a -> TestTree
both label expected (Program action) = testGroup label
  [ testCase "mutable" $ assertRight expected (runMutable action)
  , testCase "persistent" $ assertRight expected (runPersistent action)
  ]

assertRight :: (Eq a, Show a) => a -> Either EGraphError a -> Assertion
assertRight expected = \case
  Left err -> assertFailure ("unexpected graph error: " ++ show err)
  Right actual -> assertEqual "observable graph result" expected actual

observeProperty :: (Eq a, Show a) => String -> a -> Either EGraphError a -> QC.Property
observeProperty backend expected = \case
  Left err -> QC.counterexample (backend ++ ": " ++ show err) False
  Right actual -> QC.counterexample backend (actual QC.=== expected)
