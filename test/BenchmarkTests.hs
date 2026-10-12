module BenchmarkTests (benchmarkTests) where

import Control.Monad (forM_)
import Data.List (nub)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import BenchVectors

benchmarkTests :: TestTree
benchmarkTests = testGroup "benchmark vector regressions"
  [ testCase "names are unique baseline keys" $
      assertEqual "duplicate vector names" (length vectors) (length (nub (map vectorName vectors)))
  , testGroup "closed-form answers on fresh graphs"
      [ testGroup (vectorName v)
          [ testCase label $ either assertFailure pure (checkOutcome v (runner v))
          | (label, runner) <- [("mutable", runMutableVector), ("persistent", runPersistentVector)]
          ]
      | v <- vectors
      ]
  , testCase "guard rejects incorrect costs, incomplete witnesses, and false certificates" $ do
      let exact = Vector "guard-exact" (DagDiamond 1) (Exact 2 2)
          capped = Vector "guard-cap" (DagStar 2 (Just 1)) (Bounded 11 1)
          cases =
            [ (exact, Left "graph failure")
            , (exact, Right NoFiniteDag)
            , (exact, Right (Extracted 1 1 True 0 2))
            , (exact, Right (Extracted 2 2 True 0 1))
            , (capped, Right (Extracted 12 12 True 0 3))
            , (capped, Right (Extracted 11 12 False 1 3))
            , (capped, Right (Extracted 10 9 False 1 3))
            , (capped, Right (Extracted 12 1 False 2 3))
            ]
      forM_ cases $ \(v, result) -> case checkOutcome v result of
        Left _ -> pure ()
        Right () -> assertFailure ("accepted an invalid benchmark result: " ++ show result)
  ]
