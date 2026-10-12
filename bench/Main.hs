module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.Exit (die)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import BenchVectors

main :: IO ()
main = do
  -- Force input descriptions and validate all fixtures outside the timer.
  -- Validation is also part of the regular test suite, so a wrong or missing
  -- witness cannot masquerade as a speedup.
  inputs <- evaluate (force vectors)
  forM_ inputs $ \v -> do
    either die pure (checkOutcome v (runMutableVector v))
    either die pure (checkOutcome v (runPersistentVector v))
  defaultMain
    [ bgroup "mutable" [bench (vectorName v) (nf runMutableVector v) | v <- inputs]
    , bgroup "persistent" [bench (vectorName v) (nf runPersistentVector v) | v <- inputs]
    ]
