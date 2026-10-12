module Main (main) where

import BenchReport (compareCsv, renderReport)
import Control.Exception (IOException, displayException, try)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [baselinePath, currentPath] -> do
      baseline <- readCsv baselinePath
      current <- readCsv currentPath
      case compareCsv baseline current of
        Left message -> die ("yolk-bench-report: " ++ message)
        Right report -> putStr (renderReport report)
    _ -> die "Usage: yolk-bench-report BASELINE.csv CURRENT.csv"

readCsv :: FilePath -> IO String
readCsv path = do
  -- Strict IO keeps delayed file/decoding errors inside the exception handler.
  result <- try (readFile' path) :: IO (Either IOException String)
  case result of
    Left err -> die ("yolk-bench-report: cannot read " ++ show path ++ ": " ++ displayException err)
    Right contents -> pure contents
