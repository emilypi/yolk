-- |
-- Module       : BenchReport
-- Description  : CSV comparison and descriptive statistics for yolk benchmarks.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Descriptive raw mean comparisons of tasty-bench CSV exports. Workloads
-- receive equal weight; their absolute timings and timing deviations are not
-- pooled. These ratios are distinct from tasty-bench's noise-aware timing gate.
module BenchReport
  ( BenchmarkTable
  , WorkloadComparison
  , benchmarkName
  , baselineMeanPs
  , currentMeanPs
  , runtimeRatio
  , ComparisonReport
  , workloadComparisons
  , geometricMeanRatio
  , logRatioStdDev
  , parseBenchmarkCsv
  , compareBenchmarks
  , compareCsv
  , renderReport
  ) where

import Control.Monad (foldM, unless, when)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as U
import Statistics.Sample qualified as Statistics
import Text.Printf (printf)
import Text.Read (readMaybe)

-- The constructor is private: every table is nonempty and has unique,
-- nonblank names and finite, positive means. Timing deviations are validated
-- on input but are not used as samples in the cross-workload summary.
newtype BenchmarkTable = BenchmarkTable (Map.Map String Double)
  deriving (Eq, Show)

data WorkloadComparison = WorkloadComparison
  { benchmarkName :: !String
  , baselineMeanPs :: !Double
  , currentMeanPs :: !Double
  , runtimeRatio :: !Double
  -- ^ Current / baseline. Greater than one means slower. Extreme valid
  -- inputs can exceed Double's range; rendering then uses the log ratio.
  , logRuntimeRatio :: !Double
  }
  deriving (Eq, Show)

data ComparisonReport = ComparisonReport
  { workloadComparisons :: ![WorkloadComparison]
  , geometricMeanRatio :: !Double
  -- ^ exp(mean(log(current / baseline))), giving each workload equal weight.
  -- As with 'runtimeRatio', an extreme result can exceed Double's range.
  , meanLogRatio :: !Double
  , logRatioStdDev :: !Double
  -- ^ Sample standard deviation of the natural log ratios across workloads
  -- (denominator n - 1). Zero for one workload, following statistics.
  -- This is neither a confidence interval nor repeated timing samples.
  }
  deriving (Eq, Show)

-- | Parse a tasty-bench CSV table. The required columns are Name, Mean (ps),
-- and 2*Stdev (ps); column order is immaterial and extra columns are ignored.
-- LF and CRLF records, quoted fields, embedded newlines, and doubled quotes
-- are supported. Blank records, ragged rows, and ambiguous headers fail.
parseBenchmarkCsv :: String -> Either String BenchmarkTable
parseBenchmarkCsv input = do
  rows <- parseCsv (dropBom input)
  case rows of
    [] -> Left "empty CSV table"
    headers : entries -> do
      when (any (all isSpace) headers) (Left "blank column name")
      case firstDuplicate headers of
        Just name -> Left ("duplicate column name " ++ show name)
        Nothing -> pure ()
      mapM_ (requireColumn headers) ["Name", "Mean (ps)", "2*Stdev (ps)"]
      when (null entries) (Left "CSV table has no benchmark rows")
      BenchmarkTable <$> foldM (addRow headers) Map.empty (zip [2 :: Int ..] entries)
 where
  dropBom ('\xfeff' : rest) = rest
  dropBom text = text

  requireColumn headers name =
    unless (name `elem` headers) (Left ("missing column " ++ show name))

  addRow headers table (rowNumber, fields) = contextualize
    ("CSV record " ++ show rowNumber ++ ": ")
    $ do
      unless (length fields == length headers) $
        Left ("expected " ++ show (length headers) ++ " fields, got \
          \" ++ show (length fields))
      let cells = zip headers fields
          field name = maybe (Left ("missing column \
            \" ++ show name)) Right (lookup name cells)
      name <- field "Name"
      when (all isSpace name) (Left "missing benchmark name")
      when (Map.member name table) (Left ("duplicate benchmark name " ++ show name))
      mean <- field "Mean (ps)" >>= readNumber "Mean (ps)" (> 0) "positive"
      _ <- field "2*Stdev (ps)" >>= readNumber "2*Stdev (ps)" (>= 0) "nonnegative"
      pure (Map.insert name mean table)

readNumber :: String -> (Double -> Bool) -> String -> String -> Either String Double
readNumber column valid range cell = case readMaybe cell of
  Just value | finite value && valid value -> Right value
  _ -> Left (column ++ " must be finite and " ++ range ++ "; got " ++ show cell)

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
 where
  go _ [] = Nothing
  go seen (value : rest)
    | Set.member value seen = Just value
    | otherwise = go (Set.insert value seen) rest

contextualize :: String -> Either String a -> Either String a
contextualize prefix = either (Left . (prefix ++)) Right

-- | Compare matching names, in ascending name order. A slowdown is data,
-- never an error. Missing names on either side make the comparison invalid.
compareBenchmarks :: BenchmarkTable -> BenchmarkTable -> Either String ComparisonReport
compareBenchmarks (BenchmarkTable baseline) (BenchmarkTable current) = do
  let missingCurrent = Map.keysSet baseline `Set.difference` Map.keysSet current
      missingBaseline = Map.keysSet current `Set.difference` Map.keysSet baseline
      missing label names =
        [label ++ intercalate ", \
          \" (map show (Set.toAscList names)) | not (Set.null names)]
  unless (Set.null missingCurrent && Set.null missingBaseline) $
    Left $
      intercalate
        "; "
        ( missing "missing from current: " missingCurrent
            ++ missing "missing from baseline: " missingBaseline
        )
  let workloads = Map.elems (Map.intersectionWithKey compareTiming baseline current)
      logs = U.fromList (map logRuntimeRatio workloads)
      average = Statistics.mean logs
  pure
    ComparisonReport
      { workloadComparisons = workloads
      , geometricMeanRatio = exp average
      , meanLogRatio = average
      , logRatioStdDev = Statistics.stdDev logs
      }
 where
  compareTiming name old new =
    let ratio = new / old
        -- Division may overflow or underflow for otherwise valid means.
        -- Subtracting finite logs preserves the descriptive summary then.
        logRatio = if finite ratio && ratio > 0 then log ratio else log new - log old
     in WorkloadComparison name old new ratio logRatio

-- | Parse and compare two CSV documents, with baseline/current error context.
compareCsv :: String -> String -> Either String ComparisonReport
compareCsv baseline current = do
  old <- contextualize "baseline: " (parseBenchmarkCsv baseline)
  new <- contextualize "current: " (parseBenchmarkCsv current)
  compareBenchmarks old new

-- | Plain text; quoted names keep embedded newlines and control characters
-- from changing the layout. No timing threshold or significance is inferred.
renderReport :: ComparisonReport -> String
renderReport report =
  unlines $
    ["Raw mean runtime ratios (current / baseline; >1 means slower):"]
      ++ map renderWorkload (workloadComparisons report)
      ++ [ "Geometric mean of raw mean ratios ("
            ++ show (length (workloadComparisons report))
            ++ " workloads): "
            ++ renderRatio (geometricMeanRatio report) (meanLogRatio report)
         , printf
            "Stddev of log ratios across workloads (n-1; singleton = 0): %.6f"
            (logRatioStdDev report)
         , "Descriptive workload dispersion only; not a confidence interval or repeated \
           \timing samples."
         , "Separate noise-aware timing gate: tasty-bench --baseline BASELINE.csv \
           \--fail-if-slower 10"
         ]
 where
  renderWorkload workload =
    "  "
      ++ show (benchmarkName workload)
      ++ ": "
      ++ renderRatio (runtimeRatio workload) (logRuntimeRatio workload)
      ++ " ("
      ++ direction (runtimeRatio workload)
      ++ ")"

  direction ratio
    | ratio > 1 = "slower"
    | ratio < 1 = "faster"
    | otherwise = "unchanged"

renderRatio :: Double -> Double -> String
renderRatio ratio logRatio
  | not (finite ratio) || ratio <= 0 =
      printf "exp(%.6f)x [outside Double ratio range]" logRatio
  | ratio < 0.001 || ratio >= 10000 = printf "%.6ex" ratio
  | otherwise = printf "%.6fx" ratio

-- A deterministic CSV parser. Quoting is only allowed at the start of a
-- field; closing quotes must be followed by a separator or end of input.
-- Record numbers count logical records, including fields spanning lines.
parseCsv :: String -> Either String [[String]]
parseCsv = records 1
 where
  records :: Int -> String -> Either String [[String]]
  records _ [] = Right []
  records row text = do
    (fields, rest) <- record row [] text
    (fields :) <$> records (row + 1) rest

  record row fields text = do
    (cell, rest) <- field row text
    case rest of
      [] -> Right (reverse (cell : fields), [])
      ',' : more -> record row (cell : fields) more
      '\n' : more -> Right (reverse (cell : fields), more)
      '\r' : '\n' : more -> Right (reverse (cell : fields), more)
      '\r' : _ -> bad row "bare carriage return outside a quoted field"
      _ -> bad row "unexpected character after closing quote"

  field row ('"' : rest) = quoted row [] rest
  field row text = unquoted row [] text

  unquoted _ acc [] = Right (reverse acc, [])
  unquoted row acc text@(character : rest)
    | character `elem` ",\r\n" = Right (reverse acc, text)
    | character == '"' = bad row "quote inside an unquoted field"
    | otherwise = unquoted row (character : acc) rest

  quoted row _ [] = bad row "unterminated quoted field"
  quoted row acc ('"' : '"' : rest) = quoted row ('"' : acc) rest
  quoted _ acc ('"' : rest) = Right (reverse acc, rest)
  quoted row acc (character : rest) = quoted row (character : acc) rest

  bad row message = Left ("CSV record " ++ show row ++ ": " ++ message)
