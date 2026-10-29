-- |
-- Module       : BenchmarkReportTests
-- Description  : Regression tests for yolk benchmark CSV reports.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module BenchmarkReportTests (benchmarkReportTests) where

import BenchReport
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

benchmarkReportTests :: TestTree
benchmarkReportTests =
  testGroup
    "benchmark CSV report"
    [ testGroup
        "valid CSV fixtures"
        [ testCase label $ withReport csv csv $ \report -> do
          assertEqual
            "decoded names and means"
            expected
            [(benchmarkName row, baselineMeanPs row) | row <- workloadComparisons report]
          forM_ (workloadComparisons report) $ \row -> do
            assertEqual "same current mean" (baselineMeanPs row) (currentMeanPs row)
            assertApprox "same timing ratio" 1 (runtimeRatio row)
          assertApprox "geometric mean" 1 (geometricMeanRatio report)
          assertApprox "zero dispersion" 0 (logRatioStdDev report)
        | (label, csv, expected) <- validCsvFixtures
        ]
    , testGroup
        "invalid CSV fixtures"
        [ testCase label (assertLeftContaining message (parseBenchmarkCsv csv))
        | (label, csv, message) <- invalidCsvFixtures
        ]
    , testGroup
        "invalid means"
        [ testCase value $
          assertLeftContaining
            "Mean (ps) must be finite and positive"
            (parseBenchmarkCsv (header ++ "work," ++ value ++ ",0\n"))
        | value <-
            [ "0"
            , "-0"
            , "-1"
            , "NaN"
            , "Infinity"
            , "-Infinity"
            , "1e400"
            , "-1e400"
            , "1e-400"
            , ""
            , "12ps"
            ]
        ]
    , testGroup
        "invalid timing deviations"
        [ testCase value $
          assertLeftContaining
            "2*Stdev (ps) must be finite and nonnegative"
            (parseBenchmarkCsv (header ++ "work,10," ++ value ++ "\n"))
        | value <- ["-1", "NaN", "Infinity", "-Infinity", "1e400", "-1e400", "", "oops"]
        ]
    , testGroup
        "descriptive ratios"
        [ testCase
            "matches by name, equally weights ratios, and uses sample log dispersion"
            $ withReport baseline current
            $ \report -> do
              assertEqual
                "name order"
                ["large", "small"]
                (map benchmarkName (workloadComparisons report))
              assertEqual
                "per-workload ratios"
                [0.5, 2]
                (map runtimeRatio (workloadComparisons report))
              assertApprox
                "geometric mean, not ratio of pooled absolute times"
                1
                (geometricMeanRatio report)
              assertApprox
                "sample log stddev (n-1 denominator)"
                (sqrt 2 * log 2)
                (logRatioStdDev report)
        , testCase "three known ratios have geometric mean two and log dispersion log two"
            $ withReport
              (header ++ "a,10,0\nb,10,1\nc,10,2\n")
              (header ++ "c,40,1\na,10,2\nb,20,0\n")
            $ \report -> do
              assertEqual
                "ratios"
                [1, 2, 4]
                (map runtimeRatio (workloadComparisons report))
              assertApprox "geometric mean" 2 (geometricMeanRatio report)
              assertApprox "log stddev" (log 2) (logRatioStdDev report)
        , testCase "swapping baseline reverses every ratio and preserves dispersion" $ do
            let old = header ++ "a,10,0\nb,10,0\nc,10,0\n"
                new = header ++ "a,10,0\nb,20,0\nc,40,0\n"
            withReport old new $ \forward ->
              withReport new old $ \backward -> do
                forM_ (zip (workloadComparisons forward) (workloadComparisons backward)) $
                  \(a, b) -> do
                    assertEqual "matching workload" (benchmarkName a) (benchmarkName b)
                    assertApprox "reciprocal ratio" 1 (runtimeRatio a * runtimeRatio b)
                assertApprox
                  "reciprocal geometric mean"
                  1
                  (geometricMeanRatio forward * geometricMeanRatio backward)
                assertApprox
                  "same log dispersion"
                  (logRatioStdDev forward)
                  (logRatioStdDev backward)
        , testCase
            "singleton stddev is zero even when supplied timing deviation is large"
            $ withReport
              ( header
                  ++ "one,10,1000\n"
              )
              (header ++ "one,40,9999\n")
            $ \report -> do
              assertApprox "singleton geometric mean" 4 (geometricMeanRatio report)
              assertEqual "statistics singleton convention" 0 (logRatioStdDev report)
        , testCase "all ratios equal two have zero cross-workload dispersion"
            $ withReport
              (header ++ "a,1,0\nb,1000,0\n")
              (header ++ "a,2,0\nb,2000,0\n")
            $ \report -> do
              assertApprox "geometric mean" 2 (geometricMeanRatio report)
              assertApprox "log stddev" 0 (logRatioStdDev report)
        , testCase
            "timing deviations are validated but do not become summary observations"
            $ withReport baseline current
            $ \original ->
              withReport
                (header ++ "small,1,1e200\nlarge,1000,9999\n")
                (header ++ "large,500,1e250\nsmall,2,1234\n")
                $ \changed ->
                  assertEqual "descriptive comparison unaffected" original changed
        , testCase "extreme finite means retain a finite log summary when ratios overflow"
            $ withReport
              (header ++ "a,1e-300,0\nb,1e300,0\n")
              (header ++ "a,1e300,0\nb,1e-300,0\n")
            $ \report -> do
              assertApprox "canceling log ratios" 1 (geometricMeanRatio report)
              assertApprox
                "log dispersion"
                (sqrt 2 * (log 1e300 - log 1e-300))
                (logRatioStdDev report)
              assertBool
                "range limitation made explicit"
                ("outside Double ratio range" `isInfixOf` renderReport report)
              assertBool "no NaN summary" (not ("NaN" `isInfixOf` renderReport report))
        , testCase "even a large slowdown produces a successful descriptive report" $
            withReport (header ++ "slow,1,0\n") (header ++ "slow,100,0\n") $ \report -> do
              assertApprox "slowdown is data" 100 (geometricMeanRatio report)
              assertBool
                "individual slowdown shown"
                ("100.000000x (slower)" `isInfixOf` renderReport report)
        ]
    , testGroup
        "matching failures"
        [ testCase label (assertLeftContaining message (compareCsv old new))
        | (label, old, new, message) <-
            [
              ( "missing current workload"
              , header ++ "a,1,0\nb,2,0\n"
              , header ++ "a,1,0\n"
              , "missing from current: \"b\""
              )
            ,
              ( "new workload absent from baseline"
              , header ++ "a,1,0\n"
              , header ++ "a,1,0\nb,2,0\n"
              , "missing from baseline: \"b\""
              )
            ,
              ( "same count with different names"
              , header ++ "a,1,0\n"
              , header ++ "b,1,0\n"
              , "missing from current: \"a\"; missing from baseline: \"b\""
              )
            ,
              ( "invalid baseline context"
              , header ++ "a,0,0\n"
              , header ++ "a,1,0\n"
              , "baseline: CSV record 2:"
              )
            ,
              ( "invalid current context"
              , header ++ "a,1,0\n"
              , header ++ "a,1,-1\n"
              , "current: CSV record 2:"
              )
            ,
              ( "duplicates fail before comparison"
              , header ++ "a,1,0\na,2,0\n"
              , header ++ "a,1,0\n"
              , "duplicate benchmark name"
              )
            ]
        ]
    , testCase
        "rendering states direction and descriptive limits and escapes embedded newlines"
        $ withReport
          (header ++ "\"line\nwork\",2,0\nfast,10,0\nsame,1,0\n")
          (header ++ "same,1,0\nfast,5,0\n\"line\nwork\",4,0\n")
        $ \report -> do
          let rendered = renderReport report
          forM_
            [ "Raw mean runtime ratios"
            , "current / baseline"
            , "0.500000x (faster)"
            , "2.000000x (slower)"
            , "1.000000x (unchanged)"
            , "\"line\\nwork\""
            , "Geometric mean of raw mean ratios (3 workloads)"
            , "Stddev of log ratios across workloads"
            , "not a confidence interval or repeated timing samples"
            , "Separate noise-aware timing gate"
            , "--baseline BASELINE.csv --fail-if-slower 10"
            ]
            $ \fragment ->
              assertBool
                ( "report contains "
                    ++ show fragment
                )
                (fragment `isInfixOf` rendered)
    ]

header :: String
header = "Name,Mean (ps),2*Stdev (ps)\n"

baseline :: String
baseline = header ++ "small,1,0\nlarge,1000,10\n"

current :: String
current = header ++ "large,500,5\nsmall,2,0\n"

validCsvFixtures :: [(String, String, [(String, Double)])]
validCsvFixtures =
  [
    ( "plain tasty-bench CSV"
    , header ++ "All.extract,100,5\n"
    , [("All.extract", 100)]
    )
  , ("no final newline", header ++ "work,100,0", [("work", 100)])
  ,
    ( "quoted names, commas, doubled quotes, and CRLF"
    , "Name,Mean (ps),2*Stdev (ps)\r\n\"All.\"\"quoted\"\",work\",100,0\r\n"
    , [("All.\"quoted\",work", 100)]
    )
  ,
    ( "embedded CRLF and LF in names"
    , header ++ "\"line\r\nnext\nlast\",2,0\n"
    , [("line\r\nnext\nlast", 2)]
    )
  ,
    ( "all fields quoted"
    , "\"Name\",\"Mean (ps)\",\"2*Stdev (ps)\"\n\"work\",\"1.5e2\",\"0\"\n"
    , [("work", 150)]
    )
  ,
    ( "allocation columns are accepted"
    , "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory\nwork,42,0,100,0,4096\n"
    , [("work", 42)]
    )
  ,
    ( "extra columns are ignored rather than parsed as timing data"
    , "Name,Mean (ps),2*Stdev (ps),Note,Allocated\nwork,42,0,\"has,comma\",\n"
    , [("work", 42)]
    )
  ,
    ( "column order may change"
    , "2*Stdev (ps),Allocated,Name,Mean (ps)\n1,0,work,3\n"
    , [("work", 3)]
    )
  ,
    ( "Unicode names and BOM"
    , '\xfeff' : header ++ "λ.extract,12,0\n"
    , [("λ.extract", 12)]
    )
  ,
    ( "nonblank name whitespace is preserved"
    , header ++ " work ,2,0\n"
    , [(" work ", 2)]
    )
  ,
    ( "scientific notation and numeric whitespace"
    , header ++ "work, 1.25e2 , 0 \n"
    , [("work", 125)]
    )
  ]

invalidCsvFixtures :: [(String, String, String)]
invalidCsvFixtures =
  [ ("empty input", "", "empty CSV table")
  , ("only headers", header, "no benchmark rows")
  ,
    ( "headers without final newline"
    , "Name,Mean (ps),2*Stdev (ps)"
    , "no benchmark rows"
    )
  ,
    ( "missing name column"
    , "Mean (ps),2*Stdev (ps)\n1,0\n"
    , "missing column \"Name\""
    )
  ,
    ( "missing mean column"
    , "Name,2*Stdev (ps)\nwork,0\n"
    , "missing column \"Mean (ps)\""
    )
  ,
    ( "missing deviation column"
    , "Name,Mean (ps)\nwork,1\n"
    , "missing column \"2*Stdev (ps)\""
    )
  ,
    ( "duplicate header"
    , "Name,Mean (ps),2*Stdev (ps),Name\nwork,1,0,other\n"
    , "duplicate column name"
    )
  ,
    ( "blank header"
    , "Name,Mean (ps),2*Stdev (ps),\nwork,1,0,0\n"
    , "blank column name"
    )
  ,
    ( "duplicate decoded names"
    , header ++ "work,1,0\n\"work\",2,0\n"
    , "duplicate benchmark name \"work\""
    )
  , ("blank name", header ++ ",1,0\n", "missing benchmark name")
  , ("quoted blank name", header ++ "\"\",1,0\n", "missing benchmark name")
  , ("whitespace name", header ++ " \t ,1,0\n", "missing benchmark name")
  , ("too few fields", header ++ "work,1\n", "expected 3 fields, got 2")
  , ("too many fields", header ++ "work,1,0,extra\n", "expected 3 fields, got 4")
  , ("blank record", header ++ "work,1,0\n\n", "expected 3 fields, got 1")
  , ("unterminated quote", header ++ "\"work,1,0\n", "unterminated quoted field")
  ,
    ( "quote inside unquoted field"
    , header ++ "wo\"rk,1,0\n"
    , "quote inside an unquoted field"
    )
  ,
    ( "characters after closing quote"
    , header ++ "\"work\"suffix,1,0\n"
    , "unexpected character after closing quote"
    )
  ,
    ( "unescaped trailing quote"
    , header ++ "\"work\"\",1,0\n"
    , "unterminated quoted field"
    )
  , ("bare carriage return", header ++ "work,1,0\r", "bare carriage return")
  , ("trailing garbage", header ++ "work,1,0\ngarbage", "expected 3 fields, got 1")
  ]

withReport :: String -> String -> (ComparisonReport -> Assertion) -> Assertion
withReport old new assertion = case compareCsv old new of
  Left message -> assertFailure ("unexpected report error: " ++ message)
  Right report -> assertion report

assertLeftContaining :: String -> Either String a -> Assertion
assertLeftContaining expected result = case result of
  Left message ->
    assertBool
      ("expected " ++ show expected ++ " in error " ++ show message)
      (expected `isInfixOf` message)
  Right _ -> assertFailure ("expected error containing " ++ show expected)

assertApprox :: String -> Double -> Double -> Assertion
assertApprox label expected actual =
  assertBool
    (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
    (abs (expected - actual) <= 1e-12 * max 1 (abs expected))
