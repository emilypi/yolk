-- |
-- Module       : Main
-- Description  : Examples of yolk rewriting, extraction, and optics.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Run with @cabal run yolk-example@. Both interpreters simplify the same
-- many-sorted terms; all graph operations go through the public Yolk API.
module Main (main) where

import Control.Lens (review, (&), (.~), (^.))
import Control.Monad (void)
import Data.Functor.Const (Const (..))
import Data.HKD
  ( FFoldable (..)
  , FFunctor (..)
  , FTraversable (..)
  , ffmapDefault
  , ffoldMapDefault
  )
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Kind (Type)
import Data.Word (Word64)
import ExampleResults
import Numeric.Natural (Natural)
import System.Exit (die)
import Yolk
import Yolk.Optics qualified as O

data Sort = Number | Boolean

type Math :: Sort -> (Sort -> Type) -> Type
data Math s f where
  Lit :: Integer -> Math 'Number f
  BoolLit :: Bool -> Math 'Boolean f
  Add :: f 'Number -> f 'Number -> Math 'Number f
  Mul :: f 'Number -> f 'Number -> Math 'Number f
  Equal :: f 'Number -> f 'Number -> Math 'Boolean f
  If :: f 'Boolean -> f 'Number -> f 'Number -> Math 'Number f

instance FFunctor (Math s) where
  ffmap = ffmapDefault

instance FFoldable (Math s) where
  ffoldMap = ffoldMapDefault

instance FTraversable (Math s) where
  ftraverse f = \case
    Lit n -> pure (Lit n)
    BoolLit b -> pure (BoolLit b)
    Add a b -> Add <$> f a <*> f b
    Mul a b -> Mul <$> f a <*> f b
    Equal a b -> Equal <$> f a <*> f b
    If c t e -> If <$> f c <*> f t <*> f e

instance Language Math where
  operatorKey = \case
    Lit n -> "integer:" ++ show n
    BoolLit b -> "boolean:" ++ show b
    Add {} -> "add"
    Mul {} -> "multiply"
    Equal {} -> "equal"
    If {} -> "if-number"

pretty :: Term Math s -> String
pretty tree = case unTerm tree of
  Lit n -> show n
  BoolLit True -> "true"
  BoolLit False -> "false"
  Add a b -> "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
  Mul a b -> "(" ++ pretty a ++ " * " ++ pretty b ++ ")"
  Equal a b -> "(" ++ pretty a ++ " == " ++ pretty b ++ ")"
  If c t e -> "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty e

arithmetic :: Term Math 'Number
arithmetic = term (Mul (term (Add (term (Lit 7)) (term (Lit 0)))) (term (Lit 1)))

condition :: Term Math 'Boolean
condition = term (Equal arithmetic (term (Lit 7)))

expression :: Term Math 'Number
expression = term (If condition arithmetic (term (Lit 99)))

rules :: Either RuleError [Rewrite Math]
rules =
  sequence
    [ rewrite "add-zero" (node (Add x (node (Lit 0)))) x
    , rewrite "multiply-one" (node (Mul x (node (Lit 1)))) x
    , rewrite "equal-reflexive" (node (Equal x x)) (node (BoolLit True))
    , rewrite "if-true" (node (If (node (BoolLit True)) x y)) x
    ]
 where
  x, y :: Pattern Math 'Number
  x = review O._Var "x"
  y = review O._Var "y"

summarizeTree :: (cost, Term Math s) -> CandidateSummary cost
summarizeTree (cost, tree) =
  CandidateSummary {summaryCost = cost, summaryExpression = pretty tree}

-- Preserve the compact printed format at the presentation boundary.
showCandidate :: Show cost => Maybe (CandidateSummary cost) -> String
showCandidate =
  show . fmap (\candidate -> (summaryCost candidate, summaryExpression candidate))

simplify :: Backend b m => [Rewrite Math] -> EGraphT b r Math m Simplification
simplify rewrites = do
  numericRoot <- addTerm expression
  booleanRoot <- addTerm condition
  let limits = defaultLimits & O.iterationLimit .~ 20
  result <- run limits rewrites
  numeric <- extract numericRoot
  boolean <- extract booleanRoot
  pure
    Simplification
      { reason = result ^. O.stopReason
      , numericResult = summarizeTree <$> numeric
      , booleanResult = summarizeTree <$> boolean
      }

main :: IO ()
main = case rules of
  Left err -> die ("Invalid example rule: " ++ show err)
  Right rewrites -> do
    putStrLn "Many-sorted equality saturation"
    putStrLn ("Number input:  " ++ pretty expression)
    putStrLn ("Boolean input: " ++ pretty condition)
    display "Mutable" (runMutable (simplify rewrites))
    display "Persistent" (runPersistent (simplify rewrites))
    putStrLn "\nConfigurable tree minimization"
    displayChoices "Mutable" (runMutable minimize)
    displayChoices "Persistent" (runPersistent minimize)
    putStrLn "\nSolver-free optimal DAG minimization"
    displayDagChoices "Mutable" (runMutable minimizeDag)
    displayDagChoices "Persistent" (runPersistent minimizeDag)
    putStrLn "\nPareto portfolio (illustrative structural costs)"
    displayPareto "Mutable" (portfolioOutcome (runMutable paretoExample))
    displayPareto "Persistent" (portfolioOutcome (runPersistent paretoExample))

display :: String -> Either EGraphError Simplification -> IO ()
display backend = \case
  Left err -> die (backend ++ " graph failed: " ++ show err)
  Right result -> do
    putStrLn ("\n" ++ backend ++ " backend (" ++ show (reason result) ++ ")")
    printResult "Number" (numericResult result)
    printResult "Boolean" (booleanResult result)
    if reason result == Saturated
      && numericResult result == Just (CandidateSummary 1 "7")
      && booleanResult result == Just (CandidateSummary 1 "true")
      then pure ()
      else die (backend ++ " did not produce the expected simplification")
 where
  printResult :: String -> Maybe (CandidateSummary Int) -> IO ()
  printResult label = \case
    Nothing -> die (backend ++ ": no finite " ++ label ++ " term")
    Just candidate ->
      putStrLn
        ( "  "
            ++ label
            ++ " result: "
            ++ summaryExpression candidate
            ++ " (AST size "
            ++ show (summaryCost candidate)
            ++ ")"
        )

-- A payload-aware operator weight could inspect Lit's value as well. Word64
-- weights are nonnegative; weightedSize adds them with saturating arithmetic.
operationWeight :: Math s f -> Word64
operationWeight Mul {} = 10
operationWeight _ = 1

-- The cost type can also be a tuple. Minimize multiplications first and AST
-- size second, without assigning an arbitrary scalar penalty to multiplication.
preferAddition :: TreeCost Math (Word64, Word64)
preferAddition n =
  let multiplications = weightedSize multiplication (ffmap (Const . fst . getConst) n)
      size = astSize (ffmap (Const . snd . getConst) n)
   in -- When the primary objective saturates, absorb the secondary objective too.
      -- Otherwise overflow could reverse the lexicographic ordering of children.
      if multiplications == maxBound
        then (maxBound, maxBound)
        else (multiplications, size)
 where
  multiplication :: Math s f -> Word64
  multiplication Mul {} = 1
  multiplication _ = 0

minimize :: Backend b m => EGraphT b r Math m Choices
minimize = do
  let two = term (Lit 2)
      one = term (Lit 1)
  productId <- addTerm (term (Mul two (term (Lit 3))))
  sumId <- addTerm (term (Add two (term (Add two two))))
  -- Both expressions equal six. Extraction chooses among asserted equalities;
  -- it does not discover this arithmetic identity on its own.
  void (union productId sumId)
  bySize <- extractWith astSize productId
  byWeight <- extractWith (weightedSize operationWeight) productId
  byPreference <- extractWith preferAddition productId

  -- Equal expressions with opposing size/depth preferences: the conditional
  -- has more AST nodes, but its longest path is shorter.
  deep <- addTerm (term (Add one (term (Add one (term (Add one one))))))
  shallow <-
    addTerm
      ( term
          ( If
              (term (Equal one one))
              (term (Add two two))
              (term (Add two two))
          )
      )
  void (union deep shallow)
  byDepth <- extractWith astDepth deep
  pure
    Choices
      { smallest = summarizeTree <$> bySize
      , cheapest = summarizeTree <$> byWeight
      , shallowest = summarizeTree <$> byDepth
      , lexicographic = summarizeTree <$> byPreference
      }

displayChoices :: String -> Either EGraphError Choices -> IO ()
displayChoices backend = \case
  Left err -> die (backend ++ " minimization failed: " ++ show err)
  Right actual -> do
    putStrLn ("\n" ++ backend ++ " backend")
    putStrLn ("  AST size:             " ++ showCandidate (smallest actual))
    putStrLn ("  Weighted operations:  " ++ showCandidate (cheapest actual))
    putStrLn ("  AST depth:            " ++ showCandidate (shallowest actual))
    putStrLn ("  (multiplications, size): " ++ showCandidate (lexicographic actual))
    if actual == expected
      then pure ()
      else die (backend ++ " did not produce the expected cost-model choices")
 where
  expected =
    Choices
      { smallest = Just (CandidateSummary 3 "(2 * 3)")
      , cheapest = Just (CandidateSummary 5 "(2 + (2 + 2))")
      , shallowest =
          Just (CandidateSummary 3 "if (1 == 1) then (2 + 2) else (2 + 2)")
      , lexicographic = Just (CandidateSummary (0, 5) "(2 + (2 + 2))")
      }

minimizeDag :: Backend b m => EGraphT b r Math m (DagChoices (Dag Math 'Number))
minimizeDag = do
  let one = term (Lit 1)
      pair = term (Add one one)
  smallTree <- addTerm (term (Add one (term (Add (term (Lit 2)) one))))
  shared <- addTerm (term (Mul pair pair))
  -- Both expressions equal four. The first has five tree nodes and four
  -- distinct nodes; the second has seven tree nodes but only three distinct
  -- nodes. Choosing a minimum tree and deduplicating it is insufficient.
  void (union smallTree shared)
  tree <- extractWith astSize smallTree
  let exact = defaultDagLimits & O.expansionLimit .~ Nothing
      seedOnly = defaultDagLimits & O.expansionLimit .~ Just 0
  best <- extractDagOptimal exact dagSizeCost smallTree
  weighted <- extractDagOptimal exact operationWeight smallTree
  limited <- extractDagOptimal seedOnly dagSizeCost smallTree
  pure
    DagChoices
      { treeChoice = summarizeTree <$> tree
      , sharedChoice = best
      , weightedChoice = weighted
      , limitedChoice = limited
      }

displayDagChoices ::
  String -> Either EGraphError (DagChoices (Dag Math 'Number)) -> IO ()
displayDagChoices backend = \case
  Left err -> die (backend ++ " DAG extraction failed: " ++ show err)
  Right choices -> case
    (sharedChoice choices, weightedChoice choices, limitedChoice choices) of
    (Just best, Just weighted, Just limited) -> do
      putStrLn ("\n" ++ backend ++ " backend")
      putStrLn ("  Tree optimum: " ++ showCandidate (treeChoice choices))
      displayCertificate "DAG optimum" best
      displayCertificate "Weighted DAG" weighted
      displayCertificate "Zero-expansion budget" limited
      putStrLn "  Shared code (each algebra action runs once):"
      counter <- newIORef (0 :: Int)
      let emit :: Math s (Const String) -> IO (Const String s)
          emit layer = do
            ident <- atomicModifyIORef' counter (\i -> (i + 1, i))
            let name = "n" ++ show ident
            putStrLn ("    " ++ name ++ " = " ++ renderLayer layer)
            pure (Const name)
      Const name <- foldDagM emit (dag best)
      putStrLn ("    result = " ++ name)
      if treeChoice choices == Just (CandidateSummary 5 "(1 + (2 + 1))")
        && dagCost best == 3
        && dagSize (dag best) == 3
        && optimality best == Optimal
        && lowerBound best == 3
        && pretty (dagToTerm (dag best)) == "((1 + 1) * (1 + 1))"
        && dagCost weighted == 4
        && optimality weighted == Optimal
        && pretty (dagToTerm (dag weighted)) == "(1 + (2 + 1))"
        && optimality limited == Unproven
        && expansions limited == 0
        && lowerBound limited <= 3
        && dagCost limited >= 3
        then pure ()
        else die (backend ++ " did not produce the expected DAG choices")
    _ -> die (backend ++ " unexpectedly has no finite DAG")
 where
  displayCertificate label result =
    putStrLn
      ( "  "
          ++ label
          ++ ": cost "
          ++ show (dagCost result)
          ++ ", lower bound "
          ++ show (lowerBound result)
          ++ ", "
          ++ show (optimality result)
          ++ ", expansions "
          ++ show (expansions result)
          ++ ", "
          ++ pretty (dagToTerm (dag result))
      )

renderLayer :: Math s (Const String) -> String
renderLayer = \case
  Lit n -> show n
  BoolLit b -> show b
  Add (Const a) (Const b) -> a ++ " + " ++ b
  Mul (Const a) (Const b) -> a ++ " * " ++ b
  Equal (Const a) (Const b) -> a ++ " == " ++ b
  If (Const c) (Const t) (Const e) -> "if " ++ c ++ " then " ++ t ++ " else " ++ e

-- These three expressions all equal six. The costs are deliberately chosen
-- to exhibit a nondominated point unsupported by any positive weighted sum;
-- they are illustrative extraction-model units, not hardware measurements.
paretoExample ::
  Backend b m =>
  EGraphT b r Math m (Either ParetoError FrontierSummary)
paretoExample = do
  literal <- add (Lit 6)
  addition <- addTerm (term (Add (term (Lit 3)) (term (Lit 3))))
  multiplication <- addTerm (term (Mul (term (Lit 2)) (term (Lit 3))))
  void (union literal addition)
  void (union literal multiplication)
  let options = defaultParetoOptions 2 & O.paretoExpansionLimit .~ Nothing
  result <- extractPareto options weight literal
  pure $
    fmap
      ( \r ->
          FrontierSummary
            { frontierCoverage = paretoCoverage r
            , frontierCandidates =
                [ CandidateSummary
                    { summaryCost = ObjectiveCosts (candidateCost c)
                    , summaryExpression = pretty (dagToTerm (candidateValue c))
                    }
                | c <- paretoCandidates r
                ]
            }
      )
      result
 where
  weight :: Math s f -> [Natural]
  weight (Lit 6) = [1, 6]
  weight Add {} = [4, 4]
  weight Mul {} = [6, 1]
  weight _ = [0, 0]

displayPareto :: String -> PortfolioOutcome -> IO ()
displayPareto backend result = case result of
  FrontierFound frontier
    | frontierCoverage frontier == Complete
    , map costs (frontierCandidates frontier) == [[1, 6], [4, 4], [6, 1]] -> do
        putStrLn (backend ++ ": complete model frontier")
        mapM_ printCandidate (frontierCandidates frontier)
  GraphFailed err -> die (backend ++ " graph failed: " ++ show err)
  ExtractionFailed err -> die (backend ++ " Pareto extraction failed: " ++ show err)
  _ -> die (backend ++ " Pareto demonstration failed: " ++ show result)
 where
  costs = getObjectiveCosts . summaryCost
  printCandidate candidate =
    putStrLn ("  " ++ show (costs candidate) ++ "  " ++ summaryExpression candidate)
