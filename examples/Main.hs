-- | Run with @cabal run yolk-example@. Both interpreters simplify the same
-- many-sorted terms; all graph operations go through the public Yolk API.
module Main (main) where

import Data.HKD (FFoldable (..), FFunctor (..), FTraversable (..), ffoldMapDefault, ffmapDefault)
import Data.Kind (Type)
import System.Exit (die)
import Yolk

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
rules = sequence
  [ rewrite "add-zero" (node (Add x (node (Lit 0)))) x
  , rewrite "multiply-one" (node (Mul x (node (Lit 1)))) x
  , rewrite "equal-reflexive" (node (Equal x x)) (node (BoolLit True))
  , rewrite "if-true" (node (If (node (BoolLit True)) x y)) x
  ]
  where
    x, y :: Pattern Math 'Number
    x = var "x"
    y = var "y"

data Simplification = Simplification
  { reason :: StopReason
  , numericResult :: Maybe (Int, String)
  , booleanResult :: Maybe (Int, String)
  }

simplify :: Backend b m => [Rewrite Math] -> EGraphT b r Math m Simplification
simplify rewrites = do
  numericRoot <- addTerm expression
  booleanRoot <- addTerm condition
  result <- run defaultLimits rewrites
  numeric <- extract numericRoot
  boolean <- extract booleanRoot
  pure Simplification
    { reason = stopReason result
    , numericResult = fmap (\(cost, tree) -> (cost, pretty tree)) numeric
    , booleanResult = fmap (\(cost, tree) -> (cost, pretty tree)) boolean
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

display :: String -> Either EGraphError Simplification -> IO ()
display backend = \case
  Left err -> die (backend ++ " graph failed: " ++ show err)
  Right result -> do
    putStrLn ("\n" ++ backend ++ " backend (" ++ show (reason result) ++ ")")
    printResult "Number" (numericResult result)
    printResult "Boolean" (booleanResult result)
    if reason result == Saturated
        && numericResult result == Just (1, "7")
        && booleanResult result == Just (1, "true")
      then pure ()
      else die (backend ++ " did not produce the expected simplification")
  where
    printResult :: String -> Maybe (Int, String) -> IO ()
    printResult label = \case
      Nothing -> die (backend ++ ": no finite " ++ label ++ " term")
      Just (cost, rendered) ->
        putStrLn ("  " ++ label ++ " result: " ++ rendered ++ " (AST size " ++ show cost ++ ")")
