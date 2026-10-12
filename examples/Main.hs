-- | Run with @cabal run yolk-example@. Both interpreters simplify the same
-- many-sorted terms; all graph operations go through the public Yolk API.
module Main (main) where

import Control.Lens ((&), (.~), (^.), review)
import Control.Monad (void)
import Data.Functor.Const (Const (..))
import Data.HKD (FFoldable (..), FFunctor (..), FTraversable (..), ffoldMapDefault, ffmapDefault)
import Data.Kind (Type)
import Data.Word (Word64)
import System.Exit (die)
import Yolk
import qualified Yolk.Optics as O

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
    x = review O._Var "x"
    y = review O._Var "y"

data Simplification = Simplification
  { reason :: StopReason
  , numericResult :: Maybe (Int, String)
  , booleanResult :: Maybe (Int, String)
  }

simplify :: Backend b m => [Rewrite Math] -> EGraphT b r Math m Simplification
simplify rewrites = do
  numericRoot <- addTerm expression
  booleanRoot <- addTerm condition
  let limits = defaultLimits & O.iterationLimit .~ 20
  result <- run limits rewrites
  numeric <- extract numericRoot
  boolean <- extract booleanRoot
  pure Simplification
    { reason = result ^. O.stopReason
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
    putStrLn "\nConfigurable tree minimization"
    displayChoices "Mutable" (runMutable minimize)
    displayChoices "Persistent" (runPersistent minimize)

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
  -- When the primary objective saturates, absorb the secondary objective too.
  -- Otherwise overflow could reverse the lexicographic ordering of children.
  in if multiplications == maxBound then (maxBound, maxBound) else (multiplications, size)
  where
    multiplication :: Math s f -> Word64
    multiplication Mul {} = 1
    multiplication _ = 0

data Choices = Choices
  { smallest :: Maybe (Word64, String)
  , cheapest :: Maybe (Word64, String)
  , shallowest :: Maybe (Word64, String)
  , lexicographic :: Maybe ((Word64, Word64), String)
  } deriving (Eq, Show)

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
  shallow <- addTerm (term (If (term (Equal one one))
    (term (Add two two)) (term (Add two two))))
  void (union deep shallow)
  byDepth <- extractWith astDepth deep
  pure (Choices (render bySize) (render byWeight) (render byDepth) (render byPreference))
  where
    render :: Maybe (c, Term Math s) -> Maybe (c, String)
    render = fmap (\(cost, tree) -> (cost, pretty tree))

displayChoices :: String -> Either EGraphError Choices -> IO ()
displayChoices backend = \case
  Left err -> die (backend ++ " minimization failed: " ++ show err)
  Right actual -> do
    putStrLn ("\n" ++ backend ++ " backend")
    putStrLn ("  AST size:             " ++ show (smallest actual))
    putStrLn ("  Weighted operations:  " ++ show (cheapest actual))
    putStrLn ("  AST depth:            " ++ show (shallowest actual))
    putStrLn ("  (multiplications, size): " ++ show (lexicographic actual))
    if actual == expected then pure ()
      else die (backend ++ " did not produce the expected cost-model choices")
  where
    expected = Choices
      (Just (3, "(2 * 3)"))
      (Just (5, "(2 + (2 + 2))"))
      (Just (3, "if (1 == 1) then (2 + 2) else (2 + 2)"))
      (Just ((0, 5), "(2 + (2 + 2))"))
