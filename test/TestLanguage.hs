-- | A deliberately many-sorted language shared by the library tests.
module TestLanguage
  ( Sort (..)
  , Math (..)
  , number
  , boolean
  , plus
  , times
  , equal
  , ifNumber
  , pretty
  , termSize
  , arithmeticRules
  ) where

import Data.HKD (FFoldable (..), FFunctor (..), FTraversable (..), ffoldMapDefault, ffmapDefault)
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Yolk (Language (..), Pattern, Rewrite, RuleError, Term, node, rewrite, term, unTerm, var)

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
  -- The literal skeleton and payload intentionally collide across sorts.
  -- Keys are injective within each sort; the graph must also key the sort.
  operatorKey = \case
    Lit n -> "literal:" ++ show n
    BoolLit b -> "literal:" ++ if b then "1" else "0"
    Add {} -> "add"
    Mul {} -> "mul"
    Equal {} -> "equal"
    If {} -> "if-number"

number :: Integer -> Term Math 'Number
number = term . Lit

boolean :: Bool -> Term Math 'Boolean
boolean = term . BoolLit

plus :: Term Math 'Number -> Term Math 'Number -> Term Math 'Number
plus a b = term (Add a b)

times :: Term Math 'Number -> Term Math 'Number -> Term Math 'Number
times a b = term (Mul a b)

equal :: Term Math 'Number -> Term Math 'Number -> Term Math 'Boolean
equal a b = term (Equal a b)

ifNumber :: Term Math 'Boolean -> Term Math 'Number -> Term Math 'Number -> Term Math 'Number
ifNumber c t e = term (If c t e)

pretty :: Term Math s -> String
pretty expression = case unTerm expression of
  Lit n -> show n
  BoolLit True -> "true"
  BoolLit False -> "false"
  Add a b -> "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
  Mul a b -> "(" ++ pretty a ++ " * " ++ pretty b ++ ")"
  Equal a b -> "(" ++ pretty a ++ " == " ++ pretty b ++ ")"
  If c t e -> "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty e

termSize :: Term Math s -> Int
termSize expression = 1 + getSum (ffoldMap (Sum . termSize) (unTerm expression))

arithmeticRules :: Either RuleError [Rewrite Math]
arithmeticRules = sequence
  [ rewrite "add-zero" (node (Add x (node (Lit 0)))) x
  , rewrite "multiply-one" (node (Mul x (node (Lit 1)))) x
  ]
  where
    x :: Pattern Math 'Number
    x = var "x"
