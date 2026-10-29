-- |
-- Module       : ExtractionTests
-- Description  : Tests for configurable yolk tree minimization criteria.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module ExtractionTests (customExtractionTests) where

import Control.Monad (foldM, void)
import Data.Functor.Const (Const (..))
import Data.HKD (ffmap)
import Data.Word (Word64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC
import TestLanguage
import TestSupport
import Yolk

rendered :: Maybe (c, Term Math s) -> Maybe (c, String)
rendered = fmap (\(cost, tree) -> (cost, pretty tree))

operatorWeight :: Math s f -> Word64
operatorWeight Mul {} = 10
operatorWeight _ = 1

-- Prefer fewer multiplications, breaking ties by AST size. A pair's Ord
-- instance is lexicographic; no Num instance or scalar encoding is needed.
lexicographic :: TreeCost Math (Word64, Word64)
lexicographic n =
  let multiplications = weightedSize multiplication (ffmap (Const . fst . getConst) n)
      size = astSize (ffmap (Const . snd . getConst) n)
   in if multiplications == maxBound
        then (maxBound, maxBound)
        else (multiplications, size)
 where
  multiplication :: Math s f -> Word64
  multiplication Mul {} = 1
  multiplication _ = 0

alternatives :: Backend b m => EGraphT b r Math m (ClassId r 'Number)
alternatives = do
  productId <- addTerm (times (number 2) (number 3))
  sumId <- addTerm (plus (number 2) (plus (number 2) (number 2)))
  void (union productId sumId)
  pure sumId

customExtractionTests :: TestTree
customExtractionTests =
  testGroup
    "configurable tree minimization"
    [ both
        "changing the objective changes the representative on the same dirty graph"
        (Just (3, "(2 * 3)"), Just (5, "(2 + (2 + 2))"), Just (3, "(2 * 3)"))
        $ Program
        $ do
          root <- alternatives
          first <- rendered <$> extractWith astSize root
          weighted <- rendered <$> extractWith (weightedSize operatorWeight) root
          lastResult <- rendered <$> extract root
          pure (first, weighted, lastResult)
    , both
        "depth can prefer a larger but shallower tree"
        (Just (7, 7), Just (3, 10))
        $ Program
        $ do
          deep <- addTerm (plus (number 1) (plus (number 1) (plus (number 1) (number 1))))
          shallow <-
            addTerm
              ( ifNumber
                  (equal (number 1) (number 1))
                  (plus (number 2) (number 2))
                  (plus (number 2) (number 2))
              )
          void (union deep shallow)
          bySize <- extractWith astSize deep
          byDepth <- extractWith astDepth deep
          pure
            ( fmap (\(c, t) -> (c, termSize t)) bySize
            , fmap (\(c, t) -> (c, termSize t)) byDepth
            )
    , both
        "lexicographic costs need only Ord, not Num"
        (Just ((0, 5), "(2 + (2 + 2))"))
        $ Program
        $ do
          root <- alternatives
          rendered <$> extractWith lexicographic root
    , both
        "weights can inspect payloads and different result sorts"
        (Just (10, "(2 == 3)"))
        $ Program
        $ do
          root <- addTerm (equal (number 2) (number 3))
          rendered <$> extractWith (weightedSize payloadWeight) root
    , both
        "a custom algebra receives child costs in operand order"
        (Just (5, "(2 + 3)"))
        $ Program
        $ do
          left <- addTerm (plus (number 3) (number 2))
          right <- addTerm (plus (number 2) (number 3))
          void (union left right)
          rendered <$> extractWith asymmetric left
    , both
        "a later cheaper child propagates back to an existing parent"
        (Just (4, "((2 + 2) * 1)"))
        $ Program
        $ do
          four <- add (Lit 4)
          one <- add (Lit 1)
          parent <- add (Mul four one)
          replacement <- addTerm (plus (number 2) (number 2))
          void (union four replacement)
          rendered <$> extractWith (weightedSize delayedWeight) parent
    , both
        "zero weights terminate through mutually recursive classes"
        (Just 0, Just 0, True)
        $ Program
        $ do
          leaf <- add (Lit 7)
          one <- add (Lit 1)
          a <- add (Add leaf one)
          b <- add (Mul a one)
          void (union leaf b)
          first <- extractWith (weightedSize (const 0)) a
          second <- extractWith (weightedSize (const 0)) b
          same <- case first of
            Nothing -> pure False
            Just (_, witness) -> addTerm witness >>= equivalent a
          pure (fmap fst first, fmap fst second, same)
    , both "ties retain a finite witness across repeat calls" True $ Program $ do
        root <- alternatives
        first <- rendered <$> extractWith (weightedSize (const 0)) root
        second <- rendered <$> extractWith (weightedSize (const 0)) root
        pure (first == second && maybe False ((== 0) . fst) first)
    , both
        "hidden cheap nodes cannot defeat a visible expensive leaf"
        (Just (100, "5"))
        $ Program
        $ do
          root <- addTerm (plus (number 2) (number 3))
          leaf <- add (Lit 5)
          void (union root leaf)
          rebuild
          hideNonLeaves root
          rendered <$> extractWith (weightedSize payloadWeight) root
    , both
        "a zero-cost cycle without a finite visible derivation yields Nothing"
        (Nothing :: Maybe Word64)
        $ Program
        $ do
          base <- addTerm (plus (number 2) (number 3))
          one <- add (Lit 1)
          recursive <- add (Mul base one)
          rebuild
          hideNonLeaves base
          void (union base recursive)
          fmap fst <$> extractWith (weightedSize (const 0)) recursive
    , both
        "overflowing alternatives cannot wrap below a cheaper leaf"
        (Just (100, "5"))
        $ Program
        $ do
          root <- addTerm (plus (number 2) (number 3))
          leaf <- add (Lit 5)
          void (union root leaf)
          rendered <$> extractWith (weightedSize hugeWeight) root
    , both
        "AST size uses the full Word64 range and saturates beyond it"
        (Just (maxBound `div` 2), Just maxBound, Just maxBound)
        $ Program
        $ do
          base <- add (Lit 1)
          atIntMax <- double 62 base
          atWordMax <- add (Add atIntMax atIntMax)
          beyondWord <- add (Add atWordMax atWordMax)
          (,,)
            <$> (fmap fst <$> extractWith astSize atIntMax)
            <*> (fmap fst <$> extractWith astSize atWordMax)
            <*> (fmap fst <$> extractWith astSize beyondWord)
    , testCase "legacy extraction still reports Int overflow on both backends" $ do
        let action :: Backend b m => EGraphT b r Math m (Maybe Int)
            action = do
              base <- add (Lit 1)
              root <- double 64 base
              fmap fst <$> extract root
        assertEqual "mutable overflow" (Left CostOverflow) (runMutable action)
        assertEqual "persistent overflow" (Left CostOverflow) (runPersistent action)
    , testCase "saturating addition keeps exact boundary values" $ do
        assertEqual "zero" maxBound (saturatingAdd maxBound 0)
        assertEqual "exact maximum" maxBound (saturatingAdd (maxBound - 1) 1)
        assertEqual "overflow" maxBound (saturatingAdd (maxBound - 1) 2)
    , testCase "lexicographic primary saturation absorbs the tie-break cost" $ do
        let smaller = lexicographic (Mul (Const (maxBound - 1, 100)) (Const (0, 0)))
            larger = lexicographic (Mul (Const (maxBound, 0)) (Const (0, 0)))
        assertEqual "smaller child reaches top" (maxBound, maxBound) smaller
        assertEqual "larger child stays at top" (maxBound, maxBound) larger
    , QC.testProperty "weighted and depth minima agree with exhaustive finite trees" $
        \(weights :: Weights) -> QC.forAll (QC.chooseInt (0, 3)) $ \levels ->
          let trees = enumerate levels
              expectedWeight = minimum (map (score weights) trees)
              expectedDepth = minimum (map depth trees)
              action ::
                Backend b m =>
                EGraphT b r Math m (Maybe (Word64, Word64, Bool), Maybe Word64)
              action = do
                root <- buildAlternatives levels
                best <- extractWith (weightedSize (weight weights)) root
                shallow <- extractWith astDepth root
                pure
                  ( fmap
                      (\(c, t) -> (c, score weights t, pretty t `elem` map pretty trees))
                      best
                  , fmap fst shallow
                  )
              expected = (Just (expectedWeight, expectedWeight, True), Just expectedDepth)
           in QC.conjoin
                [ observeProperty "mutable exhaustive oracle" expected (runMutable action)
                , observeProperty
                    "persistent exhaustive oracle"
                    expected
                    (runPersistent action)
                ]
    , QC.testProperty "saturating addition agrees with unbounded arithmetic" $
        \(a :: Word64) (b :: Word64) ->
          toInteger (saturatingAdd a b)
            QC.=== min (toInteger (maxBound :: Word64)) (toInteger a + toInteger b)
    ]
 where
  asymmetric :: TreeCost Math Word64
  asymmetric (Lit n) = fromInteger (abs n)
  asymmetric (Add (Const a) (Const b)) = saturatingAdd 1 (max (saturatingAdd a a) b)
  asymmetric n = astSize n

  delayedWeight :: Math s f -> Word64
  delayedWeight (Lit 4) = 100
  delayedWeight (Lit _) = 0
  delayedWeight Add {} = 1
  delayedWeight Mul {} = 3
  delayedWeight _ = 1

  payloadWeight :: Math s f -> Word64
  payloadWeight (Lit 2) = 2
  payloadWeight (Lit 3) = 3
  payloadWeight (Lit 5) = 100
  payloadWeight Equal {} = 5
  payloadWeight _ = 1

  hugeWeight :: Math s f -> Word64
  hugeWeight (Lit 2) = maxBound
  hugeWeight (Lit 3) = 10
  hugeWeight (Lit 5) = 100
  hugeWeight _ = 1

double ::
  Backend b m =>
  Int ->
  ClassId r 'Number ->
  EGraphT b r Math m (ClassId r 'Number)
double count root = foldM (\child _ -> add (Add child child)) root [1 .. count]

data Weights = Weights Word64 Word64 Word64
  deriving stock (Show)

instance QC.Arbitrary Weights where
  arbitrary = Weights <$> small <*> small <*> small
   where
    small = QC.choose (0, 12)
  shrink (Weights l a m) =
    [Weights l' a' m' | (l', a', m') <- QC.shrink (l, a, m)]

weight :: Weights -> Math s f -> Word64
weight (Weights l a m) = \case
  Lit n -> l + fromInteger (abs n `mod` 5)
  Add {} -> a
  Mul {} -> m
  _ -> 1

-- Independent oracle: enumerate every tree of a small acyclic grammar, then
-- score each concrete tree directly. It neither runs relaxation nor chooses
-- a best child. At each level we assert three synthetic alternatives equal;
-- unique payloads prevent unrelated grammar classes from hashconsing together.
enumerate :: Int -> [Term Math 'Number]
enumerate 0 = [number 0]
enumerate level =
  number tag
    : [plus t (number (tag + 1)) | t <- children]
    ++ [times a b | a <- children, b <- children]
 where
  tag = toInteger (10 * level)
  children = enumerate (level - 1)

buildAlternatives ::
  Backend b m => Int -> EGraphT b r Math m (ClassId r 'Number)
buildAlternatives 0 = add (Lit 0)
buildAlternatives level = do
  child <- buildAlternatives (level - 1)
  let tag = toInteger (10 * level)
  root <- add (Lit tag)
  marker <- add (Lit (tag + 1))
  sumId <- add (Add child marker)
  productId <- add (Mul child child)
  void (union root sumId)
  void (union root productId)
  pure root

score :: Weights -> Term Math s -> Word64
score weights@(Weights l a m) t = case unTerm t of
  Lit n -> l + fromInteger (abs n `mod` 5)
  BoolLit _ -> 1
  Add x y -> a + score weights x + score weights y
  Mul x y -> m + score weights x + score weights y
  Equal x y -> 1 + score weights x + score weights y
  If c x y -> 1 + score weights c + score weights x + score weights y

depth :: Term Math s -> Word64
depth t = case unTerm t of
  Lit _ -> 1
  BoolLit _ -> 1
  Add x y -> 1 + max (depth x) (depth y)
  Mul x y -> 1 + max (depth x) (depth y)
  Equal x y -> 1 + max (depth x) (depth y)
  If c x y -> 1 + maximum [depth c, depth x, depth y]
