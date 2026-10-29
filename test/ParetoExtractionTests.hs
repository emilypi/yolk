-- |
-- Module       : ParetoExtractionTests
-- Description  : Independent vector-frontier oracles and public extraction regressions.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module ParetoExtractionTests
  ( paretoExtractionTests
  , SearchSummary (..)
  , summaryCandidatesL
  , summaryCoverageL
  , summaryFeasibilityL
  , summaryTerminationL
  , summaryWitnessesCompleteL
  ) where

import Control.Lens (Lens', lens)
import Control.Lens qualified as Lens
import Control.Monad (foldM, forM, forM_, void)
import Data.Either (isLeft)
import Data.Functor.Const (Const (..))
import Data.HKD
  ( FFoldable (..)
  , FFunctor (..)
  , FTraversable (..)
  , ffmapDefault
  , ffoldMapDefault
  )
import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC
import TestLanguage
import TestSupport
import Yolk

exact :: ParetoOptions
exact = (defaultParetoOptions 2) {paretoExpansionLimit = Nothing}

alternatives :: Backend b m => EGraphT b r Math m (ClassId r 'Number)
alternatives = do
  a <- add (Lit 6)
  b <- addTerm (plus (number 3) (number 3))
  c <- addTerm (times (number 2) (number 3))
  void (union a b)
  void (union a c)
  pure a

tradeoff :: Math s f -> [Natural]
tradeoff (Lit 6) = [1, 6]
tradeoff Add {} = [4, 4]
tradeoff Mul {} = [6, 1]
tradeoff _ = [0, 0]

-- Candidate observations vary by assertion: some compare coordinates and
-- others compare the archive size. The evidence fields retain their names.
data SearchSummary candidates = SearchSummary
  { summaryCandidates :: candidates
  , summaryCoverage :: ParetoCoverage
  , summaryFeasibility :: ParetoFeasibility
  , summaryTermination :: ParetoTermination
  , summaryWitnessesComplete :: Bool
  }
  deriving (Eq, Show)

summaryCandidatesL :: Lens' (SearchSummary candidates) candidates
summaryCandidatesL = lens summaryCandidates (\s a -> s {summaryCandidates = a})

summaryCoverageL :: Lens' (SearchSummary candidates) ParetoCoverage
summaryCoverageL = lens summaryCoverage (\s a -> s {summaryCoverage = a})

summaryFeasibilityL :: Lens' (SearchSummary candidates) ParetoFeasibility
summaryFeasibilityL = lens summaryFeasibility (\s a -> s {summaryFeasibility = a})

summaryTerminationL :: Lens' (SearchSummary candidates) ParetoTermination
summaryTerminationL = lens summaryTermination (\s a -> s {summaryTermination = a})

summaryWitnessesCompleteL :: Lens' (SearchSummary candidates) Bool
summaryWitnessesCompleteL =
  lens summaryWitnessesComplete (\s a -> s {summaryWitnessesComplete = a})

summarize ::
  ([ParetoCandidate a] -> candidates) -> ParetoResult a -> SearchSummary candidates
summarize observeCandidates result =
  SearchSummary
    { summaryCandidates = observeCandidates (paretoCandidates result)
    , summaryCoverage = paretoCoverage result
    , summaryFeasibility = paretoFeasibility result
    , summaryTermination = paretoTermination result
    , summaryWitnessesComplete = paretoWitnessesComplete result
    }

summary :: ParetoResult a -> SearchSummary [[Natural]]
summary = summarize (map candidateCost)

paretoExtractionTests :: TestTree
paretoExtractionTests =
  testGroup
    "shared Pareto extraction"
    [ testCase "option parsing enforces every scalar and combined boundary" $ do
        let invalid =
              [ exact {paretoDimensions = 0}
              , exact {paretoDimensions = 65}
              , exact {paretoArchiveLimit = 0}
              , exact {paretoArchiveLimit = 1000001}
              , exact {paretoWitnessLimit = -1}
              , exact {paretoWitnessLimit = 1000001}
              , exact {paretoNodeLimit = 0}
              , exact {paretoNodeLimit = 1000001}
              , exact {paretoRootLimit = 0}
              , exact {paretoRootLimit = 1000001}
              , exact {paretoCostBitLimit = 0}
              , exact {paretoCostBitLimit = 65537}
              , exact {paretoArchiveLimit = 1001, paretoWitnessLimit = 1000}
              , exact {paretoUpperBounds = Just [1]}
              , exact {paretoUpperBounds = Just (repeat 0)}
              , exact {paretoCostBitLimit = 1, paretoUpperBounds = Just [0, 2]}
              ]
            valid =
              [ exact
              , (defaultParetoOptions 1)
                  { paretoArchiveLimit = 1
                  , paretoWitnessLimit = 1000000
                  }
              , (defaultParetoOptions 64)
                  { paretoArchiveLimit = 1000000
                  , paretoWitnessLimit = 1
                  , paretoNodeLimit = 1000000
                  , paretoRootLimit = 1000000
                  , paretoCostBitLimit = 65536
                  , paretoUpperBounds = Just (replicate 64 0)
                  , paretoExpansionLimit = Just 0
                  }
              ]
        forM_ invalid $ \options ->
          assertBool
            "invalid configuration"
            (isLeft (parseParetoOptions options))
        forM_ valid $ \options ->
          assertEqual
            "parsed options round trip"
            (Right options)
            (Lens.view parsedParetoOptions <$> parseParetoOptions options)
    , both
        "retains a nonconvex tradeoff missed by weighted sums"
        ( Right SearchSummary
            { summaryCandidates = [[1, 6], [4, 4], [6, 1]]
            , summaryCoverage = Complete
            , summaryFeasibility = FeasibleFound
            , summaryTermination = SearchExhausted
            , summaryWitnessesComplete = True
            }
        )
        $ Program
        $ alternatives >>= fmap (fmap summary) . extractPareto exact tradeoff
    , both
        "hard upper bounds retain the feasible middle alternative"
        ( Right SearchSummary
            { summaryCandidates = [[4, 4]]
            , summaryCoverage = Complete
            , summaryFeasibility = FeasibleFound
            , summaryTermination = SearchExhausted
            , summaryWitnessesComplete = True
            }
        )
        $ Program
        $ alternatives
          >>= fmap (fmap summary)
            . extractPareto (exact {paretoUpperBounds = Just [4, 4]}) tradeoff
    , both
        "zero budget with an infeasible seed does not prove infeasibility"
        ( Right SearchSummary
            { summaryCandidates = []
            , summaryCoverage = Partial
            , summaryFeasibility = NoFeasibleFoundYet
            , summaryTermination = ExpansionBudget
            , summaryWitnessesComplete = False
            }
        )
        $ Program
        $ alternatives
          >>= fmap (fmap summary)
            . extractPareto
              (exact {paretoUpperBounds = Just [4, 4], paretoExpansionLimit = Just 0})
              tradeoff
    , both
        "bounds alone can prove infeasibility at zero budget"
        ( Right SearchSummary
            { summaryCandidates = []
            , summaryCoverage = Complete
            , summaryFeasibility = ProvenInfeasible
            , summaryTermination = SearchExhausted
            , summaryWitnessesComplete = True
            }
        )
        $ Program
        $ alternatives
          >>= fmap (fmap summary)
            . extractPareto
              (exact {paretoUpperBounds = Just [0, 0], paretoExpansionLimit = Just 0})
              tradeoff
    , both
        "archive truncation reports approximation and never complete coverage"
        ( Right SearchSummary
            { summaryCandidates = 2
            , summaryCoverage = Approximate
            , summaryFeasibility = FeasibleFound
            , summaryTermination = ArchiveBudget
            , summaryWitnessesComplete = False
            }
        )
        $ Program
        $ do
          root <- alternatives
          result <- extractPareto (exact {paretoArchiveLimit = 2}) tradeoff root
          pure (summarize length <$> result)
    , both
        "vector completeness is independent from equal-vector witness sampling"
        ( Right SearchSummary
            { summaryCandidates = 1
            , summaryCoverage = Complete
            , summaryFeasibility = FeasibleFound
            , summaryTermination = SearchExhausted
            , summaryWitnessesComplete = False
            }
        , Right SearchSummary
            { summaryCandidates = 3
            , summaryCoverage = Complete
            , summaryFeasibility = FeasibleFound
            , summaryTermination = SearchExhausted
            , summaryWitnessesComplete = True
            }
        )
        $ Program
        $ do
          root <- alternatives
          let weight :: Math s f -> [Natural]
              weight (Lit 6) = [1, 1]
              weight Add {} = [1, 1]
              weight Mul {} = [1, 1]
              weight _ = [0, 0]
          one <- extractPareto exact weight root
          allThree <- extractPareto (exact {paretoWitnessLimit = 3}) weight root
          pure (summarize length <$> one, summarize length <$> allThree)
    , both
        "a zero-cost seed can establish vector coverage before witness search"
        ( Right SearchSummary
            { summaryCandidates = [[0, 0]]
            , summaryCoverage = Complete
            , summaryFeasibility = FeasibleFound
            , summaryTermination = ExpansionBudget
            , summaryWitnessesComplete = False
            }
        )
        $ Program
        $ alternatives
          >>= fmap (fmap summary)
            . extractPareto (exact {paretoExpansionLimit = Just 0}) (const [0, 0])
    , both
        "malformed objective dimensions fail without inventing a score"
        (Left (ObjectiveDimensionMismatch 2 1))
        $ Program
        $ do
          root <- add (Lit 0)
          fmap summary <$> extractPareto exact (const [0]) root
    , both
        "input precision limit is checked before accumulation"
        (Left (ParetoInputLimit "objective or bound exceeds the input bit limit"))
        $ Program
        $ do
          root <- add (Lit 0)
          fmap summary
            <$> extractPareto (exact {paretoCostBitLimit = 3}) (const [8, 0]) root
    , both
        "exact Natural totals do not saturate at Word64"
        (Right [[2 * 2 ^ (80 :: Int), 0]])
        $ Program
        $ do
          leaf <- add (Lit 0)
          root <- add (Add leaf leaf)
          fmap (map candidateCost . paretoCandidates)
            <$> extractPareto exact (const [2 ^ (80 :: Int), 0]) root
    , both
        "root and graph input limits are enforced"
        ( Left (ParetoInputLimit "root position limit exceeded")
        , Left (ParetoInputLimit "graph node limit exceeded before snapshot")
        )
        $ Program
        $ do
          leaf <- add (Lit 0)
          root <- add (Add leaf leaf)
          tooManyRoots <-
            extractParetoMany
              (exact {paretoRootLimit = 1})
              (const [1, 1])
              (MixedRoots [root, root] [])
          tooManyNodes <- extractPareto (exact {paretoNodeLimit = 1}) (const [1, 1]) root
          pure (fmap summary tooManyRoots, fmap summary tooManyNodes)
    , both
        "joint mixed roots share identity and one fold across the forest"
        (Right ([[3, 3]], [0, 1, 1], True))
        $ Program
        $ do
          leaf <- add (Lit 0)
          a <- add (Add leaf leaf)
          b <- add (Equal leaf leaf)
          result <- extractParetoMany exact (const [1, 1]) (MixedRoots [a, a] [b])
          pure $
            fmap
              ( \r -> case paretoCandidates r of
                  [candidate] ->
                    let forest = candidateValue candidate
                        (visits, _) = foldDagManyM record forest
                        duplicate = case forest of
                          MixedRoots [x, y] [_] -> dagNodeId x == dagNodeId y
                          _ -> False
                     in (map candidateCost (paretoCandidates r), sort visits, duplicate)
                  _ -> ([], [], False)
              )
              result
    , both
        "empty root forest is a complete feasible zero vector at zero budget"
        ( Right SearchSummary
            { summaryCandidates = [[0, 0]]
            , summaryCoverage = Complete
            , summaryFeasibility = FeasibleFound
            , summaryTermination = SearchExhausted
            , summaryWitnessesComplete = True
            }
        )
        $ Program
        $ fmap summary
          <$> extractParetoMany
            (exact {paretoExpansionLimit = Just 0})
            (const [1, 1])
            (MixedRoots [] [])
    , both
        "forest folds safely combine unrelated local node IDs"
        (Right ["(1+1)", "(2+2)"])
        $ Program
        $ do
          a <- addTerm (plus (number 1) (number 1))
          b <- addTerm (plus (number 2) (number 2))
          first <- extractPareto exact (const [1, 1]) a
          second <- extractPareto exact (const [1, 1]) b
          pure $ do
            x <- first
            y <- second
            let roots =
                  MixedRoots
                    (map candidateValue (paretoCandidates x ++ paretoCandidates y)) []
                MixedRoots values _ = foldDagMany render roots
            pure (map getConst values)
    , QC.testProperty
        "complete and bounded searches agree with independent exhaustive class \
        \assignments"
        $ \specification -> QC.forAll (QC.choose (0, 8)) $
          \budget -> agrees specification budget
    ]
 where
  record :: Math s (Const Int) -> ([Int], Const Int s)
  record n =
    let children = ffoldMap (\(Const v) -> [v]) n
        height = if null children then 0 else maximum children + 1
     in ([height], Const height)
  render :: Math s (Const String) -> Const String s
  render (Lit n) = Const (show n)
  render (Add (Const a) (Const b)) = Const ("(" ++ a ++ "+" ++ b ++ ")")
  render _ = Const "other"

type MixedRoots :: (Sort -> Type) -> Type
data MixedRoots f = MixedRoots [f 'Number] [f 'Boolean]
instance FFunctor MixedRoots where ffmap = ffmapDefault
instance FFoldable MixedRoots where ffoldMap = ffoldMapDefault
instance FTraversable MixedRoots where
  ftraverse f (MixedRoots a b) = MixedRoots <$> traverse f a <*> traverse f b

-- Synthetic labels represent declared graph equalities, independent of the
-- arithmetic language. This lets the oracle enumerate cyclic selection cases.
type Synthetic :: Sort -> (Sort -> Type) -> Type
data Synthetic s f where
  Synthetic :: Int -> Int -> [Natural] -> [f 'Number] -> Synthetic 'Number f
instance FFunctor (Synthetic s) where ffmap = ffmapDefault
instance FFoldable (Synthetic s) where ffoldMap = ffoldMapDefault
instance FTraversable (Synthetic s) where
  ftraverse f (Synthetic owner choice cost children) =
    Synthetic owner choice cost <$> traverse f children
instance Language Synthetic where
  operatorKey (Synthetic owner choice cost children) =
    show (owner, choice, cost, length children)

syntheticCost :: Synthetic s f -> [Natural]
syntheticCost (Synthetic _ _ cost _) = cost

newtype NumberRoots f = NumberRoots [f 'Number]
instance FFunctor NumberRoots where ffmap = ffmapDefault
instance FFoldable NumberRoots where ffoldMap = ffoldMapDefault
instance FTraversable NumberRoots where
  ftraverse f (NumberRoots roots) = NumberRoots <$> traverse f roots

data Option = Option [Natural] [Int] deriving (Eq, Show)
data SmallGraph = SmallGraph [[Option]] [Int] (Maybe [Natural]) deriving (Show)
instance QC.Arbitrary SmallGraph where
  arbitrary = do
    count <- QC.chooseInt (1, 5)
    classes <- QC.vectorOf count $ do
      alternativesCount <- QC.chooseInt (1, 3)
      QC.vectorOf alternativesCount $ do
        vector <- QC.vectorOf 2 (fromIntegral <$> QC.chooseInt (0, 12))
        arity <- QC.chooseInt (0, 2)
        children <- QC.vectorOf arity (QC.chooseInt (0, count - 1))
        pure (Option vector children)
    rootCount <- QC.chooseInt (0, 3)
    roots <- QC.vectorOf rootCount (QC.chooseInt (0, count - 1))
    bounds <-
      QC.frequency
        [ (2, pure Nothing)
        , (1, Just <$> QC.vectorOf 2 (fromIntegral <$> QC.chooseInt (0, 20)))
        ]
    pure (SmallGraph classes roots bounds)
  shrink (SmallGraph classes roots bounds) =
    [SmallGraph classes [root] bounds | length roots > 1, root <- nub roots]

buildGraph ::
  Backend b m => SmallGraph -> EGraphT b r Synthetic m (NumberRoots (ClassId r))
buildGraph (SmallGraph classes roots _) = do
  dummy <- add (Synthetic (-1) 0 [0, 0] [])
  placeholders <- forM [0 .. length classes - 1] $
    \owner -> add (Synthetic owner (-1) [0, 0] [dummy])
  rebuild
  forM_ placeholders hideNonLeaves
  forM_ (zip [0 ..] classes) $ \(owner, choices) ->
    forM_ (zip [0 ..] choices) $ \(choice, Option cost children) -> do
      alternative <-
        add (Synthetic owner choice cost (map (placeholders !!) children))
      void (union (placeholders !! owner) alternative)
  pure (NumberRoots (map (placeholders !!) roots))

-- Complete assignments first, then ordinary DFS over demanded closure. This
-- has no lower-bound, productive-class or incremental pruning implementation.
oracle :: SmallGraph -> [[Natural]]
oracle (SmallGraph classes roots bounds) =
  nondominated (mapMaybe score (sequence classes))
 where
  score choices = do
    reached <- foldM (visit choices []) [] roots
    let cost =
          foldl'
            (zipWith (+))
            [0, 0]
            [v | owner <- reached, let Option v _ = choices !! owner]
    if maybe True (and . zipWith (<=) cost) bounds then Just cost else Nothing
  visit choices active done owner
    | owner `elem` active = Nothing
    | owner `elem` done = Just done
    | otherwise = do
        let Option _ children = choices !! owner
        reached <- foldM (visit choices (owner : active)) done children
        pure (owner : reached)

nondominated :: [[Natural]] -> [[Natural]]
nondominated vectors =
  sort
    [ v | v <- nub vectors, not (any (\w -> w /= v && and (zipWith (<=) w v)) vectors)
    ]

data Observation
  = Observation [[Natural]] ParetoCoverage ParetoFeasibility [[Natural]] Bool
  deriving (Show)

observe ::
  SmallGraph -> ParetoResult (NumberRoots (Dag Synthetic)) -> Observation
observe (SmallGraph classes expectedRoots _) result =
  Observation
    (map candidateCost candidates)
    (paretoCoverage result)
    (paretoFeasibility result)
    (paretoPendingBounds result)
    (all valid candidates)
 where
  candidates = paretoCandidates result
  valid candidate =
    let NumberRoots roots = candidateValue candidate
        rows = foldl' collect IM.empty roots
        selectedOwners = [owner | (owner, _, _, _) <- IM.elems rows]
        rootOwners = [owner | root <- roots, let Synthetic owner _ _ _ = dagNode root]
        correct (ident, (owner, choice, cost, children)) =
          owner >= 0
            && owner < length classes
            && choice >= 0
            && choice < length (classes !! owner)
            && all (< ident) children
            && case classes !! owner !! choice of
              Option expectedCost expectedChildren ->
                cost == expectedCost
                  && traverse
                    (\child -> (\(o, _, _, _) -> o) <$> IM.lookup child rows) children
                    == Just expectedChildren
        total = foldl' (zipWith (+)) [0, 0] [cost | (_, _, cost, _) <- IM.elems rows]
     in rootOwners == expectedRoots
          && length selectedOwners == length (nub selectedOwners)
          && all correct (IM.toList rows)
          && total == candidateCost candidate
  collect rows root = case dagNode root of
    Synthetic owner choice cost children ->
      if IM.member (dagNodeId root) rows
        then rows
        else
          foldl'
            collect
            ( IM.insert
                (dagNodeId root) (owner, choice, cost, map dagNodeId children) rows
            )
            children

agrees :: SmallGraph -> Word -> QC.Property
agrees specification@(SmallGraph _ _ bounds) budget =
  let expected = oracle specification
      action ::
        Backend b m => Bool -> EGraphT b r Synthetic m (Either ParetoError Observation)
      action unlimited = do
        roots <- buildGraph specification
        result <-
          extractParetoMany
            ( exact
                { paretoUpperBounds = bounds
                , paretoExpansionLimit =
                    if unlimited then Nothing else Just (fromIntegral budget)
                }
            )
            syntheticCost
            roots
        pure (observe specification <$> result)
      check ::
        String ->
        Bool ->
        Either EGraphError (Either ParetoError Observation) ->
        QC.Property
      check backend unlimited answer = QC.counterexample
        ( backend
            ++ ": "
            ++ show (expected, answer)
        )
        $ case answer of
          Right (Right (Observation vectors coverage feasible pending valid)) ->
            QC.conjoin
              [ QC.property valid
              , QC.property (not unlimited || coverage == Complete)
              , if coverage == Complete then vectors QC.=== expected else QC.property True
              , QC.property (feasible /= ProvenInfeasible || null expected)
              , QC.property (feasible /= NoFeasibleFoundYet || null vectors)
              , QC.property (feasible /= FeasibleFound || not (null vectors))
              , QC.property
                  (all (\v -> any (\e -> and (zipWith (<=) e v)) expected) vectors)
              , QC.property
                  ( all
                      ( \e ->
                          any (\v -> and (zipWith (<=) v e)) vectors
                            || any (\b -> and (zipWith (<=) b e)) pending
                      )
                      expected
                  )
              ]
          _ -> QC.property False
   in QC.conjoin
        [ check "mutable" unlimited (runMutable (action unlimited))
          QC..&&. check "persistent" unlimited (runPersistent (action unlimited))
        | unlimited <- [True, False]
        ]
