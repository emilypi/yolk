-- |
-- Module       : DagExtractionTests
-- Description  : Optimality, sharing, and budget tests for yolk DAG extraction.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module DagExtractionTests (dagExtractionTests) where

import Control.Monad (foldM, forM, forM_, void)
import Data.Functor.Const (Const (..))
import Data.HKD (FFoldable (..), FFunctor (..), FTraversable (..), ffoldMapDefault, ffmapDefault)
import qualified Data.IntMap.Strict as IM
import Data.Kind (Type)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (..))
import Data.Word (Word64)
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import TestLanguage
import TestSupport
import Yolk

unlimited :: DagLimits
unlimited = DagLimits Nothing

rendered :: Maybe (DagResult (Dag Math s)) -> Maybe (Word64, Word64, Optimality, String)
rendered = fmap (\result ->
  (dagCost result, lowerBound result, optimality result, pretty (dagToTerm (dag result))))

-- The larger tree shares a three-node subexpression. Its DAG has four nodes;
-- the smaller tree has five distinct nodes, so the two objectives disagree.
sharingAlternatives :: Backend b m => EGraphT b r Math m (ClassId r 'Number)
sharingAlternatives = do
  shared <- addTerm (times (plus (number 1) (number 2)) (plus (number 1) (number 2)))
  smallTree <- addTerm (plus (number 3) (plus (number 4) (number 2)))
  void (union shared smallTree)
  pure shared

dagExtractionTests :: TestTree
dagExtractionTests = testGroup "solver-free optimal DAG extraction"
  [ both "sharing makes the optimal DAG differ from the optimal tree"
      (Just (5, "(3 + (4 + 2))"), Just (4, 4, Optimal, "((1 + 2) * (1 + 2))")) $
      Program $ do
        root <- sharingAlternatives
        tree <- extractWith astSize root
        result <- extractDagOptimal unlimited dagSizeCost root
        pure (fmap (\(cost, witness) -> (cost, pretty witness)) tree, rendered result)
  , both "a different node-weight model changes the optimal DAG"
      (Just (4, 4, Optimal, "((1 + 2) * (1 + 2))"),
       Just (5, 5, Optimal, "(3 + (4 + 2))")) $ Program $ do
        root <- sharingAlternatives
        ordinary <- extractDagOptimal defaultDagLimits dagSizeCost root
        weighted <- extractDagOptimal unlimited multiplicationWeight root
        pure (rendered ordinary, rendered weighted)
  , both "duplicate children share IDs and monadic folds visit each node once"
      (Just (4, True, [0, 1, 2, 3], True, 7, 7, ["add", "literal:1", "literal:2", "mul"])) $
      Program $ do
        root <- addTerm (times (plus (number 1) (number 2)) (plus (number 1) (number 2)))
        result <- extractDagOptimal unlimited dagSizeCost root
        pure $ fmap (\answer ->
          let witness = dag answer
              (visits, Const monadicSize) = foldDagM recordVisit witness
              Const pureSize = foldDag treeSizeAlgebra witness
              sameChildren = case dagNode witness of
                Mul left right -> dagNodeId left == dagNodeId right
                _ -> False
              (identifiers, postorder) = mathIds witness
          in (dagSize witness, sameChildren, sort (nub identifiers), postorder,
              monadicSize, pureSize, sort visits)) result
  , both "zero-cost mutual recursion still produces a finite equivalent witness"
      (Just (0, 0, Optimal), True) $ Program $ do
        leaf <- add (Lit 7)
        one <- add (Lit 1)
        a <- add (Add leaf one)
        b <- add (Mul a one)
        void (union leaf b)
        result <- extractDagOptimal unlimited (const 0) a
        equivalentWitness <- case result of
          Nothing -> pure False
          Just answer -> addTerm (dagToTerm (dag answer)) >>= equivalent a
        pure (fmap certificate result, equivalentWitness)
  , both "hidden nonleaves cannot undercut a visible leaf"
      (Just (100, 100, Optimal, "5")) $ Program $ do
        root <- addTerm (plus (number 2) (number 3))
        leaf <- add (Lit 5)
        void (union root leaf)
        rebuild
        hideNonLeaves root
        rendered <$> extractDagOptimal unlimited hiddenWeight root
  , both "a visible zero-cost cycle with no finite derivation is infeasible even at budget zero"
      (Nothing :: Maybe Word64, Nothing :: Maybe Word64) $ Program $ do
        base <- addTerm (plus (number 2) (number 3))
        one <- add (Lit 1)
        recursive <- add (Mul base one)
        rebuild
        hideNonLeaves base
        void (union base recursive)
        exact <- extractDagOptimal unlimited (const 0) recursive
        bounded <- extractDagOptimal (DagLimits (Just 0)) (const 0) recursive
        pure (dagCost <$> exact, dagCost <$> bounded)
  , both "dirty congruence repairs stale root and child IDs before extraction"
      (Just (2, 2, Optimal), True) $ Program $ do
        one <- add (Lit 1)
        two <- add (Lit 2)
        first <- add (Add one one)
        stale <- add (Add two two)
        void (union one two)
        result <- extractDagOptimal unlimited dagSizeCost stale
        equivalentWitness <- case result of
          Nothing -> pure False
          Just answer -> addTerm (dagToTerm (dag answer)) >>= equivalent first
        pure (fmap certificate result, equivalentWitness)
  , both "mixed-sort roots optimize jointly and duplicate roots share their selected node"
      (Just 3, Just 3, Just (5, 5, Optimal, True, "(0 + 0)", "(0 == 0)", [0, 1, 2])) $
      Program $ do
        shared <- add (Lit 0)
        numberRoot <- add (Lit 1)
        booleanRoot <- add (BoolLit True)
        numberAlternative <- add (Add shared shared)
        booleanAlternative <- add (Equal shared shared)
        void (union numberRoot numberAlternative)
        void (union booleanRoot booleanAlternative)
        separateNumber <- extractDagOptimal unlimited jointWeight numberRoot
        separateBoolean <- extractDagOptimal unlimited jointWeight booleanRoot
        together <- extractDagMany unlimited jointWeight
          (MixedRoots [numberRoot, numberRoot] [booleanRoot])
        pure (dagCost <$> separateNumber, dagCost <$> separateBoolean,
              fmap observeMixed together)
  , both "an empty root family is the empty optimal DAG at budget zero"
      (Just (0, 0, Optimal, 0, 0)) $ Program $ do
        result <- extractDagMany (DagLimits (Just 0)) dagSizeCost (MixedRoots [] [])
        pure $ fmap (\answer -> case dag answer of
          MixedRoots numbers booleans ->
            (dagCost answer, lowerBound answer, optimality answer,
             expansions answer, length numbers + length booleans)) result
  , both "saturating totals stay feasible and cannot wrap below a cheaper alternative"
      (Just (maxBound, maxBound, Optimal), Just (100, 100, Optimal, "3")) $ Program $ do
        huge <- addTerm (plus (number 1) (number 2))
        saturated <- extractDagOptimal unlimited hugeWeight huge
        cheap <- add (Lit 3)
        void (union huge cheap)
        finite <- extractDagOptimal unlimited hugeWeight huge
        pure (certificate <$> saturated, rendered finite)
  , both "a budget-zero bound can prove a zero-cost solution optimal"
      (Just (0, 0, Optimal, 0)) $ Program $ do
        root <- addTerm (plus (number 1) (number 1))
        result <- extractDagOptimal (DagLimits (Just 0)) (const 0) root
        pure $ fmap (\answer ->
          (dagCost answer, lowerBound answer, optimality answer, expansions answer)) result
  , QC.testProperty "exhaustive class choices certify exact and bounded results on both backends" $
      \(specification :: SmallGraph) -> QC.forAll (QC.choose (1, 6)) $ \budget ->
        QC.classify (oracle specification == Nothing) "no finite derivation" $
        QC.classify (hasSelfEdge specification) "self-recursive alternative" $
        QC.classify (hasSharedChild specification) "repeated child" $
        agreesWithOracle specification budget
  , QC.testProperty "cycle traps and unreachable cycles agree with hand-computed optima" $
      QC.once $ QC.conjoin
        [ QC.counterexample label $ QC.conjoin
            [ oracle specification QC.=== expected
            , agreesWithOracle specification 2
            ]
        | (label, specification, expected) <-
            [ ("mutually cheapest choices form a cycle",
               SmallGraph [[Option 0 [1], Option 5 []], [Option 0 [0], Option 4 []]] [0], Just 4)
            , ("joint roots must choose one consistent acyclic representative",
               SmallGraph [[Option 0 [1], Option 3 []], [Option 0 [0], Option 3 []]] [0, 1], Just 3)
            , ("unreachable cyclic component does not destroy a finite root",
               SmallGraph [[Option 2 []], [Option 0 [1]]] [0], Just 2)
            , ("mutual recursion without a leaf is infeasible",
               SmallGraph [[Option 0 [1]], [Option 0 [0]]] [0], Nothing)
            ]
        ]
  ]
  where
    multiplicationWeight :: Math s f -> Word64
    multiplicationWeight Mul {} = 10
    multiplicationWeight _ = 1

    hiddenWeight :: Math s f -> Word64
    hiddenWeight (Lit 5) = 100
    hiddenWeight _ = 0

    jointWeight :: Math s f -> Word64
    jointWeight Lit {} = 3
    jointWeight BoolLit {} = 3
    jointWeight _ = 1

    hugeWeight :: Math s f -> Word64
    hugeWeight (Lit 1) = maxBound
    hugeWeight (Lit 2) = 10
    hugeWeight (Lit 3) = 100
    hugeWeight _ = 1

certificate :: DagResult a -> (Word64, Word64, Optimality)
certificate result = (dagCost result, lowerBound result, optimality result)

treeSizeAlgebra :: Math s (Const Int) -> Const Int s
treeSizeAlgebra n = Const (1 + getSum (ffoldMap (Sum . getConst) n))

-- The pair's Writer monad requires only base. The effect records actual
-- algebra invocations; summing child values alone would not test memoization.
recordVisit :: Math s (Const Int) -> ([String], Const Int s)
recordVisit n = ([operatorKey (ffmap (const (Const ())) n)], treeSizeAlgebra n)

mathIds :: Dag Math s -> ([Int], Bool)
mathIds root =
  let children = ffoldMap (\child ->
        let (identifiers, valid) = mathIds child
        in [(identifiers, valid && dagNodeId child < dagNodeId root)]) (dagNode root)
  in (dagNodeId root : concatMap fst children, all snd children)

type MixedRoots :: (Sort -> Type) -> Type
data MixedRoots f = MixedRoots [f 'Number] [f 'Boolean]

instance FFunctor MixedRoots where
  ffmap = ffmapDefault

instance FFoldable MixedRoots where
  ffoldMap = ffoldMapDefault

instance FTraversable MixedRoots where
  ftraverse f (MixedRoots numbers booleans) =
    MixedRoots <$> traverse f numbers <*> traverse f booleans

observeMixed :: DagResult (MixedRoots (Dag Math))
  -> (Word64, Word64, Optimality, Bool, String, String, [Int])
observeMixed answer = case dag answer of
  MixedRoots [first, duplicate] [booleanRoot] ->
    let (numberIds, numberOrder) = mathIds first
        (booleanIds, booleanOrder) = mathIds booleanRoot
        shared = case (dagNode first, dagNode booleanRoot) of
          (Add a b, Equal c d) ->
            all (== dagNodeId a) [dagNodeId b, dagNodeId c, dagNodeId d]
          _ -> False
    in (dagCost answer, lowerBound answer, optimality answer,
        dagNodeId first == dagNodeId duplicate && shared && numberOrder && booleanOrder,
        pretty (dagToTerm first), pretty (dagToTerm booleanRoot),
        sort (nub (numberIds ++ booleanIds)))
  _ -> (dagCost answer, lowerBound answer, optimality answer, False, "", "", [])

-- Each generated operator carries its class and alternative labels, preventing
-- accidental congruence between different oracle classes. We assert synthetic
-- equalities explicitly; no algebraic interpretation of these labels is needed.
type Synthetic :: Sort -> (Sort -> Type) -> Type
data Synthetic s f where
  Synthetic :: Int -> Int -> Word64 -> [f 'Number] -> Synthetic 'Number f

instance FFunctor (Synthetic s) where
  ffmap = ffmapDefault

instance FFoldable (Synthetic s) where
  ffoldMap = ffoldMapDefault

instance FTraversable (Synthetic s) where
  ftraverse f (Synthetic owner choice weight children) =
    Synthetic owner choice weight <$> traverse f children

instance Language Synthetic where
  operatorKey (Synthetic owner choice weight children) =
    show (owner, choice, weight, length children)

syntheticWeight :: Synthetic s f -> Word64
syntheticWeight (Synthetic _ _ weight _) = weight

type NumberRoots :: (Sort -> Type) -> Type
newtype NumberRoots f = NumberRoots [f 'Number]

instance FFunctor NumberRoots where
  ffmap = ffmapDefault

instance FFoldable NumberRoots where
  ffoldMap = ffoldMapDefault

instance FTraversable NumberRoots where
  ftraverse f (NumberRoots roots) = NumberRoots <$> traverse f roots

data Option = Option Word64 [Int]
  deriving stock (Eq, Show)

data SmallGraph = SmallGraph [[Option]] [Int]
  deriving stock Show

instance QC.Arbitrary SmallGraph where
  arbitrary = do
    count <- QC.chooseInt (1, 5)
    classes <- QC.vectorOf count $ do
      alternatives <- QC.chooseInt (1, 3)
      QC.vectorOf alternatives $ do
        weight <- QC.frequency [(9, QC.choose (0, 12)),
                                (1, QC.elements [maxBound, maxBound - 1])]
        arity <- QC.frequency [(4, pure 0), (3, pure 1), (3, pure 2)]
        children <- QC.vectorOf arity (QC.chooseInt (0, count - 1))
        pure (Option weight children)
    rootCount <- QC.chooseInt (1, 3)
    roots <- QC.vectorOf rootCount (QC.chooseInt (0, count - 1))
    pure (SmallGraph classes roots)
  shrink (SmallGraph classes roots) =
    [SmallGraph (replaceAt owner (replaceAt choice smaller alternatives) classes) roots
    | (owner, alternatives) <- zip [0 ..] classes
    , (choice, Option weight children) <- zip [0 ..] alternatives
    , smaller <- [Option smallerWeight children | smallerWeight <- QC.shrink weight]
              ++ [Option weight [] | not (null children)]
    ] ++ [SmallGraph classes [root] | length roots > 1, root <- nub roots]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index value values = take index values ++ value : drop (index + 1) values

buildGraph :: Backend b m => Bool -> SmallGraph
  -> EGraphT b r Synthetic m [ClassId r 'Number]
buildGraph reverseInsertion (SmallGraph classes roots) = do
  dummy <- add (Synthetic (-1) 0 0 [])
  placeholders <- forM [0 .. length classes - 1] $ \owner ->
    add (Synthetic owner (-1) 0 [dummy])
  rebuild
  -- Nonleaf placeholders let us construct cycles, but are hidden before real
  -- alternatives are added. They must never supply a spurious finite witness.
  forM_ placeholders hideNonLeaves
  let entries = [(owner, choice, option)
                | (owner, alternatives) <- zip [0 ..] classes
                , (choice, option) <- zip [0 ..] alternatives]
  forM_ (if reverseInsertion then reverse entries else entries) $ \(owner, choice, Option weight children) -> do
    member <- add (Synthetic owner choice weight (map (placeholders !!) children))
    void (union (placeholders !! owner) member)
  pure (map (placeholders !!) roots)

-- Independent exhaustive oracle: select one alternative for every class, then
-- traverse only the demanded closure and discard cyclic choices. Unreachable
-- cycles are harmless. The test graph has at most 3^5 complete assignments.
oracle :: SmallGraph -> Maybe Word64
oracle (SmallGraph classes roots) = case mapMaybe score assignments of
  [] -> Nothing
  costs -> Just (minimum costs)
  where
    assignments = sequence classes
    score choices = do
      reachable <- foldM (visit choices []) [] roots
      pure (sumWeights [weight | owner <- reachable, let Option weight _ = choices !! owner])
    visit choices active done owner
      | owner `elem` active = Nothing
      | owner `elem` done = Just done
      | otherwise = do
          let Option _ children = choices !! owner
          visited <- foldM (visit choices (owner : active)) done children
          pure (owner : visited)

-- Deliberately use unbounded arithmetic in the oracle, independent of the
-- extractor's saturating-add implementation. Generated problems are tiny.
sumWeights :: [Word64] -> Word64
sumWeights = fromInteger . min (toInteger (maxBound :: Word64)) . sum . map toInteger

data Row = Row Int Int Word64 [Int]
  deriving stock (Eq, Show)

data Observation = Observation
  { costSeen :: Word64
  , boundSeen :: Word64
  , proofSeen :: Optimality
  , expansionsSeen :: Word64
  , measuredCost :: Word64
  , validWitness :: Bool
  }
  deriving stock Show

observe :: SmallGraph -> DagResult (NumberRoots (Dag Synthetic)) -> Observation
observe (SmallGraph classes requested) answer =
  let NumberRoots roots = dag answer
      (consistent, rows) = foldl' collect (True, IM.empty) roots
      owners = [owner | Row owner _ _ _ <- IM.elems rows]
      rootOwners = [owner | root <- roots, let Synthetic owner _ _ _ = dagNode root]
      validRow (identifier, Row owner choice weight children) =
        owner >= 0 && owner < length classes &&
        choice >= 0 && choice < length (classes !! owner) &&
        all (< identifier) children &&
        case classes !! owner !! choice of
          Option expectedWeight expectedChildren ->
            weight == expectedWeight &&
            traverse (\child -> case IM.lookup child rows of
              Just (Row childOwner _ _ _) -> Just childOwner
              Nothing -> Nothing) children == Just expectedChildren
  in Observation (dagCost answer) (lowerBound answer) (optimality answer)
       (expansions answer) (sumWeights [weight | Row _ _ weight _ <- IM.elems rows])
       (consistent && rootOwners == requested && length owners == length (nub owners) &&
        IM.keys rows == [0 .. IM.size rows - 1] && all validRow (IM.toList rows))
  where
    collect :: (Bool, IM.IntMap Row) -> Dag Synthetic 'Number -> (Bool, IM.IntMap Row)
    collect (consistent, rows) root = case dagNode root of
      Synthetic owner choice weight children ->
        let identifier = dagNodeId root
            row = Row owner choice weight (map dagNodeId children)
        in case IM.lookup identifier rows of
          Just previous -> (consistent && row == previous, rows)
          Nothing -> foldl' collect (consistent, IM.insert identifier row rows) children

agreesWithOracle :: SmallGraph -> Word64 -> QC.Property
agreesWithOracle specification budget =
  let expected = oracle specification
      requests = [(unlimited, False), (DagLimits (Just 0), False),
                  (DagLimits (Just budget), False), (unlimited, True)]
      action :: Backend b m => DagLimits -> Bool -> EGraphT b r Synthetic m (Maybe Observation)
      action limits reverseInsertion = do
        roots <- buildGraph reverseInsertion specification
        result <- extractDagMany limits syntheticWeight (NumberRoots roots)
        pure (observe specification <$> result)
      check backend limits = \case
        Left err -> QC.counterexample (backend ++ ": " ++ show err) False
        Right result -> QC.counterexample backend (checkCertificate expected limits result)
  in QC.conjoin
       [ QC.conjoin [check "mutable" limits (runMutable (action limits reverseInsertion)),
                     check "persistent" limits (runPersistent (action limits reverseInsertion))]
       | (limits, reverseInsertion) <- requests
       ]

checkCertificate :: Maybe Word64 -> DagLimits -> Maybe Observation -> QC.Property
checkCertificate expected limits result = QC.counterexample (show (expected, expansionLimit limits, result)) $
  case (expected, result) of
    (Nothing, Nothing) -> QC.property True
    (Just optimum, Just observation) -> QC.conjoin
      [ QC.counterexample "invalid DAG witness" (validWitness observation)
      , QC.counterexample "reported cost does not score the returned DAG"
          (costSeen observation QC.=== measuredCost observation)
      , QC.counterexample "lower bound exceeds the independently enumerated optimum"
          (boundSeen observation <= optimum)
      , QC.counterexample "incumbent is cheaper than the independently enumerated optimum"
          (optimum <= costSeen observation)
      , case proofSeen observation of
          Optimal -> (boundSeen observation, costSeen observation) QC.=== (optimum, optimum)
          Unproven -> QC.conjoin
            [ QC.counterexample "unproven result must have exhausted its expansion cap"
                (expansionLimit limits QC.=== Just (expansionsSeen observation))
            , QC.counterexample "coincident bounds prove optimality"
                (boundSeen observation < costSeen observation)
            ]
      , case expansionLimit limits of
          Nothing -> QC.property True
          Just cap -> QC.counterexample "expansion budget exceeded"
            (expansionsSeen observation <= cap)
      ]
    _ -> QC.counterexample "finite-derivation existence differs from the oracle" False

hasSelfEdge :: SmallGraph -> Bool
hasSelfEdge (SmallGraph classes _) = or
  [owner `elem` children | (owner, alternatives) <- zip [0 ..] classes,
                          Option _ children <- alternatives]

hasSharedChild :: SmallGraph -> Bool
hasSharedChild (SmallGraph classes _) = or
  [length children /= length (nub children) | alternatives <- classes,
                                             Option _ children <- alternatives]
