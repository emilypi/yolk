{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module       : BenchVectors
-- Description  : Shared yolk benchmark workloads and expected results.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Deterministic public-API workloads shared by benchmarks and regressions.
-- Expected results come from closed forms, not from a second execution of
-- the implementation being timed. All measured runs start with a fresh graph.
module BenchVectors
  ( Vector (..)
  , Workload (..)
  , Expected (..)
  , Outcome (..)
  , vectors
  , runMutableVector
  , runPersistentVector
  , checkOutcome
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, forM, forM_, void)
import Data.HKD
  ( FFoldable (..)
  , FFunctor (..)
  , FTraversable (..)
  , ffmapDefault
  , ffoldMapDefault
  )
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Yolk

data Sort = Value

-- The tag distinguishes synthetic operators; weights are explicit Word64
-- payloads so the benchmark doesn't pay for arbitrary-precision literals.
type Expr :: Sort -> (Sort -> Type) -> Type
data Expr s f where
  Node :: !Int -> !Word64 -> [f 'Value] -> Expr 'Value f

instance FFunctor (Expr s) where
  ffmap = ffmapDefault
instance FFoldable (Expr s) where
  ffoldMap = ffoldMapDefault
instance FTraversable (Expr s) where
  ftraverse f (Node tag weight children) = Node tag weight <$> traverse f children
instance Language Expr where
  operatorKey (Node tag weight children) = show (tag, weight, length children)

nodeWeight :: Expr s f -> Word64
nodeWeight (Node _ weight _) = weight

data Workload
  = Intern !Int !Int -- insertions, distinct payloads
  | Congruence !Int
  | RewriteChain !Int
  | TreeDiamond !Int
  | DagDiamond !Int
  | DagStar !Int !(Maybe Word64)
  | DagCycle !Int !Bool -- class count, include finite exits
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Expected
  = Count !Int
  | Exact !Word64 !Int -- cost, number of nodes in returned representation
  | Bounded !Word64 !Word64 -- independently known optimum, expansion cap
  | Infeasible
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Vector = Vector
  { vectorName :: !String
  , workload :: !Workload
  , expected :: !Expected
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- A strict observation forces costs, bounds, proof status, work count, and
-- traversal of the entire returned tree/DAG. Timing only Maybe/Either to WHNF
-- would not force the extracted witness. We never render syntax while timing.
data Outcome
  = Counted !Int !Bool
  | Extracted !Word64 !Word64 !Bool !Word64 !Int
  | NoFiniteDag
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

vectors :: [Vector]
vectors =
  [ Vector "intern/fresh-256" (Intern 256 256) (Count 256)
  , Vector "intern/repeated-4096" (Intern 4096 64) (Count 64)
  , -- Union retains both distinct leaf operators; rebuilding deduplicates the
    -- two identical compound members at each of the 64 levels: 64 + 2 nodes.
    Vector "rebuild/chains-64" (Congruence 64) (Count 66)
  , Vector "rewrite/identity-chain-32" (RewriteChain 32) (Exact 1 1)
  , Vector "tree/diamond-8" (TreeDiamond 8) (Exact 511 511)
  , Vector "tree/diamond-12" (TreeDiamond 12) (Exact 8191 8191)
  , Vector "dag/diamond-12" (DagDiamond 12) (Exact 13 13)
  , Vector "dag/diamond-128" (DagDiamond 128) (Exact 129 129)
  , Vector "dag/star-6" (DagStar 6 Nothing) (Exact 31 7)
  , Vector "dag/star-10" (DagStar 10 Nothing) (Exact 51 11)
  , Vector "dag/star-16-budget-64" (DagStar 16 (Just 64)) (Bounded 81 64)
  , Vector "dag/cycle-24" (DagCycle 24 True) (Exact 1 24)
  , Vector "dag/infeasible-cycle-24" (DagCycle 24 False) Infeasible
  ]

-- Keep these as functions of the benchmark input. No top-level graph result
-- is cached and reused across samples. Pragmas follow complete definitions.
runMutableVector :: Vector -> Either String Outcome
runMutableVector v = either (Left . show) Right (runMutable (runWorkload (workload v)))
{-# NOINLINE runMutableVector #-}

runPersistentVector :: Vector -> Either String Outcome
runPersistentVector v =
  either (Left . show) Right (runPersistent (runWorkload (workload v)))
{-# NOINLINE runPersistentVector #-}

runWorkload :: Backend b m => Workload -> EGraphT b r Expr m Outcome
runWorkload = \case
  Intern count distinct -> do
    forM_ [0 .. count - 1] $ \i -> void (add (Node (i `mod` distinct) 1 []))
    Counted <$> nodeCount <*> pure True
  Congruence depth -> do
    left <- add (Node 0 1 [])
    right <- add (Node 1 1 [])
    (leftRoot, rightRoot) <-
      foldM
        ( \(a, b) _ ->
            (,) <$> add (Node 2 1 [a, a]) <*> add (Node 2 1 [b, b])
        )
        (left, right)
        [1 .. depth]
    void (union left right)
    rebuild
    Counted <$> nodeCount <*> equivalent leftRoot rightRoot
  RewriteChain depth -> do
    leaf <- add (Node 0 1 [])
    root <- foldM (\child _ -> add (Node 1 1 [child])) leaf [1 .. depth]
    case rewrite "identity" (node (Node 1 1 [var "x"])) (var "x") of
      Left err -> error ("invalid benchmark rule: " ++ show err)
      Right rule -> do
        void (run defaultLimits [rule])
        observeTree <$> extractWith astSize root
  TreeDiamond depth -> diamond depth >>= fmap observeTree . extractWith astSize
  DagDiamond depth -> diamond depth >>= extractDag (DagLimits Nothing)
  DagStar count limit -> do
    children <- forM [1 .. count] $ \i -> do
      expensive <- add (Node (100 + i) 10 [])
      indirect <- add (Node (200 + i) 0 [expensive])
      cheap <- add (Node (300 + i) 5 [])
      void (union indirect cheap)
      pure indirect
    root <- add (Node 0 1 children)
    extractDag (DagLimits limit) root
  DagCycle count finite -> do
    dummy <- add (Node (-1) 0 [])
    classes <- forM [0 .. count - 1] $ \i -> add (Node (100 + i) 0 [dummy])
    rebuild
    forM_ classes hideNonLeaves
    forM_ (zip3 [0 ..] classes (drop 1 classes ++ take 1 classes)) $
      \(i, owner, next) -> do
        step <- add (Node (200 + i) 0 [next])
        void (union owner step)
        if finite
          then do
            exit <- add (Node (300 + i) (fromIntegral (count - i)) [])
            void (union owner exit)
          else pure ()
    case classes of
      [] -> error "benchmark cycle must contain at least one class"
      root : _ -> extractDag (DagLimits Nothing) root

diamond :: Backend b m => Int -> EGraphT b r Expr m (ClassId r 'Value)
diamond depth = do
  leaf <- add (Node 0 1 [])
  foldM (\child _ -> add (Node 1 1 [child, child])) leaf [1 .. depth]

extractDag ::
  Backend b m => DagLimits -> ClassId r 'Value -> EGraphT b r Expr m Outcome
extractDag limits root = do
  result <- extractDagOptimal limits nodeWeight root
  pure $ case result of
    Nothing -> NoFiniteDag
    Just best ->
      Extracted
        (dagCost best)
        (lowerBound best)
        (optimality best == Optimal)
        (expansions best)
        (dagSize (dag best))

observeTree :: Maybe (Word64, Term Expr s) -> Outcome
observeTree Nothing = NoFiniteDag
observeTree (Just (cost, tree)) = Extracted cost cost True 0 (treeSize tree)

treeSize :: Term Expr s -> Int
treeSize tree = 1 + getSum (ffoldMap (Sum . treeSize) (unTerm tree))

checkOutcome :: Vector -> Either String Outcome -> Either String ()
checkOutcome v result = case result of
  Left err -> Left err
  Right actual
    | matches (expected v) actual -> Right ()
    | otherwise ->
        Left
          ( vectorName v
              ++ ": expected "
              ++ show (expected v)
              ++ ", got "
              ++ show actual
          )
 where
  matches (Count n) (Counted actual True) = n == actual
  matches (Exact cost size) (Extracted actual bound True _ actualSize) =
    actual == cost && bound == cost && actualSize == size
  matches (Bounded optimum cap) (Extracted cost bound proven work size) =
    bound <= optimum
      && optimum <= cost
      && work <= cap
      && size > 0
      && ( if proven
            then cost == optimum && bound == cost
            else work == cap && bound < cost
         )
  matches Infeasible NoFiniteDag = True
  matches _ _ = False
