{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Union-find storage for the typed graph core.
--
-- IDs are allocated consecutively from zero. The caller must validate every
-- raw ID before passing it to 'ufFind' or 'ufUnion': an ID must satisfy
-- @0 <= i && i < ufCount store@ and belong to that store. Invalid IDs are
-- outside this module's contract; mutable array accesses are unchecked.
--
-- Both backends use union by size, with the smaller root ID winning ties.
-- Finding a root only reads parents: it never compresses or otherwise changes
-- paths. The algorithm follows Sedgewick and Wayne's weighted quick-union
-- (<https://algs4.cs.princeton.edu/15uf/WeightedQuickUnionUF.java.html source>),
-- with an explicit root-ID tie rule.
--
-- A mutable handle contains its count, capacity, and two unboxed arrays.
-- Operations on one mutable store must be sequential, and callers must thread
-- the handle returned by each update. Old handles are not snapshots: they can
-- share arrays, and their count or capacity may be stale. Persistent handles
-- may instead be retained and branched freely; updates preserve old values.
--
-- 'ufFresh' checks the maximum count before incrementing or allocating.
-- Persistent stores permit at most @maxBound :: Int@ elements. Mutable stores
-- additionally limit each array's byte size to @maxBound :: Int@. Exhausting
-- either limit raises an error instead of allowing arithmetic to wrap.
module Yolk.Backend
  ( Mutable
  , Persistent
  , Backend (..)
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , copyMutablePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (sizeOf)

-- | Mutable storage, usable in any 'PrimMonad', including @ST@ and @IO@.
data Mutable

-- | Immutable storage whose previous versions remain usable after updates.
data Persistent

-- | Storage operations used by the core after it has validated graph IDs.
class Monad m => Backend (b :: Type) m where
  -- | A store of allocated IDs and their equivalence classes.
  data UnionFind b m

  -- | Create a store containing no IDs.
  ufEmpty :: m (UnionFind b m)

  -- | Allocate a singleton and return its ID and the updated handle.
  -- The ID equals the old count. Fails if the maximum count is exhausted;
  -- see the module documentation for the limits of each backend.
  ufFresh :: UnionFind b m -> m (Int, UnionFind b m)

  -- | Read the representative of a valid ID, without modifying the store.
  -- The caller must establish the ID preconditions documented above.
  ufFind :: UnionFind b m -> Int -> m Int

  -- | Merge the classes of two valid IDs using union by size.
  -- The Boolean is 'True' precisely when two previously distinct classes
  -- were merged. The allocated-ID count is unchanged. The caller must
  -- establish the ID preconditions documented above.
  ufUnion :: UnionFind b m -> Int -> Int -> m (Bool, UnionFind b m)

  -- | Number of allocated IDs, rather than the number of distinct classes.
  ufCount :: UnionFind b m -> Int

instance PrimMonad m => Backend Mutable m where
  data UnionFind Mutable m = MutableUF
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    !(MutablePrimArray (PrimState m) Int)
    !(MutablePrimArray (PrimState m) Int)

  ufEmpty = do
    parents <- newPrimArray 0
    sizes <- newPrimArray 0
    pure (MutableUF 0 0 parents sizes)
  {-# INLINABLE ufEmpty #-}

  ufFresh store@(MutableUF count _ _ _) = do
    let !count' = incrementCount maxMutableCount count
    MutableUF _ capacity parents sizes <- ensureCapacity store
    writePrimArray parents count count
    writePrimArray sizes count 1
    pure (count, MutableUF count' capacity parents sizes)
  {-# INLINABLE ufFresh #-}

  ufFind (MutableUF _ _ parents _) = findMutable parents
  {-# INLINABLE ufFind #-}

  ufUnion store@(MutableUF _ _ parents sizes) left right = do
    leftRoot <- findMutable parents left
    rightRoot <- findMutable parents right
    if leftRoot == rightRoot
      then pure (False, store)
      else do
        leftSize <- readPrimArray sizes leftRoot
        rightSize <- readPrimArray sizes rightRoot
        let (!winner, !loser) = linkOrder leftRoot leftSize rightRoot rightSize
            !mergedSize = leftSize + rightSize
        writePrimArray parents loser winner
        writePrimArray sizes winner mergedSize
        pure (True, store)
  {-# INLINABLE ufUnion #-}

  ufCount (MutableUF count _ _ _) = count
  {-# INLINABLE ufCount #-}

instance Monad m => Backend Persistent m where
  data UnionFind Persistent m = PersistentUF
    {-# UNPACK #-} !Int
    !(IntMap Int)
    !(IntMap Int)

  ufEmpty = pure (PersistentUF 0 IntMap.empty IntMap.empty)
  {-# INLINABLE ufEmpty #-}

  ufFresh (PersistentUF count parents sizes) =
    let !count' = incrementCount maxBound count
        !parents' = IntMap.insert count count parents
        !sizes' = IntMap.insert count 1 sizes
    in pure (count, PersistentUF count' parents' sizes')
  {-# INLINABLE ufFresh #-}

  ufFind (PersistentUF _ parents _) node =
    let !root = findPersistent parents node
    in pure root
  {-# INLINABLE ufFind #-}

  ufUnion store@(PersistentUF count parents sizes) left right =
    let !leftRoot = findPersistent parents left
        !rightRoot = findPersistent parents right
    in if leftRoot == rightRoot
      then pure (False, store)
      else
        let !leftSize = sizes IntMap.! leftRoot
            !rightSize = sizes IntMap.! rightRoot
            (!winner, !loser) = linkOrder leftRoot leftSize rightRoot rightSize
            !mergedSize = leftSize + rightSize
            !parents' = IntMap.insert loser winner parents
            !sizes' = IntMap.insert winner mergedSize sizes
        in pure (True, PersistentUF count parents' sizes')
  {-# INLINABLE ufUnion #-}

  ufCount (PersistentUF count _ _) = count
  {-# INLINABLE ufCount #-}

-- Only roots have meaningful sizes. Distinct roots describe disjoint sets,
-- so their size sum is at most the allocated count and cannot overflow.
linkOrder :: Int -> Int -> Int -> Int -> (Int, Int)
linkOrder leftRoot leftSize rightRoot rightSize
  | leftSize > rightSize || (leftSize == rightSize && leftRoot < rightRoot) =
      (leftRoot, rightRoot)
  | otherwise = (rightRoot, leftRoot)
{-# INLINABLE linkOrder #-}

findMutable :: PrimMonad m => MutablePrimArray (PrimState m) Int -> Int -> m Int
findMutable parents = go
  where
    go !node = do
      parent <- readPrimArray parents node
      if parent == node then pure node else go parent
{-# INLINABLE findMutable #-}

findPersistent :: IntMap Int -> Int -> Int
findPersistent parents = go
  where
    go !node =
      let !parent = parents IntMap.! node
      in if parent == node then node else go parent
{-# INLINABLE findPersistent #-}

-- primitive multiplies element count by element size when allocating.
-- Bound that product as well as the public Int count.
maxMutableCount :: Int
maxMutableCount = maxBound `quot` sizeOf (0 :: Int)

incrementCount :: Int -> Int -> Int
incrementCount limit count
  | count >= limit = error "Yolk.Backend.ufFresh: maximum element count reached"
  | otherwise = count + 1
{-# INLINABLE incrementCount #-}

-- Called only after checking that another element fits. Capacity doubles
-- geometrically until it reaches the byte-size limit. Only [0, count) has
-- initialized parent and size entries; spare capacity must never be read.
ensureCapacity :: PrimMonad m => UnionFind Mutable m -> m (UnionFind Mutable m)
ensureCapacity store@(MutableUF count capacity parents sizes)
  | count < capacity = pure store
  | otherwise = do
      let !capacity'
            | capacity == 0 = 16
            | capacity > maxMutableCount `quot` 2 = maxMutableCount
            | otherwise = capacity * 2
      parents' <- newPrimArray capacity'
      sizes' <- newPrimArray capacity'
      copyMutablePrimArray parents' 0 parents 0 count
      copyMutablePrimArray sizes' 0 sizes 0 count
      pure (MutableUF count capacity' parents' sizes')
{-# INLINABLE ensureCapacity #-}
