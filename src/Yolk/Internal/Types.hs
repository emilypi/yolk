{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module       : Yolk.Internal.Types
-- Description  : Language laws, finite terms and graph errors.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Shared language and term representations. Graph identities and validated
-- rewrites are defined by their own domains, with opaque constructors.
module Yolk.Internal.Types
  ( Language (..), Term (..), erase, EGraphError (..)
  ) where

import Data.Functor.Const (Const (..))
import Data.HKD (FFunctor (..), FTraversable)
import Data.Kind (Constraint, Type)
import Type.Reflection (Typeable)

-- | A language is a family of higher-kinded datatypes, indexed by its sorts.
-- 'operatorKey' identifies a constructor and all of its non-child fields.
-- It must be injective within each result sort, including the child sorts
-- and their order. Children themselves are supplied by the graph.
-- The core keys result sorts separately: equal keys at different sorts
-- never identify the same node.
type Language :: (k -> (k -> Type) -> Type) -> Constraint
class (forall s. FTraversable (l s)) => Language l where
  operatorKey :: l s (Const ()) -> String

-- | A finite, well-sorted tree. Runtime sort witnesses support safe retrieval
-- from heterogeneous storage; no unchecked casts are used by the core.
type Term :: (k -> (k -> Type) -> Type) -> k -> Type
data Term l s where
  Term :: Typeable s => l s (Term l) -> Term l s

erase :: Language l => l s f -> l s (Const ())
erase = ffmap (const (Const ()))
{-# INLINE erase #-}

data EGraphError
  = DirtyGraph
  | UnknownClass !Int
  | SortMismatch !Int
  | InvalidLimits
  | InvalidBinding !String
  | CostOverflow
  | InvalidExtraction -- ^ Internal inconsistency while reconstructing a typed DAG.
  deriving (Eq, Show)
