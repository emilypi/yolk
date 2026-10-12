{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- | Typed identities and recursive terms. Constructors stay inside the library.
module Yolk.Internal.Types where

import Data.Functor.Const (Const (..))
import Data.HKD (FFunctor (..), FTraversable)
import Data.Kind (Constraint, Type)
import Type.Reflection (TypeRep, Typeable)

-- | A language is a family of higher-kinded datatypes, indexed by its sorts.
-- 'operatorKey' identifies a constructor and all of its non-child fields.
-- It must be injective within each result sort, including the child sorts
-- and their order. Children themselves are supplied by the graph.
-- The core keys result sorts separately: equal keys at different sorts
-- never identify the same node.
type Language :: (k -> (k -> Type) -> Type) -> Constraint
class (forall s. FTraversable (l s)) => Language l where
  operatorKey :: l s (Const ()) -> String

-- | Graph-region and sort parameters are nominal: neither can be coerced.
type ClassId :: Type -> k -> Type
data ClassId r s = ClassId !(TypeRep s) !Int
type role ClassId nominal nominal

instance Eq (ClassId r s) where
  ClassId _ a == ClassId _ b = a == b

instance Ord (ClassId r s) where
  compare (ClassId _ a) (ClassId _ b) = compare a b

instance Show (ClassId r s) where
  showsPrec p (ClassId _ n) = showParen (p > 10) (showString "ClassId " . shows n)

-- | A finite, well-sorted tree. Runtime sort witnesses support safe retrieval
-- from heterogeneous storage; no unchecked casts are used by the core.
type Term :: (k -> (k -> Type) -> Type) -> k -> Type
data Term l s where
  Term :: Typeable s => l s (Term l) -> Term l s

-- Kept internal so constructor optics can share the representation without
-- exposing sort witnesses or bypassing rewrite validation in the public API.
data Pattern l s where
  PVar :: !(TypeRep s) -> !String -> Pattern l s
  PNode :: !(TypeRep s) -> !(l s (Pattern l)) -> Pattern l s

data Rewrite l where
  Rewrite :: !String -> !(Pattern l s) -> !(Pattern l s) -> Rewrite l

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
