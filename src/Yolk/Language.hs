{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module       : Yolk.Language
-- Description  : Many-sorted higher-kinded languages and finite terms.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Many-sorted higher-kinded languages and finite terms.
-- Traversal uses the @hkd@ package's 'FTraversable' interface.
module Yolk.Language
  ( Language (..), ClassId, Term, term, unTerm, foldTerm
  ) where

import Data.HKD (FFunctor (..))
import Type.Reflection (Typeable)
import Yolk.Internal.Types

-- | Construct a term at its known result sort.
term :: Typeable s => l s (Term l) -> Term l s
term = Term

-- | Inspect the root constructor of a finite term.
unTerm :: Term l s -> l s (Term l)
unTerm (Term n) = n

-- | Fold an indexed term without erasing its sorts.
foldTerm :: Language l => (forall t. l t f -> f t) -> Term l s -> f s
foldTerm alg (Term n) = alg (ffmap (foldTerm alg) n)
