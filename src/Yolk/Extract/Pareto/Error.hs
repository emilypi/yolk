{-# LANGUAGE DerivingVia #-}

-- | Failures at the Pareto configuration and extraction boundaries.
module Yolk.Extract.Pareto.Error
  ( ParetoError (..)
  , ParetoErrors (..)
  , _ParetoErrors
  ) where

import Control.Lens (Iso', iso)
import Data.List.NonEmpty (NonEmpty)

data ParetoError
  = InvalidParetoOptions !String
  | ObjectiveDimensionMismatch !Int !Int
  | ParetoInputLimit !String
  deriving (Eq, Show)

-- | A nonempty collection of independent failures, in validation order.
newtype ParetoErrors = ParetoErrors {getParetoErrors :: NonEmpty ParetoError}
  deriving stock (Eq, Show)
  deriving (Semigroup) via NonEmpty ParetoError

_ParetoErrors :: Iso' ParetoErrors (NonEmpty ParetoError)
_ParetoErrors = iso getParetoErrors ParetoErrors
