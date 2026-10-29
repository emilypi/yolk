-- | Failures at the Pareto configuration and extraction boundaries.
module Yolk.Extract.Pareto.Error
  ( ParetoError (..)
  ) where

data ParetoError
  = InvalidParetoOptions !String
  | ObjectiveDimensionMismatch !Int !Int
  | ParetoInputLimit !String
  deriving (Eq, Show)
