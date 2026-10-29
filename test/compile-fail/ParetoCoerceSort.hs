-- |
-- Module       : ParetoCoerceSort
-- Description  : Reject coercion of a Pareto candidate to a different DAG sort.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module ParetoCoerceSort where

import Data.Coerce (coerce)
import Yolk

badCoerce :: ParetoCandidate (Dag l 'True) -> ParetoCandidate (Dag l 'False)
badCoerce = coerce
