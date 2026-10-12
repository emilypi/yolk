-- |
-- Module       : DagCoerceSort
-- Description  : Compile-fail regression for coercing yolk DAG result sorts.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module DagCoerceSort where

import Data.Coerce (coerce)
import Yolk

badCoerce :: Dag l 'True -> Dag l 'False
badCoerce = coerce
