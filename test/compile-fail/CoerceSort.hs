-- |
-- Module       : CoerceSort
-- Description  : Compile-fail regression for coercing yolk class identity sorts.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module CoerceSort where

import Data.Coerce (coerce)
import Yolk

badCoerce :: ClassId r 'True -> ClassId r 'False
badCoerce = coerce
