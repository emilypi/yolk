-- |
-- Module       : InternalClassConstructor
-- Description  : Internal-module visibility does not expose graph identity constructors.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module InternalClassConstructor where

import Type.Reflection (TypeRep)
import Yolk.Internal.Graph (ClassId)

forge :: TypeRep s -> ClassId r s
forge rep = ClassId rep 0
