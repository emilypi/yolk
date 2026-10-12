-- |
-- Module       : GraphRegion
-- Description  : Compile-fail regression for mixing yolk graph regions.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module GraphRegion where

import Yolk

badRegion :: Backend b m => ClassId r s -> ClassId other s -> EGraphT b r l m Bool
badRegion = union
