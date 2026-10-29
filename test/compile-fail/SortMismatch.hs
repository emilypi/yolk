-- |
-- Module       : SortMismatch
-- Description  : Compile-fail regression for merging yolk classes of different sorts.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module SortMismatch where

import Yolk

badUnion :: Backend b m => ClassId r 'True -> ClassId r 'False -> EGraphT b r l m Bool
badUnion = union
