{-# LANGUAGE GADTs #-}

-- |
-- Module       : RewriteConstructor
-- Description  : Rewriting cannot bypass variable validation.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module RewriteConstructor where

import Yolk.Rewrite (Pattern, Rewrite)

forge :: Pattern l s -> Pattern l s -> Rewrite l
forge = Rewrite "unchecked"
