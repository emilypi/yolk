-- |
-- Module       : EscapeRegion
-- Description  : Compile-fail regression for escaping a yolk graph region.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module EscapeRegion where

import Data.Proxy (Proxy)
import Yolk

badEscape :: Backend b m
          => Proxy b
          -> (forall r. EGraphT b r l m (ClassId r s))
          -> m (Either EGraphError (ClassId escaped s))
badEscape p action = runEGraphT p action
