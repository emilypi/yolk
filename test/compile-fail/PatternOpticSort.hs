-- |
-- Module       : PatternOpticSort
-- Description  : Compile-fail regression for changing yolk pattern sorts through optics.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module PatternOpticSort where

import Control.Lens (set)
import Yolk
import qualified Yolk.Optics as O

badNode :: l 'False (Pattern l) -> Pattern l 'True -> Pattern l 'True
badNode = set O._Node
