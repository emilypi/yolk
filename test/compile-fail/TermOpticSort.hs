-- |
-- Module       : TermOpticSort
-- Description  : Compile-fail regression for changing yolk term sorts through optics.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module TermOpticSort where

import Control.Lens (set)
import Yolk
import Yolk.Optics qualified as O

badTerm :: l 'False (Term l) -> Term l 'True -> Term l 'True
badTerm = set O._Term
