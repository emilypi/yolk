-- |
-- Module       : DagOpticSort
-- Description  : Compile-fail regression for changing yolk DAG sorts through optics.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
module DagOpticSort where

import Control.Lens (view)
import Yolk
import qualified Yolk.Optics as O

badLayer :: Dag l 'True -> l 'False (Dag l)
badLayer = view O.dagNode
