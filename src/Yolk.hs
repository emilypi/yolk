-- |
-- Module       : Yolk
-- Description  : Many-sorted equality saturation over higher-kinded languages.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Many-sorted e-graphs over higher-kinded languages.
--
-- Construct terms with 'term', use 'runMutable' or 'runPersistent' to scope
-- graph identities, add terms, run validated rewrites, and extract a finite
-- result. The example executable demonstrates a two-sort language.
module Yolk
  ( module Yolk.Language, module Yolk.EGraph, module Yolk.Rewrite
  , module Yolk.Extract, module Yolk.Extract.Dag, module Yolk.Extract.Pareto, Mutable, Persistent, Backend
  ) where

import Yolk.Backend (Backend, Mutable, Persistent)
import Yolk.EGraph
import Yolk.Extract
import Yolk.Extract.Dag
import Yolk.Extract.Pareto
import Yolk.Language
import Yolk.Rewrite
