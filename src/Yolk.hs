-- | Many-sorted e-graphs over higher-kinded languages.
--
-- Construct terms with 'term', use 'runMutable' or 'runPersistent' to scope
-- graph identities, add terms, run validated rewrites, and extract a finite
-- result. The example executable demonstrates a two-sort language.
module Yolk
  ( module Yolk.Language, module Yolk.EGraph, module Yolk.Rewrite
  , module Yolk.Extract, Mutable, Persistent, Backend
  ) where

import Yolk.Backend (Backend, Mutable, Persistent)
import Yolk.EGraph
import Yolk.Extract
import Yolk.Language
import Yolk.Rewrite
