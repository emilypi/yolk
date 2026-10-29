-- |
-- Module       : ParsedParetoOptionsConstructor
-- Description  : Bounded Pareto options require the parsing boundary.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module ParsedParetoOptionsConstructor where
import Yolk.Extract.Pareto (ParsedParetoOptions)
forge :: ParsedParetoOptions
forge = ParsedParetoOptions 0 Nothing Nothing 0 0 0 0 0
