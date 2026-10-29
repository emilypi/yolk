-- |
-- Module       : ParsedParetoOptionsUpdate
-- Description  : Read-only parsed option optics cannot bypass validation.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module ParsedParetoOptionsUpdate where
import Yolk.Extract.Pareto (ParsedParetoOptions, parsedParetoOptions, defaultParetoOptions)
forge :: ParsedParetoOptions -> ParsedParetoOptions
forge value = value {parsedParetoOptions = defaultParetoOptions 0}
