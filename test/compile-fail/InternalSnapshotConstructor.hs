-- |
-- Module       : InternalSnapshotConstructor
-- Description  : Internal-module visibility does not expose snapshot constructors.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module InternalSnapshotConstructor where
import Yolk.Internal.Graph (Snapshot)
forge :: Snapshot r l
forge = Snapshot mempty mempty
