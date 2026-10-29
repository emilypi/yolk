-- |
-- Module       : InternalSnapshotUpdate
-- Description  : Snapshot inspection does not permit unchecked record updates.
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-3-Clause
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
module InternalSnapshotUpdate where

import Yolk.Internal.Graph (Snapshot, snapshotRows)

forge :: Snapshot r l -> Snapshot r l
forge snapshot = snapshot {snapshotRows = mempty}
