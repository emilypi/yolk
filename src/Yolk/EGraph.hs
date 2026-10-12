-- | The region-scoped e-graph API. See 'Yolk.Language' for the language laws.
-- Search snapshots are immutable; adding a node repairs pending congruence.
-- Explicit member/lookup queries reject dirty graphs until 'rebuild'.
module Yolk.EGraph
  ( EGraphT, EGraphError (..), Snapshot
  , runEGraphT, runMutable, runPersistent
  , add, addTerm, lookupNode, union, equivalent, findClass
  , rebuild, classNodes, nodeCount, hideNonLeaves
  , freeze, snapshotNodes
  ) where

import Yolk.Internal.Graph
import Yolk.Internal.Types (EGraphError (..))
