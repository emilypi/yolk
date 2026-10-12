module DagOpticSort where

import Control.Lens (view)
import Yolk
import qualified Yolk.Optics as O

badLayer :: Dag l 'True -> l 'False (Dag l)
badLayer = view O.dagNode
