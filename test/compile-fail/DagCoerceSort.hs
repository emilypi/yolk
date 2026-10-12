module DagCoerceSort where

import Data.Coerce (coerce)
import Yolk

badCoerce :: Dag l 'True -> Dag l 'False
badCoerce = coerce
