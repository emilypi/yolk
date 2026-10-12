module PatternOpticSort where

import Control.Lens (set)
import Yolk
import qualified Yolk.Optics as O

badNode :: l 'False (Pattern l) -> Pattern l 'True -> Pattern l 'True
badNode = set O._Node
