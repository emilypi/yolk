module TermOpticSort where

import Control.Lens (set)
import Yolk
import qualified Yolk.Optics as O

badTerm :: l 'False (Term l) -> Term l 'True -> Term l 'True
badTerm = set O._Term
