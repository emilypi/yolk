module CoerceSort where

import Data.Coerce (coerce)
import Yolk

badCoerce :: ClassId r 'True -> ClassId r 'False
badCoerce = coerce
