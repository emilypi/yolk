module EscapeRegion where

import Data.Proxy (Proxy)
import Yolk

badEscape :: Backend b m
          => Proxy b
          -> (forall r. EGraphT b r l m (ClassId r s))
          -> m (Either EGraphError (ClassId escaped s))
badEscape p action = runEGraphT p action
