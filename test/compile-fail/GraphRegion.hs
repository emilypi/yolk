module GraphRegion where

import Yolk

badRegion :: Backend b m => ClassId r s -> ClassId other s -> EGraphT b r l m Bool
badRegion = union
