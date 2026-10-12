module SortMismatch where

import Yolk

badUnion :: Backend b m => ClassId r 'True -> ClassId r 'False -> EGraphT b r l m Bool
badUnion = union
