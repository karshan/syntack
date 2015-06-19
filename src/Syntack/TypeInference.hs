module Syntack.TypeInference
    (
      typeOf
    ) where

import Syntack.Zipper (ZC)

type Type' = String

typeOf :: ZC -> Either String Type'
typeOf _ = error "wip"
