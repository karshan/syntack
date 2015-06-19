module Syntack.TypeInference
    (
      typeOf
    ) where

import Data.Generics.Zipper (query)
import Data.Typeable (cast)

type Type' = String

typeOf :: Zipper CompilationUnit -> Either String Type'
typeOf z = query (\h -> case (cast h) of
                            Just x -> f x z
                            Nothing -> Left "not an exp") z

f :: Exp -> Zipper CompilationUnit -> Either String Type'
f e z = Left "wip"
