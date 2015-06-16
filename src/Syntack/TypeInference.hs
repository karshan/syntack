module Syntack.TypeInference
    (
      typeOf
    ) where

type Type' = String

typeOf :: Zipper CompilationUnit -> Either String Type'
