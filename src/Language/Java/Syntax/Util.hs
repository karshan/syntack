module Language.Java.Syntax.Util
    (
      ident
    , name
    ) where

import Language.Java.Syntax

ident :: Ident -> String
ident (Ident _ s) = s

name :: Name -> [String]
name (Name n) = map ident n
