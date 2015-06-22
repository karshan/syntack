module Language.Java.Syntax.Util
    (
      ident
    , name
    , className
    , packageName
    ) where

import Language.Java.Syntax

ident :: Ident -> String
ident (Ident _ s) = s

name :: Name -> [String]
name (Name n) = map ident n

className :: ClassDecl -> String
className (ClassDecl _ i _ _ _ _) = ident i
className (EnumDecl _ i _ _) = ident i

packageName :: CompilationUnit -> [String]
packageName (CompilationUnit (Just (PackageDecl n)) _ _) = name n
packageName _ = []
