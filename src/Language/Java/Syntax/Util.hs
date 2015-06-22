module Language.Java.Syntax.Util
    (
      ident
    , name
    , className
    , packageName
    , typeName
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

typeName :: Type -> [String]
typeName (PrimType p) = [show p] -- FIXME "Boolean" /= "BooleanT"
typeName (RefType (ArrayType t)) = typeName t
typeName (RefType (ClassRefType (ClassType c))) = map (ident . fst) c
