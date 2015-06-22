{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Syntack.Callgraph
    (
      callsTo
    ) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Data.Generics.Zipper (fromZipper, toZipper, getHole, up)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Language.Java.Syntax hiding (Type)
import Language.Java.Syntax.Util (ident, name, className, packageName)
import Syntack.Zipper (ZC, children, upTill, unsafeGetHole)
import Syntack.TypeInference (Type(..), typeOfName)

callsTo :: [CompilationUnit] -> [String] -> [ZC]
callsTo cs n = filter (isCallTo n) (concatMap (children (undefined :: Exp) . toZipper) cs)

isCallTo :: [String] -> ZC -> Bool
isCallTo targetName z = go (fromMaybe (error "isCallTo: zipper not pointing to Exp") $ getHole z)
    where
        go :: Exp -> Bool
        go (MethodInv m) = gomi m
        -- TODO implement finding calls to constructors
        go (InstanceCreation _ (ClassType (map (ident . fst) -> _)) _ _) = False
        go (QualInstanceCreation _ _ _ _ _) = False
        go (ArrayCreate _ _ _) = False
        go _ = False

        gomi :: MethodInvocation -> Bool
        gomi (MethodCall n _) = resolveMethodCall (name n) z == targetName
        gomi (PrimaryMethodCall e rts i as) = False
        gomi (SuperMethodCall rts i as) = False
        gomi (ClassMethodCall n rts i as) = False -- n.super.<rts>i(as)
        gomi (TypeMethodCall n rts i as) = False -- n.<rts>i(as)

resolveMethodCall :: [String] -> ZC -> [String]
resolveMethodCall (i:[]) z = packageName (fromZipper z)
                          ++ [last (unfoldr (fmap ((className . unsafeGetHole) &&& id) .
                                        upTill (undefined :: ClassDecl) <=< up) z)] -- outtermost enclosing class name
                          ++ [i]
resolveMethodCall (reverse -> (i:n)) z = let t = typeToName $ typeOfName (reverse n) z in
                                             typePackageName t ++ t ++ [i]
resolveMethodCall _ _ = []

typeToName :: Type -> [String]
typeToName (PrimT p)  = [show p]
typeToName (ArrayT t) = typeToName t
typeToName (ClassT c) = map fst c

typePackageName :: [String] -> [String]
typePackageName = id
