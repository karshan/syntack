{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Syntack.Callgraph
    (
      callsTo
    ) where

import Data.Generics.Zipper (toZipper, getHole)
import Data.Maybe (fromMaybe)
import Language.Java.Syntax hiding (Type)
import Language.Java.Syntax.Util (ident)
import Syntack.Zipper (ZC, children)

callsTo :: [CompilationUnit] -> ZC -> [ZC]
callsTo cs n = filter (isCallTo n) (concatMap (children (undefined :: Exp) . toZipper) cs)

isCallTo :: ZC -> ZC -> Bool
isCallTo _ z = go (fromMaybe (error "isCallTo: zipper not pointing to Exp") $ getHole z)
    where
        go :: Exp -> Bool
        go (MethodInv m) = gomi m
        -- TODO implement finding calls to constructors
        go (InstanceCreation _ (ClassType (map (ident . fst) -> _)) _ _) = False
        go (QualInstanceCreation _ _ _ _ _) = False
        go (ArrayCreate _ _ _) = False
        go _ = False

        gomi :: MethodInvocation -> Bool
        gomi (MethodCall _ _) = False
        gomi (PrimaryMethodCall _ _ _ _) = False
        gomi (SuperMethodCall _ _ _) = False
        gomi (ClassMethodCall _ _ _ _) = False -- n.super.<rts>i(as)
        gomi (TypeMethodCall _ _ _ _) = False -- n.<rts>i(as)
