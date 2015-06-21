{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Syntack.Callgraph
    (
      callsTo
    ) where

import Data.Generics.Zipper (toZipper, getHole)
import Data.Maybe (fromMaybe)
import Language.Java.Syntax
import Language.Java.Syntax.Util (name)
import Syntack.Zipper (ZC, children)

callsTo :: [CompilationUnit] -> [String] -> [ZC]
callsTo cs n = filter (isCallTo n) (concatMap (children (undefined :: Exp) . toZipper) cs)

isCallTo :: [String] -> ZC -> Bool
isCallTo targetName z = go (fromMaybe (error "isCallTo: zipper not pointing to Exp") $ getHole z)
    where
        go :: Exp -> Bool
        go = undefined

        go' :: MethodInvocation -> Bool
        go' (MethodCall n _) = fqName (name n) z == targetName
        go' (PrimaryMethodCall e rts i as) = undefined e rts i as
        go' (SuperMethodCall rts i as) = undefined rts i as
        go' (ClassMethodCall n rts i as) = undefined n rts i as -- n.super.<rts>i(as)
        go' (TypeMethodCall n rts i as) = undefined n rts i as -- n.<rts>i(as)

-- Get fully qualified name from scope
fqName :: [String] -> ZC -> [String]
fqName _ _ = []
