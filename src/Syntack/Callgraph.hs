{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Syntack.Callgraph
    (
      callsTo
    ) where

import Data.Generics.Zipper (toZipper, getHole)
import Data.Maybe (fromMaybe)
import Language.Java.Syntax
import Language.Java.Syntax.Util (name)
import Syntack.Zipper (ZC, zfind)

callsTo :: [CompilationUnit] -> [String] -> [ZC]
callsTo cs n = filter (isCallTo n) $ concatMap (map snd . zfind (\(_ :: MethodInvocation) -> Just ()) . toZipper) cs
            ++ error "fixme: callsTo look for calls to constructors"

isCallTo :: [String] -> ZC -> Bool
isCallTo targetName z = go (fromMaybe (error "zipper not pointing to MethodInvocation") $ getHole z)
    where
        go :: MethodInvocation -> Bool
        go (MethodCall n _) = fqName (name n) z == targetName
        go (PrimaryMethodCall e rts i as) = undefined e rts i as
        go (SuperMethodCall rts i as) = undefined rts i as
        go (ClassMethodCall n rts i as) = undefined n rts i as -- n.super.<rts>i(as)
        go (TypeMethodCall n rts i as) = undefined n rts i as -- n.<rts>i(as)

-- Get fully qualified name from scope
fqName :: [String] -> ZC -> [String]
fqName _ _ = []
