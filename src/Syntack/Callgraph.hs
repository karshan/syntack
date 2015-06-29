{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Syntack.Callgraph
    (
      callsTo
    ) where

import Control.Lens ((^?))
import Data.Either.Util (maybeToEither)
import Data.Generics.Zipper (toZipper, getHole)
import Language.Java.Syntax hiding (Type)
import Language.Java.Syntax.Util (ident)
import Syntack.Zipper (ZC, children)

m_e :: e -> Maybe a -> Either e a
m_e = maybeToEither

-- TODO: error propogation (isCallTo's errors are silently surpressed)
callsTo :: [CompilationUnit] -> ZC -> [ZC]
callsTo cs target = filter (either (const False) id . (isCallTo target)) (concatMap (children (undefined :: Exp) . toZipper) cs)

isCallTo :: ZC -> ZC -> Either String Bool
isCallTo target z = do
    z' <- m_e "zipper not pointing to Exp" $ getHole z
    t <- m_e "target not pointing to MemberDecl" $ (getHole :: ZC -> Maybe MemberDecl) target
    _ <- m_e "target not pointing to MethodDecl" $ t ^? _MethodDecl
    return $ go z'
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
