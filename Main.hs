{-# LANGUAGE TemplateHaskell #-}
module Main where

import Debug.Trace

import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.TH
import Control.Monad ((>=>))
import Data.Data (Data)
import Data.Data.Lens (template)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Proxy

import Language.Java.Syntax
import Language.Java.Parser

import Text.Parsec.Error (ParseError)
import Text.Show.Pretty (ppShow)

pprint = putStrLn . ppShow

parse :: String -> Either ParseError CompilationUnit
parse = parser compilationUnit

getNodes :: (Data a, Data b) => Proxy a -> b -> [a]
getNodes p r = r ^.. template

getNodesRec :: (Data a, Data b) => Proxy a -> b -> [a]
getNodesRec p r = go $ r ^.. template
    where go [] = []
          go xs = xs ++ (go $ concatMap (^.. template) xs)

h cu = do
    cb <- cu ^. _CompilationUnit ^. _3 & head & ((^? _ClassTypeDecl) >=> (^? _ClassDecl))
    let mbs = cb ^. _6 ^. _ClassBody & mapMaybe ((^? _MemberDecl) >=> (^? _MethodDecl))
    let bs = mbs & mapMaybe (\x -> x ^. _7 ^. _MethodBody) & map (\x -> x ^. _Block)
    let tries = bs & (map.mapMaybe) ((^? _BlockStmt) >=> (^? _Try)) 
    return $ filter (and . flip map [tryWithFromXml, catchWithGetMessage] . flip id) (concat tries)

tryWithFromXml :: (Block, [Catch], Maybe Block) -> Bool
tryWithFromXml (tryBlock, _, _) = any (getNodes (Proxy :: Proxy Name) >>> any (\x -> any (\(Ident _ s) -> s == "fromXml") $ x ^. _Name ^. _2)) 
                                    $ getNodesRec (Proxy :: Proxy MethodInvocation) tryBlock

catchWithGetMessage :: (Block, [Catch], Maybe Block) -> Bool
catchWithGetMessage (_, catches, _) = any go catches
    where go :: Catch -> Bool
          go (Catch (FormalParam _ _ _ (VarId (Ident _ exceptionVar))) block) 
                  = any (go' exceptionVar) $ getNodesRec (Proxy :: Proxy MethodInvocation) block
          go _ = False

          go' :: String -> MethodInvocation -> Bool
          go' exceptionVar (MethodCall (Name _ ((Ident _ e):(Ident _ methodName):_)) _) = e == exceptionVar && methodName == "getMessage"
          go' _ _ = False

playground = do
    file <- readFile "test.java"
    either print (pprint . h) $ parse file