{-# LANGUAGE TemplateHaskell #-}
module Main where

import Debug.Trace

import Control.Applicative ((<$>))
import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.TH
import Control.Monad ((>=>))
import Data.Data (Data)
import Data.Data.Lens (template)
import Data.List (intercalate)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Proxy

import Language.Java.Syntax
import Language.Java.Parser

import Text.Parsec.Error (ParseError)
import Text.Show.Pretty (ppShow)

pprint :: (Show a) => a -> IO ()
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

type ClassDecl' = ([Modifier], Ident, [TypeParam], Maybe RefType, [RefType], ClassBody)
type MethodDecl' = ([Modifier], [TypeParam], (Maybe Type), Ident, [FormalParam], [ExceptionType], MethodBody)

members :: ClassDecl -> [MemberDecl]
members x = concat $ catMaybes [ (\x -> x ^. _6 ^. _ClassBody & mapMaybe (^? _MemberDecl)) <$> (x ^? _ClassDecl)
                      , (\x -> x ^. _4 ^. _EnumBody ^. _2 & mapMaybe (^? _MemberDecl)) <$> (x ^? _EnumDecl)
                      ]

localClasses :: MethodDecl' -> [ClassDecl]
localClasses methodDecl = maybe [] (\a -> mapMaybe (^? _LocalClass) $ a ^. _Block) ((\a -> a ^. _7 ^. _MethodBody) methodDecl)

classDecls :: CompilationUnit -> [ClassDecl]
classDecls cu = cu ^. _CompilationUnit ^. _3 & mapMaybe (^? _ClassTypeDecl)

package :: CompilationUnit -> [Ident]
package cu = (maybe [] (\x -> x ^. _PackageDecl ^. _Name ^. _2) $ cu ^. _CompilationUnit ^. _1)

methods :: CompilationUnit -> [([Ident], MemberDecl)]
methods cu = concatMap (methodsHelper $ package cu) (classDecls cu)

methodsHelper :: [Ident] -> ClassDecl -> [([Ident], MemberDecl)]
methodsHelper pak classDecl = (zip (repeat (pak ++ [classDeclName classDecl])) $ (members classDecl)) 
    ++ (concatMap (methodsHelper (pak ++ [classDeclName classDecl])) (mapMaybe (^? _MemberClassDecl) (members classDecl)))
    ++ (concatMap (\m -> concatMap (methodsHelper (pak ++ [m ^. _4])) $ localClasses m) $ mapMaybe (^? _MethodDecl) (members classDecl))

classDeclName :: ClassDecl -> Ident
classDeclName (ClassDecl _ x _ _ _ _) = x
classDeclName (EnumDecl _ x _ _) = x

memberName :: MemberDecl -> Ident
memberName (FieldDecl _ _ (x:_)) = go $ x ^. _VarDecl ^. _1
    where
        go (VarId x) = x
        go (VarDeclArray x) = go x
memberName (MethodDecl _ _ _ x _ _ _) = x
memberName (ConstructorDecl _ _ x _ _ _) = x
memberName (MemberClassDecl x) = classDeclName x
memberName (MemberInterfaceDecl (InterfaceDecl _ x _ _ _)) = x

playground = do
    file <- readFile "test.java"
    either print (mapM_ putStrLn . map myShow . methods) $ parse file

myShow :: ([Ident], MemberDecl) -> String
myShow (name, member) = intercalate "." $ (map (\x -> x ^. _Ident ^. _2) name) ++ [memberName member ^. _Ident ^. _2]
