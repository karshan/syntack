{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Main where

import Debug.Trace

import Control.Applicative ((<$>))
import Control.Arrow ((>>>))
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Lens.TH
import Control.Monad ((>=>), ap)
import qualified Data.ByteString.Char8 as BS
import Data.Data (Data)
import Data.Data.Lens (template)
import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe, catMaybes)
import Data.Proxy
import Data.Either

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty (Pretty, prettyPrint)

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Text.Parsec.Error (ParseError)
import Text.Show.Pretty (ppShow)

pprint :: (Show a) => a -> IO ()
pprint = putStrLn . ppShow

parse :: String -> Either ParseError CompilationUnit
parse = parser compilationUnit

parseFile :: FilePath -> IO (Either String CompilationUnit)
parseFile = 
  fmap (either (\(_ :: SomeException) -> Left "open failed") 
               (over _Left show . parse . BS.unpack)) . try . BS.readFile 

getNodes :: (Data a, Data b) => Proxy a -> b -> [a]
getNodes p r = r ^.. template

getNodesRec :: (Data a, Data b) => Proxy a -> b -> [a]
getNodesRec p r = go $ r ^.. template
    where go [] = []
          go xs = xs ++ (go $ concatMap (^.. template) xs)

main :: IO ()
main = do
  (mode:args) <- getArgs
  run mode args

run :: String -> [String] -> IO ()
run "-g" xs = genIndex xs
run _ _ = xssMain

genIndex :: [FilePath] -> IO ()
genIndex files = do
  results <- mapM (\f -> fmap (\a -> (f,a)) $ parseFile f) files
  mapM_ (hPutStrLn stderr) $ [f ++ ": " ++ (filter (/= '\n') a) | (f, Left a) <- results]
  writeFile "index" $ show [(f,a) | (f, Right a) <- results]
  
readIndex :: IO [(FilePath, CompilationUnit)]
readIndex = read <$> readFile "index"

xssMain :: IO ()
xssMain = do
  fcus <- readIndex
  let cus = map snd fcus
  mapM_ (putStrLn . showOne) $ filter isHTTPHandler $ concatMap methods cus

-- This is just the first level. This won't include nested methods, but will include methods inside nested classes
-- Therefore information about the fully qualified name of the method is lost
isHTTPHandler :: MemberDecl -> Bool
isHTTPHandler m = maybe False go $ m ^? _MethodDecl
    where
        go = any (\x -> isType "HttpServletRequest" $ x ^. _FormalParam ^. _2) . (^. _5)

-- Util
-- TODO safeHead
isType :: String -> Type -> Bool
isType s t = maybe False go $ ((^? _RefType) >=> (^? _ClassRefType)) t
    where
        go = (== s) . identString . fst . head . (^. _ClassType)

identString :: Ident -> String
identString x = x ^. _Ident ^. _2

showOne :: (Pretty a, Show a) => a -> String
showOne x = (takeWhile (/= ' ') $ show x) ++ ": " ++ prettyPrint x

-- TODO look for a lens operator that will make this cleaner. something like filter (^# _MethodDecl)
methods :: CompilationUnit -> [MemberDecl]
methods = filter (isJust . (^? _MethodDecl)) . (^.. template)
