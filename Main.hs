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
import Data.Maybe (mapMaybe, catMaybes)
import Data.Proxy
import Data.Either

import Language.Java.Syntax
import Language.Java.Parser

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
  
xssMain :: IO ()
xssMain = do
  fcus <- read <$> readFile "index"
  let cus = map snd fcus
  print $ map handlerFuncs cus

handlerFuncs :: CompilationUnit -> [MemberDecl]
handlerFuncs cu = cu ^.. template