{-# LANGUAGE ViewPatterns #-}
import           Data.Either (lefts, rights)
import           Data.Generics.Zipper (getHole)
import           Data.List.Split (splitOn)
import           Data.Maybe (mapMaybe)

import           Language.Java.Syntax
import           Language.Java.Parser.Util (parseFile)
import           Language.Java.Pretty (Pretty, prettyPrint)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)

import           Syntack.Callgraph (callsTo)
import           Syntack.Zipper (ZC)

import           Text.Show.Pretty (ppShow)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

usage :: String
usage = "usage: ./syntack <(find $PROJDIR -name \"*.java\") fullmethodname"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn usage
    else run (head args) (args !! 1)

run :: FilePath -> String -> IO ()
run fileOfFiles methodName = do
    fs <- mapM parseFile . lines =<< readFile fileOfFiles
    mapM_ (putStdErrLn . show) $ lefts fs
    mapM_ pps $ mapMaybe (getHole :: ZC -> Maybe Exp)
                         (callsTo (rights fs) $ splitOn "." methodName)
