{-# LANGUAGE ViewPatterns #-}
import           Control.Monad ((<=<))

import           Language.Java.Syntax
import           Language.Java.Parser.Util (parseFile)
import           Language.Java.Pretty (Pretty, prettyPrint)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)

import           Syntack.Zipper (upTill, posToZipper)
import           Syntack.TypeInference (typeOf)

import           Text.Show.Pretty (ppShow)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

usage :: String
usage = "./syntack javasrcfile line col"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then
        putStrLn usage
    else run (head args) (read $ args !! 1) (read $ args !! 2)

run :: FilePath -> Int -> Int -> IO ()
run file line col = do
    cu <- parseFile file
    either (putStdErrLn . show) 
           (maybe (putStrLn "posToExp failed") 
                  (print . typeOf) . (upTill (undefined :: Exp) <=< posToZipper line col))
           cu
