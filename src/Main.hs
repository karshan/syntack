import           Control.Exception (SomeException, try)
import           Control.Lens (_1, _2, _Left, over)

import           Data.Either (lefts, rights)

import           Language.Java.Syntax
import           Language.Java.Parser.Util (parseFile)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)
import           Text.Parsec (ParseError)

import           Syntack.Zipper

usage :: String
usage = "./syntack file-containing-list-of-filenames"

main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn usage
    else run (head args)

run :: FilePath -> IO ()
run fileOfFiles = do
    fs <- mapM parseFile . lines =<< readFile fileOfFiles
    mapM_ (putStdErrLn . show) $ lefts fs
    nameme $ head $ rights fs
