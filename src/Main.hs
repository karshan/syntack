{-# LANGUAGE ViewPatterns #-}
import           Data.Either (lefts, rights)

import           Language.Java.Parser.Util (parseFile)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)

import           Syntack.Zipper (ppj)

usage :: String
usage = "./syntack file-containing-list-of-filenames"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn usage
    else run (head args)

run :: FilePath -> IO ()
run fileOfFiles = do
    fs <- mapM parseFile . lines =<< readFile fileOfFiles
    mapM_ (putStdErrLn . show) $ lefts fs
    ppj $ head $ rights fs
