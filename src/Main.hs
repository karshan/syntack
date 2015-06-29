{-# LANGUAGE ViewPatterns #-}
import           Control.Lens ((&))
import           Data.Either (lefts, rights)
import           Data.Generics.Zipper (getHole)
import           Data.Maybe (mapMaybe)

import           Language.Java.Syntax
import           Language.Java.Parser.Util (parseFile)
import           Language.Java.Pretty (Pretty, prettyPrint)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)

import           Syntack.Callgraph (callsTo)
import           Syntack.Zipper (ZC, posToZipper, upTill)

import           Text.Show.Pretty (ppShow)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

usage :: String
usage = "usage: find $PROJDIR -name \"*.java\" > files\n" ++
        "./syntack files file line col"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then
        putStrLn usage
    else run (head args) (args !! 1) (read $ args !! 2) (read $ args !! 3)

run :: FilePath -> String -> Int -> Int -> IO ()
run fileOfFiles file line col = do
    parseFile file >>= either print (\targetCU ->
        posToZipper line col targetCU >>= upTill (undefined :: MemberDecl) &
        maybe (putStdErrLn "posToZipper failed") (\targetMemberDecl -> do
            fs <- mapM parseFile . lines =<< readFile fileOfFiles
            mapM_ (putStdErrLn . show) $ lefts fs
            mapM_ pps $ mapMaybe (getHole :: ZC -> Maybe Exp)
                         (callsTo (rights fs) targetMemberDecl)))
