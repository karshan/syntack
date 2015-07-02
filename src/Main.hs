{-# LANGUAGE ViewPatterns #-}
import           Control.Lens ((&), (^?), ix)
import           Data.Either (lefts, rights)
import           Data.Generics.Zipper (getHole)
import           Data.Maybe (mapMaybe, listToMaybe)

import           Language.Java.Syntax
import           Language.Java.Parser.Util (parseFile)
import           Language.Java.Pretty (Pretty, prettyPrint)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)

import           Syntack.Callgraph (callsTo)
import           Syntack.Zipper (ZC, posToZipper, upTill)

import           Text.Show.Pretty (ppShow)

import           Prelude hiding (read)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

usage :: String
usage = "usage: find $PROJDIR -name \"*.java\" > files\n" ++
        "./syntack files file line col"

read :: Read a => String -> Maybe a
read = fmap fst . listToMaybe . reads

type Args = (FilePath, String, Int, Int)

parseArgs :: [String] -> Maybe Args
parseArgs args = do
        fof <- args ^? ix 0
        target <- args ^? ix 1
        line <- read =<< args ^? ix 2
        col <- read =<< args ^? ix 3
        return (fof, target, line, col)

main :: IO ()
main = getArgs >>= (maybe (putStdErrLn usage) run . parseArgs)

run :: (FilePath, String, Int, Int) -> IO ()
run (fileOfFiles, file, line, col) = do
    parseFile file >>= either print (\targetCU ->
        posToZipper line col targetCU >>= upTill (undefined :: MemberDecl) &
        maybe (putStdErrLn "posToZipper failed") (\targetMemberDecl -> do
            fs <- mapM parseFile . lines =<< readFile fileOfFiles
            mapM_ (putStdErrLn . show) $ lefts fs
            mapM_ pps $ mapMaybe (getHole :: ZC -> Maybe Exp)
                         (callsTo (rights fs) targetMemberDecl)))
