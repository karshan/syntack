{-# LANGUAGE ViewPatterns #-}
import           Control.Arrow ((&&&))
import           Control.Exception (SomeException, try)
import           Control.Lens (_1, _2, _Left, over)

import           Data.Bool (bool)
import           Data.Either (lefts, rights)
import           Data.Generics.Zipper (Zipper, toZipper, getHole, up)
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Validation (zeverything, collectList, preorder)
import           Data.Maybe (isJust)

import           Language.Java.Syntax
import           Language.Java.Parser.Util (parseFile)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)
import           Text.Parsec (ParseError)
import           Text.Parsec.Pos (sourceLine, sourceColumn)

import           Syntack.Zipper (ZC, nameme)

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

upTillExp :: ZC -> Maybe ZC
upTillExp z = bool (upTillExp =<< up z) (Just z) $ isJust $ (getHole :: ZC -> Maybe Exp) z

posToZipper :: CompilationUnit -> Int -> Int -> Maybe ZC
posToZipper cu line col = 
    fmap snd $ foldl (\a e -> bool a (Just e) ((fst e) `better` (fmap fst a))) Nothing $
        zeverything collectList (preorder (mkQ Nothing identPos)) (toZipper cu)
    where
        identPos :: Ident -> Maybe (Int, Int)
        identPos (Ident p _) = fmap (sourceLine &&& sourceColumn) p
        better :: (Int, Int) -> Maybe (Int, Int) -> Bool
        better e Nothing = True
        better (nl, nc) (Just (l, c)) =
            (abs (nl - line), abs (nc - col)) < (abs (l - line), abs (c - col))
