module Syntack.Zipper where

import           Control.Arrow ((&&&))
import           Data.Bool (bool)
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Validation (zeverything, collectList, preorder)
import           Data.Generics.Zipper (Zipper, toZipper, fromZipper, getHole, up, down, left, right, query)
import           Data.Typeable (Typeable, typeOf)

import           Language.Java.Syntax
import           Language.Java.Pretty (Pretty, prettyPrint)

import           Text.Show.Pretty (ppShow)
import           Text.Parsec.Pos (sourceLine, sourceColumn)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

-- zix :: Int -> Zipper a -> Zipper a
-- zix n = left . (!! n) . iterate down . down

type ZC = Zipper CompilationUnit

upTill :: (Typeable a) => a -> ZC -> Maybe ZC
upTill t z = bool (upTill t =<< up z) (Just z) $ query typeOf z == typeOf t

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
