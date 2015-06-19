module Syntack.Zipper
    (
      ZC
    , upTill
    , posToZipper
    , zix
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad ((<=<))
import           Control.Monad.Util (iterateM)
import           Data.Bool (bool)
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Validation (zeverything, collectList, preorder)
import           Data.Generics.Zipper (Zipper, toZipper, up, down, left, query)
import           Data.Typeable (Typeable, typeOf)

import           Language.Java.Syntax

import           Text.Parsec.Pos (sourceLine, sourceColumn)

zix :: Int -> Zipper a -> Maybe (Zipper a)
zix n = left <=< iterateM n down <=< down

type ZC = Zipper CompilationUnit

upTill :: (Typeable a) => a -> ZC -> Maybe ZC
upTill t z = bool (upTill t =<< up z) (Just z) $ query typeOf z == typeOf t

posToZipper :: Int -> Int -> CompilationUnit -> Maybe ZC
posToZipper line col cu =
    fmap snd $ foldl (\a e -> bool a (Just e) ((fst e) `better` (fmap fst a))) Nothing $
        zeverything collectList (preorder (mkQ Nothing (Just . identPos))) (toZipper cu) ++
        zeverything collectList (preorder (mkQ Nothing expPos)) (toZipper cu)
    where
        identPos :: Ident -> (Int, Int)
        identPos (Ident p _) = (sourceLine &&& sourceColumn) p
        expPos :: Exp -> Maybe (Int, Int)
        expPos (This p) = Just $ (sourceLine &&& sourceColumn) p
        expPos _ = Nothing
        better :: (Int, Int) -> Maybe (Int, Int) -> Bool
        better _ Nothing = True
        better (nl, nc) (Just (l, c)) =
            (abs (nl - line), abs (nc - col)) < (abs (l - line), abs (c - col))
