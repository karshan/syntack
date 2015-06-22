module Syntack.Zipper
    (
      ZC
    , upTill
    , posToZipper
    , zix
    , zfind
    , children
    , unsafeGetHole
    ) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.Util (iterateM)
import Data.Bool (bool)
import Data.Data (Data)
import Data.Generics.Aliases (mkQ)
import Data.Generics.Validation (zeverything, collectList, preorder)
import Data.Generics.Zipper (getHole, Zipper, toZipper, up, down, left, query)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, typeOf)

import Language.Java.Syntax

import Text.Parsec.Pos (sourceLine, sourceColumn)

unsafeGetHole :: (Typeable b) => Zipper a -> b
unsafeGetHole = fromMaybe (error "getHole") . getHole

zix :: Int -> Zipper a -> Maybe (Zipper a)
zix n = left <=< iterateM n down <=< down

type ZC = Zipper CompilationUnit

upTill :: (Typeable a) => a -> ZC -> Maybe ZC
upTill t z = bool (upTill t =<< up z) (Just z) $ query typeOf z == typeOf t

zfind :: (Typeable b, Data r) => (b -> Maybe a) -> Zipper r -> [(a, Zipper r)]
zfind f = zeverything collectList (preorder (mkQ Nothing f))

children :: (Typeable a, Data r) => a -> Zipper r -> [Zipper r]
children a z = map snd $ zeverything collectList (preorder (mkQ Nothing $ f a)) z
    where
        f :: (Typeable a) => a -> a -> Maybe ()
        f _ _ = Just ()

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
