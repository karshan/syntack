module Syntack.Zipper where

import           Data.Bool (bool)
import           Data.Generics.Zipper (Zipper, toZipper, fromZipper)
import qualified Data.Generics.Zipper as Z (getHole, up, down, left, right)
import           Data.Generics.Zipper.Util (getHole, up, down, left, right)
import           Data.Maybe (isNothing, fromMaybe)
import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)

import           Language.Java.Syntax
import           Language.Java.Pretty (Pretty, prettyPrint)

import           Text.Show.Pretty (ppShow)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

zix :: Int -> Zipper a -> Zipper a
zix n = left . (!! n) . iterate down . down

type ZC = Zipper CompilationUnit

nameme :: CompilationUnit -> IO ()
nameme = undefined
