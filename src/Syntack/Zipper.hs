module Syntack.Zipper where

import           Data.Bool (bool)
import           Data.Generics.Zipper (Zipper, toZipper, fromZipper)
import qualified Data.Generics.Zipper as Z (getHole, up, down, left, right)
import           Data.Generics.Zipper.Util (getHole, up, down, left, right)
import           Data.Maybe (isNothing, fromMaybe)
import           Data.Proxy (Proxy)
import           Data.Tagged (Tagged, tagSelf, retag, untag)
import           Data.Typeable (Typeable)

import           Language.Java.Syntax
import           Language.Java.Pretty (Pretty, prettyPrint)

import           Text.Show.Pretty (ppShow)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

tag :: a -> Tagged b a
tag = retag . tagSelf

untagHole :: (Typeable a) => Tagged a (Zipper b) -> a
untagHole = getHole . untag

helper :: (a -> Maybe b) -> a -> Maybe a
helper f x = bool Nothing (Just x) $ isNothing (f x)

zix :: Int -> Tagged [a] (Zipper b) -> Tagged a (Zipper b)
zix n = tag . left . (!! n) . iterate down . down . untag

type ZC = Zipper CompilationUnit

typeDecls :: Tagged CompilationUnit ZC -> Tagged [TypeDecl] ZC 
typeDecls = tag . down . untag

classDecl :: Tagged TypeDecl ZC -> Maybe (Tagged ClassDecl ZC)
classDecl = fmap tag . helper (Z.getHole :: ZC -> Maybe ClassDecl) . down . untag

interfaceDecl :: Tagged TypeDecl ZC -> Maybe (Tagged InterfaceDecl ZC)
interfaceDecl = fmap tag . helper (Z.getHole :: ZC -> Maybe InterfaceDecl) . down . untag

classBody :: Tagged ClassDecl ZC -> Tagged ClassBody ZC 
classBody = tag . down . untag

nameme :: CompilationUnit -> IO ()
nameme = ppj . untagHole . fromMaybe (error "interfaceDecl") . interfaceDecl . zix 0 . typeDecls . tag . toZipper
