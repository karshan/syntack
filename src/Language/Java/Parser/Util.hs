{-# LANGUAGE ScopedTypeVariables #-}
module Language.Java.Parser.Util
    (
      parseFile
    ) where

import           Control.Exception (SomeException, try)
import           Control.Lens (_Left, over)

import qualified Data.ByteString as BS (readFile)
import           Data.ByteString.UTF8 (toString)

import           Text.Parsec (ParseError)

import           Language.Java.Syntax (CompilationUnit)
import           Language.Java.Parser (parser, compilationUnit)

parse :: String -> Either ParseError CompilationUnit
parse = parser compilationUnit

parseFile :: FilePath -> IO (Either (FilePath, String) CompilationUnit)
parseFile f = fmap (either (\(x :: SomeException) -> Left (f, show x))
                   (over _Left ((,) f . show) . parse . toString)) . try $ BS.readFile f
