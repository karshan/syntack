{-# LANGUAGE ViewPatterns #-}
import           Control.Lens (_2, (^..), over)
import           Data.Data.Lens (template)
import           Data.Either (isLeft, isRight)
import           Data.Generics.Zipper (getHole)
import           Data.List.Split (splitOn)
import           Data.Maybe (mapMaybe)

import           Language.Java.Syntax
import           Language.Java.Syntax.Util (typeName, name)
import           Language.Java.Parser.Util (parseFile)
import           Language.Java.Pretty (Pretty, prettyPrint)

import           System.Environment (getArgs)
import           System.IO.Util (putStdErrLn)

import           Syntack.Callgraph (callsTo)
import           Syntack.Zipper (ZC)

import           Text.Show.Pretty (ppShow)

pps :: (Show a) => a -> IO ()
pps = putStrLn . ppShow

ppj :: (Pretty a) => a -> IO ()
ppj = putStrLn . prettyPrint

usage :: String
usage = "usage: find $PROJDIR -name \"*.java\" > files; ./syntack files"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn usage
    else run (head args)

run :: FilePath -> IO ()
run fileOfFiles = do
    fs <- mapM (\x -> parseFile x >>= (\cu -> return (x, cu))) . lines =<< readFile fileOfFiles
    mapM_ (putStdErrLn . show) $ filter (isLeft . snd) fs
    mapM_ (\(f, ms) -> putStrLn (f ++ ":") >> mapM_ ppj ms) $ map (\(a, (Right b)) -> (a, findColinMethods b)) $ filter (isRight . snd) fs

--                                                  pointing to MemberDecl
findColinMethods :: CompilationUnit -> [MemberDecl]
findColinMethods cu = filter (\x -> hasNoArgs x && (has3stmts x || returnsMagic x)) $ concatMap methods $ baseActionSubClasses cu

baseActionSubClasses :: CompilationUnit -> [ClassDecl]
baseActionSubClasses cu = filter ff (cu ^.. template :: [ClassDecl])
    where
        ff (ClassDecl _ _ _ (Just t) _ _) = typeName (RefType t) == ["BaseAction"]
        ff _ = False

methods :: ClassDecl -> [MemberDecl]
methods c = filter ff $ c ^.. template
    where
        ff (MethodDecl {}) = True
        ff _ = False

has3stmts :: MemberDecl -> Bool
has3stmts x = length (x ^.. template :: [Stmt]) >= 3

returnsMagic :: MemberDecl -> Bool
returnsMagic m = not . null $ filter ff (m ^.. template :: [Stmt])
    where
        ff (Return (Just e)) = isMagicReturn e
        ff _ = False

isMagicReturn :: Exp -> Bool
isMagicReturn (ExpName (name -> (i:[]))) = i `elem` ["APPROVED", "CUSTOMER", "ERROR", "FAILED", "INPUT", "LOCKED", "NAMESPACES", "PRIVATE", "PUBLIC", "SENT", "SETCOMPANY", "SUCCESS"]
isMagicReturn _ = False

hasNoArgs :: MemberDecl -> Bool
hasNoArgs (MethodDecl _ _ _ _ [] _ _) = True
hasNoArgs _ = False
