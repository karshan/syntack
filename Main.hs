{-# LANGUAGE CPP, DeriveDataTypeable, ViewPatterns, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Arrow
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Data (Data)
import Data.Data.Lens (template)
import Data.Either
import Data.Functor.Foldable
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Typeable (Typeable)
import Debug.Trace
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Pretty (Pretty, prettyPrint)
import Language.Java.Syntax
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos, newPos)
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString.Char8 as BS
import qualified Language.Java.Parser as P

import Callgraph
-- TODO: replace calls to getNodes with (^.. template) where typeinference doesn't require a Proxy

-- Generic Util {
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither d Nothing = Left d
maybeToEither _ (Just a) = Right a

headE :: e -> [a] -> Either e a
headE e = maybeToEither e . listToMaybe

fml e a = fromMaybe undefined (e ^? a) -- need dependent types :(
infix 9 `fml`

-- `firstRight = msum` would also work but it wouldnn't concat errors
-- FIXME: error types
firstRight :: (Monoid e) => [Either e a] -> Either e a
firstRight = firstRight' mempty
    where
        firstRight' :: (Monoid e) => e -> [Either e a] -> Either e a
        firstRight' _ ((Right a):_) = Right a
        firstRight' acc [] = Left acc
        firstRight' acc ((Left e):xs) = firstRight' (acc <> e) xs

showOne :: (Pretty a, Show a) => a -> String
showOne x = (takeWhile (/= ' ') $ show x) ++ ": " ++ prettyPrint x

pprint :: (Show a) => a -> IO ()
pprint = putStrLn . ppShow

parse :: String -> Either ParseError CompilationUnit
parse = parser compilationUnit

parseFile :: FilePath -> IO (Either String CompilationUnit)
parseFile =
  fmap (either (\(_ :: SomeException) -> Left "open failed")
               (over _Left show . parse . BS.unpack)) . try . BS.readFile
-- }

-- AST Util {
type MethodDecl' = ([Modifier], [TypeParam], Maybe Type, Ident, [FormalParam], [ExceptionType], MethodBody)
type ClassDecl' = ([Modifier], Ident, [TypeParam], Maybe RefType, [RefType], ClassBody)
type Ctx = ([ClassDecl'], ClassDecl', MethodDecl') -- Name resolution context

getNodes :: (Data a, Data b) => Proxy a -> b -> [a]
getNodes p r = r ^.. template

getNodesRec :: (Data a, Data b) => Proxy a -> b -> [a]
getNodesRec p r = go $ r ^.. template
    where go [] = []
          go xs = xs ++ (go $ concatMap (^.. template) xs)

methods :: CompilationUnit -> [(ClassDecl', [MethodDecl'])]
methods = mapMaybe (\c -> (c ^? _ClassDecl) >>=
            (\c' -> return (c', mapMaybe (^? _MethodDecl)
                (getNodes (Proxy :: Proxy MemberDecl) c))))
                    . getNodesRec (Proxy :: Proxy ClassDecl)


ident :: Ident -> String
ident x = x ^. _Ident ^. _2

name :: Name -> [String]
name x = x ^. _Name ^. _2 & map ident

varDeclIdName :: VarDeclId -> String
varDeclIdName (VarId i) = ident i
varDeclIdName (VarDeclArray v) = varDeclIdName v

varDeclName :: VarDecl -> String
varDeclName (VarDecl v _) = varDeclIdName v

-- FIXME fully qualified class name
className :: ClassDecl' -> String
className = ident . (^. _2)

classDeclName :: ClassDecl -> String
classDeclName c@(ClassDecl {}) = ident $ (c `fml` _ClassDecl) ^. _2
classDeclName c@(EnumDecl {}) = ident $ (c `fml` _EnumDecl) ^. _2

--                          List of names declared in this declaration. NOT A FULLY QUALIFIED name FIXME types
memberName :: MemberDecl -> [String]
memberName m@(FieldDecl {}) = map varDeclName $ (m `fml` _FieldDecl) ^. _3
memberName m@(MethodDecl {}) = return $ ident $ (m `fml` _MethodDecl) ^. _4
memberName m@(ConstructorDecl {}) = return $ ident $ (m `fml` _ConstructorDecl) ^. _3
memberName m@(MemberClassDecl c) = return $ classDeclName c
memberName m@(MemberInterfaceDecl i) = error "WTF is this memberinterfacedecl"

typeName :: Type -> Name
typeName (PrimType p) = Name (newPos "" 0 0) [Ident Nothing $ prettyPrint p]
typeName (RefType (ArrayType t)) = typeName t --FIXME typeName ignores array
typeName (RefType (ClassRefType (ClassType cs))) = mkName $ map ((\(i, _) -> ident i)) cs

mkName :: [String] -> Name
mkName = Name (newPos "" 0 0) . map (Ident Nothing)

mkClassDecl :: ClassDecl' -> ClassDecl
mkClassDecl (a, b, c, d, e, f) = ClassDecl a b c d e f

mkMethodDecl :: MethodDecl' -> MemberDecl
mkMethodDecl (a, b, c, d, e, f, g) = MethodDecl a b c d e f g

super :: Ctx -> Maybe Type
super (_, c, _) = RefType <$> c ^. _4

-- FIXME: a name and classdecl is not enough to uniquely define a mamber a la function overloading
lookupMember :: String -> ClassDecl' -> Either String MemberDecl
lookupMember n c = maybeToEither ("member " ++ n ++ " not found in class " ++ className c) $ lookup [n] $ map (memberName &&& id) $ c ^.. template

lookupClass :: String -> [ClassDecl'] -> Either String ClassDecl'
lookupClass n cs = maybeToEither ("class " ++ n ++ " not found") $ lookup n $ map (className &&& id) cs

classType :: [String] -> Type
classType n = RefType (ClassRefType (ClassType (map (\x -> (Ident Nothing x,[])) n)))

-- FIXME this cheats and returns the return type of functions. Currently we don't have a way of representing function types
memberType :: MemberDecl -> Either String Type
memberType m@(FieldDecl {}) = return $ (m `fml` _FieldDecl) ^. _2
memberType m@(MethodDecl {}) = maybeToEither "FIXME: function returns void" $ (m `fml` _MethodDecl) ^. _3
memberType m@(ConstructorDecl {}) = Left "FIXME: constructor need context" -- (m `fml` _ConstructorDecl) ^. _3
memberType m@(MemberClassDecl {}) = return . classType . return . classDeclName $ m `fml` _MemberClassDecl -- FIXME fully qualified classname
memberType m@(MemberInterfaceDecl {}) = Left "WTF is the type of an interface"

typeOfIdentifier :: Ctx -> String -> Either String Type
typeOfIdentifier (cs, c, m) n = firstRight [ maybeToEither ("no var " ++ n ++ " in " ++ concat (memberName (mkMethodDecl m)) ++ "\n") $ (\x -> fmap (^. _2) $ listToMaybe $
                                                filter (\(_, _, vs) -> n `elem` (map varDeclName vs)) $ mapMaybe (^? _LocalVars) $ x ^. _Block) =<< m ^. _7 ^. _MethodBody -- local variable in method
                                           , memberType =<< lookupMember n c
                                           ]

{-
 - Identifiers are first resolved locally. This means I can have
 - a variable `int String = 1`. Or more interestingly if I have
 - a class Bar with a static field b then `Bar.b` refers to that
 - static field iff there is no locally defined Bar. e.g.
 - `int Bar = 1`
 - This is why we try to resolve a.{exp} with a as an identifier first
 - if that fails we check if a is a classname in which case it is a
 - static field access.
 -
 - typeOfName _ a.b.c.d
 -  = typeOfMember d _ (typeOfName _ a.b.c)
 -  = typeOfMember d _ (typeOfMember c _ (typeOfName _ a.b))
 -  = typeOfMember d _ (typeOfMember c _ (typeOfMember b _ (typeOfName _ a)))
 -
 -}
typeOfName :: Ctx -> [String] -> Either String Type
typeOfName ctx@(cs, _, _) (reverse -> n:[]) = firstRight [ typeOfIdentifier ctx n, classType . const [n] <$> lookupClass n cs ]
typeOfName ctx@(cs, c, m) (reverse -> n:ns) = typeOfMember n cs =<< typeOfName ctx (reverse ns)

-- TODO: use typeOfMember in typeOfFieldAccess
--              b                         a       typeOf a.b
typeOfMember :: String -> [ClassDecl'] -> Type -> Either String Type
typeOfMember m cs t@(PrimType _) = Left $ "attempting to access field " ++ m ++ " of primitive type " ++ concat (name (typeName t))
typeOfMember m cs (name . typeName -> (n:[])) = memberType =<< lookupMember m =<< lookupClass n cs
typeOfMember m cs (name . typeName -> (n:ns)) = Left $ "oh no I got a fqn " ++ intercalate "." (n:ns)

-- }

-- typeOf {
typeOfLit :: Literal -> Type
typeOfLit (Int _) = PrimType IntT
typeOfLit (Word _) = PrimType LongT
typeOfLit (Float _) = PrimType FloatT
typeOfLit (Double _) = PrimType DoubleT
typeOfLit (Boolean _) = PrimType BooleanT
typeOfLit (Char _) = PrimType CharT
typeOfLit (String _) = classType ["String"]
typeOfLit Null = classType ["Object"]

typeOfFieldAccess :: Ctx -> FieldAccess -> Either String Type
typeOfFieldAccess ctx (PrimaryFieldAccess e i) = (\t -> typeOfFieldAccess ctx (ClassFieldAccess (typeName t) i)) =<< (typeOf ctx e)
typeOfFieldAccess ctx@(cs, c, m) (SuperFieldAccess i) = do
    super' <- maybeToEither ("Class: " ++ className c ++ " has no super") $ super ctx
    typeOfFieldAccess ctx (ClassFieldAccess (typeName super') i)
typeOfFieldAccess ctx@(cs, _, _) (ClassFieldAccess n i) = do -- This is for Foo.class.bar accesses
    cn <- headE "empty class name" $ reverse $ name n
    c <- headE ("no such class " ++ cn) (filter ((== cn) . className) cs) -- FIXME fully qualified class names
    memberType =<< headE ("no member " ++ (ident i) ++ " in " ++ cn) (filter (((ident i) `elem`) . memberName) (getNodes (Proxy :: Proxy MemberDecl) c))

typeOfMethodInvocation :: Ctx -> MethodInvocation -> Either String Type
typeOfMethodInvocation ctx (MethodCall n _) = typeOfName ctx (name n)

typeOf :: Ctx -> Exp -> Either String Type
typeOf ctx@(cs, c, m) e = f e
    where
        f (Lit a) = return $ typeOfLit a
        f (ClassLit Nothing) = return $ classType ["Object"] -- FIXME this is actually void ?!
        f (ClassLit (Just t)) = return t
        f This = return $ classType [ident $ c ^. _2] -- FIXME fully qualified class name
        f (ThisClass n) = return $ classType $ name n
        f e@(InstanceCreation {}) = return $ RefType . ClassRefType $ (e `fml` _InstanceCreation) ^. _2
        f e@(QualInstanceCreation {}) = typeOf ctx $ (e `fml`_QualInstanceCreation) ^. _1
        f e@(ArrayCreate {}) = return $ (e `fml` _ArrayCreate) ^. _1
        f e@(ArrayCreateInit {}) = return $ (e `fml` _ArrayCreateInit) ^. _1
        f (FieldAccess a) = typeOfFieldAccess ctx a
        f (MethodInv m) = typeOfMethodInvocation ctx m
        f (ExpName n) = typeOfName ctx (name n)
        f e = error (show e)
-- }

f :: FilePath -> SourcePos -> Either String Ctx
f = undefined

main :: IO ()
main = do
    putStrLn "usage: ./syntack files class method exp"
    (files:iClass:iMeth:iExp:_) <- getArgs
    cus <- rights <$> ((mapM parseFile . lines) =<< readFile files)
    let methodMap = concatMap methods cus
    let cs = map fst methodMap
    let (Right exp') = parser P.exp iExp
    let (Just (c, m)) = do
            (c, ms) <- lookup iClass $ map (className . fst &&& id) methodMap
            m <- lookup [iMeth] $ map (memberName . mkMethodDecl &&& id) ms
            return (c, m)
    either (putStrLn . ("typeOfError: " ++)) print $ typeOf (cs, c, m) exp'
