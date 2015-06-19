module Syntack.TypeInference
    (
      typeOf
    ) where

import           Control.Lens (_1, over)
import           Control.Monad ((<=<))
import           Data.Either.Util (maybeToEither)
import           Data.Generics.Zipper (up, getHole)
import           Language.Java.Syntax hiding (Type)
import qualified Language.Java.Syntax as J (Type)
import           Language.Java.Syntax.Util (ident, name)
import           Syntack.Zipper (ZC, upTill)

data Type = PrimT PrimType | ClassT [(String, [TypeArgument])] | ArrayT Type deriving (Eq, Ord, Show)

convType :: J.Type -> Type
convType (PrimType p) = PrimT p
convType (RefType (ArrayType t)) = ArrayT $ convType t
convType (RefType (ClassRefType (ClassType t))) = ClassT $ map (over _1 ident) t

mkSimpleType :: [String] -> Type
mkSimpleType s = ClassT $ zip s (repeat [])

m_e :: e -> Maybe a -> Either e a
m_e = maybeToEither

typeOf :: ZC -> Either String Type
typeOf z = typeOf' z =<< (m_e "zipper not pointing at an Exp" $
            (getHole :: ZC -> Maybe Exp) z)

typeOf' :: ZC -> Exp -> Either String Type
typeOf' _ (Lit _ l) = Right $ typeOfLit l
typeOf' _ (ClassLit _ c) = maybe (Left "FIXME: ClassLit Nothing") (Right . convType) c
typeOf' z (This _) = typeOfThis z
typeOf' _ (ThisClass _ n) = Right $ mkSimpleType . name $ n
typeOf' _ e = error (show e)
{-
        | InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
        | QualInstanceCreation Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
        | ArrayCreate Type [Exp] Int
        | ArrayCreateInit Type Int ArrayInit
        | FieldAccess FieldAccess
        | MethodInv MethodInvocation
        | ArrayAccess ArrayIndex
        | ExpName Name
        | PostIncrement Exp
        | PostDecrement Exp
        | PreIncrement  Exp
        | PreDecrement  Exp
        | PrePlus  Exp
        | PreMinus Exp
        | PreBitCompl Exp
        | PreNot  Exp
        | Cast  Type Exp
        | BinOp Exp Op Exp
        | InstanceOf Exp RefType
        | Cond Exp Exp Exp
        | Assign Lhs AssignOp Exp
-}

typeOfLit :: Literal -> Type
typeOfLit (Int _) = PrimT IntT
typeOfLit (Word _) = PrimT LongT
typeOfLit (Float _) = PrimT FloatT
typeOfLit (Double _) = PrimT DoubleT
typeOfLit (Boolean _) = PrimT BooleanT
typeOfLit (Char _) = PrimT CharT
typeOfLit (String _) = mkSimpleType ["String"]
typeOfLit Null = mkSimpleType ["Object"]

-- FIXME this is incorrect for a this withing a InstanceCreation or QualInstanceCreation
typeOfThis :: ZC -> Either String Type
typeOfThis = go <=< m_e "`this` not within a ClassBody" . upTill (undefined :: ClassBody)
    where
        go :: ZC -> Either String Type
        go = fmap f . m_e "ClassBody not within a ClassDecl" . ((getHole :: ZC -> Maybe ClassDecl) <=< up)
        f :: ClassDecl -> Type
        f (ClassDecl _ i _ _ _ _) = mkSimpleType [(ident i)]
        f _ = error "impossible: we went up from a classbody, so its gatta be a classdecl daagie"
