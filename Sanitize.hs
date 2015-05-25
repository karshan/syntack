{-# LANGUAGE CPP,
    DeriveDataTypeable,
    TemplateHaskell,
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances #-}
module Sanitize ( sanitize
                , JavaSource) where

import Control.Lens
import Control.Lens.TH
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Language.Java.Syntax as J

#define DERIVE deriving (Eq, Show, Typeable, Data)

-- FIXME these is duplicated from Main {
jname :: J.Name -> [String]
jname x = x ^. J._Name ^. _2 & map jident
jident :: J.Ident -> String
jident x = x ^. J._Ident ^. _2
-- }

sanitize :: J.CompilationUnit -> JavaSource
sanitize cu
    = JavaSource
        (cu ^. J._CompilationUnit ^. _1 <&> (jname . (^. J._PackageDecl)))
        undefined
        undefined

data JavaSource
    = JavaSource
        { javaSourcePackageName :: Maybe Name
        , javaSourceImports :: [Import]
        , javaSourceTopDecls :: [TopDecl]
        } DERIVE

data Import
    = Import
        { importName :: Name
        , importStatic :: Bool
        , importWildcard :: Bool
        } DERIVE

data TopDecl
    = ClassDeclC ClassDecl
    | EnumDeclC EnumDecl
    | InterfaceDeclC InterfaceDecl
    DERIVE

data Decl
    = InitDeclC InitDecl
    | FieldDeclC FieldDecl
    | MethodDeclC MethodDecl
    | ContructorDeclC ConstructorDecl
    | MemberClassDeclC ClassDecl
    | MemberInterfaceDeclC InterfaceDecl
    DERIVE

data ClassDecl
    = ClassDecl
        { classDeclModifiers :: [Modifier]
        , classDeclIdent :: Ident
        , classDeclTypeParams :: [TypeParam]
        , classDeclMRefType :: Maybe RefType
        , classDeclRefTypes :: [RefType]
        , classDeclDecls :: [Decl]
        } DERIVE

data InterfaceDecl
    = InterfaceDecl
        { interfaceDeclModifiers :: [Modifier]
        , interfaceDeclIdent :: Ident
        , interfaceDeclTypeParams :: [TypeParam]
        , interfaceDeclRefTypes :: [RefType]
        , interfaceDecl :: [Decl]
        } DERIVE

data FieldDecl
    = FieldDecl
        { fieldDeclModifiers :: [Modifier]
        , fieldDeclType' :: Type
        , fieldDeclVarDecls :: [VarDecl]
        } DERIVE

data MethodDecl
    = MethodDecl
        { methodDeclModifiers :: [Modifier]
        , methodDeclTypeParams :: [TypeParam]
        , methodDeclMType :: Maybe Type
        , methodDeclIdent :: Ident
        , methodDeclFormalParams :: [FormalParam]
        , methodDeclExceptionTypes :: [RefType]
        , methodDeclMBlock :: Maybe Block
        } DERIVE

data ConstructorDecl
    = ConstructorDecl
        { constructorDeclModifiers :: [Modifier]
        , constructorDeclTypeParams :: [TypeParam]
        , constructorDeclIdent :: Ident
        , constructorDeclFormalParams :: [FormalParam]
        , constructorDeclExceptionTypes :: [RefType]
        , constructorDeclMBlock :: Maybe Block
        } DERIVE

data RefType
    = RefType
        { refTypeName :: Name
        , refTypeArrayDimension :: Int -- This should be a Nat
        } DERIVE

data TypeParam
    = TypeParam
        { typeParamIdent :: Ident
        , typeParamExtends :: [RefType]
        } DERIVE

data Type
    = PrimTypeC PrimType
    | RefTypeC RefType
    DERIVE

data PrimType
    = BooleanT
    | ByteT
    | ShortT
    | IntT
    | LongT
    | CharT
    | FloatT
    | DoubleT
    DERIVE

data VarDecl
    = VarDecl
        { varDeclIdent :: Ident
        , varDeclMVarInit :: Maybe VarInit
        } DERIVE

data FormalParam
    = FormalParam
        { formalParamModifiers :: [Modifier]
        , formalParamType' :: Type
        , formalParamEllipsis :: Bool
        , formalParamIdent :: Ident
        } DERIVE

type Block = [Stmt]

data Stmt
    = LocalClassC ClassDecl
    | LocalVarsC LocalVars
    | BlockC Block
    | IfThenElseC IfThenElse
    | WhileC While
    | BasicForC BasicFor
    | EnhancedForC EnhancedFor
    | Empty
    | ExpC Exp
    | AssertC Assert
    | SwitchC Switch
    | DoC Do
    | BreakC Break
    | ContinueC Continue
    | ReturnC Return
    | SynchronizedC Synchronized
    | ThrowC Throw
    | TryC Try
    | LabeledC Labeled
    DERIVE

data LocalVars
    = LocalVars
        { localVarsModifiers :: [Modifier]
        , localVarsType' :: Type
        , localVarsVarDecls :: [VarDecl]
        } DERIVE

data IfThenElse
    = IfThenElse
        { ifThenElseExp :: Exp
        , ifThenElseThenStmt :: Stmt
        , ifThenElseElseStmt :: Stmt
        } DERIVE

data While
    = While
        { whileExp :: Exp
        , whileStmt :: Stmt
        } DERIVE

data BasicFor
    = BasicFor
        { basicForMForInit :: Maybe ForInit
        , basicForMExp :: Maybe Exp
        , basicForExps :: [Exp]
        , basicForStmt :: Stmt
        } DERIVE

data EnhancedFor
    = EnhancedFor
        { enhancedForModifiers :: [Modifier]
        , enhancedForType' :: Type
        , enhancedForIdent :: Ident
        , enhancedForExp :: Exp
        , enhancedForStmt :: Stmt
        } DERIVE

data Assert
    = Assert
        { assertExp :: Exp
        , assertMExp :: Maybe Exp
        } DERIVE

data Switch
    = Switch
        { switchExp :: Exp
        , switchSwitchBlocks :: [SwitchBlock]
        } DERIVE

data Do
    = Do
        { doStmt :: Stmt
        , doExp :: Exp
        } DERIVE

data Break
    = Break
        { breakMIdent :: Maybe Ident } DERIVE

data Continue
    = Continue
        { continueMIdent :: Maybe Ident } DERIVE

data Return
    = Return
        { returnMExp :: Maybe Exp } DERIVE

data Synchronized
    = Synchronized
        { synchronizedExp :: Exp
        , synchronizedBlock :: Block
        } DERIVE

data Throw
    = Throw
        { throwExp :: Exp } DERIVE

data Try
    = Try
        { tryBlock :: Block
        , tryCatches :: [Catch]
        , tryFinally :: Maybe Block
        } DERIVE

data Labeled
    = Labeled
        { labeledIdent :: Ident
        , labeledStmt :: Stmt
        } DERIVE

data Catch
    = Catch
        { catchFormalParam :: FormalParam
        , catchBlock :: Block
        } DERIVE

data ForInit
    = ForLocalVars LocalVars
    | ForInitExps [Exp]
    DERIVE

data SwitchBlock
    = SwitchBlock
        { switchBlockSwitchLabel :: SwitchLabel
        , switchBlock :: Block
        } DERIVE

data SwitchLabel
    = SwitchCase Exp -- This Exp should only be a Lit or Enum constant.
    | Default
    DERIVE

data Exp
    = LitC Lit
    | ClassLitC ClassLit -- things like `a.class.b`
    | This
    | ThisClass Name -- things like `a.this`
    | InstanceCreationC InstanceCreation
    | QualInstanceCreationC QualInstanceCreation
    | ArrayCreateC ArrayCreate
    | ArrayCreateInitC ArrayCreateInit
    | FieldAccessC FieldAccess
    | MethodInvocationC MethodInvocation
    | ArrayAccessC ArrayAccess
    | ExpName Name
    | PostIncrement Exp
    | PostDecrement Exp
    | PreIncrement  Exp
    | PreDecrement  Exp
    | PrePlus  Exp
    | PreMinus Exp
    | PreBitCompl Exp
    | PreNot  Exp
    | CastC Cast
    | BinOp Exp Op Exp
    | InstanceOf Exp RefType
    | Cond Exp Exp Exp
    | Assign Lhs AssignOp Exp
    DERIVE

data Lit
    = Int Integer
    | Word Integer
    | Float Double
    | Double Double
    | Boolean Bool
    | Char Char
    | String String
    | Null
    DERIVE

data Modifier
    = Public
    | Private
    | Protected
    | Abstract
    | Final
    | Static
    | StrictFP
    | Transient
    | Volatile
    | Native
    | Annotation Annotation
    | Synchronised
    DERIVE

type Name = [String]
type Ident = String

-- TODO {
type InitDecl = String
type EnumDecl = String
type Annotation = String
type ClassLit = String
type InstanceCreation = String
type QualInstanceCreation = String
type Lhs = String
type Op = String
type FieldAccess = String
type ArrayCreate = String
type ArrayCreateInit = String
type MethodInvocation = String
type ArrayAccess = String
type AssignOp = String
type Cast = String
type VarInit = String
-- }

makeFields ''JavaSource
makeFields ''Import
makeFields ''ClassDecl
makeFields ''InterfaceDecl
makeFields ''FieldDecl
makeFields ''MethodDecl
makeFields ''ConstructorDecl
makeFields ''RefType
makeFields ''TypeParam
makeFields ''VarDecl
makeFields ''FormalParam
makeFields ''LocalVars
makeFields ''IfThenElse
makeFields ''While
makeFields ''BasicFor
makeFields ''EnhancedFor
makeFields ''Assert
makeFields ''Switch
makeFields ''Do
makeFields ''Break
makeFields ''Continue
makeFields ''Return
makeFields ''Synchronized
makeFields ''Throw
makeFields ''Try
makeFields ''Labeled
makeFields ''Catch
makeFields ''SwitchBlock
