{-# LANGUAGE NamedFieldPuns #-}

{-|

The abstract syntax tree produced by the parser. Each node carries
meta-information about its type (filled in by
"Typechecker.Typechecker") and its position in the source file
(filled in by "Parser.Parser")

-}

module AST.AST where

import Text.Parsec(SourcePos)

import Identifiers
import Types
import AST.Meta hiding(Closure)

data Program = Program {etl :: EmbedTL, imports :: [ImportDecl], functions :: [Function], classes :: [ClassDecl]} deriving(Show)

class HasMeta a where
    getMeta :: a -> Meta

    setMeta :: a -> Meta -> a

    getPos :: a -> SourcePos
    getPos = AST.Meta.getPos . getMeta

    getType :: a -> Type
    getType = AST.Meta.getType . getMeta

    setType :: Type -> a -> a

    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

    getMetaInfo :: a -> MetaInfo
    getMetaInfo = AST.Meta.metaInfo . getMeta

data EmbedTL = EmbedTL {etlmeta   :: Meta,
                        etlheader :: String,
                        etlbody   :: String} deriving (Show)

data ImportDecl = Import {imeta   :: Meta,
                          itarget :: Name } deriving (Show, Eq)

data Function = Function {funmeta   :: Meta,
                          funname   :: Name,
                          funtype   :: Type,
                          funparams :: [ParamDecl],
                          funbody   :: Expr} deriving (Show, Eq)

instance HasMeta Function where
    getMeta = funmeta
    setMeta f m = f{funmeta = m}
    setType ty f@(Function {funmeta, funtype}) = f {funmeta = AST.Meta.setType ty funmeta, funtype = ty}

data ClassDecl = Class {cmeta     :: Meta,
                        cname     :: Type,
                        fields    :: [FieldDecl], 
                        methods   :: [MethodDecl]} deriving(Show, Eq)

isActive :: ClassDecl -> Bool
isActive = isActiveRefType . cname

isMainClass :: ClassDecl -> Bool
isMainClass cdecl = (== "Main") . getId . cname $ cdecl

instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) = c {cmeta = AST.Meta.setType ty cmeta, cname = ty}

data FieldDecl = Field {fmeta :: Meta, 
                        fname :: Name, 
                        ftype :: Type} deriving(Show, Eq)

instance HasMeta FieldDecl where
    getMeta = fmeta
    setMeta f m = f{fmeta = m}
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = AST.Meta.setType ty fmeta, ftype = ty}

data ParamDecl = Param {pmeta :: Meta, pname :: Name, ptype :: Type} deriving(Show, Eq)

instance HasMeta ParamDecl where
    getMeta = pmeta
    setMeta p m = p{pmeta = m}
    setType ty p@(Param {pmeta, ptype}) = p {pmeta = AST.Meta.setType ty pmeta, ptype = ty}

data MethodDecl = Method {mmeta   :: Meta,
                          mname   :: Name,
                          mtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving (Show, Eq)

instance HasMeta MethodDecl where
    getMeta = mmeta
    setMeta mtd m = mtd{mmeta = m}
    setType ty m@(Method {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}

type Arguments = [Expr]

data Expr = Skip {emeta :: Meta}
          | TypedExpr {emeta :: Meta,
                       body :: Expr,
                       ty   :: Type}
          | MethodCall {emeta :: Meta, 
                        target :: Expr, 
                        name :: Name, 
                        args :: Arguments}
          | MessageSend {emeta :: Meta, 
                         target :: Expr, 
                         name :: Name, 
                         args :: Arguments}
          | FunctionCall {emeta :: Meta, 
                          name :: Name, 
                          args :: Arguments}
          | Closure {emeta :: Meta, 
                     eparams :: [ParamDecl],
                     body :: Expr}
          | Let {emeta :: Meta, 
                 decls :: [(Name, Expr)],
                 body :: Expr}
          | Seq {emeta :: Meta, 
                 eseq :: [Expr]}
          | IfThenElse {emeta :: Meta, 
                        cond :: Expr, 
                        thn :: Expr, 
                        els :: Expr}
          | IfThen {emeta :: Meta, 
                    cond :: Expr, 
                    thn :: Expr}
          | Unless {emeta :: Meta, 
                    cond :: Expr, 
                    thn :: Expr}
          | While {emeta :: Meta, 
                   cond :: Expr, 
                   body :: Expr}
          | Get {emeta :: Meta, 
                 val :: Expr}
          | FieldAccess {emeta :: Meta, 
                         target :: Expr, 
                         name :: Name}
          | Assign {emeta :: Meta, 
                    lhs :: Expr, 
                    rhs :: Expr}
          | VarAccess {emeta :: Meta, 
                       name :: Name}
          | Null {emeta :: Meta}
          | BTrue {emeta :: Meta}
          | BFalse {emeta :: Meta}
          | NewWithInit {emeta :: Meta, 
                         ty ::Type,
                         args :: Arguments}
          | New {emeta :: Meta, 
                 ty ::Type}
          | Print {emeta :: Meta, 
                   stringLit :: String,
                   args :: [Expr]}
          | Exit {emeta :: Meta,
                  args :: [Expr]}
          | StringLiteral {emeta :: Meta, 
                           stringLit :: String}
          | IntLiteral {emeta :: Meta, 
                        intLit :: Int}
          | RealLiteral {emeta :: Meta, 
                         realLit :: Double}
          | Embed {emeta :: Meta,
                   ty    :: Type,
                   code  :: String}
          | Unary {emeta :: Meta,
                   op    :: Op,
                   operand  :: Expr }
          | Binop {emeta :: Meta,
                   op :: Op,
                   loper :: Expr,
                   roper :: Expr} deriving(Show, Eq)

isLval :: Expr -> Bool
isLval VarAccess {} = True
isLval FieldAccess {} = True
isLval _ = False

isThisAccess :: Expr -> Bool
isThisAccess VarAccess {name = Name "this"} = True
isThisAccess _ = False

isClosure :: Expr -> Bool
isClosure Closure {} = True
isClosure _ = False

instance HasMeta Expr where
    getMeta = emeta
    setMeta e m = e{emeta = m}

    hasType (Null {}) ty = not . isPrimitive $ ty
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

    setType ty' expr@(TypedExpr {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(New {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(Embed {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty expr = expr {emeta = AST.Meta.setType ty (emeta expr)}