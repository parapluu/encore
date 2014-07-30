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
import AST.Meta

data Program = Program EmbedTL [ClassDecl] deriving(Show)

class HasMeta a where
    getPos :: a -> SourcePos

    getMetaId :: a -> String

    setMetaId :: String -> a -> a

    getType :: a -> Type

    setType :: Type -> a -> a

    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

data Activity = Active | Passive deriving(Show, Eq)

data EmbedTL = EmbedTL {etlmeta :: Meta,
                        etlcode  :: String} deriving (Show)

data ClassDecl = Class {cmeta     :: Meta,
                        cactivity :: Activity,
                        cname     :: Type,
                        fields    :: [FieldDecl], 
                        methods   :: [MethodDecl]} deriving(Show, Eq)

isActive :: ClassDecl -> Bool
isActive = (== Active) . cactivity

instance HasMeta ClassDecl where
    getPos = AST.Meta.getPos . cmeta
    getMetaId c = (metaId . cmeta) c
    setMetaId id c = c{cmeta = cmeta'} 
        where
          cmeta' = (cmeta c){metaId = id}
    getType = cname
    setType ty c@(Class {cmeta, cname}) = c {cmeta = AST.Meta.setType ty cmeta, cname = ty}

data FieldDecl = Field {fmeta :: Meta, 
                        fname :: Name, 
                        ftype :: Type} deriving(Show, Eq)

instance HasMeta FieldDecl where
    getPos = AST.Meta.getPos . fmeta
    getMetaId f = (metaId . fmeta) f
    setMetaId id f = f{fmeta = fmeta'} 
        where
          fmeta' = (fmeta f){metaId = id}
    getType = ftype
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = AST.Meta.setType ty fmeta, ftype = ty}

data ParamDecl = Param {pmeta :: Meta, pname :: Name, ptype :: Type} deriving(Show, Eq)

instance HasMeta ParamDecl where
    getPos = AST.Meta.getPos . pmeta
    getMetaId p = (metaId . pmeta) p
    setMetaId id p = p{pmeta = pmeta'} 
        where
          pmeta' = (pmeta p){metaId = id}
    getType = ptype
    setType ty p@(Param {pmeta, ptype}) = p {pmeta = AST.Meta.setType ty pmeta, ptype = ty}

data MethodDecl = Method {mmeta   :: Meta,
                          mname   :: Name,
                          mtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving (Show, Eq)

instance HasMeta MethodDecl where
    getPos = AST.Meta.getPos . mmeta
    getMetaId m = (metaId . mmeta) m
    setMetaId id m = m{mmeta = mmeta'} 
        where
          mmeta' = (mmeta m){metaId = id}
    getType = mtype
    setType ty m@(Method {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}

isMainDecl :: ClassDecl -> MethodDecl -> Bool
isMainDecl cdecl mdecl = ((== "Main") . getId . cname $ cdecl) && ((== Name "main") . mname $ mdecl)

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
                 name :: Name, 
                 val :: Expr, 
                 body :: Expr}
          | Seq {emeta :: Meta, 
                 eseq :: [Expr]}
          | IfThenElse {emeta :: Meta, 
                        cond :: Expr, 
                        thn :: Expr, 
                        els :: Expr}
          | While {emeta :: Meta, 
                   cond :: Expr, 
                   body :: Expr}
          | Get {emeta :: Meta, 
                 val :: Expr}
          | FieldAccess {emeta :: Meta, 
                         target :: Expr, 
                         name :: Name}
          | Assign {emeta :: Meta, 
                    lhs :: LVal, 
                    rhs :: Expr}
          | VarAccess {emeta :: Meta, 
                       name :: Name}
          | Null {emeta :: Meta}
          | BTrue {emeta :: Meta}
          | BFalse {emeta :: Meta}
          | New {emeta :: Meta, 
                 ty ::Type}
          | Print {emeta :: Meta, 
                   val :: Expr}
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

isThisAccess :: Expr -> Bool
isThisAccess VarAccess {name = Name "this"} = True
isThisAccess _ = False

isClosure :: Expr -> Bool
isClosure Closure {} = True
isClosure _ = False

instance HasMeta Expr where
    getPos = AST.Meta.getPos . emeta

    getMetaId expr = (metaId . emeta) expr
    setMetaId id expr = expr{emeta = emeta'} 
        where
          emeta' = (emeta expr){metaId = id}

    hasType (Null {}) ty = not . isPrimitive $ ty
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x
    getType expr = AST.Meta.getType . emeta $ expr

    setType ty' expr@(TypedExpr {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(New {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(Embed {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty expr = expr {emeta = AST.Meta.setType ty (emeta expr)}

data LVal = LVal {lmeta :: Meta, lname :: Name} | 
            LField {lmeta :: Meta, ltarget :: Expr, lname :: Name} deriving(Show, Eq)

instance HasMeta LVal where
    getPos = AST.Meta.getPos . lmeta

    getMetaId l = (metaId . lmeta) l
    setMetaId id l = l{lmeta = lmeta'} 
        where
          lmeta' = (lmeta l){metaId = id}

    getType lval = AST.Meta.getType . lmeta $ lval

    setType ty lval = lval {lmeta = AST.Meta.setType ty (lmeta lval)}
