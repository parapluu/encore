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
import AST.Meta

newtype Program = Program [ClassDecl] deriving(Show)

class HasMeta a where
    getPos :: a -> SourcePos

    getType :: a -> Type

    setType :: Type -> a -> a

    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

data ClassDecl = Class {cmeta   :: Meta,
                        cname   :: Type,
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Show, Eq)

instance HasMeta ClassDecl where
    getPos = AST.Meta.getPos . cmeta
    getType = cname
    setType ty c@(Class {cmeta}) = c {cmeta = AST.Meta.setType ty cmeta}

data FieldDecl = Field {fmeta :: Meta, 
                        fname :: Name, 
                        ftype :: Type} deriving(Show, Eq)

instance HasMeta FieldDecl where
    getPos = AST.Meta.getPos . fmeta
    getType = ftype
    setType ty f@(Field {fmeta}) = f {fmeta = AST.Meta.setType ty fmeta}

data ParamDecl = Param {pmeta :: Meta, pname :: Name, ptype :: Type} deriving(Show, Eq)

instance HasMeta ParamDecl where
    getPos = AST.Meta.getPos . pmeta
    getType = ptype
    setType ty p@(Param {pmeta}) = p {pmeta = AST.Meta.setType ty pmeta}

data MethodDecl = Method {mmeta   :: Meta,
                          mname   :: Name,
                          mtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving (Show, Eq)

instance HasMeta MethodDecl where
    getPos = AST.Meta.getPos . mmeta
    getType = mtype
    setType ty m@(Method {mmeta}) = m {mmeta = AST.Meta.setType ty mmeta}

type Arguments = [Expr]

data Expr = Skip {emeta :: Meta}
          | Call {emeta :: Meta, 
                  target :: Expr, 
                  name :: Name, 
                  args :: Arguments}
          | Let {emeta :: Meta, 
                 name :: Name, 
                 ty :: Type, 
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
                   ty :: Type, 
                   val :: Expr}
          | StringLiteral {emeta :: Meta, 
                           stringLit :: String}
          | IntLiteral {emeta :: Meta, 
                        intLit :: Int}
          | Binop {emeta :: Meta, 
                   op :: Op, 
                   loper :: Expr, 
                   roper :: Expr} deriving(Show, Eq)

instance HasMeta Expr where
    getPos = AST.Meta.getPos . emeta
    hasType (Null {}) ty = not . isPrimitive $ ty
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x
    getType Skip {} = voidType
    getType Null {} = nullType
    getType BTrue {} = boolType
    getType BFalse {} = boolType
    getType StringLiteral {} = stringType
    getType IntLiteral {} = intType
    getType expr = AST.Meta.getType . emeta $ expr

    setType ty expr = expr {emeta = AST.Meta.setType ty (emeta expr)}

data LVal = LVal {lmeta :: Meta, lname :: Name} | 
            LField {lmeta :: Meta, ltarget :: Expr, lname :: Name} deriving(Show, Eq)

instance HasMeta LVal where
    getPos = AST.Meta.getPos . lmeta

    getType lval = AST.Meta.getType . lmeta $ lval

    setType ty lval = lval {lmeta = AST.Meta.setType ty (lmeta lval)}