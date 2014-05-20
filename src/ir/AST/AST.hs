{-|

The abstract syntax tree produced by the parser. Each node carries
meta-information about its type (filled in by
"Typechecker.Typechecker") and its position in the source file
(filled in by "Parser.Parser")

-}

module AST.AST where

import Identifiers
import AST.Meta

newtype Program = Program [ClassDecl] deriving(Show)

class HasType a where
    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x
    getType :: a -> Type

    setType :: Type -> a -> a

data ClassDecl = Class {cmeta   :: Meta,
                        cname   :: Type,
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Show, Eq)

instance HasType ClassDecl where
    getType = cname
    setType ty c@(Class {cmeta = meta}) = c {cmeta = AST.Meta.setType ty meta}

data FieldDecl = Field {fmeta :: Meta, 
                        fname :: Name, 
                        ftype::Type} deriving(Show, Eq)

instance HasType FieldDecl where
    getType = ftype
    setType ty f@(Field {fmeta = meta}) = f {fmeta = AST.Meta.setType ty meta}

data MethodDecl = Method {mmeta   :: Meta,
                          mname   :: Name,
                          rtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving (Show, Eq)

instance HasType MethodDecl where
    getType = rtype
    setType ty m@(Method {mmeta = meta}) = m {mmeta = AST.Meta.setType ty meta}

type Arguments = [Expr]

data Expr = Skip {emeta :: Meta}

          | Call {emeta :: Meta, 
                  target :: Expr, 
                  tmname :: Name, 
                  args :: Arguments}

          | Let {emeta :: Meta, 
                 eid :: Name, 
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
                 fut :: Expr}

          | FieldAccess {emeta :: Meta, 
                         path :: Expr, 
                         field :: Name}

          | Assign {emeta :: Meta, 
                    lhs :: LVal, 
                    rhs :: Expr}

          | VarAccess {emeta :: Meta, 
                       eid :: Name}

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

instance HasType Expr where
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

data LVal = LVal {lmeta :: Meta, lid :: Name} | 
            LField {lmeta :: Meta, lpath :: Expr, lid :: Name} deriving(Show, Eq)

instance HasType LVal where
    getType lval = AST.Meta.getType . lmeta $ lval

    setType ty lval = lval {lmeta = AST.Meta.setType ty (lmeta lval)}