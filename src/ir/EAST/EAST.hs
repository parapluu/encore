{-|

An abstract syntax tree extended with types, produced by the type
checker.

-}

module EAST.EAST where

import Identifiers

newtype Program = Program [ClassDecl] deriving(Read, Show)

data ClassDecl = Class {cname   :: Type,
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Read, Show, Eq)

data FieldDecl = Field {fname :: Name, ftype :: Type} deriving(Read, Show, Eq)

newtype ParamDecl = Param (Name, Type) deriving(Read, Show, Eq)

data MethodDecl = Method {mname   :: Name,
                          rtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving (Read, Show, Eq)

data Expr = Skip
          | Call {ty :: Type, target :: Expr, tmname :: Name, args :: Arguments}
          | Let {ty :: Type, id :: Name, idType :: Type, val :: Expr, body :: Expr}
          | Seq {ty :: Type, seq :: [Expr]}
          | IfThenElse {ty :: Type, cond :: Expr, thn :: Expr, els :: Expr}
          | While {ty :: Type, cond :: Expr, body :: Expr}
          | Get {ty :: Type, fut :: Expr}
          | FieldAccess {ty :: Type, path :: Expr, field :: Name}
          | Assign {ty :: Type, lhs :: LVal, rhs :: Expr}
          | VarAccess {ty :: Type, id :: Name}
          | Null
          | BTrue
          | BFalse
          | New {ty :: Type}
          | Print {ty :: Type, val :: Expr}
          | StringLiteral String
          | IntLiteral Int
          | Binop {ty :: Type, op :: Op, loper :: Expr, roper :: Expr} deriving(Read, Show, Eq)

type Arguments = [Expr]

data LVal = LVal Type Name | LField Type Expr Name deriving(Read, Show, Eq)

class HasType a where
    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = getType x
    getType :: a -> Type

instance HasType Expr where
    hasType Null ty = not . isPrimitive $ ty
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = getType x
    getType Skip = voidType
    getType Null = nullType
    getType BTrue = boolType
    getType BFalse = boolType
    getType (StringLiteral _) = stringType
    getType (IntLiteral _) = intType
    getType expr = ty expr

instance HasType LVal where
    getType (LVal ty _) = ty
    getType (LField ty _ _) = ty
