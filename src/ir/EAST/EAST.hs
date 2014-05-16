{-# LANGUAGE MultiParamTypeClasses #-}

{-|

An abstract syntax tree extended with types, produced by the type
checker.

-}

module EAST.EAST where

import Identifiers
import qualified AST.AST as A

newtype Program = Program [ClassDecl] deriving(Read, Show)

data ClassDecl = Class {cname   :: Type,
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Read, Show, Eq)

data FieldDecl = Field {fname :: Name, ftype :: Type} deriving(Read, Show, Eq)

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

-- | Type class for the Extended AST nodes that can be stripped of
-- their type information (leaving a normal AST)
class ExtendedFrom a b where
    toAST   :: a -> b
    fromAST :: b -> a

instance ExtendedFrom Program A.Program where
    toAST (Program classes) = A.Program $ map toAST classes
    fromAST (A.Program classes) = Program $ map fromAST classes

instance ExtendedFrom ClassDecl A.ClassDecl where
    toAST (Class cname fields methods) = A.Class cname (map toAST fields) (map toAST methods)
    fromAST (A.Class cname fields methods) = Class cname (map fromAST fields) (map fromAST methods)

instance ExtendedFrom FieldDecl A.FieldDecl where
    toAST (Field fname ftype) = A.Field fname ftype
    fromAST (A.Field fname ftype) = Field fname ftype

instance ExtendedFrom MethodDecl A.MethodDecl where
    toAST (Method mname rtype mparams mbody) = A.Method mname rtype mparams (toAST mbody)
    fromAST (A.Method mname rtype mparams mbody) = Method mname rtype mparams (fromAST mbody)

instance ExtendedFrom Expr A.Expr where
    toAST Skip = A.Skip
    toAST (Call _ target tmname args) = A.Call (toAST target) tmname (map toAST args)
    toAST (Let _ id idType val body) = A.Let id idType (toAST val) (toAST body)
    toAST (Seq _ seq) = A.Seq (map toAST seq)
    toAST (IfThenElse _ cond thn els) = A.IfThenElse (toAST cond) (toAST thn) (toAST els)
    toAST (While _ cond body) = A.While (toAST cond) (toAST body)
    toAST (Get _ fut) = A.Get (toAST fut)
    toAST (FieldAccess _ path field) = A.FieldAccess (toAST path) field
    toAST (Assign _ lhs rhs) = A.Assign (toAST lhs) (toAST rhs)
    toAST (VarAccess _ id) = A.VarAccess id
    toAST Null = A.Null
    toAST BTrue = A.BTrue
    toAST BFalse = A.BFalse
    toAST (New ty) = A.New ty
    toAST (Print ty val) = A.Print ty (toAST val)
    toAST (StringLiteral s) = A.StringLiteral s
    toAST (IntLiteral n) = A.IntLiteral n
    toAST (Binop _ op loper roper) = A.Binop op (toAST loper) (toAST roper)

    fromAST A.Skip = Skip
    fromAST (A.Call target tmname args) = Call nullType (fromAST target) tmname (map fromAST args)
    fromAST (A.Let id idType val body) = Let nullType id idType (fromAST val) (fromAST body)
    fromAST (A.Seq seq) = Seq nullType (map fromAST seq)
    fromAST (A.IfThenElse cond thn els) = IfThenElse nullType (fromAST cond) (fromAST thn) (fromAST els)
    fromAST (A.While cond body) = While nullType (fromAST cond) (fromAST body)
    fromAST (A.Get fut) = Get nullType (fromAST fut)
    fromAST (A.FieldAccess path field) = FieldAccess nullType (fromAST path) field
    fromAST (A.Assign lhs rhs) = Assign nullType (fromAST lhs) (fromAST rhs)
    fromAST (A.VarAccess id) = VarAccess nullType id
    fromAST A.Null = Null
    fromAST A.BTrue = BTrue
    fromAST A.BFalse = BFalse
    fromAST (A.New ty) = New ty
    fromAST (A.Print ty val) = Print ty (fromAST val)
    fromAST (A.StringLiteral s) = StringLiteral s
    fromAST (A.IntLiteral n) = IntLiteral n
    fromAST (A.Binop op loper roper) = Binop nullType op (fromAST loper) (fromAST roper)

instance ExtendedFrom LVal A.LVal where
    toAST (LVal _ x) = A.LVal x
    toAST (LField _ expr f) = A.LField (toAST expr) f

    fromAST (A.LVal x) = LVal nullType x
    fromAST (A.LField expr f) = LField nullType (fromAST expr) f