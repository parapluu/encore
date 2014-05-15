{-|

The abstract syntax tree produced by the parser. Carries no
additional information other than the program itself.

-}

module AST.AST where

import Identifiers

newtype Program = Program [ClassDecl] deriving(Read, Show)

data ClassDecl = Class {cname   :: Type,
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Read, Show, Eq)

data FieldDecl = Field {fname :: Name, ftype::Type} deriving(Read, Show, Eq)

mkFields :: [(Name, Type)] -> [FieldDecl]
mkFields = map (uncurry Field)

newtype ParamDecl = Param (Name, Type) deriving(Read, Show, Eq)

data MethodDecl = Method {mname   :: Name,
                          rtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving (Read, Show, Eq)

mkMethods :: [(Name, Type, [(Name, Type)], Expr)] -> [MethodDecl]
mkMethods = map (\(n, t, ps, e) -> Method n t (map Param ps) e)

data Expr = Skip
          | Call {target :: Expr, tmname :: Name, args :: Arguments}
          | Let Name Type Expr Expr
          | Seq [Expr]
          | IfThenElse Expr Expr Expr
          | While Expr Expr
          | Get Expr
          | FieldAccess Expr Name
          | Assign LVal Expr
          | VarAccess Name
          | Null
          | BTrue
          | BFalse
          | New Type
          | Print Type Expr
          | StringLiteral String
          | IntLiteral Int
          | Binop Op Expr Expr deriving(Read, Show, Eq)

type Arguments = [Expr]

data LVal = LVal Name | LField Expr Name deriving(Read, Show, Eq)
