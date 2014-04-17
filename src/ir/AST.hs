module AST where

import Data.Traversable

type Name = String

type Type = String

newtype Program = Program [ClassDecl] deriving(Read, Show)

data ClassDecl = Class {cname   :: Name, 
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Read, Show)

data FieldDecl = Field {fname :: Name, ftype::Type} deriving(Read, Show)

mkFields :: [(Name, Type)] -> [FieldDecl]
mkFields = map (uncurry Field)

newtype ParamDecl = Param (Type, Name) deriving(Read, Show)

data MethodDecl = Method {mname   :: Name, 
                          rtype   :: Type, 
                          mparams :: [ParamDecl],
                          mbody   :: Expr} deriving(Read, Show)

mkMethods :: [(Name, Type, [(Type, Name)], Expr)] -> [MethodDecl]
mkMethods = map (\(n, t, ps, e) -> Method n t (map Param ps) e)

data Expr = Skip
          | Call {target :: Expr, tmname :: Name, args :: Arguments}
          | Let Name Expr Expr
          | Seq [Expr]
          | IfThenElse Expr Expr Expr
          | Get Expr
          | FieldAccess Expr Name
          | Assign Lvar Expr
          | VarAccess Name
          | Null
          | New Name
          | Print Expr
          | StringLiteral String
          | IntLiteral Int
          | Binop Op Expr Expr deriving(Read, Show)

data Op = LT | GT | EQ | NEQ | PLUS | MINUS deriving(Read, Show)

type Arguments = [Expr]

data Lvar = LVar Name | LField Expr Name | LThisField Name deriving(Read, Show)
