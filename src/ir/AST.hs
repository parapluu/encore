module AST where

import Data.Traversable

type Name = String

type Type = String

newtype Program = Program [ClassDecl]

data ClassDecl = Class {cname   :: Name, 
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]}

data FieldDecl = Field {fname :: Name, ftype::Type}

mkFields :: [(Name, Type)] -> [FieldDecl]
mkFields = map (uncurry Field)

newtype ParamDecl = Param (Type, Name)

data MethodDecl = Method {mname   :: Name, 
                          rtype   :: Type, 
                          mparams :: [ParamDecl],
                          mbody   :: Expr}

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
          | Binop Op Expr Expr deriving (Show)

data Op = LT | GT | EQ | NEQ | PLUS | MINUS deriving (Show,Eq)

type Arguments = [Expr]

data Lvar = LVar Name | LField Expr Name | LThisField Name deriving (Show)
