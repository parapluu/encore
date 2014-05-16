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
          | Let {id :: Name, ty :: Type, val :: Expr, body :: Expr}
          | Seq {seq :: [Expr]}
          | IfThenElse {cond :: Expr, thn :: Expr, els :: Expr}
          | While {cond :: Expr, body :: Expr}
          | Get {fut :: Expr}
          | FieldAccess {path :: Expr, field :: Name}
          | Assign {lhs :: LVal, rhs :: Expr}
          | VarAccess {id :: Name}
          | Null
          | BTrue
          | BFalse
          | New {ty ::Type}
          | Print {ty :: Type, val :: Expr}
          | StringLiteral String
          | IntLiteral Int
          | Binop {op :: Op, loper :: Expr, roper :: Expr} deriving(Read, Show, Eq)

type Arguments = [Expr]

data LVal = LVal Name | LField Expr Name deriving(Read, Show, Eq)

