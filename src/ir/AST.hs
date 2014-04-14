

type Name = String

type Type = String

data Program = [ClassDecl]

data ClassDecl = Class {cname :: Name, 
                    fields::[Field], 
                    methods :: [Method]}

data FieldDecl = Field {fname :: Name, ftype::Type}

data MethodDecl = Method {mname :: Name, rtype::Type, mparams :: [ParamDecl], mbody :: Expr}

type ParamDecl = (Type, Name)

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
          | Binop Op Expr Expr

data Op = LT | GT | EQ | NEQ


type Arguments = [Expr]

data Lvar = LVar Name | LField Name | LThisField Name
