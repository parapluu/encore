import Text.PrettyPrint

type Name = String
type Type = String

type Program = [ClassDecl]

data ClassDecl = Class Name [FieldDecl] [MethodDecl]

data FieldDecl = Field Type Name

data MethodDecl = Method Name [ArgDecl] Type Expr

type ArgDecl = (Type, Name)

data Expr = Skip
          | Call Expr Name [Expr]
          | Let Name Expr Expr
          | IfThenElse Expr Expr Expr
          | Get Expr
          | FieldAccess Expr Name
          | FieldAssign Expr Name Expr
          | VarAccess Name
          | VarAssign Name Expr
          | Null
          | New Name
          | Print Expr
          | StringLiteral String
          | IntLiteral Int
          | Binop Op Expr Expr

data Op = LT | GT | EQ | NEQ