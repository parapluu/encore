module AST where


type Name = String

type Type = String

type Program = [ClassDecl]

data ClassDecl = Class {cname   :: Name, 
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]}

data FieldDecl = Field {fname :: Name, ftype::Type}

type ParamDecl = (Type, Name)

data MethodDecl = Method {mname   :: Name, 
                          rtype   :: Type, 
                          mparams :: [ParamDecl], 
                          mbody   :: Expr}

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

data Lvar = LVar Name | LField Expr Name | LThisField Name

example :: Program
example = [Class "Driver"
               []
               [Method "fact" "Int" [("Int", "n")] 
               (IfThenElse (Binop AST.EQ (VarAccess "n") (IntLiteral 0))
                 (IntLiteral 1)
                 (Let "m" (Call (VarAccess "n") "minus" [IntLiteral 1])
                   (Call (VarAccess "n") "mult" [Call (VarAccess "this") "fact" [(VarAccess "m")]])))],
 (Class "Main" 
            [Field "foo" "Foo", 
             Field "bar" "Bar"]
            [Method "main" "Object" []
               (Seq [Print (StringLiteral "Welcome to the pasture"),
                     (Let "driver" (New "Driver")
                       (Call (VarAccess "driver") "fact" [IntLiteral 13]))])])]
