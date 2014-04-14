
module CodeGen where

import CCode
import AST


codegenP _ = undefined

codegenC (Class n f m) = undefined


codegenM (Method n rt params body) = undefined

codegenF (Field f t) = undefined

codegenE Skip = undefined
codegenE (Call t n args) = undefined
codegenE (Let v e1 e2)   = undefined
codegenE (Seq es) = undefined
codegenE (IfThenElse g t e)
codegenE (Get e) = undefined
codegenE (FieldAccess Expr Name) = undefined
codegenE (Assign Lvar Expr) = undefined
codegenE (VarAccess Name) = undefined
codegenE Null = undefined
codegenE (New Name) = undefined
codegenE (Print Expr) = undefined
codegenE (StringLiteral String) = undefined
codegenE (IntLiteral Int) = undefined
codegenE (Binop Op Expr Expr) = undefined
