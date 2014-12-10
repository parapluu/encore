{-# LANGUAGE NamedFieldPuns #-}

module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import AST.PrettyPrinter
import AST.Util
import Types

desugarProgram :: Program -> Program
desugarProgram p@(Program{classes, functions}) = p{classes = map desugarClass classes, 
                                                   functions = map desugarFunction functions}
    where
      desugarFunction f@(Function{funbody}) = f{funbody = desugarExpr funbody}
      desugarClass c@(Class{methods}) = c{methods = map desugarMethod methods}
      desugarMethod m@(Method{mbody}) = m{mbody = desugarExpr mbody}
      desugarExpr = extend desugar

desugar :: Expr -> Expr

desugar FunctionCall{emeta, name = Name "exit", args} = Exit emeta args

desugar FunctionCall{emeta, name = Name "print", args = (string@(StringLiteral {stringLit = s})):args} = 
    Print emeta s args

desugar fCall@FunctionCall{emeta, name = Name "assertTrue", args = [cond]} = 
    IfThenElse emeta cond
           (Skip emeta)
           (Seq emeta [Print emeta ("Assertion failed: " ++ (show $ ppExpr fCall) ++ "\n") [],
                       Exit emeta [IntLiteral emeta 1]])

desugar fCall@FunctionCall{emeta, name = Name "assertFalse", args = [cond]} = 
    IfThenElse emeta cond 
           (Seq emeta [Print emeta ("Assertion failed: " ++ (show $ ppExpr fCall) ++ "\n") [],
                       Exit emeta [IntLiteral emeta 1]])
           (Skip emeta)

desugar fCall@FunctionCall{emeta, name = Name "assertTrue", args = cond : lit@(StringLiteral {stringLit = s}) : rest} = 
    IfThenElse emeta cond 
           (Skip emeta)
           (Seq emeta [Print emeta ("Assertion failed: " ++ s ++ "\n") rest,
                       Exit emeta [IntLiteral emeta 1]])

desugar fCall@FunctionCall{emeta, name = Name "assertFalse", args = cond : lit@(StringLiteral {stringLit = s}) : rest} = 
    IfThenElse emeta cond 
           (Seq emeta [Print emeta ("Assertion failed: " ++ s ++ "\n") rest,
                       Exit emeta [IntLiteral emeta 1]])
           (Skip emeta)

desugar u@IfThen{emeta, cond, thn} =
    IfThenElse emeta cond thn (Skip emeta)

desugar u@Unless{emeta, cond, thn} = 
    IfThenElse emeta (Unary emeta Identifiers.NOT cond) thn (Skip emeta)

-- Desugars
--   repeat id <- e1 e2
-- into
--   let 
--     id = 0
--     tmp = e1
--   in
--     while id < tmp
--       {
--         e2;
--         id = id + 1;
--       }
desugar Repeat{emeta, name, times, body} = 
    Let emeta 
        [(name, (IntLiteral emeta 0)), (Name "__gub__", times)]
       (While emeta 
             (Binop emeta 
                   Identifiers.LT
                   (VarAccess emeta name) 
                   (VarAccess emeta (Name "__gub__")))
             (Seq emeta 
                  [body, (Assign emeta 
                               (VarAccess emeta name)
                               (Binop emeta 
                                     PLUS
                                     (VarAccess emeta name)
                                     (IntLiteral emeta 1)))]))

desugar n@NewWithInit{emeta, ty, args} = 
    Let (emeta) [(Name "__tmp__", (New emeta ty))] (Seq emeta [(MethodCall (emeta) (VarAccess emeta (Name "__tmp__")) (Name "init") args), (VarAccess emeta (Name "__tmp__"))])

desugar e = e
