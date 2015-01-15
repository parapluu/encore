{-# LANGUAGE NamedFieldPuns #-}

module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types

desugarProgram :: Program -> Program
desugarProgram p@(Program{classes, functions}) = p{classes = map desugarClass classes, 
                                                   functions = map desugarFunction functions}
    where
      desugarFunction f@(Function{funbody}) = f{funbody = desugarExpr funbody}
      desugarClass c@(Class{methods}) = c{methods = map desugarMethod methods}
      desugarMethod m@(Method{mname, mbody}) 
          | mname == Name "init" = m{mname = Name "_init", mbody = desugarExpr mbody}
          | otherwise = m{mbody = desugarExpr mbody}
      desugarMethod m@(StreamMethod{mname, mbody}) 
          | mname == Name "init" = m{mname = Name "_init", mbody = desugarExpr mbody}
          | otherwise = m{mbody = desugarExpr mbody}
      desugarExpr = (extend desugar) . (extend (\e -> setSugared e e))

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = (Meta.meta (Meta.sourcePos m))

desugar :: Expr -> Expr

desugar FunctionCall{emeta, name = Name "exit", args} = Exit emeta args

desugar FunctionCall{emeta, name = Name "print", args = (string@(StringLiteral {stringLit = s})):args} = 
    Print emeta s args

desugar fCall@FunctionCall{emeta, name = Name "assertTrue", args = [cond]} = 
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ (show $ ppExpr fCall) ++ "\n") [],
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar fCall@FunctionCall{emeta, name = Name "assertFalse", args = [cond]} = 
    IfThenElse emeta cond 
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ (show $ ppExpr fCall) ++ "\n") [],
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar FunctionCall{emeta, name = Name "assertTrue", args = cond : lit@(StringLiteral {stringLit = s}) : rest} = 
    IfThenElse emeta cond 
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ s ++ "\n") rest,
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar FunctionCall{emeta, name = Name "assertFalse", args = cond : lit@(StringLiteral {stringLit = s}) : rest} = 
    IfThenElse emeta cond 
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ s ++ "\n") rest,
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar IfThen{emeta, cond, thn} =
    IfThenElse emeta cond thn (Skip (Meta.meta (Meta.sourcePos (cloneMeta emeta))))

desugar Unless{emeta, cond, thn} = 
    IfThenElse emeta (Unary (cloneMeta emeta) Identifiers.NOT cond) thn (Skip (cloneMeta emeta))

-- Desugars
--   repeat id <- e1 e2
-- into
--   let 
--     id = 0
--     __ub__ = e1
--   in
--     while id < __ub__
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

desugar NewWithInit{emeta, ty, args} = 
    Let emeta 
        [(Name "__tmp__", (New (cloneMeta emeta) ty))] 
      (Seq (cloneMeta emeta) 
           [(MethodCall ((cloneMeta emeta)) 
               (VarAccess (cloneMeta emeta) (Name "__tmp__")) 
               (Name "_init") args), 
            (VarAccess (cloneMeta emeta) (Name "__tmp__"))])

desugar e = e
