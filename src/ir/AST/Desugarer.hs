{-# LANGUAGE NamedFieldPuns #-}

module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import AST.Util
import Types

desugarProgram :: Program -> Program
desugarProgram p@(Program{classes}) = p{classes = map desugarClass classes}
    where
      desugarClass c@(Class{methods}) = c{methods = map desugarMethod methods}
      desugarMethod m@(Method{mbody}) = m{mbody = desugarExpr mbody}
      desugarExpr = extend desugar

desugar :: Expr -> Expr
desugar FunctionCall{emeta, name = Name "exit", args} = Exit emeta args
desugar e = e