{-# LANGUAGE NamedFieldPuns #-}

module Optimizer.Optimizer(optimizeProgram) where

import Identifiers
import AST.AST
import AST.Util
import Types

optimizeProgram :: Program -> Program
optimizeProgram p@(Program{classes}) = p{classes = map optimizeClass classes}
    where
      optimizeClass c@(Class{methods}) = c{methods = map optimizeMethod methods}
      optimizeMethod m@(Method{mbody}) = m{mbody = optimizeExpr mbody}
      optimizeExpr ast = foldl (\ast opt -> opt ast) ast optimizerPasses

-- | The functions in this list will be performed in order during optimization
optimizerPasses :: [Expr -> Expr]
optimizerPasses = [constantFolding]

-- Note that this is not intended as a serious optimization, but
-- as an example to how an optimization could be made. As soon as
-- there is a serious optimization in place, please remove this
-- function.
constantFolding :: Expr -> Expr
constantFolding = extend foldConst
    where
      foldConst (Binop {emeta = meta, op = PLUS, loper = IntLiteral{intLit = m}, roper = IntLiteral{intLit = n}}) = 
          IntLiteral{emeta = meta, intLit = (m + n)}
      foldConst e = e