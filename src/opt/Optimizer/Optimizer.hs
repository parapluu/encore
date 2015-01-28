{-# LANGUAGE NamedFieldPuns #-}

module Optimizer.Optimizer(optimizeProgram) where

import Identifiers
import AST.AST
import AST.Util
import Types

optimizeProgram :: Program -> Program
optimizeProgram p@(Program{classes, functions}) =
    p{classes = map optimizeClass classes,
      functions = map optimizeFunction functions}
  where
      optimizeFunction f@(Function{funbody}) = f{funbody = optimizeExpr funbody}
      optimizeClass c@(Class{methods}) = c{methods = map optimizeMethod methods}
      optimizeMethod m = m{mbody = optimizeExpr (mbody m)}
      optimizeExpr ast = foldl (\ast opt -> opt ast) ast optimizerPasses

-- | The functions in this list will be performed in order during optimization
optimizerPasses :: [Expr -> Expr]
optimizerPasses = [constantFolding, constructors]

-- Note that this is not intended as a serious optimization, but
-- as an example to how an optimization could be made. As soon as
-- there is a serious optimization in place, please remove this
-- function.
constantFolding :: Expr -> Expr
constantFolding = extend foldConst
  where
      foldConst (Binop {emeta = meta, op = PLUS,
                        loper = IntLiteral{intLit = m},
                        roper = IntLiteral{intLit = n}}) =
          IntLiteral{emeta = meta, intLit = m + n}
      foldConst e = e

-- Calls to init are necessarily constructor calls and should
-- therefore be future-less message sends.
constructors :: Expr -> Expr
constructors = extend constr
  where
      constr e@(MethodCall {name = Name "_init", emeta, target, args})
        | (isActiveRefType . getType) target =
            MessageSend {name = Name "_init", emeta = emeta,
                         target = target, args = args}
        | otherwise = e
      constr e = e
