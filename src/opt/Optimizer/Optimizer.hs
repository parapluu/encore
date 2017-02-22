module Optimizer.Optimizer(optimizeProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.Util
import AST.PrettyPrinter
import Types
import Control.Applicative (liftA2)

optimizeProgram :: Program -> Program
optimizeProgram p@(Program{classes, traits, functions}) =
    p{classes = map optimizeClass classes
     ,traits = map optimizeTrait traits
     ,functions = map optimizeFunction functions}
    where
      optimizeFunction f@(Function{funbody}) =
          f{funbody = optimizeExpr funbody}

      optimizeTrait t@(Trait{tmethods}) =
          t{tmethods = map optimizeMethod tmethods}

      optimizeClass c@(Class{cmethods}) =
          c{cmethods = map optimizeMethod cmethods}

      optimizeMethod m =
          m{mbody = optimizeExpr (mbody m)}

      optimizeExpr ast =
          foldl (\ast opt -> opt ast) ast optimizerPasses

-- | The functions in this list will be performed in order during optimization
optimizerPasses :: [Expr -> Expr]
optimizerPasses = [extend typedDesugar, constantFolding, constructors, sugarPrintedStrings]

-- Note that this is not intended as a serious optimization, but
-- as an example to how an optimization could be made. As soon as
-- there is a serious optimization in place, please remove this
-- function.
constantFolding :: Expr -> Expr
constantFolding = extend foldConst
    where
      foldConst (Binop {emeta = meta, binop = PLUS,
                        loper = IntLiteral{intLit = m},
                        roper = IntLiteral{intLit = n}}) =
          IntLiteral{emeta = meta, intLit = m + n}
      foldConst e = e

-- Calls to init are necessarily constructor calls and should
-- therefore be future-less message sends.
constructors :: Expr -> Expr
constructors = extend constr
    where
      constr e@(MethodCall {name, emeta, target, args})
          | name == constructorName &&
            (liftA2 (||) isActiveClassType isSharedClassType . getType) target =
              MessageSend {name = constructorName
                          ,emeta = emeta
                          ,target = target
                          ,args = args
                          ,typeArguments = []}
          | otherwise = e
      constr e = e

sugarPrintedStrings = extend sugarPrintedString
    where
      sugarPrintedString e@(Print{args}) =
        e{args = map simplifyStringLit args}
      sugarPrintedString e = e
      simplifyStringLit arg
        | NewWithInit{ty} <- arg
        , isStringObjectType ty
        , Just sugared <- getSugared arg
          = setType stringType sugared
        | otherwise = arg

typedDesugar TryOrDie{emeta, target} =
  Match emeta target [succ, fail]
  where
    succ = buildMatchClause (JustData value) value guard
    fail = buildMatchClause NothingData (Abort emeta [msg]) guard
    buildMatchClause matchPattern value guard = MatchClause (setType maybeType $ MaybeValue emeta matchPattern) value guard
    value = setType (getResultType maybeType) $ VarAccess emeta (qLocal (Name "result"))
    guard = setType boolType (BTrue emeta) 
    maybeType = getType target
    pos = show (Meta.getPos emeta) 
    msg  = setType stringObjectType $ NewWithInit{emeta
               ,ty = stringObjectType
               ,args = [setType stringType $ Embed emeta (ctype "char*")
                        [(show ("tryOrDie failed at " ++ pos ++ ": died executing " ++ (show (ppExpr target))) ++ ";", Skip emeta)]]
               }

typedDesugar e = e
    
