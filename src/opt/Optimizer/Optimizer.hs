module Optimizer.Optimizer(optimizeProgram) where

import Identifiers
import AST.AST
import AST.Util
import Types
import AST.PrettyPrinter
import Debug.Trace
import Data.Maybe
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
optimizerPasses = [constantFolding
                  ,constructors
                  ,sugarPrintedStrings
                  ,tupleMaybeIdComparison]++builtinExts

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

-- Some builtin types (like arrays and maybes) should behave like objects with
-- methods. These methods are defined as functions in the module `BuiltinExt`.
-- This pass substitutes function calls to these implementations for method
-- calls to arrays.
builtinExts = [builtinExt isArrayType  "array_"
              ,builtinExt isMaybeType  "maybe_"
              ,builtinExt isFutureType "fut_"]
  where
    builtinExt :: (Type -> Bool) -> String -> Expr -> Expr
    builtinExt targetTypePred functionPrefix = extend fe
        where
          fe e@(MethodCall {emeta
                           ,target
                           ,name=Name nam
                           ,args
                           ,typeArguments})
            | targetTypePred (getType target) = ret
             where
               ret = FunctionCall {emeta
                                  ,typeArguments=(getResultType $ getType target):typeArguments
                                  ,qname = QName{qnspace  = Just $ NSExplicit []
                                                ,qnsource = Just "BuiltinExt.enc"
                                                ,qnlocal  = Name $ functionPrefix++nam}
                                  ,args  = target:args}
          fe e = e

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

-- Desugars a == b when a : Just[t] and b : Just[t] into
-- match (a, b) with
--   case (Just(_fst), Just(_snd)) when _fst == _snd => true
--   case _                                          => false
-- end
tupleMaybeIdComparison = extend tupleMaybeIdComparison'
  where
  tupleMaybeIdComparison' Binop {emeta, binop, loper=MaybeValue{mdt=NothingData}, roper=MaybeValue{mdt=NothingData}} = setType boolType BTrue{emeta}

  tupleMaybeIdComparison' Binop {emeta, binop, loper, roper}
    | (isMaybeType $ getType loper) &&
      (isMaybeType $ getType roper) &&
      (binop == Identifiers.EQ || binop == Identifiers.NEQ) =
      tupleMaybeIdComparison $ maybeNeg Match{emeta, arg=setType tt Tuple{emeta, args}, clauses=[trueClause1, trueClause2, falseClause]}
    where
      tt = tupleType [lmty, rmty]
      args = [loper, roper]
      falseClause = MatchClause{mcpattern=setType tt VarAccess{emeta, qname=qName "_"}
                               ,mchandler=setType boolType BFalse{emeta}
                               ,mcguard=setType boolType BTrue{emeta}}
      trueClause1 = MatchClause{mcpattern=setType tt Tuple{emeta
                                                          ,args=[setType lmty MaybeValue{emeta, mdt=JustData lid}
                                                                ,setType rmty MaybeValue{emeta, mdt=JustData rid}]}
                               ,mchandler
                               ,mcguard=setType boolType Binop{emeta, binop, loper=lid, roper=rid}}
      trueClause2 = MatchClause{mcpattern=setType tt Tuple{emeta
                                                          ,args=[setType lmty MaybeValue{emeta, mdt=NothingData}
                                                                ,setType rmty MaybeValue{emeta, mdt=NothingData}]}
                               ,mchandler
                               ,mcguard=setType boolType BTrue{emeta}}
      lid = setType lty VarAccess{emeta, qname=qName "_fst"}
      rid = setType rty VarAccess{emeta, qname=qName "_snd"}
      leftResult = getResultType $ getType loper
      rightResult = getResultType $ getType roper
      -- When one operand is Nothing, replace its inferred bottom type by the type of the other operand
      lty = if leftResult == bottomType then rightResult else leftResult
      rty = if rightResult == bottomType then leftResult else rightResult
      lmty = maybeType lty
      rmty = maybeType rty
      -- Negate result when != comparison
      maybeNeg n = setType boolType $ if binop == Identifiers.EQ then n else Unary{emeta, uop=NOT, operand=n}
      mchandler = setType boolType BTrue{emeta}
  tupleMaybeIdComparison' b@Binop {emeta, binop, loper, roper}
    | (isTupleType $ getType loper) &&
      (isTupleType $ getType roper) &&
      (binop == Identifiers.EQ || binop == Identifiers.NEQ) =
      tupleMaybeIdComparison $ foldl and (setType boolType BTrue{emeta}) pairwiseCompare
    where
      and loper roper = setType boolType Binop{emeta, binop=Identifiers.AND, loper, roper}
      pairwiseCompare = map mkComparison [0..(tupleLength $ getType loper)-1]
      mkComparison idx = setType boolType Binop {emeta
                                                ,binop
                                                ,loper=setType (lty!!idx) TupleAccess{emeta
                                                                                     ,target=loper
                                                                                     ,compartment=idx}
                                                ,roper=setType (rty!!idx) TupleAccess{emeta
                                                                                     ,target=roper
                                                                                     ,compartment=idx}}
      lty = getArgTypes $ getType loper
      rty = getArgTypes $ getType roper

  tupleMaybeIdComparison' e = e

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
