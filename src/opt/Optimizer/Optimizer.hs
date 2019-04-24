module Optimizer.Optimizer(optimizeProgram) where

import Identifiers
import AST.AST
import AST.Util
import qualified AST.Meta as Meta
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

      optimizeClass c@(Class{cname, cmethods})
        | isMainClass c =
            c{cmethods = map (optimizeMethod . addMainInitCall) cmethods}
        | otherwise =
            c{cmethods = map optimizeMethod cmethods}
        where
          addMainInitCall m@Method{mbody}
            | isMainMethod cname (methodName m) =
                let em = emeta mbody
                    this = setType cname
                           VarAccess{emeta = em, qname = qLocal thisName}
                    initCall = setType unitType
                               MethodCall{emeta = em
                                         ,target = this
                                         ,name = constructorName
                                         ,typeArguments = []
                                         ,args = []
                                         }
                in m{mbody = Seq{emeta = emeta mbody, eseq = [initCall, mbody]}}
            | otherwise = m

      optimizeMethod m =
          m{mbody = optimizeExpr (mbody m)}

      optimizeExpr ast =
          foldl (\ast opt -> opt ast) ast optimizerPasses

-- | The functions in this list will be performed in order during optimization
optimizerPasses :: [Expr -> Expr]
optimizerPasses = [constantFolding, sugarPrintedStrings, tupleMaybeIdComparison,
                   dropBorrowBlocks, forwardGeneral, forIntoFlatMapCall]

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
        , Just sugared@StringLiteral{} <- getSugared arg
          = setType stringType sugared
        | otherwise = arg

dropBorrowBlocks = extend dropBorrowBlock
    where
      dropBorrowBlock e@Borrow{emeta, target, name, body} =
        Let{emeta
           ,mutability = Val
           ,decls = [([VarNoType name], target)]
           ,body}
      dropBorrowBlock e = e

forwardGeneral = extend forwardGeneral'
  where
    forwardGeneral' e@(Forward{forwardExpr=MessageSend{}}) = e

    forwardGeneral' e@(Forward{forwardExpr=FutureChain{}}) = e

    forwardGeneral' e@(Forward{emeta, forwardExpr}) =
      Forward{emeta=emeta', forwardExpr=newExpr}
      where
         emeta' = Meta.setType (Meta.getType emeta) (Meta.meta $ Meta.getPos emeta)
         newExpr = FutureChain{emeta=fcmeta, future=forwardExpr, chain=idfun}
         fcmeta = Meta.setType (getType $ forwardExpr) (Meta.meta (Meta.getPos emeta'))
         idfun = Closure {emeta=mclosure
                          ,eparams=[pdecl]
                          ,mty=Just closureType
                          ,body=VarAccess {emeta=Meta.setType paramType mclosure
                                            ,qname=qName "_id_fun_tmp"}}
         closureType = arrowType [paramType] paramType
         mclosure = Meta.metaClosure "" (Meta.setType closureType emeta)
         paramType = getResultType . getType $ forwardExpr
         pdecl = Param {pmeta=Meta.setType paramType (Meta.meta (Meta.getPos emeta))
                        ,pmut =Val
                        ,pname=Name "_id_fun_tmp"
                        ,ptype=paramType
                        ,pdefault= Nothing}

    forwardGeneral' e = e

-- Desugars a for-loop into nested calls to map and flatMap:
--
-- for x <- listA, y <- listB, z <- ListC do
--      -- body of instructions
-- end
--
-- into
--
-- listA.flatMap(listB.flatMap(listC.map(body)))
-- Credit: kaeluka for the use of foldl1 and zipWith in this manner
forIntoFlatMapCall = extend forIntoFlatMapCall'
  where
    forIntoFlatMapCall' e@For{emeta, sources, body} =
      let
        n = length sources
        methodCalls = replicate (n-1) (Name "flatMap") ++ [Name "map"]
        setCalls = zipWith (intoMethodCall emeta) methodCalls sources
      in
        foldl1 (\procCall call -> procCall . call) setCalls $ body
      where

        intoMethodCall met methodName ForSource{forVar, collection} body =
          MethodCall {emeta = met,
                      typeArguments = [],
                      target = collection,
                      name = methodName,
                      args = [Closure {emeta = emeta,
                                       eparams = [Param {pmeta = Meta.meta (Meta.getPos met),
                                                         pmut = Val,
                                                         pname = forVar ,
                                                         ptype = intType, -- same problem as before, have to get the type. Hardcoded
                                                         pdefault = Nothing }],
                                       mty = Nothing,
                                       body = body}]
                      }

-- Variables that might be mutated in for-loops are boxed
--boxMutableVariables = extend boxMutableVariables'
--  extendMutableVariables' e@For{emeta, sources, body} = 0
