module Optimizer.TypedDesugarer(desugarTypedProgram) where

import Identifiers
import AST.AST
import AST.Util
import qualified AST.Meta as Meta
import Types
import Control.Applicative (liftA2)
import Data.Maybe


desugarTypedProgram :: Program -> Program
desugarTypedProgram p@(Program{classes, traits, functions}) =
    p{classes = map desugarClass classes
     ,traits = map desugarTrait traits
     ,functions = map desugarFunction functions}
    where
      desugarFunction f@(Function{funbody}) =
          f{funbody = desugarExpr funbody}

      desugarTrait t@(Trait{tmethods}) =
          t{tmethods = map desugarMethod tmethods}

      desugarClass c@(Class{cname, cmethods})=
          c{cmethods = map desugarMethod cmethods}

      desugarMethod m =
          m{mbody = desugarExpr (mbody m)}

      desugarExpr ast =
          foldl (\ast opt -> opt ast) ast desugarPasses

-- | The functions in this list will be performed in order during desugaring
desugarPasses :: [Expr -> Expr]
desugarPasses = [forDesugared]

-- Desugars a for-loop into nested calls to map and flatMap and foreach:
--
-- for x <- listA, y <- listB, z <- ListC do
--      fun
-- end
--
-- into listA.flatMap(listB.flatMap(listC.map(fun)))
--
-- Credit: kaeluka for the use of foldl1 and zipWith in this manner
forDesugared = extend forDesugared'
  where
    forDesugared' :: Expr -> Expr
    forDesugared' e@For{emeta, sources, body} =
      let
        n = length sources
        callNameList = if (AST.AST.isCaptured e) || (unitType == getType body)
                      then replicate n (Name "foreach")
                      else replicate (n-1) (Name "flatMap") ++ [Name "map"]
        revSources = reverse sources
        elemType = getType body
        forTrace = if isRangeType $ getType (collection (head sources))
                   then e
                   else nestCalls emeta callNameList sources body elemType
      in
       -- trace ("Afterwards                   " ++ (show (ppExpr forTrace))) forTrace
       forTrace
    forDesugared' e =  e

nestCalls :: Meta.Meta Expr -> [Name] -> [ForSource] -> Expr -> Type -> Expr -- nested MethodCalls and FunctionCalls
nestCalls meta (name:_) (fs:[]) body elemType = intoCall meta name fs body elemType
nestCalls meta (name:restOfNames) (fs:restFS) body elemType =
  let nestedCall = intoCall meta name fs body elemType
  in nestCalls meta restOfNames restFS nestedCall elemType

intoCall :: Meta.Meta Expr -> Name -> ForSource -> Expr -> Type -> Expr -- MethodCall or FunctionCall
intoCall met callName ForSource{fsName, fsTy, collection} bodyOrMethodCall elemType =
  if isRefType (getType collection)
  then let
        param = [intoParam met Val fsName fsTy]
        arguments = [bodyOrMethodCall] --[intoClosure met param Nothing bodyOrMethodCall]
        elemT = if callName == Name "foreach" -- this feels iffy
                then []
                else [elemType]
       in
        intoMethodCall met elemT collection callName arguments
   else let
        param = [intoParam met Val fsName fsTy]
        arguments = [bodyOrMethodCall] -- [intoClosure met param Nothing bodyOrMethodCall] ++ [collection]
        elemT = if callName == Name "foreach" -- this feels iffy
                then [fromMaybe intType fsTy]
                else [(fromMaybe intType fsTy), elemType]
       in
        intoFunctionCall met elemT callName arguments





{-forBoxed = extends forBoxed'
  where
    forBoxed' for@(For {emeta, sources, body}) =
      let listOfVar = getVar body
        listOfVarAcc = boxVar listOfVar
        fielAccBody = foldl (\ast opt -> opt ast) body [varAccessToFieldAccess]
        unBox = unBoxFreeVariables box freeVariables
      in intoSeq emeta [box, For{emeta, sources, fieldAccBody}, unBox]

getVar [] list = list
getVar (b:ody) list
  | IsVarAssignment b = getVar ody ((getLHS b):list)
  | otherwise = getVar (ody ++ (getChildren b)) list
  where
    isVarAssignment Assign{lhs = VarAccess{}} = True
    isVarAssignment _ = False
    getLHS Assign{lhs = VarAccess{}} = lhs

box v@VarAccess{emeta, qname} =
  let boxTy = getBoxtype (getType v)
    box = intoNewWithInit emeta boxTy [v]
    newDecl = intoParam Name $ "__" ++ (show (qnlocal qname))

varAccessToFieldAccess v@VarAccess{emeta, qname{qnlocal}} =
  let boxTy = getBoxType (getType v)
    argument = [v]
    box = intoNewWithInit emeta boxTy arguments
  in intoFieldAccess emeta box qnlocal
varAccessToFieldAccess m = m

getBoxType primType
  | intType == primType = MutInteger
  | uintType == primType = MutUinteger
  | realType == primType = MutReal
  | boolType == primType = MutBool
  | stringType == primType = MutString
  | charType == primType = MutChar -}

intoBinop emeta op left right =
  Binop {emeta = emeta,
          binop = op,
          loper = left,
          roper = right}

intoClosure meta parameters mty body =
  Closure {emeta = meta,
           eparams = parameters,
           mty = mty,
           body = body}

intoParam emetaP mutP nameP maybeTyP =
  Param {pmeta = Meta.meta (Meta.getPos emetaP),
         pmut = mutP,
         pname = nameP,
         ptype = fromMaybe intType maybeTyP,
         pdefault = Nothing}


intoFunctionCall meta typeArg name arguments =
  FunctionCall {emeta = meta,
                typeArguments = typeArg,
                qname = QName{qnspace = Nothing, qnsource = Nothing, qnlocal = name},
                args = arguments}

intoMethodCall meta typeArg object nam arguments =
  MethodCall {emeta = meta,
              typeArguments = typeArg,
              target = object,
              name = nam,
              args = arguments}

intoAssignment meta left right =
  Assign {emeta = meta,
          lhs = left,
          rhs = right}

intoFieldAccess met object nam =
  FieldAccess{ emeta = met,
               target = object,
               name = nam}

intoSeq meta listOfExpr =
  Seq {emeta = meta,
       eseq = listOfExpr}

intoNewWithInit meta boxTy arguments =
  NewWithInit{emeta = meta,
              ty = boxTy,
              args = arguments}
