module Optimizer.TypedDesugarer(desugarTypedProgram) where


import Debug.Trace

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import SystemUtils

-- Modular dependancies
import Identifiers
import AST.AST
import AST.Util
import Identifiers
import qualified AST.Meta as Meta
import Types
import Typechecker.Environment
import Typechecker.TypeError
import AST.PrettyPrinter

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
desugarPasses = [desugarAndBoxForInSeq, desugarAndBoxForNotInSeq]

desugarAndBoxForInSeq = extend desugarAndBoxForInSeq'
  where desugarAndBoxForInSeq' e@Seq{} = boxed e
        desugarAndBoxForInSeq' e = e
        boxed e = boxForInSeq e

desugarAndBoxForNotInSeq = extend desugarAndBoxForNotInSeq'
  where desugarAndBoxForNotInSeq' for@For{} = forBoxed for []
        desugarAndBoxForNotInSeq' e = e



-- Desugars a for-loop into nested calls to map and flatMap and foreach:
--
-- for x <- listA, y <- listB, z <- ListC do
--      fun
-- end
--
-- into listA.flatMap(listB.flatMap(listC.map(fun)))
forDesugared :: Expr -> Expr
forDesugared e@For{emeta, sources, body} =
  let n = length sources
      collectionType = getType $ collection $ head sources
      callNameList = if (not (AST.AST.isCaptured e)) || (unitType == getType body) || (isRangeObjectType collectionType)
                     then replicate n (Name "foreach")
                     else replicate (n-1) (Name "flatMap") ++ [Name "map"]
      revSources = reverse sources
      elemType = getType body
      desugaredFor = nestCalls emeta callNameList sources body elemType
  in  desugaredFor
forDesugared m = m

nestCalls :: Meta.Meta Expr -> [Name] -> [ForSource] -> Expr -> Type -> Expr
nestCalls meta (name:_) (fs:[]) body elemType = intoCall meta name fs body elemType
nestCalls meta (name:restOfNames) (fs:restFS) body elemType =
  let nestedCall = intoCall meta name fs body elemType
  in nestCalls meta restOfNames restFS nestedCall elemType

intoCall :: Meta.Meta Expr -> Name -> ForSource -> Expr -> Type -> Expr
intoCall met callName ForSource{fsName, fsTy, collection} bodyOrMethodCall elemType =
  if isRefType (getType collection)
  then let param = [intoParam met Val fsName fsTy]
           arguments = [intoClosure met param Nothing bodyOrMethodCall]
           elemT = if callName == Name "foreach" -- this feels iffy
                   then []
                   else [elemType]
       in  intoMethodCall met elemT collection callName arguments
   else let param = [intoParam met Val fsName fsTy]
            arguments = [intoClosure met param Nothing bodyOrMethodCall] ++ [collection]
            elemT =  if callName == Name "foreach" -- this feels iffy
                     then [fromMaybe intType fsTy]
                     else [(fromMaybe intType fsTy), elemType]
            name = intoQName callName
       in   intoFunctionCall met elemT name arguments

boxForInSeq :: Expr -> Expr
boxForInSeq e@Seq{emeta, eseq} =
  let newEseq = boxFor eseq []
  in  e{eseq = newEseq}
  where
    boxFor :: [Expr] -> [Expr] -> [Expr]
    boxFor [] newEseq =  newEseq
    boxFor (ex:expr) newEseq
      | isFor ex = newEseq ++ [forBoxed ex (boxFor expr [])]
      | otherwise = boxFor expr (newEseq ++ [ex])
    isFor For{} = True
    isFor _ = False


forBoxed :: Expr -> [Expr] -> Expr
forBoxed for@For{emeta, sources, body} postForExpr =
  let listOfVar = getVariables body
      listOfVarNames = map getVarName listOfVar
      getVarName VarAccess{qname} = qnlocal qname
      unBoxed = unBox listOfVar
      desugaredForWithFieldAccBody = forDesugared $ varBodyToFieldBody for [] listOfVarNames
      bodyforLetBoxes = intoSeq emeta (desugaredForWithFieldAccBody:unBoxed ++ postForExpr)
      boxLet = boxVar emeta listOfVar bodyforLetBoxes
      newSeq = intoSeq emeta [boxLet]
  in  newSeq

getVariables :: Expr -> [Expr]
getVariables body = removeDuplicates (fst (filterVar body)) [] []
  where
    removeDuplicates :: [Expr] -> [Name] -> [Expr] -> [Expr]
    removeDuplicates [] _ finalList = finalList
    removeDuplicates (e@VarAccess{qname}:expr) listOfNames finalList
      | (qnlocal qname) `elem` listOfNames = removeDuplicates expr listOfNames finalList
      | otherwise = removeDuplicates expr ((qnlocal qname):listOfNames) (e:finalList)
    removeDuplicates (_:expr) listOfNames finalList = undefined

filterVar :: Expr -> ([Expr], [Name])
filterVar = foldrExp (\e (acc, declAcc) -> if isNotLocalVar e declAcc
                                           then ((getVar e):acc, declAcc)
                                           else if isLet e
                                                then (acc, (getDecls e) ++ declAcc)
                                                else (acc, declAcc)) ([], [])
                 where
                   isNotLocalVar Assign{lhs = VarAccess{qname}} decl = not $ (Name (show (qnlocal qname))) `elem` decl
                   isNotLocalVar _ decl = False
                   isLet Let{} = True
                   isLet _ = False
                   getVar Assign{lhs} = lhs
                   getDecls Let{decls} = concatMap getDecls' $ fst $ unzip decls
                   getDecls' declList = map getDecl declList
                   getDecl VarNoType{varName} = varName
                   getDecl VarType{varName}= varName


varBodyToFieldBody body declList boxedVarList = extend (varBodyToFieldBody' declList boxedVarList) body
  where
    varBodyToFieldBody' declList boxedVarList v@VarAccess{qname}
      | isLocalVar v declList && isBoxedVar v boxedVarList = varAccToFieldAcc v
      | otherwise = v
    varBodyToFieldBody' declList boxedVarList l@Let{decls}  = varBodyToFieldBody l (getDecls l ++ declList) boxedVarList
    varBodyToFieldBody' declList boxedVarList m  = m
    isLocalVar VarAccess{qname} decl = not $ (Name (show (qnlocal qname))) `elem` decl
    isBoxedVar VarAccess{qname} boxedNameList = (qnlocal qname) `elem` boxedNameList
    getDecls Let{decls} = concatMap getDecls' $ fst $ unzip decls
    getDecls' declList = map getDecl declList
    getDecl VarNoType{varName} = varName
    getDecl VarType{varName}= varName


boxVar meta listOfVar body =
  intoLet meta (makeDecls meta listOfVar) body
  where
    makeDecls meta varAccess = map (makeDecl meta) varAccess
    makeDecl emeta v@VarAccess{qname} =
      let box = boxNewWithInit emeta [getType v] [v]
          variableDecl = intoVarDecl $ Name ("__box_mutable__" ++ show (qnlocal qname))
      in  ([variableDecl], box)


varAccToFieldAcc VarAccess{emeta, qname} =
  let boxQname = intoQName (Name ("__box_mutable__" ++ show (qnlocal qname)))
      boxVarAcc = intoVarAccess emeta boxQname
  in intoFieldAccess emeta boxVarAcc (Name "value")

unBox varAccList = map (unBoxVar) varAccList
  where unBoxVar VarAccess{emeta, qname} = intoAssignment emeta (intoVarAccess emeta qname) (fieldAccessRhs emeta qname)
        boxQname qname = intoQName (Name ("__box_mutable__" ++ show (qnlocal qname)))
        boxVarAcc emeta qname = intoVarAccess emeta (boxQname qname)
        fieldAccessRhs emeta qname = intoFieldAccess emeta (boxVarAcc emeta qname) (Name "value")

intoVarAccess meta name =
  VarAccess{emeta = Meta.meta (Meta.getPos meta),
            qname = name}

intoClosure meta parameters mty body =
  Closure {emeta = Meta.meta (Meta.getPos meta),
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
  FunctionCall {emeta = Meta.meta (Meta.getPos meta),
                typeArguments = typeArg,
                qname = name,
                args = arguments}

intoQName name =
  QName{qnspace = Nothing,
        qnsource = Nothing,
        qnlocal = name}

intoMethodCall meta typeArg object nam arguments =
  MethodCall {emeta = Meta.meta (Meta.getPos meta),
              typeArguments = typeArg,
              target = object,
              name = nam,
              args = arguments}

intoAssignment meta left right =
  Assign {emeta = Meta.meta (Meta.getPos meta),
          lhs = left,
          rhs = right}

intoFieldAccess meta object nam =
  FieldAccess{ emeta = Meta.meta (Meta.getPos meta),
               target = object,
               name = nam}

intoSeq meta listOfExpr =
  Seq {emeta = Meta.meta (Meta.getPos meta),
       eseq = listOfExpr}

boxNewWithInit meta parameters arguments =
  NewWithInit{emeta = Meta.meta (Meta.getPos meta),
              ty = boxObjectType parameters,
              args = arguments}

intoVarDecl name =
  VarNoType{varName = name}

intoLet meta varDecls body =
  Let {emeta = Meta.meta (Meta.getPos meta),
      mutability = Val,
      decls = varDecls,
      body = body}
