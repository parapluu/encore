{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @Function@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Function where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr ()
import CodeGen.Type
import CodeGen.Closure
import CodeGen.Task
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import Types

import Control.Monad.State hiding(void)
import Control.Arrow((&&&))

globalFunctionDecl :: A.Function -> CCode Toplevel
globalFunctionDecl f =
  FunctionDecl typ name (Ptr encoreCtxT:params)
  where
    params = map (translate . A.ptype) $ A.functionParams f
    typ = translate $ A.functionType f
    name = globalFunctionNameOf f

globalFunctionClosureDecl :: A.Function -> CCode Toplevel
globalFunctionClosureDecl f =
  DeclTL (closure, AsLval $ globalFunctionClosureNameOf f)

initGlobalFunctionClosure :: A.Function -> CCode Toplevel
initGlobalFunctionClosure f =
  AssignTL var value
  where
    var = Decl (Ptr struct, AsLval $ globalFunctionClosureNameOf f)
    value = CompoundLiteral struct [(closureFun, address)]
    struct = Struct closureStructName
    closureFun = AsLval closureStructFFieldName
    address = Cast (Ptr void) $ Amp $ globalFunctionWrapperNameOf f

globalFunctionWrapper :: A.Function -> CCode Toplevel
globalFunctionWrapper f =
  let
    args = A.functionParams f
    argList = encoreCtxVar : extractArgs args
  in
    Function
      (Typ "value_t")
      name
      [(Ptr encoreCtxT, encoreCtxVar), (Typ "value_t", Var "_args[]"), (Ptr void, Var "_env_not_used")]
      $ returnStmnt (Call globalFunctionName argList) typ
  where
    typ = A.functionType f
    name = globalFunctionWrapperNameOf f

    globalFunctionName = globalFunctionNameOf f

    extractArgs :: [A.ParamDecl] -> [CCode Lval]
    extractArgs args = [getArg arg i | (arg, i) <- zip args [0..]]

    getArg :: A.ParamDecl -> Int -> CCode Lval
    getArg A.Param{A.ptype} i =
      fromEncoreArgT (translate ptype) $ AsExpr $ ArrAcc i $ Var "_args"

    returnStmnt :: UsableAs e Expr => CCode e -> Type -> CCode Stat
    returnStmnt var ty = Return $ asEncoreArgT (translate ty) var

instance Translatable A.Function (ClassTable -> CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funbody}) ctable =
      let funParams = A.functionParams fun
          funType   = A.functionType fun
          funName   = globalFunctionName $ A.functionName fun
          (encArgNames, encArgTypes) =
              unzip . map (A.pname &&& A.ptype) $ funParams
          argNames  = map (AsLval . argName) encArgNames
          argTypes  = map translate encArgTypes
          ctx       = Ctx.new (zip encArgNames argNames) ctable
          ((bodyName, bodyStat), _) = runState (translate funbody) ctx
          closures = map (\clos -> translateClosure clos [] ctable)
                         (reverse (Util.filter A.isClosure funbody))
          tasks = map (\tas -> translateTask tas ctable) $
                      reverse $ Util.filter A.isTask funbody
      in
        Concat $
        closures ++
        tasks ++
        [Function (translate funType)
                  funName
                  ((Ptr encoreCtxT, encoreCtxVar):(zip argTypes argNames))
                  (Seq $
                   [bodyStat, returnStmnt bodyName funType])]
    where
      returnStmnt var ty
          | isVoidType ty = Return unit
          | otherwise     = Return var
