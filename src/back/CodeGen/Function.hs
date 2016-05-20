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
  FunctionDecl typ name (Ptr encoreCtxT:initParam ++ params)
  where
    initParam = replicate (length (A.functionTParams f)) (Ptr ponyTypeT)
    params = map (translate . A.ptype) $ A.functionParams f
    typ = translate $ A.functionType f
    name = globalFunctionNameOf f

-- local function
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

functionTypeDecl :: A.Function -> CCode Toplevel
functionTypeDecl f@(A.Function {}) =
  let name = A.functionName f in
  Typedef (Struct $ functionTypeName name) (functionTypeName name)


globalFunctionWrapper :: A.Function -> CCode Toplevel
globalFunctionWrapper f =
  let
    args = A.functionParams f
    argList = extractArgs args

    -- TODO: get correct type
    varNames = replicate (length $ A.functionTParams f) (Var "ENCORE_PRIMITIVE")
    result = returnStmnt (Call globalFunctionName (encoreCtxVar : varNames ++ argList)) typ
  in
    Function
      (Typ "value_t")
      name
      [(Ptr encoreCtxT, encoreCtxVar), (Typ "value_t", Var "_args[]"), (Ptr void, Var "_env_not_used")]
      (Seq [result])
  where
    typ = A.functionType f
    name = globalFunctionWrapperNameOf f

    encoreAlloc name = (Call (Nam "encore_alloc") [Sizeof $ AsType name])

    globalFunctionName = globalFunctionNameOf f

    extractArgs :: [A.ParamDecl] -> [CCode Lval]
    extractArgs args = [getArg arg i | (arg, i) <- zip args [0..]]

    getArg :: A.ParamDecl -> Int -> CCode Lval
    getArg A.Param{A.ptype} i =
      fromEncoreArgT (translate ptype) $ AsExpr $ ArrAcc i $ Var "_args"

    returnStmnt :: UsableAs e Expr => CCode e -> Type -> CCode Stat
    returnStmnt var ty = Return $ asEncoreArgT (translate ty) var

instance Translatable A.Function (TableLookup -> CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funbody}) tableLookup =
      let funParams = A.functionParams fun
          funType   = A.functionType fun
          fName     = A.functionName fun
          funName   = globalFunctionName fName
          funInitStruct = functionTypeName fName
          (encArgNames, encArgTypes) =
              unzip . map (A.pname &&& A.ptype) $ funParams
          argNames  = map (AsLval . argName) encArgNames
          argTypes  = map translate encArgTypes
          ctx       = Ctx.new (zip encArgNames argNames) tableLookup Ctx.functionCtx
          ((bodyName, bodyStat), _) = runState (translate funbody) ctx
          closures = map (\clos -> translateClosure clos [] tableLookup Ctx.functionCtx)
                         (reverse (Util.filter A.isClosure funbody))
          tasks = map (\tas -> translateTask tas tableLookup Ctx.functionCtx) $
                      reverse $ Util.filter A.isTask funbody
          typeVariableVars = (\x y -> (x,y)) <$> [Ptr ponyTypeT] <*> (map (Var . getId) (A.functionTParams fun))
      in
        Concat $
        closures ++
        tasks ++
        [Function (translate funType)
                  funName
                  ((Ptr encoreCtxT, encoreCtxVar) : typeVariableVars ++ (zip argTypes argNames))
                  -- (map initRuntimeType (A.functionTParams fun))
                  (Seq $
                   [bodyStat, returnStmnt bodyName funType])]
    where
      initRuntimeType ty =
        Assign (Var "this" `Arrow` typeVarRefName ty)
               (Call (Nam "va_arg") [Var "params", Var "pony_type_t *"])
      returnStmnt var ty
          | isVoidType ty = Return unit
          | otherwise     = Return var
