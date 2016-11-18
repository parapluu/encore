{-# LANGUAGE MultiParamTypeClasses,  FlexibleInstances #-}

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
import qualified Identifiers as ID
import Types

import Control.Monad.State hiding(void)
import Control.Arrow((&&&))

globalFunction :: A.Function -> (Maybe (CCode Stat) -> CCode Toplevel)
globalFunction fun = createHeader
  where
    createHeader (Just body) =
      Function (translate funType) funName
                   ((Ptr (Ptr encoreCtxT), encoreCtxVar):
                   encoreRuntimeTypeParam :
                   (zip argTypes argNames)) body

    createHeader Nothing  =
      FunctionDecl (translate funType) funName
                  (Ptr (Ptr encoreCtxT): encoreRuntimeTypeT :
                   argTypes)

    funParams = A.functionParams fun
    funType   = A.functionType fun

    encoreRuntimeTypeParam = (Ptr (Ptr ponyTypeT), encoreRuntimeType)
    encoreRuntimeTypeT = fst encoreRuntimeTypeParam

    funName   = globalFunctionNameOf fun
    (encArgNames, encArgTypes) =
              unzip . map (A.pname &&& A.ptype) $ funParams
    argNames  = map (AsLval . argName) encArgNames
    argTypes  = map translate encArgTypes


globalFunctionDecl :: A.Function -> CCode Toplevel
globalFunctionDecl f = globalFunction f Nothing

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

globalFunctionWrapperDecl :: A.Function -> CCode Toplevel
globalFunctionWrapperDecl f =
  FunctionDecl (Typ "value_t") name [Ptr (Ptr encoreCtxT), Ptr (Ptr ponyTypeT),
                                     Ptr $ Typ "value_t", Ptr void]
  where
    name = globalFunctionWrapperNameOf f

-- TODO: different header from shared!
globalFunctionWrapper :: A.Function -> CCode Toplevel
globalFunctionWrapper f =
  let argList = encoreCtxVar : encoreRuntimeType : extractArgs
  in
    Function
      (Typ "value_t")
      name
      [(Ptr (Ptr encoreCtxT), encoreCtxVar),
       (Ptr (Ptr ponyTypeT), encoreRuntimeType),
       (Typ "value_t", Var "_args[]"),
       (Ptr void, Var "_env_not_used")]
      $ returnStmnt (Call (globalFunctionNameOf f) argList) typ
  where
    typ = A.functionType f
    name = globalFunctionWrapperNameOf f

    extractArgs :: [CCode Lval]
    extractArgs =
      let typeArgs = map A.ptype (A.functionParams f)
      in [getArg arg i | (arg, i) <- zip typeArgs [0..]]

    getArg :: Type -> Int -> CCode Lval
    getArg ty i = fromEncoreArgT (translate ty) $ AsExpr $ ArrAcc i $ Var "_args"

    returnStmnt :: UsableAs e Expr => CCode e -> Type -> CCode Stat
    returnStmnt var ty = Return $ asEncoreArgT (translate ty) var

instance Translatable A.Function (ProgramTable -> CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funbody}) table =
      let funParams = A.functionParams fun
          funTypeParams = A.functionTypeParams fun
          funType   = A.functionType fun
          encArgNames = map A.pname funParams
          argNames  = map (AsLval . argName) encArgNames
          paramTypesDecl = curry Decl
                           <$> [Ptr ponyTypeT]
                           <*> map (AsLval . typeVarRefName) funTypeParams
          assignRuntimeFn p i = Assign p (ArrAcc i encoreRuntimeType)
          runtimeTypeAssignments = zipWith assignRuntimeFn paramTypesDecl [0..]
          typeParamSubst = map (\t -> (ID.Name $ getId t, AsLval $ typeVarRefName t)) funTypeParams
          ctx       = Ctx.new (zip encArgNames argNames ++ typeParamSubst) table
          ((bodyName, bodyStat), _) = runState (translate funbody) ctx
          closures = map (\clos -> translateClosure clos funTypeParams table)
                         (reverse (Util.filter A.isClosure funbody))
          tasks = map (\tas -> translateTask tas table) $
                      reverse $ Util.filter A.isTask funbody
          bodyResult = (Seq $ runtimeTypeAssignments ++
                             [bodyStat, returnStatement funType bodyName])
      in
        Concat $ closures ++ tasks ++ [globalFunction fun (Just bodyResult)]

returnStatement ty var
    | isVoidType ty = Return $ AsExpr unit
    | otherwise     = Return $ Cast (translate ty) var
