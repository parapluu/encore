{-# LANGUAGE MultiParamTypeClasses,  FlexibleInstances #-}

{-| Makes @Function@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Function where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr ()
import CodeGen.Type
import CodeGen.Closure
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx
import CodeGen.DTrace

import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified Identifiers as ID
import Types

import Control.Monad.State hiding(void)
import Control.Arrow((&&&))

createFunction :: A.Function -> (A.Function -> CCode Name) -> Maybe (CCode Stat)
               -> CCode Toplevel
createFunction fun nameOf = createHeader
  where
    createHeader (Just body) =
      Function (translate funType) funName
                   ((Ptr (Ptr encoreCtxT), encoreCtxVar):
                   encoreRuntimeTypeParam :
                   zip argTypes argNames) body

    createHeader Nothing  =
      FunctionDecl (translate funType) funName
                  (Ptr (Ptr encoreCtxT): encoreRuntimeTypeT :
                   argTypes)

    funParams = A.functionParams fun
    funType   = A.functionType fun

    encoreRuntimeTypeParam = (Ptr (Ptr ponyTypeT), encoreRuntimeType)
    encoreRuntimeTypeT = fst encoreRuntimeTypeParam

    funName   = nameOf fun
    (encArgNames, encArgTypes) =
              unzip . map (A.pname &&& A.ptype) $ funParams
    argNames  = map (AsLval . argName) encArgNames
    argTypes  = map translate encArgTypes

globalFunction :: A.Function -> CCode Stat -> CCode Toplevel
globalFunction fun body = createFunction fun globalFunctionNameOf (Just body)

localFunction :: A.Function -> CCode Stat -> CCode Toplevel
localFunction fun body = createFunction fun localFunctionNameOf (Just body)

globalFunctionDecl :: A.Function -> CCode Toplevel
globalFunctionDecl f = createFunction f globalFunctionNameOf Nothing

localFunctionDecl :: A.Function -> CCode Toplevel
localFunctionDecl f = createFunction f localFunctionNameOf Nothing

functionClosureDecl :: (A.Function -> CCode Name) -> A.Function
                    -> CCode Toplevel
functionClosureDecl nameOf f = DeclTL (closure, AsLval $ nameOf f)

globalFunctionClosureDecl :: A.Function -> CCode Toplevel
globalFunctionClosureDecl = functionClosureDecl functionClosureNameOf

localFunctionClosureDecl :: A.Function -> CCode Toplevel
localFunctionClosureDecl = functionClosureDecl functionClosureNameOf

initFunctionClosure :: A.Function -> CCode Toplevel
initFunctionClosure f =
  AssignTL var value
  where
    var = Decl (Ptr struct, AsLval $ functionClosureNameOf f)
    value = CompoundLiteral struct [(closureFun, address)]
    struct = Struct closureStructName
    closureFun = AsLval closureStructFFieldName
    address = Cast (Ptr void) $ Amp $ functionWrapperNameOf f

functionWrapperDecl :: A.Function -> CCode Toplevel
functionWrapperDecl f =
  FunctionDecl (Typ "value_t") (functionWrapperNameOf f)
               [Ptr (Ptr encoreCtxT), Ptr (Ptr ponyTypeT),
                Ptr $ Typ "value_t", Ptr void]

-- TODO: different header from shared!
functionWrapper ::
  (A.Function -> CCode Name) -> (A.Function -> CCode Name) -> A.Function
  -> CCode Toplevel
functionWrapper nameOf wrapperNameOf f =
  let argList = encoreCtxVar : encoreRuntimeType : extractArgs
  in
    Function
      (Typ "value_t")
      name
      [(Ptr (Ptr encoreCtxT), encoreCtxVar),
       (Ptr (Ptr ponyTypeT), encoreRuntimeType),
       (Typ "value_t", Var "_args[]"),
       (Ptr void, Var "_env_not_used")]
      $ returnStmnt (Call (nameOf f) argList) typ
  where
    typ = A.functionType f
    name = wrapperNameOf f

    extractArgs :: [CCode Lval]
    extractArgs =
      let typeArgs = map A.ptype (A.functionParams f)
      in [getArg arg i | (arg, i) <- zip typeArgs [0..]]

    getArg :: Type -> Int -> CCode Lval
    getArg ty i = fromEncoreArgT (translate ty) $ AsExpr $ ArrAcc i $ Var "_args"

    returnStmnt :: UsableAs e Expr => CCode e -> Type -> CCode Stat
    returnStmnt var ty = Return $ asEncoreArgT (translate ty) var

globalFunctionWrapper :: A.Function -> CCode Toplevel
globalFunctionWrapper =
  functionWrapper globalFunctionNameOf functionWrapperNameOf

localFunctionWrapper :: A.Function -> CCode Toplevel
localFunctionWrapper =
  functionWrapper localFunctionNameOf functionWrapperNameOf

instance Translatable A.Function
                     (ProgramTable ->
                      (A.Function -> CCode Stat -> CCode Toplevel)
                     -> CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funbody, A.funlocals}) table create =
      let names     = map (ID.qLocal . A.functionName) funlocals
          localized = map (localize (A.functionName fun)) funlocals
          headers   = map A.funheader localized
          cnames    = map localFunctionNameOf localized
          newTable  = withLocalFunctions names headers cnames table
          locals    = translateLocalFunctions newTable localized
          funParams = A.functionParams fun
          funTypeParams = A.functionTypeParams fun
          funType   = A.functionType fun
          encArgNames = map A.pname funParams
          argNames  = map (AsLval . argName) encArgNames
          argSubst  = zip encArgNames argNames
          paramTypesDecl = curry Decl
                           <$> [Ptr ponyTypeT]
                           <*> map (AsLval . typeVarRefName) funTypeParams
          assignRuntimeFn p i = Assign p (ArrAcc i encoreRuntimeType)
          runtimeTypeAssignments = zipWith assignRuntimeFn paramTypesDecl [0..]
          typeParamSubst = map (\t -> (ID.Name $ getId t, AsLval $ typeVarRefName t)) funTypeParams
          ctx      = Ctx.setExecCtx (Ctx.new (argSubst ++ typeParamSubst) newTable) Ctx.FunctionContext{Ctx.fname=fun}
          ((bodyName, bodyStat), _) = runState (translate funbody) ctx
          closures = map (\clos -> translateClosure clos funTypeParams newTable)
                         (reverse (Util.filter A.isClosure funbody))
          bodyResult = (Seq $ dtraceFunctionEntry (A.functionName fun) argNames :
                              runtimeTypeAssignments ++
                              [bodyStat
                              ,dtraceFunctionExit (A.functionName fun)
                              ,returnStatement funType bodyName
                              ])
      in Concat $ locals ++ closures ++ [create fun bodyResult]
      where
        localize prefix fun =
          let oldName = A.functionName fun
              newName = ID.Name $ show prefix ++ "_" ++ show oldName
          in A.setFunctionName newName fun

translateLocalFunctions :: ProgramTable -> [A.Function] -> [CCode Toplevel]
translateLocalFunctions table funs =
  let localDecls = map localFunctionDecl funs
      localClosureDecls = map localFunctionClosureDecl funs
      localWrapperDecls = map functionWrapperDecl funs
      localClosureInits = map initFunctionClosure funs
      localFunctionWrappers = map localFunctionWrapper funs
      translated = map (\fun -> translate fun table localFunction) funs
  in localDecls ++
     localClosureDecls ++
     localWrapperDecls ++
     translated ++
     localClosureInits ++
     localFunctionWrappers

returnStatement ty var
    | isVoidType ty = Return $ AsExpr unit
    | otherwise     = Return $ Cast (translate ty) var
