{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure (
  translateClosure,
  varSubFromTypeVars,
) where

import CodeGen.Type
import CodeGen.Typeclasses
import CodeGen.Expr ()
import CodeGen.Trace (traceVariable)
import CodeGen.CCodeNames
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx
import CodeGen.DTrace
import CCode.Main
import qualified Identifiers as ID

import Data.List (intersect)

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta

import Types as Ty

import Control.Monad.State hiding (void)
import Control.Arrow(first)
import Debug.Trace

varSubFromTypeVars :: [Type] -> [(ID.Name, CCode Lval)]
varSubFromTypeVars = map each
  where
    each ty =
      let ty' = typeVarRefName ty
      in (ID.Name $ Ty.getId ty, AsLval ty')

translateClosure :: A.Expr -> [Type] -> ProgramTable -> CCode Toplevel
translateClosure closure typeVars table
    | A.isClosure closure =
       let arrowType   = A.getType closure
           resultType  = Ty.getResultType arrowType
           argTypes    = Ty.getArgTypes arrowType
           params      = A.eparams closure
           body        = A.body closure
           id          = Meta.getMetaId . A.getMeta $ closure
           funName     = closureFunName id
           nameForwarding = forwardingClosureImplName $ (ID.Name . show) funName
           envName     = closureEnvName id
           traceName   = closureTraceName id
           boundVars   = map (ID.qName . show . A.pname) params
           freeVars    = map (first ID.qnlocal) $
                         filter (ID.isLocalQName . fst) $
                         Util.freeVariables boundVars body
           fTypeVars   = typeVars `intersect` Util.freeTypeVars body
           encEnvNames = map fst freeVars
           envNames    = map (AsLval . fieldName) encEnvNames
           encArgNames = map A.pname params
           argNames    = map (AsLval . argName) encArgNames
           subst       = zip encEnvNames envNames ++
                         zip encArgNames argNames ++
                         varSubFromTypeVars fTypeVars
           ctx = Ctx.setClsCtx (Ctx.new subst table) closure
           ((bodyName, bodyStat), _) = runState (translate body) ctx
           forwardingCtx = Ctx.setClsCtx(Ctx.newWithForwarding subst table) closure
           ((forwardingBodyName,forwardingBody),_) =
             runState (translate body) forwardingCtx
           normalClosureImpl =
             Function (Static $ Typ "value_t") funName
                      [(Ptr (Ptr encoreCtxT), encoreCtxVar),
                       (Ptr (Ptr ponyTypeT), encoreRuntimeType),
                       (Typ "value_t", Var "_args[]"),
                       (Ptr void, envVar)]
                      (Seq $
                        dtraceClosureEntry argNames :
                        extractArguments params ++
                        extractEnvironment envName freeVars fTypeVars ++
                        [bodyStat
                        ,dtraceClosureExit
                        ,returnStmnt bodyName resultType]
                      )
           forwardingClosureImpl =
             Function (Static $ Typ "value_t") funName
                      [(Ptr (Ptr encoreCtxT), encoreCtxVar),
                       (Ptr (Ptr ponyTypeT), encoreRuntimeType),
                       (Typ "value_t", Var "_args[]"),
                       (Ptr void, envVar)]
                      (Seq $
                        dtraceClosureEntry argNames :
                        extractArguments params ++
                        extractEnvironmentForward envName freeVars fTypeVars ++
                        [forwardingBody
                        ,dtraceClosureExit
                        ,returnStmnt forwardingBodyName unitType])
       in
         Concat $  [buildEnvironmentForward envName freeVars fTypeVars] ++
                   if null $ Util.filter A.isForward body
                   then [tracefunDecl traceName envName freeVars fTypeVars extractEnvironment,
                         normalClosureImpl]
                   else [tracefunDecl traceName envName freeVars fTypeVars extractEnvironmentForward,
                         forwardingClosureImpl]
  | otherwise =
        error
        "Tried to translate a closure from something that was not a closure"
    where
      returnStmnt var ty
          | isUnitType ty = Return $ asEncoreArgT (translate ty) unit
          | otherwise     = Return $ asEncoreArgT (translate ty) var

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname, A.ptype}):args) i =
          Assign (Decl (ty, arg)) (getArgument i) : extractArguments' args (i+1)
          where
            ty = translate ptype
            arg = AsLval $ argName pname
            getArgument i = fromEncoreArgT ty $ AsExpr $ ArrAcc i (Var "_args")

      buildEnvironmentForward name vars typeVars =
        StructDecl (Typ $ show name) $
          (map translateBinding vars) ++ (map translateTypeVar typeVars) ++ [(future, futVar)]
          where
            translateBinding (name, ty) =
              (translate ty, AsLval $ fieldName name)
            translateTypeVar ty =
              (Ptr ponyTypeT, AsLval $ typeVarRefName ty)

      buildEnvironment name vars typeVars =
        StructDecl (Typ $ show name) $
          (map translateBinding vars) ++ (map translateTypeVar typeVars)
          where
            translateBinding (name, ty) =
              (translate ty, AsLval $ fieldName name)
            translateTypeVar ty =
              (Ptr ponyTypeT, AsLval $ typeVarRefName ty)

      extractEnvironmentForward envName vars typeVars =
        (extractEnvironment envName vars typeVars) ++ [(assignFut $ ID.Name "_fut")]
        where
          assignFut name =
            let fName = Nam $ show name
            in Assign (Decl (future, AsLval fName)) $ getVar fName
          getVar name =
              (Deref $ Cast (Ptr $ Struct envName) envVar) `Dot` name

      extractEnvironment envName vars typeVars =
        map assignVar vars ++ map assignTypeVar typeVars
        where
          assignVar (name, ty) =
            let fName = fieldName name
            in Assign (Decl (translate ty, AsLval fName)) $ getVar fName
          assignTypeVar ty =
            let fName = typeVarRefName ty
            in Seq [Assign (Decl (Ptr ponyTypeT, AsLval fName)) $ getVar fName,
                    encoreAssert (AsExpr $ AsLval fName)]
          getVar name =
              (Deref $ Cast (Ptr $ Struct envName) envVar) `Dot` name

      tracefunDecl traceName envName members fTypeVars extractEnv =
        Function (Static void) traceName args body
        where
          args = [(Ptr encoreCtxT, ctxArg), (Ptr void, Var "p")]
          ctxArg = Var "_ctx_arg"
          body = Seq $
              Assign (Decl (Ptr (Ptr encoreCtxT), encoreCtxVar)) (Amp ctxArg) :
              Assign (Decl (Ptr $ Struct envName, envVar)) (Var "p") :
              extractEnv envName members fTypeVars ++
              map traceMember members
          traceMember (name, ty) = traceVariable ty $ getVar name
          getVar name = envVar `Arrow` fieldName name
