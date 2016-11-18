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
      in (ID.Name $ show $ ty', AsLval ty')

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
           ctx = Ctx.new subst table

           ((bodyName, bodyStat), _) = runState (translate body) ctx
       in
         Concat [buildEnvironment envName freeVars fTypeVars,
                 tracefunDecl traceName envName freeVars,
                 Function (Static $ Typ "value_t") funName
                          [(Ptr (Ptr encoreCtxT), encoreCtxVar),
                           (Ptr (Ptr ponyTypeT), encoreRuntimeType),
                           (Typ "value_t", Var "_args[]"),
                           (Ptr void, Var "_env")]
                          (Seq $
                            extractArguments params ++
                            extractEnvironment envName freeVars fTypeVars ++
                            [bodyStat, returnStmnt bodyName resultType])]
  | otherwise =
        error
        "Tried to translate a closure from something that was not a closure"
    where
      returnStmnt var ty
          | isVoidType ty = Return $ asEncoreArgT (translate ty) unit
          | otherwise     = Return $ asEncoreArgT (translate ty) var

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname, A.ptype}):args) i =
          Assign (Decl (ty, arg)) (getArgument i) : extractArguments' args (i+1)
          where
            ty = translate ptype
            arg = AsLval $ argName pname
            getArgument i = fromEncoreArgT ty $ AsExpr $ ArrAcc i (Var "_args")

      buildEnvironment name vars typeVars =
        StructDecl (Typ $ show name) $
          (map translateBinding vars) ++ (map translateTypeVar typeVars)
          where
            translateBinding (name, ty) =
              (translate ty, AsLval $ fieldName name)
            translateTypeVar ty =
              (Ptr ponyTypeT, AsLval $ typeVarRefName ty)

      extractEnvironment envName vars typeVars =
        map assignVar vars ++ map assignTypeVar typeVars
        where
          assignVar (name, ty) =
            let fName = fieldName name
            in Assign (Decl (translate ty, AsLval fName)) $ getVar fName
          assignTypeVar ty =
            let fName = typeVarRefName ty
            in Assign (Decl (Ptr ponyTypeT, AsLval fName)) $ getVar fName
          getVar name =
              (Deref $ Cast (Ptr $ Struct envName) (Var "_env")) `Dot` name

      tracefunDecl traceName envName members =
        Function (Static void) traceName args body
        where
          args = [(Ptr encoreCtxT, ctxArg), (Ptr void, Var "p")]
          ctxArg = Var "_ctx_arg"
          body = Seq $
              Assign (Decl (Ptr (Ptr encoreCtxT), encoreCtxVar)) (Amp ctxArg) :
              Assign (Decl (Ptr $ Struct envName, Var "_this")) (Var "p") :
              map traceMember members
          traceMember (name, ty) = traceVariable ty $ getVar name
          getVar name =
              (Var "_this") `Arrow` fieldName name
