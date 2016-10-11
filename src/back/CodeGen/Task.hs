module CodeGen.Task where

import CCode.Main
import CodeGen.Typeclasses
import CodeGen.Type
import CodeGen.Expr ()
import CodeGen.ClassTable
import CodeGen.CCodeNames
import Types as Ty

import qualified Identifiers as ID
import qualified AST.AST as A
import qualified AST.Meta as Meta
import qualified AST.Util as Util
import qualified CodeGen.Context as Ctx


import Control.Monad.State hiding(void)
import Control.Arrow(first)

translateTask :: A.Expr -> ProgramTable -> CCode Toplevel
translateTask task table
  |  A.isTask task =
       let taskType      = A.getType task
           body          = A.body task
           resultType    = Ty.getResultType taskType
           id            = Meta.getMetaId . A.getMeta $ task
           funTaskName   = taskFunctionName id
           envTaskName   = taskEnvName id
           dependencyTaskName = taskDependencyName id
           traceTaskName = taskTraceName id
           freeVars      = map (first ID.qnlocal) $
                           filter (ID.isLocalQName . fst) $
                           Util.freeVariables [] body
           encEnvNames   = map fst freeVars
           envNames      = map (AsLval . fieldName) encEnvNames
           subst         = zip encEnvNames envNames
           ctx           = Ctx.new subst table
           ((bodyName, bodyStat), _) = runState (translate body) ctx
       in
        Concat [buildEnvironment envTaskName freeVars,
                buildDependency dependencyTaskName,
                tracefunDecl traceTaskName envTaskName freeVars, -- TODO: Should we include dependencies?
                Function (Typ "encore_arg_t") funTaskName
                         [(Ptr encoreCtxT, encoreCtxVar),
                          (Ptr void, Var "_env"), (Ptr void, Var "_dep")]
                         (Seq $ extractEnvironment envTaskName freeVars ++
                                [bodyStat,
                                 returnStmnt bodyName resultType]
                         )]
  | otherwise = error "Tried to translate Task from something that wasn't a task"
  where
    returnStmnt var ty
     | isVoidType ty = Return $ asEncoreArgT (translate ty) unit
     | otherwise = Return $ asEncoreArgT (translate ty) var

    extractEnvironment _ [] = []
    extractEnvironment envName ((name, ty):freeVars) =
      let decl = Decl (translate ty, AsLval $ fieldName name)
          rval = (Deref $ Cast (Ptr $ Struct envName) (Var "_env"))
                 `Dot` fieldName name
      in Assign decl rval : extractEnvironment envName freeVars

    buildDependency name = StructDecl (Typ $ show name) []  -- TODO: extract dependencies
    buildEnvironment name members =
      StructDecl (Typ $ show name) (map translateBinding members)
        where
          translateBinding (name, ty) = (translate ty, AsLval $ fieldName name)
    tracefunDecl traceName envName members =
      Function void traceName [(Ptr encoreCtxT, encoreCtxVar), (Ptr void, Var "p")]
      (Seq $ map traceMember members)
      where
        traceMember (name, ty)
          | Ty.isActiveClassType ty =
              Call ponyTraceActor [AsExpr encoreCtxVar, Cast (Ptr ponyActorT) (getVar name)]
          | Ty.isPassiveClassType ty =
              Call ponyTraceObject
              [encoreCtxVar, getVar name, AsLval $ classTraceFnName ty]
          | otherwise = Comm $ "Not tracing member '" ++ show name ++ "'"
        getVar name =
          (Deref $ Cast (Ptr $ Struct envName) (Var "p")) `Dot` fieldName name
