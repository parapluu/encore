{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, FlexibleContexts #-}

module CodeGen.Task where

import CCode.Main
import CodeGen.Typeclasses
import CodeGen.Type
import CodeGen.Expr
import CodeGen.ClassTable
import CodeGen.CCodeNames
import Types as Ty

import qualified AST.AST as A
import qualified AST.Meta as Meta
import qualified AST.Util as Util
import qualified CodeGen.Context as Ctx


import Control.Monad.State hiding(void)
import Control.Monad.Reader hiding(void)
import Data.Maybe

translateTask :: A.Expr -> ClassTable -> CCode Toplevel
translateTask task ctable
  |  A.isTask task =
       let taskType = A.getType task
           body = A.body task
           resultType = Ty.getResultType taskType
           id = Meta.getMetaId . A.getMeta $ task
           fun_task_name = task_function_name id
           env_task_name = task_env_name id
           dependency_task_name = task_dependency_name id
           trace_task_name = task_trace_name id
           freeVars = Util.freeVariables [] body
           ((bodyName, bodyStat), _) = runState (translate body) $ Ctx.empty ctable
       in
        Concat [buildEnvironment env_task_name freeVars,
                buildDependency dependency_task_name,
                tracefun_decl trace_task_name env_task_name freeVars, -- TODO: Should we include dependencies?
                Function (Typ "encore_arg_t") fun_task_name
                         [(Ptr void, Var "_env"), (Ptr void, Var "_dep")]
                         (Seq $ extractEnvironment env_task_name freeVars ++
                                [bodyStat,
                                 returnStmnt bodyName resultType]
                         )]
  | otherwise = error "Tried to translate Task from something that wasn't a task"
  where
    returnStmnt var ty
     | isVoidType ty = Return $ (as_encore_arg_t (translate ty) unit)
     | otherwise = Return $ (as_encore_arg_t (translate ty) var)

    extractEnvironment _ [] = []
    extractEnvironment envName ((name, ty):freeVars) =
      let decl = Decl (translate ty, Var $ show name)
          rval = (Deref $ Cast (Ptr $ Struct envName) (Var "_env")) `Dot` (Nam $ show name)
      in Assign decl rval : extractEnvironment envName freeVars

    buildDependency name = StructDecl (Typ $ show name) []  -- TODO: extract dependencies
    buildEnvironment name members =
      StructDecl (Typ $ show name) (map translate_binding members)
        where
          translate_binding (name, ty) = (translate ty, Var $ show name)
    tracefun_decl traceName envName members =
      Function void traceName [(Ptr void, Var "p")]
      (Seq $ map traceMember members)
      where
        traceMember (name, ty)
          | Ty.isActiveRefType ty =
              Call (Nam "pony_traceactor") [Cast (Ptr pony_actor_t) (getVar name)]
          | Ty.isPassiveRefType ty =
              Call (Nam "pony_traceobject")
              [getVar name, AsLval $ class_trace_fn_name ty]
          | otherwise = Comm $ "Not tracing member '" ++ show name ++ "'"
        getVar name =
          (Deref $ Cast (Ptr $ Struct envName) (Var "p")) `Dot` (Nam $ show name)
