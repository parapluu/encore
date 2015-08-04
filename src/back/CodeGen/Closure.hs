{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure where

import CodeGen.Type
import CodeGen.Typeclasses
import CodeGen.Expr ()
import CodeGen.CCodeNames
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx
import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta

import Types as Ty

import Control.Monad.State hiding (void)

translateClosure :: A.Expr -> ClassTable -> CCode Toplevel
translateClosure closure ctable
    | A.isClosure closure =
           let arrowType  = A.getType closure
               resultType = Ty.getResultType arrowType
               argTypes   = Ty.getArgTypes arrowType
               params     = A.eparams closure
               body       = A.body closure
               id         = Meta.getMetaId . A.getMeta $ closure
               fun_name   = closure_fun_name id
               env_name   = closure_env_name id
               trace_name = closure_trace_name id
               freeVars   = Util.freeVariables (map A.pname params) body

               enc_env_names = map fst freeVars
               env_names     = map (AsLval . field_name) enc_env_names
               enc_arg_names = map A.pname params
               enc_arg_types = map A.ptype params
               arg_names = map arg_name enc_arg_names
               arg_types = map translate enc_arg_types
               subst     = (zip enc_env_names env_names) ++
                           (zip enc_arg_names arg_names)
               ctx = Ctx.new subst ctable

               ((bodyName, bodyStat), _) = runState (translate body) ctx
           in
             Concat [buildEnvironment env_name freeVars,
                     tracefun_decl trace_name env_name freeVars,
                     Function (Typ "value_t") fun_name
                              [(Typ "value_t", Var "_args[]"), (Ptr void, Var "_env")]
                              (Seq $
                                extractArguments params ++
                                extractEnvironment env_name freeVars ++
                                [bodyStat, returnStmnt bodyName resultType])]
    | otherwise = error "Tried to translate a closure from something that was not a closure"
    where
      returnStmnt var ty
          | isVoidType ty = Return $ (as_encore_arg_t (translate ty) unit)
          | otherwise     = Return $ (as_encore_arg_t (translate ty) var)

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname, A.ptype}):args) i =
          (Assign (Decl (ty, arg)) (getArgument i)) : (extractArguments' args (i+1))
          where
            ty = translate ptype
            arg = arg_name pname
            getArgument i = from_encore_arg_t ty $ AsExpr $ ArrAcc i (Var "_args")

      buildEnvironment name members =
          StructDecl (Typ $ show name) (map translate_binding members)
              where
                translate_binding (name, ty) = (translate ty, AsLval $ field_name name)

      extractEnvironment _ [] = []
      extractEnvironment envName ((name, ty):vars) =
          (Assign (Decl (translate ty, AsLval $ field_name name)) (getVar name)) : extractEnvironment envName vars
              where
                getVar name =
                    (Deref $ Cast (Ptr $ Struct envName) (Var "_env")) `Dot` field_name name

      tracefun_decl traceName envName members =
          Function void traceName [(Ptr void, Var "p")]
                   (Seq $ map traceMember members)
              where
                traceMember (name, ty)
                    | Ty.isActiveClassType ty =
                        Call (Nam "pony_traceactor") [getVar name]
                    | Ty.isPassiveClassType ty =
                        Call (Nam "pony_traceobject")
                             [getVar name, AsLval $ class_trace_fn_name ty]
                    | otherwise = Comm $ "Not tracing member '" ++ show name ++ "'"
                getVar name =
                    (Deref $ Cast (Ptr $ Struct envName) (Var "p")) `Dot` (Nam $ show name)
