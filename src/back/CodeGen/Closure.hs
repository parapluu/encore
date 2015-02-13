{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}

{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure where

import CodeGen.Typeclasses
import CodeGen.Expr
import CodeGen.CCodeNames
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx
import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta

import Types as Ty
import Identifiers as Id

import Control.Monad.Reader hiding(void)
import Control.Monad.State hiding(void)
import Data.Maybe

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
               ((bodyName, bodyStat), _) = runState (translate body) $ Ctx.empty ctable
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
          | isVoidType ty = Return $ (arg_cast ty unit)
          | otherwise     = Return $ (arg_cast ty var)
          where 
            arg_cast ty var
                | isIntType  ty = Cast (encore_arg_t) (UnionInst (Nam "i") var)
                | isBoolType ty = Cast (encore_arg_t) (UnionInst (Nam "i") var)
                | isRealType ty = Cast (encore_arg_t) (UnionInst (Nam "d") var)
                | otherwise     = Cast (encore_arg_t) (UnionInst (Nam "p") var)

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname, A.ptype}):args) i = 
          (Assign (Decl (ty, arg)) (getArgument i)) : (extractArguments' args (i+1))
          where
            ty = translate ptype
            arg = Var $ show pname
            getArgument i = ArrAcc i (Var "_args") `Dot` arg_member
            arg_member
                | isIntType ptype  = Nam "i"
                | isRealType ptype = Nam "d"
                | otherwise        = Nam "p"

      buildEnvironment name members = 
          StructDecl (Typ $ show name) (map translate_binding members)
              where
                translate_binding (name, ty) = (translate ty, Var $ show name)

      extractEnvironment _ [] = []
      extractEnvironment envName ((name, ty):vars) = 
          (Assign (Decl (translate ty, Var $ show name)) (getVar name)) : extractEnvironment envName vars
              where
                getVar name = 
                    (Deref $ Cast (Ptr $ Struct envName) (Var "_env")) `Dot` (Nam $ show name)

      tracefun_decl traceName envName members =
          Function void traceName [(Ptr void, Var "p")]
                   (Seq $ map traceMember members)
              where
                traceMember (name, ty) 
                    | Ty.isActiveRefType ty = 
                        Call (Nam "pony_traceactor") [getVar name]
                    | Ty.isPassiveRefType ty = 
                        Call (Nam "pony_traceobject") 
                             [getVar name, AsLval $ class_trace_fn_name ty]
                    | otherwise = Comm $ "Not tracing member '" ++ show name ++ "'"
                getVar name = 
                    (Deref $ Cast (Ptr $ Struct envName) (Var "p")) `Dot` (Nam $ show name)
