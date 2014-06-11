{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}

{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure where

import CodeGen.Typeclasses
import CodeGen.Expr
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx
import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util

import Types as Ty
import Identifiers as Id

import Control.Monad.Reader hiding(void)
import Control.Monad.State hiding(void)
import Data.Maybe

translateClosure :: A.Expr -> (Reader Ctx.Context (CCode Toplevel))
translateClosure closure 
    | A.isClosure closure = 
        do ctx <- ask
           let arrowType = A.getType closure
               resultType = Ty.getResultType arrowType
               argTypes = Ty.getArgTypes arrowType
               params = A.eparams closure
               body = A.body closure
               id = A.getMetaId closure
               freeVars = Util.freeVariables (map A.pname params) body
           let ((bodyName, bodyStat), _) = runState (translate body) ctx
           return $ ConcatTL 
                      [buildEnvironment (closure_env_name id) freeVars,
                       Function (voidOrValue resultType) (closure_fun_name id)
                         [(Typ "value", Var "_args[]"), (Ptr void, Var "_env")]
                         (Seq $ 
                            extractArguments params ++ 
                            extractEnvironment (closure_env_name id) freeVars ++
                            [bodyStat, returnStmnt bodyName resultType])]
    | otherwise = error "Tried to translate a closure from something that was not a closure"
    where
      voidOrValue ty
          | isVoidType ty = void
          | otherwise     = Typ "value"
      returnStmnt var ty 
          | isVoidType ty = Embed ""
          | otherwise  = Embed $ "return " ++ (toValFun ty) ++ "(" ++ (show var) ++ ")"
          where 
            toValFun ty
                | isIntType  ty = "int_to_val"
                | isRealType ty = "dbl_to_val"
                | otherwise     = "ptr_to_val"

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname = pname, A.ptype = ptype}):args) i = 
          (Assign (Decl (ty, arg)) (getArgument i)) : (extractArguments' args (i+1))
          where
            ty = translate ptype
            arg = Var $ show pname
            getArgument i = Call getValFun [ArrAcc i (Var "_args")]
            getValFun
                | isIntType ptype  = Nam "val_to_int"
                | isRealType ptype = Nam "val_to_dbl"
                | otherwise        = Nam "val_to_ptr"

      buildEnvironment name members = 
          StructDecl (Typ $ show name) (map translate_binding members)
              where
                translate_binding (name, ty) = (translate ty, Var $ show name)

      extractEnvironment _ [] = []
      extractEnvironment envName ((name, ty):vars) = 
          (Assign (Decl (translate ty, Var $ show name)) (getVar name)) : extractEnvironment envName vars
              where
                getVar name = 
                    (Deref $ Var $ "((struct ___" ++ (show envName) ++ "*) " ++ "_env)") `Dot` (Nam $ show name)