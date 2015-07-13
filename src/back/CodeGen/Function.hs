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

instance Translatable A.Function (ClassTable -> CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funtype, A.funname, A.funparams, A.funbody}) ctable =
      let fun_name = global_function_name funname
          enc_arg_names = map A.pname funparams
          enc_arg_types = map A.ptype funparams
          arg_names = map arg_name enc_arg_names
          arg_types = map translate enc_arg_types
          ctx = Ctx.new (zip enc_arg_names arg_names) ctable
          ((bodyName, bodyStat), _) = runState (translate funbody) ctx
          closures = map (\clos -> translateClosure clos ctable) (reverse (Util.filter A.isClosure funbody))
          tasks = map (\tas -> translateTask tas ctable) $ reverse $ Util.filter A.isTask funbody
      in
        Concat $
        closures ++ tasks ++
        [Function (Typ "value_t") fun_name
                  [(Typ "value_t", Var "_args[]"), (Ptr void, Var "_env_not_used")]
                  (Seq $
                   extractArguments funparams ++
                   [bodyStat, returnStmnt bodyName funtype])]
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
            getArgument i
                | is_encore_arg_t ty = ArrAcc i (Var "_args")
                | otherwise = from_encore_arg_t ty $ AsExpr $ ArrAcc i (Var "_args")
