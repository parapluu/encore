{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}

{-| Makes @Function@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Function where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
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

--import Control.Monad.Reader
import Control.Monad.State hiding(void)
import Data.Maybe
import Data.List

instance Translatable A.Function (ClassTable -> CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funtype, A.funname, A.funparams, A.funbody}) ctable =
      let fun_name = global_function_name funname
          ((bodyName, bodyStat), _) = runState (translate funbody) $ Ctx.empty ctable
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
            arg = Var $ show pname
            getArgument i
                | is_encore_arg_t ty = ArrAcc i (Var "_args")
                | otherwise = from_encore_arg_t ty $ AsExpr $ ArrAcc i (Var "_args")
