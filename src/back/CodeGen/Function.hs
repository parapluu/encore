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
      let funName = globalFunctionName funname
          encArgNames = map A.pname funparams
          encArgTypes = map A.ptype funparams
          argNames = map (AsLval . argName) encArgNames
          argTypes = map translate encArgTypes
          ctx = Ctx.new (zip encArgNames argNames) ctable
          ((bodyName, bodyStat), _) = runState (translate funbody) ctx
          closures = map (\clos -> translateClosure clos ctable) (reverse (Util.filter A.isClosure funbody))
          tasks = map (\tas -> translateTask tas ctable) $ reverse $ Util.filter A.isTask funbody
      in
        Concat $
        closures ++ tasks ++
        [Function (Typ "value_t") funName
                  [(Typ "value_t", Var "_args[]"), (Ptr void, Var "_env_not_used")]
                  (Seq $
                   extractArguments funparams ++
                   [bodyStat, returnStmnt bodyName funtype])]
    where
      returnStmnt var ty
          | isVoidType ty = Return $ (asEncoreArgT (translate ty) unit)
          | otherwise     = Return $ (asEncoreArgT (translate ty) var)

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname, A.ptype}):args) i =
          (Assign (Decl (ty, arg)) (getArgument i)) : (extractArguments' args (i+1))
          where
            ty = translate ptype
            arg = AsLval $ argName pname
            getArgument i
                | isEncoreArgT ty = ArrAcc i (Var "_args")
                | otherwise = fromEncoreArgT ty $ AsExpr $ ArrAcc i (Var "_args")
