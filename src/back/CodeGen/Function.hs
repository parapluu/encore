{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}

{-| Makes @Function@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Function where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
import CodeGen.Type
import CodeGen.Closure
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

instance Translatable A.Function (CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funtype, A.funname, A.funparams, A.funbody}) =
      let argTypes = map (\A.Param{A.ptype} -> ptype) funparams
          fun_name = global_function_name funname
          ((bodyName, bodyStat), _) = runState (translate funbody) Ctx.empty
          closures = map translateClosure (reverse (Util.filter A.isClosure funbody)) 
      in
        Concat $ 
        closures ++ 
        [Function (Typ "value_t") fun_name
                  [(Typ "value_t", Var "_args[]"), (Ptr void, Var "_env_not_used")]
                  (Seq $ 
                   extractArguments funparams ++ 
                   [bodyStat, returnStmnt bodyName funtype])]
    where
      returnStmnt var ty 
          | isVoidType ty = Return $ (arg_cast ty unit)
          | otherwise     = Return $ (arg_cast ty var)
          where 
            arg_cast ty var
                | isIntType  ty = Cast (pony_arg_t) (UnionInst (Nam "i") var)
                | isBoolType ty = Cast (pony_arg_t) (UnionInst (Nam "i") var)
                | isRealType ty = Cast (pony_arg_t) (UnionInst (Nam "d") var)
                | otherwise     = Cast (pony_arg_t) (UnionInst (Nam "p") var)

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