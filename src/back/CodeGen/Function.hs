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
import qualified Types as Ty

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.List

instance Translatable A.Function (CCode Toplevel) where
  -- | Translates a global function into the corresponding C-function
  translate fun@(A.Function {A.funtype, A.funname, A.funparams, A.funbody}) =
    let ((bodyn,bodys),_) = runState (translate funbody) Ctx.empty
        -- This reverse makes nested closures come before their enclosing closures. Not very nice...
        closures = map translateClosure (reverse (Util.filter A.isClosure funbody)) 
    in
      Concat $ closures ++ 
       [Function (translate funtype) (global_function_name funname) (map funparam_to_cvardecl funparams)
           (if not $ Ty.isVoidType funtype
            then (Seq $ bodys : [Return bodyn])
            else (Seq $ bodys : [Return unit]))]
    where
      funparam_to_cvardecl (A.Param {A.pname, A.ptype}) = (translate ptype, Var $ show pname)