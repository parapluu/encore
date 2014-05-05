{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.MethodDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
import CodeGen.Type
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST as A

import Control.Monad.Reader
import Data.Maybe

instance Translatable A.MethodDecl (Reader Ctx.Context (CCode Toplevel)) where
  translate mdecl = do
    this_ty <- asks (A.cname . fromJust . Ctx.the_class)
    cdecl <- asks (fromJust . Ctx.the_class)
    tmbody <- local (Ctx.with_method mdecl) $ translate (A.mbody mdecl)
    return $ 
      (Function (translate (A.rtype mdecl)) (method_impl_name (A.cname cdecl) (A.mname mdecl))
       ((data_rec_ptr this_ty, Var "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
       (Statement (tmbody::CCode Expr)))
    where
      mparam_to_cvardecl (A.Param (na, ty)) = (translate ty, Var $ show na)
