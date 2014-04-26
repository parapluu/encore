{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.MethodDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST as A

import Control.Monad.Reader
import Data.Maybe

instance Translatable A.MethodDecl (Reader Ctx.Context CCode) where
  translate mdecl = do
    this_ty <- asks (A.cname . fromJust . Ctx.the_class)
    cdecl <- asks (fromJust . Ctx.the_class)
    tmbody <- local (Ctx.with_method mdecl) $ translate (A.mbody mdecl)
    return $ 
      (Function (data_rec_pointer (A.rtype mdecl)) (method_impl_name (A.cname cdecl) (A.mname mdecl))
       (CVarSpec (data_rec_pointer this_ty, "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
       [Statement tmbody])
    where
      mparam_to_cvardecl (A.Param (ty, na)) = CVarSpec (data_rec_pointer ty, show na)
