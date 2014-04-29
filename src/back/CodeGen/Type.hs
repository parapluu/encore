{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.Type where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx

import CCode.Main
import CCode.PrettyCCode
import Data.Char

import qualified AST as A

instance Translatable A.Type (CCode Ty) where
    translate (A.Type "Object") = Ptr . Typ $ "void"
    translate (A.Type "string") = Ptr char
    translate (A.Type other_ty) =
          if isLower $ head $ other_ty
          then Typ other_ty
          else data_rec_ptr (A.Type other_ty)

-- a pointer to a class' state
data_rec_ptr :: A.Type -> CCode Ty
data_rec_ptr = Ptr . data_rec_name
