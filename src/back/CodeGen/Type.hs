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
    translate (A.Type "void") = void
    translate (A.Type "Object") = Ptr void
    translate (A.Type "int") = int
    translate (A.Type "string") = Ptr char
    translate (A.Type other_ty) =
          if isLower $ head $ other_ty
          then error $
                   "don't know how to translate type `"++other_ty++"` to pony.c"
          else Ptr pony_actor_t
