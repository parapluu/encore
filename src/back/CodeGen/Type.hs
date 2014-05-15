{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Make Type (see "AST") an instance of @Translatable@ (see
"CodeGen.Typeclasses"). For instance, any object reference will be
translated to a void*, but primitives will remain unchanged -}

module CodeGen.Type where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx

import CCode.Main
import CCode.PrettyCCode
import Data.Char

import qualified Identifiers as ID

instance Translatable ID.Type (CCode Ty) where
    translate (ID.Type "void") = void
    translate (ID.Type "Object") = Ptr void
    translate (ID.Type "int") = int
    translate (ID.Type "string") = Ptr char
    translate (ID.Type other_ty) = 
        if isLower $ head $ other_ty
        then error $
                 "don't know how to translate type `"++other_ty++"` to pony.c"
        else Ptr pony_actor_t
                                                                       