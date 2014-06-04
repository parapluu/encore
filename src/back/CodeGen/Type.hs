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
import qualified Types as Ty

translatePrimitive :: Ty.Type -> CCode Ty
translatePrimitive ty
    | Ty.isVoidType ty = void
    | Ty.isIntType ty = int
    | Ty.isRealType ty = double
    | Ty.isBoolType ty = bool
    | Ty.isStringType ty = Ptr char
    | otherwise = error $ show ty ++ " is not a primitive"

instance Translatable Ty.Type (CCode Ty) where
    translate ty
        | Ty.isPrimitive ty = translatePrimitive ty
        | Ty.isRefType ty = if Ty.isActiveRefType ty then Ptr pony_actor_t else Ptr void
        | otherwise = error $ "I don't know how to translate "++ show ty ++" to pony.c"