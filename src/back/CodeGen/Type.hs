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
    | Ty.isVoidType ty = Ptr void
    | Ty.isIntType ty = int
    | Ty.isRealType ty = double
    | Ty.isBoolType ty = bool
    | Ty.isStringType ty = Ptr char
    | otherwise = error $ show ty ++ " is not a primitive"

instance Translatable Ty.Type (CCode Ty) where
    translate ty
        | Ty.isPrimitive ty = translatePrimitive ty
        | Ty.isActiveRefType ty = Ptr pony_actor_t 
        | Ty.isPassiveRefType ty = Ptr $ Typ (show (data_rec_name ty))
        | Ty.isRefType ty = error $ "Unknown activity of class '" ++ show ty ++ "'"
        | Ty.isArrowType ty = closure
        | Ty.isTypeVar ty = Ptr void
        | Ty.isFutureType ty = future
        | otherwise = error $ "I don't know how to translate "++ show ty ++" to pony.c"