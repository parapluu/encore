{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Make Type (see "AST") an instance of @Translatable@ (see
"CodeGen.Typeclasses"). -}

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
    | Ty.isVoidType ty   = Ptr void
    | Ty.isIntType ty    = int
    | Ty.isRealType ty   = double
    | Ty.isBoolType ty   = bool
    | Ty.isStringType ty = Ptr char
    | otherwise = error $ show ty ++ " is not a primitive"

instance Translatable Ty.Type (CCode Ty) where
    translate ty
        | Ty.isPrimitive ty      = translatePrimitive ty
        | Ty.isRefType ty        = Ptr . AsType $ class_type_name ty
        | Ty.isArrowType ty      = closure
        | Ty.isTypeVar ty        = Ptr void
        | Ty.isFutureType ty     = future
        | Ty.isStreamType ty     = stream
        | otherwise = error $ "I don't know how to translate "++ show ty ++" to pony.c"

runtime_type :: Ty.Type -> CCode Expr
runtime_type ty 
    | Ty.isActiveRefType ty  = AsExpr $ Var "PONY_ACTOR"
    | Ty.isPassiveRefType ty = Amp $ type_rec_name ty
    | Ty.isFutureType ty ||
      Ty.isStreamType ty = Amp $ future_type_rec_name
    | Ty.isArrowType ty = Amp $ closure_type_rec_name
    | otherwise = AsExpr $ Var "PONY_NONE"