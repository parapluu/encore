{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs #-}

{-| Make Type (see "AST") an instance of @Translatable@ (see
"CodeGen.Typeclasses"). -}

module CodeGen.Type where

import CodeGen.Typeclasses
import CodeGen.CCodeNames

import CCode.Main
import CCode.PrettyCCode ()

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
        | Ty.isTypeVar ty        = encore_arg_t
        | Ty.isFutureType ty     = future
        | Ty.isStreamType ty     = stream
        | Ty.isArrayType ty      = array
        | Ty.isRangeType ty      = range
        | Ty.isMaybeType ty      = option
        | otherwise = error $ "I don't know how to translate "++ show ty ++" to pony.c"

runtime_type :: Ty.Type -> CCode Expr
runtime_type ty
    | Ty.isActiveClassType ty  = AsExpr $ Var "ENCORE_ACTIVE"
    | Ty.isPassiveClassType ty = Amp $ runtime_type_name ty
    | Ty.isFutureType ty ||
      Ty.isStreamType ty = Amp future_type_rec_name
    | Ty.isArrowType ty  = Amp closure_type_rec_name
    | Ty.isArrayType ty  = Amp array_type_rec_name
    | Ty.isRangeType ty  = Amp range_type_rec_name
    | otherwise = AsExpr $ Var "ENCORE_PRIMITIVE"

encore_arg_t_tag :: CCode Ty -> CCode Name
encore_arg_t_tag (Ptr _)         = Nam "p"
encore_arg_t_tag (Typ "int64_t") = Nam "i"
encore_arg_t_tag (Typ "double")  = Nam "d"
encore_arg_t_tag other           =
    error $ "Type.hs: no encore_arg_t_tag for " ++ show other

as_encore_arg_t :: UsableAs e Expr => CCode Ty -> CCode e -> CCode Expr
as_encore_arg_t ty expr
    | is_encore_arg_t ty = EmbedC expr
    | otherwise = Cast encore_arg_t $ UnionInst (encore_arg_t_tag ty) expr

from_encore_arg_t :: CCode Ty -> CCode Expr -> CCode Lval
from_encore_arg_t ty expr
    | is_encore_arg_t ty = EmbedC expr
    | otherwise = expr `Dot` (encore_arg_t_tag ty)
