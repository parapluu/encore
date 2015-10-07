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
        | Ty.isRefType ty        = Ptr . AsType $ classTypeName ty
        | Ty.isArrowType ty      = closure
        | Ty.isTypeVar ty        = encoreArgT
        | Ty.isFutureType ty     = future
        | Ty.isStreamType ty     = stream
        | Ty.isArrayType ty      = array
        | Ty.isRangeType ty      = range
        | Ty.isMaybeType ty      = option
        | Ty.isCType ty          = Embed $ Ty.getId ty
        | Ty.isParType ty        = par
        | otherwise = error $ "I don't know how to translate "++ show ty ++" to pony.c"

runtimeType :: Ty.Type -> CCode Expr
runtimeType ty
    | Ty.isActiveClassType ty  = AsExpr $ Var "ENCORE_ACTIVE"
    | Ty.isPassiveClassType ty = Amp $ runtimeTypeName ty
    | Ty.isFutureType ty ||
      Ty.isStreamType ty = Amp futureTypeRecName
    | Ty.isArrowType ty  = Amp closureTypeRecName
    | Ty.isArrayType ty  = Amp arrayTypeRecName
    | Ty.isRangeType ty  = Amp rangeTypeRecName
    | Ty.isParType ty    = Amp partyTypeRecName
    | otherwise = AsExpr $ Var "ENCORE_PRIMITIVE"

encoreArgTTag :: CCode Ty -> CCode Name
encoreArgTTag (Ptr _)         = Nam "p"
encoreArgTTag (Typ "int64_t") = Nam "i"
encoreArgTTag (Typ "double")  = Nam "d"
encoreArgTTag (Embed _)       = Nam "p"
encoreArgTTag other           =
    error $ "Type.hs: no encoreArgTTag for " ++ show other

asEncoreArgT :: UsableAs e Expr => CCode Ty -> CCode e -> CCode Expr
asEncoreArgT ty expr
   | isEncoreArgT ty = EmbedC expr
   | otherwise = Cast encoreArgT $ UnionInst (encoreArgTTag ty) expr

fromEncoreArgT :: CCode Ty -> CCode Expr -> CCode Lval
fromEncoreArgT ty expr
    | isEncoreArgT ty = EmbedC expr
    | otherwise = expr `Dot` (encoreArgTTag ty)
