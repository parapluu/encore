{-# LANGUAGE NamedFieldPuns #-}

module Types(Type, arrowType, isArrowType, futureType, isFutureType,
             parType, isParType, refType, isRefType, typeVar, isTypeVar,
             voidType, isVoidType, nullType, isNullType, 
             boolType, isBoolType, intType, isIntType, 
             realType, isRealType, stringType, isStringType, 
             isPrimitive, isNumeric, emptyType,
             getArgTypes, getResultType, getId,
             typeComponents, subtypeOf) where

import Data.List

import Identifiers

data Type = VoidType | StringType | IntType | BoolType | RealType
          | NullType | RefType {ident :: String} | TypeVar {ident :: String}
          | Arrow {argTypes :: [Type], resultType :: Type} 
          | FutureType {resultType :: Type} | ParType {resultType :: Type}
            deriving (Read, Eq)

typeComponents :: Type -> [Type]
typeComponents arrow@(Arrow argTys ty) = arrow:(concatMap typeComponents argTys ++ typeComponents ty)
typeComponents fut@(FutureType ty)     = fut:(typeComponents ty)
typeComponents par@(ParType ty)        = par:(typeComponents ty)
typeComponents ty                      = [ty]


getArgTypes = argTypes
getResultType = resultType
getId = ident

maybeParen :: Type -> String
maybeParen arr@(Arrow _ _) = "(" ++ show arr ++ ")"
maybeParen fut@(FutureType _) = "(" ++ show fut ++ ")"
maybeParen par@(ParType _) = "(" ++ show par ++ ")"
maybeParen ty = show ty

instance Show Type where
    show VoidType          = "void"
    show StringType        = "string"
    show IntType           = "int"
    show RealType          = "real"
    show BoolType          = "bool"
    show (RefType name)    = name
    show (TypeVar t)       = t
    show NullType          = "NullType"
    show (Arrow argTys ty) = "(" ++ (concat $ (intersperse ", " (map show argTys))) ++ ") -> " ++ show ty
    show (FutureType ty)   = "Fut " ++ maybeParen ty
    show (ParType ty)      = "Par " ++ maybeParen ty

arrowType = Arrow
isArrowType (Arrow {}) = True
isArrowType _ = False

futureType = FutureType 
isFutureType FutureType {} = True
isFutureType _ = False

parType = ParType
isParType ParType {} = True
isParType _ = False

refType = RefType
isRefType (RefType _) = True
isRefType _ = False

typeVar = TypeVar
isTypeVar (TypeVar _) = True
isTypeVar _ = False

-- | Used to give types to AST nodes during parsing (i.e. before
-- typechecking)
emptyType :: Type
emptyType = RefType "*** UN-TYPED ***"

voidType :: Type
voidType = VoidType

isVoidType :: Type -> Bool
isVoidType = (== voidType)

nullType :: Type
nullType = NullType

isNullType :: Type -> Bool
isNullType = (== nullType)

boolType :: Type
boolType = BoolType

isBoolType :: Type -> Bool
isBoolType = (== boolType)

intType :: Type
intType = IntType

isIntType :: Type -> Bool
isIntType = (== intType)

realType :: Type
realType = RealType

isRealType :: Type -> Bool
isRealType = (== realType)

stringType :: Type
stringType = StringType

isStringType :: Type -> Bool
isStringType = (== stringType)

primitives :: [Type]
primitives = [voidType, intType, realType, boolType, stringType]

isPrimitive :: Type -> Bool
isPrimitive = flip elem primitives

isNumeric :: Type -> Bool
isNumeric ty = isRealType ty || isIntType ty

subtypeOf :: Type -> Type -> Bool
subtypeOf ty1 ty2
    | isNullType ty2 = isNullType ty1 || isRefType ty1
    | otherwise      = ty1 == ty2
