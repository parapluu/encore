module Types(Type, arrowType, isArrowType, futureType, isFutureType,
             parType, isParType, refType, isRefType, 
             voidType, isVoidType, nullType, isNullType, 
             boolType, isBoolType, intType, isIntType, 
             realType, isRealType, stringType, isStringType, 
             isPrimitive, isNumeric, emptyType) where

import Data.List

import Identifiers

data Type = VoidType | StringType | IntType | BoolType | RealType
          | NullType | RefType String
          | Arrow {argTypes :: [Type], resultType :: Type} 
          | Future {resultType :: Type} | Par {resultType :: Type}
            deriving (Read, Eq)

maybeParen :: Type -> String
maybeParen arr@(Arrow _ _) = "(" ++ show arr ++ ")"
maybeParen fut@(Future _) = "(" ++ show fut ++ ")"
maybeParen par@(Par _) = "(" ++ show par ++ ")"
maybeParen ty = show ty

instance Show Type where
    show VoidType          = "void"
    show StringType        = "string"
    show IntType           = "int"
    show RealType           = "real"
    show BoolType          = "bool"
    show (RefType name)    = name
    show NullType          = "NullType"
    show (Arrow argTys ty) = "(" ++ (concat $ (intersperse ", " (map show argTys))) ++ ") -> " ++ show ty
    show (Future ty)       = "Fut " ++ maybeParen ty
    show (Par ty)          = "Par " ++ maybeParen ty

arrowType = Arrow
isArrowType (Arrow {}) = True
isArrowType _ = False

futureType = Future 
isFutureType Future {} = True
isFutureType _ = False

parType = Par
isParType Par {} = True
isParType _ = False

refType = RefType
isRefType (RefType _) = True
isRefType _ = False

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