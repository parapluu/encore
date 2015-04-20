{-# LANGUAGE NamedFieldPuns #-}

module Types(Type, arrowType, isArrowType, futureType, isFutureType,
             parType, isParType, streamType, isStreamType, arrayType, isArrayType,
             refTypeWithParams, passiveRefTypeWithParams, activeRefTypeWithParams,
             refType, isRefType, passiveRefType, activeRefType, 
             isActiveRefType, isPassiveRefType, isMainType,
             makeActive, makePassive, typeVar, isTypeVar, replaceTypeVars,
             voidType, isVoidType, nullType, isNullType, 
             boolType, isBoolType, intType, isIntType, 
             realType, isRealType, stringType, isStringType, 
             isPrimitive, isNumeric, emptyType,
             getArgTypes, getResultType, getId, getTypeParameters, setTypeParameters,
             typeComponents, subtypeOf, typeMap) where

import Data.List

import Identifiers

data Activity = Active | Passive | Unknown deriving(Eq, Show)

data RefTypeInfo = RefTypeInfo {refId :: String, activity :: Activity, parameters :: [Type]}

instance Eq RefTypeInfo where
    RefTypeInfo {refId = id1} == RefTypeInfo {refId = id2} = id1 == id2

data Type = VoidType | StringType | IntType | BoolType | RealType
          | NullType | RefType RefTypeInfo | TypeVar {ident :: String}
          | Arrow {argTypes :: [Type], resultType :: Type}
          | FutureType {resultType :: Type} | ParType {resultType :: Type}
          | StreamType {resultType :: Type} | ArrayType {resultType :: Type}
            deriving (Eq)

typeComponents :: Type -> [Type]
typeComponents arrow@(Arrow argTys ty) = arrow:(concatMap typeComponents argTys ++ typeComponents ty)
typeComponents fut@(FutureType ty)     = fut:(typeComponents ty)
typeComponents par@(ParType ty)        = par:(typeComponents ty)
typeComponents ref@(RefType RefTypeInfo{parameters}) = ref : (concatMap typeComponents parameters)
typeComponents str@(StreamType ty)     = str:(typeComponents ty)
typeComponents arr@(ArrayType ty)      = arr:(typeComponents ty)
typeComponents ty                      = [ty]

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty 
    | isArrowType ty = 
        f (Arrow (map (typeMap f) (argTypes ty)) (typeMap f (resultType ty)))
    | isFutureType ty =
        f (FutureType (typeMap f (resultType ty)))
    | isParType ty =
        f (ParType (typeMap f (resultType ty)))
    | isRefType ty =
        case ty of 
          (RefType (info@(RefTypeInfo{parameters}))) -> 
              f $ RefType info{parameters = map (typeMap f) parameters}
          otherwise -> 
              error $ "Couldn't deconstruct refType: " ++ show ty
    | isStreamType ty =
        f (StreamType (typeMap f (resultType ty)))
    | isArrayType ty =
        f (ArrayType (typeMap f (resultType ty)))
    | otherwise = f ty

getArgTypes = argTypes
getResultType = resultType
getId (RefType info) = refId info
getId TypeVar {ident} = ident

getTypeParameters (RefType RefTypeInfo{parameters}) = parameters
getTypeParameters ty = error $ "Can't get type parameters from type: " ++ show ty

setTypeParameters (RefType info@(RefTypeInfo{})) params = RefType info{parameters = params}
setTypeParameters ty _ = error $ "Can't set type parameters from type: " ++ show ty

maybeParen :: Type -> String
maybeParen arr@(Arrow _ _)    = "(" ++ show arr ++ ")"
maybeParen fut@(FutureType _) = "(" ++ show fut ++ ")"
maybeParen par@(ParType _)    = "(" ++ show par ++ ")"
maybeParen str@(StreamType _) = "(" ++ show str ++ ")"
maybeParen arr@(ArrayType _)  = "(" ++ show arr ++ ")"
maybeParen ty = show ty

instance Show Type where
    show VoidType          = "void"
    show StringType        = "string"
    show IntType           = "int"
    show RealType          = "real"
    show BoolType          = "bool"
    show (RefType (RefTypeInfo {refId, parameters = []})) = refId
    show (RefType (RefTypeInfo {refId, parameters})) = 
        refId ++ "<" ++ (concat $ (intersperse ", " (map show parameters))) ++ ">"
    show (TypeVar t)       = t
    show NullType          = "null type"
    show (Arrow argTys ty) = "(" ++ (concat $ (intersperse ", " (map show argTys))) ++ ") -> " ++ show ty
    show (FutureType ty)   = "Fut " ++ maybeParen ty
    show (ParType ty)      = "Par " ++ maybeParen ty
    show (StreamType ty)   = "Stream " ++ maybeParen ty
    show (ArrayType ty)    = "[" ++ show ty ++ "]"

arrowType = Arrow
isArrowType (Arrow {}) = True
isArrowType _ = False

futureType = FutureType 
isFutureType FutureType {} = True
isFutureType _ = False

parType = ParType
isParType ParType {} = True
isParType _ = False

refTypeWithParams = \id params -> RefType $ RefTypeInfo id Unknown params
refType id = refTypeWithParams id []
streamType = StreamType
isStreamType StreamType {} = True
isStreamType _ = False

arrayType = ArrayType
isArrayType ArrayType {} = True
isArrayType _ = False

isRefType RefType {} = True
isRefType _ = False

passiveRefTypeWithParams id = makePassive . refTypeWithParams id
passiveRefType id = passiveRefTypeWithParams id []
makePassive (RefType info) = RefType $ info {activity = Passive}
makePassive ty = ty

isPassiveRefType (RefType (RefTypeInfo {activity = Passive})) = True
isPassiveRefType _ = False

activeRefTypeWithParams id = makeActive . refTypeWithParams id
activeRefType id = activeRefTypeWithParams id []
makeActive (RefType info)  = RefType $ info {activity = Active}
makeActive ty = ty

isActiveRefType (RefType (RefTypeInfo {activity = Active})) = True
isActiveRefType _ = False

isMainType (RefType (RefTypeInfo {refId = "Main"})) = True
isMainType _ = False

typeVar = TypeVar
isTypeVar (TypeVar _) = True
isTypeVar _ = False

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings ty = typeMap replace ty
    where replace ty = case lookup ty bindings of
                         Just ty' -> ty'
                         Nothing  -> ty

-- | Used to give types to AST nodes during parsing (i.e. before
-- typechecking)
emptyType :: Type
emptyType = refType "*** UN-TYPED ***"

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
    | isNullType ty1 = isNullType ty2 || isRefType ty2
    | otherwise      = ty1 == ty2
