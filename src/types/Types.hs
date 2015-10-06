{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types(
              Type
            , Capability
            , TypeTree
            , RoseTree (..)
            , TypeOp (..)
            , RefInfo (..)
            , fromTypeTree
            , emptyCapability
            , singleCapability
            ,arrowType
            ,isArrowType
            ,futureType
            ,isFutureType
            ,parType
            ,isParType
            ,streamType
            ,isStreamType
            ,rangeType
            ,arrayType
            ,isArrayType
            ,isRangeType
            ,refTypeWithParams
            ,refType
            ,traitTypeFromRefType
            ,passiveClassTypeFromRefType
            ,activeClassTypeFromRefType
            ,isRefType
            ,isTraitType
            ,isActiveClassType
            ,isPassiveClassType
            ,isClassType
            ,isMainType
            ,capabilityType
            ,isCapabilityType
            ,incapability
            ,typeVar
            ,isTypeVar
            ,replaceTypeVars
            ,voidType
            ,isVoidType
            ,nullType
            ,isNullType
            ,boolType
            ,isBoolType
            ,intType
            ,isIntType
            ,realType
            ,isRealType
            ,stringType
            ,isStringType
            ,isPrimitive
            ,isNumeric
            ,getArgTypes
            ,getResultType
            ,getId
            ,getTypeParameters
            ,setTypeParameters
            , conjunctiveTypesFromCapability
            ,typesFromCapability
            ,typeComponents
            ,typeMap
            ,typeMapM
            ,showWithKind
            ,hasSameKind
            ,maybeType
            ,isMaybeType
            ,bottomType
            ,isBottomType
            ,hasResultType
            ,setResultType
            ) where

import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Traversable

data Activity = Active
              | Passive
                deriving(Eq, Show)

data TypeOp = Product | Addition
  deriving (Eq)

instance Show TypeOp where
  show Product = "*"
  show Addition = "+"

type TypeTree = RoseTree RefInfo

data RoseTree t = Leaf t
                | RoseTree TypeOp [RoseTree t]
              deriving (Eq)

instance Functor RoseTree where
  fmap f (Leaf i) = Leaf $ f i
  fmap f (RoseTree op ts) = RoseTree op $ map (fmap f) ts

instance Foldable RoseTree where
  foldMap f (Leaf i) = f i
  foldMap f (RoseTree _ ts) = mconcat $ map (foldMap f) ts

instance Traversable RoseTree where
  traverse f (Leaf i) = Leaf <$> f i
  traverse f (RoseTree op ts) = RoseTree op <$> traverse (traverse f) ts

instance Show TypeTree where
  show = removeParens . pp'
    where
      removeParens :: String -> String
      removeParens = init . tail

      pp' :: TypeTree -> String
      pp' (Leaf t) = show t
      pp' (RoseTree op ts) =
        let
          opStr = concat $ [" ", show op, " "]
          strings = map pp' ts
          result = concat $ intersperse opStr strings
          needParens = op == Addition
        in
          if needParens then parens result else result
        where
          parens str = concat ["(", str, ")"]

data Capability = Capability { typeTree :: TypeTree }
                | EmptyCapability
  deriving (Eq)

instance Show Capability where
  show Capability{typeTree} = show typeTree

data RefInfo = RefInfo{refId :: String
                      ,parameters :: [Type]
                      } deriving(Eq)

instance Show RefInfo where
    show RefInfo{refId, parameters}
        | null parameters = refId
        | otherwise = refId ++ "<" ++ params ++ ">"
        where
          params = intercalate ", " (map show parameters)

data Type = UntypedRef{refInfo :: RefInfo}
          | TraitType{refInfo :: RefInfo}
          | ClassType{refInfo :: RefInfo
                     ,activity   :: Activity
                     }
          | CapabilityType{capability :: Capability}
          | TypeVar{ident :: String}
          | ArrowType{argTypes   :: [Type]
                     ,resultType :: Type
                     }
          | FutureType{resultType :: Type}
          | ParType{resultType :: Type}
          | StreamType{resultType :: Type}
          | ArrayType{resultType :: Type}
          | RangeType
          | MaybeType {resultType :: Type}
          | VoidType
          | StringType
          | IntType
          | BoolType
          | RealType
          | NullType
          | BottomType
            deriving(Eq)

hasResultType x
  | (isArrowType x || isFutureType x || isParType x ||
     isStreamType x || isArrowType x || isMaybeType x) = True
  | otherwise = False

setResultType ty res
  | hasResultType ty = ty { resultType = res}
  | otherwise = error $ "Types.hs: tried to set the resultType of " ++ show ty

getArgTypes = argTypes
getResultType = resultType
getId UntypedRef{refInfo} = refId refInfo
getId TraitType{refInfo} = refId refInfo
getId ClassType{refInfo} = refId refInfo
getId TypeVar{ident} = ident
getId ty = error $ "Types.hs: Tried to get the ID of " ++ showWithKind ty

instance Show Type where
    show UntypedRef{refInfo} = show refInfo
    show TraitType{refInfo} = show refInfo
    show ClassType{refInfo} = show refInfo
    show CapabilityType{capability} = show capability
    show TypeVar{ident} = ident
    show ArrowType{argTypes, resultType} =
        "(" ++ args ++ ") -> " ++ show resultType
        where
          args = intercalate ", " (map show argTypes)
    show FutureType{resultType} = "Fut " ++ maybeParen resultType
    show ParType{resultType}    = "Par " ++ maybeParen resultType
    show StreamType{resultType} = "Stream " ++ maybeParen resultType
    show ArrayType{resultType}  = "[" ++ show resultType ++ "]"
    show RangeType   = "Range"
    show (MaybeType ty)    = "Maybe " ++ maybeParen ty
    show VoidType   = "void"
    show StringType = "string"
    show IntType    = "int"
    show RealType   = "real"
    show BoolType   = "bool"
    show NullType   = "null type"
    show BottomType      = "Bottom"

maybeParen :: Type -> String
maybeParen arr@(ArrowType _ _) = "(" ++ show arr ++ ")"
maybeParen fut@(FutureType _)  = "(" ++ show fut ++ ")"
maybeParen par@(ParType _)     = "(" ++ show par ++ ")"
maybeParen str@(StreamType _)  = "(" ++ show str ++ ")"
maybeParen arr@(ArrayType _)   = "(" ++ show arr ++ ")"
maybeParen ty = show ty

showWithKind :: Type -> String
showWithKind ty = kind ty ++ " " ++ show ty
    where
    kind VoidType                      = "primitive type"
    kind StringType                    = "primitive type"
    kind IntType                       = "primitive type"
    kind RealType                      = "primitive type"
    kind BoolType                      = "primitive type"
    kind UntypedRef{}                  = "untyped type"
    kind TraitType{}                   = "trait type"
    kind ClassType{activity = Active}  = "active class type"
    kind ClassType{activity = Passive} = "passive class type"
    kind CapabilityType{}              = "capability type"
    kind TypeVar{}                     = "polymorphic type"
    kind ArrowType{}                   = "function type"
    kind FutureType{}                  = "future type"
    kind ParType{}                     = "parallel type"
    kind StreamType{}                  = "stream type"
    kind RangeType{}                   = "range type"
    kind ArrayType{}                   = "array type"
    kind MaybeType{}                   = "maybe type"
    kind BottomType{}                  = "bottom type"
    kind _                             = "type"

hasSameKind :: Type -> Type -> Bool
hasSameKind ty1 ty2
  | areBoth isMaybeType ||
    areBoth isFutureType ||
    areBoth isParType ||
    areBoth isCapabilityType = getResultType ty1 `hasSameKind` getResultType ty2
  | (isBottomTy1 || isBottomTy2) && not (areBoth isBottomType) = True -- xor
  | otherwise = True
  where
    isBottomTy1 = isBottomType ty1
    isBottomTy2 = isBottomType ty2
    areBoth typeFun = typeFun ty1 && typeFun ty2

typeComponents :: Type -> [Type]
typeComponents arrow@(ArrowType argTys ty) =
    arrow : (concatMap typeComponents argTys ++ typeComponents ty)
typeComponents fut@(FutureType ty) =
    fut : typeComponents ty
typeComponents par@(ParType ty) =
    par : typeComponents ty
typeComponents ref@(UntypedRef{refInfo}) =
    ref : refInfoTypeComponents refInfo
typeComponents ref@(TraitType{refInfo}) =
    ref : refInfoTypeComponents refInfo
typeComponents ref@(ClassType{refInfo}) =
    ref : refInfoTypeComponents refInfo
typeComponents ref@(CapabilityType{capability}) =
    ref : capabilityComponents capability
typeComponents str@(StreamType ty) =
    str : typeComponents ty
typeComponents arr@(ArrayType ty)  =
    arr : typeComponents ty
typeComponents maybe@(MaybeType ty) =
    maybe : typeComponents ty
typeComponents ty = [ty]

refInfoTypeComponents = concatMap typeComponents . parameters

-- TODO: Should maybe extract the power set?
capabilityComponents :: Capability -> [Type]
capabilityComponents Capability{typeTree} =
  concatMap traitToType $ toList typeTree
  where
    traitToType :: RefInfo -> [Type]
    traitToType t@RefInfo{parameters} = TraitType t : parameters

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty@UntypedRef{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@TraitType{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@ClassType{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@CapabilityType{capability} =
    f ty{capability = capabilityTypeMap f capability}
typeMap f ty@ArrowType{argTypes, resultType} =
    f ty{argTypes = map (typeMap f) argTypes
        ,resultType = typeMap f resultType}
typeMap f ty@FutureType{resultType} =
    f ty{resultType = typeMap f resultType}
typeMap f ty@ParType{resultType} =
    f ty{resultType = typeMap f resultType}
typeMap f ty@StreamType{resultType} =
    f ty{resultType = typeMap f resultType}
typeMap f ty@ArrayType{resultType} =
    f ty{resultType = typeMap f resultType}
typeMap f ty@MaybeType{resultType} =
    f ty{resultType = typeMap f resultType}
typeMap f ty = f ty

refInfoTypeMap :: (Type -> Type) -> RefInfo -> RefInfo
refInfoTypeMap f info@RefInfo{parameters} =
    info{parameters = map (typeMap f) parameters}

capabilityTypeMap :: (Type -> Type) -> Capability -> Capability
capabilityTypeMap f cap@Capability{typeTree} =
    cap{typeTree = fmap (refInfoTypeMap f) typeTree}

typeMapM :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapM f ty@UntypedRef{refInfo} = do
  refInfo' <- refInfoTypeMapM f refInfo
  f ty{refInfo = refInfo'}
typeMapM f ty@TraitType{refInfo} = do
  refInfo' <- refInfoTypeMapM f refInfo
  f ty{refInfo = refInfo'}
typeMapM f ty@ClassType{refInfo} = do
  refInfo' <- refInfoTypeMapM f refInfo
  f ty{refInfo = refInfo'}
typeMapM f ty@CapabilityType{capability} = do
  capability' <- capabilityTypeMapM f capability
  f ty{capability = capability'}
typeMapM f ty@ArrowType{argTypes, resultType} = do
  argTypes' <- mapM (typeMapM f) argTypes
  resultType' <- f resultType
  f ty{argTypes = argTypes'
               ,resultType = resultType'}
typeMapM f ty
  | isFutureType ty || isParType ty || isStreamType ty ||
    isArrayType ty || isMaybeType ty = typeMapMResultType f ty
  | otherwise = f ty

typeMapMResultType :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapMResultType f ty = do
  resultType' <- f $ resultType ty
  f ty{resultType = resultType'}

refInfoTypeMapM :: Monad m => (Type -> m Type) -> RefInfo -> m RefInfo
refInfoTypeMapM f info@RefInfo{parameters} = do
  parameters' <- mapM (typeMapM f) parameters
  return info{parameters = parameters'}

capabilityTypeMapM :: Monad m => (Type -> m Type) -> Capability -> m Capability
capabilityTypeMapM f EmptyCapability = return EmptyCapability
capabilityTypeMapM f cap@Capability{typeTree} = do
  typeTree' <- mapM (refInfoTypeMapM f) typeTree
  return $ cap{typeTree = typeTree'}

getTypeParameters :: Type -> [Type]
getTypeParameters UntypedRef{refInfo} = parameters refInfo
getTypeParameters TraitType{refInfo} = parameters refInfo
getTypeParameters ClassType{refInfo} = parameters refInfo
getTypeParameters ty =
    error $ "Types.hs: Can't get type parameters from type " ++ show ty

setTypeParameters ty@UntypedRef{refInfo} parameters =
    ty{refInfo = refInfo{parameters}}
setTypeParameters ty@TraitType{refInfo} parameters =
    ty{refInfo = refInfo{parameters}}
setTypeParameters ty@ClassType{refInfo} parameters =
    ty{refInfo = refInfo{parameters}}
setTypeParameters ty _ =
    error $ "Types.hs: Can't set type parameters of type " ++ show ty

emptyCapability :: Type -> Bool
emptyCapability CapabilityType{capability=EmptyCapability} = True
emptyCapability _ = False

singleCapability :: Type -> Bool
singleCapability CapabilityType{capability=EmptyCapability} = False
singleCapability CapabilityType{capability=Capability{typeTree}} =
  let
    leaves = toList typeTree
    first = head leaves
    single = leaves == first : []
  in
    single
singleCapability ty =
  error $ "Expects CapabilityType " ++ show ty

conjunctiveTypesFromCapability :: Type -> [[[Type]]]
conjunctiveTypesFromCapability t@TraitType{} = []
conjunctiveTypesFromCapability CapabilityType{capability=EmptyCapability} = []
conjunctiveTypesFromCapability ty@CapabilityType{capability} =
  collect $ typeTree capability
  where
    collect :: TypeTree -> [[[Type]]]
    collect (Leaf _) = []
    collect (RoseTree Addition ts) = concatMap collect ts
    collect (RoseTree Product ts) =
      let
        parTypes = map toList $ map (fmap TraitType) ts
      in
        concatMap collect ts ++ [parTypes]

typesFromCapability :: Type -> [Type]
typesFromCapability t@TraitType{} = [t]
typesFromCapability CapabilityType{capability=EmptyCapability} = []
typesFromCapability ty@CapabilityType{capability} =
    map TraitType ((toList . typeTree) capability)
typesFromCapability ty =
    error $ "Types.hs: Can't get the traits of non-capability type "
      ++ showWithKind ty

refTypeWithParams refId parameters =
    UntypedRef{refInfo = RefInfo{refId, parameters}}

refType :: String -> Type
refType id = refTypeWithParams id []

activeClassTypeFromRefType UntypedRef{refInfo} =
      ClassType{refInfo, activity = Active}
activeClassTypeFromRefType ty =
    error $ "Types.hs: Can't make active type from type: " ++ show ty

passiveClassTypeFromRefType UntypedRef{refInfo}  =
      ClassType{refInfo, activity = Passive}
passiveClassTypeFromRefType ty =
    error $ "Types.hs: Can't make passive type from type: " ++ show ty

traitTypeFromRefType UntypedRef{refInfo} =
    TraitType{refInfo}
traitTypeFromRefType ty =
    error $ "Types.hs: Can't make trait type from type: " ++ show ty

isRefType UntypedRef {} = True
isRefType TraitType {} = True
isRefType ClassType {} = True
isRefType _ = False

isTraitType TraitType{} = True
isTraitType _ = False

isActiveClassType ClassType{activity = Active} = True
isActiveClassType _ = False

isPassiveClassType ClassType{activity = Passive} = True
isPassiveClassType _ = False

isClassType ClassType{} = True
isClassType _ = False

fromTypeTree :: TypeTree -> Type
fromTypeTree typeTree =
  CapabilityType{capability = Capability{typeTree}}

capabilityType :: TypeTree -> Type
capabilityType typeTree =
    CapabilityType{capability = Capability{typeTree}}

isCapabilityType CapabilityType{} = True
isCapabilityType _ = False

incapability :: Type
incapability = CapabilityType{capability = EmptyCapability}

arrowType = ArrowType
isArrowType (ArrowType {}) = True
isArrowType _ = False

futureType = FutureType
isFutureType FutureType {} = True
isFutureType _ = False

maybeType = MaybeType
isMaybeType MaybeType {} = True
isMaybeType _ = False

bottomType = BottomType
isBottomType BottomType {} = True
isBottomType _ = False

parType = ParType
isParType ParType {} = True
isParType _ = False

streamType = StreamType
isStreamType StreamType {} = True
isStreamType _ = False

rangeType = RangeType
isRangeType RangeType = True
isRangeType _         = False

arrayType = ArrayType
isArrayType ArrayType {} = True
isArrayType _ = False

typeVar = TypeVar
isTypeVar (TypeVar _) = True
isTypeVar _ = False

isMainType ClassType{refInfo = RefInfo{refId = "Main"}} = True
isMainType _ = False

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where replace ty = fromMaybe ty (lookup ty bindings)

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
