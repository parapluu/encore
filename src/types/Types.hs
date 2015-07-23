{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Types(
             Type
            ,Activity (..)
            ,Capability
            ,TypeTree
            ,RoseTree (..)
            ,TypeOp (..)
            ,fromTypeTree
            ,isEmptyCapability
            ,isSingleCapability
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
            ,activeClassTypeFromRefType
            ,passiveClassTypeFromRefType
            ,sharedClassTypeFromRefType
            ,traitTypeFromRefType
            ,typeTreeFromRefType
            ,classType
            ,isRefType
            ,isTraitType
            ,isActiveClassType
            ,isSharedClassType
            ,isPassiveClassType
            ,isClassType
            ,isUntypedRef
            ,isMainType
            ,stringObjectType
            ,isStringObjectType
            ,capabilityType
            ,isCapabilityType
            ,incapability
            ,typeVar
            ,isTypeVar
            ,replaceTypeVars
            ,ctype
            ,isCType
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
            ,charType
            ,isCharType
            ,stringType
            ,isStringType
            ,isPrimitive
            ,isNumeric
            ,getArgTypes
            ,getResultType
            ,getId
            ,getTypeParameters
            ,setTypeParameters
            ,conjunctiveTypesFromCapability
            ,typesFromCapability
            ,withModeOf
            ,typeComponents
            ,typeMap
            ,typeMapM
            ,showWithKind
            ,hasSameKind
            ,maybeType
            ,isMaybeType
            ,tupleType
            ,isTupleType
            ,bottomType
            ,isBottomType
            ,hasResultType
            ,setResultType
            ,showWithoutMode
            ,isModeless
            ,modeSubtypeOf
            ,makeUnsafe
            ,makeLinear
            ,isLinearRefType
            ) where

import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Traversable
import Control.Monad

data Activity = Active
              | Shared
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
  show EmptyCapability = ""
  show Capability{typeTree} = show typeTree

data Mode = Linear
          | Unsafe
            deriving(Eq)

instance Show Mode where
    show Linear = "linear"
    show Unsafe = "unsafe"

modeSubtypeOf ty1 ty2 =
    mode (refInfo ty1) == mode (refInfo ty2)

data RefInfo = RefInfo{refId :: String
                      ,parameters :: [Type]
                      ,mode :: Maybe Mode
                      }

-- The current modes are irrelevant for equality checks
instance Eq RefInfo where
    ref1 == ref2 = refId ref1 == refId ref2 &&
                   parameters ref1 == parameters ref2

instance Show RefInfo where
    show RefInfo{mode, refId, parameters}
        | null parameters = smode ++ refId
        | otherwise = smode ++ refId ++ "<" ++ params ++ ">"
        where
          smode
              | isNothing mode = ""
              | otherwise = show (fromJust mode) ++ " "
          params = intercalate ", " (map show parameters)

showRefInfoWithoutMode RefInfo{refId, parameters}
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
          | TupleType {argTypes :: [Type]}
          | CType{ident :: String}
          | VoidType
          | StringType
          | CharType
          | IntType
          | BoolType
          | RealType
          | NullType
          | BottomType
            deriving(Eq)

hasResultType x
  | isArrowType x || isFutureType x || isParType x ||
    isStreamType x || isArrayType x || isMaybeType x = True
  | otherwise = False

setResultType ty res
  | hasResultType ty = ty { resultType = res}
  | otherwise = error $ "Types.hs: tried to set the resultType of " ++ show ty

getArgTypes = argTypes
getResultType ty
    | hasResultType ty = resultType ty
    | otherwise = error $ "Types.hs: tried to get the resultType of " ++ show ty
getId UntypedRef{refInfo} = refId refInfo
getId TraitType{refInfo} = refId refInfo
getId ClassType{refInfo} = refId refInfo
getId TypeVar{ident} = ident
getId CType{ident} = ident
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
    show (TupleType{argTypes}) = "(" ++ args ++ ")"
      where
        args = intercalate ", " (map show argTypes)
    show (CType ty) = ty
    show VoidType   = "void"
    show StringType = "string"
    show CharType   = "char"
    show IntType    = "int"
    show RealType   = "real"
    show BoolType   = "bool"
    show NullType   = "null type"
    show BottomType = "Bottom"

maybeParen :: Type -> String
maybeParen arr@(ArrowType _ _) = "(" ++ show arr ++ ")"
maybeParen fut@(FutureType _)  = "(" ++ show fut ++ ")"
maybeParen par@(ParType _)     = "(" ++ show par ++ ")"
maybeParen str@(StreamType _)  = "(" ++ show str ++ ")"
maybeParen arr@(ArrayType _)   = "(" ++ show arr ++ ")"
maybeParen opt@(MaybeType _)   = "(" ++ show opt ++ ")"
maybeParen ty = show ty

showWithKind :: Type -> String
showWithKind ty = kind ty ++ " " ++ show ty
    where
    kind VoidType                      = "primitive type"
    kind StringType                    = "primitive type"
    kind CharType                      = "primitive type"
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
    kind TupleType{}                   = "tuple type"
    kind BottomType{}                  = "bottom type"
    kind CType{}                       = "embedded type"
    kind _                             = "type"

hasSameKind :: Type -> Type -> Bool
hasSameKind ty1 ty2
  | areBoth isMaybeType ||
    areBoth isFutureType ||
    areBoth isParType ||
    areBoth isArrayType ||
    areBoth isStreamType = getResultType ty1 `hasSameKind` getResultType ty2
  | (isBottomTy1 || isBottomTy2) && not (areBoth isBottomType) = True -- xor
  | areBoth isPrimitive ||
    areBoth isTupleType ||
    areBoth isTypeVar ||
    areBoth isRefType ||
    areBoth isCapabilityType ||
    areBoth isArrowType = True
  | otherwise = False
  where
    isBottomTy1 = isBottomType ty1
    isBottomTy2 = isBottomType ty2
    areBoth typeFun = typeFun ty1 && typeFun ty2

showWithoutMode :: Type -> String
showWithoutMode ty
    | isRefType ty = showRefInfoWithoutMode $ refInfo ty
    | otherwise = show ty

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
typeComponents tuple@(TupleType{argTypes}) =
    tuple : (concatMap typeComponents argTypes)
typeComponents ty = [ty]

refInfoTypeComponents = concatMap typeComponents . parameters

-- TODO: Should maybe extract the power set?
capabilityComponents :: Capability -> [Type]
capabilityComponents EmptyCapability = []
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
typeMap f ty@TupleType{argTypes} =
    f ty{argTypes = map (typeMap f) argTypes}
typeMap f ty = f ty

refInfoTypeMap :: (Type -> Type) -> RefInfo -> RefInfo
refInfoTypeMap f info@RefInfo{parameters} =
    info{parameters = map (typeMap f) parameters}

capabilityTypeMap :: (Type -> Type) -> Capability -> Capability
capabilityTypeMap _ EmptyCapability = EmptyCapability
capabilityTypeMap f cap@Capability{typeTree} =
    cap{typeTree = fmap (refInfo . typeMap f . TraitType) typeTree}

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
typeMapM f ty@TupleType{argTypes} = do
  argTypes' <- mapM (typeMapM f) argTypes
  f ty{argTypes = argTypes'}
typeMapM f ty
  | isFutureType ty || isParType ty || isStreamType ty ||
    isArrayType ty || isMaybeType ty = typeMapMResultType f ty
  | otherwise = f ty

typeMapMResultType :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapMResultType f ty = do
  resultType' <- typeMapM f $ resultType ty
  f ty{resultType = resultType'}

refInfoTypeMapM :: Monad m => (Type -> m Type) -> RefInfo -> m RefInfo
refInfoTypeMapM f info@RefInfo{parameters} = do
  parameters' <- mapM (typeMapM f) parameters
  return info{parameters = parameters'}

capabilityTypeMapM :: Monad m => (Type -> m Type) -> Capability -> m Capability
capabilityTypeMapM f EmptyCapability = return EmptyCapability
capabilityTypeMapM f cap@Capability{typeTree} = do
  typeTree' <- mapM (liftM refInfo . typeMapM f . TraitType) typeTree
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

isEmptyCapability :: Type -> Bool
isEmptyCapability CapabilityType{capability=EmptyCapability} = True
isEmptyCapability _ = False

isSingleCapability :: Type -> Bool
isSingleCapability CapabilityType{capability=Capability{typeTree}} =
  let
    leaves = toList typeTree
    first = head leaves
    single = leaves == first : []
  in
    single
isSingleCapability _ = False

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
conjunctiveTypesFromCapability ty =
  error $ "Types.hs: Cannot get conjunctive types from " ++ showWithKind ty

typesFromCapability :: Type -> [Type]
typesFromCapability t@TraitType{} = [t]
typesFromCapability CapabilityType{capability=EmptyCapability} = []
typesFromCapability ty@CapabilityType{capability} =
    map TraitType ((toList . typeTree) capability)
typesFromCapability ty =
    error $ "Types.hs: Can't get the traits of non-capability type "
      ++ showWithKind ty

withModeOf sink source
    | isRefType sink
    , isRefType source
    , info <- refInfo sink
    , m <- mode $ refInfo source
      = sink{refInfo = info{mode = m}}
    | otherwise =
        error $ "Types.hs: Can't transfer modes from " ++
                showWithKind source ++ " to " ++ showWithKind sink

withTypeParametersOf sink source =
    let formals = getTypeParameters sink
        actuals = getTypeParameters source
        bindings = zip formals actuals
    in
       replaceTypeVars bindings sink

refTypeWithParams refId parameters =
    UntypedRef{refInfo = RefInfo{refId,
                                 parameters,
                                 mode = Nothing}}

refType :: String -> Type
refType id = refTypeWithParams id []

classType :: Activity -> String -> [Type] -> Type
classType activity name parameters =
    ClassType{refInfo = RefInfo{refId = name
                               ,parameters
                               ,mode = Nothing}, activity}

activeClassTypeFromRefType :: Type -> Type
activeClassTypeFromRefType ty
    | isRefType ty = ClassType{refInfo = refInfo ty, activity = Active}
    | otherwise =
        error $ "Types.hs: Tried to make a class type out of " ++
                showWithKind ty

passiveClassTypeFromRefType :: Type -> Type
passiveClassTypeFromRefType ty
    | isRefType ty = ClassType{refInfo = refInfo ty, activity = Passive}
    | otherwise =
        error $ "Types.hs: Tried to make a class type out of " ++
                showWithKind ty

sharedClassTypeFromRefType :: Type -> Type
sharedClassTypeFromRefType ty
    | isRefType ty = ClassType{refInfo = refInfo ty, activity = Shared}
    | otherwise =
        error $ "Types.hs: Tried to make a class type out of " ++
                showWithKind ty

traitTypeFromRefType :: Type -> Type
traitTypeFromRefType ty
    | isRefType ty = TraitType{refInfo = refInfo ty}
    | otherwise =
        error $ "Types.hs: Can't make trait type from type: " ++
                showWithKind ty

typeTreeFromRefType :: Type -> TypeTree
typeTreeFromRefType ty
    | isRefType ty = Leaf (refInfo ty)
    | otherwise = error $ "Types.hs: Can't make typeTree from type: " ++ show ty

isRefType UntypedRef {} = True
isRefType TraitType {} = True
isRefType ClassType {} = True
isRefType _ = False

isTraitType TraitType{} = True
isTraitType _ = False

isActiveClassType ClassType{activity = Active} = True
isActiveClassType _ = False

isSharedClassType ClassType{activity = Shared} = True
isSharedClassType _ = False

isPassiveClassType ClassType{activity = Passive} = True
isPassiveClassType _ = False

isClassType ClassType{} = True
isClassType _ = False

isUntypedRef UntypedRef{} = True
isUntypedRef _ = False

-- TODO: Maybe a type can have several modes?
-- TODO: Should classes ever have modes (except the "inherited ones")?
makeUnsafe ty
    | isRefType ty = ty{refInfo = info{mode = Just Unsafe}}
    | otherwise = error $ "Types.hs: Cannot make type unsafe: " ++
                          show ty
    where
      info = refInfo ty

makeLinear ty
    | isRefType ty = ty{refInfo = info{mode = Just Linear}}
    | otherwise = error $ "Types.hs: Cannot make type linear: " ++
                          show ty
    where
      info = refInfo ty

isModeless ty
    | isRefType ty = isNothing $ mode (refInfo ty)
    | otherwise = error $ "Types.hs: Cannot get modes of type: " ++
                          show ty

isLinearRefType ty
    | isRefType ty = mode (refInfo ty) == Just Linear
    | otherwise = False

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

tupleType = TupleType
isTupleType TupleType {} = True
isTupleType _ = False

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

stringObjectType = classType Passive "String" []

isStringObjectType = (==stringObjectType)

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where replace ty = fromMaybe ty (lookup ty bindings)

ctype :: String -> Type
ctype = CType

isCType CType{} = True
isCType _ = False

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

charType :: Type
charType = CharType

isCharType :: Type -> Bool
isCharType = (== charType)

primitives :: [Type]
primitives = [voidType, intType, realType, boolType, stringType, charType]

isPrimitive :: Type -> Bool
isPrimitive = (`elem` primitives)

isNumeric :: Type -> Bool
isNumeric ty = isRealType ty || isIntType ty
