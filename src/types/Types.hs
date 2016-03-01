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
            ,isEmptyCapability
            ,isSingleCapability
            ,refTypeFromSingletonCapability
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
            ,getMode
            ,setTypeParameters
            ,conjunctiveTypesFromCapability
            ,typesFromCapability
            ,withModeOf
            ,bar
            ,barredFields
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
            ,isSafeType
            ,isModeless
            ,modeSubtypeOf
            ,makeUnsafe
            ,makeLinear
            ,makePristine
            ,makeRead
            ,makeLockfree
            ,makeSafe
            ,isLinearRefType
            ,isPristineRefType
            ,isLockfreeRefType
            ,isReadRefType
            ,makeStackbound
            ,isStackboundType
            ) where

import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Traversable
import Control.Monad
import Control.Arrow(first)

import Identifiers

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
          | Read
          | Lockfree
          | Safe
            deriving(Eq)

instance Show Mode where
    show Linear   = "linear"
    show Unsafe   = "unsafe"
    show Read     = "read"
    show Lockfree = "lockfree"
    show Safe     = "safe"

modeSubtypeOf ty1 ty2
    | isPrimitive ty1 = True
    | Just Read <- getMode ty1
    , Just Safe <- getMode ty2 = True
    | Just Lockfree <- getMode ty1
    , Just Safe <- getMode ty2 = True
    | otherwise = getMode ty1 == getMode ty2

modeIsSafe Read = True
modeIsSafe Lockfree = True
modeIsSafe Safe = True
modeIsSafe _    = False

data RefInfo = RefInfo{refId :: String
                      ,parameters :: [Type]
                      ,mode :: Maybe Mode
                      ,barred :: [Name]
                      }

-- The current modes are irrelevant for equality checks
instance Eq RefInfo where
    ref1 == ref2 = refId ref1 == refId ref2 &&
                   parameters ref1 == parameters ref2 &&
                   barred ref1 == barred ref2

instance Show RefInfo where
    show RefInfo{mode, refId, parameters, barred}
        | null parameters = smode ++ refId ++ bar barred
        | otherwise = smode ++ refId ++ "<" ++ params ++ ">" ++ bar barred
        where
          smode
              | isNothing mode = ""
              | otherwise = show (fromJust mode) ++ " "
          params = intercalate ", " (map show parameters)
          bar [] = ""
          bar (f:fs) = " | " ++ show f ++ bar fs

showRefInfoWithoutMode RefInfo{refId, parameters, barred}
    | null parameters = refId ++ bar barred
    | otherwise = refId ++ "<" ++ params ++ ">" ++ bar barred
    where
      params = intercalate ", " (map show parameters)
      bar [] = ""
      bar (f:fs) = " | " ++ show f ++ bar fs

data Box = Stackbound
         | Pristine deriving(Eq)

instance Show Box where
    show Stackbound = "borrowed"
    show Pristine = "pristine"

data Type = Type {inner :: InnerType
                 ,box   :: Maybe Box}

bar ty f
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
    , barred <- barred info
      = ty{inner = iType{refInfo = info{barred = barred `union` [f]}}}
    | otherwise = error $ "Types.hs: Cannot bar " ++ showWithKind ty

barredFields ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
    , barred <- barred info = barred
    | otherwise = error $ "Types.hs: No barred fields in " ++ showWithKind ty

typ ity = Type{inner = ity, box = Nothing}

transferBox ty1 ty2 = ty2{box = box ty1}

instance Eq Type where
    ty1 == ty2 = inner ty1 == inner ty2

instance Show Type where
    show Type{inner, box = Nothing} = show inner
    show Type{inner, box = Just s} =
        show s ++ " " ++ show inner

data InnerType =
          UntypedRef{refInfo :: RefInfo}
        | TraitType{refInfo :: RefInfo}
        | ClassType{refInfo :: RefInfo
                   ,activity   :: Activity
                   }
        | CapabilityType{capability :: Capability}
        | TypeVar{ident :: String
                 ,tmode :: Maybe Mode}
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

getArgTypes = argTypes . inner
getResultType ty
    | hasResultType ty = resultType $ inner ty
    | otherwise = error $ "Types.hs: tried to get the resultType of " ++ show ty
getId ty
    | isRefType ty = refId . refInfo . inner $ ty
    | isTypeVar ty = ident . inner $ ty
    | isCType ty = ident . inner $ ty
    | otherwise = error $ "Types.hs: Tried to get the id of " ++
                          showWithKind ty
getMode ty
    | isRefType ty = mode . refInfo . inner $ ty
    | isTypeVar ty = tmode . inner $ ty
    | otherwise = error $ "Types.hs: Cannot get mode of " ++
                          showWithKind ty

hasResultType x
  | isArrowType x || isFutureType x || isParType x ||
    isStreamType x || isArrayType x || isMaybeType x = True
  | otherwise = False

setResultType ty res
  | hasResultType ty = ty{inner = iType{resultType = res}}
  | otherwise = error $ "Types.hs: tried to set the resultType of " ++ show ty
  where
    iType = inner ty

instance Show InnerType where
    show UntypedRef{refInfo} = show refInfo
    show TraitType{refInfo} = show refInfo
    show ClassType{refInfo} = show refInfo
    show CapabilityType{capability} = show capability
    show TypeVar{tmode = Nothing, ident} = ident
    show TypeVar{tmode = Just mode, ident} = show mode ++ " " ++ ident
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
maybeParen ty
    | isArrowType ty  ||
      isFutureType ty ||
      isParType ty    ||
      isMaybeType ty    ||
      isStreamType ty = "(" ++ show ty ++ ")"
    | otherwise = show ty

showWithKind :: Type -> String
showWithKind ty = kind (inner ty) ++ " " ++ show ty
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
    | isRefType ty = showRefInfoWithoutMode $ refInfo (inner ty)
    | otherwise = show ty

typeComponents :: Type -> [Type]
typeComponents ty
    |  isFutureType ty
    || isParType ty
    || isStreamType ty
    || isArrayType ty
    || isMaybeType ty =
        ty : typeComponents (getResultType ty)
    | isArrowType ty =
        ty : concatMap typeComponents (getArgTypes ty) ++
             typeComponents (getResultType ty)
    | isTupleType ty =
        ty : concatMap typeComponents (getArgTypes ty)
    | isRefType ty =
        ty : refInfoTypeComponents (refInfo iType)
    | isCapabilityType ty =
        ty : capabilityComponents (capability iType)
    | otherwise = [ty]
    where
      iType = inner ty

      refInfoTypeComponents = concatMap typeComponents . parameters

-- TODO: Should maybe extract the power set?
capabilityComponents :: Capability -> [Type]
capabilityComponents EmptyCapability = []
capabilityComponents Capability{typeTree} =
  concatMap traitToType $ toList typeTree
  where
    traitToType :: RefInfo -> [Type]
    traitToType t@RefInfo{parameters} = typ (TraitType t) : parameters

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty
    | isRefType ty =
        f ty{inner = refTypeMap f iType}
    | isCapabilityType ty =
        f ty{inner = capabilityTypeMap f iType}
    | isArrowType ty =
        f ty{inner = resultTypeMap f . argTypesMap f $ iType}
    |  isFutureType ty
    || isParType ty
    || isStreamType ty
    || isArrayType ty
    || isMaybeType ty =
        f ty{inner = resultTypeMap f iType}
    | isTupleType ty =
        f ty{inner = argTypesMap f iType}
    | otherwise = f ty
    where
      iType = inner ty

      refTypeMap f ity =
          ity{refInfo = refInfoTypeMap f (refInfo ity)}

      capabilityTypeMap f ity@CapabilityType{capability = EmptyCapability} = ity
      capabilityTypeMap f ity@CapabilityType{capability} =
          ity{capability = capability'}
          where
            capability' = capability{typeTree = typeTree'}
            typeTree' = fmap (traitMapAndBack f) (typeTree capability)
            traitMapAndBack f =
                refInfo . inner . typeMap f . typ . TraitType
      capabilityTypeMap f ity =
          error $ "Types.hs: Cannot call capabilityTypeMap on " ++
                  showWithKind (typ ity)

      resultTypeMap f ity =
          ity{resultType = typeMap f (resultType ity)}

      argTypesMap f ity =
          ity{argTypes = map (typeMap f) (argTypes ity)}

      refInfoTypeMap f info@RefInfo{parameters} =
          info{parameters = map (typeMap f) parameters}

typeMapM :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapM f ty
    | isRefType ty = do
        iType' <- refTypeMapM f iType
        f ty{inner = iType'}
    | isCapabilityType ty = do
        iType' <- capabilityTypeMapM f iType
        f ty{inner = iType'}
    | isArrowType ty = do
        iType' <- argTypesMapM f iType >>=
                  resultTypeMapM f
        f ty{inner = iType'}
    |  isFutureType ty
    || isParType ty
    || isStreamType ty
    || isMaybeType ty
    || isArrayType ty = do
        iType' <- resultTypeMapM f iType
        f ty{inner = iType'}
    | isTupleType ty = do
        iType' <- argTypesMapM f iType
        f ty{inner = iType'}
    | otherwise = f ty
    where
      iType = inner ty

      capabilityTypeMapM f ity@CapabilityType{capability = EmptyCapability} =
          return ity
      capabilityTypeMapM f ity@CapabilityType{capability} = do
        typeTree' <- mapM (traitMapMAndBack f) (typeTree capability)
        return ity{capability = capability{typeTree = typeTree'}}
        where
          traitMapMAndBack f =
              liftM (refInfo . inner) . typeMapM f . typ . TraitType
      capabilityTypeMapM f ity =
          error $ "Types.hs: Cannot call capabilityTypeMapM on" ++
                  showWithKind (typ ity)

      refTypeMapM f ity = do
        refInfo' <- refInfoTypeMapM f (refInfo ity)
        return ity{refInfo = refInfo'}

      refInfoTypeMapM f info@RefInfo{parameters} = do
        parameters' <- mapM (typeMapM f) parameters
        return info{parameters = parameters'}

      argTypesMapM f ity = do
        argTypes' <- mapM (typeMapM f) (argTypes ity)
        return ity{argTypes = argTypes'}

      resultTypeMapM f ity = do
        resultType' <- typeMapM f $ resultType ity
        return ity{resultType = resultType'}

getTypeParameters :: Type -> [Type]
getTypeParameters ty
    | isRefType ty = parameters $ refInfo (inner ty)
    | otherwise =
        error $ "Types.hs: Can't get type parameters from type " ++ show ty

setTypeParameters :: Type -> [Type] -> Type
setTypeParameters ty parameters
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{parameters}}}
    | otherwise =
        error $ "Types.hs: Can't set type parameters of type " ++ show ty

isEmptyCapability :: Type -> Bool
isEmptyCapability Type{inner =
                       CapabilityType{capability = EmptyCapability}} = True
isEmptyCapability _ = False

isSingleCapability :: Type -> Bool
isSingleCapability Type{inner =
                        CapabilityType{capability = Capability{typeTree}}} =
  let
    leaves = toList typeTree
  in
    length leaves == 1
isSingleCapability _ = False

refTypeFromSingletonCapability :: Type -> [Name] -> Type
refTypeFromSingletonCapability ty fs
    | isSingleCapability ty
    , info <- head . toList . typeTree . capability . inner $ ty =
        typ UntypedRef{refInfo = info{barred = fs}}
    | otherwise = error $ "Types.hs: " ++ showWithKind ty ++
                          " is not a singleton capability"

conjunctiveTypesFromCapability :: Type -> [[[Type]]]
conjunctiveTypesFromCapability ty
    | TraitType{} <- inner ty = []
    | CapabilityType{capability = EmptyCapability} <- inner ty = []
    | CapabilityType{capability} <- inner ty =
        collect $ typeTree capability
    | otherwise = error $ "Types.hs: Cannot get conjunctive types from " ++
                          showWithKind ty
    where
      collect :: TypeTree -> [[[Type]]]
      collect (Leaf _) = []
      collect (RoseTree Addition ts) = concatMap collect ts
      collect (RoseTree Product ts) =
        let
          parTypes = map (toList . fmap (typ . TraitType)) ts
        in
          concatMap collect ts ++ [parTypes]

typesFromCapability :: Type -> [Type]
typesFromCapability ty
    | ity@TraitType{} <- inner ty = [transferBox ty (typ ity)]
    | CapabilityType{capability=EmptyCapability} <- inner ty = []
    | ity@CapabilityType{capability} <- inner ty =
        map (typ . TraitType) ((toList . typeTree) capability)
    | otherwise =
        error $ "Types.hs: Can't get the traits of non-capability type " ++
                showWithKind ty

withModeOf sink source
    | isRefType sink
    , isRefType source
    , iType <- inner sink
    , info <- refInfo iType
    , mode <- mode $ refInfo (inner source)
      = sink{inner = iType{refInfo = info{mode}}}
    | isTypeVar sink
    , isTypeVar source
    , iType <- inner sink
    , tmode <- (tmode . inner) source
      = sink{inner = iType{tmode}}
    | otherwise =
        error $ "Types.hs: Can't transfer modes from " ++
                showWithKind source ++ " to " ++ showWithKind sink

refTypeWithParams refId parameters =
    typ UntypedRef{refInfo}
    where
      refInfo = RefInfo{refId
                       ,parameters
                       ,mode = Nothing
                       ,barred = []
                       }

refType id = refTypeWithParams id []

classType :: Activity -> String -> [Type] -> Type
classType activity name parameters =
    Type{inner = ClassType{refInfo = RefInfo{refId = name
                                            ,parameters
                                            ,mode = Nothing
                                            ,barred = []}
                          , activity}
        ,box = Nothing
        }

activeClassTypeFromRefType ref
    | isRefType ref
    , refInfo <- refInfo $ inner ref
      = Type{inner = ClassType{refInfo
                              ,activity = Active}
            ,box = box ref}
    | otherwise =
        error $ "Types.hs: Can't make active type from type: " ++ show ref

passiveClassTypeFromRefType ref
    | isRefType ref
    , refInfo <- refInfo $ inner ref
      = Type{inner = ClassType{refInfo
                              ,activity = Passive}
            ,box = box ref}
    | otherwise =
        error $ "Types.hs: Can't make passive type from type: " ++ show ref

sharedClassTypeFromRefType ref
    | isRefType ref
    , refInfo <- refInfo $ inner ref
      = Type{inner = ClassType{refInfo
                              ,activity = Shared}
            ,box = box ref}
    | otherwise =
        error $ "Types.hs: Can't make shared type from type: " ++ show ref

traitTypeFromRefType ty
    | isRefType ty
    , refInfo <- refInfo $ inner ty
      = Type{inner = TraitType{refInfo}
            ,box = box ty}
    | otherwise =
        error $ "Types.hs: Can't make trait type from type: " ++ show ty

typeTreeFromRefType :: Type -> TypeTree
typeTreeFromRefType ty
    | isRefType ty = Leaf (refInfo $ inner ty)
    | otherwise = error $ "Types.hs: Can't make typeTree from type: " ++ show ty

isRefType ty = isUntypedRef ty ||
               isTraitType ty ||
               isClassType ty

isTraitType Type{inner = TraitType{}} = True
isTraitType _ = False

isActiveClassType Type{inner = ClassType{activity = Active}} = True
isActiveClassType _ = False

isSharedClassType Type{inner = ClassType{activity = Shared}} = True
isSharedClassType _ = False

isPassiveClassType Type{inner = ClassType{activity = Passive}} = True
isPassiveClassType _ = False

isClassType Type{inner = ClassType{}} = True
isClassType _ = False

isUntypedRef Type{inner = UntypedRef{}} = True
isUntypedRef _ = False

-- TODO: Maybe a type can have several modes?
-- TODO: Should classes ever have modes (except the "inherited ones")?
makeUnsafe ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just Unsafe}}}
    | isTypeVar ty
    , iType <- inner ty
      = ty{inner = iType{tmode = Just Unsafe}}
    | otherwise = error $ "Types.hs: Cannot make type unsafe: " ++
                          show ty

makeLinear ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just Linear}}}
    | isTypeVar ty
    , iType <- inner ty
      = ty{inner = iType{tmode = Just Linear}}
    | otherwise = error $ "Types.hs: Cannot make type linear: " ++
                          show ty

makePristine ty = ty{box = Just Pristine}

makeRead ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just Read}}}
    | isTypeVar ty
    , iType <- inner ty
      = ty{inner = iType{tmode = Just Read}}
    | otherwise = error $ "Types.hs: Cannot make type read: " ++
                          show ty

makeSafe ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just Safe}}}
    | isTypeVar ty
    , iType <- inner ty
      = ty{inner = iType{tmode = Just Safe}}
    | otherwise = error $ "Types.hs: Cannot make type safe: " ++
                          show ty

makeLockfree ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just Lockfree}}}
    | isTypeVar ty
    , iType <- inner ty
      = ty{inner = iType{tmode = Just Lockfree}}
    | otherwise = error $ "Types.hs: Cannot make type safe: " ++
                          show ty

isSafeType ty
    | isCapabilityType ty
    , traits <- typesFromCapability ty = all isSafeType traits
    | isModeless ty = isPrimitive ty || isActiveClassType ty
    | otherwise = let mode = getMode ty in
                  isJust mode && modeIsSafe (fromJust mode)
    -- TODO: More cases?

isModeless ty
    | isRefType ty = isNothing . mode . refInfo . inner $ ty
    | isTypeVar ty = isNothing . tmode . inner $ ty
    | otherwise = True

isLinearRefType ty
    | isRefType ty = (mode . refInfo . inner) ty == Just Linear
    | isTypeVar ty = (tmode . inner) ty == Just Linear
    | otherwise = False

isPristineRefType Type{box = Just Pristine} = True
isPristineRefType _ = False

isReadRefType ty
    | isRefType ty = (mode . refInfo . inner) ty == Just Read
    | isTypeVar ty = (tmode . inner) ty == Just Read
    | otherwise = False

isLockfreeRefType ty
    | isRefType ty = (mode . refInfo . inner) ty == Just Lockfree
    | isTypeVar ty = (tmode . inner) ty == Just Lockfree
    | otherwise = False

isSafeRefType ty
    | isRefType ty = (mode . refInfo . inner) ty == Just Safe
    | isTypeVar ty = (tmode . inner) ty == Just Safe
    | otherwise = False

capabilityType :: TypeTree -> Type
capabilityType typeTree =
    typ CapabilityType{capability = Capability{typeTree}}

isCapabilityType Type{inner = CapabilityType{}} = True
isCapabilityType _ = False

incapability :: Type
incapability = Type{inner = CapabilityType{capability = EmptyCapability}
                   ,box = Nothing}

isStackboundType ty = box ty == Just Stackbound

makeStackbound ty = ty{box = Just Stackbound}

arrowType args ty = typ (ArrowType args ty)
isArrowType Type{inner = ArrowType {}} = True
isArrowType _ = False

futureType = typ . FutureType
isFutureType Type{inner = FutureType {}} = True
isFutureType _ = False

maybeType = typ . MaybeType
isMaybeType Type{inner = MaybeType {}} = True
isMaybeType _ = False

tupleType = typ . TupleType
isTupleType Type{inner = TupleType {}} = True
isTupleType _ = False

bottomType = typ BottomType
isBottomType Type{inner = BottomType {}} = True
isBottomType _ = False

parType = typ . ParType
isParType Type{inner = ParType {}} = True
isParType _ = False

streamType = typ . StreamType
isStreamType Type{inner = StreamType {}} = True
isStreamType _ = False

rangeType = typ RangeType
isRangeType Type{inner = RangeType {}} = True
isRangeType _ = False

arrayType = typ . ArrayType
isArrayType Type{inner = ArrayType {}} = True
isArrayType _ = False

typeVar ident = Type{inner = TypeVar{ident, tmode = Nothing}, box = Nothing}
isTypeVar Type{inner = TypeVar {}} = True
isTypeVar _ = False

isMainType Type{inner = ClassType{refInfo = RefInfo{refId = "Main"}}} = True
isMainType _ = False

stringObjectType = classType Passive "String" []

isStringObjectType = (==stringObjectType)

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where
      replace ty
          | isTypeVar ty =
              transferBox ty $
                  fromMaybe ty (lookup (getId ty) (map (first getId) bindings))
          | otherwise = ty

ctype :: String -> Type
ctype = typ . CType

isCType Type{inner = CType{}} = True
isCType _ = False

voidType = typ VoidType
isVoidType = (== voidType)

nullType = typ NullType
isNullType = (== nullType)

boolType = typ BoolType
isBoolType = (== boolType)

intType = typ IntType
isIntType = (== intType)

realType = typ RealType
isRealType = (== realType)

stringType = typ StringType
isStringType = (== stringType)

charType :: Type
charType = typ CharType

isCharType :: Type -> Bool
isCharType = (== charType)

primitives :: [Type]
primitives = [voidType, intType, realType, boolType, stringType, charType]

isPrimitive :: Type -> Bool
isPrimitive = (`elem` primitives)

isNumeric :: Type -> Bool
isNumeric ty = isRealType ty || isIntType ty
