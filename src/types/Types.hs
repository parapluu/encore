{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Types(
              Type
            , Activity (..)
            , Capability
            , TypeTree
            , RoseTree (..)
            , TypeOp (..)
            , RefInfo (..) -- probably don't want to expose this
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
            , classType
            ,isRefType
            ,isTraitType
            ,isActiveClassType
            , isSharedClassType
            ,isPassiveClassType
            ,isClassType
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
            ,setArgTypes
            ,getResultType
            ,getId
            ,maybeGetId
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
            ,tupleType
            ,isTupleType
            ,bottomType
            ,isBottomType
            ,hasResultType
            ,setResultType
            ,isPrintable
            ,typeSynonym
            ,isTypeSynonym
            ,typeSynonymLHS
            ,typeSynonymRHS
            ,typeSynonymSetRHS
            ,unfoldTypeSynonyms
            ) where

import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Traversable

data Activity = Active
              | Shared
              | Passive
                deriving(Eq, Show)

data TypeOp = Product | Addition
  deriving (Eq)

instance Show TypeOp where
  show Product = "*"
  show Addition = "+"

type TypeTree = RoseTree Type

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

data RefInfo = RefInfo{refId :: String
                      ,parameters :: [Type]
                      } deriving(Eq)

instance Show RefInfo where
    show RefInfo{refId, parameters}
        | null parameters = refId
        | otherwise = refId ++ "<" ++ params ++ ">"
        where
          params = intercalate ", " (map show parameters)

data Type = Unresolved{refInfo :: RefInfo}
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
          | TypeSynonym{refInfo :: RefInfo, resolvesTo :: Type}
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
setArgTypes ty argTypes = ty{argTypes}

getResultType ty
    | hasResultType ty = resultType ty
    | otherwise = error $ "Types.hs: tried to get the resultType of " ++ show ty

getId ty = case maybeGetId ty of
     Nothing -> error $ "Types.hs: Tried to get the ID of " ++ showWithKind ty
     Just t -> t
 
maybeGetId Unresolved{refInfo} = Just $ refId refInfo
maybeGetId TraitType{refInfo} = Just $ refId refInfo
maybeGetId ClassType{refInfo} = Just $ refId refInfo
maybeGetId TypeSynonym{refInfo} = Just $ refId refInfo 
maybeGetId TypeVar{ident} = Just $ ident
maybeGetId CType{ident} = Just $ ident
maybeGetId _ = Nothing

instance Show Type where
    show Unresolved{refInfo} = show refInfo
    show TraitType{refInfo} = show refInfo
    show ClassType{refInfo} = show refInfo
    show CapabilityType{capability} = show capability
    show TypeVar{ident} = ident
    show ArrowType{argTypes = [ty], resultType} =
        show ty ++ " -> " ++ show resultType
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
    show TypeSynonym{refInfo, resolvesTo} = show refInfo ++ "(= " ++ show resolvesTo ++ ")"   
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
    kind Unresolved{}                  = "unresolved type"
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
    kind TypeSynonym{}                 = "type synonym"
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

typeComponents :: Type -> [Type]
typeComponents arrow@(ArrowType argTys ty) =
    arrow : (concatMap typeComponents argTys ++ typeComponents ty)
typeComponents fut@(FutureType ty) =
    fut : typeComponents ty
typeComponents par@(ParType ty) =
    par : typeComponents ty
typeComponents ref@(Unresolved{refInfo}) =
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
    tuple : concatMap typeComponents argTypes
typeComponents ty = [ty]

refInfoTypeComponents = concatMap typeComponents . parameters

-- TODO: Should maybe extract the power set?
capabilityComponents :: Capability -> [Type]
capabilityComponents EmptyCapability = []
capabilityComponents Capability{typeTree} =
  concatMap typeComponents $ toList typeTree

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty@Unresolved{refInfo} =
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
typeMap f ty@TypeSynonym{refInfo, resolvesTo} =
 f ty{refInfo = refInfoTypeMap f refInfo, resolvesTo = typeMap f resolvesTo}    
typeMap f ty = f ty

refInfoTypeMap :: (Type -> Type) -> RefInfo -> RefInfo
refInfoTypeMap f info@RefInfo{parameters} =
    info{parameters = map (typeMap f) parameters}

capabilityTypeMap :: (Type -> Type) -> Capability -> Capability
capabilityTypeMap _ EmptyCapability = EmptyCapability
capabilityTypeMap f cap@Capability{typeTree} =
    cap{typeTree = fmap (typeMap f) typeTree}

typeMapM :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapM f ty@Unresolved{refInfo} = do
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
  resultType' <- typeMapM f resultType
  f ty{argTypes = argTypes'
      ,resultType = resultType'}
typeMapM f ty@TupleType{argTypes} = do
  argTypes' <- mapM (typeMapM f) argTypes
  f ty{argTypes = argTypes'}
typeMapM f ty@TypeSynonym{refInfo, resolvesTo} = do
 refInfo' <- refInfoTypeMapM f refInfo
 resolvesTo' <- typeMapM f resolvesTo
 f ty{refInfo = refInfo', resolvesTo = resolvesTo'}
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
  typeTree' <- mapM (typeMapM f) typeTree
  return $ cap{typeTree = typeTree'}

getTypeParameters :: Type -> [Type]
getTypeParameters Unresolved{refInfo} = parameters refInfo
getTypeParameters TraitType{refInfo} = parameters refInfo
getTypeParameters ClassType{refInfo} = parameters refInfo
getTypeParameters TypeSynonym{refInfo} = parameters refInfo
getTypeParameters ty =
    error $ "Types.hs: Can't get type parameters from type " ++ show ty

setTypeParameters ty@Unresolved{refInfo} parameters =
    ty{refInfo = refInfo{parameters}}
setTypeParameters ty@TraitType{refInfo} parameters =
    ty{refInfo = refInfo{parameters}}
setTypeParameters ty@ClassType{refInfo} parameters =
    ty{refInfo = refInfo{parameters}}
setTypeParameters ty@TypeSynonym{refInfo, resolvesTo} params =
    let subst = zip (parameters refInfo) params
    in ty{refInfo = refInfo{parameters = params}, resolvesTo=apply subst resolvesTo}
setTypeParameters ty _ =
    error $ "Types.hs: Can't set type parameters of type " ++ show ty

class TypeSubstitution t where
  apply :: [(Type, Type)] -> t -> t

instance TypeSubstitution Type where
  apply subst t@TypeVar{} =
       case lookup t subst of
           Nothing -> t
           Just t' -> t'
  apply subst t@Unresolved{refInfo} = t{refInfo=apply subst refInfo}
  apply subst t@TraitType{refInfo} = t{refInfo=apply subst refInfo}
  apply subst t@ClassType{refInfo} = t{refInfo=apply subst refInfo}
  apply subst t@TypeSynonym{refInfo, resolvesTo} =
       t{refInfo = apply subst refInfo, resolvesTo = apply subst resolvesTo}
  apply subst t@CapabilityType{capability} = t{capability = apply subst capability}
  apply subst t@ArrowType{argTypes, resultType} =
       t{argTypes = apply subst argTypes, resultType = apply subst resultType}
  apply subst t@FutureType{resultType} = t{resultType = apply subst resultType}
  apply subst t@ParType{resultType} = t{resultType = apply subst resultType}
  apply subst t@StreamType{resultType} = t{resultType = apply subst resultType}
  apply subst t@ArrayType{resultType} = t{resultType = apply subst resultType}
  apply subst t@MaybeType{resultType} = t{resultType = apply subst resultType}
  apply subst t@TupleType{argTypes} = t{argTypes = apply subst argTypes}
  apply _ t = t

instance TypeSubstitution RefInfo where
  apply subst r@RefInfo{parameters} = r{parameters=map (apply subst) parameters}

instance TypeSubstitution t => TypeSubstitution [t] where
  apply subst l = map (apply subst) l

instance TypeSubstitution Capability where
  apply subst EmptyCapability = EmptyCapability
  apply subst t@Capability{typeTree} = t{typeTree = apply subst typeTree}

instance TypeSubstitution t => TypeSubstitution (RoseTree t) where
  apply subst (Leaf t) = Leaf (apply subst t)
  apply subst (RoseTree op l) = RoseTree op (apply subst l)

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
  error $ "Types.hs: Expects CapabilityType " ++ show ty

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
        parTypes = map toList ts
      in
        concatMap collect ts ++ [parTypes]
conjunctiveTypesFromCapability ty =
  error $ "Types.hs: Cannot get conjunctive types from " ++ showWithKind ty

typesFromCapability :: Type -> [Type]
typesFromCapability t@TraitType{} = [t]
typesFromCapability CapabilityType{capability=EmptyCapability} = []
typesFromCapability ty@CapabilityType{capability} =
    toList $ typeTree capability
typesFromCapability ty =
    error $ "Types.hs: Can't get the traits of non-capability type "
      ++ showWithKind ty

refTypeWithParams refId parameters =
    Unresolved{refInfo = RefInfo{refId, parameters}}

refType :: String -> Type
refType id = refTypeWithParams id []

classType :: Activity -> String -> [Type] -> Type
classType activity name parameters =
  ClassType{refInfo = RefInfo{refId = name, parameters}, activity}

traitTypeFromRefType Unresolved{refInfo} =
    TraitType{refInfo}
traitTypeFromRefType ty =
    error $ "Types.hs: Can't make trait type from type: " ++ show ty

isRefType Unresolved {} = True
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

isPrintable :: Type -> Bool
isPrintable ty
  | isTypeVar ty     = False
  | isCType ty       = False
  | hasResultType ty = isPrintable $ getResultType ty
  | isTupleType ty   = all isPrintable $ getArgTypes ty
  | otherwise        = True

typeSynonym :: String -> [Type] -> Type -> Type
typeSynonym name parameters resolution =
  TypeSynonym{refInfo = RefInfo{refId = name, parameters}, resolvesTo = resolution}

typeSynonymLHS :: Type -> RefInfo
typeSynonymLHS TypeSynonym{refInfo} = refInfo
typeSynonymLHS _ = error $ "Types.hs: Expected type synonym"

typeSynonymRHS :: Type -> Type
typeSynonymRHS TypeSynonym{resolvesTo} = resolvesTo
typeSynonymRHS _ = error $ "Types.hs: Expected type synonymm"

typeSynonymSetRHS :: Type -> Type -> Type
typeSynonymSetRHS t@TypeSynonym{} rhs = t{resolvesTo = rhs}
typeSynonymSetRHS _ _ = error $ "Types.hs: Expected type synonymm"

isTypeSynonym TypeSynonym{} = True
isTypeSynonym _ = False

unfoldTypeSynonyms :: Type -> Type
unfoldTypeSynonyms = typeMap unfoldSingleSynonym
     where
       unfoldSingleSynonym TypeSynonym{resolvesTo = t} = t
       unfoldSingleSynonym t = t