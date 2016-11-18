{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Types(
             Type
            ,Activity (..)
            ,arrowType
            ,arrowWithTypeParam
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
            ,classType
            ,isRefAtomType
            ,isRefType
            ,isTraitType
            ,isActiveClassType
            ,isSharedClassType
            ,isPassiveClassType
            ,isClassType
            ,isMainType
            ,stringObjectType
            ,isStringObjectType
            ,conjunctiveType
            ,disjunctiveType
            ,isCapabilityType
            ,incapability
            ,isIncapability
            ,unionType
            ,isUnionType
            ,unionMembers
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
            ,uintType
            ,isUIntType
            ,realType
            ,isRealType
            ,charType
            ,isCharType
            ,stringType
            ,isStringType
            ,isPrimitive
            ,isNumeric
            ,getTypeParams
            ,getArgTypes
            ,setArgTypes
            ,getResultType
            ,getId
            ,maybeGetId
            ,getRefNamespace
            ,setRefNamespace
            ,getRefSourceFile
            ,setRefSourceFile
            ,translateTypeNamespace
            ,getTypeParameters
            ,setTypeParameters
            ,conjunctiveTypesFromCapability
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
            ,getTypeParameterBindings
            ) where

import Identifiers

import Data.List
import Data.Maybe
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import Data.Traversable
import Text.Parsec.Pos as P

import Debug.Trace

data Activity = Active
              | Shared
              | Passive
                deriving(Eq, Show)

data TypeOp = Product | Addition
  deriving (Eq)

instance Show TypeOp where
  show Product = "*"
  show Addition = "+"

data RefInfo = RefInfo{refId         :: String
                      ,parameters    :: [Type]
                      ,refNamespace  :: Maybe Namespace
                      ,refSourceFile :: Maybe SourceName
                      } deriving(Eq)

instance Show RefInfo where
    show RefInfo{refId, parameters, refNamespace}
        | null parameters = fullName refNamespace refId
        | otherwise = fullName refNamespace refId ++ "<" ++ params ++ ">"
        where
          fullName Nothing refId = refId
          fullName (Just ns) refId =
              if null ns
              then refId
              else intercalate "." (map show ns) ++ "." ++ refId
          params = intercalate ", " (map show parameters)

data Type = Unresolved{refInfo :: RefInfo}
          | TraitType{refInfo :: RefInfo}
          | ClassType{refInfo :: RefInfo
                     ,activity   :: Activity
                     }
          | CapabilityType{typeop :: TypeOp
                          ,ltype  :: Type
                          ,rtype  :: Type}
          | UnionType{ltype :: Type, rtype :: Type}
          | EmptyCapability{}
          | TypeVar{ident :: String}
          | ArrowType{paramTypes :: [Type]
                     ,argTypes   :: [Type]
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
          | UIntType
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

getTypeParams = paramTypes

getArgTypes = argTypes
setArgTypes ty argTypes = ty{argTypes}

getResultType ty
    | hasResultType ty = resultType ty
    | otherwise = error $ "Types.hs: tried to get the resultType of " ++ show ty

getId ty = case maybeGetId ty of
     Nothing -> error $ "Types.hs: Tried to get the ID of " ++ showWithKind ty
     Just t -> t

getRefNamespace ty
    | isRefAtomType ty || isTypeSynonym ty = refNamespace (refInfo ty)
    | otherwise = error $ "Types.hs: tried to get the namespace of " ++ show ty

setRefNamespace ns ty
    | isRefAtomType ty || isTypeSynonym ty
    , info <- refInfo ty = ty{refInfo = info{refNamespace = Just ns}}
    | otherwise = error $ "Types.hs: tried to set the namespace of " ++ show ty

getRefSourceFile ty
    | isRefAtomType ty || isTypeSynonym ty =
        fromMaybe err $ refSourceFile (refInfo ty)
    | otherwise = error $ "Types.hs: tried to get the sourcefile of " ++ showWithKind ty
    where err = error "Types.hs: type without sourceFile: " ++ showWithKind ty

setRefSourceFile file ty
    | isRefAtomType ty || isTypeSynonym ty
    , info <- refInfo ty = ty{refInfo = info{refSourceFile = Just file}}
    | otherwise = error $ "Types.hs: tried to set the source of " ++ show ty

hasRefSourceFile ty
    | isRefAtomType ty || isTypeSynonym ty
    , info <- refInfo ty = isJust $ refSourceFile info
    | otherwise = False

translateTypeNamespace :: Map SourceName Namespace -> Type -> Type
translateTypeNamespace table = typeMap translate
    where
      translate ty
        | hasRefSourceFile ty =
            let source = getRefSourceFile ty
                ns = table Map.! source
            in setRefNamespace ns ty
        | otherwise = ty

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
    show CapabilityType{typeop = Product, ltype, rtype} =
        let lhs = if isDisjunction ltype
                  then "(" ++ show ltype ++ ")"
                  else show ltype
            rhs = if isDisjunction rtype
                  then "(" ++ show rtype ++ ")"
                  else show rtype
        in lhs ++ " " ++ show Product ++ " " ++ rhs
    show CapabilityType{typeop = Addition, ltype, rtype} =
        show ltype ++ " " ++ show Addition ++ " " ++ show rtype
    show UnionType{ltype, rtype} = show ltype ++ " | " ++ show rtype
    show EmptyCapability = ""
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
    show TypeSynonym{refInfo, resolvesTo} = show refInfo
    show VoidType   = "void"
    show StringType = "string"
    show CharType   = "char"
    show IntType    = "int"
    show UIntType   = "uint"
    show RealType   = "real"
    show BoolType   = "bool"
    show NullType   = "null type"
    show BottomType = "Bottom"

maybeParen :: Type -> String
maybeParen arr@(ArrowType{}) = "(" ++ show arr ++ ")"
maybeParen fut@(FutureType{})  = "(" ++ show fut ++ ")"
maybeParen par@(ParType{})     = "(" ++ show par ++ ")"
maybeParen str@(StreamType{})  = "(" ++ show str ++ ")"
maybeParen arr@(ArrayType{})   = "(" ++ show arr ++ ")"
maybeParen opt@(MaybeType{})   = "(" ++ show opt ++ ")"
maybeParen cap@(CapabilityType{}) = "(" ++ show cap ++ ")"
maybeParen inter@(UnionType{}) = "(" ++ show inter ++ ")"
maybeParen ty = show ty

showWithKind :: Type -> String
showWithKind ty = kind ty ++ " " ++ show ty
    where
    kind VoidType                      = "primitive type"
    kind StringType                    = "primitive type"
    kind CharType                      = "primitive type"
    kind IntType                       = "primitive type"
    kind UIntType                      = "primitive type"
    kind RealType                      = "primitive type"
    kind BoolType                      = "primitive type"
    kind Unresolved{}                  = "unresolved type"
    kind TraitType{}                   = "trait type"
    kind ClassType{activity = Active}  = "active class type"
    kind ClassType{activity = Passive} = "passive class type"
    kind CapabilityType{}              = "capability type"
    kind EmptyCapability{}             = "the empty capability type"
    kind UnionType{}                   = "union type"
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
    areBoth isArrowType ||
    areBoth isRangeType ||
    areBoth isCType = True
  | otherwise = False
  where
    isBottomTy1 = isBottomType ty1
    isBottomTy2 = isBottomType ty2
    areBoth typeFun = typeFun ty1 && typeFun ty2

typeComponents :: Type -> [Type]
typeComponents arrow@(ArrowType typeParams argTys ty) =
    arrow : (concatMap typeComponents typeParams ++
             concatMap typeComponents argTys ++
             typeComponents ty)
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
typeComponents cap@(CapabilityType{ltype, rtype}) =
    cap : typeComponents ltype ++ typeComponents rtype
typeComponents ty@(UnionType{ltype, rtype}) =
    ty : typeComponents ltype ++ typeComponents rtype
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

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty@Unresolved{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@TraitType{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@ClassType{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@CapabilityType{ltype, rtype} =
    f ty{ltype = typeMap f ltype
        ,rtype = typeMap f rtype}
typeMap f ty@UnionType{ltype, rtype} =
    f ty{ltype = typeMap f ltype
        ,rtype = typeMap f rtype}
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
typeMapM f ty@CapabilityType{ltype, rtype} = do
  ltype' <- typeMapM f ltype
  rtype' <- typeMapM f rtype
  f ty{ltype = ltype', rtype = rtype'}
typeMapM f ty@UnionType{ltype, rtype} = do
  ltype' <- typeMapM f ltype
  rtype' <- typeMapM f rtype
  f ty{ltype = ltype', rtype = rtype'}
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

getTypeParameters :: Type -> [Type]
getTypeParameters Unresolved{refInfo} = parameters refInfo
getTypeParameters TraitType{refInfo} = parameters refInfo
getTypeParameters ClassType{refInfo} = parameters refInfo
getTypeParameters TypeSynonym{refInfo} = parameters refInfo
getTypeParameters ArrowType{paramTypes} = paramTypes
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
    in ty{refInfo = refInfo{parameters = params}, resolvesTo=replaceTypeVars subst resolvesTo}
setTypeParameters ty@ArrowType{} paramTypes =
    ty{paramTypes}
setTypeParameters ty _ =
    error $ "Types.hs: Can't set type parameters of type " ++ show ty

getTypeParameterBindings :: [(Type, Type)] -> [(Type, Type)]
getTypeParameterBindings [] = []
getTypeParameterBindings (binding:ps) = findRootType binding ++
                                        getTypeParameterBindings ps
  where
    findRootType (formal, actual)=
      nub . filter (\(f, _) -> isTypeVar f) $
      zip (typeComponents formal) (typeComponents actual)


conjunctiveTypesFromCapability :: Type -> [([Type], [Type])]
conjunctiveTypesFromCapability ty
    | isConjunction ty
    , ltype <- ltype ty
    , rtype <- rtype ty =
        (typesFromCapability ltype, typesFromCapability rtype) :
        conjunctiveTypesFromCapability ltype ++
        conjunctiveTypesFromCapability rtype
    | isDisjunction ty
    , ltype <- ltype ty
    , rtype <- rtype ty =
        conjunctiveTypesFromCapability ltype ++
        conjunctiveTypesFromCapability rtype
    | otherwise = []

typesFromCapability :: Type -> [Type]
typesFromCapability CapabilityType{ltype, rtype} =
    typesFromCapability ltype ++ typesFromCapability rtype
typesFromCapability EmptyCapability{} = []
typesFromCapability ty = [ty]

refTypeWithParams refId parameters =
    Unresolved{refInfo = RefInfo{refId
                                ,parameters
                                ,refNamespace = Nothing
                                ,refSourceFile = Nothing
                                }
              }

refType :: String -> Type
refType id = refTypeWithParams id []

classType :: Activity -> String -> [Type] -> Type
classType activity name parameters =
  ClassType{refInfo = RefInfo{refId = name
                             ,parameters
                             ,refNamespace = Nothing
                             ,refSourceFile = Nothing
                             }
           ,activity}

traitTypeFromRefType Unresolved{refInfo} =
    TraitType{refInfo}
traitTypeFromRefType ty =
    error $ "Types.hs: Can't make trait type from type: " ++ show ty

isRefAtomType Unresolved {} = True
isRefAtomType TraitType {} = True
isRefAtomType ClassType {} = True
isRefAtomType _ = False

isRefType ty
    | isUnionType ty =
        all isRefType (unionMembers ty)
    | otherwise =
        isRefAtomType ty ||
        isCapabilityType ty

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

disjunctiveType ltype rtype = CapabilityType{typeop = Addition, ltype, rtype}
conjunctiveType ltype rtype = CapabilityType{typeop = Product, ltype, rtype}

isCapabilityType CapabilityType{} = True
isCapabilityType TraitType{} = True
isCapabilityType EmptyCapability{} = True
isCapabilityType _ = False

isDisjunction CapabilityType{typeop = Addition} = True
isDisjunction _ = False

isConjunction CapabilityType{typeop = Product} = True
isConjunction _ = False

incapability :: Type
incapability = EmptyCapability

isIncapability EmptyCapability = True
isIncapability _ = False

unionType = UnionType
isUnionType UnionType{} = True
isUnionType _ = False

unionMembers UnionType{ltype, rtype} =
    unionMembers ltype ++ unionMembers rtype
unionMembers ty = [ty]

arrowType :: [Type] -> Type -> Type
arrowType = ArrowType []
arrowWithTypeParam = ArrowType

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

stringObjectType = setRefSourceFile "String.enc" $
                    classType Passive "String" []

isStringObjectType ty = isPassiveClassType ty &&
                        getId ty == "String"

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where replace ty = fromMaybe ty (lookup ty bindings)

ctype :: String -> Type
ctype = CType

isCType :: Type -> Bool
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

uintType :: Type
uintType = UIntType

isUIntType :: Type -> Bool
isUIntType = (== uintType)

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
primitives = [voidType
             ,intType
             ,uintType
             ,realType
             ,boolType
             ,stringType
             ,charType]

isPrimitive :: Type -> Bool
isPrimitive = (`elem` primitives)

isNumeric :: Type -> Bool
isNumeric ty = isRealType ty || isIntType ty || isUIntType ty

isPrintable :: Type -> Bool
isPrintable ty
  | isTypeVar ty     = False
  | isCType ty       = False
  | hasResultType ty = isPrintable $ getResultType ty
  | isTupleType ty   = all isPrintable $ getArgTypes ty
  | otherwise        = True

typeSynonym :: String -> [Type] -> Type -> Type
typeSynonym name parameters resolution =
  TypeSynonym{refInfo = RefInfo{refId = name
                               ,parameters
                               ,refNamespace = Nothing
                               ,refSourceFile = Nothing
                               }
             ,resolvesTo = resolution}

typeSynonymLHS :: Type -> (String, [Type])
typeSynonymLHS TypeSynonym{refInfo = RefInfo{refId = name, parameters}} =
                   (name, parameters)
typeSynonymLHS ty = error "Types.hs: Expected type synonym"

typeSynonymRHS :: Type -> Type
typeSynonymRHS TypeSynonym{resolvesTo} = resolvesTo
typeSynonymRHS ty = error "Types.hs: Expected type synonym"

typeSynonymSetRHS :: Type -> Type -> Type
typeSynonymSetRHS t@TypeSynonym{} rhs = t{resolvesTo = rhs}
typeSynonymSetRHS ty _ = error "Types.hs: Expected type synonym"

isTypeSynonym TypeSynonym{} = True
isTypeSynonym _ = False

unfoldTypeSynonyms :: Type -> Type
unfoldTypeSynonyms = typeMap unfoldSingleSynonym

unfoldSingleSynonym :: Type -> Type
unfoldSingleSynonym TypeSynonym{resolvesTo = t} = t
unfoldSingleSynonym t = t
