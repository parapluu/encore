{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Types(
             Type
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
            ,abstractTraitFromTraitType
            ,classType
            ,isRefAtomType
            ,traitType
            ,isRefType
            ,isTraitType
            ,isAbstractTraitType
            ,isClassType
            ,isPassiveClassType
            ,isMainType
            ,stringObjectType
            ,isStringObjectType
            ,conjunctiveType
            ,isConjunctiveType
            ,isDisjunctiveType
            ,getTypeOperands
            ,disjunctiveType
            ,isCapabilityType
            ,isCompositeType
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
            ,unitType
            ,isUnitType
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
            ,withModeOf
            ,withBoxOf
            ,unbox
            ,typeComponents
            ,typeMap
            ,typeMapM
            ,showWithKind
            ,hasSameKind
            ,maybeType
            ,isMaybeType
            ,tupleType
            ,isTupleType
            ,tupleLength
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
            ,isPassiveRefType
            ,showModeOf
            ,showWithoutMode
            ,isModeless
            ,hasMinorMode
            ,modeSubtypeOf
            ,hasSafeMode
            ,safeToComposeWith
            ,makeUnsafe
            ,makeLinear
            ,makeLocal
            ,makeActive
            ,makeRead
            ,makeSubordinate
            ,isLinearRefType
            ,isLinearArrowType
            ,isLocalRefType
            ,isLocalArrowType
            ,isActiveRefType
            ,isSharedRefType
            ,isReadRefType
            ,isSubordinateRefType
            ,isSubordinateArrowType
            ,isUnsafeRefType
            ,makeStackbound
            ,isStackboundType
            ) where

import Identifiers

import Data.List
import Data.Maybe
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import Data.Traversable
import Control.Monad

import Debug.Trace

data TypeOp = Product | Addition
  deriving (Eq)

instance Show TypeOp where
  show Product = "*"
  show Addition = "+"

data Mode = Linear
          | Local
          | Active
          | Shared
          | Unsafe
          | Read
          | Subordinate
            deriving(Eq)

instance Show Mode where
    show Linear = "linear"
    show Local  = "local"
    show Active = "active"
    show Shared = "shared"
    show Unsafe = "unsafe"
    show Read   = "read"
    show Subordinate = "subord"

isMinorMode :: Mode -> Bool
isMinorMode Read = True
isMinorMode Subordinate = True
isMinorMode _ = False

hasMinorMode :: Type -> Bool
hasMinorMode ty
  | isModeless ty = True
  | otherwise = isMinorMode . fromJust $ getMode ty

modeSubtypeOf ty1 ty2
  | isArrowType ty1 && isArrowType ty2 =
      let modes1 = modes $ inner ty1
          modes2 = modes $ inner ty2
      in
        all (`elem` modes2) modes1
  | otherwise = getMode ty1 == getMode ty2

modeIsSafe Read   = True
modeIsSafe Active = True
modeIsSafe Shared = True
modeIsSafe _      = False

hasSafeMode ty
  | Just mode <- getMode ty = modeIsSafe mode
  | otherwise = False

-- | Is @class cls : trait@ a valid composition?
safeToComposeWith :: Type -> Type -> Bool
safeToComposeWith cls trait
  | isModeless cls     = True
  | isModeless trait   = True
  | otherwise = trait `modeSubtypeOf` cls

data RefInfo = RefInfo{refId         :: String
                      ,parameters    :: [Type]
                      ,mode          :: Maybe Mode
                      ,refNamespace  :: Maybe Namespace
                      ,refSourceFile :: Maybe FilePath
                      }

-- The current modes are irrelevant for equality checks
instance Eq RefInfo where
    ref1 == ref2 = refId ref1 == refId ref2 &&
                   parameters ref1 == parameters ref2 &&
                   refSourceFile ref1 == refSourceFile ref2

instance Show RefInfo where
    show RefInfo{mode, refId, parameters, refNamespace}
        | null parameters = smode ++ fullName refNamespace refId
        | otherwise =
            smode ++ fullName refNamespace refId ++ "[" ++ params ++ "]"
        where
          smode
              | isNothing mode = ""
              | otherwise = show (fromJust mode) ++ " "
          params = intercalate ", " (map show parameters)
          fullName Nothing refId = refId
          fullName (Just ns) refId =
              if isEmptyNamespace ns
              then refId
              else show ns ++ "." ++ refId


showRefInfoWithoutMode info = show info{mode = Nothing}

data Box = Stackbound deriving(Eq)

instance Show Box where
    show Stackbound = "borrowed"

data Type = Type {inner :: InnerType
                 ,box   :: Maybe Box}

typ ity = Type{inner = ity, box = Nothing}

instance Eq Type where
    ty1 == ty2 = inner ty1 == inner ty2

instance Show Type where
    show Type{inner, box = Nothing} = show inner
    show Type{inner, box = Just s} =
        show s ++ " " ++ maybeParen (typ inner)

data InnerType =
          Unresolved{refInfo :: RefInfo}
        | TraitType{refInfo :: RefInfo}
          -- | The @AbstractTraitType@ is used to type @this@ when
          -- overriding methods. A class that implements a trait
          -- @T@ and overrides one of its methods @m@ will
          -- typecheck @m@ with @this@ as the *abstract* trait
          -- @T@. This is to be able to distinguish e.g. method
          -- parameters that have the type @T@ from the included
          -- trait @T@, which might have been extended with
          -- additional attributes.
        | AbstractTraitType{refInfo :: RefInfo}
        | ClassType{refInfo :: RefInfo}
        | CapabilityType{typeop :: TypeOp
                        ,ltype  :: Type
                        ,rtype  :: Type}
        | UnionType{ltype :: Type, rtype :: Type}
        | EmptyCapability{}
        | TypeVar{ident :: String}
        | ArrowType{paramTypes :: [Type]
                   ,argTypes   :: [Type]
                   ,resultType :: Type
                   ,modes :: [Mode]
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
        | UnitType
        | StringType
        | CharType
        | IntType
        | UIntType
        | BoolType
        | RealType
        | NullType
        | BottomType
          deriving(Eq)

getArgTypes = argTypes . inner
setArgTypes ty argTypes = ty{inner = iType{argTypes}}
    where
      iType = inner ty
getResultType ty
    | hasResultType ty = resultType . inner $ ty
    | otherwise = error $ "Types.hs: tried to get the resultType of " ++ show ty
getId ty =
    fromMaybe
      (error $ "Types.hs: Tried to get the ID of " ++ showWithKind ty)
      (maybeGetId ty)

maybeGetId Type{inner = Unresolved{refInfo}} = Just $ refId refInfo
maybeGetId Type{inner = TraitType{refInfo}} = Just $ refId refInfo
maybeGetId Type{inner = AbstractTraitType{refInfo}} = Just $ refId refInfo
maybeGetId Type{inner = ClassType{refInfo}} = Just $ refId refInfo
maybeGetId Type{inner = TypeSynonym{refInfo}} = Just $ refId refInfo
maybeGetId Type{inner = TypeVar{ident}} = Just ident
maybeGetId Type{inner = CType{ident}} = Just ident
maybeGetId _ = Nothing

getMode ty
    |  isRefAtomType ty
    || isTypeSynonym ty = mode . refInfo . inner $ ty
    | otherwise = Nothing

hasResultType x
  | isArrowType x || isFutureType x || isParType x ||
    isStreamType x || isArrayType x || isMaybeType x = True
  | otherwise = False

getRefNamespace ty
    | isRefAtomType ty || isTypeSynonym ty = refNamespace (refInfo $ inner ty)
    | otherwise = error $ "Types.hs: tried to get the namespace of " ++ show ty

setRefNamespace ns ty
    | isRefAtomType ty || isTypeSynonym ty
    , iType <- inner ty
    , info <- refInfo iType =
        ty{inner = iType{refInfo = info{refNamespace = Just ns}}}
    | otherwise = error $ "Types.hs: tried to set the namespace of " ++ show ty

getRefSourceFile ty
    | isRefAtomType ty || isTypeSynonym ty =
        fromMaybe err $ refSourceFile (refInfo $ inner ty)
    | otherwise = error $ "Types.hs: tried to get the sourcefile of " ++ showWithKind ty
    where err = error "Types.hs: type without sourceFile: " ++ showWithKind ty

setRefSourceFile file ty
    | isRefAtomType ty || isTypeSynonym ty
    , iType <- inner ty
    , info <- refInfo iType =
        ty{inner = iType{refInfo = info{refSourceFile = Just file}}}
    | otherwise = error $ "Types.hs: tried to set the source of " ++ show ty

hasRefSourceFile ty
    | isRefAtomType ty || isTypeSynonym ty
    , info <- refInfo $ inner ty = isJust $ refSourceFile info
    | otherwise = False

translateTypeNamespace :: Map FilePath Namespace -> Type -> Type
translateTypeNamespace table = typeMap translate
    where
      translate ty
        | hasRefSourceFile ty =
            let source = getRefSourceFile ty
            in case Map.lookup source table of
                 Just ns -> setRefNamespace ns ty
                 Nothing ->
                   error $ "Types.hs: Type '" ++ show ty ++
                           "' has unknown source '" ++ show source ++ "'"
        | otherwise = ty

setResultType ty res
  | hasResultType ty = ty{inner = iType{resultType = res}}
  | otherwise = error $ "Types.hs: tried to set the resultType of " ++ show ty
  where
    iType = inner ty

instance Show InnerType where
    show Unresolved{refInfo} = show refInfo
    show TraitType{refInfo} = show refInfo
    show AbstractTraitType{refInfo} = show refInfo
    show ClassType{refInfo} = showRefInfoWithoutMode refInfo
    show CapabilityType{typeop = Product, ltype, rtype} =
        let lhs = if isDisjunctiveType ltype
                  then "(" ++ show ltype ++ ")"
                  else show ltype
            rhs = if isDisjunctiveType rtype
                  then "(" ++ show rtype ++ ")"
                  else show rtype
        in lhs ++ " " ++ show Product ++ " " ++ rhs
    show CapabilityType{typeop = Addition, ltype, rtype} =
        show ltype ++ " " ++ show Addition ++ " " ++ show rtype
    show UnionType{ltype, rtype} = show ltype ++ " | " ++ show rtype
    show EmptyCapability = ""
    show TypeVar{ident} = ident
    show ArrowType{argTypes = [ty], resultType, modes = []} =
        if isTupleType ty
        then "(" ++ show ty ++ ") -> " ++ show resultType
        else show ty ++ " -> " ++ show resultType
    show ArrowType{argTypes, resultType, modes = []} =
        "(" ++ args ++ ") -> " ++ show resultType
        where
          args = intercalate ", " (map show argTypes)
    show arrow@ArrowType{modes} =
        unwords (map show modes) ++
        " (" ++ show arrow{modes = []} ++ ")"
    show FutureType{resultType} = "Fut" ++ brackets resultType
    show ParType{resultType}    = "Par" ++ brackets resultType
    show StreamType{resultType} = "Stream" ++ brackets resultType
    show ArrayType{resultType}  = brackets resultType
    show RangeType   = "Range"
    show (MaybeType ty)    = "Maybe" ++ brackets ty
    show (TupleType{argTypes}) = "(" ++ args ++ ")"
      where
        args = intercalate ", " (map show argTypes)
    show (CType ty) = "EMBED " ++ ty ++ " END"
    show TypeSynonym{refInfo, resolvesTo} = show refInfo
    show UnitType   = "unit"
    show StringType = "string"
    show CharType   = "char"
    show IntType    = "int"
    show UIntType   = "uint"
    show RealType   = "real"
    show BoolType   = "bool"
    show NullType   = "null type"
    show BottomType = "Bottom"

brackets ty = "[" ++ show ty ++ "]"

maybeParen :: Type -> String
maybeParen ty
    | isArrowType ty  ||
      isCapabilityType ty ||
      isUnionType ty  ||
      isFutureType ty ||
      isParType ty    ||
      isMaybeType ty  ||
      isStreamType ty = "(" ++ show ty ++ ")"
    | otherwise = show ty

showWithKind :: Type -> String
showWithKind ty = kind (inner ty) ++ " " ++ show ty
    where
    kind UnitType                      = "primitive type"
    kind StringType                    = "primitive type"
    kind CharType                      = "primitive type"
    kind IntType                       = "primitive type"
    kind UIntType                      = "primitive type"
    kind RealType                      = "primitive type"
    kind BoolType                      = "primitive type"
    kind Unresolved{}                  = "unresolved type"
    kind TraitType{}                   = "trait type"
    kind AbstractTraitType{}           = "abstract trait type"
    kind ty@ClassType{}                = if isActiveRefType (typ ty)
                                         then "active class type"
                                         else if isSharedRefType (typ ty)
                                         then "shared class type"
                                         else "class type"
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

showModeOf :: Type -> String
showModeOf ty
  | isModeless ty =
      error $ "Types.hs: Cannot show mode of " ++ showWithKind ty
  | otherwise = show . fromJust $ getMode ty

showWithoutMode :: Type -> String
showWithoutMode ty
  | isRefAtomType ty = showRefInfoWithoutMode $ refInfo (inner ty)
  | otherwise = show ty

typeComponents :: Type -> [Type]
typeComponents ty
    | isArrowType ty =
        ty : concatMap typeComponents (getTypeParameters ty) ++
             concatMap typeComponents (getArgTypes ty) ++
             typeComponents (getResultType ty)
    | hasResultType ty =
        ty : typeComponents (getResultType ty)
    | isTupleType ty =
        ty : concatMap typeComponents (getArgTypes ty)
    |  isRefAtomType ty
    || isTypeSynonym ty =
        ty : refInfoTypeComponents (refInfo iType)
    | isCompositeType ty || isUnionType ty =
        ty : typeComponents (ltype iType) ++ typeComponents (rtype iType)
    | otherwise = [ty]
    where
      iType = inner ty

      refInfoTypeComponents = concatMap typeComponents . parameters

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty
    | isRefAtomType ty =
        f ty{inner = refTypeMap f iType}
    | isCompositeType ty || isUnionType ty =
        f ty{inner = iType{ltype = typeMap f (ltype iType)
                          ,rtype = typeMap f (rtype iType)}}
    | isArrowType ty =
        f ty{inner = resultTypeMap f .
                     argTypesMap f .
                     typeParamMap f $ iType}
    | hasResultType ty =
        f ty{inner = resultTypeMap f iType}
    | isTupleType ty =
        f ty{inner = argTypesMap f iType}
    | isTypeSynonym ty =
        f ty{inner = refTypeMap f
                     iType{resolvesTo = typeMap f (resolvesTo iType)}
            }
    | otherwise = f ty
    where
      iType = inner ty

      refTypeMap f ity =
          ity{refInfo = refInfoTypeMap f (refInfo ity)}

      resultTypeMap f ity =
          ity{resultType = typeMap f (resultType ity)}

      argTypesMap f ity =
          ity{argTypes = map (typeMap f) (argTypes ity)}

      typeParamMap f ity =
          ity{paramTypes = map (typeMap f) (paramTypes ity)}

      refInfoTypeMap f info@RefInfo{parameters} =
          info{parameters = map (typeMap f) parameters}

typeMapM :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapM f ty
    | isRefAtomType ty = do
        iType' <- refTypeMapM f iType
        f ty{inner = iType'}
    | isCompositeType ty || isUnionType ty = do
        ltype' <- typeMapM f (ltype iType)
        rtype' <- typeMapM f (rtype iType)
        let iType' = iType{ltype = ltype', rtype = rtype'}
        f ty{inner = iType'}
    | isArrowType ty = do
        iType' <- argTypesMapM f iType >>=
                  typeParamMapM f >>=
                  resultTypeMapM f
        f ty{inner = iType'}
    | hasResultType ty = do
        iType' <- resultTypeMapM f iType
        f ty{inner = iType'}
    | isTupleType ty = do
        iType' <- argTypesMapM f iType
        f ty{inner = iType'}
    | isTypeSynonym ty = do
        resolvesTo' <- typeMapM f (resolvesTo iType)
        iType' <- refTypeMapM f iType{resolvesTo = resolvesTo'}
        f ty{inner = iType'}
    | otherwise = f ty
    where
      iType = inner ty

      refTypeMapM f ity = do
        refInfo' <- refInfoTypeMapM f (refInfo ity)
        return ity{refInfo = refInfo'}

      refInfoTypeMapM f info@RefInfo{parameters} = do
        parameters' <- mapM (typeMapM f) parameters
        return info{parameters = parameters'}

      argTypesMapM f ity = do
        argTypes' <- mapM (typeMapM f) (argTypes ity)
        return ity{argTypes = argTypes'}

      typeParamMapM f ity = do
        paramTypes' <- mapM (typeMapM f) (paramTypes ity)
        return ity{paramTypes = paramTypes'}

      resultTypeMapM f ity = do
        resultType' <- typeMapM f $ resultType ity
        return ity{resultType = resultType'}

getTypeParameters :: Type -> [Type]
getTypeParameters ty
    |  isRefAtomType ty
    || isTypeSynonym ty = parameters $ refInfo (inner ty)
    | isArrowType ty =
        paramTypes (inner ty)
    | otherwise =
        error $ "Types.hs: Can't get type parameters from type " ++ show ty

setTypeParameters :: Type -> [Type] -> Type
setTypeParameters ty params
    | isRefAtomType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{parameters = params}}}
    | isTypeSynonym ty
    , iType <- inner ty
    , info <- refInfo iType
      = let subst = zip (parameters info) params
            resolvesTo' = replaceTypeVars subst (resolvesTo iType)
        in ty{inner = iType{refInfo = info{parameters = params}
                           ,resolvesTo = resolvesTo'}}
    | isArrowType ty
    , iType <- inner ty
      = ty{inner = iType{paramTypes = params}}
    | otherwise =
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
    | isConjunctiveType ty
    , ltype <- ltype . inner $ ty
    , rtype <- rtype . inner $ ty =
        (typesFromCapability ltype, typesFromCapability rtype) :
        conjunctiveTypesFromCapability ltype ++
        conjunctiveTypesFromCapability rtype
    | isDisjunctiveType ty
    , ltype <- ltype . inner $ ty
    , rtype <- rtype . inner $ ty =
        conjunctiveTypesFromCapability ltype ++
        conjunctiveTypesFromCapability rtype
    | otherwise = []

typesFromCapability :: Type -> [Type]
typesFromCapability Type{inner = CapabilityType{ltype, rtype}} =
    typesFromCapability ltype ++ typesFromCapability rtype
typesFromCapability Type{inner = EmptyCapability{}} = []
typesFromCapability ty = [ty]

withModeOf sink source
    | isRefAtomType sink || isTypeSynonym sink
    , isRefAtomType source
    , iType <- inner sink
    , info <- refInfo iType
    , mode <- mode $ refInfo (inner source)
      = sink{inner = iType{refInfo = info{mode}}}
    | otherwise =
        error $ "Types.hs: Can't transfer modes from " ++
                showWithKind source ++ " to " ++ showWithKind sink

withBoxOf sink source = sink{box = box source}

unbox ty = ty{box = Nothing}

refTypeWithParams refId parameters =
    typ Unresolved{refInfo}
    where
      refInfo = RefInfo{refId
                       ,parameters
                       ,mode = Nothing
                       ,refNamespace = Nothing
                       ,refSourceFile = Nothing
                       }

refType id = refTypeWithParams id []

classType :: String -> [Type] -> Type
classType name parameters =
  Type{inner = ClassType{refInfo = RefInfo{refId = name
                                          ,parameters
                                          ,mode = Nothing
                                          ,refNamespace = Nothing
                                          ,refSourceFile = Nothing
                                          }
                        }
      ,box = Nothing
      }

traitType :: String -> [Type] -> Type
traitType name parameters =
    Type{inner = TraitType{refInfo = RefInfo{refId = name
                                            ,parameters
                                            ,mode = Nothing
                                            ,refNamespace = Nothing
                                            ,refSourceFile = Nothing
                                            }
                          }
        ,box = Nothing
        }

-- | Calling @abstractTraitFromTraitType ty@ returns the *trait
-- type* @ty@ as an *abstract* trait type. See the definition of
-- @AbstractTraitType@ for more details.
abstractTraitFromTraitType ty@Type{inner = TraitType{refInfo}} =
  ty{inner = AbstractTraitType{refInfo}}
abstractTraitFromTraitType ty@Type{inner = AbstractTraitType{}} = ty
abstractTraitFromTraitType ty =
  error $ "Types.hs: Can't form abstract trait from " ++ showWithKind ty

isRefAtomType Type{inner = Unresolved {}} = True
isRefAtomType Type{inner = TraitType {}} = True
isRefAtomType Type{inner = AbstractTraitType {}} = True
isRefAtomType Type{inner = ClassType {}} = True
isRefAtomType _ = False

isRefType ty
    | isUnionType ty =
        all isRefType (unionMembers ty)
    | otherwise =
        isRefAtomType ty ||
        isCapabilityType ty

isUnresolved Type{inner = Unresolved{}} = True
isUnresolved _ = False

isTraitType Type{inner = TraitType{}} = True
isTraitType Type{inner = AbstractTraitType{}} = True
isTraitType _ = False

isAbstractTraitType Type{inner = AbstractTraitType{}} = True
isAbstractTraitType _ = False

isPassiveRefType ty = isRefType ty && not (isActiveRefType ty) &&
                                      not (isSharedRefType ty)

isClassType Type{inner = ClassType{}} = True
isClassType _ = False

isPassiveClassType ty = isPassiveRefType ty && isClassType ty

disjunctiveType ltype rtype =
    typ CapabilityType{typeop = Addition, ltype, rtype}
conjunctiveType ltype rtype =
    typ CapabilityType{typeop = Product, ltype, rtype}

setMode ty m
    | isRefAtomType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just m}}}
    | isArrowType ty
    , iType <- inner ty
    , modes <- modes iType
      = ty{inner = iType{modes = nub $ m:modes}}
    | otherwise = error $ "Types.hs: Cannot set mode of " ++ showWithKind ty

makeUnsafe ty = setMode ty Unsafe
makeLinear ty = setMode ty Linear
makeLocal ty = setMode ty Local
makeActive ty = setMode ty Active
makeShared ty = setMode ty Shared
makeRead ty = setMode ty Read
makeSubordinate ty = setMode ty Subordinate

isModeless ty
    | Just m <- getMode ty = False
    | otherwise = True

isLinearRefType ty
    | Just Linear <- getMode ty = True
    | otherwise = False

isLinearArrowType ty
    | isArrowType ty = Linear `elem` modes (inner ty)
    | otherwise = False

isLocalRefType ty
    | Just Local <- getMode ty = True
    | otherwise = False

isLocalArrowType ty
    | isArrowType ty = Local `elem` modes (inner ty)
    | otherwise = False

isActiveRefType ty
    | Just Active <- getMode ty = True
    | otherwise = False

isSharedRefType ty
    | Just Shared <- getMode ty = True
    | otherwise = False

isReadRefType ty
    | Just Read <- getMode ty = True
    | otherwise = False

isSubordinateRefType ty
    | Just Subordinate <- getMode ty = True
    | otherwise = False

isSubordinateArrowType ty
    | isArrowType ty = Subordinate `elem` modes (inner ty)
    | otherwise = False

isUnsafeRefType ty
    | Just Unsafe <- getMode ty = True
    | otherwise = False

isCapabilityType Type{inner = CapabilityType{}} = True
isCapabilityType Type{inner = TraitType{}} = True
isCapabilityType Type{inner = AbstractTraitType{}} = True
isCapabilityType Type{inner = EmptyCapability{}} = True
isCapabilityType _ = False

isDisjunctiveType Type{inner = CapabilityType{typeop = Addition}} = True
isDisjunctiveType _ = False

isConjunctiveType Type{inner = CapabilityType{typeop = Product}} = True
isConjunctiveType _ = False

isComposite ty = isDisjunctiveType ty || isConjunctiveType ty

getTypeOperands Type{inner = CapabilityType{ltype, rtype}} = (ltype, rtype)
getTypeOperands ty =
  error $ "Types.hs: Cannot get type operands of " ++ showWithKind ty

isCompositeType ty = isDisjunctiveType ty || isConjunctiveType ty

incapability :: Type
incapability = typ EmptyCapability

isIncapability Type{inner = EmptyCapability} = True
isIncapability _ = False

isStackboundType ty = box ty == Just Stackbound
makeStackbound ty = ty{box = Just Stackbound}

unionType ltype rtype = typ UnionType{ltype, rtype}
isUnionType Type{inner = UnionType{}} = True
isUnionType _ = False

unionMembers Type{inner = UnionType{ltype, rtype}} =
    unionMembers ltype ++ unionMembers rtype
unionMembers ty = [ty]

arrowType :: [Type] -> Type -> Type
arrowType args ty = typ (ArrowType [] args ty [])
arrowWithTypeParam params args ty = typ $ ArrowType params args ty []

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

typeVar = typ . TypeVar
isTypeVar Type{inner = TypeVar {}} = True
isTypeVar _ = False

isMainType Type{inner = ClassType{refInfo = RefInfo{refId = "Main"}}} = True
isMainType _ = False

stringObjectType = setRefSourceFile "String.enc" $
                    makeRead $ classType "String" []

isStringObjectType ty = isClassType ty &&
                        getId ty == "String"

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where replace ty =
              fromMaybe ty (lookup ty bindings)
              `withBoxOf` ty

ctype :: String -> Type
ctype = typ . CType

isCType Type{inner = CType{}} = True
isCType _ = False

unitType :: Type
unitType = typ UnitType

isUnitType :: Type -> Bool
isUnitType = (== unitType)

nullType = typ NullType
isNullType = (== nullType)

boolType = typ BoolType
isBoolType = (== boolType)

intType = typ IntType
isIntType = (== intType)

uintType :: Type
uintType = typ UIntType

isUIntType :: Type -> Bool
isUIntType = (== uintType)

realType :: Type
realType = typ RealType

isRealType :: Type -> Bool
isRealType = (== realType)

stringType = typ StringType
isStringType = (== stringType)

charType :: Type
charType = typ CharType

isCharType :: Type -> Bool
isCharType = (== charType)

primitives :: [Type]
primitives = [unitType
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
  typ TypeSynonym{refInfo = RefInfo{refId = name
                                   ,parameters
                                   ,mode = Nothing
                                   ,refNamespace = Nothing
                                   ,refSourceFile = Nothing
                                   }
                 ,resolvesTo = resolution
                 }

typeSynonymLHS :: Type -> (String, [Type])
typeSynonymLHS ty
  | isTypeSynonym ty
  , RefInfo{refId, parameters} <- refInfo . inner $ ty
    = (refId, parameters)
  | otherwise = error "Types.hs: Expected type synonym"

typeSynonymRHS :: Type -> Type
typeSynonymRHS ty
  | isTypeSynonym ty = resolvesTo . inner $ ty
  | otherwise = error "Types.hs: Expected type synonymm"

typeSynonymSetRHS :: Type -> Type -> Type
typeSynonymSetRHS ty rhs
  | isTypeSynonym ty
  , iType <- inner ty
    = ty{inner = iType{resolvesTo = rhs}}
  | otherwise = error "Types.hs: Expected type synonymm"

isTypeSynonym Type{inner = TypeSynonym{}} = True
isTypeSynonym _ = False

unfoldTypeSynonyms :: Type -> Type
unfoldTypeSynonyms = typeMap unfoldSingleSynonym

unfoldSingleSynonym :: Type -> Type
unfoldSingleSynonym Type{inner = TypeSynonym{resolvesTo = t}} = t
unfoldSingleSynonym t = t

tupleLength Type{inner = TupleType {argTypes}} = length argTypes
tupleLength _ = error "Types.hs: Expected a tuple type"
