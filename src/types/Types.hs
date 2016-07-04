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
            ,classType
            ,isRefAtomType
            ,traitType
            ,isRefType
            ,isTraitType
            ,isActiveClassType
            ,isSharedClassType
            ,isPassiveClassType
            ,isClassType
            ,mainType
            ,isMainType
            ,stringObjectType
            ,isStringObjectType
            ,conjunctiveType
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
            ,getArgTypes
            ,setArgTypes
            ,getResultType
            ,getId
            ,getTypeParameters
            ,setTypeParameters
            ,conjunctiveTypesFromCapability
            ,typesFromCapability
            ,withModeOf
            ,withBoxOf
            ,resolvedFrom
            ,unbox
            ,unrestrict
            ,bar
            ,doubleBar
            ,tilde
            ,restrictedFields
            ,barredFields
            ,weaklyRestrictedFields
            ,stronglyRestrictedFields
            ,transferRestrictedFields
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
            ,showWithoutMode
            ,isModeless
            ,isSafeType
            ,modeSubtypeOf
            ,makeUnsafe
            ,makeLinear
            ,makeThread
            ,makePristine
            ,makeRead
            ,makeSubordinate
            ,makeSpine
            ,makeLockfree
            ,isLinearRefType
            ,isPristineRefType
            ,isThreadRefType
            ,isLockfreeRefType
            ,isReadRefType
            ,isSubordinateRefType
            ,isUnsafeRefType
            ,isSpineRefType
            ,makeStackbound
            ,isStackboundType
            ) where

import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Traversable
import Control.Monad

import Debug.Trace

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

data Mode = Linear
          | Thread
          | Unsafe
          | Read
          | Lockfree
          | Subordinate
          | Spine
            deriving(Eq)

instance Show Mode where
    show Linear      = "linear"
    show Thread      = "thread"
    show Unsafe      = "unsafe"
    show Read        = "read"
    show Lockfree    = "lockfree"
    show Subordinate = "subord"
    show Spine       = "spine"

modeSubtypeOf ty1 ty2 = getMode ty1 == getMode ty2

modeIsSafe Read = True
modeIsSafe Lockfree = True
modeIsSafe _    = False

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

data Box = Stackbound
         | Pristine deriving(Eq)

instance Show Box where
    show Stackbound = "borrowed"
    show Pristine = "pristine"

data RestrictedField =
    Strong{fname :: Name}
  | Weak{fname :: Name}
  | Transfer{fname :: Name}
    deriving(Eq, Ord)

instance Show RestrictedField where
    show Strong{fname} = "|| " ++ show fname
    show Weak{fname} = "| " ++ show fname
    show Transfer{fname} = "~ " ++ show fname

isWeak Weak{} = True
isWeak _ = False

isStrong Strong{} = True
isStrong _ = False

isTransfer Transfer{} = True
isTransfer _ = False

data Type = Type
    {inner      :: InnerType
    ,box        :: Maybe Box
    ,restricted :: [RestrictedField]
    }

unbox ty = ty{box = Nothing}

unrestrict ty f =
    let rs = restricted ty
    in ty{restricted = rs \\ [Weak f, Strong f, Transfer f]}

bar ty f =
    let rs = restricted ty
    in ty{restricted = (rs \\ [Strong f]) `union` [Weak f]}

doubleBar ty f =
    let rs = restricted ty
    in ty{restricted = (rs \\ [Weak f]) `union` [Strong f]}

tilde ty f =
    let rs = restricted ty
    in ty{restricted = rs `union` [Transfer f]}

restrictedFields = map fname . restricted
barredFields = map fname . filter (not . isTransfer) . restricted
weaklyRestrictedFields = map fname . filter isWeak . restricted
stronglyRestrictedFields = map fname . filter isStrong . restricted
transferRestrictedFields = map fname . filter isTransfer . restricted

typ ity = Type{inner = ity, box = Nothing, restricted = []}

transferBox ty1 ty2 = ty2{box = box ty1}

instance Eq Type where
    ty1 == ty2 =
        inner ty1 == inner ty2 &&
        sort (restricted ty1) == sort (restricted ty2)

instance Show Type where
    show Type{inner, box = Nothing, restricted} =
        show inner ++ showRestricted restricted
    show Type{inner, box = Just s, restricted} =
        show s ++ " " ++ show inner ++ showRestricted restricted

showRestricted = unwords . ("":) . map show

data InnerType =
          Unresolved{refInfo :: RefInfo}
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

setResultType ty res
  | hasResultType ty = ty{inner = iType{resultType = res}}
  | otherwise = error $ "Types.hs: tried to set the resultType of " ++ show ty
  where
    iType = inner ty

instance Show InnerType where
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
    | isConjunction ty
    , ltype <- ltype . inner $ ty
    , rtype <- rtype . inner $ ty =
        (typesFromCapability ltype, typesFromCapability rtype) :
        conjunctiveTypesFromCapability ltype ++
        conjunctiveTypesFromCapability rtype
    | isDisjunction ty
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

refTypeWithParams refId parameters =
    typ Unresolved{refInfo}
    where
      refInfo = RefInfo{refId
                       ,parameters
                       ,mode = Nothing
                       }

resolvedFrom actual formal
    | isRefAtomType actual && isRefAtomType formal
    , refInfo <- refInfo . inner $ actual
        = case () of _
                      | isActiveClassType formal ->
                          actual{inner = ClassType{refInfo, activity = Active}}
                      | isPassiveClassType formal ->
                          actual{inner = ClassType{refInfo, activity = Passive}}
                      | isSharedClassType formal ->
                          actual{inner = ClassType{refInfo, activity = Shared}}
                      | isTraitType formal ->
                          actual{inner = TraitType{refInfo}}
                      | otherwise ->
                          error $ "Types.hs: Unknown kind of reftype: " ++
                                  showWithKind formal
    | isRefAtomType actual && isTypeSynonym formal
    , refInfo <- refInfo . inner $ actual
    , resolvesTo <- resolvesTo . inner $ formal
    , subst <- zip (getTypeParameters formal) (getTypeParameters actual)
    , resolvesTo' <- replaceTypeVars subst resolvesTo
      = actual{inner = TypeSynonym{refInfo, resolvesTo = resolvesTo'}}
    | otherwise =
        error $ "Types.hs: " ++ showWithKind actual ++
                " is not resolvable from " ++ showWithKind formal

classType :: Activity -> String -> [Type] -> Type
classType activity name parameters =
    Type{inner = ClassType{refInfo = RefInfo{refId = name
                                            ,parameters
                                            ,mode = Nothing}
                          , activity}
        ,box = Nothing
        ,restricted = []
        }

traitType :: String -> [Type] -> Type
traitType name parameters =
    Type{inner = TraitType{refInfo = RefInfo{refId = name
                                            ,parameters
                                            ,mode = Nothing}}

        ,box = Nothing
        ,restricted = []
        }

isRefAtomType Type{inner = Unresolved {}} = True
isRefAtomType Type{inner = TraitType {}} = True
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
isTraitType _ = False

isActiveClassType Type{inner = ClassType{activity = Active}} = True
isActiveClassType _ = False

isSharedClassType Type{inner = ClassType{activity = Shared}} = True
isSharedClassType _ = False

isPassiveClassType Type{inner = ClassType{activity = Passive}} = True
isPassiveClassType _ = False

isClassType Type{inner = ClassType{}} = True
isClassType _ = False

disjunctiveType ltype rtype =
    typ CapabilityType{typeop = Addition, ltype, rtype}
conjunctiveType ltype rtype =
    typ CapabilityType{typeop = Product, ltype, rtype}

-- TODO: Maybe a type can have several modes?
-- TODO: Should classes ever have modes (except the "inherited ones")?
setMode ty m
    | isRefAtomType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just m}}}
    | otherwise = error $ "Types.hs: Cannot set mode of " ++ showWithKind ty

makeUnsafe ty = setMode ty Unsafe
makeLinear ty = setMode ty Linear
makeThread ty = setMode ty Thread

makePristine ty = ty{box = Just Pristine}

makeRead ty = setMode ty Read
makeSubordinate ty = setMode ty Subordinate

makeSpine ty = setMode ty Spine

makeLockfree ty = setMode ty Lockfree

isSafeType ty
    |  isMaybeType ty
    || isStreamType ty
    || isFutureType ty
    || isParType ty = isSafeType $ getResultType ty
    | isTupleType ty = all isSafeType $ getArgTypes ty
    | isCompositeType ty
    , traits <- typesFromCapability ty = all isSafeType traits
    | isModeless ty =  isPrimitive ty
                    || isActiveClassType ty
                    || isRangeType ty
    | otherwise = let mode = getMode ty in
                  isJust mode && modeIsSafe (fromJust mode)

isModeless ty
    | Just m <- getMode ty = False
    | otherwise = True

isLinearRefType ty
    | Just Linear <- getMode ty = True
    | Just Spine  <- getMode ty = True
    | otherwise = False

isThreadRefType ty
    | Just Thread <- getMode ty = True
    | otherwise = False

isPristineRefType Type{box = Just Pristine} = True
isPristineRefType _ = False

isReadRefType ty
    | Just Read <- getMode ty = True
    | otherwise = False

isSubordinateRefType ty
    | Just Subordinate <- getMode ty = True
    | Just Spine       <- getMode ty = True
    | otherwise = False

isSpineRefType ty
    | Just Spine <- getMode ty = True
    | otherwise = False

isUnsafeRefType ty
    | Just Unsafe <- getMode ty = True
    | otherwise = False

isLockfreeRefType ty
    | Just Lockfree <- getMode ty = True
    | otherwise = False

isCapabilityType Type{inner = CapabilityType{}} = True
isCapabilityType Type{inner = TraitType{}} = True
isCapabilityType Type{inner = EmptyCapability{}} = True
isCapabilityType _ = False

isDisjunction Type{inner = CapabilityType{typeop = Addition}} = True
isDisjunction _ = False

isConjunction Type{inner = CapabilityType{typeop = Product}} = True
isConjunction _ = False

isCompositeType ty = isDisjunction ty || isConjunction ty

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
arrowType args ty = typ (ArrowType [] args ty)
arrowWithTypeParam params args ty = typ $ ArrowType params args ty

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

mainType = refTypeWithParams "Main" []

isMainType Type{inner = ClassType{refInfo = RefInfo{refId = "Main"}}} = True
isMainType _ = False

stringObjectType = classType Passive "String" []

isStringObjectType = (==stringObjectType)

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where replace ty =
              transferBox ty $
              fromMaybe ty (lookup ty bindings)

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
  typ TypeSynonym{refInfo = RefInfo{refId = name
                                   ,parameters
                                   ,mode = Nothing}
                 ,resolvesTo = resolution}

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
