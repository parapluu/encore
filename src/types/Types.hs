module Types(
             Type
            ,arrowType
            ,isArrowType
            ,futureType
            ,isFutureType
            ,parType
            ,isParType
            ,streamType
            ,isStreamType
            ,arrayType
            ,isArrayType
            ,refTypeWithParams
            ,refType
            ,traitTypeFromRefType
            ,passiveClassTypeFromRefType
            ,activeClassTypeFromRefType
            ,getCapability
            ,isRefType
            ,isTraitType
            ,isActiveClassType
            ,isPassiveClassType
            ,isClassType
            ,isUntypedRef
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
            ,getImplementedTraits
            ,getTypeParameters
            ,setTypeParameters
            ,withTypeParametersOf
            ,typeComponents
            ,typeMap
            ,typeMapM
            ,subtypeOf
            ,strictSubtypeOf
            ,showWithKind
            ,showWithoutMode
            ,isModeless
            ,makeUnsafe
            ,makeLinear
            ,isLinearType
            ,makeStackbound
            ,isStackboundType
            ) where

import Data.List
import Data.Maybe
import Control.Monad

data Activity = Active
              | Passive
                deriving(Eq, Show)

data Capability = Capability{traits :: [RefInfo]}
                deriving(Eq)

instance Show Capability where
    show Capability{traits}
        | null traits = "I"
        | otherwise   = intercalate " + " (map show traits)

data Mode = Linear
          | Unsafe
            deriving(Eq)

instance Show Mode where
    show Linear = "linear"
    show Unsafe = "unsafe"

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

data Box = Stackbound deriving(Eq)

instance Show Box where
    show Stackbound = "S"

data Type = Type {inner :: InnerType
                 ,box   :: Maybe Box}

typ ity = Type{inner = ity, box = Nothing}

transferBox ty1 ty2 = ty2{box = box ty1}

instance Eq Type where
    ty1 == ty2 = inner ty1 == inner ty2

instance Show Type where
    show Type{inner, box = Nothing} = show inner
    show Type{inner, box = Just s} =
        show s ++ "(" ++ show inner ++ ")"

data InnerType = UntypedRef{refInfo :: RefInfo}
               | TraitType{refInfo :: RefInfo}
               | ClassType{refInfo :: RefInfo
                          ,capability :: Capability
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
               | VoidType
               | StringType
               | IntType
               | BoolType
               | RealType
               | NullType
                 deriving(Eq)

getArgTypes = argTypes . inner
getResultType = resultType . inner
getId ty
    | isRefType ty = refId . refInfo . inner $ ty
    | isTypeVar ty = ident . inner $ ty
    | otherwise = error $ "Types.hs: Tried to get the id of " ++
                          showWithKind ty

instance Show InnerType where
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
    show VoidType   = "void"
    show StringType = "string"
    show IntType    = "int"
    show RealType   = "real"
    show BoolType   = "bool"
    show NullType   = "null type"

maybeParen :: Type -> String
maybeParen ty
    | isArrowType ty  ||
      isFutureType ty ||
      isParType ty    ||
      isStreamType ty = "(" ++ show ty ++ ")"
    | otherwise = show ty

showWithKind :: Type -> String
showWithKind ty = kind (inner ty) ++ " " ++ show ty
    where
    kind VoidType                      = "primitive type"
    kind StringType                    = "primitive type"
    kind IntType                       = "primitive type"
    kind RealType                      = "primitive type"
    kind BoolType                      = "primitive type"
    kind TraitType{}                   = "trait type"
    kind ClassType{activity = Active}  = "active class type"
    kind ClassType{activity = Passive} = "passive class type"
    kind CapabilityType{}              = "capability type"
    kind TypeVar{}                     = "polymorphic type"
    kind ArrowType{}                   = "function type"
    kind FutureType{}                  = "future type"
    kind ParType{}                     = "parallel type"
    kind StreamType{}                  = "stream type"
    kind ArrayType{}                   = "array type"
    kind _                             = "type"

showWithoutMode :: Type -> String
showWithoutMode ty
    | isRefType ty = showRefInfoWithoutMode $ refInfo (inner ty)
    | otherwise = show ty

typeComponents :: Type -> [Type]
typeComponents ty
    |  isFutureType ty
    || isParType ty
    || isStreamType ty
    || isArrayType ty =
        ty : typeComponents (getResultType ty)
    | isArrowType ty =
        ty : concatMap typeComponents (getArgTypes ty) ++
             typeComponents (getResultType ty)
    |  isUntypedRef ty
    || isTraitType ty =
        ty : refInfoTypeComponents (refInfo iType)
    | isClassType ty =
        ty : capabilityComponents (capability iType) ++
             refInfoTypeComponents (refInfo iType)
    | isCapabilityType ty =
        ty : capabilityComponents (capability iType)
    | otherwise = [ty]
    where
      iType = inner ty

      refInfoTypeComponents = concatMap typeComponents . parameters

-- TODO: Should maybe extract the power set?
capabilityComponents :: Capability -> [Type]
capabilityComponents Capability{traits} =
    concatMap atomComponents traits
    where
      atomComponents atom =
          typeComponents $ traitTypeFromRefType (typ UntypedRef{refInfo = atom})

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty
    |  isUntypedRef ty
    || isTraitType ty =
        f ty{inner = refTypeMap f iType}
    | isClassType ty =
        f ty{inner = capabilityTypeMap f . refTypeMap f $ iType}
    | isCapabilityType ty =
        f ty{inner = capabilityTypeMap f iType}
    | isArrowType ty =
        f ty{inner = resultTypeMap f . argTypesMap f $ iType}
    |  isFutureType ty
    || isParType ty
    || isStreamType ty
    || isArrayType ty =
        f ty{inner = resultTypeMap f iType}
    | otherwise = f ty
    where
      iType = inner ty

      refTypeMap f ity =
          ity{refInfo = refInfoTypeMap f (refInfo ity)}

      capabilityTypeMap f ity =
          ity{capability = capabilityMap f (capability ity)}

      resultTypeMap f ity =
          ity{resultType = typeMap f (resultType ity)}

      argTypesMap f ity =
          ity{argTypes = map (typeMap f) (argTypes ity)}

      refInfoTypeMap f info@RefInfo{parameters} =
          info{parameters = map (typeMap f) parameters}

      capabilityMap f cap@Capability{traits} =
          cap{traits = map (traitMapAndBack f) traits}
          where
            traitMapAndBack f =
                refInfo . inner . typeMap f . typ . TraitType

typeMapM :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapM f ty
    |  isUntypedRef ty
    || isTraitType ty = do
        iType' <- refTypeMapM f iType
        f ty{inner = iType'}
    | isClassType ty = do
        iType' <- refTypeMapM f iType >>=
                  capabilityTypeMapM f
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
    || isArrayType ty = do
        iType' <- resultTypeMapM f iType
        f ty{inner = iType'}
    | otherwise = f ty
    where
      iType = inner ty

      capabilityTypeMapM f ity = do
        capability' <- capabilityMapM f (capability ity)
        return ity{capability = capability'}

      refTypeMapM f ity = do
        refInfo' <- refInfoTypeMapM f (refInfo ity)
        return ity{refInfo = refInfo'}


      refInfoTypeMapM f info@RefInfo{parameters} = do
        parameters' <- mapM (typeMapM f) parameters
        return info{parameters = parameters'}

      argTypesMapM f ity = do
        argTypes' <- mapM f (argTypes ity)
        return ity{argTypes = argTypes'}

      resultTypeMapM f ity = do
        resultType' <- f $ resultType ity
        return ity{resultType = resultType'}

      capabilityMapM f cap@Capability{traits} = do
        traits' <- mapM (traitMapAndBack f) traits
        return cap{traits = traits'}
        where
          traitMapAndBack f =
              liftM (refInfo . inner) . typeMapM f . typ . TraitType

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

withTypeParametersOf sink source =
    let formals = getTypeParameters sink
        actuals = getTypeParameters source
        bindings = zip formals actuals
        result = replaceTypeVars bindings sink
    in
      transferBox source result

getImplementedTraits :: Type -> [Type]
getImplementedTraits ty
    | isTraitType ty = [ty]
    | isClassType ty =
        getImplementedTraits $ typ CapabilityType{capability = capability iType}
    | isCapabilityType ty =
        map (typ . TraitType) (traits (capability iType))
    | otherwise =
        error $ "Types.hs: Can't get implemented traits of type " ++ show ty
    where
      iType = inner ty

refTypeWithParams refId parameters =
    typ UntypedRef{refInfo =
                       RefInfo{refId
                              ,parameters
                              ,mode = Nothing
                              }}

refType id = refTypeWithParams id []

activeClassTypeFromRefType ref cap
    | isUntypedRef ref
    , isCapabilityType cap
    , UntypedRef{refInfo} <- inner ref
    , CapabilityType{capability} <- inner cap
      = Type{inner = ClassType{refInfo
                              ,activity = Active
                              ,capability}
            ,box = box ref}
    | isUntypedRef ref
    , isUntypedRef cap
    , UntypedRef{refInfo} <- inner ref
    , UntypedRef{refInfo = capInfo} <- inner cap
      = Type{inner = ClassType{refInfo
                              ,activity = Active
                              ,capability = Capability [capInfo]}
            ,box = box ref}
    | otherwise =
        error $ "Types.hs: Can't make active type from type: " ++ show ref

passiveClassTypeFromRefType ref cap
    | isUntypedRef ref
    , isCapabilityType cap
    , UntypedRef{refInfo} <- inner ref
    , CapabilityType{capability} <- inner cap
      = Type{inner = ClassType{refInfo
                              ,activity = Passive
                              ,capability}
            ,box = box ref}
    | isUntypedRef ref
    , isUntypedRef cap
    , UntypedRef{refInfo} <- inner ref
    , UntypedRef{refInfo = capInfo} <- inner cap
      = Type{inner = ClassType{refInfo
                              ,activity = Passive
                              ,capability = Capability [capInfo]}
            ,box = box ref}
    | otherwise =
        error $ "Types.hs: Can't make passive type from type: " ++ show ref

-- TODO: Should preserve boxing?
getCapability ty
    | isClassType ty =
        typ CapabilityType{capability = capability (inner ty)}
    | isCapabilityType ty = ty
    | isTraitType ty =
        typ CapabilityType{capability =
                               Capability{traits = [refInfo (inner ty)]}}
    | otherwise =
        error $ "Types.hs: Can't get capability from type: " ++ show ty

traitTypeFromRefType ty
    | isUntypedRef ty
    , UntypedRef{refInfo} <- inner ty
      = Type{inner = TraitType{refInfo}
            ,box = box ty}
    | otherwise =
        error $ "Types.hs: Can't make trait type from type: " ++ show ty

isRefType ty = isUntypedRef ty ||
               isTraitType ty ||
               isClassType ty

isTraitType Type{inner = TraitType{}} = True
isTraitType _ = False

isActiveClassType Type{inner = ClassType{activity = Active}} = True
isActiveClassType _ = False

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
    | otherwise = error $ "Types.hs: Cannot make type unsafe: " ++
                          show ty

makeLinear ty
    | isRefType ty
    , iType <- inner ty
    , info <- refInfo iType
      = ty{inner = iType{refInfo = info{mode = Just Linear}}}
    | otherwise = error $ "Types.hs: Cannot make type linear: " ++
                          show ty

isModeless ty
    |  isTraitType ty
    || isUntypedRef ty = isNothing . mode . refInfo . inner $ ty
    | isClassType ty = False
    | otherwise = error $ "Types.hs: Cannot get modes of type: " ++
                          show ty

isLinearRefType ty
    | isRefType ty = (mode . refInfo . inner) ty == Just Linear
    | otherwise = False

isLinearType = any isLinearRefType . typeComponents . dropArrows
    where
      dropArrows = typeMap dropArrow
      dropArrow ty
          | isArrowType ty = voidType
          | otherwise = ty

isStackboundType ty = box ty == Just Stackbound

makeStackbound ty = ty{box = Just Stackbound}

capabilityType traits =
    typ CapabilityType{capability = Capability{
                                      traits = map (refInfo . inner) traits}}
isCapabilityType Type{inner = CapabilityType{}} = True
isCapabilityType _ = False

-- TODO: Make this into an empty trait instead
incapability = typ CapabilityType{capability = Capability{traits = []}}

arrowType args ty = typ (ArrowType args ty)
isArrowType Type{inner = ArrowType {}} = True
isArrowType _ = False

futureType = typ . FutureType
isFutureType Type{inner = FutureType {}} = True
isFutureType _ = False

parType = typ . ParType
isParType Type{inner = ParType {}} = True
isParType _ = False

streamType = typ . StreamType
isStreamType Type{inner = StreamType {}} = True
isStreamType _ = False

arrayType = typ . ArrayType
isArrayType Type{inner = ArrayType {}} = True
isArrayType _ = False

typeVar = typ . TypeVar
isTypeVar Type{inner = TypeVar {}} = True
isTypeVar _ = False

isMainType Type{inner = ClassType{refInfo = RefInfo{refId = "Main"}}} = True
isMainType _ = False

replaceTypeVars :: [(Type, Type)] -> Type -> Type
replaceTypeVars bindings = typeMap replace
    where replace ty =
              transferBox ty $
              fromMaybe ty (lookup ty bindings)

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

primitives :: [Type]
primitives = [voidType, intType, realType, boolType, stringType]

isPrimitive :: Type -> Bool
isPrimitive = (`elem` primitives)

isNumeric :: Type -> Bool
isNumeric ty = isRealType ty || isIntType ty


strictSubtypeOf :: Type -> Type -> Bool
strictSubtypeOf ty1 ty2 =
    ty1 `subtypeOf` ty2 &&
    ty1 /= ty2

-- TODO: Extend to match all combinations
subtypeOf :: Type -> Type -> Bool
subtypeOf ty1 ty2
    | isNullType ty1 = isNullType ty2 || isRefType ty2
    | isClassType ty1 && isTraitType ty2 =
        getImplementedTraits ty1 `containsTraitMatching` ty2
    | isClassType ty1 && isCapabilityType ty2 =
        capability (inner ty1) `capabilitySubtypeOf` capability (inner ty2)
    | isCapabilityType ty1 && isCapabilityType ty2 =
        capability (inner ty1) `capabilitySubtypeOf` capability (inner ty2)
    | isCapabilityType ty1 && isTraitType ty2 =
        getImplementedTraits ty1 `containsTraitMatching` ty2
    | isTraitType ty1 && isTraitType ty2 =
        ty1 == ty2 && ty1 `modeSubtypeOf` ty2
    | otherwise = ty1 == ty2
    where
      matchesTrait ty1 ty2 = ty1 == ty2 && ty1 `modeSubtypeOf` ty2
      containsTraitMatching ts t = any (`matchesTrait` t) ts
      modeSubtypeOf ty1 ty2
          | isLinearType ty1 &&
            isStackboundType ty2 = True
          | otherwise = mode (refInfo (inner ty1)) == mode (refInfo (inner ty2))
      capabilitySubtypeOf cap1 cap2 =
          let
              traits1 = map (typ . TraitType) $ traits cap1
              traits2 = map (typ . TraitType) $ traits cap2
          in
            all (traits1 `containsTraitMatching`) traits2