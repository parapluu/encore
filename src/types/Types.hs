module Types(
             Type
            ,Capability
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
            ,typeComponents
            ,typeMap
            ,typeMapM
            ,subtypeOf
            ,strictSubtypeOf
            ,showWithKind
            ,makeLinear
            ,isLinearRefType
            ,isLinearType
            ) where

import Data.List
import Data.Maybe

data Activity = Active
              | Passive
                deriving(Eq, Show)

data Capability = Capability{traits :: [RefInfo]}
                deriving(Eq)

instance Show Capability where
    show Capability{traits}
        | null traits = "I"
        | otherwise   = intercalate " + " (map show traits)

data RefInfo = RefInfo{refId :: String
                      ,parameters :: [Type]
                      ,isLinear :: Bool
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

getArgTypes = argTypes
getResultType = resultType
getId UntypedRef{refInfo} = refId refInfo
getId TraitType{refInfo} = refId refInfo
getId ClassType{refInfo} = refId refInfo
getId TypeVar{ident} = ident

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
    show VoidType   = "void"
    show StringType = "string"
    show IntType    = "int"
    show RealType   = "real"
    show BoolType   = "bool"
    show NullType   = "null type"

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
typeComponents ref@(ClassType{refInfo, capability}) =
    ref : capabilityComponents capability ++ refInfoTypeComponents refInfo
typeComponents ref@(CapabilityType{capability}) =
    ref : capabilityComponents capability
typeComponents str@(StreamType ty) =
    str : typeComponents ty
typeComponents arr@(ArrayType ty)  =
    arr : typeComponents ty
typeComponents ty = [ty]

refInfoTypeComponents = concatMap typeComponents . parameters

-- TODO: Should maybe extract the power set?
capabilityComponents :: Capability -> [Type]
capabilityComponents Capability{traits} =
    concatMap traitToType traits
    where
      traitToType t@RefInfo{parameters} =
          CapabilityType{capability = Capability{traits = [t]}} : parameters

typeMap :: (Type -> Type) -> Type -> Type
typeMap f ty@UntypedRef{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@TraitType{refInfo} =
    f ty{refInfo = refInfoTypeMap f refInfo}
typeMap f ty@ClassType{refInfo, capability} =
    f ty{refInfo = refInfoTypeMap f refInfo
         ,capability = capabilityTypeMap f capability}
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
typeMap f ty = f ty

refInfoTypeMap :: (Type -> Type) -> RefInfo -> RefInfo
refInfoTypeMap f info@RefInfo{parameters} =
    info{parameters = map (typeMap f) parameters}

capabilityTypeMap :: (Type -> Type) -> Capability -> Capability
capabilityTypeMap f cap@Capability{traits} =
    cap{traits = map (refInfoTypeMap f) traits}

typeMapM :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapM f ty@UntypedRef{refInfo} = do
  refInfo' <- refInfoTypeMapM f refInfo
  f ty{refInfo = refInfo'}
typeMapM f ty@TraitType{refInfo} = do
  refInfo' <- refInfoTypeMapM f refInfo
  f ty{refInfo = refInfo'}
typeMapM f ty@ClassType{refInfo, capability} = do
  refInfo' <- refInfoTypeMapM f refInfo
  capability' <- capabilityTypeMapM f capability
  f ty{refInfo = refInfo'
               ,capability = capability'}
typeMapM f ty@CapabilityType{capability} = do
  capability' <- capabilityTypeMapM f capability
  f ty{capability = capability'}
typeMapM f ty@ArrowType{argTypes, resultType} = do
  argTypes' <- mapM (typeMapM f) argTypes
  resultType' <- f resultType
  f ty{argTypes = argTypes'
               ,resultType = resultType'}
typeMapM f ty@FutureType{resultType} =
  typeMapMResultType f ty
typeMapM f ty@ParType{resultType} =
  typeMapMResultType f ty
typeMapM f ty@StreamType{resultType} =
  typeMapMResultType f ty
typeMapM f ty@ArrayType{resultType} =
  typeMapMResultType f ty
typeMapM f ty = f ty

typeMapMResultType :: Monad m => (Type -> m Type) -> Type -> m Type
typeMapMResultType f ty = do
  resultType' <- f $ resultType ty
  f ty{resultType = resultType'}

refInfoTypeMapM :: Monad m => (Type -> m Type) -> RefInfo -> m RefInfo
refInfoTypeMapM f info@RefInfo{parameters} = do
  parameters' <- mapM (typeMapM f) parameters
  return info{parameters = parameters'}

capabilityTypeMapM :: Monad m => (Type -> m Type) -> Capability -> m Capability
capabilityTypeMapM f cap@Capability{traits} = do
  traits' <- mapM (refInfoTypeMapM f) traits
  return cap{traits = traits'}

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

getImplementedTraits :: Type -> [Type]
getImplementedTraits ty@TraitType{} = [ty]
getImplementedTraits ty@ClassType{capability} =
    map TraitType (traits capability)
getImplementedTraits ty@CapabilityType{capability} =
    map TraitType (traits capability)
getImplementedTraits ty =
    error $ "Types.hs: Can't get implemented traits of type " ++ show ty

refTypeWithParams refId parameters =
    UntypedRef{refInfo = RefInfo{refId,
                                 parameters,
                                 isLinear = False}}
refType id = refTypeWithParams id []

activeClassTypeFromRefType UntypedRef{refInfo} CapabilityType{capability} =
      ClassType{refInfo, activity = Active, capability}
activeClassTypeFromRefType ty _ =
    error $ "Types.hs: Can't make active type from type: " ++ show ty

passiveClassTypeFromRefType UntypedRef{refInfo} CapabilityType{capability} =
      ClassType{refInfo, activity = Passive, capability}
passiveClassTypeFromRefType ty _ =
    error $ "Types.hs: Can't make passive type from type: " ++ show ty

getCapability ty
    | isClassType ty = CapabilityType $ capability ty
    | isCapabilityType ty = ty
    | isTraitType ty =
        CapabilityType{capability = Capability{traits = [refInfo ty]}}
    | otherwise =
        error $ "Types.hs: Can't get capability from type: " ++ show ty

traitTypeFromRefType UntypedRef{refInfo} =
    TraitType{refInfo}
traitTypeFromRefType ty =
    error $ "Types.hs: Can't make trait type from type: " ++ show ty

isRefType UntypedRef {} = True
isRefType TraitType {} = True
isRefType ClassType {} = True
isRefType CapabilityType {} = True
isRefType _ = False

isTraitType TraitType{} = True
isTraitType _ = False

isActiveClassType ClassType{activity = Active} = True
isActiveClassType _ = False

isPassiveClassType ClassType{activity = Passive} = True
isPassiveClassType _ = False

isClassType ClassType{} = True
isClassType _ = False

makeLinear = undefined
isLinearRefType = undefined
isLinearType = undefined

capabilityType traits =
    CapabilityType{capability = Capability{traits = map refInfo traits}}
isCapabilityType CapabilityType{} = True
isCapabilityType _ = False

incapability = CapabilityType{capability = Capability{traits = []}}

arrowType = ArrowType
isArrowType (ArrowType {}) = True
isArrowType _ = False

futureType = FutureType
isFutureType FutureType {} = True
isFutureType _ = False

parType = ParType
isParType ParType {} = True
isParType _ = False

streamType = StreamType
isStreamType StreamType {} = True
isStreamType _ = False

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

strictSubtypeOf :: Type -> Type -> Bool
strictSubtypeOf ty1 ty2
  | isClassType ty1 && isTraitType ty2 =
      ty2 `elem` getImplementedTraits ty1
  | otherwise = False

subtypeOf :: Type -> Type -> Bool
subtypeOf ty1 ty2
    | isNullType ty1 = isNullType ty2 || isRefType ty2
    | isClassType ty1 && isTraitType ty2 =
        ty2 `elem` getImplementedTraits ty1
    | isClassType ty1 && isCapabilityType ty2 =
        capability ty1 `capabilitySubtypeOf` capability ty2
    | isCapabilityType ty1 && isCapabilityType ty2 =
        capability ty1 `capabilitySubtypeOf` capability ty2
    | otherwise = ty1 == ty2
    where
      capabilitySubtypeOf cap1 cap2 =
      -- TODO: Needs to handle parameters as well!
          let
              traits1 = traits cap1
              traits2 = traits cap2
        in
          all (`elem` traits1) traits2
