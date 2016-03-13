{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util(TypecheckM
                       ,CapturecheckM
                       ,whenM
                       ,anyM
                       ,unlessM
                       ,tcError
                       ,tcWarning
                       ,resolveType
                       ,subtypeOf
                       ,assertDistinctThing
                       ,assertDistinct
                       ,classOrTraitName
                       ,findField
                       ,findMethod
                       ,findMethodWithCalledType
                       ,findCapability
                       ,formalBindings
                       ,propagateResultType
                       ,isLinearType
                       ,typeMinus
                       ) where

import Identifiers
import Types as Ty
import AST.AST as AST
import Data.List
import Data.Maybe
import Text.Printf (printf)
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow(second)
import Control.Monad.State

-- Module dependencies
import Typechecker.TypeError
import Typechecker.Environment

-- Monadic versions of common functions
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM p = foldM (\b x -> liftM (b ||) (p x)) False

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM p = foldM (\b x -> liftM (b &&) (p x)) True

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond action = cond >>= (`when` action)

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM cond action = cond >>= (`unless` action)

-- | The monad in which all typechecking is performed. A function
-- of return type @TypecheckM Bar@ may read from an 'Environment'
-- and returns a @Bar@ or throws a typechecking exception.
type TypecheckM a =
    forall m . (MonadState [TCWarning] m,
                MonadError TCError m,
                MonadReader Environment m) => m a

type CapturecheckM a =
    forall m . (MonadError CCError m, MonadReader Environment m) => m a

-- | convenience function for throwing an exception with the
-- current backtrace
tcError msg =
    do bt <- asks backtrace
       throwError $ TCError (msg, bt)

tcWarning wrn =
    do bt <- asks backtrace
       modify (TCWarning bt wrn:)

-- | @matchTypeParameterLength ty1 ty2@ ensures that the type parameter
-- lists of its arguments have the same length.
matchTypeParameterLength :: Type -> Type -> TypecheckM ()
matchTypeParameterLength ty1 ty2 = do
  let params1 = getTypeParameters ty1
      params2 = getTypeParameters ty2
  unless (length params1 == length params2) $
    tcError $ printf "'%s' expects %d type arguments, but '%s' has %d"
              (showWithoutMode ty1) (length params1)
              (showWithoutMode ty2) (length params2)

-- | @resolveType ty@ checks all the components of @ty@, resolving
-- reference types to traits or classes and making sure that any
-- type variables are in the current environment.
resolveType :: Type -> TypecheckM Type
resolveType = typeMapM resolveSingleType
    where
      resolveSingleType ty
        | isTypeVar ty = do
            params <- asks typeParameters
            let ty' = find ((== getId ty) . getId) params
            when (isNothing ty') $
                 tcError $ "Free type variables in type '" ++ show ty ++ "'"
            return (ty `withModeOf` fromJust ty')
        | isRefType ty = do
            result <- asks $ refTypeLookup ty
            case result of
              Just formal -> do
                formalBindings ty
                resolveRefType ty formal
              Nothing ->
                tcError $ "Couldn't find class or trait '" ++ show ty ++ "'"
        | isCapabilityType ty = resolveCapa ty
        | isMaybeType ty = do
            let resultType = getResultType ty
            resolveSingleType resultType
            return ty
        | isStringType ty = do
            tcWarning StringDeprecatedWarning
            return ty
        | otherwise = return ty
        where
          resolveCapa :: Type -> TypecheckM Type
          resolveCapa t
            | isEmptyCapability t = return t
            | isSingleCapability t = resolveType $ head $ typesFromCapability t
            | otherwise =
              mapM_ resolveSingleTrait (typesFromCapability ty) >> return ty

      resolveSingleTrait t = do
          result <- asks $ traitLookup t
          when (isNothing result) $
               tcError $ "Couldn't find trait '" ++ getId t ++ "'"

      resolveRefType actual formal
          | isModeless actual && not (isModeless formal) =
              resolveType $ actual `withModeOf` formal
          | isActiveClassType formal && not (isClassType actual) =
              resolveType $ activeClassTypeFromRefType actual
          | isPassiveClassType formal && not (isClassType actual) =
              resolveType $ passiveClassTypeFromRefType actual
          | isSharedClassType formal && not (isClassType actual) =
              resolveType $ sharedClassTypeFromRefType actual
          | isTraitType formal && not (isTraitType actual) =
              resolveType $ traitTypeFromRefType actual
          | isClassType actual = do
              unless (isModeless actual) $
                     tcError "Class types can not have modes"
              mapM_ (findField formal) (barredFields actual)
              return actual
          | isTraitType actual = do
              when (isModeless actual) $
                   tcError $ "No mode given to " ++ classOrTraitName actual
              when (isReadRefType actual) $ do
                   tdecl <- liftM fromJust . asks $ traitLookup actual
                   unless (isReadRefType formal ||
                           all safeValField (requiredFields tdecl)) $
                          tcError $ "Cannot give read mode to " ++
                                    classOrTraitName actual ++
                                    ". It has fields that are not val and safe"
              unless (null $ barredFields actual) $
                     tcError $ classOrTraitName actual ++
                               " cannot have barred types"
              return actual
          | otherwise =
              error $ "Util.hs: Cannot resolve unknown reftype: " ++ show formal

subtypeOf :: Type -> Type -> TypecheckM Bool
subtypeOf ty1 ty2
    | hasResultType ty1 && hasResultType ty2 =
        liftM (ty1 `hasSameKind` ty2 &&) $
              getResultType ty1 `subtypeOf` getResultType ty2
    | isNullType ty1 = return (isNullType ty2 || isRefType ty2)
    | isClassType ty1 && isClassType ty2 =
        ty1 `refSubtypeOf` ty2
    | isClassType ty1 && isTraitType ty2 = do
        traits <- getImplementedTraits ty1
        anyM (`subtypeOf` ty2) traits
    | isClassType ty1 && isCapabilityType ty2 = do
        capability <- findCapability ty1
        capability `capabilitySubtypeOf` ty2
    | isTupleType ty1 && isTupleType ty2 = do
      let argTys1 = getArgTypes ty1
          argTys2 = getArgTypes ty2
      results <- zipWithM subtypeOf argTys1 argTys2
      return $ and results
    | isTraitType ty1 && isTraitType ty2 =
        liftM (ty1 `modeSubtypeOf` ty2 &&)
              (ty1 `refSubtypeOf` ty2)
    | isTraitType ty1 && isCapabilityType ty2 = do
        let traits = typesFromCapability ty2
        allM (ty1 `subtypeOf`) traits
    | isCapabilityType ty1 && isTraitType ty2 = do
        let traits = typesFromCapability ty1
        anyM (`subtypeOf` ty2) traits
    | isCapabilityType ty1 && isCapabilityType ty2 =
        ty1 `capabilitySubtypeOf` ty2
    | isBottomType ty1 && (not . isBottomType $ ty2) = return True
    | otherwise = return (ty1 == ty2)
    where
      refSubtypeOf ref1 ref2
          | getId ref1 == getId ref2
          , params1 <- getTypeParameters ref1
          , params2 <- getTypeParameters ref2
          , barred1 <- barredFields ref1
          , barred2 <- barredFields ref2
          , length params1 == length params2 = do
              results <- zipWithM subtypeOf params1 params2
              return $ and results && null (barred1 \\ barred2) &&
                       matchingPristiness ref1 ref2
          | otherwise = return False
          where
            matchingPristiness ref1 ref2
                | isPristineRefType ref1 = True
                | not (isPristineRefType ref1) && not (isPristineRefType ref2) = True
                | otherwise = False

      capabilitySubtypeOf cap1 cap2 = do
        let traits1 = typesFromCapability cap1
            traits2 = typesFromCapability cap2
        allM (\t2 -> anyM (`subtypeOf` t2) traits1) traits2

-- | Convenience function for asserting distinctness of a list of
-- things. @assertDistinct "declaration" "field" [f : Foo, f :
-- Bar]@ will throw an error with the message "Duplicate
-- declaration of field 'f'".
assertDistinctThing :: (Eq a, Show a) =>
                       String -> String -> [a] -> TypecheckM ()
assertDistinctThing something kind l =
  let
    duplicates = l \\ nub l
    duplicate = head duplicates
  in
    unless (null duplicates) $
      tcError $ printf "Duplicate %s of %s %s" something kind $ show duplicate

-- | Convenience function for asserting distinctness of a list of
-- things that @HasMeta@ (and thus knows how to print its own
-- kind). @assertDistinct "declaration" [f : Foo, f : Bar]@ will
-- throw an error with the message "Duplicate declaration of field
-- 'f'".
assertDistinct :: (Eq a, AST.HasMeta a) =>
                  String -> [a] -> TypecheckM ()
assertDistinct something l =
  let
    duplicates = l \\ nub l
    first = head duplicates
  in
    unless (null duplicates) $
      tcError $ printf "Duplicate %s of %s" something $ AST.showWithKind first

classOrTraitName :: Type -> String
classOrTraitName ty
    | isClassType ty = "class '" ++ getId ty ++ "'"
    | isTraitType ty = "trait '" ++ getId ty ++ "'"
    | isCapabilityType ty = "capability '" ++ show ty ++ "'"
    | otherwise = error $ "Util.hs: No class or trait name for " ++
                          Ty.showWithKind ty

findField :: Type -> Name -> TypecheckM FieldDecl
findField ty f = do
  result <- asks $ fieldLookup ty f
  case result of
    Just fdecl -> return fdecl
    Nothing ->
        if f `elem` barredFields ty
        then tcError $ "Field '" ++ show f ++
                       "' is barred in type '" ++ show ty ++ "'"
        else tcError $ "No field '" ++ show f ++ "' in " ++
                       classOrTraitName ty

findMethod :: Type -> Name -> TypecheckM FunctionHeader
findMethod ty = liftM fst . findMethodWithCalledType ty

findMethodWithCalledType :: Type -> Name -> TypecheckM (FunctionHeader, Type)
findMethodWithCalledType ty name = do
  result <- asks $ methodAndCalledTypeLookup ty name
  when (isNothing result) $ tcError $
    concat [noMethod name, " in ", classOrTraitName ty]
  return $ fromJust result
  where
    noMethod (Name "_init") = "No constructor"
    noMethod n = concat ["No method '", show n, "'"]

findCapability :: (MonadReader Environment m) => Type -> m Type
findCapability ty = do
  result <- asks $ capabilityLookup ty
  return $ fromMaybe err result
    where
        err = error $ "Util.hs: No capability in " ++ classOrTraitName ty

formalBindings :: Type -> TypecheckM [(Type, Type)]
formalBindings actual = do
  origin <- asks $ refTypeLookupUnsafe actual
  matchTypeParameterLength origin actual
  let formals = getTypeParameters origin
      actuals = getTypeParameters actual
      bindings = zip formals actuals
  mapM_ matchModes bindings
  return bindings
  where
    matchModes (x, ty) =
        unless (isModeless x || ty `modeSubtypeOf` x) $
               tcError $ "Mode of type '" ++ show ty ++
                         "' does not match expected mode of type '" ++
                         show x ++ "'"

getImplementedTraits :: Type -> TypecheckM [Type]
getImplementedTraits ty
    | isClassType ty = do
        capability <- findCapability ty
        fBindings <- formalBindings ty
        let capability' = replaceTypeVars fBindings capability
        return $ typesFromCapability capability'
    | otherwise =
        error $ "Types.hs: Can't get implemented traits of type " ++ show ty

propagateResultType :: Type -> Expr -> Expr
propagateResultType ty e
    | hasResultingBody e =
        let body' = propagateResultType ty (body e)
        in setType ty e{body = body'}
    | Match{clauses} <- e =
        let clauses' = map propagateMatchClause clauses
        in setType ty e{clauses = clauses'}
    | Seq{eseq} <- e =
        let result = propagateResultType ty (last eseq)
        in setType ty e{eseq = init eseq ++ [result]}
    | IfThenElse{thn, els} <- e =
        setType ty e{thn = propagateResultType ty thn
                    ,els = propagateResultType ty els}
    | otherwise = setType ty e
    where
      hasResultingBody TypedExpr{} = True
      hasResultingBody Let{} = True
      hasResultingBody While{} = True
      hasResultingBody For{} = True
      hasResultingBody _ = False

      propagateMatchClause mc@MatchClause{mchandler} =
          mc{mchandler = propagateResultType ty mchandler}

isLinearType :: Type -> TypecheckM Bool
isLinearType = isLinearType' []
    where
      isLinearType' :: [Type] -> Type -> TypecheckM Bool
      isLinearType' checked ty = do
        let components = typeComponents (dropArrows ty)
            unchecked = components \\ checked
            classes = filter isClassType unchecked
        capabilities <- mapM findCapability classes
        liftM2 (||)
              (anyM isDirectlyLinear unchecked)
              (anyM (isLinearType' (checked ++ unchecked)) capabilities)

      isDirectlyLinear :: Type -> TypecheckM Bool
      isDirectlyLinear ty
          | isClassType ty = do
              cap <- findCapability ty
              let components = typeComponents (dropArrows ty) ++
                               typeComponents (dropArrows cap)
              return $ any isLinearRefType components
          | otherwise = do
              let components = typeComponents (dropArrows ty)
              return $ any isLinearRefType components

      dropArrows = typeMap dropArrow
      dropArrow ty
          | isArrowType ty = voidType
          | otherwise = ty

typeMinus ty1 ty2 = do
    Just fields1 <- asks $ fields ty1
    let fs1 = barredFields ty1
        fs2 = barredFields ty2
        barrableFields = map fname $ filter (not . isValField) fields1
        fs = fs1 `union` (barrableFields \\ fs2)
        (ty1', _) = mapAccumL (\t f -> (t `bar` f, undefined)) ty1 fs
    return $ unbox ty1'
    -- TODO: What if ty1 is not linear? What if it is borrowed ?!
