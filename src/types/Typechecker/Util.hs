{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util(TypecheckM
                       ,whenM
                       ,unlessM
                       ,tcError
                       ,resolveType
                       ,subtypeOf
                       ,assertDistinctThing
                       ,assertDistinct
                       ,classOrTraitName
                       ,findField
                       ,findMethod
                       ,findCapability
                       ,formalBindings
                       ) where

import Identifiers
import Types as Ty
import AST.AST as AST
import Data.List
import Text.Printf (printf)

-- Module dependencies
import Typechecker.TypeError
import Typechecker.Environment
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

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
    forall m . (MonadError TCError m, MonadReader Environment m) => m a

-- | convenience function for throwing an exception with the
-- current backtrace
tcError msg =
    do bt <- asks backtrace
       throwError $ TCError (msg, bt)

-- | @matchTypeParameterLength ty1 ty2@ ensures that the type parameter
-- lists of its arguments have the same length.
matchTypeParameterLength :: Type -> Type -> TypecheckM ()
matchTypeParameterLength ty1 ty2 = do
  let params1 = getTypeParameters ty1
      params2 = getTypeParameters ty2
  unless (length params1 == length params2) $
    tcError $ printf "'%s' expects %d type arguments, but '%s' has %d"
              (show ty1) (length params1)
              (show ty2) (length params2)

-- | @resolveType ty@ checks all the components of @ty@, resolving
-- reference types to traits or classes and making sure that any
-- type variables are in the current environment.
resolveType :: Type -> TypecheckM Type
resolveType = typeMapM resolveSingleType
    where
      resolveSingleType ty
        | isTypeVar ty = do
            params <- asks typeParameters
            unless (ty `elem` params) $
                   tcError $ "Free type variables in type '" ++ show ty ++ "'"
            return ty
        | isRefType ty = do
            result <- asks $ refTypeLookup ty
            case result of
              Just formal -> do
                matchTypeParameterLength formal ty
                return $ setTypeParameters formal $ getTypeParameters ty
              Nothing ->
                tcError $ "Couldn't find class or trait '" ++ show ty ++ "'"
        -- | isMaybeType ty = do
        --     let resultType = getResultType ty
        --     resolveSingleType resultType
        --     return ty
        | isCapabilityType ty = do
            let traits = traitsFromCapability ty
            mapM_ resolveSingleTrait traits
            return ty
        | otherwise = return ty
        where
          resolveSingleTrait t = do
            result <- asks $ traitLookup t
            when (isNothing result) $
                   tcError $ "Couldn't find trait '" ++ getId t ++ "'"

subtypeOf :: Type -> Type -> TypecheckM Bool
subtypeOf ty1 ty2
    | isMaybeType ty1 = maybeSubtypeOf ty1 ty2
    | isNullType ty1 = return (isNullType ty2 || isRefType ty2)
    | isClassType ty1 && isClassType ty2 =
        ty1 `refSubtypeOf` ty2
    | isClassType ty1 && isTraitType ty2 = do
        traits <- getImplementedTraits ty1
        anyM (`subtypeOf` ty2) traits
    | isClassType ty1 && isCapabilityType ty2 = do
        capability <- findCapability ty1
        ty1 `capabilitySubtypeOf` ty2
    | isTraitType ty1 && isTraitType ty2 =
        ty1 `refSubtypeOf` ty2
    | isTraitType ty1 && isCapabilityType ty2 = do
        let traits = traitsFromCapability ty2
        allM (ty1 `subtypeOf`) traits
    | isCapabilityType ty1 && isTraitType ty2 = do
        let traits = traitsFromCapability ty1
        anyM (`subtypeOf` ty2) traits
    | isCapabilityType ty1 && isCapabilityType ty2 =
        ty1 `capabilitySubtypeOf` ty2
    | otherwise = return (ty1 == ty2)
    where
      maybeSubtypeOf t1 t2
        | (isBottomType $ getResultType t1) && (not.isNullType) t2 = return True
        | otherwise = return $ (getResultType t1) == (getResultType t2)
      refSubtypeOf ref1 ref2
          | getId ref1 == getId ref2
          , params1 <- getTypeParameters ref1
          , params2 <- getTypeParameters ref2
          , length params1 == length params2 = do
              results <- zipWithM subtypeOf params1 params2
              return (and results)
          | otherwise = return False

      capabilitySubtypeOf cap1 cap2 = do
        let traits1 = traitsFromCapability cap1
            traits2 = traitsFromCapability cap2
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
    | otherwise = error $ "Util.hs: No class or trait name for " ++
                          Ty.showWithKind ty

findField :: Type -> Name -> TypecheckM FieldDecl
findField ty f = do
  result <- asks $ fieldLookup ty f
  case result of
    Just fdecl -> return fdecl
    Nothing -> tcError $ "No field '" ++ show f ++ "' in " ++
                         classOrTraitName ty

findMethod :: Type -> Name -> TypecheckM MethodDecl
findMethod ty name = do
  m' <- asks $ methodLookup ty name
  when (isNothing m') $ tcError $
    concat [no_method name, " in ", classOrTraitName ty]
  return $ fromJust m'
  where
    no_method (Name "_init") = "No constructor"
    no_method n = concat ["No method '", show n, "'"]

findCapability :: Type -> TypecheckM Type
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
  return $ zip formals actuals

getImplementedTraits :: Type -> TypecheckM [Type]
getImplementedTraits ty
    | isClassType ty = do
        capability <- findCapability ty
        fBindings <- formalBindings ty
        let capability' = replaceTypeVars fBindings capability
        return $ traitsFromCapability capability'
    | otherwise =
        error $ "Types.hs: Can't get implemented traits of type " ++ show ty
