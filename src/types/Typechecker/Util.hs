{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util where

import Types
import qualified AST.AST as AST
import Data.List
import Data.Maybe
import Text.Printf (printf)

-- Module dependencies
import Typechecker.TypeError
import Typechecker.Environment
import Control.Monad.Reader
import Control.Monad.Except

-- | Convenience function for throwing an exception with the
-- current backtrace
tcError msg =
    do bt <- asks backtrace
       throwError $ TCError (msg, bt)

-- | @matchTypeParameterLength ty1 ty2@ ensures that the type parameter
-- lists of its arguments have the same length.
matchTypeParameterLength ::
    Type -> Type -> ExceptT TCError (Reader Environment) ()
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
resolveType :: Type -> ExceptT TCError (Reader Environment) Type
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
                    resolveRefType ty formal
                Nothing ->
                    tcError $ "Couldn't find class or trait '" ++ show ty ++ "'"
          | otherwise = return ty

      resolveRefType actual formal
          | not (isClassType actual) &&
            isModeless actual && not (isModeless formal) =
              resolveType $ formal `withTypeParametersOf` actual
          | not $ isUntypedRef actual =
              return actual
          | isActiveClassType formal =
              resolveType $ activeClassTypeFromRefType actual capability
          | isPassiveClassType formal =
              resolveType $ passiveClassTypeFromRefType actual capability
          | isTraitType formal =
              resolveType $ traitTypeFromRefType actual
          | otherwise =
              error $ "Util.hs: Cannot resolve unknown reftype: " ++ show formal
          where
            capability = getCapability (formal `withTypeParametersOf` actual)

assertResolvedTraits :: Type -> ExceptT TCError (Reader Environment) ()
assertResolvedTraits ty = mapM_ resolve $ getImplementedTraits ty
    where
      resolve t = do
        result <- asks $ traitLookup t
        unless (isJust result) $
               tcError $ "Couldn't find trait '" ++ getId t ++ "'"

assertResolvedModes :: Type -> ExceptT TCError (Reader Environment) ()
assertResolvedModes ty =
    let
        modeless = filter isModeless (getImplementedTraits ty)
        first = head modeless
    in
      unless (null modeless) $
             tcError $ "No mode given to trait '" ++ show first ++ "'"

-- | Convenience function for asserting distinctness of a list of
-- things. @assertDistinct "declaration" "field" [f : Foo, f :
-- Bar]@ will throw an error with the message "Duplicate
-- declaration of field 'f'".
assertDistinctThing ::
    (MonadError TCError m, MonadReader Environment m, Eq a, Show a) =>
    String -> String -> [a] -> m ()
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
assertDistinct :: (MonadError TCError m, MonadReader Environment m,
  Eq a, AST.HasMeta a) => String -> [a] -> m ()
assertDistinct something l =
  let
    duplicates = l \\ nub l
    first = head duplicates
  in
    unless (null duplicates) $
      tcError $ printf "Duplicate %s of %s" something $ AST.showWithKind first
