{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util where

import Types
import qualified AST.AST as AST
import Data.List
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
              (show ty1) (length params1)
              (show ty2) (length params2)

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
                    return $ setTypeParameters formal $ getTypeParameters ty
                Nothing ->
                    tcError $ "Couldn't find class or trait '" ++ show ty ++ "'"
          | otherwise = return ty

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
