{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util(TypecheckM
                       ,whenM
                       ,anyM
                       ,unlessM
                       ,concatMapM
                       ,tcError
                       ,pushError
                       ,tcWarning
                       ,resolveType
                       ,resolveTypeAndCheckForLoops
                       ,subtypeOf
                       ,assertDistinctThing
                       ,assertDistinct
                       ,findTrait
                       ,findField
                       ,findMethod
                       ,findMethodWithCalledType
                       ,findCapability
                       ,findVar
                       ,propagateResultType
                       ,unifyTypes
                       ) where

import Identifiers
import Types as Ty
import AST.AST as AST
import Data.List
import Text.Printf (printf)
import Debug.Trace

-- Module dependencies
import Typechecker.TypeError
import Typechecker.Environment
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow(second)
import Control.Monad.State
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

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
  b <- p x
  if b
  then return $ Just x
  else findM p xs

-- | A version of 'concatMap' that works with a monadic predicate.
-- Source: https://hackage.haskell.org/package/extra-1.5/docs/src/Control-Monad-Extra.html
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
    where
      f x xs = do x <- op x
                  if null x
                  then xs
                  else do
                    xs <- xs
                    return $ x++xs

-- | The monad in which all typechecking is performed. A function
-- of return type @TypecheckM Bar@ may read from an 'Environment'
-- and returns a @Bar@ or throws a typechecking exception.
type TypecheckM a =
    forall m . (MonadState [TCWarning] m,
                MonadError TCError m,
                MonadReader Environment m) => m a

-- | Convenience function for throwing an exception with the
-- current backtrace
tcError err =
    do bt <- asks backtrace
       throwError $ TCError err bt

-- | Push the expression @expr@ and throw error err
pushError expr err = local (pushBT expr) $ tcError err

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
    tcError $ WrongNumberOfTypeParametersError
              ty1 (length params1) ty2 (length params2)

-- | @resolveType ty@ checks all the components of @ty@, resolving
-- reference types to traits or classes and making sure that any
-- type variables are in the current environment.
resolveType :: Type -> TypecheckM Type
resolveType = typeMapM resolveSingleType

resolveSingleType :: Type -> TypecheckM Type
resolveSingleType ty
  | isTypeVar ty = do
      params <- asks typeParameters
      unless (ty `elem` params) $
             tcError $ FreeTypeVariableError ty
      return ty
  | isRefAtomType ty = do
      res <- resolveRefAtomType ty
      if isTypeSynonym res
      then resolveType res -- Force unfolding of type synonyms
      else return res
  | isCapabilityType ty =
      resolveCapa ty
  | isStringType ty = do
      tcWarning StringDeprecatedWarning
      return ty
  | isTypeSynonym ty = do
      let unfolded = unfoldTypeSynonyms ty
      resolveType unfolded
  | otherwise = return ty
  where
    resolveCapa t = do
        let traits = typesFromCapability t
        mapM_ resolveSingleTrait traits
        assertDistinctThing "occurrence" "trait" traits
        return t
    resolveSingleTrait t
          | isRefAtomType t = do
              result <- asks $ traitLookup t
              when (isNothing result) $
                 tcError $ UnknownTraitError t
          | otherwise =
              tcError $ MalformedCapabilityError t

resolveTypeAndCheckForLoops ty =
  evalStateT (typeMapM resolveAndCheck ty) []
  where
    resolveAndCheck ty
      | isRefAtomType ty = do
          seen <- get
          let tyid = getId ty
          when (tyid `elem` seen) $
            lift . tcError $ RecursiveTypesynonymError ty
          res <- lift $ resolveRefAtomType ty
          when (isTypeSynonym res) $ put (tyid : seen)
          if isTypeSynonym res
          then typeMapM resolveAndCheck res
          else return res
      | otherwise = lift $ resolveType ty

resolveRefAtomType :: Type -> TypecheckM Type
resolveRefAtomType ty
  | isRefAtomType ty = do
      result <- asks $ refTypeLookup ty
      case result of
        Just [] ->
          tcError $ UnknownRefTypeError ty
        Just [formal] -> do
          matchTypeParameterLength formal ty
          let res = formal `setTypeParameters` getTypeParameters ty
          return res
        Just l ->
          tcError $ AmbiguousTypeError ty l
        Nothing ->
          tcError $ UnknownNamespaceError (getRefNamespace ty)
  | otherwise = error $ "Util.hs: " ++ Ty.showWithKind ty ++ " isn't a ref-type"

subtypeOf :: Type -> Type -> TypecheckM Bool
subtypeOf ty1 ty2
    | isArrowType ty1 && isArrowType ty2 = do
        let argTys1 = getArgTypes ty1
            argTys2 = getArgTypes ty2
            resultTy1 = getResultType ty1
            resultTy2 = getResultType ty2
        contravariance <- liftM and $ zipWithM subtypeOf argTys2 argTys1
        covariance <- resultTy1 `subtypeOf` resultTy2
        return $ length argTys1 == length argTys2 &&
                 contravariance && covariance
    | hasResultType ty1 && hasResultType ty2 =
        liftM (ty1 `hasSameKind` ty2 &&) $
              getResultType ty1 `subtypeOf` getResultType ty2
    | isNullType ty1 = return (isNullType ty2 || isRefType ty2)
    | isClassType ty1 && isClassType ty2 =
        return $ ty1 == ty2
    | isClassType ty1 && isCapabilityType ty2 = do
        capability <- findCapability ty1
        capability `capabilitySubtypeOf` ty2
    | isTupleType ty1 && isTupleType ty2 = do
      let argTys1 = getArgTypes ty1
          argTys2 = getArgTypes ty2
      results <- zipWithM subtypeOf argTys1 argTys2
      return $ and results && length argTys1 == length argTys2
    | isTraitType ty1 && isTraitType ty2 =
        return $ ty1 == ty2
    | isTraitType ty1 && isCapabilityType ty2 = do
        let traits = typesFromCapability ty2
        allM (ty1 `subtypeOf`) traits
    | isCapabilityType ty1 && isTraitType ty2 = do
        let traits = typesFromCapability ty1
        anyM (`subtypeOf` ty2) traits
    | isCapabilityType ty1 && isCapabilityType ty2 =
        ty1 `capabilitySubtypeOf` ty2
    | isUnionType ty1 && isUnionType ty2 = do
        let members1 = unionMembers ty1
            members2 = unionMembers ty2
        allM (\ty -> anyM (ty `subtypeOf`) members2) members1
    | isUnionType ty1 = do
        let members1 = unionMembers ty1
        allM (`subtypeOf` ty2) members1
    | isUnionType ty2 = do
        let members2 = unionMembers ty2
        anyM (ty1 `subtypeOf`) members2
    | isBottomType ty1 && (not . isBottomType $ ty2) = return True
    | isNumeric ty1 && isNumeric ty2 =
        return $ ty1 `numericSubtypeOf` ty2
    | otherwise = return (ty1 == ty2)
    where
      capabilitySubtypeOf cap1 cap2 = do
        let traits1 = typesFromCapability cap1
            traits2 = typesFromCapability cap2
        allM (\t2 -> anyM (`subtypeOf` t2) traits1) traits2

      numericSubtypeOf ty1 ty2
          | isIntType ty1 && isRealType ty2 = True
          | isIntType ty1 && isUIntType ty2 = True
          | isUIntType ty1 && isIntType ty2 = True
          | otherwise = ty1 == ty2

equivalentTo :: Type -> Type -> TypecheckM Bool
equivalentTo ty1 ty2 = do
  b1 <- ty1 `subtypeOf` ty2
  b2 <- ty2 `subtypeOf` ty1
  return $ b1 && b2

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
      tcError $ DuplicateThingError something (kind ++ " " ++ show duplicate)

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
      tcError $ DuplicateThingError something (AST.showWithKind first)

findTrait :: Type -> TypecheckM TraitDecl
findTrait t = do
  result <- asks $ traitLookup t
  case result of
    Just [] ->
      tcError $ UnknownTraitError t
    Just [tdecl] ->
      return tdecl
    Just l ->
      tcError $ AmbiguousTypeError t (map tname l)
    Nothing ->
      tcError $ UnknownNamespaceError (getRefNamespace t)

findField :: Type -> Name -> TypecheckM FieldDecl
findField ty f = do
  result <- asks $ fieldLookup ty f
  case result of
    Just fdecl -> return fdecl
    Nothing -> tcError $ FieldNotFoundError f ty

findMethod :: Type -> Name -> TypecheckM FunctionHeader
findMethod ty = liftM fst . findMethodWithCalledType ty

findMethodWithCalledType :: Type -> Name -> TypecheckM (FunctionHeader, Type)
findMethodWithCalledType ty name
    | isUnionType ty = do
        let members = unionMembers ty
        results <- mapM (`findMethodWithCalledType` name) members
        let result@(_, calledType) = head results
        unless (all (==calledType) (map snd results)) $
               tcError $ UnionMethodAmbiguityError ty name
        return result
    | otherwise = do
        result <- asks $ methodAndCalledTypeLookup ty name
        when (isNothing result) $
          tcError $ MethodNotFoundError name ty
        return $ fromJust result

findCapability :: Type -> TypecheckM Type
findCapability ty = do
  result <- asks $ capabilityLookup ty
  return $ fromMaybe err result
    where
        err = error $ "Util.hs: No capability in " ++ Ty.showWithKind ty

findVar :: QualifiedName -> TypecheckM (Maybe (QualifiedName, Type))
findVar x = do
  result <- asks $ varLookup x
  case result of
    Just [] ->
      return Nothing
    Just [qvar] ->
      return (Just qvar)
    Just l ->
      tcError $ AmbiguousNameError x l
    Nothing ->
      tcError $ UnknownNamespaceError (qnspace x)

getImplementedTraits :: Type -> TypecheckM [Type]
getImplementedTraits ty
    | isClassType ty = do
        capability <- findCapability ty
        return $ typesFromCapability capability
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

typeIsUnifiable ty
    | isPassiveClassType ty = do
        capability <- findCapability ty
        return $ not (isIncapability capability)
    | isCapabilityType ty = return $ not (isIncapability ty)
    | otherwise =
        return $
        isUnionType ty ||
        isNullType ty ||
        isBottomType ty

isUnifiableWith ty types
    | isArrowType ty = return False
    | hasResultType ty &&
      all (hasSameKind ty) types =
          isUnifiableWith (getResultType ty) (map getResultType types)
    | isPassiveClassType ty = do
      capability <- findCapability ty
      if isIncapability capability
      then return $ all (==ty) types
      else allM typeIsUnifiable types
    | otherwise = do
        tyUniable <- typeIsUnifiable ty
        tysUniable <- allM typeIsUnifiable types
        return $ tyUniable && tysUniable &&
                 not (isNullType ty) && not (isBottomType ty)

unifyTypes :: [Type] -> TypecheckM (Maybe Type)
unifyTypes tys = do
  result <- findM (`isUnifiableWith` tys) tys
  case result of
    Just ty -> do
      union <- doUnifyTypes ty tys
      liftM Just $ lub union
    Nothing ->
      return Nothing
  where
    lub union = do
      let members = unionMembers union
      bounds <- filterM (\t -> allM (`subtypeOf` t) members) members
      if null bounds
      then return union
      else return $ head bounds

doUnifyTypes :: Type -> [Type] -> TypecheckM Type
doUnifyTypes inter [] = return inter
doUnifyTypes inter args@(ty:tys)
    | hasResultType inter = do
        let res = getResultType inter
            args' = map getResultType args
        res' <- doUnifyTypes res args'
        return $ setResultType inter res'
    | isNullType ty =
        doUnifyTypes inter tys
    | isBottomType ty =
        doUnifyTypes inter tys
    | isPassiveClassType ty =
        if ty == inter
        then doUnifyTypes inter tys
        else do
          cap <- findCapability ty
          doUnifyTypes inter (cap:tys)
    | isPassiveClassType inter = do
        cap <- findCapability inter
        doUnifyTypes cap (ty:tys)
    | isCapabilityType ty = do
        let members = unionMembers inter
        isSubsumed <- anyM (ty `equivalentTo`) members
        if isSubsumed
        then doUnifyTypes inter tys
        else do
          unlessM (anyM (\t -> allM (`subtypeOf` t) members)
                        (typesFromCapability ty)) $
                 tcError $ MalformedUnionTypeError ty inter
          doUnifyTypes (unionType inter ty) tys
    | isUnionType ty =
        doUnifyTypes inter (unionMembers ty ++ tys)
    | otherwise =
        error "Util.hs: Tried to form an union without a capability"
