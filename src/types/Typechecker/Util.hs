{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util(TypecheckM
                       ,CapturecheckM
                       ,whenM
                       ,anyM
                       ,allM
                       ,unlessM
                       ,tcError
                       ,pushError
                       ,tcWarning
                       ,resolveType
                       ,resolveTypeAndCheckForLoops
                       ,subtypeOf
                       ,assertDistinctThing
                       ,assertDistinct
                       ,findField
                       ,findMethod
                       ,findMethodWithCalledType
                       ,findCapability
                       ,propagateResultType
                       ,unifyTypes
                       ,isLinearType
                       ,isSubordinateType
                       ,isEncapsulatedType
                       ,isThreadType
                       ,isAliasableType
                       ,checkConjunction
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

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
  b <- p x
  if b
  then return $ Just x
  else findM p xs

-- | The monad in which all typechecking is performed. A function
-- of return type @TypecheckM Bar@ may read from an 'Environment'
-- and returns a @Bar@ or throws a typechecking exception.
type TypecheckM a =
    forall m . (MonadState [TCWarning] m,
                MonadError TCError m,
                MonadReader Environment m) => m a

type CapturecheckM a =
    forall m . (MonadError CCError m,
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
      (res, formal) <- resolveRefAtomType ty
      if isTypeSynonym res
      then resolveType res -- Force unfolding of type synonyms
      else resolveMode res formal
  | isCapabilityType ty =
      resolveCapa ty
  | isStringType ty = do
      tcWarning StringDeprecatedWarning
      return ty
  | isTypeSynonym ty = do
      unless (isModeless ty) $
        tcError $ CannotHaveModeError ty
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

resolveTypeAndCheckForLoops :: Type -> TypecheckM Type
resolveTypeAndCheckForLoops ty =
  evalStateT (typeMapM resolveAndCheck ty) []
  where
    resolveAndCheck ty
      | isRefAtomType ty = do
          seen <- get
          let tyid = getId ty
          when (tyid `elem` seen) $
            lift . tcError $ RecursiveTypesynonymError ty
          (res, formal) <- lift $ resolveRefAtomType ty
          when (isTypeSynonym res) $ put (tyid : seen)
          if isTypeSynonym res
          then typeMapM resolveAndCheck res
          else lift $ resolveMode res formal
      | otherwise = lift $ resolveType ty

resolveRefAtomType :: Type -> TypecheckM (Type, Type)
resolveRefAtomType ty
  | isRefAtomType ty = do
      result <- asks $ refTypeLookup ty
      case result of
        Just formal -> do
          matchTypeParameterLength formal ty
          let res = ty `resolvedFrom` formal
          return (res, formal)
        Nothing ->
          tcError $ UnknownRefTypeError ty
  | otherwise = error $ "Util.hs: " ++ Ty.showWithKind ty ++ " isn't a ref-type"

resolveMode :: Type -> Type -> TypecheckM Type
resolveMode actual formal
  | isModeless actual && not (isModeless formal) =
      resolveMode (actual `withModeOf` formal) formal
  | isClassType actual = do
      unless (isModeless actual) $
        tcError $ CannotHaveModeError actual
      mapM_ (findField formal) (barredFields actual)
      return actual
  | isTraitType actual = do
      when (isModeless actual) $
           tcError $ ModelessError actual
      unless (isModeless formal || actual `modeSubtypeOf` formal) $
           tcError $ ModeOverrideError formal
      when (isReadRefType actual) $
           unless (isReadRefType formal) $
                  tcError CannotGiveReadModeError
      unless (null $ barredFields actual) $
             tcError $ CannotHaveRestrictedFieldsError actual
      return actual
  | otherwise =
      error $ "Util.hs: Cannot resolve unknown reftype: " ++ show formal

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
        ty1 `refSubtypeOf` ty2
    | isClassType ty1 && isCapabilityType ty2 = do
        capability <- findCapability ty1
        capability `capabilitySubtypeOf` ty2
    | isTupleType ty1 && isTupleType ty2 = do
      let argTys1 = getArgTypes ty1
          argTys2 = getArgTypes ty2
      results <- zipWithM subtypeOf argTys1 argTys2
      return $ and results && length argTys1 == length argTys2
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
            preservesConjunctions = cap1 `preservesConjunctionsOf` cap2
        isSubsumed <- allM (\t2 -> anyM (`subtypeOf` t2) traits1) traits2
        return (preservesConjunctions && isSubsumed)

      preservesConjunctionsOf cap1 cap2 =
        let pairs1 = conjunctiveTypesFromCapability cap1
            pairs2 = conjunctiveTypesFromCapability cap2
        in all (`existsIn` pairs1) pairs2
      existsIn (left, right) =
        any (separates left right)
      separates left right (l, r) =
        all (`elem` l) left && all (`elem` r) right ||
        all (`elem` l) right && all (`elem` r) left

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

findField :: Type -> Name -> TypecheckM FieldDecl
findField ty f = do
  result <- asks $ fieldLookup ty f
  case result of
    Just fdecl -> return fdecl
    Nothing ->
        if f `elem` barredFields ty
        then tcError $ RestrictedFieldLookupError f ty
        else tcError $ FieldNotFoundError f ty

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

findCapability :: (MonadReader Environment m) => Type -> m Type
findCapability ty = do
  result <- asks $ capabilityLookup ty
  return $ fromMaybe err result
    where
        err = error $ "Util.hs: No capability in " ++ Ty.showWithKind ty

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

isLinearType :: (MonadReader Environment m) => Type -> m Bool
isLinearType ty
    | isRefAtomType ty = do
        Just flds <- asks $ fields ty
        if all (\f -> isValField f || isSpecField f) flds then
            return False
        else
            isLinearType' [] ty
    | otherwise = isLinearType' [] ty
    where
      isLinearType' :: (MonadReader Environment m) => [Type] -> Type -> m Bool
      isLinearType' checked ty = do
        let components = typeComponents (dropArrows ty)
            unchecked = components \\ checked
            classes = filter isClassType unchecked
        capabilities <- mapM findCapability classes
        liftM2 (||)
              (anyM isDirectlyLinear unchecked)
              (anyM (isLinearType' (checked ++ unchecked)) capabilities)

      isDirectlyLinear :: (MonadReader Environment m) => Type -> m Bool
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

isSubordinateType :: Type -> TypecheckM Bool
isSubordinateType ty
    | isCompositeType ty
    , traits <- typesFromCapability ty
      = anyM isSubordinateType traits
    | isPassiveClassType ty = do
        capability <- findCapability ty
        isSubordinateType capability
    | hasResultType ty =
        isSubordinateType (getResultType ty)
    | isTupleType ty =
        anyM isSubordinateType (getArgTypes ty)
    | otherwise = return $ isSubordinateRefType ty

isEncapsulatedType :: Type -> TypecheckM Bool
isEncapsulatedType ty
    | isCompositeType ty
    , traits <- typesFromCapability ty
      = allM isSubordinateType traits
    | isPassiveClassType ty = do
        capability <- findCapability ty
        isEncapsulatedType capability
    | hasResultType ty =
        isEncapsulatedType (getResultType ty)
    | isTupleType ty =
        allM isEncapsulatedType (getArgTypes ty)
    | otherwise = return $ isSubordinateRefType ty

isThreadType :: Type -> TypecheckM Bool
isThreadType ty
    | isCompositeType ty
    , traits <- typesFromCapability ty
      = anyM isThreadType traits
    | isPassiveClassType ty = do
        capability <- findCapability ty
        isThreadType capability
    | hasResultType ty =
        isThreadType (getResultType ty)
    | isTupleType ty =
        anyM isThreadType (getArgTypes ty)
    | otherwise = return $ isThreadRefType ty

isUnsafeType :: Type -> TypecheckM Bool
isUnsafeType ty
    | isPassiveClassType ty = do
        capability <- findCapability ty
        isUnsafeType capability
    | otherwise = return $
                  any isUnsafeRefType $ typeComponents ty

isAliasableType :: Type -> TypecheckM Bool
isAliasableType ty =
    anyM (\f -> f ty)
         [return . isSafeType
         ,isThreadType
         ,isSubordinateType
         ,isUnsafeType
         ]

checkConjunction :: Type -> [Type] -> TypecheckM ()
checkConjunction source sinks
  | isCompositeType source = do
      let sourceConjunctions = conjunctiveTypesFromCapability source
      mapM_ (\ty -> wellFormedConjunction sourceConjunctions
                                          (sinks \\ [ty]) ty) sinks
  | isPassiveClassType source = do
      cap <- findCapability source
      when (isIncapability cap) $
           tcError $ CannotUnpackError source
      when (source `elem` sinks) $
           tcError $ CannotInferUnpackingError source
      checkConjunction cap sinks
  | otherwise =
      return ()
  where
    wellFormedConjunction pairs siblings ty = do
      when (null pairs) $
        tcError $ MalformedConjunctionError ty (head siblings) source
      let nonDisjoints =
            filter (\ty' -> any (not . singleConjunction ty ty') pairs) siblings
          nonDisjoint = head nonDisjoints
      unless (null nonDisjoints) $
        tcError $ MalformedConjunctionError ty nonDisjoint source
    singleConjunction ty1 ty2 (tys1, tys2) =
        ty1 `elem` tys1 && ty2 `elem` tys2 ||
        ty1 `elem` tys2 && ty2 `elem` tys1