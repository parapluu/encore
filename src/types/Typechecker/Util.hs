{-|
  Utility functions shared by several modules of "Typechecker".
-}

module Typechecker.Util(TypecheckM
                       ,whenM
                       ,anyM
                       ,allM
                       ,unlessM
                       ,concatMapM
                       ,tcError
                       ,pushError
                       ,tcWarning
                       ,pushWarning
                       ,checkType
                       ,resolveType
                       ,resolveTypeAndCheckForLoops
                       ,findFormalRefType
                       ,isKnownRefType
                       ,assertSafeTypeArguments
                       ,checkTypeArgumentBounds
                       ,subtypeOf
                       ,assertSubtypeOf
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
                       ,uniquifyTypeVars
                       ,checkValidUseOfBreak
                       ,checkValidUseOfContinue
                       ,abstractTraitFrom
                       ,isLinearType
                       ,isSubordinateType
                       ,isEncapsulatedType
                       ,isLocalType
                       ,isPassiveType
                       ,isActiveType
                       ,isSharedType
                       ,isAliasableType
                       ,isSharableType
                       ,checkConjunction
                       ,includesMarkerTrait
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

pushWarning expr wrn = local (pushBT expr) $ tcWarning wrn

checkValidUseOfBreak = Typechecker.TypeError.validUseOfBreak . bt
checkValidUseOfContinue = Typechecker.TypeError.validUseOfContinue . bt

-- | @matchTypeParameterLength ty1 ty2@ ensures that the type parameter
-- lists of its arguments have the same length.
matchTypeParameterLength :: Type -> Type -> TypecheckM ()
matchTypeParameterLength ty1 ty2 = do
  let params1 = getTypeParameters ty1
      params2 = getTypeParameters ty2
  unless (length params1 == length params2) $
    tcError $ WrongNumberOfTypeParametersError
              ty1 (length params1) ty2 (length params2)

checkType :: Type -> TypecheckM Type
checkType = typeMapM checkSingleType
  where
    checkSingleType ty
      | isRefAtomType ty = do
          ty' <- resolveType ty

          -- ty' could be an unfolded type synonym
          when (isRefAtomType ty') $ do
            formal <- findFormalRefType ty'
            let formalTypeParams = getTypeParameters formal
                actualTypeParams = getTypeParameters ty'
            assertSafeTypeArguments formalTypeParams actualTypeParams
            checkTypeArgumentBounds formalTypeParams actualTypeParams
          return ty'
      | otherwise = resolveType ty

-- | @resolveType ty@ checks all the components of @ty@, resolving
-- reference types to traits or classes and making sure that any
-- type variables are in the current environment.
resolveType :: Type -> TypecheckM Type
resolveType = typeMapM resolveSingleType

resolveSingleType :: Type -> TypecheckM Type
resolveSingleType ty
  | isTypeVar ty = do
      params <- asks typeParameters
      case find ((getId ty ==) . getId) params of
        Just ty' -> return $ ty' `withBoxOf` ty
        Nothing  -> tcError $ FreeTypeVariableError ty
  | isRefAtomType ty = do
      res <- resolveRefAtomType ty
      formal <- findFormalRefType ty
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
  | isArrayType ty = do
      let elementType = getResultType ty
      when (isStackboundType elementType) $
           tcError $ StackboundArrayTypeError elementType
      return ty
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
          res <- lift $ resolveRefAtomType ty
          formal <- lift $ findFormalRefType ty
          when (isTypeSynonym res) $ put (tyid : seen)
          if isTypeSynonym res
          then typeMapM resolveAndCheck res
          else lift $ resolveMode res formal
      | otherwise = lift $ resolveType ty

-- | Resolve a ref atom type (class type, trait type or typedef)
-- and ensure that it has the correct number type arguments.
resolveRefAtomType :: Type -> TypecheckM Type
resolveRefAtomType ty = do
  formal <- findFormalRefType ty
  matchTypeParameterLength formal ty
  let res = formal `setTypeParameters` getTypeParameters ty
                   `withModeOf` ty
                   `withBoxOf` ty
  return res

-- | Find the formal version of a type with any type parameters of
-- that type uninstantied. Throws a typechecking error if a formal
-- type is not found or if several matching formal types are
-- found.
findFormalRefType :: Type -> TypecheckM Type
findFormalRefType ty
  | isRefAtomType ty = do
      result <- asks $ refTypeLookup ty
      case result of
        Just [] ->
          tcError $ UnknownRefTypeError ty
        Just [formal] ->
          case getRefNamespace formal of
            Just ns -> do
              unless (isExplicitNamespace ns) $
                     tcError $ UnknownRefTypeError ty
              return formal
            Nothing ->
              error $ "Util.hs: No namespace after resolving type " ++ show ty
        Just l ->
          tcError $ AmbiguousTypeError ty l
        Nothing ->
          tcError $ UnknownNamespaceError (getRefNamespace ty)
  | otherwise = error $ "Util.hs: " ++ Ty.showWithKind ty ++ " isn't a ref-type"

resolveMode :: Type -> Type -> TypecheckM Type
resolveMode actual formal
  | isModeless actual && not (isModeless formal) =
      resolveMode (actual `withModeOf` formal) formal
  | isClassType actual = do
      when (isModeless formal) $
           unless (isModeless actual) $
                  tcError $ CannotHaveModeError actual
      unless (actual `modeSubtypeOf` formal) $
             tcError $ ModeOverrideError formal
      when (isSharableSingleType actual) $
           tcError $ CannotGiveSharableModeError actual
      return actual
  | isTraitType actual = do
      when (isModeless actual) $
           tcError $ ModelessError actual
      unless (hasMinorMode formal || actual `modeSubtypeOf` formal) $
           tcError $ ModeOverrideError formal
      when (isReadSingleType actual) $
           unless (isReadSingleType formal) $
                  tcError $ CannotGiveReadModeError actual
      when (isSharableSingleType actual) $
           tcError $ CannotGiveSharableModeError actual
      return actual
  | otherwise =
      error $ "Util.hs: Cannot resolve unknown reftype: " ++ show formal

assertSafeTypeArguments :: [Type] -> [Type] -> TypecheckM ()
assertSafeTypeArguments = zipWithM_ assertSafeTypeArgument
  where
    assertSafeTypeArgument formal arg
      | isModeless formal = do
          unlessM (isAliasableType arg) $
                  tcError $ UnsafeTypeArgumentError formal arg
          when (isArrayType arg) $
               tcWarning ArrayTypeArgumentWarning
      | isClassType arg
      , isModeless arg = do
          cap <- findCapability arg
          let traits = typesFromCapability cap
          mapM_ (assertSafeTypeArgument formal) traits
          `catchError` \(TCError _ bt) ->
                           throwError $
                             TCError (UnsafeTypeArgumentError formal arg) bt
      | otherwise = do
          unlessM (isSharableType arg) $
            unless (arg `modeSubtypeOf` formal || hasMinorMode arg) $
              tcError $ UnsafeTypeArgumentError formal arg
          when (isArrayType arg) $
           tcWarning ArrayTypeArgumentWarning

checkTypeArgumentBounds :: [Type] -> [Type] -> TypecheckM ()
checkTypeArgumentBounds params args =
  let bindings = zip params args
  in zipWithM_ (checkBound bindings) params args
  where
    checkBound bindings param arg
      | Just bound <- getBound param = do
          unless (isCapabilityType bound) $
                 tcError $ MalformedCapabilityError bound
          arg `assertSubtypeOf` replaceTypeVars bindings bound
      | otherwise = return ()

subtypeOf :: Type -> Type -> TypecheckM Bool
subtypeOf sub super
    | isStackboundType sub =
        liftM (isStackboundType super &&) $ unbox sub `subtypeOf` unbox super
    | isArrowType sub && isArrowType super = do
        let subTypeParams = getTypeParameters sub
            superTypeParams = getTypeParameters super
            bindings = zip superTypeParams subTypeParams
            subResultTy = getResultType sub
            superResultType = replaceTypeVars bindings $ getResultType super
            subArgTys = getArgTypes sub
            superArgTys = map (replaceTypeVars bindings) $ getArgTypes super
            subBounds = map getBound subTypeParams
            superBounds = map (fmap (replaceTypeVars bindings) . getBound) superTypeParams
        contravariance <- liftM and $ zipWithM subtypeOf superArgTys subArgTys
        covariance <- subResultTy `subtypeOf` superResultType
        return $ length subArgTys == length superArgTys &&
                 length subTypeParams == length superTypeParams &&
                 subBounds == superBounds &&
                 sub `modeSubtypeOf` super &&
                 contravariance && covariance
    | isArrayType sub && isArrayType super =
        getResultType sub `equivalentTo` getResultType super
    | hasResultType sub && hasResultType super =
        liftM (sub `hasSameKind` super &&) $
              getResultType sub `subtypeOf` getResultType super
    | isNullType sub = return (isNullType super || isRefType super)
    | isClassType sub && isClassType super =
        return $ sub == super
    | isClassType sub && isCapabilityType super = do
        capability <- findCapability sub
        capability `capabilitySubtypeOf` super
    | isTupleType sub && isTupleType super = do
      let subArgTys = getArgTypes sub
          superArgTys = getArgTypes super
      results <- zipWithM subtypeOf subArgTys superArgTys
      return $ and results && length subArgTys == length superArgTys
    | isAbstractTraitType sub && isTraitType super =
        return $ sub == abstractTraitFromTraitType super
    | isTraitType sub && isAbstractTraitType super =
        return $ abstractTraitFromTraitType sub == super
    | isTraitType sub && isTraitType super =
        return $ sub `modeSubtypeOf` super &&
                 sub == super
    | isTraitType sub && isCapabilityType super = do
        let traits = typesFromCapability super
        allM (sub `subtypeOf`) traits
    | isCapabilityType sub && isTraitType super = do
        let traits = typesFromCapability sub
        anyM (`subtypeOf` super) traits
    | isCapabilityType sub && isCapabilityType super =
        sub `capabilitySubtypeOf` super
    | isUnionType sub && isUnionType super = do
        let members1 = unionMembers sub
            members2 = unionMembers super
        allM (\ty -> anyM (ty `subtypeOf`) members2) members1
    | isUnionType sub = do
        let members1 = unionMembers sub
        allM (`subtypeOf` super) members1
    | isUnionType super = do
        let members2 = unionMembers super
        anyM (sub `subtypeOf`) members2
    | isBottomType sub && (not . isBottomType $ super) = return True
    | isNumeric sub && isNumeric super =
        return $ sub `numericSubtypeOf` super
    | isTypeVar sub && not (isTypeVar super)
    , Just bound <- getBound sub
      = bound `subtypeOf` super
    | otherwise = return (sub == super)
    where
      capabilitySubtypeOf subCap superCap = do
        let traits1 = typesFromCapability subCap
            traits2 = typesFromCapability superCap
            preservesConjunctions = subCap `preservesConjunctionsOf` superCap
            preservesModes =
              all (\t1 -> isReadSingleType t1 || isLinearSingleType t1 ||
                          any (`modeSubtypeOf` t1) traits2) traits1
        isSubsumed <- allM (\t2 -> anyM (`subtypeOf` t2) traits1) traits2
        return (preservesConjunctions && preservesModes && isSubsumed)

      preservesConjunctionsOf subCap superCap =
        let pairs1 = conjunctiveTypesFromCapability subCap
            pairs2 = conjunctiveTypesFromCapability superCap
        in all (`existsIn` pairs1) pairs2
      existsIn (left, right) =
        any (separates left right)
      separates left right (l, r) =
        all (`elem` l) left && all (`elem` r) right ||
        all (`elem` l) right && all (`elem` r) left

      numericSubtypeOf sub super
          | isIntType sub && isRealType super = True
          | isIntType sub && isUIntType super = True
          | isUIntType sub && isIntType super = True
          | otherwise = sub == super

assertSubtypeOf :: Type -> Type -> TypecheckM ()
assertSubtypeOf sub super =
    unlessM (sub `subtypeOf` super) $ do
      capability <- if isClassType sub
                    then do
                      cap <- findCapability sub
                      if isIncapability cap
                      then return Nothing
                      else return $ Just cap
                    else return Nothing
      case capability of
        Just cap ->
            tcError $ TypeWithCapabilityMismatchError sub cap super
        Nothing ->
            tcError $ TypeMismatchError sub super

equivalentTo :: Type -> Type -> TypecheckM Bool
equivalentTo ty1 ty2 = do
  b1 <- ty1 `subtypeOf` ty2
  b2 <- ty2 `subtypeOf` ty1
  return $ b1 && b2

includesMarkerTrait :: Type -> Type -> TypecheckM Bool
includesMarkerTrait ty trait
  | isTraitType ty = return $ ty == trait
  | isClassType ty = do
      cap <- findCapability ty
      includesMarkerTrait cap trait
  | isCapabilityType ty = do
      let traits = typesFromCapability ty
      anyM (`includesMarkerTrait` trait) traits
  | otherwise = return False

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

isKnownRefType :: Type -> TypecheckM Bool
isKnownRefType ty
  | isRefAtomType ty = do
      result <- asks $ refTypeLookup ty
      case result of
        Just [] -> return False
        Just [ref] -> return $ maybe False isExplicitNamespace
                               (getRefNamespace ref)
        Just l -> tcError $ AmbiguousTypeError ty l
        Nothing -> return False
  | isCapabilityType ty = do
      let traits = typesFromCapability ty
      results <- mapM isKnownRefType traits
      return $ and results
  | isUnionType ty = do
      let members = unionMembers ty
      results <- mapM isKnownRefType members
      return $ and results
  | otherwise = return True


findField :: Type -> Name -> TypecheckM FieldDecl
findField ty f = do
  isKnown <- isKnownRefType ty
  unless isKnown $
         tcError $ UnknownTypeUsageError "access field of" ty
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
    | isTypeVar ty
    , Just bound <- getBound ty = do
        (header, bound') <- findMethodWithCalledType bound name
        return (header, setBound (Just bound') ty)
    | otherwise = do
        isKnown <- isKnownRefType ty
        unless isKnown $
               tcError $ UnknownTypeUsageError "call method on" ty
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
    | isClassType ty = do
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
      all hasResultType types &&
      all (hasSameKind ty) types =
          isUnifiableWith (getResultType ty) (map getResultType types)
    | isClassType ty = do
      capability <- findCapability ty
      if isIncapability capability
      then return $ all (==ty) types
      else allM typeIsUnifiable types
    | otherwise = do
        tyUniable <- typeIsUnifiable ty
        tysUniable <- allM typeIsUnifiable types
        return $ tyUniable && tysUniable &&
                 not (isNullType ty)

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
    | isClassType ty =
        if ty == inter
        then doUnifyTypes inter tys
        else do
          cap <- findCapability ty
          doUnifyTypes inter (cap:tys)
    | isClassType inter = do
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

uniquifyTypeVars :: [Type] -> Type -> TypecheckM Type
uniquifyTypeVars params = typeMapM (uniquifyTypeVar params)

uniquifyTypeVar :: [Type] -> Type -> TypecheckM Type
uniquifyTypeVar params ty
  | isTypeVar ty = do
      localTypeVars <- asks typeParameters
      boundTypeVars <- map fst <$> asks bindings
      if ty `elem` params && (ty `elem` localTypeVars || ty `elem` boundTypeVars)
      then uniquify ty
      else return ty
  | otherwise = return ty
  where
    uniquify :: Type -> TypecheckM Type
    uniquify ty = do
      localTypeVars <- asks typeParameters
      boundTypeVars <- map fst <$> asks bindings
      let candidates = map (appendToTypeVar ty) [0..]
      return $ fromJust $
               find (`notElem` localTypeVars ++ boundTypeVars) candidates
    appendToTypeVar ty i =
      let id = getId ty
          id' = id ++ show i
          bound = getBound ty
      in setBound bound $
         typeVar id' `withModeOf` ty `withBoxOf` ty

isSafeValField :: FieldDecl -> TypecheckM Bool
isSafeValField f@Field{ftype} = do
  isSafe <- isSharableType ftype
  return $ isValField f && isSafe

abstractTraitFrom :: Type -> (Type, [TraitExtension]) -> TypecheckM TraitDecl
abstractTraitFrom cname (t, exts) = do
  tdecl@Trait{tname, treqs, tmethods} <- findTrait t
  let bindings = zip (getTypeParameters tname) (getTypeParameters t)
      (fieldNames, methodNames) = partitionTraitExtensions exts
  fields <- mapM (findField cname) fieldNames
  checkLocalFields t fields
  fields' <- checkReadFields t fields
  methods <- mapM (findMethod cname) methodNames
  treqs' <- mapM (resolveReq t) treqs
  let newReqs = treqs' ++ map RequiredField fields' ++ map RequiredMethod methods
      tmethods' = map (concretizeMethod bindings) tmethods
  return tdecl{treqs = newReqs
              ,tname = t
              ,tmethods = tmethods'}
  where
    resolveReq trait r@RequiredField{rfield = Field{fname}} = do
      rfield' <- findField trait fname
      return r{rfield = rfield'}
    resolveReq trait r@RequiredMethod{rheader} = do
      rheader' <- findMethod trait (hname rheader)
      return r{rheader = rheader'}

    concretizeMethod :: [(Type, Type)] -> MethodDecl -> MethodDecl
    concretizeMethod bindings m =
      let mheader' = replaceHeaderTypes bindings (mheader m)
      in m{mheader = mheader'}

    checkReadFields t fields
      | isReadSingleType t = do
          unsafeFields <- filterM (liftM not . isAliasableType . ftype) fields
          let unsafeField = head unsafeFields
          unless (null unsafeFields) $
                 tcError $ NonSafeInExtendedReadTraitError
                           t (fname unsafeField) (ftype unsafeField)
          return $ map (\f -> f{fmut = Val}) fields
      | otherwise = return fields
    checkLocalFields t fields =
      unless (isLocalSingleType t || isActiveSingleType t) $ do
        localFields <- filterM (isLocalType . ftype) fields
        unless (null localFields) $
               tcError $ ThreadLocalFieldExtensionError
                         t (head localFields)

partly :: (Type -> TypecheckM Bool) -> Type -> TypecheckM Bool
partly isKind ty
    | isCompositeType ty
    , traits <- typesFromCapability ty
      = anyM (partly isKind) traits
    | isUnionType ty
    , tys <- unionMembers ty
      = anyM (partly isKind) tys
    | isClassType ty = do
        capability <- findCapability ty
        capIsPartly <- partly isKind capability
        tyIsKind <- isKind ty
        return $ tyIsKind || capIsPartly
    | hasResultType ty &&
      not (isArrowType ty) =
        partly isKind (getResultType ty)
    | isTupleType ty =
        anyM (partly isKind) (getArgTypes ty)
    | otherwise = isKind ty

fully :: (Type -> Bool) -> Type -> TypecheckM Bool
fully isKind ty
    | isCompositeType ty
    , traits <- typesFromCapability ty
      = allM (fully isKind) traits
    | isUnionType ty
    , tys <- unionMembers ty
      = allM (fully isKind) tys
    | isClassType ty = do
        capability <- findCapability ty
        liftM (isKind ty ||) (fully isKind capability)
    | hasResultType ty &&
      not (isArrowType ty) =
        fully isKind (getResultType ty)
    | isTupleType ty =
        allM (fully isKind) (getArgTypes ty)
    | otherwise = return $ isKind ty

isLinearType :: Type -> TypecheckM Bool
isLinearType = partly (return . isLinearSingleType)

isSubordinateType :: Type -> TypecheckM Bool
isSubordinateType = partly (return . isSubordinateSingleType)

isEncapsulatedType :: Type -> TypecheckM Bool
isEncapsulatedType = fully isSubordinateSingleType

isLocalType :: Type -> TypecheckM Bool
isLocalType = partly (isLocalType' [])
  where
    isLocalType' :: [Type] -> Type -> TypecheckM Bool
    isLocalType' checked ty
      | ty `elem` checked = return False
      | otherwise = do
          holdsLocal <- holdsLocalData checked ty
          return $ isLocalSingleType ty || holdsLocal
    holdsLocalData :: [Type] -> Type -> TypecheckM Bool
    holdsLocalData checked ty
      | isPassiveRefType ty && isRefAtomType ty &&
        not (isUnsafeSingleType ty) && ty `notElem` checked =
          anyM (isLocalType' (ty:checked)) $ getTypeParameters ty
      | otherwise = return False

isPassiveType :: Type -> TypecheckM Bool
isPassiveType ty
    | isClassType ty && isModeless ty = do
        capability <- findCapability ty
        isPassiveType capability
    | isClassType ty =
        return $ isPassiveRefType ty
    | isCapabilityType ty =
        fully isPassiveRefType ty
    | isUnionType ty
    , tys <- unionMembers ty
      = allM isPassiveType tys
    | isTypeVar ty
    , Just bound <- getBound ty
      = isPassiveType bound
    | otherwise = return False

isActiveType :: Type -> TypecheckM Bool
isActiveType ty
    | isClassType ty && isModeless ty = do
        capability <- findCapability ty
        isActiveType capability
    | isClassType ty =
        return $ isActiveSingleType ty
    | isCapabilityType ty =
        fully isActiveSingleType ty
    | isUnionType ty
    , tys <- unionMembers ty
      = allM isActiveType tys
    | isTypeVar ty
    , Just bound <- getBound ty
      = isActiveType bound
    | otherwise = return False

isSharedType :: Type -> TypecheckM Bool
isSharedType ty
    | isClassType ty && isModeless ty = do
        capability <- findCapability ty
        isSharedType capability
    | isClassType ty =
        return $ isSharedSingleType ty
    | isCapabilityType ty =
        fully isSharedSingleType ty
    | isUnionType ty
    , tys <- unionMembers ty
      = allM isSharedType tys
    | isTypeVar ty
    , Just bound <- getBound ty
      = isSharedType bound
    | otherwise = return False

isSharableType :: Type -> TypecheckM Bool
isSharableType ty
    | isArrowType ty = return $ isModeless ty
    | hasResultType ty = isSharableType $ getResultType ty
    | isTupleType ty = allM isSharableType $ getArgTypes ty
    | isCompositeType ty
    , traits <- typesFromCapability ty = allM isSharableType traits
    | isClassType ty && isModeless ty = do
        capability <- findCapability ty
        isSharableType capability
    | isModeless ty =
        return $ isPrimitive ty
              || isRangeType ty
              || isCType ty
              || isIncapability ty
    | otherwise = return $ hasSharableMode ty

isUnsafeType :: Type -> TypecheckM Bool
isUnsafeType ty
    | isClassType ty = do
        capability <- findCapability ty
        capIsUnsafe <- isUnsafeType capability
        return $ isUnsafeSingleType ty || capIsUnsafe
    | otherwise = return $
                  any isUnsafeSingleType $ typeComponents ty

isAliasableType :: Type -> TypecheckM Bool
isAliasableType ty
  | isArrowType ty = return . not $ isLinearSingleType ty
  | hasResultType ty = isAliasableType $ getResultType ty
  | isTupleType ty = allM isAliasableType $ getArgTypes ty
  | otherwise =
    anyM (\f -> f ty)
         [isSharableType
         ,isLocalType
         ,\t -> return $
                isTypeVar t && (isModeless t || hasSharableMode t)
         ]

checkConjunction :: Type -> [Type] -> TypecheckM ()
checkConjunction source sinks
  | isCompositeType source = do
      let sourceConjunctions = conjunctiveTypesFromCapability source
      mapM_ (\ty -> wellFormedConjunction sourceConjunctions
                                          (sinks \\ [ty]) ty) sinks
  | isClassType source = do
      cap <- findCapability source
      when (isIncapability cap) $
           tcError $ CannotUnpackError source
      when (source `elem` sinks) $
           tcError $ CannotInferUnpackingError source
      checkConjunction cap sinks
  | isTraitType source =
      whenM (isLinearType source) $
            tcError $ DuplicatingSplitError source
  | otherwise =
      tcError $ UnsplittableTypeError source
  where
    wellFormedConjunction pairs siblings ty = do
      when (null pairs) $
        tcError $ MalformedConjunctionError ty (head siblings) source
      let nonDisjoints =
            filter (\ty' -> all (not . singleConjunction ty ty') pairs) siblings
          nonDisjoint = head nonDisjoints
      unless (null nonDisjoints) $
        tcError $ MalformedConjunctionError ty nonDisjoint source
    singleConjunction ty1 ty2 (tys1, tys2) =
        ty1 `elem` tys1 && ty2 `elem` tys2 ||
        ty1 `elem` tys2 && ty2 `elem` tys1
