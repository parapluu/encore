{-# LANGUAGE LambdaCase, ConstrainedClassMethods #-}

{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information of every expression node. It throws an exception
with a meaningful error message if it fails.

-}

module Typechecker.Typechecker(typecheckProgram, checkForMainClass) where

import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Arrow((&&&), second)

-- Module dependencies
import Identifiers
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST (getType)
import qualified AST.Util as Util (freeVariables, filter, markStatsInBody,
                                  isStatement, isForwardInExpr)
import AST.PrettyPrinter
import AST.Util(extend)
import Types as Ty
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util
import Text.Printf (printf)
import Debug.Trace

-- | The top-level type checking function
typecheckProgram :: Map FilePath LookupTable -> Program ->
                    (Either TCError (Environment, Program), [TCWarning])
typecheckProgram table p = do
  let env = buildEnvironment table p
  let reader = (\p -> (env, p)) <$> runReaderT (doTypecheck p) env
  runState (runExceptT reader) []

checkForMainClass :: FilePath -> Program -> Maybe TCError
checkForMainClass source Program{classes} =
  case find (isLocalMain source) classes of
    Just Class{cname,cmethods} ->
      if any (isMainMethod cname . methodName) cmethods
      then Nothing
      else Just $ TCError (MethodNotFoundError (Name "main") cname) []
    Nothing -> Just $ TCError MissingMainClass []
  where
    isLocalMain source c@Class{cname} =
        isMainClass c &&
        getRefSourceFile cname == source


-- | The actual typechecking is done using a Reader monad wrapped
-- in an Error monad. The Reader monad lets us do lookups in the
-- "Environment", and the Error monad lets us throw a
-- "TCError" exception anywhere.
class Checkable a where
    -- | Returns the typechecked version of its argument (i.e. an
    -- AST-node extended with type information)
    doTypecheck :: a -> TypecheckM a

    -- | Like 'doTypecheck' but records the backtrace for better
    -- error messages
    typecheck :: Pushable a => a -> TypecheckM a
    typecheck x = local (pushBT x) $ doTypecheck x

instance Checkable Program where
    --  E |- fun1 .. E |- funn
    --  E |- class1 .. E |- classm
    -- ----------------------------
    --  E |- funs classes
  doTypecheck p@Program{typedefs, functions, traits, classes} = do
    etypedefs <- mapM typecheck typedefs
    etraits  <- mapM typecheck traits
    eclasses <- mapM typecheck classes
    efuns    <- mapM typecheck functions
    return p{functions = efuns
            ,typedefs = etypedefs
            ,traits = etraits
            ,classes = eclasses
            }

instance Checkable Typedef where
  doTypecheck t@Typedef{typedefdef} = do
      let (refId, parameters) = typeSynonymLHS typedefdef
      unless (distinctParams parameters) $
             tcError $ DistinctTypeParametersError typedefdef
      let rhs = typeSynonymRHS typedefdef
      let addTypeParams = addTypeParameters $ getTypeParameters typedefdef
      rhs' <- local addTypeParams $ checkType rhs
      return $ t{typedefdef = typeSynonymSetRHS typedefdef rhs'}
       where
         distinctParams p = length p == length (nub p)

typecheckNotNull :: Expr -> TypecheckM Expr
typecheckNotNull expr = do
  eExpr <- typecheck expr
  let ty = AST.getType eExpr
  if isNullType ty
  then local (pushBT expr) $ coerceNull eExpr ty
  else return eExpr


addLocalFunctions = extendEnvironment . map localFunctionType
  where
    localFunctionType f =
        let params = functionParams f
            ptypes = map ptype params
            typeParams = functionTypeParams f
            returnType = functionType f
            arrowType = arrowWithTypeParam typeParams ptypes returnType
        in
          (functionName f, arrowType)


instance Checkable Function where
    --  E, x1 : t1, .., xn : tn, xa: a, xb: b |- funbody : funtype
    -- ----------------------------------------------------------
    --  E |- def funname<a, b>(x1 : t1, .., xn : tn, xa: a, xb: b) : funtype funbody
    doTypecheck f@(Function {funbody, funlocals}) = do
      let funtype = functionType f
          funparams = functionParams f
          funtypeparams = functionTypeParams f
          body = Util.markStatsInBody funtype funbody
          isForward = Util.isForwardInExpr funbody
      when (isForward) $
            pushError funbody ForwardInFunction
      local (addTypeParameters funtypeparams) $
            mapM_ typecheck funparams
      local (addTypeParameters funtypeparams) $
            checkType funtype

      eBody <-
        local (addTypeParameters funtypeparams .
               addParams funparams .
               addLocalFunctions funlocals) $
                  if isUnitType funtype
                  then typecheckNotNull body
                  else hasType body funtype
      eLocals <- local (addTypeParameters funtypeparams .
                        addLocalFunctions funlocals) $
                       mapM typecheck funlocals
      return f{funbody = eBody
              ,funlocals = eLocals}

instance Checkable TraitDecl where
  doTypecheck t@Trait{tname, tmethods, treqs} = do
    ereqs <- local (addTypeParams . addThis) $
             mapM doTypecheck treqs
    emethods <- local (addTypeParams . addMinorThis) $
                mapM typecheck tmethods
    return t{tmethods = emethods, treqs = ereqs}
    where
      addTypeParams = addTypeParameters $ getTypeParameters tname
      addMinorThis =
        extendEnvironmentImmutable $
          if hasMinorMode tname && not (isADT tname)
          then [(thisName, makeSubordinate tname)]
          else [(thisName, tname)]
      addThis = extendEnvironmentImmutable [(thisName, tname)]

instance Checkable Requirement where
  doTypecheck r@RequiredField{rfield} = do
    rfield' <- typecheck rfield
    return r{rfield = rfield'}
  doTypecheck r@RequiredMethod{rheader} = do
    let typeParams = htypeparams rheader
    local (addTypeParameters typeParams) $
          mapM_ typecheck (hparams rheader)
    local (addTypeParameters typeParams) $
          checkType (htype rheader)
    return r

instance Checkable FieldDecl where
  doTypecheck f@Field{ftype} = do
    checkType ftype
    Just (_, thisType)  <- findVar (qLocal thisName)
    when (isReadSingleType thisType) $ do
         unless (isValField f) $
                tcError $ NonValInReadContextError thisType
         isAliasable <- isAliasableType ftype
         unless isAliasable $
                tcError $ NonSafeInReadContextError thisType ftype
         when (isArrayType ftype) $
              tcWarning ArrayInReadContextWarning
    isLocalField <- isLocalType ftype
    isLocalThis <- isLocalType thisType
    when isLocalField $
      unless (isModeless thisType || isLocalThis ||
              isActiveSingleType thisType) $
      tcError $ ThreadLocalFieldError thisType
    return f


matchArgumentLength :: Type -> FunctionHeader -> Arguments -> TypecheckM Name
matchArgumentLength targetType header args =
      if (actual == expected)
        then return name
        else
          do
          result <- asks $ methodLookup targetType defName
          case result of
            Just header' -> return defName
            Nothing -> tcError $ WrongNumberOfMethodArgumentsError
              (hname header) targetType expected actual
  where
    name = hname header
    defName = Name ("_" ++ show(name) ++ show (expected - actual))
    actual = length args
    expected = length (hparams header)


meetRequiredFields :: [FieldDecl] -> Type -> TypecheckM ()
meetRequiredFields cFields trait = do
  tdecl <- findTrait trait
  mapM_ matchField (requiredFields tdecl)
    where
    matchField tField = do
      expField <- findField trait (fname tField)
      let expected = ftype expField
          result = find (==expField) cFields
          cField = fromJust result
          cFieldType = ftype cField
      if isNothing result then
          tcError $ MissingFieldRequirementError expField trait
      else if isValField expField then
          unlessM (cFieldType `subtypeOf` expected) $
              tcError $ CovarianceViolationError cField expected trait
      else do
        isSub <- cFieldType `subtypeOf` expected
        unless (cFieldType == expected) $
            tcError $ RequiredFieldMismatchError cField expected trait isSub
        when (isVarField expField) $
             unless (isVarField cField) $
                 tcError $ RequiredFieldMutabilityError trait cField

noOverlapFields :: Type -> Maybe TraitComposition -> TypecheckM ()
noOverlapFields cname composition =
  let
    conjunctiveTraits = conjunctiveTypesFromComposition composition
  in
    mapM_ (checkPair cname) conjunctiveTraits
  where
    checkPair :: Type -> ([ExtendedTrait], [ExtendedTrait]) -> TypecheckM ()
    checkPair cname (left, right) = do
      leftPairs  <- mapM (pairTypeFields cname) left
      rightPairs <- mapM (pairTypeFields cname) right
      mapM_ conjunctiveVarErr (concatMap (commonVarFields rightPairs) leftPairs)

    findTypeHasField :: [(Type, [FieldDecl])] -> FieldDecl -> Type
    findTypeHasField pairs field =
      head [fst pair | pair <- pairs, field `elem` snd pair]

    commonVarFields :: [(Type, [FieldDecl])] -> (Type, [FieldDecl]) -> [(Type, Type, FieldDecl)]
    commonVarFields pairs (t, fields) =
      let
        otherFields = concatMap snd pairs
        common = intersect fields otherFields
        leftCommon = [f | f <- fields, f `elem` common, notVal f]
        rightCommon = [f | f <- otherFields, f `elem` common, notVal f]
        firstErrField = if (not . null) leftCommon
                        then head leftCommon
                        else head rightCommon
        otherType = findTypeHasField pairs firstErrField
      in
        if null leftCommon && null rightCommon then
          []
        else
          [(t, otherType, firstErrField)]

    conjunctiveVarErr :: (Type, Type, FieldDecl) -> TypecheckM ()
    conjunctiveVarErr (left, right, field) =
      tcError $ NonDisjointConjunctionError left right field

    notVal :: FieldDecl -> Bool
    notVal = not . isValField

    pairTypeFields ::
      Type -> ExtendedTrait -> TypecheckM (Type, [FieldDecl])
    pairTypeFields cname (t, ext) = do
      abstractDecl <- abstractTraitFrom cname (t, ext)
      return (t, requiredFields abstractDecl)

ensureNoMethodConflict :: [MethodDecl] -> [Type] -> TypecheckM ()
ensureNoMethodConflict methods traits = do
  tdecls <- mapM findTrait traits
  let traitMethods = concatMap tmethods tdecls
      duplicates = traitMethods \\ nub traitMethods
      nonOverridden = duplicates \\ methods
      dup = head nonOverridden
      overlappingTraits = filter ((dup `elem`) . tmethods) tdecls
  unless (null nonOverridden) $
         tcError $ IncludedMethodConflictError
                     (methodName dup)
                     (tname (head overlappingTraits))
                     (tname (overlappingTraits !! 1))

ensureMatchingTraitFootprint ::
  Type -> [ExtendedTrait] -> ExtendedTrait ->
  TypecheckM ()
ensureMatchingTraitFootprint cname extTraits extTrait = do
  abstractDecl <- abstractTraitFrom cname extTrait
  let otherTraits = extTraits \\ [extTrait]
  abstractDecls <- mapM (abstractTraitFrom cname) otherTraits
  mapM_ (checkMatchingFootprint abstractDecl) abstractDecls
  where
    checkMatchingFootprint requirer provider = do
      let reqFields = requiredFields requirer
          reqValFields = filter isValField reqFields
          methods = tmethods provider
          providedMethods = filter (`isRequiredBy` requirer) methods
          footprint = requiredFields provider
          varFootprint = filter (not . isValField) footprint
          mutatedValFields = filter (`elem` reqValFields) varFootprint
      unless (null providedMethods) $ do
        unless (null mutatedValFields) $
          tcError $ ProvidingTraitFootprintError
                      (tname provider) (tname requirer)
                      (methodName $ head providedMethods) mutatedValFields
        when (isReadSingleType $ tname requirer) $
          unless (isReadSingleType $ tname provider) $
            tcError $ ProvidingToReadTraitError
                        (tname provider) (tname requirer)
                        (methodName $ head providedMethods)

    isRequiredBy :: MethodDecl -> TraitDecl -> Bool
    isRequiredBy m = any ((== methodName m) . hname) . requiredMethods

meetRequiredMethods :: [MethodDecl] -> [Type] -> TypecheckM ()
meetRequiredMethods cMethods traits = do
  tdeclPairs <- mapM tdeclAssoc traits
  let reqMethodPairs = collectReqPairs tdeclPairs
  tMethods <- concatMapM collectMethods tdeclPairs
  let allMethods = map mheader cMethods ++ tMethods
  mapM_ (matchMethod allMethods) reqMethodPairs
  where
    tdeclAssoc :: Type -> TypecheckM (Type, TraitDecl)
    tdeclAssoc t = do
      tdecl <- findTrait t
      return (t, tdecl)

    collectReqPairs :: [(Type, TraitDecl)] -> [(Type, FunctionHeader)]
    collectReqPairs tdeclPairs =
        let reqMethods = map (second requiredMethods) tdeclPairs
        in concatMap (\(t, reqs) ->
                          map (\req -> (t, req)) reqs)
                     reqMethods

    collectMethods :: (Type, TraitDecl) -> TypecheckM [FunctionHeader]
    collectMethods (t, tdecl) = do
      let methods = tmethods tdecl
      mapM (findMethod t . methodName) methods

    matchMethod :: [FunctionHeader] -> (Type, FunctionHeader) -> TypecheckM ()
    matchMethod provided (requirer, reqHeader) = do
      expHeader <- findMethod requirer (hname reqHeader)
      unlessM (anyM (matchesHeader expHeader) provided) $
           tcError $ MissingMethodRequirementError expHeader requirer
    matchesHeader expected actual =
      let
        expectedName = hname expected
        expectedType = htype expected
        expectedParamTypes = map ptype (hparams expected)
        actualName = hname actual
        actualType = htype actual
        actualParamTypes = map ptype (hparams actual)
      in
        liftM ((actualName == expectedName &&
                actualParamTypes == expectedParamTypes) &&) $
              actualType `subtypeOf` expectedType

checkOverriding ::
  Type -> [Type] -> [MethodDecl] -> [ExtendedTrait] -> TypecheckM ()
checkOverriding cname typeParameters methods extendedTraits = do
  abstractDecls <- mapM (abstractTraitFrom cname) extendedTraits
  let overridden = concatMap (pairMethod methods) abstractDecls
  mapM_ (checkOverride typeParameters) overridden
  where
    pairMethod :: [MethodDecl] -> TraitDecl ->
                  [(TraitDecl, FunctionHeader, MethodDecl)]
    pairMethod methods abstractDecl =
      mapMaybe (matchHeader abstractDecl) methods
      where
      matchHeader abstractDecl method =
        let headers = traitInterface abstractDecl in
        case find ((== methodName method) . hname) headers of
          Just required -> Just (abstractDecl, required, method)
          Nothing -> Nothing

    checkOverride typeParameters (abstractDecl, required, method) = do
      let expectedParamTypes = map ptype $ hparams required
          expectedType = htype required
          expectedTypeParams = htypeparams required
          expectedMethodType = arrowType expectedParamTypes expectedType
                               `setTypeParameters` expectedTypeParams
          actualParamTypes = map ptype $ methodParams method
          actualType = methodType method
          actualTypeParams = methodTypeParams method
          actualMethodType = arrowType actualParamTypes actualType
                             `setTypeParameters` actualTypeParams
          requirer = tname abstractDecl
      unlessM (actualMethodType `subtypeOf` expectedMethodType) $
             pushError method $
               OverriddenMethodTypeError
                 (methodName method) expectedMethodType requirer actualMethodType
      typecheckWithTrait `catchError`
                          \(TCError e bt) ->
                             throwError $
                               TCError (OverriddenMethodError
                                        (methodName method) requirer e) bt

      where
        addAbstractTrait = withAbstractTrait abstractDecl
        addTypeParams = addTypeParameters typeParameters
        extendedThisType = abstractTraitFromTraitType (tname abstractDecl)
        addThis = extendEnvironment [(thisName, extendedThisType)]
        typecheckWithTrait =
          local (addAbstractTrait . addTypeParams . addThis) $
                typecheck method

checkManifestModes :: [Type] -> TypecheckM ()
checkManifestModes traits = do
  formalTraits <- mapM findFormalRefType traits
  mapM_ (checkForConflictingModes traits) formalTraits
  where
    checkForConflictingModes actualTraits formal =
      unless (hasMinorMode formal) $ do
        let conflicting = filter (not . (`isCompatibleWith` formal))
                                 actualTraits
        unless (null conflicting) $
               tcError $ ManifestConflictError formal (head conflicting)
    isCompatibleWith other ty =
        other `modeSubtypeOf` ty ||
        hasMinorMode other

instance Checkable ClassDecl where
  -- TODO: Update this rule!
  --  E, this : cname |- method1 .. E, this : cname |- methodm
  -- -----------------------------------------------------------
  --  E |- class cname fields methods
  doTypecheck c@(Class {cname, cfields, cmethods, ccomposition}) = do

    when (isPassiveClassType cname) $
         when (any isForwardMethod cmethods) $
                tcError $ ForwardInPassiveContext cname

    let traits = typesFromTraitComposition ccomposition
        extendedTraits = extendedTraitsFromComposition ccomposition

    local (addTypeVars . addThis) $
          mapM_ checkType traits

    mapM_ (meetRequiredFields cfields) traits
    meetRequiredMethods cmethods traits
    ensureNoMethodConflict cmethods traits

    checkManifestModes traits

    mapM_ (ensureMatchingTraitFootprint cname extendedTraits) extendedTraits
    noOverlapFields cname ccomposition

    checkMethodExtensionAllowed

    -- TODO: Add namespace for trait methods

    efields <- local (addTypeVars . addThis) $
               mapM typecheck cfields
    emethods <- local (addTypeVars . addThis) $
                mapM typecheck cmethods

    checkOverriding cname typeParameters cmethods extendedTraits

    return c{cmethods = emethods, cfields = efields}
    where
      typeParameters = getTypeParameters cname
      addTypeVars = addTypeParameters typeParameters
      addThis = extendEnvironmentImmutable [(thisName, cname)]
      isForwardMethod m@Method{mbody} = Util.isForwardInExpr mbody

      checkMethodExtensionAllowed
        | isModeless cname = do
            let extendedTraits = extendedTraitsFromComposition ccomposition
                extensions = concatMap snd extendedTraits
                (_, extendedMethods) = partitionTraitExtensions extensions
                cap = capabilityFromTraitComposition ccomposition
            mapM_ (extensionAllowed cap extendedMethods) cmethods
        | otherwise = return ()

      extensionAllowed cap extensions method
        | isMainMethod cname (methodName method) = return ()
        | isConstructor method = return ()
        | isImplicitMethod method = return ()
        | otherwise = do
            let name = methodName method
            lookupResult <- asks $ methodLookup cap name
            unless (isJust lookupResult || name `elem` extensions) $
                tcError $ UnmodedMethodExtensionError cname name


instance Checkable MethodDecl where
    --  E, x1 : t1, .., xn : tn |- mbody : mtype
    -- -----------------------------------------------------
    --  E |- def mname(x1 : t1, .., xn : tn) : mtype mbody
    --
    --  E |- this : C
    --  isActiveClass(C)
    --  E, x1 : t1, .., xn : tn |- mbody : mtype
    -- -----------------------------------------------------
    --  E |- stream mname(x1 : t1, .., xn : tn) : mtype mbody
    doTypecheck m@(Method {mbody, mlocals}) = do
        let mType   = methodType m
            mparams = methodParams m
            mtypeparams = methodTypeParams m
            body = Util.markStatsInBody mType mbody
        local (addTypeParameters mtypeparams) $
               mapM_ typecheck mparams
        local (addTypeParameters mtypeparams) $
              checkType mType

        eBody <-
            local (addTypeParameters mtypeparams .
                   addParams mparams .
                   addLocalFunctions mlocals) $
                       if isUnitType mType || isStreamMethod m
                       then typecheckNotNull body
                       else hasType body mType
        when (isMatchMethod m) $
             checkPurity eBody

        eLocals <- local (addTypeParameters mtypeparams .
                          addLocalFunctions mlocals .
                          dropLocal thisName) $
                         mapM typecheck mlocals

        return $ m{mbody = eBody
                  ,mlocals = eLocals}
      where
        checkPurity e = mapM_ checkImpureExpr (Util.filter isImpure e)
        checkImpureExpr call@MethodCall{target, name} = do
          let targetType = AST.getType target
          header <- findMethod targetType name
          unless (isMatchMethodHeader header) $
                 pushError call $ ImpureMatchMethodError call
        checkImpureExpr match@Match{arg, clauses} =
          mapM_ (checkImpurePattern (AST.getType arg) . mcpattern) clauses
        checkImpureExpr Assign{lhs = VarAccess{}} = return ()
        checkImpureExpr e = pushError e $ ImpureMatchMethodError e

        checkImpurePattern argTy p@ExtractorPattern{name} = do
          header <- findMethod argTy name
          unless (isMatchMethodHeader header) $
                 pushError p $ ImpureMatchMethodError p
        checkImpurePattern argTy _ = return ()

instance Checkable ParamDecl where
    doTypecheck p@Param{ptype} = do
      ptype' <- checkType ptype
      return $ setType ptype' p

-- | 'hasType e ty' typechecks 'e' (with backtrace) and returns
-- the result if 'e' is a subtype of 'ty'
hasType :: Expr -> Type -> TypecheckM Expr
hasType e ty = local (pushBT e) $ checkHasType e ty
    where
      checkHasType expr ty =
          do eExpr <- doTypecheck expr
             let exprType = AST.getType eExpr
             resultType <- exprType `coercedInto` ty
             assertSubtypeOf resultType ty
             let result = propagateResultType resultType eExpr
             return $ setType resultType result

instance Checkable Expr where
    --
    -- ----------------
    --  E |- () : unit
    doTypecheck skip@(Skip {}) = return $ setType unitType skip

    --
    -- ----------------
    --  E |- break : unit
    doTypecheck break@(Break {emeta}) = do
      unless (Util.isStatement break) $
        tcError BreakUsedAsExpressionError
      unlessM (asks checkValidUseOfBreak) $
        tcError BreakOutsideOfLoopError
      return $ setType unitType break

    --
    -- ----------------
    --  E |- Continue : void
    doTypecheck continue@(Continue {emeta}) = do
      unless (Util.isStatement continue) $
        tcError ContinueUsedAsExpressionError
      unlessM (asks checkValidUseOfContinue) $
        tcError ContinueOutsideOfLoopError
      return $ setType unitType continue

    --    |- t
    --  E |- body : t
    -- ----------------------
    --  E |- (body : t) : t
    doTypecheck te@(TypedExpr {body, ty}) =
        do ty' <- checkType ty
           eBody <- typecheck body
           bodyType <- AST.getType eBody `coercedInto` ty'
           let eBody' = setType bodyType eBody
           return $ setType ty' $ te{body = eBody', ty = ty'}

    doTypecheck p@(PartyPar {parl, parr}) = do
      pl <- typecheck parl
      pr <- hasType parr (AST.getType pl)
      let lType = AST.getType pl
          rType = AST.getType pr

      unless (isParType lType) $
        pushError pl $ TypeMismatchError lType (parType lType)
      unless (isParType rType) $
        pushError pr $ TypeMismatchError rType (parType rType)

      lIsSubtype <- lType `subtypeOf` rType
      rType `subtypeOf` lType
      if lIsSubtype
      then return $ setType rType p {parl = pl, parr = pr}
      else return $ setType lType p {parl = pl, parr = pr}

    doTypecheck pSeq@(PartySeq {par, seqfunc}) = do
      ePar <- typecheck par
      eSeqFunc <- typecheck seqfunc
      let seqType = AST.getType eSeqFunc
          pType = AST.getType ePar
          numberArgs = length (getArgTypes seqType)
      unless (isCallable eSeqFunc) $
        pushError eSeqFunc $ NonFunctionTypeError seqType
      unless (numberArgs == 1) $ pushError eSeqFunc $
        WrongNumberOfFunctionArgumentsError (qname seqfunc) 1 numberArgs
      unless (isParType pType) $
        pushError ePar $ TypeMismatchError pType (parType pType)

      expectedFunType <- typecheckParametricFun [getResultType pType] eSeqFunc
      let eSeqFunc' = setType expectedFunType eSeqFunc
          funResultType = getResultType expectedFunType
      return $ setType (parType funResultType) pSeq {par=ePar, seqfunc=eSeqFunc'}


    doTypecheck red@(PartyReduce {seqfun, pinit, par}) = do
      ePar <- typecheck par
      eFunc <- typecheck seqfun
      ePinit <- typecheck pinit
      handleErrors ePar eFunc
      let pType = AST.getType ePar
          initType = AST.getType ePinit
      expectedFunType <- typecheckParametricFun
                           [initType, getResultType pType] eFunc
      let eSeqFunc' = setType expectedFunType eFunc
      parallelRun <- assocTypes initType (getResultType pType)
      return $ setType (futureType initType) red {seqfun=eSeqFunc'
                                                 ,pinit=ePinit
                                                 ,par=ePar
                                                 ,runassoc=parallelRun}
      where
        assocTypes initType pType = do
          lIsSubtype <- initType `subtypeOf` pType
          rIsSubtype <- pType `subtypeOf` initType
          return $ and [lIsSubtype, rIsSubtype]

        handleErrors ePar eFunc = do
          let numberArgs = length (getArgTypes seqType)
              seqType = AST.getType eFunc
              pType = AST.getType ePar
          unless (isCallable eFunc) $
            pushError eFunc $ NonFunctionTypeError seqType
          unless (numberArgs == 2) $ pushError eFunc $
            WrongNumberOfFunctionArgumentsError (qname seqfun) 1 numberArgs
          unless (isParType pType) $
            pushError ePar $ TypeMismatchError pType (parType pType)

    doTypecheck mcall
      | isMethodCallOrMessageSend mcall = do
          eTarget <- typecheck (target mcall)
          let targetType = AST.getType eTarget
              methodName = name mcall

          isKnown <- isKnownRefType targetType
          methodResult <- if isKnown
                          then asks $ methodLookup targetType methodName
                          else return Nothing

          fieldResult <- if isKnown && isRefAtomType targetType
                         then asks $ fieldLookup targetType methodName
                         else return Nothing

          case () of _  -- Might be accessing an array or a closure in a field
                      | isNothing methodResult -- Methods take precedence
                      , MethodCall{} <- mcall  -- Only for '.', not '!'
                      , Just Field{ftype} <- fieldResult ->
                          handleArrayOrFieldClosure eTarget targetType
                                                    methodName ftype
                      | otherwise ->
                          handleMethodCall eTarget targetType
        where
          handleArrayOrFieldClosure eTarget targetType name fieldType =
            let fieldAccess = FieldAccess{emeta = emeta mcall
                                         ,target = eTarget
                                         ,name
                                         }
            in
            if isArrayType fieldType && length (args mcall) == 1 then
              doTypecheck ArrayAccess{emeta = emeta mcall
                                     ,target = fieldAccess
                                     ,index = head (args mcall)}
            else if isArrowType fieldType then
              let body = FunctionCall{emeta = emeta mcall
                                     ,typeArguments = typeArguments mcall
                                     ,qname = qLocal name
                                     ,args = args mcall
                                     }
              in
                doTypecheck Let{emeta = emeta mcall
                               ,mutability = Val
                               ,decls = [([VarNoType name], fieldAccess)]
                               ,body
                               }
            else
                handleMethodCall eTarget targetType

          handleMethodCall eTarget targetType = do
            handleErrors targetType mcall
            typecheckPrivateModifier eTarget (name mcall)

            (header, calledType) <- findMethodWithCalledType targetType (name mcall)

            calledName <- matchArgumentLength targetType header (args mcall)

            isActive <- isActiveType targetType
            let eTarget' = if isThisAccess eTarget &&
                              isActive && isMethodCall mcall
                           then setType (makeLocal calledType) eTarget
                           else setType calledType eTarget

                typeParams = htypeparams header
                argTypes = map ptype $ take (length (args mcall)) (hparams header)
                resultType = htype header

            (eArgs, resultType', typeArgs) <-
               if null (typeArguments mcall) then
                  inferenceCall mcall typeParams argTypes resultType
               else
                  typecheckCall mcall typeParams argTypes resultType

            checkEncapsulation eTarget' (name mcall) resultType eArgs

            returnType <- retType mcall calledType header resultType'
            let isThisCall = isThisAccess (target mcall)
                isStream = isStreamMethodHeader header
                isAsync = isMessageSend mcall
                returnType' = if isAsync && isThisCall
                              then futureType returnType
                              else returnType

            when (isStream && isThisCall) $ tcError SyncStreamCall
            return $ setArrowType (arrowType argTypes resultType) $
                     setType returnType' mcall {target = eTarget'
                                              ,args = eArgs
                                              ,typeArguments = typeArgs
                                              ,name = calledName }

          errorInitMethod targetType name = do
            when (name == constructorName) $ tcError ConstructorCallError
            when (isMainMethod targetType name) $ tcError MainMethodCallError

          handleErrors targetType m
            | isMessageSend m = do
              errorInitMethod targetType (name m)
              isActive <- isActiveType targetType
              isShared <- isSharedType targetType
              unless (isActive || isShared) $
                tcError $ NonSendableTargetError targetType
            | isMethodCall m = do
              when (isRefType targetType) $ do
                isPassive <- isPassiveType targetType
                unless (isPassive || isThisAccess (target mcall)) $
                       tcError BadSyncCallError
              let name' = name m
              unless (isRefType targetType) $
                     tcError $ NonCallableTargetError targetType
              errorInitMethod targetType name'
            | otherwise =
                error $ "Typechecker.hs: expression '" ++ show m ++ "' " ++
                        "is not a method or function call"

    doTypecheck maybeData@(MaybeValue {mdt}) = do
      when (Util.isStatement maybeData) $
        tcWarning $ ExpressionResultIgnoredWarning maybeData

      eBody <- maybeTypecheck mdt
      let returnType = case eBody of
                         (JustData exp) -> AST.getType exp
                         NothingData -> bottomType
      return $ setType (maybeType returnType) maybeData { mdt = eBody }
        where
          maybeTypecheck just@(JustData exp) = do
            eBody <- typecheckNotNull exp
            return $ just { e = eBody }

          maybeTypecheck nothing@NothingData = return nothing

    -- E |- arg1 :ty1 .. E |- argn : tyn
    -- ---------------------------------
    -- E |- (arg1, .., argn) : (ty1, .., tyn)
    doTypecheck tuple@(Tuple {args}) = do
      when (Util.isStatement tuple) $
        tcWarning $ ExpressionResultIgnoredWarning tuple
      eArgs <- mapM typecheck args
      let argTypes = map AST.getType eArgs
      return $ setType (tupleType argTypes) tuple{args = eArgs}



    --  E |- f : (t1 .. tn) -> t
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    --  B'(t) = t'
    -- --------------------------------------
    --  E |- f(arg1, .., argn) : t'
    doTypecheck fcall@(FunctionCall {emeta, qname, args, typeArguments}) = do
      result <- findVar qname
      (qname', ty) <- case result of
        Just (qname', ty) -> return (qname', ty)
        Nothing -> tcError $ UnboundFunctionError qname

      if isArrayType ty && length args == 1 then
          doTypecheck ArrayAccess{emeta
                                 ,target = VarAccess{emeta, qname}
                                 ,index = head args}
      else if (isArrowType ty) then do
          let typeParams  = getTypeParameters ty
              argTypes    = getArgTypes ty
              resultType  = getResultType ty
              actualLength = length args
              expectedLength = length argTypes
              defName = qname'{qnlocal = Name $ "_" ++ show qname ++ show (expectedLength - actualLength)}
          calledName <-
            if (actualLength == expectedLength)
              then return qname'
              else do
                result2 <- findVar defName
                case result2 of
                  Just (qname2, ty2) -> return defName
                  Nothing -> tcError $ WrongNumberOfFunctionArgumentsError
                              qname (length argTypes) (length args)
          (eArgs, returnType, typeArgs) <-
            if null typeArguments
            then inferenceCall fcall typeParams (take actualLength argTypes) resultType
            else do
              unless (length typeArguments == length typeParams) $
                     tcError $ WrongNumberOfFunctionTypeArgumentsError qname
                               (length typeParams) (length typeArguments)
              typecheckCall fcall typeParams (take actualLength argTypes) resultType
          return $ setArrowType ty $
                   setType returnType fcall {args = eArgs,
                                             qname = calledName,
                                             typeArguments = typeArgs}
        else do
          tcError $
             ExpectingOtherTypeError "an array or a function call" ty

   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  t != nullType
    -- ------------------------------------------------------
    --  E |- \ (x1 : t1, .., xn : tn) -> body : (t1 .. tn) -> t
    doTypecheck closure@(Closure {eparams, mty, body}) = do
      eEparams <- mapM typecheck eparams
      mty' <- mapM checkType mty
      eBody <- case mty' of
                 Just expected ->
                   if isUnitType expected then
                     local (addParams eEparams) $
                           typecheckNotNull body
                   else
                     local (addParams eEparams) $
                           body `hasType` expected
                 Nothing ->
                   local (addParams eEparams) $
                         typecheckNotNull body
      let paramNames = map pname eEparams
          captured = Util.freeVariables (map qLocal paramNames) eBody
          capturedVariables = map (qnlocal . fst) captured
          capturedTypes = map snd captured

      shadowingParams <- filterM doesShadow paramNames
      unless (null shadowingParams) $
         tcError $ TypeVariableAndVariableCommonNameError shadowingParams

      local (addParams eEparams . makeImmutable capturedVariables) $
            typecheck eBody -- Check for mutation of captured variables
      let returnType = AST.getType eBody
          ty = arrowType (map ptype eEparams) returnType
      modedType <- giveModes capturedTypes ty
      return $ setType modedType closure {body = eBody
                                         ,mty = mty'
                                         ,eparams = eEparams}
      where
        doesShadow paramName = do
          typeParams <- asks typeParameters
          return $ paramName `elem` map (Name . getId) typeParams
        giveModes types arrow = foldM giveMode arrow types
        giveMode arrow ty = do
          isLocal <- isLocalType ty
          isSubord <- isSubordinateType ty
          let modes = [isLocal `thenDo` makeLocal
                      ,isSubord `thenDo` makeSubordinate
                      ,isStackboundType ty `thenDo` makeStackbound
                      ]
          return $ foldr ($) arrow modes
        thenDo True f = f
        thenDo False _ = id

    --  E |- e1 : t1; E, x1 : t1 |- e2 : t2; ..; E, x1 : t1, .., x(n-1) : t(n-1) |- en : tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  x1 != nullType .. xn != nullType
    -- --------------------------------------------------------------------------------------
    --  E |- let x1 = e1 .. xn = en in body : t

    doTypecheck let_@(Let {mutability, decls, body}) =
        do eDecls <- typecheckDecls decls
           let extract (vars, e) = let ty = AST.getType e
                                   in map (extractBindings ty) vars
               locals = concatMap extract eDecls
               declTypes = map snd locals
           when (any isBottomType (concatMap typeComponents declTypes)) $
                tcError BottomTypeInferenceError
           let addNames = (if mutability == Val
                           then extendEnvironmentImmutable
                           else extendEnvironment) locals
           eBody <- local addNames $ typecheck body
           return $ setType (AST.getType eBody) let_ {decls = eDecls
                                                     ,body = eBody}
        where
          extractBindings :: Type -> VarDecl -> (Name, Type)
          extractBindings ty VarType{varName, varType} = (varName, varType)
          extractBindings ty VarNoType{varName} = (varName, ty)

          typecheckDecls :: [([VarDecl], Expr)] ->
                            TypecheckM [([VarDecl], Expr)]
          typecheckDecls [] = return []
          typecheckDecls ((vars, expr):decls') = do
            eExpr <- typecheckNotNull expr
            let eType = AST.getType eExpr
            eVars <- mapM (checkBinding eType) vars
            let localBindings = map (extractBindings eType) eVars
                varTypes = map snd localBindings
            hasLinear <- anyM isLinearType varTypes
            when (length varTypes > 1 && hasLinear) $ do
                 checkConjunction eType varTypes
                 tcWarning CapabilitySplitWarning
            let addNames = (if mutability == Val
                            then extendEnvironmentImmutable
                            else extendEnvironment) localBindings
            eDecls <- local addNames $
                            typecheckDecls decls'
            return $ (eVars, eExpr):eDecls

          checkBinding eType (VarType x ty) = do
            ty' <- checkType ty
            eType `assertSubtypeOf` ty'
            return (VarType x ty')
          checkBinding eType (VarNoType x) =
              return (VarNoType x)

    --  E |- en : t
    -- ------------------------
    --  E |- {e1; ..; en} : t
    doTypecheck e@(Seq {eseq}) =
        do eInit <- mapM typecheckNotNull (init eseq)
           eResult <- typecheck (last eseq)
           let seqType = AST.getType eResult
               eEseq = eInit ++ [eResult]
           return $ setType seqType e {eseq = eEseq}

    --  E |- cond : bool
    --  E |- thn : t'
    --  E |- els : t''
    --  t = matchBranches(t', t'')
    -- ------------------------------------
    --  E |- if cond then thn else els : t
    doTypecheck ifThenElse@(IfThenElse {cond, thn, els}) =
        do eCond <- hasType cond boolType
           eThn <- typecheck thn
           eEls <- typecheck els
           let thnType = AST.getType eThn
               elsType = AST.getType eEls
           resultType <- matchBranches thnType elsType
           thnTypeInf <- if knownType thnType
                         then return thnType
                         else thnType `coercedInto` resultType
           elsTypeInf <- if knownType elsType
                         then return elsType
                         else elsType `coercedInto` resultType
           return $ setType resultType
                    ifThenElse {cond = eCond
                               ,thn = setType thnTypeInf eThn
                               ,els = setType elsTypeInf eEls
                               }
        where
          knownType ty =
              not (isNullType ty) &&
              all (not . isBottomType) (typeComponents ty)
          matchBranches ty1 ty2
              | isNullType ty1 && isNullType ty2 =
                  tcError IfInferenceError
              | otherwise = do
                  result <- unifyTypes [ty1, ty2]
                  case result of
                    Just ty -> return ty
                    Nothing -> do
                      ty1Sub <- ty1 `subtypeOf` ty2
                      ty2Sub <- ty2 `subtypeOf` ty1
                      if ty1Sub || isUnitType ty2 then
                          return ty2
                      else if ty2Sub || isUnitType ty1 then
                          return ty1
                      else if knownType ty1 || knownType ty2 then
                          tcError $ IfBranchMismatchError ty1 ty2
                      else
                          tcError IfInferenceError


    --  E |- arg : t'
    --  clauses = (pattern1, guard1, expr1),..., (patternN, guardN, exprN)
    --  not isActiveRefType(t')
    --  not null clauses
    --  E |- pattern1 : t', ..., patternN : t'
    --  E |- guard1 : bool, .. , guardN : bool
    --  E |- expr1 : t, ..., exprN : t
    ---------------------------------------
    --  E |- match arg clauses : t
    doTypecheck match@(Match {arg, clauses}) = do
        when (null clauses) $
          tcError EmptyMatchClauseError
        eArg <- typecheck arg
        checkMatchArgument eArg
        let argType = AST.getType eArg
        when (isActiveSingleType argType) $
          unless (isThisAccess arg) $
            tcError ActiveMatchError
        eClauses <- mapM (checkClause argType) clauses
        checkForPrivateExtractors eArg (map mcpattern eClauses)
        resultType <- checkAllHandlersSameType eClauses
        let updateClauseType m@MatchClause{mchandler} =
                m{mchandler = setType resultType mchandler}
            eClauses' = map updateClauseType eClauses
        return $ setType resultType match {arg = eArg, clauses = eClauses'}
      where
        checkMatchArgument arg = do
          let argType = AST.getType arg
          when (isActiveSingleType argType) $
            unless (isThisAccess arg) $
              tcError ActiveMatchError
          when (any isBottomType (typeComponents argType)) $
               pushError arg BottomTypeInferenceError
          when (any isNullType (typeComponents argType)) $
               pushError arg NullTypeInferenceError

        checkForPrivateExtractors arg = mapM (checkForPrivateExtractor arg)
        checkForPrivateExtractor matchArg p@ExtractorPattern{name, arg} = do
          local (pushBT p) $ typecheckPrivateModifier matchArg name
          checkForPrivateExtractor arg arg
        checkForPrivateExtractor Tuple{args}
                                 Tuple{args = patternArgs} =
          zipWithM_ checkForPrivateExtractor args patternArgs
        checkForPrivateExtractor MaybeValue{mdt = JustData{e}}
                                 MaybeValue{mdt = JustData{e = patternE}} =
          checkForPrivateExtractor e patternE
        checkForPrivateExtractor _ _ = return ()

        checkAllHandlersSameType clauses = do
          let types = map (AST.getType . mchandler) clauses
          result <- unifyTypes types
          case result of
            Just ty -> return ty
            Nothing ->
              case find (hasKnownType . mchandler) clauses of
                Just e -> do
                  let ty = AST.getType $ mchandler e
                  mapM_ (`assertSubtypeOf` ty) types
                  return ty
                Nothing ->
                  tcError MatchInferenceError

        hasKnownType e =
            let ty = AST.getType e
            in all (not . isBottomType) (typeComponents ty) &&
               not (isNullType ty)

        getPatternVars pt pattern =
            local (pushBT pattern) $
              doGetPatternVars pt pattern

        doGetPatternVars pt va@(VarAccess {qname}) = do
          when (isThisAccess va) $
            tcError ThisReassignmentError
          return [(qnlocal qname, pt)]

        doGetPatternVars pt mcp@(MaybeValue{mdt = JustData {e}})
          | isMaybeType pt =
              let innerType = getResultType pt
              in getPatternVars innerType e
          | otherwise = tcError $ PatternTypeMismatchError mcp pt

        doGetPatternVars pt fcall@(FunctionCall {qname, args = []})
          | isADT pt =
              getPatternVars pt (fcall {args = [Skip {emeta = emeta fcall}]})
          | otherwise = do
              header <- findMethod pt (qnlocal qname)
              let hType = htype header
                  extractedType = getResultType hType
              unless (isUnitType extractedType) $ do
                     let expectedLength = if isTupleType extractedType
                                          then length (getArgTypes extractedType)
                                          else 1
                     tcError $ PatternArityMismatchError (qnlocal qname)
                               expectedLength 0
              getPatternVars pt (fcall {args = [Skip {emeta = emeta fcall}]})

        doGetPatternVars pt fcall@(FunctionCall {qname, args = [arg]})
          | isADT pt = do
              c <- findADTClass (classType (show $ qnlocal qname) [])
              let fields = drop 1 $ cfields c
                  bindings =
                    zip (getTypeParameters (cname c)) (getTypeParameters pt)
                  fieldTypes = map (replaceTypeVars bindings . ftype) fields
                  expectedLength = length fields
                  actualLength
                    | Tuple{args} <- arg = length args
                    | Skip{} <- arg = 0
                    | otherwise = 1
              unless (actualLength == expectedLength) $
                     tcError $ PatternArityMismatchError (qnlocal qname)
                               expectedLength actualLength
              case arg of
                Tuple{args} -> do
                  vars <- zipWithM getPatternVars fieldTypes args
                  return $ concat $ reverse vars
                Skip{} -> return []
                _ -> doGetPatternVars (head fieldTypes) arg
          | otherwise = do
              unless (isRefType pt) $
                tcError $ NonCallableTargetError pt
              header <- findMethod pt (qnlocal qname)
              let hType = htype header
              unless (isMaybeType hType) $
                tcError $ NonMaybeExtractorPatternError fcall
              let extractedType = getResultType hType
                  expectedLength
                    | isTupleType extractedType = length (getArgTypes extractedType)
                    | isUnitType extractedType = 0
                    | otherwise = 1
                  actualLength
                    | Tuple{args} <- arg = length args
                    | Skip{} <- arg = 0
                    | otherwise = 1
              unless (actualLength == expectedLength) $
                     tcError $ PatternArityMismatchError (qnlocal qname)
                               expectedLength actualLength
              getPatternVars extractedType arg

        doGetPatternVars pt fcall@(FunctionCall {args}) = do
          let tupMeta = getMeta $ head args
              tupArg = Tuple {emeta = tupMeta, args}
          getPatternVars pt (fcall {args = [tupArg]})

        doGetPatternVars pt tuple@(Tuple {args}) = do
          unless (isTupleType pt) $
            tcError $ PatternTypeMismatchError tuple pt
          let elemTypes = getArgTypes pt

          varLists <- zipWithM getPatternVars elemTypes args
          return $ concat $ reverse varLists

        doGetPatternVars pt typed@(TypedExpr {body}) =
          getPatternVars pt body

        doGetPatternVars pt pattern = return []

        checkPattern pattern argty =
            local (pushBT pattern) $
              doCheckPattern pattern argty

        doCheckPattern pattern@(FunctionCall {args = []}) argty = do
          let meta = getMeta pattern
              unitArg = Skip {emeta = meta}
          checkPattern (pattern {args = [unitArg]}) argty

        doCheckPattern pattern@(FunctionCall {emeta
                                             ,qname
                                             ,args = [arg]}) argty
          | isADT argty
          , Tuple{} <- arg = do
              let name = qnlocal qname
              -- TODO: Catch MethodNotFoundError and rethrow as
              -- ADTCaseNotFoundError (or similar)
              header <- findMethod argty name
              c <- findADTClass (classType (show name) [])
              let bindings =
                    zip (getTypeParameters (cname c)) (getTypeParameters argty)
                  fields = drop 1 $ cfields c
                  fieldTypes =
                    if null fields
                    then [unitType]
                    else map (replaceTypeVars bindings . ftype) fields
              eArg <- checkPattern arg $ tupleType fieldTypes
              return $ setArrowType (arrowType [] intType) $
                       setType argty AdtExtractorPattern {emeta
                                                         ,name
                                                         ,arg = eArg
                                                         ,adtClassDecl = c}
          | not (isADT argty) = do
              let name = qnlocal qname
              header <- findMethod argty name
              let hType = htype header
                  extractedType = getResultType hType
              eArg <- checkPattern arg extractedType
              matchArgumentLength argty header []
              checkReturnEncapsulation (qnlocal qname) extractedType argty
              return $ setArrowType (arrowType [] hType) $
                       setType argty ExtractorPattern {emeta
                                                      ,name
                                                      ,arg = eArg}

        doCheckPattern pattern@(FunctionCall {args}) argty = do
          let tupMeta = getMeta $ head args
              tupArg = Tuple {emeta = tupMeta, args}
          checkPattern (pattern {args = [tupArg]}) argty

        doCheckPattern pattern@(MaybeValue{mdt = JustData {e}}) argty = do
          unless (isMaybeType argty) $
            tcError $ PatternTypeMismatchError pattern argty
          let innerType = getResultType argty
          eExpr <- checkPattern e innerType
          return $ setType argty (pattern {mdt = JustData {e = eExpr}})

        doCheckPattern pattern@(Tuple{args}) tupty = do
          let argTypes = getArgTypes tupty
          unless (length argTypes == length args) $
            tcError $ PatternTypeMismatchError pattern tupty
          eArgs <- zipWithM checkPattern args argTypes
          return $ setType tupty (pattern {args=eArgs})

        doCheckPattern pattern@(TypedExpr{body, ty}) argty = do
          eBody <- checkPattern body argty
          ty' <- checkType ty
          argty `assertSubtypeOf` ty'
          return $ setType ty' eBody

        doCheckPattern pattern argty
            | isValidPattern pattern = hasType pattern argty
            | otherwise = tcError $ InvalidPatternError pattern

        checkClause pt clause@MatchClause{mcpattern, mchandler, mcguard} = do
          vars <- getPatternVars pt mcpattern
          let duplicates = vars \\ nub vars
          unless (null duplicates) $
                 tcError $
                 DuplicatePatternVarError (fst (head duplicates)) mcpattern
          let withLocalEnv = local (extendEnvironmentImmutable vars)
          ePattern <- withLocalEnv $ checkPattern mcpattern pt
          eHandler <- withLocalEnv $ typecheck mchandler
          eGuard <- withLocalEnv $ hasType mcguard boolType
          return $ clause {mcpattern = extend makePattern ePattern
                          ,mchandler = eHandler
                          ,mcguard = eGuard}

    doTypecheck borrow@(Borrow{target, name, body}) = do
      eTarget <- typecheck target
      let targetType   = AST.getType eTarget
          borrowedType = makeStackbound targetType
          eTarget'     = setType borrowedType eTarget
      targetIsLinear <- isLinearType targetType
      let root         = findRoot target
          borrowEnv    =
            (case root of
               VarAccess{qname} ->
                 if targetIsLinear
                 then dropLocal (qnlocal qname)
                 else id
               _ -> id) . extendEnvironmentImmutable [(name, borrowedType)]
      eBody <- local borrowEnv $ typecheck body
               `catchError` handleBurying root
      let bodyTy = AST.getType eBody
      return $ setType bodyTy borrow{target = eTarget', body = eBody}
      where
        handleBurying :: Expr -> TCError -> TypecheckM Expr
        handleBurying VarAccess{qname}
                      (TCError err@(UnboundVariableError unbound) bt) =
          if unbound == qname
          then throwError $ TCError (BuriedVariableError qname) bt
          else throwError $ TCError err bt
        handleBurying _ (TCError err bt) =
          throwError $ TCError err bt

    --  E |- cond : bool
    --  E |- body : t
    -- -----------------------
    --  E |- while cond body : t
    doTypecheck while@(While {cond, body}) =
        do eCond <- hasType cond boolType
           eBody <- typecheck body
           return $ setType unitType while {cond = eCond, body = eBody}

    --  E |- cond : bool
    --  E |- body : t
    -- -----------------------
    --  E |- do body while cond : t
    doTypecheck while@(DoWhile {cond, body}) =
        do eCond <- hasType cond boolType
           eBody <- typecheck body
           return $ setType unitType while {cond = eCond, body = eBody}

    --  E |- val : Fut t
    -- ------------------
    --  E |- get val : t
    doTypecheck get@(Get {val}) =
        do eVal <- typecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty || isStreamType ty) $
                  pushError eVal $ ExpectingOtherTypeError
                                     "a future or a stream" ty
           return $ setType (getResultType ty) get {val = eVal}

    --  E |- val : Fut t
    -- ------------------
    --  E |- forward val : t
    doTypecheck forward@(Forward {forwardExpr}) =
        do eExpr <- typecheck forwardExpr
           let ty = AST.getType eExpr
           unless (isFutureType ty) $
                  pushError eExpr $ ExpectingOtherTypeError
                                      "a future" ty
           context <- asks currentExecutionContext
           case context of
             MethodContext mdecl -> do
                let returnType = methodType mdecl
                unlessM (getResultType ty `subtypeOf` returnType) $
                      pushError eExpr $ ForwardTypeError returnType ty
                return $ setType (getResultType ty) forward {forwardExpr = eExpr}
             ClosureContext (Just mty) -> do
                mty' <- resolveType mty
                unlessM (getResultType ty `subtypeOf` mty') $
                    pushError eExpr $ ForwardTypeClosError mty' ty
                return $ setType (getResultType ty) forward {forwardExpr = eExpr}
             ClosureContext Nothing -> tcError ClosureForwardError
             _ -> pushError eExpr ForwardInFunction

    --  E |- val : t
    --  isStreaming(currentMethod)
    -- -----------------------------
    --  E |- yield val : unit
    doTypecheck yield@(Yield {val}) =
        do eVal <- typecheck val
           context <- asks currentExecutionContext
           case context of
             MethodContext mtd -> do
               let mType = methodType mtd
                   eType = AST.getType eVal
               unless (isStreamMethod mtd) $
                      tcError $ NonStreamingContextError yield
               eType `assertSubtypeOf` mType
               return $ setType unitType yield {val = eVal}
             _ -> tcError $ NonStreamingContextError yield


    --  E |- expr : t
    --  E |- currentMethod : _ -> t
    -- -----------------------------
    --  E |- return expr : _|_
    doTypecheck ret@(Return {val}) =
        do eVal <- typecheck val
           context <- asks currentExecutionContext
           ty <- case context of
                   MethodContext mtd -> return $ methodType mtd
                   FunctionContext _ ty -> return ty
                   ClosureContext (Just ty) -> return ty
                   ClosureContext Nothing -> tcError ClosureReturnError

           let eType = AST.getType eVal
           unlessM (eType `subtypeOf` ty) $
             pushError ret $ ExpectingOtherTypeError
                (show ty ++ " (type of the enclosing method or function)") eType
           return $ setType bottomType ret {val = eVal}

    --  isStreaming(currentMethod)
    -- ----------------------------
    --  E |- eos : unit
    doTypecheck eos@(Eos {}) =
        do context <- asks currentExecutionContext
           case context of
             MethodContext mtd -> do
               unless (isStreamMethod mtd) $
                      tcError $ NonStreamingContextError eos
               return $ setType unitType eos
             _ -> tcError $ NonStreamingContextError eos

    --  E |- s : Stream t
    -- ---------------------
    --  E |- eos s : bool
    doTypecheck iseos@(IsEos {target}) =
        do eTarget <- typecheck target
           let targetType = AST.getType eTarget
           unless (isStreamType targetType) $
                  pushError eTarget $ ExpectingOtherTypeError
                                        "a stream" targetType
           return $ setType boolType iseos{target = eTarget}

    --  E |- s : Stream t
    -- ---------------------------
    --  E |- s.next() : Stream t
    doTypecheck next@(StreamNext {target}) =
        do eTarget <- typecheck target
           let targetType = AST.getType eTarget
           unless (isStreamType targetType) $
                  pushError eTarget $ ExpectingOtherTypeError
                                        "a stream" targetType
           return $ setType targetType next{target = eTarget}

    --
    --    ------------------ :: suspend
    --    suspend : unit
    doTypecheck suspend@(Suspend {}) =
        return $ setType unitType suspend

    --    f : Fut T
    --    ------------------ :: await
    --    await f : unit
    doTypecheck await@(Await {val}) =
        do eVal <- typecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty) $
                  pushError eVal $ ExpectingOtherTypeError "a future" ty
           return $ setType unitType await {val = eVal}

    --    f : Fut T
    --    c : T -> T'
    --    ------------------ :: chain
    --    f then c : Fut T'
    doTypecheck futureChain@(FutureChain {future, chain}) =
        do eFuture <- typecheck future
           eChain <- typecheck chain
           let ty = AST.getType eFuture
           unless (isFutureType ty) $
                  pushError eFuture $ ExpectingOtherTypeError "a future" ty
           let chainType = AST.getType eChain
               returnType = getResultType chainType
               expectedFunType = arrowType [getResultType ty] returnType
           unless (isArrowType chainType) $
                  pushError eChain $ NonFunctionTypeError chainType
           chainType `assertSubtypeOf` expectedFunType
           return $ setType (futureType returnType)
                            futureChain {future = eFuture, chain = eChain}

    --  E |- target : (c1:t1 , ..., cn:tn)
    --  1 <= j <= n
    -- ---------------------------
    --  E |- target.j : tj
    doTypecheck ta@(TupleAccess {target, compartment}) = do
      eTarget <- typecheck target
      let targetType = AST.getType eTarget
      unless (isTupleType targetType) $
        tcError $ InvalidTupleTargetError eTarget compartment targetType
      let compartments = getArgTypes targetType
      unless (0 <= compartment && compartment < tupleLength targetType) $
        tcError $ InvalidTupleAccessError eTarget compartment
      return $ setType (compartments !! compartment) ta {target = eTarget}

    --  E |- target : t'
    --  fieldLookup(t', name) = t
    -- ---------------------------
    --  E |- target.name : t
    doTypecheck fAcc@(FieldAccess {target, name}) = do
      eTarget <- typecheck target
      let targetType = AST.getType eTarget
      isActive <- isActiveType targetType
      let accessedType = if isThisAccess eTarget && isActive
                         then makeLocal targetType
                         else targetType
          eTarget' = setType accessedType eTarget
      unless (isThisAccess eTarget' ||
              isPassiveClassType targetType && not (isModeless targetType)) $
        tcError $ CannotReadFieldError eTarget
      fdecl <- findField targetType name
      let ty = ftype fdecl
      checkFieldEncapsulation name eTarget ty
      return $ setType ty fAcc {target = eTarget'}

    --  E |- lhs : t
    --  isLval(lhs)
    --  E |- rhs : t
    -- ------------------------
    --  E |- name = rhs : unit
    doTypecheck assign@(Assign {lhs = lhs@VarAccess{qname}, rhs}) =
        do eLhs <- typecheck lhs
           varIsMutable <- asks $ isMutableLocal qname
           varIsLocal <- asks $ isLocal qname
           unless varIsMutable $
                  if varIsLocal
                  then tcError $ ImmutableVariableError qname
                  else pushError eLhs NonAssignableLHSError
           eRhs <- hasType rhs (AST.getType eLhs)
           return $ setType unitType assign {lhs = eLhs, rhs = eRhs}

    doTypecheck assign@(Assign {lhs, rhs}) =
        do eLhs <- typecheck lhs
           unless (isLval eLhs) $
                  pushError eLhs NonAssignableLHSError
           context <- asks currentExecutionContext
           case context of
             MethodContext mtd ->
               unless (isConstructor mtd) $
                 assertNotValField eLhs
             _ -> assertNotValField eLhs
           eRhs <- hasType rhs (AST.getType eLhs)
           return $ setType unitType assign {lhs = eLhs, rhs = eRhs}
        where
          assertNotValField f
              | FieldAccess {target, name} <- f = do
                  let targetType = AST.getType target
                  fdecl <- findField targetType name
                  when (isValField fdecl) $
                       tcError $ ValFieldAssignmentError name targetType
              | otherwise = return ()


    doTypecheck fun@(FunctionAsValue {qname, typeArgs}) = do
         result <- findVar qname
         case result of
           Nothing -> tcError $ UnboundFunctionError qname
           Just (qname', ty) -> do
             unless (isArrowType ty) $
                    tcError (NonFunctionTypeError ty)
             typeArgs' <- mapM checkType typeArgs
             let typeParams = getTypeParameters ty
             unless (length typeArgs' == length typeParams) $
                    tcError $ WrongNumberOfFunctionTypeArgumentsError qname
                              (length typeParams) (length typeArgs')
             checkTypeArgumentBounds typeParams typeArgs'

             let bindings = zip typeParams typeArgs'
                 ty' = replaceTypeVars bindings ty
                 ty'' = setTypeParameters ty' []

             return $ setType ty'' fun{qname = qname', typeArgs = typeArgs'}

    --  name : t \in E
    -- ----------------
    --  E |- name : t
    doTypecheck var@(VarAccess {qname}) = do
      when (Util.isStatement var) $
        tcWarning $ ExpressionResultIgnoredWarning var

      result <- findVar qname
      case result of
        Just (qname', ty) -> do
          result <- asks $ classLookup (classType (show $ qnlocal qname') [])
          unless (isNothing result || null (fromJust result)) $
            tcWarning $ ShadowingADTCaseWarning (qnlocal qname)

          if isArrowType ty
          then do
            let typeParams = getTypeParameters ty
            unless (null typeParams) $
               tcError $ WrongNumberOfFunctionTypeArgumentsError qname
                         (length typeParams) 0
            return $ setType ty var{qname = qname'}
          else return $ setType ty var{qname = qname'}
        Nothing -> tcError $ UnboundVariableError qname


    --  e : t \in E
    --  isLval e
    -- ----------------------
    --  E |- consume e : t
    doTypecheck cons@(Consume {target}) =
        do eTarget <- typecheck target
           unless (isLval eTarget) $
                  tcError $ CannotConsumeError eTarget
           whenM (isGlobalVar eTarget) $
                 tcError $ CannotConsumeError eTarget
           unlessM (isMutableTarget eTarget) $
                 tcError $ ImmutableConsumeError eTarget
           let ty = AST.getType eTarget
           unless (canBeNull ty || isMaybeType ty) $
                  tcError $ CannotConsumeTypeError eTarget
           return $ setType ty cons {target = eTarget}
        where
          isGlobalVar VarAccess{qname} =
              liftM not $ asks $ isLocal qname
          isGlobalVar _ = return False

          isMutableTarget VarAccess{qname} =
              asks $ isMutableLocal qname
          isMutableTarget FieldAccess{target, name} = do
              let targetType = AST.getType target
              fdecl <- findField targetType name
              return $ not $ isValField fdecl
          isMutableTarget _ = return True


    --
    -- ----------------------
    --  E |- null : nullType
    doTypecheck e@Null {} = do
      when (Util.isStatement e) $
        tcWarning $ ExpressionResultIgnoredWarning e

      return $ setType nullType e

    --
    -- ------------------
    --  E |- true : bool
    doTypecheck true@BTrue {} = do
      when (Util.isStatement true) $
        tcWarning $ ExpressionResultIgnoredWarning true

      return $ setType boolType true

    --
    -- ------------------
    --  E |- false : bool
    doTypecheck false@BFalse {} = do
      when (Util.isStatement false) $
        tcWarning $ ExpressionResultIgnoredWarning false

      return $ setType boolType false

   ---  |- ty
    --  classLookup(ty) = _
    --  methodLookup(ty, "init") = (t1 .. tn, _)
    --  E |- arg1 : t1 .. argn : tn
    --  ty != Main
    -- -----------------------
    --  E |- new ty(args) : ty
    doTypecheck new@(NewWithInit {ty, args})
      | isRefAtomType ty && null (getTypeParameters ty) = do
          formal <- findFormalRefType ty
          header <- if isTypeSynonym formal
                    then findConstructor (typeSynonymRHS formal) args
                    else findConstructor formal args
          let typeParams = getTypeParameters formal
              argTypes = map ptype (hparams header)

          (eArgs, resolvedTy, _) <-
              inferenceCall fakeInitCall typeParams argTypes formal

          checkArgsEncapsulation eArgs resolvedTy

          resolvedTy' <- checkType resolvedTy

          return $ setArrowType (arrowType argTypes unitType) $
                   setType resolvedTy' new{ty = resolvedTy', args = eArgs}

      | otherwise = do
          ty' <- checkType ty

          header <- findConstructor ty' args
          let expectedTypes = map ptype (hparams header)

          (eArgs, _) <- matchArguments args expectedTypes

          -- Could be a new array
          when (isRefAtomType ty) $ do
            formal <- findFormalRefType ty
            let typeParams = getTypeParameters formal
                typeArgs = getTypeParameters ty'
            checkTypeArgumentBounds typeParams typeArgs

          checkArgsEncapsulation eArgs ty'

          return $ setArrowType (arrowType expectedTypes unitType) $
                   setType ty' new{ty = ty', args = eArgs}

      where
        findConstructor ty args = do
          unless (isClassType ty && not (isMainType ty)) $
                 tcError $ ObjectCreationError ty
          header <- findMethod ty constructorName
          when (isPrivateMethodHeader header) $
               tcError $ PrivateAccessModifierTargetError constructorName
          matchArgumentLength ty header args
          return header

        fakeInitCall =
          MethodCall{emeta = emeta new
                    ,typeArguments = []
                    ,target = setType ty Skip{emeta = emeta new}
                    ,name = constructorName
                    ,args
                    }

    --  E |- n : int
    --  E |- m : int
    --  E |- k : int
    -- ----------------------------
    --  E |- [n..m by k] : Range
    doTypecheck range@(RangeLiteral {start, stop, step}) = do
        when (Util.isStatement range) $
          tcWarning $ ExpressionResultIgnoredWarning range

        eStart <- hasType start intType
        eStop  <- hasType stop  intType
        eStep  <- hasType step  intType

        return $ setType rangeType range{start = eStart
                                        ,stop = eStop
                                        ,step = eStep}


    -- JOY for-comprehension
    -- returnType is unittye, is changed during the second typechecking when it has been desugaraed into a method call
    doTypecheck for@(For {sources, body}) = do
      sourcesTyped <- typeCheckSources sources
      forVarList <- getForVarTypeList sources
      bodyTyped <- typecheckBody forVarList body
      return $ setType unitType for{sources = sourcesTyped,
                                      body = bodyTyped}
      where
        typeCheckSources :: [ForSource] -> TypecheckM [ForSource]
        typeCheckSources sourceList =  do
          typedSources <- mapM typeCheckSource sourceList
          return typedSources
        typeCheckSource fors@(ForSource{forVarType, collection}) = do
            collectionTyped <- doTypecheck collection
            let collectionType = AST.getType collectionTyped
            let forVarType = return $ getInnerType collectionType
            return fors{forVarType = forVarType,
                        collection = setType collectionType collectionTyped}

        getForVarTypeList sourceList = mapM getForVarType sourceList
        getForVarType ForSource{forVar, collection} = do
          collectionTyped <- doTypecheck collection
          let collectionType = AST.getType collectionTyped
          unless (isRefType collectionType || isArrayType collectionType || isRangeType collectionType) $
             pushError collection $ NonIterableError collectionType
          let forVarType = getInnerType collectionType
          return (forVar, forVarType)

        getInnerType collectionType
         | isArrayType collectionType = getResultType collectionType
         | isRangeType collectionType = intType
         | otherwise = head $ getTypeParameters collectionType

        addIteratorVariable forVarList = extendEnvironmentImmutable forVarList
        typecheckBody forVarList = local (addIteratorVariable forVarList) . typecheck

   ---  |- ty
    --  E |- size : int
    -- ----------------------------
    --  E |- new [ty](size) : [ty]
    doTypecheck new@(ArrayNew {ty, size}) =
        do ty' <- checkType ty
           eSize <- hasType size intType
           return $ setType (arrayType ty') new{ty = ty', size = eSize}

    --  E |- arg1 : ty .. E |- argn : ty
    -- ----------------------------------
    --  E |- [arg1, .., argn] : [ty]
    doTypecheck arr@(ArrayLiteral {args}) = do
        when (Util.isStatement arr) $
          tcWarning $ ExpressionResultIgnoredWarning arr

        when (null args) $
             tcError EmptyArrayLiteralError
        eArg1 <- doTypecheck (head args)
        let ty = AST.getType eArg1
        eArgs <- mapM (`hasType` ty) args
        return $ setType (arrayType ty) arr{args = eArgs}

    --  E |- target : [ty]
    --  E |- index : int
    -- -------------------------
    --  E |- target[index] : ty
    doTypecheck arrAcc@(ArrayAccess {target, index}) =
        do eTarget <- typecheck target
           let targetType = AST.getType eTarget
           unless (isArrayType targetType) $
                  pushError eTarget $ NonIndexableError targetType
           eIndex <- hasType index intType
           return $ setType (getResultType targetType)
                            arrAcc{target = eTarget, index = eIndex}

    --  E |- target : [_]
    -- -------------------------
    --  E |- |target| : int
    doTypecheck arrSize@(ArraySize {target}) =
        do eTarget <- typecheck target
           let targetType = AST.getType eTarget
           unless (isArrayType targetType) $
                  pushError eTarget $ NonSizeableError targetType
           return $ setType intType arrSize{target = eTarget}

    --  count("{}", stringLit) = n
    --  E |- arg1 : t1 .. E |- argn : tn
    -- ---------------------------------------------
    --  E |- print(stringLit, arg1 .. argn) : unit
    doTypecheck e@(Print {args}) =
        do eArgs <- mapM typecheck args
           let fst = head eArgs
               rest = tail eArgs
               fstString = if isStringObjectType $ AST.getType fst
                           then fromJust $ getSugared fst
                           else fst
               unprintable = filter (not . isPrintable . AST.getType) eArgs
               unprintableHead = head unprintable
           unless (isStringLiteral fstString) $
                  pushError fst FormatStringLiteralError
           unless (null unprintable) $
                pushError unprintableHead $
                    UnprintableExpressionError (AST.getType unprintableHead)
           let formatString = stringLit fstString
               noArgs = T.count (T.pack "{}") (T.pack formatString)
           unless (noArgs == length rest) $
                 tcError $ WrongNumberOfPrintArgumentsError (length rest) noArgs
           let eFormatString = setType stringType $
                               StringLiteral (emeta fstString) formatString
               newArgs = eFormatString : rest
           return $ setType unitType e {args = newArgs}

    --  E |- arg : int
    -- ------------------------
    --  E |- exit(arg) : unit
    doTypecheck exit@(Exit {args}) =
        do eArgs <- mapM typecheck args
           let expectedTypes = [intType]
           unless (length args == length expectedTypes) $
             tcError $ WrongNumberOfFunctionArgumentsError
                       (topLevelQName (Name "exit"))
                       (length expectedTypes) (length args)
           matchArguments args expectedTypes
           return $ setType unitType exit {args = eArgs}

    -- ------------------------
    --  E |- abort() : _|_
    doTypecheck abort@Abort{args} = do
      sty <- checkType stringObjectType
      let expectedTypes = [sty]
      unless (length args == length expectedTypes) $
        tcError $ WrongNumberOfFunctionArgumentsError
                  (topLevelQName (Name "abort"))
                  (length expectedTypes) (length args)
      eArgs <- mapM typecheck args
      matchArguments args expectedTypes
      return $ setType bottomType abort{args=([]::[Expr])}

    doTypecheck stringLit@(StringLiteral {}) = do
      when (Util.isStatement stringLit) $
        tcWarning $ ExpressionResultIgnoredWarning stringLit

      return $ setType stringType stringLit

    doTypecheck charLit@(CharLiteral {}) = do
      when (Util.isStatement charLit) $
        tcWarning $ ExpressionResultIgnoredWarning charLit

      return $ setType charType charLit

    doTypecheck intLit@(IntLiteral {}) = do
      when (Util.isStatement intLit) $
        tcWarning $ ExpressionResultIgnoredWarning intLit

      return $ setType intType intLit

    doTypecheck uintLit@(UIntLiteral {}) = do
      when (Util.isStatement uintLit) $
        tcWarning $ ExpressionResultIgnoredWarning uintLit

      return $ setType uintType uintLit

    doTypecheck realLit@(RealLiteral {}) = do
      when (Util.isStatement realLit) $
        tcWarning $ ExpressionResultIgnoredWarning realLit

      return $ setType realType realLit

   ---  |- ty
    -- ---------------------
    -- E |- embed ty _ : ty
    doTypecheck embed@(Embed {ty, embedded}) =
        do ty' <- checkType ty
           embedded' <- mapM typecheckPair embedded
           return $ setType ty' embed{ty = ty'
                                     ,embedded = embedded'}
        where
          typecheckPair (code, e) = do
            e' <- typecheck e
            return (code, e')

    --  E |- operand : bool
    -- -------------------------
    --  E |- not operand : bool
    doTypecheck unary@(Unary {uop, operand}) = do
        let isExpected | uop == Identifiers.NOT = isBoolType
                       | uop == Identifiers.NEG = \ty -> isNumeric ty &&
                                                         not (isUIntType ty)
        eOperand <- typecheck operand
        let eType = AST.getType eOperand
        unless (isExpected eType) $
               tcError $ UnaryOperandMismatchError uop eType
        let resultType | uop == Identifiers.NOT = boolType
                       | uop == Identifiers.NEG = eType
        return $ setType resultType unary {operand = eOperand}

    --  op \in {and, or}
    --  E |- loper : bool
    --  E |- roper : bool
    -- ----------------------------
    --  E |- loper op roper : bool
    --
    --  op \in {<, >, <=, >=}
    --  E |- loper : t
    --  E |- roper : t'
    --  isNumeric(t)
    --  isNumeric(t')
    -- ----------------------------
    --  E |- loper op roper : bool
    --
    -- etc.
    doTypecheck bin@(Binop {binop, loper, roper})
      | binop `elem` boolOps = do
          checkThatResultIsUsed

          eLoper <- typecheck loper
          eRoper <- typecheck roper
          let lType = AST.getType eLoper
              rType = AST.getType eRoper
          unless (isBoolType lType && isBoolType rType) $
            tcError $ BinaryOperandMismatchError binop "boolean" lType rType
          return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` cmpOps = do
          checkThatResultIsUsed

          eLoper <- typecheck loper
          eRoper <- typecheck roper
          let lType = AST.getType eLoper
              rType = AST.getType eRoper
          unless (isNumeric lType && isNumeric rType) $
            tcError $ BinaryOperandMismatchError binop "numeric" lType rType
          return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` eqOps = do
          checkThatResultIsUsed

          eLoper <- typecheck loper
          eRoper <- typecheck roper

          let lType = AST.getType eLoper
          let rType = AST.getType eRoper

          eLoper' <- if isNullType lType
                     then coerceNull eLoper rType
                     else return eLoper
          eRoper' <- if isNullType rType
                     then coerceNull eRoper lType
                     else return eRoper

          let lType' = AST.getType eLoper'
          let rType' = AST.getType eRoper'

          unlessM (lType' `subtypeOf` rType') $
            unlessM (rType' `subtypeOf` lType') $
              tcError $ IdComparisonTypeMismatchError lType' rType'

          unless (isTypeVar lType') $ checkIdComparisonSupport lType'
          unless (isTypeVar rType') $ checkIdComparisonSupport rType'

          when (isStringObjectType lType') $
            unless (isNullLiteral eRoper || isNullLiteral eLoper) $
                         tcWarning StringIdentityWarning
          when (isTypeVar lType') $
            tcWarning PolymorphicIdentityWarning

          return $ setType boolType bin {loper = eLoper', roper = eRoper'}
      | binop `elem` arithOps = do
          checkThatResultIsUsed

          eLoper <- typecheck loper
          eRoper <- typecheck roper
          let lType = AST.getType eLoper
              rType = AST.getType eRoper
          unless (isNumeric lType && isNumeric rType) $
            tcError $ BinaryOperandMismatchError binop "numeric" lType rType
          return $ setType (coerceTypes lType rType) bin {loper = eLoper, roper = eRoper}
      | otherwise = tcError $ UndefinedBinaryOperatorError binop
      where
        checkThatResultIsUsed = when (Util.isStatement bin) $
                                  tcWarning $ ExpressionResultIgnoredWarning bin
        boolOps  = [Identifiers.AND, Identifiers.OR]
        cmpOps   = [Identifiers.LT, Identifiers.GT, Identifiers.LTE, Identifiers.GTE]
        eqOps    = [Identifiers.EQ, NEQ]
        arithOps = [PLUS, MINUS, TIMES, DIV, MOD]
        coerceTypes ty1 ty2
            | isRealType ty1 = realType
            | isRealType ty2 = realType
            | isUIntType ty1 = uintType
            | otherwise = intType
        checkIdComparisonSupport ty
            | isMaybeType ty = checkIdComparisonSupport $ getResultType ty
            | isArrayType ty = checkIdComparisonSupport $ getResultType ty
            | isTupleType ty = mapM_ checkIdComparisonSupport $ getArgTypes ty
            | otherwise = do
                id <- checkType (refType "Id")
                includesId <- ty `includesMarkerTrait` id
                unless (includesId || isPrimitive ty ||
                        isNullType ty || isBottomType ty ||
                        isAbstractTraitType ty) $
                  tcError $ IdComparisonNotSupportedError ty

    doTypecheck e = error $ "Cannot typecheck expression " ++ show (ppExpr e)

canBeNull ty =
  isRefType ty || isFutureType ty || isArrayType ty ||
  isStreamType ty || isCapabilityType ty || isArrowType ty || isParType ty

checkEncapsulation :: Expr -> Name -> Type -> [Expr] -> TypecheckM ()
checkEncapsulation target name returnType args = do
  let targetType = AST.getType target
  unless (isThisAccess target) $ do
    checkArgsEncapsulation args targetType
    checkReturnEncapsulation name returnType targetType

checkArgsEncapsulation :: [Expr] -> Type -> TypecheckM ()
checkArgsEncapsulation args targetType = do
  checkSubordinateArgs args targetType
  checkLocalArgs args targetType

checkReturnEncapsulation :: Name -> Type -> Type -> TypecheckM ()
checkReturnEncapsulation name returnType targetType = do
  checkSubordinateReturn name returnType targetType
  checkLocalReturn name returnType targetType

checkSubordinateReturn :: Name -> Type -> Type -> TypecheckM ()
checkSubordinateReturn name returnType targetType = do
  subordReturn <- isSubordinateType returnType
  targetIsEncaps <- isEncapsulatedType targetType
  when subordReturn $
       unless targetIsEncaps $
              tcError $ SubordinateReturnError name returnType

checkSubordinateArgs :: [Expr] -> Type -> TypecheckM ()
checkSubordinateArgs args targetType = do
  subordinateArgs <- filterM (isSubordinateType . AST.getType) args
  let subordinateArg = head subordinateArgs
  targetIsEncaps <- isEncapsulatedType targetType
  unless (null subordinateArgs) $
         unless targetIsEncaps $
                tcError $ SubordinateArgumentError subordinateArg

checkFieldEncapsulation :: Name -> Expr -> Type -> TypecheckM ()
checkFieldEncapsulation name target fieldType = do
  fieldIsSubord <- isSubordinateType fieldType
  let targetType = AST.getType target
  targetIsEncaps <- isEncapsulatedType targetType
  when fieldIsSubord $
       unless (targetIsEncaps || isThisAccess target) $
              tcError $ SubordinateFieldError name

checkLocalArgs :: [Expr] -> Type -> TypecheckM ()
checkLocalArgs args targetType =
  when (isActiveSingleType targetType) $ do
    localArgs <- filterM (isLocalType . AST.getType) args
    let localArg = head localArgs
    unless (null localArgs) $
      tcError $ ThreadLocalArgumentError localArg

    let nonSharablePolymorphicArgs =
          filter (any nonSharableTypeVar . typeComponents . AST.getType) args
        nonSharableArg = head nonSharablePolymorphicArgs
        nonSharableType =
          head $ filter nonSharableTypeVar . typeComponents . AST.getType $
                  nonSharableArg
    unless (null nonSharablePolymorphicArgs) $
      tcError $ PolymorphicArgumentSendError nonSharableArg nonSharableType

    let sharedArrays = filter (\arg -> isArrayType (AST.getType arg) &&
                                       not (isArrayLiteral arg)) args
    unless (null sharedArrays) $
      tcWarning SharedArrayWarning

checkLocalReturn :: Name -> Type -> Type -> TypecheckM ()
checkLocalReturn name returnType targetType =
  when (isActiveSingleType targetType) $ do
    localReturn <- isLocalType returnType
    when localReturn $
       tcError $ ThreadLocalReturnError name returnType
    let nonSharable =
          find nonSharableTypeVar $ typeComponents returnType
    when (isJust nonSharable) $
       tcError $ PolymorphicReturnError name (fromJust nonSharable)
    when (isArrayType returnType) $
       tcWarning SharedArrayWarning

--  classLookup(ty) = _
-- ---------------------
--  null : ty
coerceNull null ty
    | canBeNull ty = return $ setType ty null
    | isNullType ty = tcError NullTypeInferenceError
    | otherwise =
        tcError $ CannotBeNullError ty

coercedInto :: Type -> Type -> TypecheckM Type
coercedInto actual expected
  | hasResultType expected && hasResultType actual = do
      resultType <- getResultType actual `coercedInto` getResultType expected
      return $ setResultType actual resultType
  | isTupleType actual && isTupleType expected = do
      let actualArgTypes = getArgTypes actual
          expectedArgTypes = getArgTypes expected
      argTypes <- zipWithM coercedInto actualArgTypes expectedArgTypes
      return $ setArgTypes actual argTypes
  | isNullType actual = do
      when (isNullType expected) $
        tcError NullTypeInferenceError
      unless (canBeNull expected) $
        tcError $ CannotBeNullError expected
      return expected
  | isBottomType actual = return expected
  | isBottomType expected =
      tcError BottomTypeInferenceError
  | otherwise = do
      actual `assertSubtypeOf` expected
      return actual

--  E |- arg1 : t
--  matchTypes(B, t1, t) = B1
--  E, B1 |- arg2 : t2 .. argn : tn -| B'
-- ------------------------------------------------
--  E, B |- arg1 : t1 arg2 : t2 .. argn : tn -| B'
-- | @matchArguments args types@ checks if @argI@ matches
-- @typeI@ and throws a type checking error if they don't.
-- Returns the type checked arguments and a list of inferred
-- bindings, i.e. type variables to types.
matchArguments :: [Expr] -> [Type] -> TypecheckM ([Expr], [(Type, Type)])
matchArguments [] [] = do bindings <- asks bindings
                          return ([], bindings)
matchArguments (arg:args) (typ:types) = do
  eArg <- do
    eArg <- typecheck arg
    if isNullType (AST.getType eArg) then
      coerceNull eArg typ
    else
      return eArg
  let actualTyp = AST.getType eArg
  bindings <- matchTypes typ actualTyp
  (eArgs, bindings') <- local (bindTypes bindings) $
                              matchArguments args types
  needCast <- fmap (&& typ /= actualTyp) $
                   actualTyp `subtypeOf` typ
  let
    casted = TypedExpr{emeta=getMeta eArg, body=eArg, ty=typ}
    eArg' = if needCast then casted else eArg
  return (eArg':eArgs, bindings')
  (eArgs, bindings') <- local (bindTypes bindings) $
                              matchArguments args types
  return (eArg:eArgs, bindings')

--  Note that the bindings B is implicit in the reader monad
--
--  matchTypes(B, t1, t2) = B'
-- ----------------------------------
--  matchTypes(B, _ t1, _ t2) = B'
--
--  matchTypes(B, t11, t21) = B1
--  matchTypes(B1, t12, t22) = B2 .. matchTypes(B(n-1), t1n, t2n) = Bn
--  matchTypes(Bn, t1, t2) = B'
-- ---------------------------------------------------------------------
--  matchTypes(B, (t11, .., t1n) -> t1, (t21, .., t2n) -> t2) = B'
--
--  B(x) = t'
--  t <: t'
-- --------------------------
--  matchTypes(B, x, t) = B
--
--  x notin dom(B)
-- -------------------------------
--  matchTypes(B, x, t) = B[x->t]
--
--  !compoundType(t)
--  !compoundType(t')
--  t <: t'
-- --------------------------
--  matchTypes(B, t, t') = B
-- | @matchTypes ty1 ty2@ checks if @ty1@ and @ty2@ match and
-- throws a type checking error if they don't. If @ty1@ is a type
-- variable, it tries to bind that variable to @ty2@ and throws an
-- error if it is already bound to a different type. Returns the
-- list of inferred bindings, i.e. type variables to types,
-- together with the preexisting bindings.
matchTypes :: Type -> Type -> TypecheckM [(Type, Type)]
matchTypes expected ty
    | isFutureType expected && isFutureType ty ||
      isParType expected    && isParType ty    ||
      isStreamType expected && isStreamType ty ||
      isArrayType expected  && isArrayType ty  ||
      isMaybeType expected  && isMaybeType ty =
        matchTypes (getResultType expected) (getResultType ty)
        `catchError` (\case
                       TCError (TypeMismatchError _ _) _ ->
                           tcError $ TypeMismatchError ty expected
                       TCError err _ -> tcError err
                     )
    | isTupleType expected && isTupleType ty = do
        let expArgTypes = getArgTypes expected
            argTypes = getArgTypes ty
        matchArgs expArgTypes argTypes
    | isArrowType expected && isArrowType ty = do
        let expArgTypes = getArgTypes expected
            argTypes    = getArgTypes ty
            expRes      = getResultType expected
            resTy       = getResultType ty
        unless (ty `modeSubtypeOf` expected) $
               tcError $ TypeMismatchError ty expected
        argBindings <- matchArgs expArgTypes argTypes
        bindings <- local (bindTypes argBindings) $ matchTypes expRes resTy
        let expected' = replaceTypeVars bindings expected
        assertMatch expected' ty
        return bindings
    | isTraitType expected   && isTraitType ty ||
      isClassType expected   && isClassType ty ||
      isTypeSynonym expected && isTypeSynonym ty = do
        bindings <- matchArgs (getTypeParameters expected) (getTypeParameters ty)
          `catchError` (\case
                         TCError (TypeMismatchError _ _) _ ->
                             tcError $ TypeMismatchError ty expected
                         TCError err _ -> tcError err
                       )
        let expected' = replaceTypeVars bindings expected
        assertMatch expected' ty
        return bindings
    | isTraitType expected && isClassType ty = do
      cap <- findCapability ty
      let traits = typesFromCapability cap
      case find (==expected) traits of
        Just trait -> do
          bindings <- matchTypes expected trait
          let expected' = replaceTypeVars bindings expected
          assertMatch expected' ty
          return bindings
        Nothing -> assertMatch expected ty
    | isTraitType expected && isCapabilityType ty = do
      let traits = typesFromCapability ty
      case find (==expected) traits of
        Just trait -> do
          bindings <- matchTypes expected trait
          let expected' = replaceTypeVars bindings expected
          assertMatch expected' ty
          return bindings
        Nothing -> assertMatch expected ty
    | isTypeVar expected = do
      params <- asks typeParameters
      if expected `elem` params then
          assertMatch expected ty
      else do
        result <- asks $ typeVarLookup expected
        case result of
          Just boundType -> do
            unlessM (unbox ty `subtypeOf` boundType) $
              tcError $ TypeVariableAmbiguityError expected ty boundType
            asks bindings
          Nothing -> do
            bindings <- asks bindings
            return $ (expected, ty) : bindings
    | isTypeVar ty && (not . isTypeVar) expected = do
        isConcrete <- asks $ isLocalTypeParameter ty
        if isConcrete then
          assertMatch expected ty
        else
          return [(ty, expected)]
    | otherwise = assertMatch expected ty
    where
      matchArgs tys1 tys2 = do
        unless (length tys1 == length tys2) $
               tcError $ TypeMismatchError ty expected
        matchArgs' tys1 tys2

      matchArgs' [] [] = asks bindings
      matchArgs' (ty1:types1) (ty2:types2) = do
        bindings <- matchTypes ty1 ty2
        local (bindTypes bindings) $ matchArgs' types1 types2

      assertMatch expected ty = do
        ty `assertSubtypeOf` expected
        asks bindings

inferenceCall call typeParams argTypes resultType
  | isMethodCallOrMessageSend call || isFunctionCall call = do
      let uniquify = uniquifyTypeVars typeParams
      uniqueArgTypes <- mapM uniquify argTypes
      (eArgs, bindings) <- matchArguments (args call) uniqueArgTypes
      let resolve t = replaceTypeVars bindings <$> uniquify t
      resultType' <- resolve resultType
      typeArgs <- mapM resolve typeParams
      typeParams' <- mapM uniquify typeParams
      let unresolved = filter (isNothing . (`lookup` bindings)) typeParams'
      unless (null unresolved) $
         tcError $ TypeArgumentInferenceError call (head unresolved)

      assertSafeTypeArguments typeParams' typeArgs
      checkTypeArgumentBounds typeParams' typeArgs
      return (eArgs, resultType', typeArgs)
  | otherwise = error $ "Typechecker.hs: expression '" ++ show call ++ "' " ++
                        "is not a method or function call"
  where
  functionCallName = qname call
  methodCallName = name call

typecheckCall call formalTypeParameters argTypes resultType
  | isMethodCallOrMessageSend call || isFunctionCall call = do
      let uniquify ty = uniquifyTypeVars formalTypeParameters ty
      uniqueTypeParameters <- mapM uniquify formalTypeParameters
      uniqueArgTypes <- mapM uniquify argTypes
      uniqueResultType <- uniquify resultType

      typeArgs' <- mapM checkType (typeArguments call)
      let bindings = zip uniqueTypeParameters typeArgs'
  -- NOTE: it seems redundant to use the bindTypes when we are substituting
  --  the argTypes by the bindings. this is necessary because the
  --  method type params get mixed with the function type params in the
  --  doPrecheck functionheader and there is no simple way to disambiguate
  --  these two.
      let expectedTypes = map (replaceTypeVars bindings) uniqueArgTypes
      (eArgs, _) <-
        local (bindTypes bindings) $
              matchArguments (args call) expectedTypes

      let returnType = replaceTypeVars bindings uniqueResultType
      assertSafeTypeArguments uniqueTypeParameters typeArgs'
      checkTypeArgumentBounds uniqueTypeParameters typeArgs'
      return (eArgs, returnType, typeArgs')
 | otherwise = error $ "Typechecker.hs: expression '" ++ show call ++ "' " ++
                        "is not a method or function call"

-- Helper function for return type of method calls
retType mcall targetType header t = do
  isPassive <- isPassiveType targetType
  if isPassive || isThisAccess (target mcall)
  then return t
  else if isStreamMethodHeader header
  then return $ streamType t
  else return $ futureType t

typecheckPrivateModifier target name = do
  let targetType = AST.getType target
  header <- fst <$> findMethodWithCalledType targetType name
  unless (isThisAccess target) $
    when (isPrivateMethodHeader header) $
       tcError $ PrivateAccessModifierTargetError name

typecheckParametricFun argTypes eSeqFunc
  | isVarAccess eSeqFunc || isClosure eSeqFunc = do
      let seqType = AST.getType eSeqFunc
          resultType = getResultType seqType
          expectedFunType = arrowType argTypes resultType
      seqType `assertSubtypeOf` expectedFunType
      return expectedFunType
  | isFunctionAsValue eSeqFunc = do
      let funname = (qname eSeqFunc)
          actualTypeParams = typeArgs eSeqFunc
          seqType = AST.getType eSeqFunc
          funResultType = getResultType seqType
      result <- findVar funname
      ty <- case result of
          Just (_, ty) -> return ty
          Nothing -> tcError $ UnboundFunctionError funname
      let formalTypeParams = getTypeParameters ty
      unless (length formalTypeParams == length actualTypeParams) $
         tcError $ WrongNumberOfFunctionTypeArgumentsError funname
                   (length formalTypeParams) (length actualTypeParams)
      let bindings = zip formalTypeParams actualTypeParams
          expectedFunType = replaceTypeVars bindings $
                                   arrowType argTypes funResultType
      expectedFunType' <- checkType expectedFunType
      seqType `assertSubtypeOf` expectedFunType'
      return expectedFunType
  | otherwise = error $ "Function that is callable but distinct from" ++
                        " 'VarAccess' or 'FunctionAsValue' AST node used."
  where
    isFunctionAsValue FunctionAsValue{} = True
    isFunctionAsValue _ = False
