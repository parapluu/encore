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
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Arrow((&&&), second)

-- Module dependencies
import Identifiers
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST (getType)
import qualified AST.Util as Util (freeVariables, filter, markStatsInBody, isStatement)
import AST.PrettyPrinter
import Types
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util
import Text.Printf (printf)

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
      rhs' <- local addTypeParams $ resolveType rhs
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
  doTypecheck t@Trait{tname, tmethods} = do
    emethods <- mapM typecheckMethod tmethods
    return t{tmethods = emethods}
    where
      addTypeParams = addTypeParameters $ getTypeParameters tname
      addThis = extendEnvironmentImmutable [(thisName, tname)]
      typecheckMethod = local (addTypeParams . addThis) . typecheck

matchArgumentLength :: Type -> FunctionHeader -> Arguments -> TypecheckM ()
matchArgumentLength targetType header args =
  unless (actual == expected) $
         tcError $ WrongNumberOfMethodArgumentsError
                   (hname header) targetType expected actual
  where
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
      unless (null providedMethods) $
        unless (null mutatedValFields) $
          tcError $ ProvidingTraitFootprintError
                      (tname provider) (tname requirer)
                      (methodName $ head providedMethods) mutatedValFields

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
          expectedMethodType = arrowType expectedParamTypes expectedType
          actualParamTypes = map ptype $ methodParams method
          actualType = methodType method
          actualMethodType = arrowType actualParamTypes actualType
          requirer = tname abstractDecl
      unlessM (actualMethodType `subtypeOf` expectedMethodType) $
             pushError method $
               OverriddenMethodTypeError
                 (methodName method) expectedMethodType requirer
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
instance Checkable ClassDecl where
  -- TODO: Update this rule!
  --  E, this : cname |- method1 .. E, this : cname |- methodm
  -- -----------------------------------------------------------
  --  E |- class cname fields methods
  doTypecheck c@(Class {cname, cfields, cmethods, ccomposition}) = do
    unless (isPassiveClassType cname || isNothing ccomposition) $
           tcError TraitsInActiveClassError

    when (isPassiveClassType cname) $
         unless (null $ filter isForwardMethod cmethods) $
                tcError $ ForwardInPassiveContext cname

    let traits = typesFromTraitComposition ccomposition
        extendedTraits = extendedTraitsFromComposition ccomposition

    mapM_ (meetRequiredFields cfields) traits
    meetRequiredMethods cmethods traits
    ensureNoMethodConflict cmethods traits

    mapM_ (ensureMatchingTraitFootprint cname extendedTraits) extendedTraits
    noOverlapFields cname ccomposition

    checkOverriding cname typeParameters cmethods extendedTraits
    -- TODO: Add namespace for trait methods

    emethods <- mapM typecheckMethod cmethods

    return c{cmethods = emethods}
    where
      typeParameters = getTypeParameters cname
      addTypeVars = addTypeParameters typeParameters
      addThis = extendEnvironmentImmutable [(thisName, cname)]
      typecheckMethod m = local (addTypeVars . addThis) $ typecheck m
      isForwardMethod m@Method{mbody} = not . null $ Util.filter isForward mbody

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
        checkImpureExpr ext@ExtractorPattern{name, ty} = do
          header <- findMethod ty name
          unless (isMatchMethodHeader header) $
                 pushError ext $ ImpureMatchMethodError ext
        checkImpureExpr Assign{lhs = VarAccess{}} = return ()
        checkImpureExpr e = pushError e $ ImpureMatchMethodError e

instance Checkable ParamDecl where
    doTypecheck p@Param{ptype} = do
      ptype' <- resolveType ptype
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
      unlessM (asks $ checkValidUseOfContinue) $
        tcError ContinueOutsideOfLoopError
      return $ setType unitType continue

    --    |- t
    --  E |- body : t
    -- ----------------------
    --  E |- (body : t) : t
    doTypecheck te@(TypedExpr {body, ty}) =
        do ty' <- resolveType ty
           eBody <- hasType body ty'
           return $ setType ty' $ te{body = eBody, ty = ty'}

    doTypecheck l@(Liftf {val}) = do
      e <- typecheck val
      let typ = AST.getType e
      unless (isFutureType typ) $
             pushError e $ ExpectingOtherTypeError "a future" typ
      return $ setType (parType $ getResultType typ) l {val = e}

    doTypecheck l@(Liftv {val}) = do
      e <- typecheck val
      let typ = AST.getType e
      return $ setType (parType typ) l {val = e}

    doTypecheck p@(PartyJoin {val}) = do
      e <- typecheck val
      let typ = AST.getType e
      unless (isParType typ && isParType (getResultType typ)) $
             pushError e $ ExpectingOtherTypeError "a nested Par" typ
      return $ setType (getResultType typ) p {val = e}

    doTypecheck p@(PartyEach {val}) = do
      e <- typecheck val
      let typ = AST.getType e
      unless (isArrayType typ) $
             pushError e $ ExpectingOtherTypeError "an array" typ
      return $ setType ((parType.getResultType) typ) p {val = e}

    doTypecheck p@(PartyExtract {val}) = do
      e <- typecheck val
      let typ = AST.getType e
      unless (isParType typ) $
             pushError e $ ExpectingOtherTypeError "a Par" typ
      return $ setType ((arrayType.getResultType) typ) p {val = e}

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
                               ,decls = [(name, fieldAccess)]
                               ,body
                               }
            else
                handleMethodCall eTarget targetType

          handleMethodCall eTarget targetType = do
            handleErrors targetType mcall
            typecheckPrivateModifier eTarget (name mcall)

            (header, calledType) <- findMethodWithCalledType targetType (name mcall)

            matchArgumentLength targetType header (args mcall)
            let eTarget' = setType calledType eTarget
                typeParams = htypeparams header
                argTypes = map ptype $ hparams header
                resultType = htype header

            (eArgs, resultType', typeArgs) <-
               if null (typeArguments mcall) then
                  inferenceCall mcall typeParams argTypes resultType
               else
                  typecheckCall mcall typeParams argTypes resultType
            let returnType = retType mcall calledType header resultType'
                isThisCall = isThisAccess (target mcall)
                isStream = isStreamMethodHeader header
                isAsync = isMessageSend mcall

            let returnType' = if isAsync && isThisCall
                              then futureType returnType
                              else returnType

            when (isStream && isThisCall) $ tcError SyncStreamCall
            return $ setType returnType' mcall {target = eTarget'
                                              ,args = eArgs
                                              ,typeArguments = typeArgs}

          errorInitMethod targetType name = do
            when (name == constructorName) $ tcError ConstructorCallError
            when (isMainMethod targetType name) $ tcError MainMethodCallError

          handleErrors targetType m
            | isMessageSend m = do
              errorInitMethod targetType (name m)
              unless (isActiveClassType targetType ||
                      isSharedClassType targetType) $
                tcError $ NonSendableTargetError targetType
            | isMethodCall m = do
              when (isRefType targetType) $
                unless (isPassiveType targetType || isThisAccess (target mcall)) $
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
      else do
        let typeParams  = getTypeParameters ty
            argTypes    = getArgTypes ty
            resultType  = getResultType ty

        unless (isArrowType ty) $
          tcError $ NonFunctionTypeError ty
        unless (length args == length argTypes) $
          tcError $ WrongNumberOfFunctionArgumentsError
                      qname (length argTypes) (length args)

        (eArgs, returnType, typeArgs) <-
          if null typeArguments
          then inferenceCall fcall typeParams argTypes resultType
          else do
            unless (length typeArguments == length typeParams) $
                   tcError $ WrongNumberOfFunctionTypeArgumentsError qname
                             (length typeParams) (length typeArguments)
            typecheckCall fcall typeParams argTypes resultType
        return $ setType returnType fcall {args = eArgs,
                                           qname = qname',
                                           typeArguments = typeArgs}

   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  t != nullType
    -- ------------------------------------------------------
    --  E |- \ (x1 : t1, .., xn : tn) -> body : (t1 .. tn) -> t
    doTypecheck closure@(Closure {eparams, mty, body}) = do
      let returns = Util.filter isReturn body
      when (not (null returns)) $
        pushError (head returns) $ ClosureReturnError
      eEparams <- mapM typecheck eparams
      mty' <- mapM resolveType mty
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
          capturedVariables = map (qnlocal . fst) $
                              Util.freeVariables (map qLocal paramNames) eBody

      shadowingParams <- filterM doesShadow paramNames
      unless (null shadowingParams) $
         tcError $ TypeVariableAndVariableCommonNameError shadowingParams

      local (addParams eEparams . makeImmutable capturedVariables) $
            typecheck eBody -- Check for mutation of captured variables
      let returnType = AST.getType eBody
          ty = arrowType (map ptype eEparams) returnType
      return $ setType ty closure {body = eBody, mty = mty', eparams = eEparams}
      where
        doesShadow paramName = do
          typeParams <- asks typeParameters
          return $ paramName `elem` map (Name . getId) typeParams

    --  E |- e1 : t1; E, x1 : t1 |- e2 : t2; ..; E, x1 : t1, .., x(n-1) : t(n-1) |- en : tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  x1 != nullType .. xn != nullType
    -- --------------------------------------------------------------------------------------
    --  E |- let x1 = e1 .. xn = en in body : t
    doTypecheck let_@(Let {mutability, decls, body}) =
        do eDecls <- typecheckDecls decls
           let declNames = map fst eDecls
               declTypes = map (AST.getType . snd) eDecls
               addNames = (if mutability == Val
                           then extendEnvironmentImmutable
                           else extendEnvironment) $ zip declNames declTypes
           when (any isBottomType (concatMap typeComponents declTypes)) $
                tcError BottomTypeInferenceError
           eBody <- local addNames $ typecheck body
           return $ setType (AST.getType eBody) let_ {decls = eDecls
                                                     ,body = eBody}
        where
          typecheckDecls [] = return []
          typecheckDecls ((name, expr):decls') =
              do eExpr <- typecheckNotNull expr
                 let addName =
                         (if mutability == Val
                          then extendEnvironmentImmutable
                          else extendEnvironment) [(name, AST.getType eExpr)]
                 eDecls <- local addName $ typecheckDecls decls'
                 return $ (name, eExpr):eDecls

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
        let argType = AST.getType eArg
        when (isActiveClassType argType) $
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

        doGetPatternVars pt fcall@(FunctionCall {qname, args = []}) = do
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

        doGetPatternVars pt fcall@(FunctionCall {qname, args = [arg]}) = do
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
                                             ,args = [arg]}) argty = do
          let name = qnlocal qname
          header <- findMethod argty name
          let hType = htype header
              extractedType = getResultType hType
          eArg <- checkPattern arg extractedType
          matchArgumentLength argty header []
          return $ setType argty ExtractorPattern {emeta
                                                  ,ty = argty
                                                  ,name
                                                  ,arg = eArg
                                                  }

        doCheckPattern pattern@(FunctionCall {args}) argty = do
          let tupMeta = getMeta $ head args
              tupArg = Tuple {emeta = tupMeta, args = args}
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
          ty' <- resolveType ty
          argty `assertSubtypeOf` ty'
          return $ setType ty' eBody

        doCheckPattern pattern argty
            | isPattern pattern = hasType pattern argty
            | otherwise = tcError $ InvalidPatternError pattern

        checkClause pt clause@MatchClause{mcpattern, mchandler, mcguard} = do
          vars <- getPatternVars pt mcpattern
          let withLocalEnv = local (extendEnvironmentImmutable vars)
          ePattern <- withLocalEnv $ checkPattern mcpattern pt
          eHandler <- withLocalEnv $ typecheck mchandler
          eGuard <- withLocalEnv $ hasType mcguard boolType
          return $ clause {mcpattern = ePattern
                          ,mchandler = eHandler
                          ,mcguard = eGuard}

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
           unless (isMessageSend forwardExpr || isFutureChain forwardExpr) $
                  pushError eExpr $ ForwardArgumentError
           unless (isFutureType ty) $
                  pushError eExpr $ ExpectingOtherTypeError
                                      "a future" ty
           result <- asks currentMethod
           when (isNothing result) $
                pushError eExpr $ ForwardInFunction
           let returnType = methodType (fromJust result)
           typeCmp <- (getResultType ty) `subtypeOf` returnType
           unless typeCmp $
                pushError eExpr $ ForwardTypeError returnType ty
           return $ setType (getResultType ty) forward {forwardExpr = eExpr}

    --  E |- val : t
    --  isStreaming(currentMethod)
    -- -----------------------------
    --  E |- yield val : unit
    doTypecheck yield@(Yield {val}) =
        do eVal <- typecheck val
           result <- asks currentMethod
           when (isNothing result) $
                tcError $ NonStreamingContextError yield
           let mtd = fromJust result
               mType = methodType mtd
               eType = AST.getType eVal
           unless (isStreamMethod mtd) $
                  tcError $ NonStreamingContextError yield
           eType `assertSubtypeOf` mType
           return $ setType unitType yield {val = eVal}

    --  E |- expr : t
    --  E |- currentMethod : _ -> t
    -- -----------------------------
    --  E |- return expr : t
    doTypecheck ret@(Return {val}) =
        do eVal <- typecheck val
           cm <- asks currentMethod
           cf <- asks currentFunction
           ty <- case (cm, cf) of
                   (Just method, _)   -> return (methodType method)
                   (_, Just (n, t))   -> return t
                   (Nothing, Nothing) -> error $
                      "Typechecker.hs: Could not get method or function surrounding a return"

           let eType = AST.getType eVal
           unlessM (eType `subtypeOf` ty) $
             pushError ret $ ExpectingOtherTypeError
                (show ty ++ " (type of the enclosing method or function)") eType
           return $ setType eType ret {val = eVal}

    --  isStreaming(currentMethod)
    -- ----------------------------
    --  E |- eos : unit
    doTypecheck eos@(Eos {}) =
        do result <- asks currentMethod
           when (isNothing result) $
                tcError $ NonStreamingContextError eos
           let mtd = fromJust result
           unless (isStreamMethod mtd) $
                  tcError $ NonStreamingContextError eos
           return $ setType unitType eos

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
      return $ setType (compartments!!(compartment)) ta {target = eTarget}

    --  E |- target : t'
    --  fieldLookup(t', name) = t
    -- ---------------------------
    --  E |- target.name : t
    doTypecheck fAcc@(FieldAccess {target, name}) = do
      eTarget <- typecheck target
      let targetType = AST.getType eTarget
      unless (isThisAccess target || isPassiveClassType targetType) $
        tcError $ CannotReadFieldError eTarget
      ty <- ftype <$> findField targetType name
      return $ setType ty fAcc {target = eTarget}

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
           mtd <- asks currentMethod
           unless (isNothing mtd || isConstructor (fromJust mtd)) $
                  assertNotValField eLhs
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
             typeArgs' <- mapM resolveType typeArgs
             let typeParams = getTypeParameters ty
             unless (length typeArgs' == length typeParams) $
                    tcError $ WrongNumberOfFunctionTypeArgumentsError qname
                              (length typeParams) (length typeArgs')

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
        Just (qname', ty) ->
          if isArrowType ty
          then do
            let typeParams = getTypeParameters ty
            unless (null typeParams) $
               tcError $ WrongNumberOfFunctionTypeArgumentsError qname
                         (length typeParams) 0
            return $ setType ty var{qname = qname'}
          else return $ setType ty var{qname = qname'}
        Nothing -> tcError $ UnboundVariableError qname

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

          header <- findConstructor formal args
          let typeParams = getTypeParameters formal
              argTypes = map ptype (hparams header)

          (eArgs, resolvedTy, _) <-
              inferenceCall fakeInitCall typeParams argTypes formal
          return $ setType resolvedTy new{ty = resolvedTy, args = eArgs}

      | otherwise = do
          ty' <- resolveType ty

          header <- findConstructor ty' args
          let expectedTypes = map ptype (hparams header)

          (eArgs, _) <- matchArguments args expectedTypes
          return $ setType ty' new{ty = ty', args = eArgs}

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

    --  E |- rng : Range
    --  E, x : int |- e : ty
    -- --------------------------
    --  E |- for x <- rng e : ty

    --  E |- arr : [ty]
    --  E, x : int |- e : ty
    -- --------------------------
    --  E |- for x <- arr e : ty
    doTypecheck for@(For {name, step, src, body}) =
        do stepTyped <- doTypecheck step
           srcTyped  <- doTypecheck src
           let srcType = AST.getType srcTyped

           unless (isArrayType srcType || isRangeType srcType) $
             pushError src $ NonIterableError srcType

           let elementType = if isRangeType srcType
                             then intType
                             else getResultType srcType
           bodyTyped <- typecheckBody elementType body
           return $ setType unitType for{step = stepTyped
                                        ,src  = srcTyped
                                        ,body = bodyTyped}
        where
          addIteratorVariable ty = extendEnvironmentImmutable [(name, ty)]
          typecheckBody ty = local (addIteratorVariable ty) . typecheck

   ---  |- ty
    --  E |- size : int
    -- ----------------------------
    --  E |- new [ty](size) : [ty]
    doTypecheck new@(ArrayNew {ty, size}) =
        do ty' <- resolveType ty
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
      sty <- resolveType stringObjectType
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
        do ty' <- resolveType ty
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

          unlessM (lType `subtypeOf` rType) $
            unlessM (rType `subtypeOf` lType) $
              tcError $ IdComparisonTypeMismatchError lType rType

          unless (isTypeVar lType) $ checkIdComparisonSupport lType
          unless (isTypeVar rType) $ checkIdComparisonSupport rType

          when (isStringObjectType lType) $
            unless (isNullLiteral eRoper || isNullLiteral eLoper) $
                         tcWarning StringIdentityWarning
          when (isTypeVar lType) $
            tcWarning PolymorphicIdentityWarning

          return $ setType boolType bin {loper = eLoper, roper = eRoper}
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
            | isTupleType ty = do
                x <- mapM checkIdComparisonSupport (getArgTypes ty)
                return ()
            | otherwise = do
                id <- resolveType (refType "Id")
                includesId <- ty `subtypeOf` id
                unless (includesId || isPrimitive ty) $
                  tcError $ IdComparisonNotSupportedError ty

    doTypecheck e = error $ "Cannot typecheck expression " ++ show (ppExpr e)

--  classLookup(ty) = _
-- ---------------------
--  null : ty
coerceNull null ty
    | isRefType ty = return $ setType ty null
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
  | isBottomType actual = do
      when (any isBottomType $ typeComponents expected) $
        tcError BottomTypeInferenceError
      return expected
  | isBottomType expected =
      tcError BottomTypeInferenceError
  | otherwise = do
      actual `assertSubtypeOf` expected
      return actual
  where
    canBeNull ty =
      isRefType ty || isFutureType ty || isArrayType ty ||
      isStreamType ty || isArrowType ty || isParType ty

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
    casted = TypedExpr{emeta=(getMeta eArg),body=eArg,ty=typ}
    eArg' = if needCast then casted else eArg
  return (eArg':eArgs, bindings')

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
        argBindings <- matchArgs expArgTypes argTypes
        local (bindTypes argBindings) $ matchTypes expRes resTy
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
            unlessM (ty `subtypeOf` boundType) $
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

      typeArgs' <- mapM resolveType (typeArguments call)
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
      return (eArgs, returnType, typeArgs')
 | otherwise = error $ "Typechecker.hs: expression '" ++ show call ++ "' " ++
                        "is not a method or function call"

-- Helper function for return type of method calls
retType mcall targetType header t
  | isSyncCall targetType = t
  | isStreamMethodHeader header = streamType t
  | otherwise = futureType t
  where
    isSyncCall targetType =
      isThisAccess (target mcall) ||
      isPassiveClassType targetType ||
      isTraitType targetType -- TODO now all trait methods calls are sync

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
      let formalTypeParams = getTypeParams ty
      unless (length formalTypeParams == length actualTypeParams) $
         tcError $ WrongNumberOfFunctionTypeArgumentsError funname
                   (length formalTypeParams) (length actualTypeParams)
      let bindings = zip formalTypeParams actualTypeParams
          expectedFunType = replaceTypeVars bindings $
                                   arrowType argTypes funResultType
      expectedFunType' <- resolveType expectedFunType
      seqType `assertSubtypeOf` expectedFunType'
      return expectedFunType
  | otherwise = error $ "Function that is callable but distinct from" ++
                        " 'VarAccess' or 'FunctionAsValue' AST node used."
  where
    isVarAccess VarAccess{} = True
    isVarAccess _ = False

    isFunctionAsValue FunctionAsValue{} = True
    isFunctionAsValue _ = False
