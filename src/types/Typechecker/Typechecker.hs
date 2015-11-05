{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information of every expression node. It throws an exception
with a meaningful error message if it fails.

-}

module Typechecker.Typechecker(typecheckEncoreProgram) where

import Data.List
import Data.Maybe
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Except

-- Module dependencies
import Identifiers
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST (getType)
import AST.PrettyPrinter
import Types
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util
import Text.Printf (printf)


-- | The top-level type checking function
typecheckEncoreProgram :: Program -> Either TCError Program
typecheckEncoreProgram p =
    do env <- buildEnvironment p
       runReader (runExceptT (doTypecheck p)) env

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
  doTypecheck p@Program{imports, functions, traits, classes} = do
    etraits  <- mapM typecheck traits
    eclasses <- mapM typecheck classes
    eimps    <- mapM typecheck imports
    efuns    <- mapM typecheck functions
    return p{imports = eimps
            ,functions = efuns
            ,traits = etraits
            ,classes = eclasses
            }

instance Checkable ImportDecl where
     -- TODO write down type rule
     doTypecheck (PulledImport meta name src program) =
       do eprogram <- doTypecheck program
          return $ PulledImport meta name src eprogram
     doTypecheck (Import _ _) =
         error "BUG: Import AST Nodes should not exist during typechecking"

typecheckNotNull :: Expr -> TypecheckM Expr
typecheckNotNull expr = do
  eExpr <- typecheck expr
  let ty = AST.getType eExpr
  if isNullType ty
  then local (pushBT expr) $ coerceNull eExpr ty
  else return eExpr

instance Checkable Function where
    --  E, x1 : t1, .., xn : tn |- funbody : funtype
    -- ----------------------------------------------------------
    --  E |- def funname(x1 : t1, .., xn : tn) : funtype funbody
    doTypecheck f@(Function {funheader, funbody}) = do
      let funtype = functionType f
          funparams = functionParams f
      eBody   <- local (addParams funparams) $
                     if isVoidType funtype
                     then typecheckNotNull funbody
                     else hasType funbody funtype
      return $ f{funbody = eBody}

instance Checkable TraitDecl where
  doTypecheck t@Trait{tname, tmethods} = do
    emethods <- mapM typecheckMethod tmethods
    return t{tmethods = emethods}
    where
      addTypeParams = addTypeParameters $ getTypeParameters tname
      addThis = extendEnvironment [(thisName, tname)]
      typecheckMethod = local (addTypeParams . addThis) . typecheck

matchArgumentLength :: FunctionHeader -> Arguments -> TypecheckM ()
matchArgumentLength header args =
  unless (actual == expected) $
         tcError $ concat [toStr (hname header), " expects ", show expected,
                           " arguments. Got ", show actual]
  where
    actual = length args
    expected = length sigTypes
    sigTypes = map ptype (hparams header)
    toStr (Name "_init") = "Constructor"
    toStr n = concat ["Method '", show n, "'"]

meetRequiredFields :: [FieldDecl] -> Type -> TypecheckM ()
meetRequiredFields cFields trait = do
  tdecl <- liftM fromJust . asks . traitLookup $ trait
  mapM_ matchField (requiredFields tdecl)
    where
    matchField tField = do
      bindings <- formalBindings trait
      let expected = replaceTypeVars bindings (ftype tField)
          expField = tField{ftype = expected}
          result = find (==expField) cFields
          cField = fromJust result
          cFieldType = ftype cField
      if isNothing result then
          tcError $
              "Cannot find field '" ++ show expField ++
              "' required by included " ++ classOrTraitName trait
      else if isValField expField then
          unlessM (cFieldType `subtypeOf` expected) $
              tcError $
                "Field '" ++ show cField ++ "' must have a subtype of '" ++
                show expected ++ "' to meet the requirements of " ++
                "included " ++ classOrTraitName trait
      else do
        isSub <- cFieldType `subtypeOf` expected
        unless (cFieldType == expected) $
            tcError $
              "Field '" ++ show cField ++ "' must exactly match type '" ++
              show expected ++ "' to meet the requirements of " ++
              "included " ++ classOrTraitName trait ++
              if isSub
              then ". Consider turning '" ++ show (fname expField) ++
                   "' into a val-field in " ++ classOrTraitName trait
              else ""

noOverlapFields :: Type -> TypecheckM ()
noOverlapFields capability =
  let
    parTraits = conjunctiveTypesFromCapability capability
  in
    mapM_ perLevel parTraits
  where
    perLevel :: [[Type]] -> TypecheckM ()
    perLevel level = mapM_ perPair $ pair level

    perPair :: ([Type], [Type]) -> TypecheckM ()
    perPair pair = do
      leftPairs <- mapM pairTypeFields $ fst pair
      rightPairs <- mapM pairTypeFields $ snd pair
      mapM_ conjunctiveVarErr $ commonVarFields leftPairs rightPairs

    findTypeHasField :: [(Type, [FieldDecl])] -> FieldDecl -> Type
    findTypeHasField pairs field =
      head $ [fst pair | pair <- pairs, field `elem` snd pair]

    commonVarFields :: [(Type, [FieldDecl])] -> [(Type, [FieldDecl])] -> [(Type, Type, FieldDecl)]
    commonVarFields leftPairs rightPairs =
      let
        leftFields = concatMap snd leftPairs
        rightFields = concatMap snd rightPairs
        common = intersect leftFields rightFields
        leftCommon = [f | f <- leftFields, f `elem` common, notVal f]
        rightCommon = [f | f <- rightFields, f `elem` common, notVal f]
        firstErrField = if (not . null) leftCommon then head leftCommon else head rightCommon
        leftType = findTypeHasField leftPairs firstErrField
        rightType = findTypeHasField rightPairs firstErrField
      in
        if null leftCommon && null rightCommon then
          []
        else
          [(leftType, rightType, firstErrField)]

    conjunctiveVarErr :: (Type, Type, FieldDecl) -> TypecheckM ()
    conjunctiveVarErr (left, right, field) =
      tcError $ printf
        "Conjunctive traits '%s' and '%s' cannot share mutable field '%s'"
         (show left) (show right) (show field)

    notVal :: FieldDecl -> Bool
    notVal = not . isValField

    pairTypeFields :: Type -> TypecheckM (Type, [FieldDecl])
    pairTypeFields t = do
      trait <- liftM fromJust . asks . traitLookup $ t
      return (t, requiredFields trait)

    pair :: [[Type]] -> [([Type], [Type])]
    pair list = pair' $ tail list
      where
        pair' [] = []
        pair' shadow = zip list shadow ++ (pair' $ tail shadow)

ensureNoMethodConflict :: [MethodDecl] -> [TraitDecl] -> TypecheckM ()
ensureNoMethodConflict methods tdecls =
  let allMethods = methods ++ concatMap tmethods tdecls
      unique = nub allMethods
      diff = allMethods \\ unique
      dup = head diff
      overlappingTraits = filter ((dup `elem`) . tmethods) tdecls
  in
  unless (null diff) $
         if dup `elem` methods then
             tcError $ "Method '" ++ show (methodName dup) ++
                       "' is defined both in current class and " ++
                       classOrTraitName (tname $ head overlappingTraits)
         else
             tcError $ "Conflicting inclusion of method '" ++
                       show (methodName dup) ++ "' from " ++
                       classOrTraitName (tname (head overlappingTraits)) ++
                       " and " ++
                       classOrTraitName (tname (overlappingTraits !! 1))

meetRequiredMethods :: [MethodDecl] -> Type -> TypecheckM ()
meetRequiredMethods cMethods trait = do
  tdecl <- liftM fromJust . asks . traitLookup $ trait
  mapM_ matchMethod (requiredMethods tdecl)
  where
    matchMethod reqHeader = do
      bindings <- formalBindings trait
      let expHeader = replaceHeaderTypes bindings reqHeader
      unlessM (anyM (matchesHeader expHeader) cMethods) $
           tcError $
               "Cannot find method '" ++ show (ppFunctionHeader expHeader) ++
               "' required by included " ++ classOrTraitName trait
    matchesHeader header mdecl =
      let
        mName = methodName mdecl
        mType = methodType mdecl
        mParamTypes = map ptype (methodParams mdecl)
        hName = hname header
        hType = htype header
        hParamTypes = map ptype (hparams header)
      in
        liftM ((mName == hName && mParamTypes == hParamTypes) &&) $
              mType `subtypeOf` hType

instance Checkable ClassDecl where
  -- TODO: Update this rule!
  --  E, this : cname |- method1 .. E, this : cname |- methodm
  -- -----------------------------------------------------------
  --  E |- class cname fields methods
  doTypecheck c@(Class {cname, cfields, cmethods, ccapability}) = do
    let traits = typesFromCapability ccapability
    unless (isPassiveClassType cname || null traits) $
           tcError "Traits can only be used for passive classes"
    mapM_ (meetRequiredFields cfields) traits
    mapM_ (meetRequiredMethods cmethods) traits
    noOverlapFields ccapability
    -- TODO: Add namespace for trait methods
    tdecls <- mapM (liftM fromJust . asks . traitLookup) traits
    ensureNoMethodConflict cmethods tdecls

    emethods <- mapM typecheckMethod cmethods
    return c{cmethods = emethods}
    where
      typeParameters = getTypeParameters cname
      addTypeVars = addTypeParameters typeParameters
      addThis = extendEnvironment [(thisName, cname)]
      typecheckMethod m = local (addTypeVars . addThis) $ typecheck m

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
    doTypecheck m@(Method {mbody}) = do
        let mType   = methodType m
            mparams = methodParams m
        eBody <- local (addParams mparams) $
                       if isVoidType mType || isStreamMethod m
                       then typecheckNotNull mbody
                       else hasType mbody mType
        return $ m{mbody = eBody}

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
             resultType <- coerce ty exprType
             assertSubtypeOf resultType ty
             let result = propagateResultType resultType eExpr
             return $ setType resultType result

instance Checkable Expr where
    --
    -- ----------------
    --  E |- () : void
    doTypecheck skip@(Skip {}) = return $ setType voidType skip

    --
    -- ----------------
    --  E |- breathe : void
    doTypecheck breathe@(Breathe {}) = return $ setType voidType breathe

   ---  |- t
    --  E |- body : t
    -- ----------------------
    --  E |- (body : t) : t
    doTypecheck te@(TypedExpr {body, ty}) =
        do ty' <- resolveType ty
           eBody <- hasType body ty'
           return $ setType ty' $ te{body = eBody, ty = ty'}

    doTypecheck m@(MatchDecl {arg, matchbody}) =
      do eArg <- typecheck arg
         checkErrors eArg matchbody
         eMatchBody <- mapM (tuplecheckE (AST.getType eArg)) matchbody

         let resultType = (AST.getType . snd . head) eMatchBody
             patternMatchingTypes = map (AST.getType . fst) eMatchBody

         -- check pattern matching branches have same type as the parent type
         mapM (checkTypes (AST.getType eArg)) patternMatchingTypes

         -- check returning type of match conditions are of the same kind
         unless (all ((hasSameKind resultType) . AST.getType . snd) eMatchBody) $
           tcError $ "Match clause must return same type in all branches. " ++
                     tipSentence resultType

         return $ setType resultType m {arg = eArg, matchbody = eMatchBody}
      where
        tipSentence resultType
          | isMaybeType resultType = "Did you forget to cast a 'Nothing' expression?"
          | otherwise = ""

        checkErrors eArg matchbody = do
          let eArgType = AST.getType eArg
          unless (isMaybeType eArgType) $
            tcError $ "Match clause needs to match on an Option type, not on " ++ show eArgType
          unless (length matchbody > 0) $
            tcError "Match clause has no pattern to match against"
          when (any isBottomType (typeComponents eArgType)) $
            tcError $ "Matching argument of ambiguous type; " ++
                      "did you forget to cast a 'Nothing' expression?"

        getBindings parentType m@(MaybeValue _ (JustData exp)) = do
          unless (hasResultType parentType) $
            tcError "Type mismatch in 'match' expression"
          getBindings (getResultType parentType) exp

        getBindings parentType var@(VarAccess {name}) =
          return $ Just (name, parentType)

        getBindings parentType _ = return Nothing

        checkTypes parentType xType = do
          unless (parentType == xType) $
            tcError $ "Type mismatch in match expression, matching: " ++
                      show parentType ++ " with " ++ show xType

        tuplecheckE parentType (lhs, rhs) = do
          bindings <- getBindings parentType lhs
          let bindings' = case bindings of
                           Just x -> [x]
                           Nothing -> []
          tBody <- local (extendEnvironment bindings') (tuplecheck parentType (lhs, rhs))
          return tBody

        tuplecheck parentType (x@(MaybeValue _ (JustData (VarAccess {}))), y) = do
          x' <- typecheck x
          y' <- typecheck y
          unless (hasResultType parentType && parentType == AST.getType x') $
            tcError $ "Type mismatch in match expression, matching: " ++
                      show parentType ++ " with " ++ show (AST.getType x')
          return (x', y')

        tuplecheck parentType (x@(MaybeValue _ (JustData innerMaybe@(MaybeValue {}))), y) = do
          unless (hasResultType parentType) $
            tcError $ "Error matching '" ++ show parentType ++ "' type to '"
                      ++ show (AST.getType x) ++ "'"
          typedX <- typecheck x
          (typedInnerMaybe', y') <- tuplecheck (getResultType parentType) (innerMaybe, y)

          -- update typedX with typed values from the inner maybe
          let typedX' = setType (maybeType $ AST.getType typedInnerMaybe')
                                (typedX {mdt = JustData typedInnerMaybe'})
          return (typedX', y')

        tuplecheck parentType (x@(MaybeValue _ (NothingData {})), y) = do
          y' <- typecheck y
          x' <- typecheck x

          x'' <- hasType x' parentType
          return (x'', y')

        tuplecheck parentType (x@(MaybeValue {}), y) = do
          x' <- typecheck x
          y' <- typecheck y
          let typeX = getResultType $ AST.getType x'
              tip = "If you would like to match an object, pattern match on a variable, e.g. 'Just z'"
          unless (isPrimitive typeX) $
            tcError $ "Cannot pattern match on something different " ++
                      "from primitive and option types, trying to match '" ++
                      show parentType ++ "' and '" ++ show typeX ++ "'." ++ tip
          return (x', y')

        tuplecheck _ (x@VarAccess {}, y) = do
          x' <- typecheck x
          y' <- typecheck y
          return (x', y')

        tuplecheck parentType (x, y) = do
          x' <- typecheck x
          tcError $ "Cannot pattern match on expression of type '" ++
                    show parentType ++ "' with expression of type '" ++
                    show (AST.getType x') ++ "'"

    --  E |- e : t
    --  methodLookup(t, m) = (t1 .. tn, t')
    --  E |- arg1 : t1 .. E |- argn : tn
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    --  B'(t') = t''
    -- ----------------------------------------
    --  E |- this.m(arg1, .., argn) : t''
    --
    --  E |- e : t
    --  isPassiveRefType(t)
    --  methodLookup(t, m) = (t1 .. tn, t')
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    --  B'(t') = t''
    -- ----------------------------------------
    --  E |- e.m(arg1, .., argn) : t''
    --
    --  E |- e : t
    --  isActiveRefType(t)
    --  methodLookup(t, m) = (t1 .. tn, t')
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    --  B'(t') = t''
    -- ----------------------------------------
    --  E |- e.m(arg1, .., argn) : Fut t
    doTypecheck mcall@(MethodCall {target, name, args}) = do
      eTarget <- typecheck target
      let targetType = AST.getType eTarget
      unless (isRefType targetType) $
        tcError $ "Cannot call method on expression '" ++
                  show (ppExpr target) ++
                  "' of type '" ++ show targetType ++ "'"
      when (isMainMethod targetType name) $ tcError "Cannot call the main method"
      when (name == Name "init") $ tcError
        "Constructor method 'init' can only be called during object creation"
      header <- findMethod targetType name
      matchArgumentLength header args
      fBindings <- formalBindings targetType
      let paramTypes = map ptype (hparams header)
          expectedTypes = map (replaceTypeVars fBindings) paramTypes
          mType = htype header
      (eArgs, bindings) <- local (bindTypes fBindings) $
                                 matchArguments args expectedTypes
      let resultType = replaceTypeVars bindings mType
          returnType = retType targetType header resultType
      return $ setType returnType mcall {target = eTarget, args = eArgs}
      where
        retType targetType header t
         | isSyncCall targetType = t
         | isStreamMethodHeader header = streamType t
         | otherwise = futureType t
        isSyncCall targetType =
          isThisAccess target ||
          isPassiveClassType targetType ||
          isTraitType targetType -- TODO now all trait methods calls are sync

    --  E |- e : t'
    --  isActiveRefType(t')
    --  methodLookup(t', m) = (t1 .. tn, _)
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    -- --------------------------------------
    --  E |- e!m(arg1, .., argn) : ()
    doTypecheck msend@(MessageSend {target, name, args}) = do
      eTarget <- typecheck target
      let targetType = AST.getType eTarget
      unless (isActiveClassType targetType) $
           tcError $ "Cannot send message to expression '" ++
                     show (ppExpr target) ++
                     "' of type '" ++ show targetType ++ "'"
      header <- findMethod targetType name
      matchArgumentLength header args
      fBindings <- formalBindings targetType
      let paramTypes = map ptype (hparams header)
          expectedTypes = map (replaceTypeVars fBindings) paramTypes
      (eArgs, _) <- local (bindTypes fBindings) $
                    matchArguments args expectedTypes
      return $ setType voidType msend {target = eTarget, args = eArgs}

    doTypecheck maybeData@(MaybeValue {mdt}) = do
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



    --  E |- f : (t1 .. tn) -> t
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    --  B'(t) = t'
    -- --------------------------------------
    --  E |- f(arg1, .., argn) : t'
    doTypecheck fcall@(FunctionCall {name, args}) = do
      funType <- asks $ varLookup name
      ty <- case funType of
        Just ty -> return ty
        Nothing -> tcError $ "Unbound function variable '" ++ show name ++ "'"
      unless (isArrowType ty) $
        tcError $ "Cannot use value of type '" ++ show ty ++ "' as a function"
      let argTypes = getArgTypes ty
      unless (length args == length argTypes) $
             tcError $ "Function '" ++ show name ++ "' of type '" ++ show ty ++
                       "' expects " ++ show (length argTypes) ++ " arguments. Got " ++
                       show (length args)
      (eArgs, bindings) <- matchArguments args argTypes
      let resultType = replaceTypeVars bindings (getResultType ty)
      return $ setType resultType fcall {args = eArgs}

   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  t != nullType
    -- ------------------------------------------------------
    --  E |- \ (x1 : t1, .., xn : tn) -> body : (t1 .. tn) -> t
    doTypecheck closure@(Closure {eparams, body}) = do
      eEparams <- mapM (local addTypeVars . typecheck) eparams
      eBody <- local (addTypeVars . addParams eEparams) $ typecheckNotNull body
      let returnType = AST.getType eBody
          ty = arrowType (map ptype eEparams) returnType
      return $ setType ty closure {body = eBody, eparams = eEparams}
      where
        typeParams = concatMap (typeComponents . ptype) eparams
        typeVars = nub $ filter isTypeVar typeParams
        addTypeVars = addTypeParameters typeVars

    --  E |- body : t
    --  ------------------
    --  E |- async body : t
    doTypecheck task@(Async {body}) =
        do eBody <- typecheckNotNull body
           let returnType = AST.getType eBody
           return $ setType (futureType returnType) task {body = eBody}

    --  E |- e1 : t1; E, x1 : t1 |- e2 : t2; ..; E, x1 : t1, .., x(n-1) : t(n-1) |- en : tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  x1 != nullType .. xn != nullType
    -- --------------------------------------------------------------------------------------
    --  E |- let x1 = e1 .. xn = en in body : t
    doTypecheck let_@(Let {decls, body}) =
        do eDecls <- typecheckDecls decls
           let declNames = map fst eDecls
               declTypes = map (AST.getType . snd) eDecls
           when (any isBottomType (concatMap typeComponents declTypes)) $
                tcError "Cannot infer type of 'Nothing'"
           eBody <- local (extendEnvironment (zip declNames declTypes)) $ typecheck body
           return $ setType (AST.getType eBody) let_ {decls = eDecls, body = eBody}
        where
          typecheckDecls [] = return []
          typecheckDecls ((name, expr):decls') =
              do eExpr <- typecheckNotNull expr
                 eDecls <- local (extendEnvironment [(name, AST.getType eExpr)]) $ typecheckDecls decls'
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
           return $ setType resultType ifThenElse {cond = eCond, thn = setType resultType eThn, els = setType resultType eEls}
        where
          matchBranches ty1 ty2
              | isNullType ty1 && isNullType ty2 =
                  tcError "Cannot infer result type of if-statement"
              | isNullType ty1 && isRefType ty2 = return ty2
              | isNullType ty2 && isRefType ty1 = return ty1
              | otherwise = if ty2 == ty1
                            then return ty1
                            else tcError $ "Type mismatch in different branches of if-statement:\n" ++
                                           "  then:  " ++ show ty1 ++ "\n" ++
                                           "  else:  " ++ show ty2

    --  E |- cond : bool
    --  E |- body : t
    -- -----------------------
    --  E |- while cond body : t
    doTypecheck while@(While {cond, body}) =
        do eCond <- hasType cond boolType
           eBody <- typecheck body
           return $ setType (AST.getType eBody) while {cond = eCond, body = eBody}

    --  E |- val : Fut t
    -- ------------------
    --  E |- get val : t
    doTypecheck get@(Get {val}) =
        do eVal <- typecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty || isStreamType ty) $
                  tcError $ "Cannot get the value of non-future type '" ++ show ty ++ "'"
           return $ setType (getResultType ty) get {val = eVal}

    --  E |- val : t
    --  isStreaming(currentMethod)
    -- -----------------------------
    --  E |- yield val : void
    doTypecheck yield@(Yield {val}) =
        do eVal <- typecheck val
           result <- asks currentMethod
           when (isNothing result) $
                tcError "Can only yield from (streaming) methods"
           let mtd = fromJust result
               mType = methodType mtd
               eType = AST.getType eVal
           unless (isStreamMethod mtd) $
                  tcError $ "Cannot yield in non-streaming method '" ++
                            show (methodName mtd) ++ "'"
           unlessM (eType `subtypeOf` mType) $
                  tcError $ "Cannot yield value of type '" ++ show eType ++
                            "' in streaming method of type '" ++ show mType ++ "'"
           return $ setType voidType yield {val = eVal}

    --  isStreaming(currentMethod)
    -- ----------------------------
    --  E |- eos : void
    doTypecheck eos@(Eos {}) =
        do result <- asks currentMethod
           when (isNothing result) $
                tcError "Can only yield from (streaming) methods"
           let mtd = fromJust result
           unless (isStreamMethod mtd) $
                  tcError $ "Cannot have end-of-stream in non-streaming method '" ++
                            show (methodName mtd) ++ "'"
           return $ setType voidType eos

    --  E |- s : Stream t
    -- ---------------------
    --  E |- eos s : bool
    doTypecheck iseos@(IsEos {target}) =
        do eTarget <- typecheck target
           unless (isStreamType $ AST.getType eTarget) $
                  tcError $ "Cannot check end of stream on non-stream target '" ++ show (ppExpr target) ++ "'"
           return $ setType boolType iseos{target = eTarget}

    --  E |- s : Stream t
    -- ---------------------------
    --  E |- s.next() : Stream t
    doTypecheck next@(StreamNext {target}) =
        do eTarget <- typecheck target
           let eType = AST.getType eTarget
           unless (isStreamType eType) $
                  tcError $ "Cannot get next value from non-stream target '" ++ show (ppExpr target) ++ "'"
           return $ setType eType next{target = eTarget}

    --
    --    ------------------ :: suspend
    --    suspend : void
    doTypecheck suspend@(Suspend {}) =
        return $ setType voidType suspend

    --    f : Fut T
    --    ------------------ :: await
    --    await f : void
    doTypecheck await@(Await {val}) =
        do eVal <- typecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty) $
                  tcError $ "Cannot await the value of non-future type '" ++ show ty ++ "'"
           return $ setType voidType await {val = eVal}

    --    f : Fut T
    --    c : T -> T'
    --    ------------------ :: chain
    --    f then c : Fut T'
    doTypecheck futureChain@(FutureChain {future, chain}) =
        do eFuture <- typecheck future
           eChain <- typecheck chain
           let ty = AST.getType eFuture
           unless (isFutureType ty) $
                  tcError $ "Cannot chain with a non-future type '" ++ show ty ++ "'"
           let ty' = AST.getType eChain
           unless (isArrowType ty') $
                  tcError $ "Chaining requires a closure argument '" ++ show ty' ++ "'"
           unless ([getResultType ty] == getArgTypes ty') $
                  tcError $ "Future value has type '" ++ show (getResultType ty) ++ "' but chained closure expects '" ++ show (head (getArgTypes ty')) ++ "'"
           return $ setType (futureType (getResultType ty')) futureChain {future = eFuture, chain = eChain}

    --  E |- target : t'
    --  fieldLookup(t', name) = t
    -- ---------------------------
    --  E |- target.name : t
    doTypecheck fAcc@(FieldAccess {target, name}) = do
      eTarget <- typecheck target
      let targetType = AST.getType eTarget
      unless (isThisAccess target || isPassiveClassType targetType) $
        tcError $ "Cannot read field of expression '" ++
          show (ppExpr target) ++ "' of " ++ Types.showWithKind targetType
      fdecl <- findField targetType name
      bindings <- formalBindings targetType
      let ty' = replaceTypeVars bindings (ftype fdecl)
      return $ setType ty' fAcc {target = eTarget}

    --  E |- lhs : t
    --  isLval(lhs)
    --  E |- rhs : t
    -- ------------------------
    --  E |- name = rhs : void
    doTypecheck assign@(Assign {lhs = lhs@VarAccess{name}, rhs}) =
        do eLhs <- typecheck lhs
           varIsLocal <- asks $ isLocal name
           unless varIsLocal $
                  tcError $ "Left hand side '" ++ show (ppExpr lhs) ++
                            "' is a global variable and cannot be assigned to"
           eRhs <- hasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}

    doTypecheck assign@(Assign {lhs, rhs}) =
        do unless (isLval lhs) $
             tcError $ "Left hand side '" ++ show (ppExpr lhs) ++
               "' cannot be assigned to"
           eLhs <- typecheck lhs
           mtd <- asks currentMethod
           unless (isNothing mtd || isConstructor (fromJust mtd)) $
                  assertNotValField eLhs
           eRhs <- hasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}
        where
          assertNotValField f
              | FieldAccess {target, name} <- f = do
                  let targetType = AST.getType target
                  fdecl <- findField targetType name
                  when (isValField fdecl) $
                       tcError $ "Cannot assign to val-field '" ++
                                 show name ++ "' in " ++
                                 classOrTraitName targetType
              | otherwise = return ()

    --  name : t \in E
    -- ----------------
    --  E |- name : t
    doTypecheck var@(VarAccess {name}) =
        do varType <- asks $ varLookup name
           case varType of
             Just ty -> return $ setType ty var
             Nothing -> tcError $ "Unbound variable '" ++ show name ++ "'"

    --
    -- ----------------------
    --  E |- null : nullType
    doTypecheck e@Null {} = return $ setType nullType e

    --
    -- ------------------
    --  E |- true : bool
    doTypecheck true@BTrue {} = return $ setType boolType true

    --
    -- ------------------
    --  E |- false : bool
    doTypecheck false@BFalse {} = return $ setType boolType false


   ---  |- ty
    --  classLookup(ty) = _
    --  ty != Main
    -- ----------------------
    --  E |- new ty : ty
    doTypecheck new@(New {ty}) = do
      ty' <- resolveType ty
      unless (isClassType ty') $
             tcError $ "Cannot create an object of type '" ++ show ty ++ "'"
      when (isMainType ty') $
           tcError "Cannot create additional Main objects"
      return $ setType ty' new{ty = ty'}

   ---  |- ty
    --  classLookup(ty) = _
    --  ty != Main
    -- ----------------------
    --  E |- peer ty : ty
    doTypecheck peer@(Peer {ty}) =
        do ty' <- resolveType ty
           unless (isActiveClassType ty') $
                  tcError $ "Cannot create an object of type '" ++
                  show ty ++ "'"
           when (isMainType ty') $
                tcError "Cannot create additional Main objects"
           return $ setType ty' peer{ty = ty'}

    --  E |- n : int
    --  E |- m : int
    --  E |- k : int
    -- ----------------------------
    --  E |- [n..m by k] : Range
    doTypecheck range@(RangeLiteral {start, stop, step}) =
        do eStart <- hasType start intType
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
             tcError "For loops can only iterate over ranges or arrays"

           let elementType = if isRangeType srcType
                             then intType
                             else getResultType srcType
           bodyTyped <- typecheckBody elementType body
           return $ setType (AST.getType bodyTyped) for{step = stepTyped
                                                       ,src  = srcTyped
                                                       ,body = bodyTyped}
        where
          addIteratorVariable ty = extendEnvironment [(name, ty)]
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
    doTypecheck arr@(ArrayLiteral {args}) =
        do when (null args) $
                tcError "Array literal must have at least one element"
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
                  tcError $ "Cannot index non-array '" ++
                            show (ppExpr target) ++
                            "' of type '" ++ show targetType ++ "'"
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
                  tcError $ "Cannot calculate the size of non-array '" ++
                            show (ppExpr target) ++
                            "' of type '" ++ show targetType ++ "'"
           return $ setType intType arrSize{target = eTarget}

    --  count("{}", stringLit) = n
    --  E |- arg1 : t1 .. E |- argn : tn
    -- ---------------------------------------------
    --  E |- print(stringLit, arg1 .. argn) : void
    doTypecheck e@(Print {stringLit, args}) =
        do let noArgs = T.count (T.pack "{}") (T.pack stringLit)
           unless (noArgs == length args) $
                  tcError $ "Wrong number of arguments to format string. " ++
                            "Expected " ++ show (length args) ++ ", got " ++ show noArgs ++ "."
           eArgs <- mapM typecheck args
           return $ setType voidType e {args = eArgs}

    --  E |- arg : int
    -- ------------------------
    --  E |- exit(arg) : void
    doTypecheck exit@(Exit {args}) =
        do eArgs <- mapM typecheck args
           unless (length eArgs == 1 && isIntType (AST.getType (head eArgs))) $
                  tcError "exit expects a single integer argument"
           return $ setType voidType exit {args = eArgs}

    doTypecheck stringLit@(StringLiteral {}) = return $ setType stringType stringLit

    doTypecheck intLit@(IntLiteral {}) = return $ setType intType intLit

    doTypecheck realLit@(RealLiteral {}) = return $ setType realType realLit

   ---  |- ty
    -- ---------------------
    -- E |- embed ty _ : ty
    doTypecheck embed@(Embed {ty}) =
        do ty' <- resolveType ty
           return $ setType ty' embed{ty = ty'}

    --  E |- operand : bool
    -- -------------------------
    --  E |- not operand : bool
    doTypecheck unary@(Unary {uop, operand})
      | uop == Identifiers.NOT = do
        eOperand <- typecheck operand
        let eType = AST.getType eOperand
        unless (isBoolType eType) $
                tcError $ "Operator '" ++ show uop ++ "' is only defined for boolean types\n" ++
                          "Expression '" ++ show (ppExpr eOperand) ++ "' has type '" ++ show eType ++ "'"
        return $ setType boolType unary { operand = eOperand }

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
          eLoper <- typecheck loper
          eRoper <- typecheck roper
          let lType = AST.getType eLoper
              rType = AST.getType eRoper
          unless (isBoolType lType && isBoolType rType) $
                  tcError $ "Operator '"++ show binop ++ "' is only defined for boolean types\n" ++
                          "   Left type: '" ++ show lType ++ "'\n" ++
                          "   Right type: '" ++ show rType ++ "'"
          return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` cmpOps = do
             eLoper <- typecheck loper
             eRoper <- typecheck roper
             let lType = AST.getType eLoper
                 rType = AST.getType eRoper
             unless (isNumeric lType && isNumeric rType) $
                    tcError $ "Operator '"++ show binop ++ "' is only defined for numeric types\n" ++
                          "   Left type: '" ++ show lType ++ "'\n" ++
                          "   Right type: '" ++ show rType ++ "'"
             return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` eqOps = do
             eLoper <- typecheck loper
             eRoper <- hasType roper (AST.getType eLoper)
             return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` arithOps = do
             eLoper <- typecheck loper
             eRoper <- typecheck roper
             let lType = AST.getType eLoper
                 rType = AST.getType eRoper
             unless (isNumeric lType && isNumeric rType) $
                    tcError $ "Operator '"++ show binop ++ "' is only defined for numeric types\n" ++
                          "   Left type: '" ++ show lType ++ "'\n" ++
                          "   Right type: '" ++ show rType ++ "'"
             return $ setType (coerceTypes lType rType) bin {loper = eLoper, roper = eRoper}
      | otherwise = tcError $ "Undefined binary operator '" ++ show binop ++ "'"
      where
        boolOps  = [Identifiers.AND, Identifiers.OR]
        cmpOps   = [Identifiers.LT, Identifiers.GT, Identifiers.LTE, Identifiers.GTE]
        eqOps    = [Identifiers.EQ, NEQ]
        arithOps = [PLUS, MINUS, TIMES, DIV, MOD]
        coerceTypes ty1 ty2
            | isRealType ty1 = realType
            | isRealType ty2 = realType
            | otherwise = intType

    doTypecheck e = error $ "Cannot typecheck expression " ++ show (ppExpr e)

--  classLookup(ty) = _
-- ---------------------
--  null : ty
coerceNull null ty
    | isNullType ty ||
      isTypeVar ty = tcError "Cannot infer type of null valued expression"
    | isRefType ty = return $ setType ty null
    | isMaybeType ty = return $ setType ty null
    | otherwise =
        tcError $ "Null valued expression cannot have type '" ++
                  show ty ++ "' (must have reference type)"

coerce :: Type -> Type -> TypecheckM Type
coerce expected actual
  | isRefType actual && isRefType expected = do
     resultTypeParams <- zipWithM coerce (getTypeParameters expected)
                                         (getTypeParameters actual)
     return $ setTypeParameters actual resultTypeParams
  | hasResultType expected && hasResultType actual = do
       resultType <- coerce (getResultType expected) (getResultType actual)
                     `catchError` (\_ -> tcError $ "Type '" ++ show actual ++
                                         "' does not match expected type '" ++
                                         show expected ++ "'")
       return $ setResultType actual resultType
  | isNullType actual = do
      when (isNullType expected || isTypeVar expected) $
        tcError "Cannot infer type of null valued expression"
      unless (canBeNull expected) $
        tcError $ "Null valued expression cannot have type '" ++
        show actual ++ "' (must have reference type)"
      return expected
  | isBottomType actual = do
      when (isBottomType expected) $
        tcError $ "Cannot infer type of 'Nothing'"
      return expected
  | otherwise = do
      unless (actual == expected) $
        tcError $ "Type '" ++ show actual ++ "' does not match expected type '" ++
                  show expected ++ "'"
      return actual
  where
    canBeNull ty =
      isRefType ty || isFutureType ty || isArrayType ty ||
      isStreamType ty || isCapabilityType ty || isArrowType ty || isParType ty

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
matchArguments (arg:args) (typ:types) =
    do eArg <- do eArg <- typecheck arg
                  if isNullType (AST.getType eArg) then
                      coerceNull eArg typ
                  else
                      return eArg
       bindings <- matchTypes typ (AST.getType eArg)
       (eArgs, bindings') <-
           local (bindTypes bindings) $ matchArguments args types
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
      isStreamType expected && isStreamType ty =
        matchTypes (getResultType expected) (getResultType ty)
        `catchError` (\_ -> tcError $ "Type '" ++ show ty ++
                                      "' does not match expected type '" ++
                                      show expected ++ "'")
    | isArrowType expected  && isArrowType ty =
        let expArgTypes = getArgTypes expected
            argTypes    = getArgTypes ty
            expRes      = getResultType expected
            resTy       = getResultType ty
        in
          do
            argBindings <- matchArgs expArgTypes argTypes
            local (bindTypes argBindings) $ matchTypes expRes resTy
    | isTypeVar expected = do
      params <- asks typeParameters
      if expected `elem` params then
          assertMatch expected ty
      else do
        result <- asks $ typeVarLookup expected
        case result of
          Just boundType -> do
            unlessM (ty `subtypeOf` boundType) $
              tcError $ "Type variable '" ++ show expected ++
                "' cannot be bound to both '" ++ show ty ++
                "' and '" ++ show boundType ++ "'"
            asks bindings
          Nothing -> do
            bindings <- asks bindings
            return $ (expected, ty) : bindings
    | isMaybeType expected && isMaybeType ty =
         matchTypes (getResultType expected) (getResultType ty)
         `catchError` (\_ -> tcError $ "Type '" ++ show ty ++
                                      "' does not match expected type '" ++
                                      show expected ++ "'")
    | otherwise = assertMatch expected ty
    where
      matchArgs [] [] = asks bindings
      matchArgs (ty1:types1) (ty2:types2) = do
        bindings <- matchTypes ty1 ty2
        local (bindTypes bindings) $ matchArgs types1 types2

      assertMatch expected ty = do
        assertSubtypeOf ty expected
        asks bindings

assertSubtypeOf :: Type -> Type -> TypecheckM ()
assertSubtypeOf sub super =
    unlessM (sub `subtypeOf` super) $
           tcError $ "Type '" ++ show sub ++
                     "' does not match expected type '" ++ show super ++ "'"
