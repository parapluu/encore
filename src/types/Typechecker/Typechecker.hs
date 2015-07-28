{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information of every expression node. It throws an exception
with a meaningful error message if it fails.

-}

module Typechecker.Typechecker(typecheckEncoreProgram) where

import Data.List
import Data.Maybe
import Text.Printf (printf)
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative ((<$>))

-- Module dependencies
import Identifiers
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST (getType, showWithKind)
import AST.PrettyPrinter
import Types
import Typechecker.Environment
import Typechecker.TypeError


-- | The top-level type checking function
typecheckEncoreProgram :: Program -> Either TCError Program
typecheckEncoreProgram p =
    do env <- buildEnvironment p
       runReader (runExceptT (doTypecheck p)) env

-- | Convenience function for throwing an exception with the
-- current backtrace
tcError msg =
    do bt <- asks backtrace
       throwError $ TCError (msg, bt)

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
  Eq a, AST.AST.HasMeta a) => String -> [a] -> m ()
assertDistinct something l =
  let
    duplicates = l \\ nub l
    first = head duplicates
  in
    unless (null duplicates) $
      tcError $ printf "Duplicate %s of %s" something $ AST.showWithKind first

resolveType :: Type -> ExceptT TCError (Reader Environment) Type
resolveType ty
  | isRefType ty = do
      result <- asks $ refTypeLookup ty
      case result of
        Nothing -> tcError $ concat ["Unknown type '", show ty, "'"]
        Just ty' -> return $ setTypeParameters ty' type_vars
  | otherwise = return ty
      where
        type_vars = getTypeParameters ty

-- | Convenience function for checking if a type is
-- well-formed. Returns the same type with correct activity
-- information.
checkType :: Type -> ExceptT TCError (Reader Environment) Type
checkType ty
    | isPrimitive ty = return ty
    | isTypeVar ty = do
      params <- asks typeParameters
      unless (ty `elem` params) $
        tcError $ "Free type variables in type '" ++ show ty ++ "'"
      return ty
    | isTrait ty = return ty
    | isClass ty = return ty
    | isRefType ty = resolveType ty
    | isFutureType ty = futureType <$> checkType result_type
    | isStreamType ty = streamType <$> checkType result_type
    | isArrayType ty = arrayType <$> checkType result_type
    | isParType ty = parType <$> checkType result_type
    | isArrowType ty = do argTypes <- mapM checkType (getArgTypes ty)
                          retType <- checkType result_type
                          return $ arrowType argTypes retType
    | otherwise = tcError $ "Unknown type '" ++ show ty ++ "'"
      where
        type_vars = getTypeParameters ty
        result_type = getResultType ty

-- | The actual typechecking is done using a Reader monad wrapped
-- in an Error monad. The Reader monad lets us do lookups in the
-- "Environment", and the Error monad lets us throw a
-- "TCError" exception anywhere.
class Checkable a where
    -- | Returns the typechecked version of its argument (i.e. an
    -- AST-node extended with type information)
    doTypecheck :: a -> ExceptT TCError (Reader Environment) a

    -- | Like 'doTypecheck' but records the backtrace for better
    -- error messages
    typecheck :: Pushable a => a -> ExceptT TCError (Reader Environment) a
    typecheck x = local (pushBT x) $ doTypecheck x

instance Checkable Program where
    --  E |- fun1 .. E |- funn
    --  E |- class1 .. E |- classm
    -- ----------------------------
    --  E |- funs classes
  doTypecheck p@Program{imports, functions, traits, classes} = do
    assertDistinct "definition" (allTraits p)
    etraits <- mapM typecheck traits
    assertDistinct "definition" (allClasses p)
    eclasses <- mapM typecheck classes
    assertDistinctThing "declaration" "class or trait name" $
                        map traitName (allTraits p) ++
                        map cname (allClasses p)
    eimps <- mapM doTypecheck imports   -- TODO: should probably use Pushable and typecheck
    assertDistinct "definition" (allFunctions p)
    efuns <- mapM typecheck functions
    return p{imports = eimps, functions = efuns,
      traits = etraits, classes = eclasses}

instance Checkable ImportDecl where
     -- TODO write down type rule
     -- TODO probably should use the Pushable mechanism
     doTypecheck (PulledImport meta name src program) =
       do eprogram <- doTypecheck program
          return $ PulledImport meta name src eprogram
     doTypecheck (Import _ _) = error "BUG: Import AST Nodes should not exist during typechecking"

instance Checkable Function where
   ---  |- funtype
    --  hasTypeVars(funtype) => funtype \in t1 .. tn
   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- funbody : funtype
    -- ----------------------------------------------------------
    --  E |- def funname(x1 : t1, .., xn : tn) : funtype funbody
    doTypecheck f@(Function {funtype, funparams, funbody}) = do
      ty <- checkType funtype
      eParams <- mapM typecheckParam funparams
      eBody <- local (addParams eParams) $
                     if isVoidType ty
                     then typecheck funbody
                     else hasType funbody ty
      return $ setType ty f {funtype = ty, funbody = eBody, funparams = eParams}
      where
        typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $
                                               do ty <- checkType ptype
                                                  return $ setType ty p)
        addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

instance Checkable Trait where
  doTypecheck t@Trait{traitName, traitFields, traitMethods} = do
    assertDistinctThing "declaration" "type parameter" typeParameters
    assertDistinct "requirement" traitFields
    efields <- mapM (local add_type_vars . typecheck) traitFields
    assertDistinct "definition" traitMethods
    emethods <- mapM typecheckMethod traitMethods
    return $ setType traitName $
      t{traitFields = efields, traitMethods = emethods}
    where
      typeParameters = getTypeParameters traitName
      add_type_vars = addTypeParameters typeParameters
      add_this = extendEnvironment [(thisName, traitName)]
      typecheckMethod m = local (add_type_vars . add_this) $ typecheck m

formal_bindings :: Type -> ExceptT TCError (Reader Environment) [(Type, Type)]
formal_bindings actual = do
  origin <- asks $ refTypeLookupUnsafe actual
  formal_vars <- return $ getTypeParameters origin
  actual_vars <- return $ getTypeParameters actual
  when (length formal_vars /= length actual_vars) $
    tcError $ printf "'%s' expects %d type arguments, but '%s' has %d"
      (show origin) (length formal_vars) (show actual) (length actual_vars)
  matchTypeParameters formal_vars actual_vars

find_trait_or_error :: (MonadError TCError m, MonadReader Environment m) =>
  Trait -> m Trait
find_trait_or_error trait = do
    trait' <- asks $ traitLookup $ traitName trait
    when (isNothing trait') $ tcError $ "couldnt find trait: " ++ show trait
    return $ fromJust trait'

find_method_or_error :: (MonadError TCError m, MonadReader Environment m) =>
  Type -> Name -> m MethodDecl
find_method_or_error ty name = do
  m' <- asks $ methodLookup ty name
  when (isNothing m') $ tcError $
    concat [no_method name, " in ", ref, " '", show ty, "'"]
  return $ fromJust m'
  where
    ref
      | isClass ty = "class"
      | isTrait ty = "trait"
    no_method (Name "_init") = "No constructor"
    no_method n = concat ["No method '", show n, "'"]

match_args_or_error :: (MonadError TCError m, MonadReader Environment m) =>
  MethodDecl -> Arguments -> m ()
match_args_or_error method args = do
  unless (actual == expected) $ tcError $
    concat [to_str name, " expect ", show expected, " but got ", show actual]
  where
    actual = length args
    expected = length sig_types
    sig_types = map ptype $ mparams method
    name = mname method
    to_str (Name "_init") = "Constructor"
    to_str n = concat ["Method '", show n, "'"]

isMainMethod :: Type -> Name -> Bool
isMainMethod ty name = isMainType ty && (name == Name "main")

instance Checkable ImplementedTrait where
  doTypecheck t@ImplementedTrait{itrait} = do
    trait <- find_trait_or_error itrait
    mapM checkType type_vars
    bindings <- formal_bindings ty
    ty' <- checkType $ replaceTypeVars bindings ty
    efields <- return $ map (update_field bindings) $ traitFields trait
    return $ setType ty' t{itrait=trait{traitFields = efields}}
    where
      ty = traitName itrait
      type_vars = getTypeParameters ty
      add_type_vars = addTypeParameters type_vars
      update_field bindings f =
        let
          ft = ftype f
          ft' = replaceTypeVars bindings ft
        in
          setType ft' f

match_field :: [FieldDecl] -> FieldDecl -> Bool
match_field fields f =
  let
    r = find (==f) fields
    t = ftype f
    t' = ftype $ fromJust r
  in
    isJust r && t' == t

match_field_or_error :: (MonadError TCError m, MonadReader Environment m) =>
  [FieldDecl] -> FieldDecl -> m ()
match_field_or_error fields f = do
  unless (match_field fields f) $
    tcError $ concat ["couldnt find field: '", show f, "'"]

meet_required_fields :: (MonadError TCError m, MonadReader Environment m) =>
  [FieldDecl] -> ImplementedTrait -> m ()
meet_required_fields fields t@ImplementedTrait{itrait} = do
  mapM_ (match_field_or_error fields) $ traitFields itrait

ensure_no_method_conflict :: (MonadError TCError m, MonadReader Environment m)
  => [MethodDecl] -> [ImplementedTrait] -> m ()
ensure_no_method_conflict methods itraits =
  let
    all_methods = methods ++ concatMap itraitMethods itraits
    unique = nub all_methods
    diff = all_methods \\ unique
    first = head diff
    in_this_class = first `elem` methods
    overlapping_traits = filter (contain_f first) itraits
  in
    if null diff then
      return ()
    else if in_this_class then
      tcError $ concat ["'", show (mname first),
        "' is defined in current class and trait '",
        show (overlapping_traits !! 0), "'"]
    else
      tcError $ concat ["'", show (mname first),
        "' is defined in trait '", show (overlapping_traits !! 0),
        "' and trait '", show (overlapping_traits !! 1), "'"]
  where
    contain_f f ImplementedTrait{itrait} = f `elem` (traitMethods itrait)

instance Checkable ClassDecl where
  --  distinctNames(fields)
  ---  |- field1 .. |- fieldn
  --  distinctNames(methods)
  --  E, this : cname |- method1 .. E, this : cname |- methodm
  -- -----------------------------------------------------------
  --  E |- class cname fields methods
  doTypecheck c@(Class {cname, ctraits, fields, methods}) = do
    assertDistinctThing "declaration" "type parameter" typeParameters
    assertDistinct "occurrence" ctraits
    unless (isPassiveRefType cname || null ctraits) $
           tcError $ "Traits can only be used for passive classes"
    assertDistinct "declaration" fields
    efields <- mapM (local add_type_vars . typecheck) fields
    ectraits <- mapM (local (add_type_vars . add_this) . typecheck) ctraits
    mapM (meet_required_fields efields) ectraits
    emethods <- mapM typecheckMethod methods
    -- TODO add namespace for trait methods
    ensure_no_method_conflict emethods ectraits
    return $ setType cname c{fields = efields, ctraits = ectraits,
      methods = emethods}
    where
      typeParameters = getTypeParameters cname
      add_type_vars = addTypeParameters typeParameters
      add_this = extendEnvironment [(thisName, cname)]
      typecheckMethod m = local (add_type_vars . add_this) $ typecheck m

instance Checkable FieldDecl where
   ---  |- t
    -- -----------------
   ---  |- f : t
    doTypecheck f@(Field {ftype}) = do
      ty <- checkType ftype
      return $ setType ty f

instance Checkable ParamDecl where
  doTypecheck p@Param{ptype} = do
    ty <- checkType ptype
    return $ p{ptype = ty}

instance Checkable MethodDecl where
   ---  |- mtype
    --  E |- e : mtype
    -- --------------------------------------------
    --  E, this : Main |- def main() : mtype mbody
    --
   ---  |- mtype
   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- mbody : mtype
    -- -----------------------------------------------------
    --  E |- def mname(x1 : t1, .., xn : tn) : mtype mbody
    doTypecheck m@(Method {mtype, mparams, mbody, mname}) =
        do ty <- checkType mtype
           Just thisType <- asks $ varLookup thisName
           when (isMainMethod thisType mname) checkMainParams
           eMparams <- mapM typecheckParam mparams
           eBody <- local (addParams eMparams) $
                          if isVoidType ty
                          then typecheck mbody
                          else hasType mbody ty
           return $ setType ty m {mtype = ty, mbody = eBody, mparams = eMparams}
        where
          checkMainParams = unless ((map ptype mparams) `elem` [[], [arrayType stringType]]) $
                              tcError $
                                "Main method must have argument type () or ([string])"
          typecheckParam p@(Param{ptype}) = local (pushBT p) $
                                            do ty <- checkType ptype
                                               return $ setType ty p
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

   ---  |- mtype
   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- mbody : mtype
    -- -----------------------------------------------------
    --  E |- stream mname(x1 : t1, .., xn : tn) : mtype mbody
    doTypecheck m@(StreamMethod {mtype, mparams, mbody}) =
        do Just thisType <- asks $ varLookup thisName
           unless (isActiveRefType thisType) $
                  tcError "Cannot have streaming methods in a passive class"
           ty <- checkType mtype
           eMparams <- mapM typecheckParam mparams
           eBody    <- local (addParams eMparams) $ typecheck mbody
           return $ setType ty m {mtype = ty, mbody = eBody, mparams = eMparams}
        where
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

-- | 'hasType e ty' typechecks 'e' (with backtrace) and returns
-- the result if 'e' is a subtype of 'ty' 
hasType :: Expr -> Type -> ExceptT TCError (Reader Environment) Expr
hasType e ty = local (pushBT e) $ checkHasType e ty
    where
      checkHasType expr ty = 
          do eExpr <- doTypecheck expr
             let exprType = AST.getType eExpr
             if isNullType exprType then
                 coerceNull eExpr ty
             else
                 do assertSubtypeOf exprType ty
                    return eExpr

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
        do ty' <- checkType ty
           eBody <- hasType body ty'
           return $ setType ty' $ te{body = eBody, ty = ty'}

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
                  (show $ ppExpr target) ++
                  "' of type '" ++ show targetType ++ "'"
      when (isMainMethod targetType name) $ tcError "Cannot call the main method"
      when (name == Name "init") $ tcError
        "Constructor method 'init' can only be called during object creation"
      mdecl <- find_method_or_error targetType name
      match_args_or_error mdecl args
      f_bindings <- formal_bindings targetType
      paramTypes <- mapM (resolveType . ptype) (mparams mdecl)
      methodType <- resolveType $ mtype mdecl
      (eArgs, bindings) <- local (bindTypes f_bindings) $
        matchArguments args paramTypes
      resultType <- resolveType $ replaceTypeVars bindings methodType
      let returnType = ret_type targetType mdecl resultType
      return $ setType returnType mcall {target = eTarget, args = eArgs}
      where
        ret_type targetType method t
          | is_sync_call targetType = t
          | isStreamMethod method = streamType t
          | otherwise = futureType t
        is_sync_call targetType =
          isThisAccess target ||
          isPassiveRefType targetType ||
          isTrait targetType -- TODO now all trait methods calls are sync

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
      unless (isActiveRefType targetType) $
           tcError $ "Cannot send message to expression '" ++
                     (show $ ppExpr target) ++
                     "' of type '" ++ show targetType ++ "'"
      mdecl <- find_method_or_error targetType name
      paramTypes <- mapM (resolveType . ptype) (mparams mdecl)
      match_args_or_error mdecl args
      bindings <- formal_bindings targetType
      (eArgs, _) <- local (bindTypes bindings) $ matchArguments args paramTypes
      return $ setType voidType msend {target = eTarget, args = eArgs}

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
      argTypes <- mapM resolveType $ getArgTypes ty
      unless (length args == length argTypes) $
             tcError $ "Function '" ++ show name ++ "' of type '" ++ show ty ++
                       "' expects " ++ show (length argTypes) ++ " arguments. Got " ++
                       show (length args)
      (eArgs, bindings) <- matchArguments args argTypes
      resultType <- checkType $ replaceTypeVars bindings (getResultType ty)
      return $ setType resultType fcall {args = eArgs}

   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  t != nullType
    -- ------------------------------------------------------
    --  E |- \ (x1 : t1, .., xn : tn) -> body : (t1 .. tn) -> t
    doTypecheck closure@(Closure {eparams, body}) = do
      eEparams <- mapM (local add_type_vars . typecheck) eparams
      eBody <- local (add_type_vars . (addParams eEparams)) $ typecheck body
      let returnType = AST.getType eBody
          ty = arrowType (map ptype eEparams) returnType
      when (isNullType returnType) $
           tcError $ "Cannot infer return type of closure with null-valued body"
      return $ setType ty closure {body = eBody, eparams = eEparams}
      where
        all_param_types = concatMap (typeComponents . ptype) eparams
        type_vars = nub $ filter isTypeVar all_param_types
        add_type_vars = addTypeParameters type_vars
        to_tuple params = map (\Param{pname, ptype} -> (pname, ptype)) params
        addParams = extendEnvironment . to_tuple

    --  E |- body : t
    --  ------------------
    --  E |- async body : t
    doTypecheck task@(Async {body}) =
        do eBody <- typecheck body
           let returnType = AST.getType eBody
           when (isNullType returnType) $
               tcError $ "Cannot infer the return type of the task expression"
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
           when (any isNullType declTypes) $
                tcError $ "Cannot infer type of null-valued expression"
           eBody <- local (extendEnvironment (zip declNames declTypes)) $ typecheck body
           return $ setType (AST.getType eBody) let_ {decls = eDecls, body = eBody}
        where
          typecheckDecls [] = return []
          typecheckDecls ((name, expr):decls') =
              do eExpr <- typecheck expr
                 eDecls <- local (extendEnvironment [(name, AST.getType eExpr)]) $ typecheckDecls decls'
                 return $ (name, eExpr):eDecls

    --  E |- en : t
    -- ------------------------
    --  E |- {e1; ..; en} : t
    doTypecheck e@(Seq {eseq}) =
        do eEseq <- mapM typecheck eseq
           let seqType = AST.getType (last eEseq)
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
                  tcError $ "Cannot infer result type of if-statement"
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
           bt <- asks backtrace
           let mtd   = currentMethod bt
               mType = mtype mtd
               eType = AST.getType eVal
           unless (isStreamMethod mtd) $
                  tcError $ "Cannot yield in non-streaming method '" ++ show (mname mtd) ++ "'"
           unless (eType `subtypeOf` mType) $
                  tcError $ "Cannot yield value of type '" ++ show eType ++
                            "' in streaming method of type '" ++ show mType ++ "'"
           return $ setType voidType yield {val = eVal}

    --  isStreaming(currentMethod)
    -- ----------------------------
    --  E |- eos : void
    doTypecheck eos@(Eos {}) =
        do bt <- asks backtrace
           let mtd = currentMethod bt
           unless (isStreamMethod mtd) $
                  tcError $ "Cannot have end-of-stream in non-streaming method '" ++ show (mname mtd) ++ "'"
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
        do return $ setType voidType suspend

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
      unless (isThisAccess target || isPassiveRefType targetType) $
        tcError $ "Cannot read field of expression '" ++
          (show $ ppExpr target) ++ "' of " ++ Types.showWithKind targetType
      fdecl <- asks $ fieldLookup targetType name
      case fdecl of
        Just Field{ftype} -> do
          bindings <- formal_bindings targetType
          ty' <- checkType $ replaceTypeVars bindings ftype
          return $ setType ty' fAcc {target = eTarget}
        Nothing -> tcError $ "No field '" ++ show name ++
          "' in ref type '" ++ show targetType ++ "'"

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
        do eLhs <- typecheck lhs
           unless (isLval lhs) $
             tcError $ "Left hand side '" ++ show (ppExpr lhs) ++
               "' cannot be assigned to"
           eRhs <- hasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}

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
      ty' <- checkType ty
      unless (isClass ty') $
             tcError $ "Cannot create an object of type '" ++ show ty ++ "'"
      when (isMainType ty') $
           tcError "Cannot create additional Main objects"
      bindings <- formal_bindings ty'
      ty'' <- return $ replaceTypeVars bindings ty'
      return $ setType ty'' new{ty = ty''}

   ---  |- ty
    --  classLookup(ty) = _
    --  ty != Main
    -- ----------------------
    --  E |- peer ty : ty
    doTypecheck peer@(Peer {ty}) =
        do ty' <- checkType ty
           unless (isActiveRefType ty') $
                  tcError $ "Cannot create an object of type '" ++
                  show ty ++ "'"
           when (isMainType ty') $
                tcError "Cannot create additional Main objects"
           return $ setType ty' peer{ty = ty'}

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
    doTypecheck arr@(ArrayLiteral {args}) =
        do unless (length args > 0) $
                  tcError $ "Array literal must have at least one element"
           eArg1 <- doTypecheck (head args)
           let ty = AST.getType eArg1
           eArgs <- mapM (\e -> hasType e ty) args
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
                            (show $ ppExpr target) ++
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
                            (show $ ppExpr target) ++
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
           unless (length eArgs == 1 && (isIntType $ AST.getType (head eArgs))) $
                  tcError $ "exit expects a single integer argument"
           return $ setType voidType exit {args = eArgs}

    doTypecheck stringLit@(StringLiteral {}) = return $ setType stringType stringLit

    doTypecheck intLit@(IntLiteral {}) = return $ setType intType intLit

    doTypecheck realLit@(RealLiteral {}) = return $ setType realType realLit

   ---  |- ty
    -- ---------------------
    -- E |- embed ty _ : ty
    doTypecheck embed@(Embed {ty}) =
        do ty' <- checkType ty
           return $ setType ty' embed{ty = ty'}

    --  E |- operand : bool
    -- -------------------------
    --  E |- not operand : bool
    doTypecheck unary@(Unary {uop, operand})
      | uop == (Identifiers.NOT) = do
        eOperand <- typecheck operand
        let eType = AST.getType eOperand
        unless (isBoolType eType) $
                tcError $ "Operator '" ++ show uop ++ "' is only defined for boolean types\n" ++
                          "Expression '" ++ (show $ ppExpr eOperand) ++ "' has type '" ++ show eType ++ "'"
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
                          "   Left type: '" ++ (show $ lType) ++ "'\n" ++
                          "   Right type: '" ++ (show $ rType) ++ "'"
          return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` cmpOps =
          do eLoper <- typecheck loper
             eRoper <- typecheck roper
             let lType = AST.getType eLoper
                 rType = AST.getType eRoper
             unless (isNumeric lType && isNumeric rType) $
                    tcError $ "Operator '"++ show binop ++ "' is only defined for numeric types\n" ++
                          "   Left type: '" ++ (show $ lType) ++ "'\n" ++
                          "   Right type: '" ++ (show $ rType) ++ "'"
             return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` eqOps =
          do eLoper <- typecheck loper
             eRoper <- hasType roper (AST.getType eLoper)
             return $ setType boolType bin {loper = eLoper, roper = eRoper}
      | binop `elem` arithOps =
          do eLoper <- typecheck loper
             eRoper <- typecheck roper
             let lType = AST.getType eLoper
                 rType = AST.getType eRoper
             unless (isNumeric lType && isNumeric rType) $
                    tcError $ "Operator '"++ show binop ++ "' is only defined for numeric types\n" ++
                          "   Left type: '" ++ (show $ lType) ++ "'\n" ++
                          "   Right type: '" ++ (show $ rType) ++ "'"
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

    doTypecheck e = error $ "Cannot typecheck expression " ++ (show $ ppExpr e)

--  classLookup(ty) = _
-- ---------------------
--  null : ty
coerceNull null ty
    | isNullType ty ||
      isTypeVar ty = tcError "Cannot infer type of null valued expression"
    | isRefType ty = return $ setType ty null
    | otherwise =
        tcError $ "Null valued expression cannot have type '" ++
                  show ty ++ "' (must have reference type)"

--  E |- arg1 : t
--  matchTypes(B, t1, t) = B1
--  E, B1 |- arg2 : t2 .. argn : tn -| B'
-- ------------------------------------------------
--  E, B |- arg1 : t1 arg2 : t2 .. argn : tn -| B'
-- | @matchArguments args types@ checks if @arg_i@ matches
-- @type_i@ and throws a type checking error if they don't.
-- Returns the type checked arguments and a list of inferred
-- bindings, i.e. type variables to types.
matchArguments :: [Expr] -> [Type] -> ExceptT TCError (Reader Environment) ([Expr], [(Type, Type)])
matchArguments [] [] = do bindings <- asks bindings
                          return ([], bindings)
matchArguments (arg:args) (typ:types) =
    do eArg <- do eArg <- typecheck arg
                  if (isNullType (AST.getType eArg)) then
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
matchTypes :: Type -> Type -> ExceptT TCError (Reader Environment) [(Type, Type)]
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
            argBindings <- matchArguments expArgTypes argTypes
            local (bindTypes argBindings) $ matchTypes expRes resTy
    | isTypeVar expected && isTypeVar ty = do
      unless (expected == ty) $ tcError $
        concat ["Can't match '", show expected, "' with '", show ty, "'"]
      asks bindings
      -- TODO
      -- It's dangerous to mach typevar with anything other than typevar, isn't?
    | isTypeVar expected = do
      result <- asks $ typeVarLookup expected
      case result of
        Just boundType -> do
          unless (ty `subtypeOf` boundType) $
            tcError $ "Type variable '" ++ show expected ++
              "' cannot be bound to both '" ++ show ty ++
                "' and '" ++ show boundType ++ "'"
          asks bindings
        Nothing -> do
          bindings <- asks bindings
          return $ (expected, ty) : bindings
    | otherwise = do assertSubtypeOf ty expected
                     asks bindings
    where
      matchArguments [] [] = asks bindings
      matchArguments (ty1:types1) (ty2:types2) = do
        bindings <- matchTypes ty1 ty2
        local (bindTypes bindings) $ matchArguments types1 types2

matchTypeParameters :: [Type] -> [Type] ->
                       ExceptT TCError (Reader Environment) [(Type, Type)]
matchTypeParameters formals params = do
  bindings <- mapM (uncurry matchTypes) $ zip formals params
  return $ concat bindings

assertSubtypeOf :: Type -> Type -> ExceptT TCError (Reader Environment) ()
assertSubtypeOf sub super =
    unless (sub `subtypeOf` super) $
           tcError $ "Type '" ++ show sub ++
                     "' does not match expected type '" ++ show super ++ "'"
