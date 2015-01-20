{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-}

{-|

Typechecks an "AST.AST" and produces the same tree, extended with
type information of every expression node. It throws an exception
with a meaningful error message if it fails.

-}

module Typechecker.Typechecker(typecheckEncoreProgram) where

-- Library dependencies
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Control.Monad.Reader
-- import Control.Monad.Error
import Control.Monad.Except

-- Module dependencies
import Identifiers
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST(hasType, getType)
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import Types
import Typechecker.Environment
import Typechecker.TypeError

-- | The top-level type checking function
typecheckEncoreProgram :: Program -> Either TCError Program
typecheckEncoreProgram p = do env <- buildEnvironment p
                              runReader (runExceptT (typecheck p)) env

-- | Convenience function for throwing an exception with the
-- current backtrace
tcError msg = do bt <- asks backtrace
                 throwError $ TCError (msg, bt)

-- | Convenience function for checking if a type is
-- well-formed. Returns the same type with correct activity
-- information.
checkType :: Type -> ExceptT TCError (Reader Environment) Type
checkType ty 
    | isPrimitive ty = return ty
    | isTypeVar ty = do params <- asks typeParameters
                        unless (ty `elem` params) $ 
                               tcError $ "Free type variables in type '" ++ show ty ++ "'"
                        return ty
    | isRefType ty = do result <- asks $ classTypeLookup ty
                        params <- mapM checkType (getTypeParameters ty)
                        case result of
                          Nothing -> tcError $ "Unknown type '" ++ show ty ++ "'"
                          Just refType -> 
                              do let formalParams = getTypeParameters refType
                                 unless (length params == length formalParams) $
                                        tcError $ "Class '" ++ show refType ++ "' " ++
                                                  "expects " ++ show (length formalParams) ++ " type parameters.\n" ++
                                                  "Type '" ++ show ty ++ "' has " ++ show (length params)
                                 if isActiveRefType refType then
                                     return $ makeActive $ setTypeParameters ty params
                                 else
                                     return $ makePassive $ setTypeParameters ty params

    | isFutureType ty = do ty' <- checkType $ getResultType ty
                           return $ futureType ty'
    | isParType ty = do ty' <- checkType $ getResultType ty
                        return $ parType ty'
    | isArrowType ty = do argTypes <- mapM checkType (getArgTypes ty)
                          retType <- checkType $ getResultType ty
                          return $ arrowType argTypes retType
    | otherwise = tcError $ "Unknown type '" ++ show ty ++ "'"

-- | The actual typechecking is done using a Reader monad wrapped
-- in an Error monad. The Reader monad lets us do lookups in the
-- "Environment", and the Error monad lets us throw a
-- "TCError" exception anywhere.
class Checkable a where
    -- | Returns the extended version of its argument (e.g. an
    -- AST-node extended with type information)
    typecheck :: a -> ExceptT TCError (Reader Environment) a

    -- | Returns the extended version of its argument if its type
    -- agrees with the second argument
    hasType   :: a -> Type -> ExceptT TCError (Reader Environment) a
    hasType _ _ = tcError "Typechecking not implemented for construct"

    -- | Convenience function for pushing and typechecking a
    -- component in one step.
    pushTypecheck :: Pushable a => a -> ExceptT TCError (Reader Environment) a
    pushTypecheck x = local (pushBT x) $ typecheck x

    pushHasType :: Pushable a => a -> Type -> ExceptT TCError (Reader Environment) a
    pushHasType x ty = local (pushBT x) $ hasType x ty

instance Checkable Program where
    --  E |- fun1 .. E |- funn
    --  E |- class1 .. E |- classm
    -- ----------------------------
    --  E |- funs classes
    typecheck (Program etl imps funs classes) = 
        do efuns <- mapM pushTypecheck funs
           eclasses <- mapM pushTypecheck classes
           return $ Program etl imps efuns eclasses

instance Checkable Function where
   ---  |- funtype
    --  hasTypeVars(funtype) => funtype \in t1 .. tn
   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- funbody : funtype
    -- ----------------------------------------------------------
    --  E |- def funname(x1 : t1, .., xn : tn) : funtype funbody
    typecheck f@(Function {funtype, funparams, funbody, funname}) = 
        do let typeParams = nub $ filter isTypeVar $ concatMap (typeComponents . ptype) funparams
           ty <- local (addTypeParameters typeParams) $ checkType funtype
           eParams <- mapM (\p -> local (addTypeParameters typeParams) $ typecheckParam p) funparams
           eBody <- local (addParams eParams) $ pushHasType funbody ty
           return $ setType ty f {funtype = ty, funbody = eBody, funparams = eParams}
        where
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $ 
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

instance Checkable ClassDecl where
    --  distinctNames(fields)
   ---  |- field1 .. |- fieldn
    --  distinctNames(methods)
    --  E, this : cname |- method1 .. E, this : cname |- methodm
    -- -----------------------------------------------------------
    --  E |- class cname fields methods
    typecheck c@(Class {cname, fields, methods}) =
        do distinctTypeParams
           distinctFieldNames
           efields <- mapM (\f -> withParams $ pushTypecheck f) fields
           distinctMethodNames
           emethods <- mapM typecheckMethod methods
           return $ setType cname c {fields = efields, methods = emethods}
        where
          withParams = local (addTypeParameters (getTypeParameters cname))
          typecheckMethod m = withParams $ local (extendEnvironment [(thisName, cname)]) $ pushTypecheck m
          distinctTypeParams = 
              let params = getTypeParameters cname
                  paramsNoDuplicates = nub params
              in
                case params \\ paramsNoDuplicates of
                  [] -> return ()
                  (p:_) -> tcError $ "Duplicate type parameter '" ++ show p ++ "'"
          distinctFieldNames = 
              let fieldsNoDuplicates = nubBy (\f1 f2 -> (fname f1 == fname f2)) fields
              in
                case fields \\ fieldsNoDuplicates of
                  [] -> return ()
                  (f:_) -> do bt <- asks backtrace
                              throwError $ TCError ("Duplicate definition of field '" ++ show (fname f) ++ "'" , push f bt)
          distinctMethodNames = 
              case methods \\ nubBy (\m1 m2 -> (mname m1 == mname m2)) methods of
                [] -> return ()
                (m:_) -> do bt <- asks backtrace
                            throwError $ TCError ("Duplicate definition of method '" ++ show (mname m) ++ "'" , push m bt)

instance Checkable FieldDecl where
   ---  |- t
    --  !hasTypeVars(t)
    -- -----------------
   ---  |- f : t
    typecheck f@(Field {ftype}) = do ty <- checkType ftype
                                     return $ setType ty f  

instance Checkable MethodDecl where
   ---  |- mtype 
    --  !hasTypeVars(mtype)
    --  E |- e : mtype
    -- --------------------------------------------
    --  E, this : Main |- def main() : mtype mbody
    --
   ---  |- mtype
    --  hasTypeVars(mtype) => mtype \in t1 .. tn
   ---  |- t1 .. |- tn
    --  E, x1 : t1, .., xn : tn |- mbody : mtype
    -- -----------------------------------------------------
    --  E |- def mname(x1 : t1, .., xn : tn) : mtype mbody
    typecheck m@(Method {mtype, mparams, mbody, mname}) = 
        do let typeParams = nub $ filter isTypeVar $ concatMap (typeComponents . ptype) mparams
           ty <- local (addTypeParameters typeParams) $ checkType mtype
           Just thisType <- asks $ varLookup thisName
           when (isMainType thisType && mname == Name "main") checkMainParams
           eMparams <- mapM (\p -> local (addTypeParameters typeParams) $ typecheckParam p) mparams
           eBody <- local (addParams eMparams) $ pushHasType mbody ty
           return $ setType ty m {mtype = ty, mbody = eBody, mparams = eMparams}
        where
          checkMainParams = unless ((map ptype mparams) `elem` [[] {-, [intType, arrayType stringType]-}]) $ 
                              tcError $
                                "Main method must have argument type () or (int, string[]) (but arrays are not supported yet)"
          typecheckParam p@(Param{ptype}) = local (pushBT p) $ 
                                            do ty <- checkType ptype
                                               return $ setType ty p
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

instance Checkable Expr where
    hasType expr ty = do eExpr <- pushTypecheck expr
                         let exprType = AST.getType eExpr
                         if isNullType exprType then
                             coerceNull eExpr ty
                         else
                             do assertSubtypeOf exprType ty
                                return eExpr
        where
          coerceNull null ty 
              | isNullType ty || 
                isTypeVar ty = tcError "Cannot infer type of null valued expression"
              | isRefType ty = return $ setType ty null
              | otherwise = tcError $ "Null valued expression cannot have type '" ++ show ty ++ "' (must have reference type)"

    -- 
    -- ----------------
    --  E |- () : void
    typecheck skip@(Skip {}) = return $ setType voidType skip

   ---  |- t
    --  E |- body : t
    -- ----------------------
    --  E |- (body : t) : t
    typecheck tExpr@(TypedExpr {body, ty}) = do ty' <- checkType ty
                                                pushHasType body ty'

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
    typecheck mcall@(MethodCall {target, name, args}) = 
        do eTarget <- pushTypecheck target
           let targetType = AST.getType eTarget
           unless (isRefType targetType) $ 
                tcError $ "Cannot call method on expression '" ++ 
                          (show $ ppExpr target) ++ 
                          "' of type '" ++ show targetType ++ "'"
           when (isMainType targetType && name == Name "main") $ tcError "Cannot call the main method"
           when (name == Name "init") $ tcError "Constructor method 'init' can only be called during object creation"
           lookupResult <- asks $ methodLookup targetType name
           (paramTypes, methodType) <- 
               case lookupResult of
                 Nothing -> 
                     tcError $
                        (case name of
                          Name "_init" -> "No constructor" 
                          _ -> "No method '" ++ show name ++ "'") ++
                        " in class '" ++ show targetType ++ "'"
                 Just result -> return result
           unless (length args == length paramTypes) $ 
                  tcError $
                     (case name of
                           Name "_init" -> "Constructor"
                           _ -> "Method '" ++ show name ++ "'") ++ 
                        " of class '" ++ show targetType ++ "' expects " ++ show (length paramTypes) ++ 
                        " arguments. Got " ++ show (length args)
           let actualTypeParams = getTypeParameters targetType
           formalTypeParams <- asks $ classTypeParameterLookup targetType
           targetBindings <- matchTypeParameters formalTypeParams actualTypeParams
           (eArgs, bindings) <- local (bindTypes targetBindings) $ matchArguments args paramTypes
           let resultType = replaceTypeVars bindings methodType
               returnType = if isThisAccess target || isPassiveRefType targetType
                            then resultType
                            else futureType resultType
           return $ setType returnType mcall {target = eTarget, args = eArgs}

    --  E |- e : t'
    --  isActiveRefType(t')
    --  methodLookup(t', m) = (t1 .. tn, _)
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    -- --------------------------------------
    --  E |- e!m(arg1, .., argn) : ()
    typecheck msend@(MessageSend {target, name, args}) = 
        do eTarget <- pushTypecheck target
           let targetType = AST.getType eTarget
           unless (isActiveRefType targetType) $ 
                tcError $ "Cannot send message to expression '" ++ 
                          (show $ ppExpr target) ++ 
                          "' of type '" ++ show targetType ++ "'"
           lookupResult <- asks $ methodLookup targetType name
           (paramTypes, _) <- 
               case lookupResult of
                 Nothing -> 
                     tcError $
                        (case name of
                          Name "_init" -> "No constructor" 
                          _ -> "No method '" ++ show name ++ "'") ++
                        " in class '" ++ show targetType ++ "'"
                 Just result -> return result
           unless (length args == length paramTypes) $ 
                  tcError $
                        (case name of
                           Name "_init" -> "Constructor"
                           _ -> "Method '" ++ show name ++ "'") ++ 
                        " of class '" ++ show targetType ++ "' expects " ++ show (length paramTypes) ++ 
                        " arguments. Got " ++ show (length args)
           let actualTypeParams = getTypeParameters targetType
           formalTypeParams <- asks $ classTypeParameterLookup targetType
           targetBindings <- matchTypeParameters formalTypeParams actualTypeParams
           (eArgs, bindings) <- local (bindTypes targetBindings) $ matchArguments args paramTypes
           return $ setType voidType msend {target = eTarget, args = eArgs}

    --  E |- f : (t1 .. tn) -> t
    --  typeVarBindings() = B
    --  E, B |- arg1 : t1 .. argn : tn -| B'
    --  B'(t) = t'
    -- --------------------------------------
    --  E |- f(arg1, .., argn) : t'
    typecheck fcall@(FunctionCall {name, args}) = 
        do funType <- asks $ varLookup name
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
    typecheck closure@(Closure {eparams, body}) = 
        do let typeParams = nub $ filter isTypeVar $ concatMap (typeComponents . ptype) eparams
           eEparams <- mapM (\p -> local (addTypeParameters typeParams) $ typecheckParam p) eparams
           eBody <- local (addParams eEparams) $ pushTypecheck body
           let returnType = AST.getType eBody
           when (isNullType returnType) $ 
                tcError $ "Cannot infer return type of closure with null-valued body"
           return $ setType (arrowType (map ptype eparams) returnType) closure {body = eBody, eparams = eEparams}
        where
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params
           
    --  E |- e1 : t1; E, x1 : t1 |- e2 : t2; ..; E, x1 : t1, .., x(n-1) : t(n-1) |- en : tn
    --  E, x1 : t1, .., xn : tn |- body : t
    --  x1 != nullType .. xn != nullType
    -- --------------------------------------------------------------------------------------
    --  E |- let x1 = e1 .. xn = en in body : t
    typecheck let_@(Let {decls, body}) = 
        do eDecls <- typecheckDecls decls
           let declNames = map fst eDecls
               declTypes = map (AST.getType . snd) eDecls
           when (any isNullType declTypes) $ 
                tcError $ "Cannot infer type of null-valued expression"
           eBody <- local (extendEnvironment (zip declNames declTypes)) $ pushTypecheck body
           return $ setType (AST.getType eBody) let_ {decls = eDecls, body = eBody}
        where
          typecheckDecls [] = return []
          typecheckDecls ((name, expr):decls) = 
              do eExpr <- pushTypecheck expr
                 eDecls <- local (extendEnvironment [(name, AST.getType eExpr)]) $ typecheckDecls decls
                 return $ (name, eExpr):eDecls

    --  E |- en : t
    -- ------------------------
    --  E |- {e1; ..; en} : t
    typecheck seq@(Seq {eseq}) = 
        do eEseq <- mapM pushTypecheck eseq 
           let seqType = AST.getType (last eEseq)
           return $ setType seqType seq {eseq = eEseq}

    --  E |- cond : bool
    --  E |- thn : t'
    --  E |- els : t''
    --  t = matchBranches(t', t'')
    -- ------------------------------------
    --  E |- if cond then thn else els : t
    typecheck ifThenElse@(IfThenElse {cond, thn, els}) = 
        do eCond <- pushHasType cond boolType
           eThn <- pushTypecheck thn
           eEls <- pushTypecheck els
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
    typecheck while@(While {cond, body}) = 
        do eCond <- pushHasType cond boolType
           eBody <- pushTypecheck body
           return $ setType (AST.getType eBody) while {cond = eCond, body = eBody}

    --  E |- val : Fut t
    -- ------------------
    --  E |- get val : t
    typecheck get@(Get {val}) = 
        do eVal <- pushTypecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty) $ 
                  tcError $ "Cannot get the value of non-future type '" ++ show ty ++ "'"
           return $ setType (getResultType ty) get {val = eVal}

    --    ------------------ :: suspend
    --    suspend : void
    typecheck suspend@(Suspend {}) = 
        do return $ setType voidType suspend 

    --    f : Fut T
    --    ------------------ :: await
    --    await f : void
    typecheck await@(Await {val}) = 
        do eVal <- pushTypecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty) $ 
                  tcError $ "Cannot await the value of non-future type '" ++ show ty ++ "'"
           return $ setType voidType await {val = eVal}

    --    f : Fut T
    --    c : T -> T'
    --    ------------------ :: chain
    --    f then c : Fut T'
    typecheck futureChain@(FutureChain {future, chain}) = 
        do eFuture <- pushTypecheck future
           eChain <- pushTypecheck chain
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
    typecheck fAcc@(FieldAccess {target, name}) = 
        do eTarget <- pushTypecheck target
           let targetType = AST.getType eTarget
           when (isPrimitive targetType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of primitive type '" ++ show targetType ++ "'"
           when (isTypeVar targetType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of polymorphic type '" ++ show targetType ++ "'"
           when (isActiveRefType targetType && (not $ isThisAccess eTarget)) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of active object type '" ++ show targetType ++ "'"
           fType <- asks $ fieldLookup targetType name
           case fType of
             Just ty -> do let actualTypeParams = getTypeParameters targetType
                           formalTypeParams <- asks $ classTypeParameterLookup targetType
                           bindings <- matchTypeParameters formalTypeParams actualTypeParams
                           let ty' = replaceTypeVars bindings ty
                           return $ setType ty' fAcc {target = eTarget}
             Nothing -> tcError $ "No field '" ++ show name ++ "' in class '" ++ show targetType ++ "'"

    --  E |- lhs : t
    --  isLval(lhs)
    --  E |- rhs : t
    -- ------------------------
    --  E |- name = rhs : void
    typecheck assign@(Assign {lhs = lhs@VarAccess{name}, rhs}) = 
        do eLhs <- pushTypecheck lhs
           isLocal <- asks $ isLocal name
           unless isLocal $ 
                  tcError $ "Left hand side '" ++ show (ppExpr lhs) ++ 
                            "' is a global variable and cannot be assigned to"
           eRhs <- pushHasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}
    typecheck assign@(Assign {lhs, rhs}) = 
        do eLhs <- pushTypecheck lhs
           unless (isLval lhs) $ 
                  tcError $ "Left hand side '" ++ show (ppExpr lhs) ++ "' cannot be assigned to"
           eRhs <- pushHasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}

    --  name : t \in E
    -- ----------------
    --  E |- name : t
    typecheck var@(VarAccess {name}) = 
        do varType <- asks $ varLookup name
           case varType of
             Just ty -> return $ setType ty var
             Nothing -> tcError $ "Unbound variable '" ++ show name ++ "'"

    --
    -- ----------------------
    --  E |- null : nullType
    typecheck null@Null {} = return $ setType nullType null

    --
    -- ------------------
    --  E |- true : bool
    typecheck true@BTrue {} = return $ setType boolType true 

    --
    -- ------------------
    --  E |- false : bool
    typecheck false@BFalse {} = return $ setType boolType false 

   ---  |- t
    --  classLookup(ty) = _
    --  ty != Main
    -- ----------------------
    --  E |- new ty : ty
    typecheck new@(New {ty}) = 
        do ty' <- checkType ty
           unless (isRefType ty') $ tcError $ "Cannot create an object of type '" ++ show ty ++ "'"
           when (isMainType ty') $ tcError "Cannot create additional Main objects"
           return $ setType ty' new

    --  count("{}", stringLit) = n
    --  E |- arg1 : t1 .. E |- argn : tn
    -- ---------------------------------------------
    --  E |- print(stringLit, arg1 .. argn) : void
    typecheck print@(Print {stringLit, args}) =
        do let noArgs = T.count (T.pack "{}") (T.pack stringLit)
           unless (noArgs == length args) $
                  tcError $ "Wrong number of arguments to format string. " ++
                            "Expected " ++ show (length args) ++ ", got " ++ show noArgs ++ "."
           eArgs <- mapM pushTypecheck args
           return $ setType voidType print {args = eArgs}

    --  E |- arg : int
    -- ------------------------
    --  E |- exit(arg) : void
    typecheck exit@(Exit {args}) = 
        do eArgs <- mapM pushTypecheck args
           unless (length eArgs == 1 && (isIntType $ AST.getType (head eArgs))) $
                  tcError $ "exit expects a single integer argument"
           return $ setType voidType exit {args = eArgs}

    typecheck stringLit@(StringLiteral {}) = return $ setType stringType stringLit

    typecheck intLit@(IntLiteral {}) = return $ setType intType intLit

    typecheck realLit@(RealLiteral {}) = return $ setType realType realLit

   ---  |- ty
    -- ---------------------
    -- E |- embed ty _ : ty
    typecheck embed@(Embed {ty}) = 
        do eTy <- checkType ty
           return $ setType eTy embed

    --  E |- operand : bool
    -- -------------------------
    --  E |- not operand : bool
    typecheck unary@(Unary {op, operand})
      | op == (Identifiers.NOT) = do
        eOperand <- pushTypecheck operand
        let eType = AST.getType eOperand
        unless (isBoolType eType) $
                tcError $ "Operator '" ++ show op ++ "' is only defined for boolean types"
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
    typecheck binop@(Binop {op, loper, roper})
      | op `elem` boolOps = do
          eLoper <- pushTypecheck loper
          eRoper <- pushTypecheck roper
          let lType = AST.getType eLoper
              rType = AST.getType eRoper
          unless (isBoolType lType && isBoolType rType) $
                  tcError $ "Operator '"++ show op ++ "' is only defined for boolean types"
          return $ setType boolType binop {loper = eLoper, roper = eRoper}
      | op `elem` cmpOps =
          do eLoper <- pushTypecheck loper
             eRoper <- pushTypecheck roper
             let lType = AST.getType eLoper
                 rType = AST.getType eRoper
             unless (isNumeric lType && isNumeric rType) $
                    tcError $ "Operator "++ show op ++ " is only defined for numeric types"
             return $ setType boolType binop {loper = eLoper, roper = eRoper}
      | op `elem` eqOps =
          do eLoper <- pushTypecheck loper
             eRoper <- pushHasType roper (AST.getType eLoper)
             return $ setType boolType binop {loper = eLoper, roper = eRoper}
      | op `elem` arithOps =
          do eLoper <- pushTypecheck loper
             eRoper <- pushTypecheck roper
             let lType = AST.getType eLoper
                 rType = AST.getType eRoper
             unless (isNumeric lType && isNumeric rType) $
                    tcError $ "Operator "++ show op ++ " is only defined for numeric types"
             return $ setType (coerceTypes lType rType) binop {loper = eLoper, roper = eRoper}
      | otherwise = tcError $ "Undefined binary operator '" ++ show op ++ "'"
      where
        boolOps  = [Identifiers.AND, Identifiers.OR]
        cmpOps   = [Identifiers.LT, Identifiers.GT, Identifiers.LTE, Identifiers.GTE]
        eqOps    = [Identifiers.EQ, NEQ]
        arithOps = [PLUS, MINUS, TIMES, DIV, MOD]
        coerceTypes ty1 ty2
            | isRealType ty1 = realType
            | isRealType ty2 = realType
            | otherwise = intType

    typecheck e = error $ "Cannot typecheck expression " ++ (show $ ppExpr e)

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
matchArguments (arg:args) (typ:types) = do eArg <- pushTypecheck arg
                                           bindings <- matchTypes typ (AST.getType eArg)
                                           (eArgs, bindings') <- local (bindTypes bindings) $ matchArguments args types
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
      isParType expected    && isParType ty   = matchTypes (getResultType expected) (getResultType ty)
    | isArrowType expected  && isArrowType ty = let expArgTypes = getArgTypes expected
                                                    argTypes    = getArgTypes ty
                                                    expRes      = getResultType expected
                                                    resTy       = getResultType ty
                                                in
                                                  do argBindings <- matchArguments expArgTypes argTypes
                                                     local (bindTypes argBindings) $ matchTypes expRes resTy
    | isTypeVar expected = do result <- asks $ typeVarLookup expected
                              case result of
                                Just boundType -> 
                                    do unless (ty `subtypeOf` boundType) $
                                              tcError $ "Type variable '" ++ show expected ++ 
                                                        "' cannot be bound to both '" ++ show ty ++ 
                                                        "' and '" ++ show boundType ++ "'"
                                       asks bindings
                                Nothing -> 
                                    do bindings <- asks bindings
                                       return $ (expected, ty) : bindings
    | otherwise = do assertSubtypeOf ty expected
                     asks bindings
    where
      matchArguments [] [] = asks bindings
      matchArguments (ty1:types1) (ty2:types2) = do bindings <- matchTypes ty1 ty2
                                                    local (bindTypes bindings) $ matchArguments types1 types2

matchTypeParameters :: [Type] -> [Type] -> ExceptT TCError (Reader Environment) [(Type, Type)]
matchTypeParameters formals params = do bindings <- mapM (uncurry matchTypes) $ zip formals params
                                        return $ concat bindings

assertSubtypeOf :: Type -> Type -> ExceptT TCError (Reader Environment) ()
assertSubtypeOf sub super = 
    unless (sub `subtypeOf` super) $ 
           tcError $ "Type '" ++ show sub ++ 
                     "' does not match expected type '" ++ show super ++ "'"
