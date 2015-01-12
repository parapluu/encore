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
    | isTypeVar ty = return ty
    | isRefType ty = do result <- asks $ classActivityLookup ty
                        case result of
                          Nothing -> tcError $ "Unknown type '" ++ show ty ++ "'"
                          Just refType -> return refType -- This will be ty with activity information
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
    -- | Returns the extended version of its argument
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
    typecheck (Program etl imps funs classes) = 
        do efuns <- mapM pushTypecheck funs
           eclasses <- mapM pushTypecheck classes
           return $ Program etl imps efuns eclasses

instance Checkable Function where
    typecheck f@(Function {funtype, funparams, funbody, funname}) = 
        do ty <- checkType funtype
           noFreeTypeVariables
           when (funname == Name "main") checkMainParams
           eParams <- mapM typecheckParam funparams
           eBody <- local (addParams eParams) $ pushHasType funbody ty
           return $ setType ty f {funtype = ty, funbody = eBody, funparams = eParams}
        where
          noFreeTypeVariables = 
              let retVars = nub $ filter isTypeVar $ typeComponents funtype 
                  paramVars = nub $ filter isTypeVar $ concatMap (\(Param{ptype}) -> typeComponents ptype) funparams
              in
                when (not . null $ retVars \\ paramVars) $
                     tcError $ "Free type variables in return type '" ++ show funtype ++ "'"
          checkMainParams = unless ((map ptype funparams) `elem` [[] {-, [intType, arrayType stringType]-}]) $ 
                              tcError $
                                "Main function must have argument type () or (int, string[]) (but arrays are not supported yet)"
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $ 
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

instance Checkable ClassDecl where
    typecheck c@(Class {cname, fields, methods}) =
        do distinctFieldNames
           efields <- mapM pushTypecheck fields
           distinctMethodNames
           emethods <- mapM typecheckMethod methods
           return $ setType cname c {fields = efields, methods = emethods}
        where
          typecheckMethod m = local (extendEnvironment [(thisName, cname)]) $ pushTypecheck m
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
    typecheck f@(Field {ftype}) = do ty <- checkType ftype
                                     let types = typeComponents ty
                                     when (any isTypeVar types) $ 
                                          tcError $ "Free type variables in field type"
                                     return $ setType ty f
  

instance Checkable MethodDecl where
    typecheck m@(Method {mtype, mparams, mbody, mname}) = 
        do ty <- checkType mtype
           noFreeTypeVariables
           Just thisType <- asks $ varLookup thisName
           when (isMainType thisType && mname == Name "main") checkMainParams
           eMparams <- mapM typecheckParam mparams
           eBody <- local (addParams eMparams) $ pushHasType mbody ty
           return $ setType ty m {mtype = ty, mbody = eBody, mparams = eMparams}
        where
          noFreeTypeVariables = 
              let retVars = nub $ filter isTypeVar $ typeComponents mtype 
                  paramVars = nub $ filter isTypeVar $ concatMap (\(Param{ptype}) -> typeComponents ptype) mparams
              in
                when (not . null $ retVars \\ paramVars) $
                     tcError $ "Free type variables in return type '" ++ show mtype ++ "'"
          checkMainParams = unless ((map ptype mparams) `elem` [[] {-, [intType, arrayType stringType]-}]) $ 
                              tcError $
                                "Main method must have argument type () or (int, string[]) (but arrays are not supported yet)"
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $ 
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

instance Checkable Expr where
    hasType expr ty = do eExpr <- pushTypecheck expr
                         let exprType = AST.getType eExpr
                         if isNullType exprType then
                             coerceNull eExpr ty
                         else
                             do matchTypes ty exprType
                                return eExpr
        where
          coerceNull null ty 
              | isNullType ty || 
                isTypeVar ty = tcError "Cannot infer type of null valued expression"
              | isRefType ty = return $ setType ty null
              | otherwise = tcError $ "Null valued expression cannot have type '" ++ show ty ++ "' (must have reference type)"

    -- 
    -- --------------
    --   () : void
    typecheck skip@(Skip {}) = return $ setType voidType skip

    --    e : t
    -- --------------
    --   (e : t) : t
    typecheck tExpr@(TypedExpr {body, ty}) = do ty' <- checkType ty
                                                pushHasType body ty'

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
           (eArgs, bindings) <- checkArguments args paramTypes
           returnType <- 
               do ty <- if isTypeVar methodType then
                            case lookup methodType bindings of
                              Just ty -> return ty
                              Nothing -> tcError $ "Could not resolve return type '" ++ show methodType ++ "'"
                        else
                            return methodType
                  return $ if isThisAccess target || (not $ isActiveRefType targetType) 
                           then ty
                           else futureType ty
           return $ setType returnType mcall {target = eTarget, args = eArgs}

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
           (eArgs, bindings) <- checkArguments args paramTypes
           return $ setType voidType msend {target = eTarget, args = eArgs}

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
           (eArgs, bindings) <- checkArguments args argTypes
           let resultType = replaceTypeVars bindings (getResultType ty)
           return $ setType resultType fcall {args = eArgs}

    typecheck closure@(Closure {eparams, body}) = 
        do eEparams <- mapM typecheckParam eparams
           eBody <- local (addParams eEparams) $ pushTypecheck body
           let returnType = AST.getType eBody
           when (isNullType returnType) $ 
                tcError $ "Cannot infer return type of closure with null-valued body"
           return $ setType (arrowType (map ptype eparams) returnType) closure {body = eBody, eparams = eEparams}
        where
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          onlyParams = replaceLocals $ map (\(Param {pname, ptype}) -> (pname, ptype)) eparams
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params
           
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

    typecheck seq@(Seq {eseq}) = 
        do eEseq <- mapM pushTypecheck eseq 
           let seqType = AST.getType (last eEseq)
           return $ setType seqType seq {eseq = eEseq}

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

    typecheck while@(While {cond, body}) = 
        do eCond <- pushHasType cond boolType
           eBody <- pushTypecheck body
           return $ setType (AST.getType eBody) while {cond = eCond, body = eBody}

    typecheck get@(Get {val}) = 
        do eVal <- pushTypecheck val
           let ty = AST.getType eVal
           unless (isFutureType ty) $ 
                  tcError $ "Cannot get the value of non-future type '" ++ show ty ++ "'"
           return $ setType (getResultType ty) get {val = eVal}

    typecheck fAcc@(FieldAccess {target, name}) = 
        do eTarget <- pushTypecheck target
           let pathType = AST.getType eTarget
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of primitive type '" ++ show pathType ++ "'"
           when (isTypeVar pathType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of polymorphic type '" ++ show pathType ++ "'"
           when (isActiveRefType pathType && (not $ isThisAccess eTarget)) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of active object type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup pathType name
           case fType of
             Just ty -> return $ setType ty fAcc {target = eTarget}
             Nothing -> tcError $ "No field '" ++ show name ++ "' in class '" ++ show pathType ++ "'"

    typecheck assign@(Assign {lhs = lhs@VarAccess{name}, rhs}) = 
        do eLhs <- pushTypecheck lhs
           isLocal <- asks $ isLocal name
           unless isLocal $ 
                  tcError $ "Left hand side '" ++ show (ppExpr lhs) ++ "' is a global variable and cannot be assigned to"
           eRhs <- pushHasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}
    typecheck assign@(Assign {lhs, rhs}) = 
        do eLhs <- pushTypecheck lhs
           unless (isLval lhs) $ 
                  tcError $ "Left hand side '" ++ show (ppExpr lhs) ++ "' cannot be assigned to"
           eRhs <- pushHasType rhs (AST.getType eLhs)
           return $ setType voidType assign {lhs = eLhs, rhs = eRhs}

    typecheck var@(VarAccess {name}) = 
        do varType <- asks $ varLookup name
           case varType of
             Just ty -> return $ setType ty var
             Nothing -> tcError $ "Unbound variable '" ++ show name ++ "'"

    typecheck null@Null {} = return $ setType nullType null

    typecheck true@BTrue {} = return $ setType boolType true 

    typecheck false@BFalse {} = return $ setType boolType false 

    typecheck new@(New {ty}) = 
        do ty' <- checkType ty
           unless (isRefType ty') $ tcError $ "Cannot create an object of type '" ++ show ty ++ "'"
           when (isMainType ty') $ tcError "Cannot create additional Main objects"
           return $ setType ty' new

    typecheck print@(Print {stringLit, args}) =
        do let noArgs = T.count (T.pack "{}") (T.pack stringLit)
           unless (noArgs == length args) $
                  tcError $ "Wrong number of arguments to format string. " ++
                            "Expected " ++ show (length args) ++ ", got " ++ show noArgs ++ "."
           eArgs <- mapM pushTypecheck args
           return $ setType voidType print {args = eArgs}

    typecheck exit@(Exit {args}) = 
        do eArgs <- mapM pushTypecheck args
           unless (length eArgs == 1 && (isIntType $ AST.getType (head eArgs))) $
                  tcError $ "exit expects a single integer argument"
           return $ setType voidType exit {args = eArgs}

    typecheck stringLit@(StringLiteral {}) = return $ setType stringType stringLit

    typecheck intLit@(IntLiteral {}) = return $ setType intType intLit

    typecheck realLit@(RealLiteral {}) = return $ setType realType realLit

    typecheck embed@(Embed {ty}) = 
        do eTy <- checkType ty
           return $ setType eTy embed

    typecheck unary@(Unary {op, operand})
      | op == (Identifiers.NOT) = do
        eOperand <- pushTypecheck operand
        let eType = AST.getType eOperand
        unless (isBoolType eType) $
                tcError $ "Operator '" ++ show op ++ "' is only defined for boolean types"
        return $ setType boolType unary { operand = eOperand }

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
        boolOps   = [Identifiers.AND, Identifiers.OR]
        cmpOps   = [Identifiers.LT, Identifiers.GT, Identifiers.LTE, Identifiers.GTE]
        eqOps    = [Identifiers.EQ, NEQ]
        arithOps = [PLUS, MINUS, TIMES, DIV, MOD]
        coerceTypes ty1 ty2
            | isRealType ty1 = realType
            | isRealType ty2 = realType
            | otherwise = intType
    typecheck e = error $ "Cannot typecheck expression " ++ (show $ ppExpr e)

checkArguments :: [Expr] -> [Type] -> ExceptT TCError (Reader Environment) ([Expr], [(Type, Type)])
checkArguments [] [] = do bindings <- asks bindings
                          return ([], bindings)
checkArguments (arg:args) (typ:types) = do eArg <- pushHasType arg typ
                                           bindings <- matchTypes typ (AST.getType eArg)
                                           (eArgs, bindings') <- local (bindTypes bindings) $ checkArguments args types
                                           return (eArg:eArgs, bindings')

matchTypes :: Type -> Type -> ExceptT TCError (Reader Environment) [(Type, Type)]
matchTypes ty1 ty2 
    | isFutureType ty1 && isFutureType ty2 ||
      isParType ty1    && isParType ty2 = matchTypes (getResultType ty1) (getResultType ty2)
    | isArrowType ty1 && isArrowType ty2 = do let argTypes1 = getArgTypes ty1
                                                  argTypes2 = getArgTypes ty2
                                                  res1 = getResultType ty1
                                                  res2 = getResultType ty2
                                              argBindings <- matchArguments argTypes1 argTypes2
                                              local (bindTypes argBindings) $ matchTypes res1 res2
    | isTypeVar ty1 = do boundType <- asks $ typeVarLookup ty1
                         case boundType of 
                           Just ty -> do unless (ty `subtypeOf` ty2) $ 
                                                tcError $ "Type variable '" ++ show ty1 ++ "' cannot be bound to both '" ++ 
                                                     show ty ++ "' and '" ++ show ty2 ++ "'"
                                         asks bindings
                           Nothing -> do bindings <- asks bindings
                                         return ((ty1, ty2):bindings)
    | otherwise = do unless (ty1 `subtypeOf` ty2) $ tcError $ "Type '" ++ show ty2 ++ "' does not match expected type '" ++ show ty1 ++ "'"
                     asks bindings
    where
      matchArguments [] [] = asks bindings
      matchArguments (ty1:types1) (ty2:types2) = do bindings <- matchTypes ty1 ty2
                                                    local (bindTypes bindings) $ matchArguments types1 types2