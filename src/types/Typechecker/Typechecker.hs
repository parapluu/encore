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
import Control.Monad.Reader
import Control.Monad.Error

-- Module dependencies
import Identifiers
import AST.AST hiding (hasType, getType)
import qualified AST.AST as AST(hasType, getType)
import AST.PrettyPrinter
import Types
import Typechecker.Environment
import Typechecker.TypeError

-- | The top-level type checking function
typecheckEncoreProgram :: Program -> Either TCError Program
typecheckEncoreProgram p = case buildClassTable p of
                             Right env -> runReader (runErrorT (typecheck p)) env
                             Left err -> Left err

-- | Convenience function for throwing an exception with the
-- current backtrace
tcError msg = do bt <- asks backtrace
                 throwError $ TCError (msg, bt)

-- | Convenience function for checking if a type is
-- well-formed. Returns the same type with correct activity
-- information.
checkType :: Type -> ErrorT TCError (Reader Environment) Type
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

-- | The actual typechecking is done using a Reader monad wrapped
-- in an Error monad. The Reader monad lets us do lookups in the
-- "Environment", and the Error monad lets us throw a
-- "TCError" exception anywhere.
class Checkable a where
    -- | Returns the extended version of its argument
    typecheck :: a -> ErrorT TCError (Reader Environment) a

    -- | Returns the extended version of its argument if its type
    -- agrees with the second argument
    hasType   :: a -> Type -> ErrorT TCError (Reader Environment) a
    hasType _ _ = tcError "Typechecking not implemented for construct"

    -- | Convenience function for pushing and typechecking a
    -- component in one step.
    pushTypecheck :: Pushable a => a -> ErrorT TCError (Reader Environment) a
    pushTypecheck x = local (pushBT x) $ typecheck x

    pushHasType :: Pushable a => a -> Type -> ErrorT TCError (Reader Environment) a
    pushHasType x ty = local (pushBT x) $ hasType x ty

instance Checkable Program where
    typecheck (Program classes) = do eclasses <- mapM pushTypecheck classes
                                     return $ Program eclasses

instance Checkable ClassDecl where
    typecheck c@(Class {cname, fields, methods}) =
        do efields <- mapM pushTypecheck fields
           emethods <- mapM typecheckMethod methods
           distinctFieldNames
           distinctMethodNames
           return $ setType cname c {fields = efields, methods = emethods}
        where
          typecheckMethod m = local (extendEnvironment [(thisName, cname)]) $ pushTypecheck m
          distinctFieldNames = 
              case fields \\ nubBy (\f1 f2 -> (fname f1 == fname f2)) fields of
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
                                     when (any isTypeVar types) $ tcError $ "Free type variables in field type"
                                     return $ setType ty f

instance Checkable MethodDecl where
    typecheck m@(Method {mtype, mparams, mbody}) = 
        do ty <- checkType mtype
           noFreeTypeVariables
           eMparams <- mapM typecheckParam mparams
           eBody <- local (addParams eMparams) $ pushHasType mbody ty
           return $ setType mtype m {mtype = ty, mbody = eBody, mparams = eMparams}
        where
          noFreeTypeVariables = 
              let retVars = nub $ filter isTypeVar $ typeComponents mtype 
                  paramVars = nub $ filter isTypeVar $ concatMap (\(Param{ptype}) -> typeComponents ptype) mparams
              in
                when (not . null $ retVars \\ paramVars) $
                     tcError $ "Free type variables in return type '" ++ show mtype ++ "'"
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $ 
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params

instance Checkable Expr where
    hasType expr ty = do eExpr <- pushTypecheck expr
                         exprType <- return $ AST.getType eExpr
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


    typecheck skip@(Skip {}) = return $ setType voidType skip

    typecheck tExpr@(TypedExpr {body, ty}) = do ty' <- checkType ty
                                                pushHasType body ty'

    typecheck mcall@(MethodCall {target, name, args}) = 
        do eTarget <- pushTypecheck target
           targetType <- return $ AST.getType eTarget
           unless (isRefType targetType) $ 
                tcError $ "Cannot call method on expression '" ++ 
                          (show $ ppExpr target) ++ 
                          "' of type '" ++ show targetType ++ "'"
           lookupResult <- asks $ methodLookup targetType name
           case lookupResult of
             Nothing -> tcError $ "No method '" ++ show name ++ "' in class '" ++ show targetType ++ "'"
             Just (params, returnType) -> 
                 do unless (length args == length params) $ 
                       tcError $ "Method '" ++ show name ++ "' of class '" ++ show targetType ++
                                 "' expects " ++ show (length params) ++ " arguments. Got " ++ show (length args)
                    (eArgs, bindings) <- checkArguments args (map (\(Param{ptype}) -> ptype) params) 
                    if isTypeVar returnType then
                        case lookup returnType bindings of
                          Just ty -> return $ setType ty mcall {target = eTarget, args = eArgs}
                          Nothing -> tcError $ "Could not resolve return type '" ++ show returnType ++ "'"
                    else
                        return $ setType returnType mcall {target = eTarget, args = eArgs}

    typecheck fcall@(FunctionCall {name, args}) = 
        do funType <- asks $ varLookup name
           case funType of
             Just ty -> do unless (isArrowType ty) $ 
                                  tcError $ "Cannot use value of type '" ++ show ty ++ "' as a function"
                           argTypes <- return $ getArgTypes ty
                           unless (length args == length argTypes) $ 
                                  tcError $ "Function '" ++ show name ++ "' of type '" ++ show ty ++
                                            "' expects " ++ show (length argTypes) ++ " arguments. Got " ++ show (length args)
                           (eArgs, bindings) <- checkArguments args argTypes
                           let resultType = replaceTypeVars bindings (getResultType ty)
                           return $ setType resultType fcall {args = eArgs}
             Nothing -> tcError $ "Unbound function variable '" ++ show name ++ "'"

    typecheck closure@(Closure {eparams, body}) = 
        do eEparams <- mapM typecheckParam eparams
           eBody <- local (addParams eEparams) $ pushTypecheck body
           returnType <- return $ AST.getType eBody
           when (isNullType returnType) $ 
                tcError $ "Cannot infer return type of closure with null-valued body"
           return $ setType (arrowType (map ptype eparams) returnType) closure {body = eBody, eparams = eEparams}
        where
          typecheckParam = (\p@(Param{ptype}) -> local (pushBT p) $
                                                 do ty <- checkType ptype
                                                    return $ setType ty p)
          onlyParams = replaceLocals $ map (\(Param {pname, ptype}) -> (pname, ptype)) eparams
          addParams params = extendEnvironment $ map (\(Param {pname, ptype}) -> (pname, ptype)) params
           
    typecheck let_@(Let {name, val, body}) = 
        do eVal <- pushTypecheck val
           valType <- return (AST.getType eVal)
           when (isNullType valType) $ 
                tcError $ "Cannot infer type of null-valued expression"
           eBody <- local (extendEnvironment [(name, valType)]) $ pushTypecheck body
           return $ setType (AST.getType eBody) let_ {val = eVal, body = eBody}

    typecheck seq@(Seq {eseq}) = 
        do eEseq <- mapM pushTypecheck eseq 
           seqType <- return $ AST.getType (last eEseq)
           return $ setType seqType seq {eseq = eEseq}

    typecheck ifThenElse@(IfThenElse {cond, thn, els}) = 
        do eCond <- pushHasType cond boolType
           eThn <- pushTypecheck thn
           thnType <- return $ AST.getType eThn
           eEls <- pushTypecheck els
           elsType <- return $ AST.getType eEls
           resultType <- matchBranches thnType elsType
           return $ setType resultType ifThenElse {cond = eCond, thn = setType resultType eThn, els = setType resultType eEls}
        where
          matchBranches ty1 ty2
              | isNullType ty1 && isNullType ty2 =
                  tcError $ "Cannot infer result type of if-statement"
              | isNullType ty1 = return ty2
              | isNullType ty2 = return ty1
              | otherwise = if ty2 == ty1 
                            then return ty1
                            else tcError $ "Type mismatch in different branches of if-statement"

    typecheck while@(While {cond, body}) = 
        do eCond <- pushHasType cond boolType
           eBody <- pushTypecheck body
           return $ setType (AST.getType eBody) while {cond = eCond, body = eBody}

    typecheck get@(Get {}) = mzero

    typecheck fAcc@(FieldAccess {target, name}) = 
        do eTarget <- pushTypecheck target
           pathType <- return $ AST.getType eTarget
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of primitive type '" ++ show pathType ++ "'"
           when (isTypeVar pathType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr target) ++ "' of polymorphic type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup pathType name
           case fType of
             Just ty -> return $ setType ty fAcc {target = eTarget}
             Nothing -> tcError $ "No field '" ++ show name ++ "' in class '" ++ show pathType ++ "'"

    typecheck assign@(Assign {lhs, rhs}) = 
        do eLhs <- pushTypecheck lhs
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
           return $ setType ty' new

    typecheck print@(Print {val}) = 
        do eVal <- pushTypecheck val
           return $ setType voidType print {val = eVal}

    typecheck stringLit@(StringLiteral {}) = return $ setType stringType stringLit

    typecheck intLit@(IntLiteral {}) = return $ setType intType intLit

    typecheck realLit@(RealLiteral {}) = return $ setType realType realLit

    typecheck embed@(Embed {ty}) = return $ setType ty embed

    typecheck binop@(Binop {op, loper, roper})
        | op `elem` cmpOps = 
            do eLoper <- pushTypecheck loper
               lType  <- return $ AST.getType eLoper
               eRoper <- pushTypecheck roper
               rType  <- return $ AST.getType eRoper
               unless (isNumeric lType && isNumeric rType) $
                      tcError $ "Operator "++ show op ++ " is only defined for numeric types"
               return $ setType boolType binop {loper = eLoper, roper = eRoper}
        | op `elem` eqOps =
            do eLoper <- pushTypecheck loper
               eRoper <- pushHasType roper (AST.getType eLoper)
               return $ setType boolType binop {loper = eLoper, roper = eRoper}
        | op `elem` arithOps = 
            do eLoper <- pushTypecheck loper
               lType  <- return $ AST.getType eLoper
               eRoper <- pushTypecheck roper
               rType  <- return $ AST.getType eRoper
               unless (isNumeric lType && isNumeric rType) $
                      tcError $ "Operator "++ show op ++ " is only defined for numeric types"
               return $ setType (coerceTypes lType rType) binop {loper = eLoper, roper = eRoper}
        | otherwise = tcError $ "Undefined binary operator '" ++ show op ++ "'"
        where
          cmpOps   = [Identifiers.LT, Identifiers.GT]
          eqOps    = [Identifiers.EQ, NEQ]
          arithOps = [PLUS, MINUS, TIMES, DIV]
          coerceTypes ty1 ty2 
              | isRealType ty1 = realType
              | isRealType ty2 = realType
              | otherwise = intType

checkArguments :: [Expr] -> [Type] -> ErrorT TCError (Reader Environment) ([Expr], [(Type, Type)])
checkArguments [] [] = do bindings <- asks bindings
                          return ([], bindings)
checkArguments (arg:args) (typ:types) = do eArg <- pushHasType arg typ
                                           bindings <- matchTypes typ (AST.getType eArg)
                                           (eArgs, bindings') <- local (bindTypes bindings) $ checkArguments args types
                                           return (eArg:eArgs, bindings')

matchTypes :: Type -> Type -> ErrorT TCError (Reader Environment) [(Type, Type)]
matchTypes ty1 ty2 
    | isFutureType ty1 && isFutureType ty2 ||
      isParType ty1    && isParType ty2 = matchTypes (getResultType ty1) (getResultType ty2)
    | isArrowType ty1 && isArrowType ty2 = do argTypes1 <- return $ getArgTypes ty1
                                              argTypes2 <- return $ getArgTypes ty2
                                              argBindings <- matchArguments argTypes1 argTypes2
                                              res1  <- return $ getResultType ty1
                                              res2  <- return $ getResultType ty2
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

instance Checkable LVal where
    hasType lval ty = do eLVal <- typecheck lval
                         unless (eLVal `AST.hasType` ty) $ 
                                tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show (AST.getType eLVal) ++ "'"
                         return eLVal

    typecheck lval@(LVal {lname}) = 
        do varType <- asks (varLookup lname)
           case varType of
             Just ty -> return $ setType ty lval
             Nothing -> tcError $ "Unbound variable '" ++ show lname ++ "'"
    typecheck lval@(LField {ltarget, lname}) = 
        do eTarget <- typecheck ltarget
           pathType <- return $ AST.getType eTarget
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ (show $ ppExpr ltarget) ++ 
                          "' of primitive type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup (AST.getType eTarget) lname
           case fType of
             Just ty -> return $ setType ty lval {ltarget = eTarget}
             Nothing -> tcError $ "No field '" ++ show lname ++ "' in class '" ++ show pathType ++ "'"



