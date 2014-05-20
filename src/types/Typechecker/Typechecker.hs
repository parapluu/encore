{-# LANGUAGE MultiParamTypeClasses #-}
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
import Typechecker.Types
import Typechecker.Environment
import Typechecker.TypeError
--import qualified EAST.EAST as Ext

-- | The top-level type checking function
typecheckEncoreProgram :: Program -> Either TCError Program
typecheckEncoreProgram p = runReader (runErrorT (typecheck p)) (buildClassTable p)

-- | Convenience function for throwing an exception with the
-- current backtrace
tcError msg = do bt <- asks backtrace
                 throwError $ TCError (msg, bt)

-- | Convenience function for checking if a type is well-formed
wfType :: Type -> ErrorT TCError (Reader Environment) ()
wfType ty = do refType <- asks $ classLookup ty
               unless (isPrimitive ty || isJust refType) $ tcError $ "Unknown type '" ++ show ty ++ "'"

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
    typecheck c@(Class {cname = cname, fields = fields, methods = methods}) =
        do efields <- mapM pushTypecheck fields
           emethods <- mapM typecheckMethod methods
           unless distinctFieldNames $ tcError $ "Duplicate field names"
           unless distinctMethodNames $ tcError $ "Duplicate method names"
           return $ c {fields = efields, methods = emethods}
        where
          typecheckMethod m = local (extendEnvironment [(thisName, cname)]) $ pushTypecheck m
          distinctFieldNames = 
              nubBy (\f1 f2 -> (fname f1 == fname f2)) fields == fields
          distinctMethodNames = 
              nubBy (\m1 m2 -> (mname m1 == mname m2)) methods == methods

instance Checkable FieldDecl where
    typecheck f@(Field {ftype = ty}) = do wfType ty
                                          return $ setType ty f

instance Checkable MethodDecl where
     typecheck m@(Method {mmeta = meta, rtype = rtype, mparams=params, mbody = body}) = 
         do wfType rtype
            mapM_ typecheckParam params
            eBody <- local addParams $ pushHasType body rtype
            return $ setType rtype m {mbody = eBody}
         where
           typecheckParam = (\p@(Param(_, ty)) -> local (pushBT p) $ do {wfType ty; return $ p})
           addParams = extendEnvironment (map (\(Param p) -> p) params)

instance Checkable Expr where
    hasType expr ty = do eExpr <- typecheck expr
                         unless (eExpr `AST.hasType` ty) $
                                tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show (AST.getType eExpr) ++ "'"
                         return eExpr

    typecheck skip@(Skip {}) = return $ setType voidType skip

    typecheck call@(Call {target = target, tmname = name, args = args}) = 
        do eTarget <- pushTypecheck target
           targetType <- return $ AST.getType eTarget
           when (isPrimitive targetType) $ 
                tcError $ "Cannot call method on expression '" ++ 
                          (show $ ppExpr target) ++ 
                          "' of primitive type '" ++ show targetType ++ "'"
           lookupResult <- asks $ methodLookup targetType name
           case lookupResult of
             Nothing -> tcError $ "No method '" ++ show name ++ "' in class '" ++ show targetType ++ "'"
             Just (returnType, params) -> 
                 do unless (length args == length params) $ 
                       tcError $ "Method '" ++ show name ++ "' of class '" ++ show targetType ++
                                 "' expects " ++ show (length params) ++ " arguments. Got " ++ show (length args)
                    eArgs <- zipWithM (\eArg (Param (_, ty)) -> pushHasType eArg ty) args params
                    return $ setType returnType call {target = eTarget, args = eArgs}

    typecheck let_@(Let {eid = x, ty = ty, val = val, body = body}) = 
        do eVal <- pushHasType val ty
           eBody <- local (extendEnvironment [(x, ty)]) $ pushTypecheck body
           return $ setType (AST.getType eBody) let_ {val = eVal, body = eBody}

    typecheck seq@(Seq {eseq = exprs}) = 
        do eExprs <- mapM pushTypecheck exprs 
           seqType <- return $ AST.getType (last eExprs)
           return $ setType seqType seq{eseq = eExprs}

    typecheck ifThenElse@(IfThenElse {cond = cond, thn = thn, els = els}) = 
        do eCond <- pushHasType cond boolType
           eThn <- pushTypecheck thn
           thnType <- return $ AST.getType eThn
           eEls <- pushHasType els thnType
           return $ setType thnType ifThenElse {cond = eCond, thn = eThn, els = eEls}

    typecheck while@(While {cond = cond, body = expr}) = 
        do eCond <- pushHasType cond boolType
           eExpr <- pushTypecheck expr
           return $ setType (AST.getType eExpr) while {cond = eCond, body = eExpr}

    typecheck get@(Get {}) = mzero

    typecheck fAcc@(FieldAccess {path = expr, field = f}) = 
        do ePath <- pushTypecheck expr
           pathType <- return $ AST.getType ePath
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr expr) ++ "' of primitive type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup pathType f
           case fType of
             Just ty -> return $ setType ty fAcc {path = ePath}
             Nothing -> tcError $ "No field '" ++ show f ++ "' in class '" ++ show pathType ++ "'"                                                         

    typecheck assign@(Assign {lhs = lval, rhs = rval}) = 
        do eLVal <- pushTypecheck lval
           eRVal <- pushHasType rval (AST.getType eLVal)
           return $ setType voidType assign {lhs = eLVal, rhs = eRVal}

    typecheck var@(VarAccess {eid = x}) = 
        do varType <- asks $ varLookup x
           case varType of
             Just ty -> return $ setType ty var
             Nothing -> tcError $ "Unbound variable '" ++ show x ++ "'"

    typecheck null@Null {} = return $ setType nullType null

    typecheck true@BTrue {} = return $ setType boolType true 

    typecheck false@BFalse {} = return $ setType boolType false 

    typecheck new@(New {ty = ty}) = 
        do wfType ty
           return $ setType ty new

    typecheck print@(Print {ty = ty, val = expr}) = 
        do eExpr <- pushHasType expr ty
           return $ setType voidType print {val = eExpr}

    typecheck stringLit@(StringLiteral {}) = return $ setType stringType stringLit

    typecheck intLit@(IntLiteral {}) = return $ setType intType intLit

    typecheck binop@(Binop {op = op, loper = e1, roper = e2})
        | op `elem` cmpOps = 
            do eE1 <- pushHasType e1 intType
               eE2 <- pushHasType e2 intType
               return $ setType boolType binop {loper = eE1, roper = eE2}
        | op `elem` eqOps =
            do eE1 <- pushTypecheck e1
               eE2 <- pushHasType e2 (AST.getType eE1)
               return $ setType boolType binop {loper = eE1, roper = eE2}
        | op `elem` arithOps = 
            do eE1 <- pushHasType e1 intType
               eE2 <- pushHasType e2 intType
               return $ setType intType binop {loper = eE1, roper = eE2}
        | otherwise = tcError $ "Undefined binary operator '" ++ show op ++ "'"
        where
          cmpOps   = [Identifiers.LT, Identifiers.GT]
          eqOps    = [Identifiers.EQ, NEQ]
          arithOps = [PLUS, MINUS, TIMES, DIV]

instance Checkable LVal where
    hasType lval ty = do eLVal <- typecheck lval
                         unless (eLVal `AST.hasType` ty) $ 
                                tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show (AST.getType eLVal) ++ "'"
                         return eLVal

    typecheck lval@(LVal {lid = x}) = 
        do varType <- asks (varLookup x)
           case varType of
             Just ty -> return $ setType ty lval
             Nothing -> tcError $ "Unbound variable '" ++ show x ++ "'"
    typecheck lval@(LField {lpath = expr, lid = f}) = 
        do ePath <- typecheck expr
           pathType <- return $ AST.getType ePath
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ (show $ ppExpr expr) ++ 
                          "' of primitive type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup (AST.getType ePath) f
           case fType of
             Just ty -> return $ setType ty lval {lpath = ePath}
             Nothing -> tcError $ "No field '" ++ show f ++ "' in class '" ++ show pathType ++ "'"