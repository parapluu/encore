{-# LANGUAGE MultiParamTypeClasses #-}
{-|

Typechecks an "AST.AST" and produces a meaningful error message if it
fails. 

-}

module Typechecker.Typechecker(typecheckEncoreProgram) where

-- Library dependencies
import Data.Maybe
import Data.List
import Control.Monad.Reader
import Control.Monad.Error

-- Module dependencies
import Identifiers
import AST.AST
import AST.PrettyPrinter
import Typechecker.Types
import Typechecker.Environment
import Typechecker.TypeError
import qualified EAST.EAST as Ext

-- | The top-level type checking function
typecheckEncoreProgram :: Program -> Either TCError Ext.Program
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
class Checkable a b where
    -- | Returns the extended version of its argument
    typecheck :: a -> ErrorT TCError (Reader Environment) b

    -- | Returns the extended version of its argument if its type
    -- agrees with the second argument
    hasType   :: a -> Type -> ErrorT TCError (Reader Environment) b

    -- | Convenience function for pushing and typechecking a
    -- component in one step.
    pushTypecheck :: Pushable a => a -> ErrorT TCError (Reader Environment) b
    pushTypecheck x = local (pushBT x) $ typecheck x

    pushHasType :: Pushable a => a -> Type -> ErrorT TCError (Reader Environment) b
    pushHasType x ty = local (pushBT x) $ hasType x ty

instance Checkable Program Ext.Program where
    typecheck (Program classes) = do eclasses <- mapM pushTypecheck classes
                                     return $ Ext.Program eclasses
    hasType _ _ = tcError "Trying to compare the type of a whole program"

instance Checkable ClassDecl Ext.ClassDecl where
    typecheck (Class cname fields methods) =
        do efields <- mapM pushTypecheck fields
           emethods <- mapM typecheckMethod methods
           unless distinctFieldNames $ tcError $ "Duplicate field names"
           unless distinctMethodNames $ tcError $ "Duplicate method names"
           return $ Ext.Class cname efields emethods
        where
          typecheckMethod m = local (extendEnvironment [(thisName, cname)]) $ pushTypecheck m
          distinctFieldNames = 
              nubBy (\f1 f2 -> (fname f1 == fname f2)) fields == fields
          distinctMethodNames = 
              nubBy (\m1 m2 -> (mname m1 == mname m2)) methods == methods

    hasType cls ty = do wfType ty
                        eCls@(Ext.Class cname fields methods) <- pushTypecheck cls
                        unless (ty == cname) $ tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show cname ++ "'"
                        return eCls

instance Checkable FieldDecl Ext.FieldDecl where
    typecheck (Field name ty) = do wfType ty
                                   return $ Ext.Field name ty

    hasType (f@(Field name fType)) ty = do unless (fType == ty) $ tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show fType ++ "'"
                                           typecheck f

instance Checkable MethodDecl Ext.MethodDecl where
     typecheck (Method name rtype params body) = 
         do wfType rtype
            eParams <- mapM typecheckParam params
            eBody <- local addParams $ pushHasType body rtype
            return $ Ext.Method name rtype eParams eBody
         where
           typecheckParam = (\p@(Param(name, ty)) -> local (pushBT p) $ do {wfType ty; return $ Ext.Param(name, ty)})
           addParams = extendEnvironment (map (\(Param p) -> p) params)

     hasType (m@(Method _ rtype _ _)) ty = do unless (rtype == ty) $ 
                                                     tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show rtype ++ "'"
                                              typecheck m

instance Checkable Expr Ext.Expr where
    hasType expr ty = do eExpr <- typecheck expr
                         unless (eExpr `Ext.hasType` ty) $
                                tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show (Ext.getType eExpr) ++ "'"
                         return eExpr

    typecheck Skip = return $ Ext.Skip

    typecheck (Call target name args) = 
        do eTarget <- pushTypecheck target
           targetType <- return $ Ext.getType eTarget
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
                    return $ Ext.Call returnType eTarget name eArgs

    typecheck (Let x ty val expr) = 
        do eVal <- pushHasType val ty
           eExpr <- local (extendEnvironment [(x, ty)]) $ pushTypecheck expr
           return $ Ext.Let (Ext.getType eExpr) x ty eVal eExpr

    typecheck (Seq exprs) = 
        do eExprs <- mapM pushTypecheck exprs 
           seqType <- return $ Ext.getType (last eExprs)
           return $ Ext.Seq seqType eExprs

    typecheck (IfThenElse cond thn els) = 
        do eCond <- pushHasType cond boolType
           eThn <- pushTypecheck thn
           thnType <- return $ Ext.getType eThn
           eEls <- pushHasType els thnType
           return $ Ext.IfThenElse thnType eCond eThn eEls

    typecheck (While cond expr) = 
        do eCond <- pushHasType cond boolType
           eExpr <- pushTypecheck expr
           return $ Ext.While (Ext.getType eExpr) eCond eExpr

    typecheck (Get expr) = mzero

    typecheck (FieldAccess expr f) = 
        do ePath <- pushTypecheck expr
           pathType <- return $ Ext.getType ePath
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ 
                          (show $ ppExpr expr) ++ "' of primitive type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup pathType f
           case fType of
             Just ty -> return $ Ext.FieldAccess ty ePath f
             Nothing -> tcError $ "No field '" ++ show f ++ "' in class '" ++ show pathType ++ "'"                                                         

    typecheck (Assign lval rval) = 
        do eLVal <- pushTypecheck lval
           eRVal <- pushHasType rval (Ext.getType eLVal)
           return $ Ext.Assign voidType eLVal eRVal

    typecheck (VarAccess x) = 
        do varType <- asks $ varLookup x
           case varType of
             Just ty -> return $ Ext.VarAccess ty x
             Nothing -> tcError $ "Unbound variable '" ++ show x ++ "'"

    typecheck Null = return $ Ext.Null

    typecheck BTrue = return $ Ext.BTrue

    typecheck BFalse = return $ Ext.BFalse

    typecheck (New ty) = 
        do wfType ty
           return $ Ext.New ty

    typecheck (Print ty expr) = 
        do eExpr <- pushHasType expr ty
           return $ Ext.Print voidType eExpr

    typecheck (StringLiteral s) = return $ Ext.StringLiteral s

    typecheck (IntLiteral n) = return $ Ext.IntLiteral n

    typecheck (Binop op e1 e2) 
        | op `elem` cmpOps = 
            do eE1 <- pushHasType e1 intType
               eE2 <- pushHasType e2 intType
               return $ Ext.Binop boolType op eE1 eE2
        | op `elem` eqOps =
            do eE1 <- pushTypecheck e1
               eE2 <- pushHasType e2 (Ext.getType eE1)
               return $ Ext.Binop boolType op eE1 eE2
        | op `elem` arithOps = 
            do eE1 <- pushHasType e1 intType
               eE2 <- pushHasType e2 intType
               return $ Ext.Binop intType op eE1 eE2
        | otherwise = tcError $ "Undefined binary operator '" ++ show op ++ "'"
        where
          cmpOps   = [Identifiers.LT, Identifiers.GT]
          eqOps    = [Identifiers.EQ, NEQ]
          arithOps = [PLUS, MINUS, TIMES, DIV]

instance Checkable LVal Ext.LVal where
    hasType lval ty = do eLVal <- typecheck lval
                         unless (eLVal `Ext.hasType` ty) $ 
                                tcError $ "Type mismatch. Expected type '" ++ show ty ++ "', got '" ++ show (Ext.getType eLVal) ++ "'"
                         return eLVal

    typecheck (LVal x) = 
        do varType <- asks (varLookup x)
           case varType of
             Just ty -> return $ Ext.LVal ty x
             Nothing -> tcError $ "Unbound variable '" ++ show x ++ "'"
    typecheck (LField expr f) = 
        do ePath <- typecheck expr
           pathType <- return $ Ext.getType ePath
           when (isPrimitive pathType) $ 
                tcError $ "Cannot read field of expression '" ++ (show $ ppExpr expr) ++ 
                          "' of primitive type '" ++ show pathType ++ "'"
           fType <- asks $ fieldLookup (Ext.getType ePath) f
           case fType of
             Just ty -> return $ Ext.LField ty ePath f
             Nothing -> tcError $ "No field '" ++ show f ++ "' in class '" ++ show pathType ++ "'"