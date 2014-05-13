module Typechecker(typecheckEncoreProgram) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error

import AST
import PrettyPrinter
import Types
import Environment

newtype TCError = TCError (String, Backtrace)
instance Error TCError
instance Show TCError where
    show (TCError (msg, bt)) = 
        "*** Error during typechecking ***\n" ++
        msg ++ "\n" ++
        (concat $ map ((++"\n") . show) bt)

tcError s = do bt <- asks backtrace
               throwError $ TCError (s, bt)

typecheckEncoreProgram :: Program -> Either TCError ()
typecheckEncoreProgram p = runReader (runErrorT (typecheck p)) (buildClassTable p)

wfType :: Type -> ErrorT TCError (Reader Environment) ()
wfType ty = if isPrimitive ty then
                return ()
            else
                do refType <- asks $ classLookup ty
                   unless (isJust $ refType) $ tcError $ "Unknown type '" ++ show ty ++ "'"

class Checkable a where
    typecheck :: a -> ErrorT TCError (Reader Environment) ()

instance Checkable Program where
    typecheck (Program classes) = mapM_ typecheckClass classes
        where
          typecheckClass = (\c -> local (pushBT c) $ typecheck c)

instance Checkable ClassDecl where
    typecheck (Class cname fields methods) =
        do mapM_ typecheckField fields
           mapM_ typecheckMethod methods 
        where
          typecheckField = (\f -> local (pushBT f) $ typecheck f)
          typecheckMethod = (\m -> local ((pushBT m) . extendEnvironment [(Name "this", cname)]) $ typecheck m)
          distinctFieldNames = 
              nubBy (\f1 f2 -> (fname f1 == fname f2)) fields == fields
          distinctMethodNames = 
              nubBy (\m1 m2 -> (mname m1 == mname m2)) methods == methods

instance Checkable FieldDecl where
    typecheck (Field name ty) = wfType ty

instance Checkable MethodDecl where
     typecheck (Method name rtype params body) = 
         do wfType rtype
            mapM typecheckParam params
            matchingBody <- local (extendEnvironment (map (\(Param p) -> p) params)) $ hastype body rtype
            unless matchingBody $ tcError $ "Method body does not match return type '" ++ show rtype ++ "'"
         where
           typecheckParam = (\p@(Param(name, ty)) -> local (pushBT p) $ wfType ty)

-- TODO: Make hastype more general and return an error message when the two types do not match, rather than a bool
hastype :: Expr -> Type -> ErrorT TCError (Reader Environment) Bool
hastype Null ty = return $ not $ isPrimitive ty
hastype expr (Type "_NullType") = do eType <- local (pushBT expr) $ typeof expr
                                     return . not . isPrimitive $ eType
hastype expr ty = do eType <- local (pushBT expr) $ typeof expr
                     return $ eType == ty

class Typeable a where
     typeof :: a -> ErrorT TCError (Reader Environment) Type
     typeof x = tcError "Typechecking not implemented for construct"

instance Typeable Expr where
    typeof Skip = return $ Type "void"
    typeof (Call target name args) = do targetType <- local (pushBT target) $ typeof target -- TODO: Check if targetType is primitive and give a good error message
                                        lookupResult <- asks $ methodLookup targetType name
                                        case lookupResult of
                                          Just (returnType, params) -> 
                                              do unless (length args == length params) $ 
                                                        tcError $ "Method '" ++ show name ++ "' of class '" ++ show targetType ++ "' expects " ++
                                                                  show (length params) ++ " arguments. Got " ++ show (length args)
                                                 argTypes <- mapM typeof args -- TODO: Show which arguments have the wrong types
                                                 if and $ zipWith (\ty (Param (_, pty)) -> ty == pty) argTypes params then
                                                     return returnType
                                                 else
                                                     tcError $ "Type mismatch in arguments to call of '" ++ show name ++ "'"
                                          Nothing -> tcError $ "No method '" ++ show name ++ "' in class '" ++ show targetType ++ "'"
    typeof (Let x ty val expr) = do varType <- local (pushBT val) $ typeof val
                                    if varType == ty then
                                        local ((pushBT expr) . (extendEnvironment [(x, ty)])) $ typeof expr
                                    else
                                        tcError $ "Declared type '" ++ show ty ++ "' does not match value type '" ++ show varType ++ "'"
    typeof (Seq exprs) = do mapM typeof exprs 
                            typeof (last exprs)
    typeof (IfThenElse cond thn els) = 
        do condType <- typeof cond
           unless (condType == Type "bool") $ tcError $ "Non boolean condition: " ++ (show $ ppExpr cond)
           thnType <- typeof thn
           elsType <- typeof els
           unless (thnType == elsType) $ tcError $ "Type of then-branch (" ++ show thnType ++ ") does not match type of else-branch (" ++ show elsType ++ ")"
           return thnType
    typeof (While cond expr) = 
        do condType <- typeof cond
           unless (condType == Type "bool") $ tcError $ "Non boolean condition: " ++ (show $ ppExpr cond)
           typeof expr
    typeof (Get expr) = mzero
    typeof (FieldAccess expr f) = do pathType <- typeof expr -- TODO: Check if pathType is primitive and give a good error message
                                     fType <- asks $ fieldLookup pathType f
                                     case fType of
                                       Just ty -> return $ ty
                                       Nothing -> tcError $ "No field '" ++ show f ++ "' in class '" ++ show pathType ++ "'"
                                                         
    typeof (Assign lval expr) = return $ Type "void"
--         -- TODO: Uncomment when LVal implements Typeable
--         -- do ltype <- typeof env lval
--         --    rtype <- typeof env expr
--         --    unless (ltype == rtype) $ tcError "Non-compatible assignment"
--         --    return ltype
    typeof (VarAccess x) = do varType <- asks (varLookup x)
                              case varType of
                                Just ty -> return ty
                                Nothing -> tcError $ "Unbound variable '" ++ show x ++ "'"
    typeof Null = return $ Type "_NullType"
    typeof BTrue = return $ Type "bool"
    typeof BFalse = return $ Type "bool"
    typeof (New ty) = do wfType ty
                         return ty
    typeof (Print ty expr) = return $ Type "void"
    typeof (StringLiteral s) = return $ Type "string"
    typeof (IntLiteral n) = return $ Type "int"
    typeof (Binop op e1 e2) 
        | op `elem` cmpOps   = do leftIsInt <- hastype e1 (Type "int")
                                  rightIsInt <- hastype e2 (Type "int")
                                  if leftIsInt && rightIsInt then
                                      return $ Type "bool"
                                  else
                                      tcError $ "Trying to compare values of object type"
        | op `elem` eqOps    = do ty1 <- typeof e1
                                  eqType <- hastype e2 ty1
                                  if eqType then
                                      return $ Type "bool"
                                  else
                                      tcError $ "Trying to compare objects of different type"
        | op `elem` arithOps = do leftIsInt <- hastype e1 (Type "int")
                                  rightIsInt <- hastype e2 (Type "int")
                                  if leftIsInt && rightIsInt then
                                      return $ Type "int"
                                  else
                                      tcError $ "Trying to apply operator '" ++ show op ++ "' to something that is not an int"
        | otherwise          = error "*** Trying to typecheck undefined binary operator ***"
        where
          cmpOps   = [AST.LT, AST.GT]
          eqOps    = [AST.EQ, AST.NEQ]
          arithOps = [AST.PLUS, AST.MINUS, AST.TIMES, AST.DIV]

instance Typeable LVal
-- TODO: Implement this if we want assignment to have another type than void