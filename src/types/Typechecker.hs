module Typechecker(typecheckEncoreProgram) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Reader

import AST
import Types
import Environment

typecheckEncoreProgram :: Program -> Bool
typecheckEncoreProgram p = runReader (typecheck p) (buildClassTable p)

class Checkable a where
    typecheck :: a -> Reader Environment Bool
    typecheck _ = return False

instance Checkable Program where
    typecheck (Program classes) = do result <- mapM typecheck classes
                                     return $ and result

instance Checkable ClassDecl where
    typecheck (Class cname fields methods) =
        do fieldResults <- mapM typecheck fields
           methodResults <- mapM typecheckWithThis methods
           return $ and fieldResults && and methodResults && distinctFieldNames && distinctMethodNames
        where
          typecheckWithThis = (\ty -> local (extendEnvironment [(Name "this", cname)]) $ typecheck ty)
          distinctFieldNames = 
              nubBy (\f1 f2 -> (fname f1 == fname f2)) fields == fields
          distinctMethodNames = 
              nubBy (\m1 m2 -> (mname m1 == mname m2)) methods == methods

instance Checkable FieldDecl where
    typecheck (Field name ty) = asks (wfType ty)

instance Checkable MethodDecl where
     typecheck (Method name rtype params body) = 
         do returnResult <- asks (wfType rtype)
            paramResults <- mapM (\(Param(name, ty)) -> (asks (wfType ty))) params
            bodyResult <- local (extendEnvironment (map (\(Param p) -> p) params)) $ hastype body rtype
            return $ returnResult && and paramResults && bodyResult

hastype :: Expr -> Type -> Reader Environment Bool
hastype Null ty = return $ not $ isPrimitive ty
hastype expr (Type "_NullType") = do env <- ask
                                     case runReaderT (typeof expr) env of
                                         Just ty -> return . not . isPrimitive $ ty
                                         Nothing -> return False
hastype expr ty = do env <- ask
                     let etype = runReaderT (typeof expr) env in
                         return $ etype == Just ty

class Typeable a where
     typeof :: a -> ReaderT Environment Maybe Type
     typeof x = lift Nothing

instance Typeable Expr where
    typeof Skip = return $ Type "void"
    typeof (Call target name args) = do targetType <- typeof target
                                        Just (returnType, params) <- asks $ methodLookup targetType name
                                        guard $ length args == length params
                                        env <- ask
                                        guard $ and $ zipWith (\expr (Param (_, ty)) -> runReader (hastype expr ty) env) args params
                                        return returnType
    typeof (Let x ty val expr) = do varType <- typeof val
                                    guard $ varType == ty
                                    local (extendEnvironment [(x, ty)]) $ typeof expr
    typeof (Seq exprs) = typeof (last exprs)
    typeof (IfThenElse cond thn els) = 
        do condType <- typeof cond
           guard $ condType == Type "bool"
           thnType <- typeof thn
           elsType <- typeof els
           guard $ thnType == elsType
           return thnType
    typeof (While cond expr) = 
        do condType <- typeof cond
           guard $ condType == Type "bool"
           typeof expr
    typeof (Get expr) = mzero
    typeof (FieldAccess expr f) = do pathType <- typeof expr
                                     fType <- asks $ fieldLookup pathType f
                                     lift fType
    typeof (Assign lval expr) = return $ Type "void"
--         -- TODO: Uncomment when LVal implements Typeable
--         -- do ltype <- typeof env lval
--         --    rtype <- typeof env expr
--         --    guard $ ltype == rtype
--         --    return ltype
    typeof (VarAccess x) = do ty <- asks (varLookup x)
                              lift ty
    typeof Null = return $ Type "_NullType"
    typeof BTrue = return $ Type "bool"
    typeof BFalse = return $ Type "bool"
    typeof (New ty) = return ty
    typeof (Print ty expr) = return $ Type "void"
    typeof (StringLiteral s) = return $ Type "string"
    typeof (IntLiteral n) = return $ Type "int"
    typeof (Binop op e1 e2) 
        | op `elem` cmpOps   = do env <- ask 
                                  guard $ runReader (hastype e1 (Type "int")) env
                                  guard $ runReader (hastype e2 (Type "int")) env
                                  return $ Type "bool"
        | op `elem` eqOps    = do env <- ask
                                  ty1 <- typeof e1
                                  guard $ runReader (hastype e2 ty1) env
                                  return $ Type "bool"
        | op `elem` arithOps = do env <- ask
                                  guard $ runReader (hastype e1 (Type "int")) env
                                  guard $ runReader (hastype e2 (Type "int")) env
                                  return $ Type "int"
        | otherwise          = error "*** Trying to typecheck undefined binary operator ***"
        where
          cmpOps   = [AST.LT, AST.GT]
          eqOps    = [AST.EQ, AST.NEQ]
          arithOps = [AST.PLUS, AST.MINUS, AST.TIMES, AST.DIV]

instance Typeable LVal
-- TODO: Implement this if we want assignment to have another type than void