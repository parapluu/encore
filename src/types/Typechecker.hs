module Typechecker(typecheckEncoreProgram) where

import Data.Maybe
import Data.List
import Control.Monad

import AST
import Types
import Environment

typecheckEncoreProgram :: Program -> Bool
typecheckEncoreProgram p = typecheck (buildClassTable p) p

class Checkable a where
    typecheck :: Environment -> a -> Bool
    typecheck env x = False

instance Checkable Program where
    typecheck env (Program classes) = all (typecheck env) classes

instance Checkable ClassDecl where
    typecheck env (Class cname fields methods) =
        all (typecheck env) fields &&
        all (typecheck env') methods &&
        distinctFieldNames &&
        distinctMethodNames 
        where
          env' = extendEnvironment env [(Name "this", cname)]
          distinctFieldNames = 
              nubBy (\f1 f2 -> (fname f1 == fname f2)) fields == fields
          distinctMethodNames = 
              nubBy (\m1 m2 -> (mname m1 == mname m2)) methods == methods

instance Checkable FieldDecl where
    typecheck env (Field name ty) = wfType env ty

instance Checkable MethodDecl where
    typecheck env (Method name rtype params body) = 
        wfType env rtype &&
        all (\(Param(name, ty)) -> wfType env ty) params &&
        hastype env' body rtype
        where
          env' = extendEnvironment env (map (\(Param p) -> p) params)

hastype :: Environment -> Expr -> Type -> Bool
hastype _ Null ty = not $ isPrimitive ty
hastype env expr (Type "_NullType") = case typeof env expr of
                                        Just ty -> not $ isPrimitive ty
                                        Nothing -> False
hastype env expr ty = typeof env expr == Just ty

class Typeable a where
    typeof :: Environment -> a -> Maybe Type
    typeof env x = Nothing

instance Typeable Expr where
    typeof env Skip = return $ Type "void"
    typeof env (Call target name args) = do targetType <- typeof env target
                                            (returnType, params) <- methodLookup env targetType name
                                            guard $ length args == length params
                                            guard $ and $ zipWith (\expr (Param (_, ty)) -> hastype env expr ty) args params
                                            return returnType
    typeof env (Let x ty val expr) = do varType <- typeof env val
                                        guard $ varType == ty
                                        typeof env' expr
                                            where
                                              env' = extendEnvironment env [(x, ty)]
    typeof env (Seq exprs) = typeof env (last exprs)
    typeof env (IfThenElse cond thn els) = 
        do condType <- typeof env cond
           guard $ condType == Type "bool"
           thnType <- typeof env thn
           elsType <- typeof env els
           guard $ thnType == elsType
           return thnType
    typeof env (While cond expr) = 
        do condType <- typeof env cond
           guard $ condType == Type "bool"
           typeof env expr
    typeof env (Get expr) = Nothing
    typeof env (FieldAccess expr f) = 
        do ty <- typeof env expr
           fieldLookup env ty f
    typeof env (Assign lval expr) = return $ Type "void"
        -- TODO: Uncomment when LVal implements Typeable
        -- do ltype <- typeof env lval
        --    rtype <- typeof env expr
        --    guard $ ltype == rtype
        --    return ltype
    typeof env (VarAccess x) = varLookup env x
    typeof env Null = return $ Type "_NullType"
    typeof env BTrue = return $ Type "bool"
    typeof env BFalse = return $ Type "bool"
    typeof env (New ty) = return ty
    typeof env (Print ty expr) = return $ Type "void"
    typeof env (StringLiteral s) = return $ Type "string"
    typeof env (IntLiteral n) = return $ Type "int"
    typeof env (Binop op e1 e2) 
        | op `elem` cmpOps   = do guard $ hastype env e1 (Type "int")
                                  guard $ hastype env e2 (Type "int")
                                  return $ Type "bool"
        | op `elem` eqOps    = do ty1 <- typeof env e1
                                  guard $ hastype env e2 ty1
                                  return $ Type "bool"
        | op `elem` arithOps = do guard $ hastype env e1 (Type "int")
                                  guard $ hastype env e2 (Type "int")
                                  return $ Type "int"
        | otherwise          = error "*** Trying to typecheck undefined binary operator ***"
        where
          cmpOps   = [AST.LT, AST.GT]
          eqOps    = [AST.EQ, AST.NEQ]
          arithOps = [AST.PLUS, AST.MINUS, AST.TIMES, AST.DIV]

instance Typeable LVal
-- TODO: Implement this if we want assignment to have another type than void