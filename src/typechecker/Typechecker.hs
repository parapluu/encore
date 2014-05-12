module Typechecker(typecheck) where

import Data.Maybe
import Data.List
import Control.Monad

import AST
import Types
import Environment
import Examples

class Checkable a where
    typecheck :: Environment -> a -> Bool
    typecheck env x = False

instance Checkable Program where
    typecheck env (Program classes) = all (typecheck env) classes

instance Checkable ClassDecl where
    typecheck env (Class name fields methods) =
        all (typecheck env) fields &&
        all (typecheck env) methods &&
        distinctFieldNames &&
        distinctMethodNames 
        where
          distinctFieldNames = 
              nubBy (\f1 f2 -> (fname f1 == fname f2)) fields == fields
          distinctMethodNames = 
              nubBy (\m1 m2 -> (mname m1 == mname m2)) methods == methods

instance Checkable FieldDecl where
    typecheck env (Field name ty) = isPrimitive ty || (isJust $ classLookup env ty)

instance Checkable MethodDecl where
    typecheck env (Method name rtype params body) = 
        typeof env' body == Just rtype
        where
          env' = extendEnvironment env (map (\(Param p) -> p) params)

class Typeable a where
    typeof :: Environment -> a -> Maybe Type
    typeof env x = Nothing

instance Typeable Expr where
    typeof env Skip = return $ Type "void"
    typeof env (Call target name args) = Nothing
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
        -- do ltype <- typeof env lval
        --    rtype <- typeof env expr
        --    guard $ ltype == rtype
        --    return ltype
    typeof env (VarAccess x) = varLookup env x
    typeof env Null = Nothing
    typeof env BTrue = return $ Type "bool"
    typeof env BFalse = return $ Type "bool"
    typeof env (New ty) = return ty
    typeof env (Print ty expr) = return $ Type "void"
    typeof env (StringLiteral s) = return $ Type "string"
    typeof env (IntLiteral n) = return $ Type "int"
    typeof env (Binop op e1 e2) = Nothing

instance Typeable LVal