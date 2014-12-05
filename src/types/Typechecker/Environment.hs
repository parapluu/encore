{-# LANGUAGE NamedFieldPuns #-}

{-| 

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings and
typevar-type bindings for doing lookups, as well as the
'Typechecker.TypeError.Backtrace' used for error handling.

-}

module Typechecker.Environment(Environment, 
                               buildEnvironment, 
                               classLookup, 
                               classActivityLookup, 
                               methodLookup, 
                               fieldLookup, 
                               varLookup,
                               isLocal,
                               typeVarLookup,
                               extendEnvironment,
                               replaceLocals,
                               bindType,
                               bindTypes,
                               bindings,
                               backtrace,
                               pushBT) where

import Data.List
import Data.Maybe
--import Control.Monad.Error
import Control.Monad.Except

-- Module dependencies
import Identifiers
import AST.AST
import Types
import Typechecker.TypeError

type FunctionType = (Name, Type)
type VarType = (Name, Type)
type FieldType = (Name, Type)
type MethodType = (Name, ([Type], Type))
type ClassType = (Type, ([FieldType], [MethodType]))

data Environment = Env {ctable :: [ClassType], 
                        globals :: [FunctionType], 
                        locals :: [VarType], 
                        bindings :: [(Type, Type)], 
                        bt :: Backtrace}

buildEnvironment :: Program -> Either TCError Environment
buildEnvironment Program {functions, classes} = 
    do distinctFunctions
       distinctClasses
       return $ Env {ctable = map getClassType classes,
                     globals = map getFunctionType functions, 
                     locals = [], bindings = [], bt = emptyBT}
    where
      -- Each class knows if it's passive or not, but reference
      -- types in functions, methods and fields must be given the
      -- correct activity
      setActivity ty = 
          case find ((==ty) . cname) classes of
            Just c -> cname c
            Nothing -> ty

      distinctFunctions = 
          case functions \\ nubBy (\f1 f2 -> (funname f1 == funname f2)) functions of
            [] -> return ()
            (fun:_) -> throwError $ TCError ("Duplicate definition of function '" ++ show (funname fun) ++ "'" , push fun emptyBT)

      distinctClasses = 
          case classes \\ nubBy (\c1 c2 -> (cname c1 == cname c2)) classes of
            [] -> return ()
            (cls:_) -> throwError $ TCError ("Duplicate definition of class '" ++ show (cname cls) ++ "'" , push cls emptyBT)

      getFunctionType Function {funname, funtype, funparams} = 
          (funname, arrowType (map (setActivity . ptype) funparams) (setActivity funtype))

      getClassType Class {cname = c, fields, methods} = 
          (c, (map getFieldType fields, map getMethodType methods))

      getFieldType Field {fname, ftype} =
          (fname, typeMap setActivity ftype)

      getMethodType Method {mname, mtype, mparams} = 
          (mname, (map ((typeMap setActivity) . ptype) mparams, typeMap setActivity mtype))

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env = env {bt = push x (bt env)}

backtrace = bt

fieldLookup :: Type -> Name -> Environment -> Maybe Type
fieldLookup cls f env = do (fields, _) <- classLookup cls env
                           lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe ([Type], Type)
methodLookup cls m env = do (_, methods) <- classLookup cls env
                            lookup m methods

classLookup :: Type -> Environment -> Maybe ([FieldType], [MethodType])
classLookup cls env
    | isRefType cls = lookup cls (ctable env)
    | otherwise = error $ "Tried to lookup the class of '" ++ show cls ++ "' which is not a reference type"

classActivityLookup :: Type -> Environment -> Maybe Type
classActivityLookup cls env 
    | isRefType cls = do (cls', _) <- find (\(cls', _) -> getId cls == getId cls') (ctable env)
                         return cls'
    | otherwise = error $ "Tried to lookup the activity of '" ++ show cls ++ "' which is not a reference type"

varLookup :: Name -> Environment -> Maybe Type
varLookup x env = case lookup x (locals env) of
                       Nothing -> lookup x (globals env)
                       result -> result

isLocal :: Name -> Environment -> Bool
isLocal x env = isJust $ lookup x (locals env)

typeVarLookup :: Type -> Environment -> Maybe Type
typeVarLookup ty env 
    | isTypeVar ty = lookup ty (bindings env)
    | otherwise    = error "Tried to lookup the binding of something that was not a type variable"

extendEnvironment :: [VarType] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) env = 
    extendEnvironment newTypes $ env {locals = extend (locals env) name ty}
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):locals) name' ty'
          | name == name' = (name', ty'):locals
          | otherwise     = (name, ty):(extend locals name' ty')

bindType :: Type -> Type -> Environment -> Environment
bindType var ty env
    | isTypeVar var = env {bindings = (var, ty) : (bindings env)}
    | otherwise     = error "Tried to bind something that was not a type variable"

bindTypes :: [(Type, Type)] -> Environment -> Environment
bindTypes bindings env = foldr (\(tyVar, ty) env -> bindType tyVar ty env) env bindings -- env{bindings = b ++ (bindings env)}

replaceLocals :: [VarType] -> Environment -> Environment
replaceLocals newTypes env = env {locals = newTypes}
