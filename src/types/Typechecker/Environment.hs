{-# LANGUAGE NamedFieldPuns #-}

{-| 

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings and
typevar-type bindings for doing lookups, as well as the
'Typechecker.TypeError.Backtrace' used for error handling.

-}

module Typechecker.Environment(Environment, 
                               buildClassTable, 
                               classLookup, 
                               classActivityLookup, 
                               methodLookup, 
                               fieldLookup, 
                               varLookup,
                               typeVarLookup,
                               extendEnvironment,
                               replaceLocals,
                               bindType,
                               bindTypes,
                               bindings,
                               backtrace,
                               pushBT) where

import Data.List

-- Module dependencies
import Identifiers
import AST.AST
import Types
import Typechecker.TypeError

type VarType = (Name, Type)
type FieldType = (Name, Type)
type MethodType = (Name, ([ParamDecl], Type))
type ClassType = (Type, ([FieldType], [MethodType]))

data Environment = Env {ctable :: [ClassType], locals :: [VarType], bindings :: [(Type, Type)], bt :: Backtrace}

buildClassTable :: Program -> Either TCError Environment
buildClassTable (Program classes) = 
    case duplicateClasses of
      [] -> Right $ Env (map getClassType classes) [] [] emptyBT
      (cls:_) -> Left $ TCError ("Duplicate definition of class '" ++ show (cname cls) ++ "'" , push cls emptyBT)
    where
      duplicateClasses = classes \\ nubBy (\c1 c2 -> (cname c1 == cname c2)) classes
      getClassType Class {cname, fields, methods} = (cname, (fieldTypes, methodTypes))
          where
            fieldTypes  = map getFieldType fields
            methodTypes = map getMethodType methods
            getFieldType Field {fname, ftype} = (fname, ftype)
            getMethodType Method {mname, mtype, mparams} = (mname, (mparams, mtype))

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env = env {bt = push x (bt env)}

backtrace = bt

fieldLookup :: Type -> Name -> Environment -> Maybe Type
fieldLookup cls f env = do (fields, _) <- classLookup cls env
                           lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe ([ParamDecl], Type)
methodLookup cls m env = do (_, methods) <- classLookup cls env
                            lookup m methods

classLookup :: Type -> Environment -> Maybe ([FieldType], [MethodType])
classLookup cls env
    | isRefType cls = lookup cls (ctable env)
    | otherwise = error $ "Tried to lookup the class of '" ++ show cls ++ "' whiich is not a reference type"

classActivityLookup :: Type -> Environment -> Maybe Type
classActivityLookup cls env 
    | isRefType cls = do (cls', _) <- find (\(cls', _) -> getId cls == getId cls') (ctable env)
                         return cls'
    | otherwise = error $ "Tried to lookup the activity of '" ++ show cls ++ "' whiich is not a reference type"

varLookup :: Name -> Environment -> Maybe Type
varLookup x env = lookup x (locals env)

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
