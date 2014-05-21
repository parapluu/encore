{-# LANGUAGE NamedFieldPuns #-}

{-| 

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings for doing
lookups, as well as the 'Typechecker.TypeError.Backtrace' used for
error handling.

-}

module Typechecker.Environment(Environment, 
                               buildClassTable, 
                               classLookup, 
                               methodLookup, 
                               fieldLookup, 
                               varLookup,
                               extendEnvironment,
                               backtrace,
                               pushBT) where

import Data.List

-- Module dependencies
import Identifiers
import AST.AST
import Typechecker.Types
import Typechecker.TypeError

data Environment = Env {ctable :: [ClassType], locals :: [VarType], bt :: Backtrace}

buildClassTable :: Program -> Maybe Environment
buildClassTable (Program classes) = if distinctClassNames then
                                        Just $ Env (map getClassType classes) [] emptyBT
                                    else
                                        Nothing
    where
      distinctClassNames = nubBy (\c1 c2 -> (cname c1 == cname c2)) classes == classes
      getClassType Class {cname, fields, methods} = (cname, (fields', methods'))
          where
            fields' = map getFieldType fields
            methods' = map getMethodType methods
            getFieldType Field {fname, ftype} = (fname, ftype)
            getMethodType Method {mname, mtype, mparams} = (mname, (mtype, mparams))

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env = env {bt = push x (bt env)}

backtrace = bt

fieldLookup :: Type -> Name -> Environment -> Maybe Type
fieldLookup cls f env = do (fields, _) <- lookup cls (ctable env)
                           lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe (Type, [ParamDecl])
methodLookup cls m env = do (_, methods) <- lookup cls (ctable env)
                            lookup m methods

classLookup :: Type -> Environment -> Maybe ([FieldType], [MethodType])
classLookup cls env = lookup cls (ctable env)

varLookup :: Name -> Environment -> Maybe Type
varLookup x env = lookup x (locals env)

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) env = 
    extendEnvironment newTypes $ env {locals = extend (locals env) name ty}
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):bindings) name' ty'
          | name == name' = (name', ty'):bindings
          | otherwise     = (name, ty):(extend bindings name' ty')