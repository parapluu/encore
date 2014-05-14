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

-- Module dependencies
import AST
import Typechecker.Types
import Typechecker.TypeError

data Environment = Env {ctable :: [ClassType], locals :: [VarType], bt :: Backtrace}

buildClassTable :: Program -> Environment
buildClassTable (Program classes) = Env (map getClassType classes) [] emptyBT
    where
      getClassType (Class name fields methods) = (name, (fields', methods'))
          where
            fields' = map getFieldType fields
            methods' = map getMethodType methods
            getFieldType fld = (fname fld, ftype fld)
            getMethodType mtd = (mname mtd, (rtype mtd, mparams mtd))

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