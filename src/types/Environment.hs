module Environment(Environment, 
                   buildClassTable, 
                   classLookup, 
                   methodLookup, 
                   fieldLookup, 
                   varLookup,
                   extendEnvironment,
                   wfType,
                   fooEnv) where

import Data.Maybe

import Types
import AST

type ClassTable = [ClassType]
data Environment = Env ClassTable [VarType]

fooEnv = Env [] [(Name "x", Type "Foo")]

buildClassTable :: Program -> Environment
buildClassTable (Program classes) = Env (map getClassType classes) []

getClassType :: ClassDecl -> ClassType
getClassType (Class name fields methods) = (name, (fields', methods'))
    where
      fields' = map getFieldType fields
      methods' = map getMethodType methods
      getFieldType (Field name ty) = (name, ty)
      getMethodType (Method name rtype params _) = (name, (rtype, params))

fieldLookup :: Type -> Name -> Environment -> Maybe Type
fieldLookup cls f (Env ctable _) = do (fields, _) <- lookup cls ctable
                                      lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe (Type, [ParamDecl])
methodLookup cls m (Env ctable _) = do (_, methods) <- lookup cls ctable
                                       lookup m methods

classLookup :: Type -> Environment -> Maybe ([FieldType], [MethodType])
classLookup cls (Env ctable _) = lookup cls ctable

varLookup :: Name -> Environment -> Maybe Type
varLookup x (Env _ locals) = lookup x locals

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) (Env ctable locals) = 
    extendEnvironment newTypes (Env ctable (extend locals name ty))
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):bindings) name' ty'
          | name == name' = (name', ty'):bindings
          | otherwise     = (name, ty):(extend bindings name' ty')

wfType :: Type -> Environment -> Bool
wfType ty env = isPrimitive ty || (isJust $ classLookup ty env)