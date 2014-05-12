module Environment(Environment, 
                   buildClassTable, 
                   classLookup, 
                   methodLookup, 
                   fieldLookup, 
                   varLookup,
                   extendEnvironment) where

import Types
import AST

type ClassTable = [ClassType]
data Environment = Env ClassTable [VarType]

buildClassTable :: Program -> Environment
buildClassTable (Program classes) = Env (map getClassType classes) []

getClassType :: ClassDecl -> ClassType
getClassType (Class name fields methods) = (name, (fields', methods'))
    where
      fields' = map getFieldType fields
      methods' = map getMethodType methods
      getFieldType (Field name ty) = (name, ty)
      getMethodType (Method name rtype params _) = (name, (rtype, params))

fieldLookup :: Environment -> Type -> Name -> Maybe Type
fieldLookup (Env ctable _) cls f = do (fields, _) <- lookup cls ctable
                                      lookup f fields

methodLookup :: Environment -> Type -> Name -> Maybe (Type, [ParamDecl])
methodLookup (Env ctable _) cls m = do (_, methods) <- lookup cls ctable
                                       lookup m methods

classLookup :: Environment -> Type -> Maybe ([FieldType], [MethodType])
classLookup (Env ctable _) cls = lookup cls ctable

varLookup :: Environment -> Name -> Maybe Type
varLookup (Env _ locals) x = lookup x locals

extendEnvironment :: Environment -> [(Name, Type)] -> Environment
extendEnvironment env [] = env
extendEnvironment (Env ctable locals) ((name, ty):newTypes) = 
    extendEnvironment (Env ctable (extend locals name ty)) newTypes
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):bindings) name' ty'
          | name == name' = (name', ty'):bindings
          | otherwise     = (name, ty):(extend bindings name' ty')
