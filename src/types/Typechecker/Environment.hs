{-# LANGUAGE NamedFieldPuns #-}

{-|

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings and
typevar-type bindings for doing lookups, as well as the
'Typechecker.TypeError.Backtrace' used for error handling.

-}

module Typechecker.Environment
    (
      Environment
    , addTypeParameters
    , backtrace
    , bindType
    , bindTypes
    , bindings
    , buildEnvironment
    , classLookup
    , classTypeLookup
    , classTypeParameterLookup
    , extendEnvironment
    , fieldLookup
    , isLocal
    , methodLookup
    , pushBT
    , replaceLocals
    , typeParameters
    , typeVarLookup
    , varLookup
    ) where

import Data.Function (on)
import Data.List
import Data.Maybe
import Control.Monad.Except

-- Module dependencies
import Identifiers
import AST.AST
import Types
import Typechecker.TypeError

type VarTable    = [(Name, Type)]
type FieldTable  = [(Name, FieldDecl)]
type MethodTable = [(Name, MethodDecl)]
type ClassTable  = [(Type, (FieldTable, MethodTable))]

data Environment = Env
    { ctable   :: ClassTable
    , globals  :: VarTable
    , locals   :: VarTable
    , bindings :: [(Type, Type)]
    , typeParameters :: [Type]
    , bt :: Backtrace
    }
    -- TODO: Add "current control abstraction"

buildEnvironment :: Program -> Either TCError Environment
buildEnvironment Program {functions, classes} = do
    distinctFunctions
    distinctClasses
    return Env {ctable  = map getClassEntry classes,
                globals = map getFunctionType functions,
                locals = [], bindings = [], typeParameters = [],
                bt = emptyBT}
  where
      -- Each class knows if it's passive or not, but reference
      -- types in functions, methods and fields must be given the
      -- correct activity
      setActivity ty =
          case find ((==ty) . cname) classes of
               Just c -> if isActiveRefType $ cname c
                         then makeActive ty
                         else makePassive ty
               Nothing -> ty

      distinctFunctions =
          case functions \\ nubBy ((==) `on` funname) functions of
               [] -> return ()
               (fun:_) -> throwError $ TCError
                   ("Duplicate definition of function '" ++
                     show (funname fun) ++ "'" , push fun emptyBT)

      distinctClasses =
          case classes \\ nubBy ((==) `on` cname) classes of
               [] -> return ()
               (cls:_) -> throwError $ TCError
                   ("Duplicate definition of class '" ++ show (cname cls) ++
                     "'" , push cls emptyBT)

      getFunctionType Function {funname, funtype, funparams} =
          (funname, arrowType (map (setActivity . ptype) funparams)
            (setActivity funtype))

      getClassEntry Class {cname = c, fields, methods} =
          (c, (map getField fields, map getMethod methods))

      getField f@(Field {fname, ftype}) =
          (fname, f{ftype = typeMap setActivity ftype})

      getMethod m = (mname m,
                     m{mparams = map (\p@(Param{ptype}) ->
                       p{ptype = typeMap setActivity ptype}) (mparams m),
                       mtype = typeMap setActivity (mtype m)})

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env = env {bt = push x (bt env)}

backtrace = bt

fieldLookup :: Type -> Name -> Environment -> Maybe FieldDecl
fieldLookup cls f env = do
    (fields, _) <- classLookup cls env
    lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe MethodDecl
methodLookup cls m env = do
    (_, methods) <- classLookup cls env
    lookup m methods

-- TODO: Merge these two functions
classLookup :: Type -> Environment -> Maybe (FieldTable, MethodTable)
classLookup cls env
  | isRefType cls = lookup cls (ctable env)
  | otherwise = error $ "Tried to lookup the class of '" ++ show cls ++ "' which is not a reference type"

classTypeLookup :: Type -> Environment -> Maybe Type
classTypeLookup cls env
  | isRefType cls = do
      (cls', _) <- find (\(cls', _) -> getId cls == getId cls') (ctable env)
      return cls'
  | otherwise = error $ "Tried to lookup the class declaration of '" ++
                        show cls ++ "' which is not a reference type"

classTypeParameterLookup :: Type -> Environment -> [Type]
classTypeParameterLookup cls env
  | isRefType cls = let Just (cls', _) =
                            find (\(cls', _) -> getId cls == getId cls')
                                (ctable env)
                    in getTypeParameters cls'
  | otherwise = error $ "Tried to lookup the type parameters of '" ++
                        show cls ++ "' which is not a reference type"

varLookup :: Name -> Environment -> Maybe Type
varLookup x env = case lookup x (locals env) of
                       Nothing -> lookup x (globals env)
                       result -> result

isLocal :: Name -> Environment -> Bool
isLocal x env = isJust $ lookup x (locals env)

typeVarLookup :: Type -> Environment -> Maybe Type
typeVarLookup ty env
    | isTypeVar ty = lookup ty (bindings env)
    | otherwise    = error $ "Tried to lookup the binding of something that " ++
                             "was not a type variable"

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) env =
    extendEnvironment newTypes $ env {locals = extend (locals env) name ty}
  where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty) : locals) name' ty'
        | name == name' = (name', ty') : locals
        | otherwise     = (name, ty) : extend locals name' ty'

addTypeParameters :: [Type] -> Environment -> Environment
addTypeParameters [] env = env
addTypeParameters xs env@(Env{typeParameters}) =
    if all isTypeVar xs
    then env{typeParameters = xs ++ typeParameters}
    else error "Tried to add a type parameter that was not a type parameter"

bindType :: Type -> Type -> Environment -> Environment
bindType var ty env
  | isTypeVar var = env {bindings = (var, ty) : bindings env}
  | otherwise     = error "Tried to bind something that was not a type variable"

bindTypes :: [(Type, Type)] -> Environment -> Environment
bindTypes bindings env = foldr (\(tyVar, ty) env -> bindType tyVar ty env) env
                            bindings

replaceLocals :: VarTable -> Environment -> Environment
replaceLocals newTypes env = env {locals = newTypes}
