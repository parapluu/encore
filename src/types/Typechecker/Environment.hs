{-|

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings and
typevar-type bindings for doing lookups, as well as the
'Typechecker.TypeError.Backtrace' used for error handling.

-}

module Typechecker.Environment(Environment,
                               buildEnvironment,
                               traitLookup,
                               traitLookupUnsafe,
                               refTypeLookup,
                               refTypeLookupUnsafe,
                               methodLookup,
                               fieldLookup,
                               varLookup,
                               isLocal,
                               typeVarLookup,
                               extendEnvironment,
                               addTypeParameters,
                               typeParameters,
                               replaceLocals,
                               bindTypes,
                               bindings,
                               backtrace,
                               pushBT,
                               refTypeParameters
                               ) where

import Data.List
import Data.Maybe
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as M
import Text.Printf (printf)

-- Module dependencies
import Identifiers
import AST.AST
import Types
import Typechecker.TypeError

type VarTable    = [(Name, Type)]

data Environment = Env {
  class_table :: M.HashMap String ClassDecl,
  trait_table :: M.HashMap String Trait,
  globals  :: VarTable,
  locals   :: VarTable,
  bindings :: [(Type, Type)],
  typeParameters :: [Type],
  bt :: Backtrace
} deriving Show

empty_env = Env {
  class_table = M.empty,
  trait_table = M.empty,
  globals = [],
  locals = [],
  bindings = [],
  typeParameters = [],
  bt = emptyBT
}

buildEnvironment :: Program -> Either TCError Environment
buildEnvironment p@(Program{functions, classes, imports}) =
  merge_envs $ mapProgram p buildEnvironment'
  where
    buildEnvironment' :: Program -> [Either TCError Environment]
    buildEnvironment' p@(Program {functions, classes, traits, imports}) =
      return Env {
        class_table = M.fromList [(getId (cname c), c) | c <- classes],
        trait_table = M.fromList [(getId (traitName t), t) | t <- traits],
        globals = map getFunctionType functions,
        locals = [],
        bindings = [],
        typeParameters = [],
        bt = emptyBT
      } : []

    getFunctionType Function {funname, funtype, funparams} =
        (funname, arrowType (map (ptype) funparams) funtype)

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env@Env{bt} = env{bt = push x bt}

backtrace = bt

fieldLookup :: Type -> Name -> Environment -> Maybe FieldDecl
fieldLookup t f env
  | isTrait t = do
    trait <- M.lookup (getId t) $ trait_table env
    find (\Field{fname} -> fname == f) $ traitFields trait
  | isClass t = do
    cls <- classLookup t env
    find (\Field{fname} -> fname == f) $ fields cls
  | otherwise = error $ "Trying to lookup field in a non ref type " ++ show t

match_method :: Name -> MethodDecl -> Bool
match_method m Method{mname} = mname == m
match_method m StreamMethod{mname} = mname == m

trait_method_lookup :: Type -> Name -> Environment -> Maybe MethodDecl
trait_method_lookup trait m env = do
  trait <- traitLookup trait env
  find (match_method m) $ traitMethods trait

methodLookup :: Type -> Name -> Environment -> Maybe MethodDecl
methodLookup ty m env
  | isClass ty = do
    cls <- classLookup ty env
    c_m <- return $ find (match_method m) $ methods cls
    t_ms <- return $ map (\t -> trait_method_lookup t m env) traits
    ret <- find isJust $ (c_m:t_ms)
    return $ fromJust ret
  | isTrait ty = do
    trait_method_lookup ty m env
  | otherwise = error "methodLookup in non-ref type"
    where
      traits = getImplTraits ty

traitLookup :: Type -> Environment -> Maybe Trait
traitLookup t env =
  M.lookup (getId t) $ trait_table env

traitLookupUnsafe :: Type -> Environment -> Trait
traitLookupUnsafe t env =
  let
    ret = traitLookup t env
    found = isJust ret
  in
    if found then
      fromJust ret
    else
      error $ printf "Can't find trait %s" $ show t

-- TODO: Merge these two functions
classLookup :: Type -> Environment -> Maybe ClassDecl
classLookup cls env
    | isRefType cls = M.lookup (getId cls) $ class_table env
    | otherwise = error $
      "Tried to lookup the class of '" ++ show cls
      ++ "' which is not a reference type"

refTypeLookup :: Type -> Environment -> Maybe Type
refTypeLookup t env =
  let
    cls = fmap cname $ M.lookup (getId t) $ class_table env
    trait = fmap traitName $ M.lookup (getId t) $ trait_table env
  in
    cls <|> trait

refTypeLookupUnsafe :: Type -> Environment -> Type
refTypeLookupUnsafe t env =
  let
    ret = refTypeLookup t env
    found = isJust ret
  in
    if found then
      fromJust ret
    else
      error $ printf "Can't find ref type %s" $ show t

refTypeParameters :: Type -> Environment -> [Type]
refTypeParameters t env =
  let Just t' = refTypeLookup t env
  in getTypeParameters t'

varLookup :: Name -> Environment -> Maybe Type
varLookup x env = case lookup x (locals env) of
                       Nothing -> lookup x (globals env)
                       result -> result

isLocal :: Name -> Environment -> Bool
isLocal x env = isJust $ lookup x (locals env)

typeVarLookup :: Type -> Environment -> Maybe Type
typeVarLookup ty env
    | isTypeVar ty = lookup ty (bindings env)
    | otherwise    = error
      "Tried to lookup the binding of something that was not a type variable"

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) env =
    extendEnvironment newTypes $ env {locals = extend (locals env) name ty}
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):locals) name' ty'
          | name == name' = (name', ty'):locals
          | otherwise     = (name, ty):(extend locals name' ty')

addTypeParameters :: [Type] -> Environment -> Environment
addTypeParameters [] env = env
addTypeParameters xs env@(Env{typeParameters}) =
  if all isTypeVar xs then
    env{typeParameters = xs ++ typeParameters}
  else
    error $ printf "'%s' is not a type parameter" $
      show $ head $ filter (not.isTypeVar) xs

bindType :: Type -> Type -> Environment -> Environment
bindType var ty env
    | isTypeVar var = env {bindings = (var, ty) : (bindings env)}
    | otherwise     = error "Tried to bind something that was not a type variable"

bindTypes :: [(Type, Type)] -> Environment -> Environment
bindTypes bindings env = foldr (\(tyVar, ty) env -> bindType tyVar ty env) env bindings

replaceLocals :: VarTable -> Environment -> Environment
replaceLocals newTypes env = env {locals = newTypes}

merge_envs :: [Either TCError Environment] -> Either TCError Environment
merge_envs envs = foldr merge (return empty_env) envs
  where
    merge :: Either TCError Environment -> Either TCError Environment
             -> Either TCError Environment
    merge e1 e2 = do
      Env{class_table=class_table, trait_table=trait_table,
        globals=globs, locals=locals,
        bindings=binds, typeParameters=tparams, bt=bt} <- e1
      Env{class_table=class_table', trait_table=trait_table',
        globals=globs', locals=locals',
        bindings=binds', typeParameters=tparams', bt=bt} <- e2
      return $ Env{
        class_table = M.union class_table class_table',
        trait_table = M.union trait_table trait_table',
        globals=(mergeGlobals globs globs'),
        locals=(mergeLocals locals locals'),
        bindings=(mergeBindings binds binds'),
        typeParameters=(mergeTypeParams tparams tparams'),
        bt=emptyBT
      }
    -- TODO: Be smarter and detect errors
    mergeGlobals = (++)
    mergeLocals = (++)
    mergeBindings = (++)
    mergeTypeParams = (++)
