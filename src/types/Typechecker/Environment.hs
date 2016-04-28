{-|

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings and
typevar-type bindings for doing lookups, as well as the
'Typechecker.TypeError.Backtrace' used for error handling.

-}

module Typechecker.Environment(Environment,
                               ModuleMap,
                               Result,
                               Whereabouts(..),
                               ModuleEnvironment,
                               result,
                               emptyEnv,
                               buildEnvironment,
                               applyImportRestrictions,
                               applyExportRestrictions,
                               squash,
                               allNames,
                               pullLocal,
                               traitLookup,
                               refTypeLookup,
                               methodAndCalledTypeLookup,
                               fieldLookup,
                               capabilityLookup,
                               varLookup,
                               checkImports,
                               checkExports,
                               isLocal,
                               typeVarLookup,
                               extendEnvironment,
                               addParams,
                               addTypeParameters,
                               typeParameters,
                               bindTypes,
                               bindings,
                               backtrace,
                               currentMethod,
                               currentModule,
                               modulePaths,
                               pushBT) where

import Data.List
import Data.Maybe
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)
import Data.Bifunctor(second)

-- Module dependencies
import Identifiers
import AST.AST hiding(showWithKind)
import Types
import Typechecker.TypeError


type VarTable = [(Name, Type)]

type ModuleMap = Map Path Program

data Environment = GE {modules :: [(Path, ModuleEnvironment)],
                       mlocal :: ModuleEnvironment,  -- global frame for the current module
                       current :: Path,
                       local  :: LocalEnvironment} deriving Show

emptyEnv :: Environment
emptyEnv = GE{modules=[], current=Path [], mlocal=emptyModuleEnv, local=emptyLocalEnv}

data Whereabouts = Global  -- defined in a global name space
                 | Local   -- defined in some local scope
  deriving (Show,Eq)

type Result a = (Path, Whereabouts, a)

result (_,_,c) = c

data ModuleEnvironment = MEnv {
    typeSynonymTableME :: Map Name Typedef,
    classTableME       :: Map Name ClassDecl,
    traitTableME       :: Map Name TraitDecl,
    functionTableME    :: Map Name Type} 
  | FilteredEnv {
    keptNames :: [Name],
    inner :: ModuleEnvironment }
  | HidingEnv {
    hiddenNames :: [Name],
    inner :: ModuleEnvironment }
  | RenameEnv {
    fullName :: Path,
    inner :: ModuleEnvironment }
  | QualifiedEnv {
    inner :: ModuleEnvironment
  }
  deriving Show

typeSynonymTable = mlookup typeSynonymTableME
functionTable = mlookup functionTableME
classTable = mlookup classTableME
traitTable = mlookup traitTableME

typeSynonymTableLookup path ty env = Map.lookup (getName ty) $ typeSynonymTable path env
classTableLookup path ty env = Map.lookup (getName ty) $ classTable path env
traitTableLookup path ty env = Map.lookup (getName ty) $ traitTable path env
functionTableLookup path n env = Map.lookup n $ functionTable path env

mlookup :: (ModuleEnvironment -> Map Name a) -> Path -> ModuleEnvironment -> Map Name (Path, a)
mlookup f p env@MEnv{} = Map.map (\x -> (p,x)) (f env)
mlookup f p env@FilteredEnv{keptNames,inner} = 
    Map.filterWithKey (\k _ -> k `elem` keptNames) $ mlookup f p inner
mlookup f p env@HidingEnv{hiddenNames,inner} =
    Map.filterWithKey (\k _ -> not $ k `elem` hiddenNames) $ mlookup f p inner
mlookup f p env@RenameEnv{fullName,inner} = mlookup f fullName inner
mlookup f p env@QualifiedEnv{inner} = mlookup f p inner

isQualified :: ModuleEnvironment -> Bool
isQualified MEnv{} = False
isQualified QualifiedEnv{} = True
isQualified FilteredEnv{inner} = isQualified inner
isQualified RenameEnv{inner} = isQualified inner
isQualified HidingEnv{inner} = isQualified inner

emptyModuleEnv = MEnv {
  typeSynonymTableME = Map.empty,
  classTableME = Map.empty,
  traitTableME = Map.empty,
  functionTableME = Map.empty
  }

data LocalEnvironment = LEnv {
  locals   :: Map Name Type,
  bindingsLE :: [(Type, Type)],
  typeParametersLE :: [Type],
  bt :: Backtrace
} deriving Show

emptyLocalEnv = LEnv {
  locals = Map.empty,
  bindingsLE = [],
  typeParametersLE = [],
  bt = emptyBT
}


-- TODO: rationalise the paths vs not paths, localise, lookup via paths etc etc
-- by moving it to lowest level
-- rationalise the crappy parameter passing
newLookup :: (Path -> Type -> ModuleEnvironment -> Maybe (Path, a)) -> Type -> Environment -> [Result a]
newLookup f ty env =
  removeDuplicates $
  case decompose $ getId ty of
    (Nothing, _) -> catMaybes $ 
        map (\(namespace, lenv) -> lift $ f namespace ty lenv) (unQualifiedModules env)
    (Just p, _) -> do
      (path, candidateModule) <- allLookup p $ allModules env
      lift2 $ f path ty candidateModule

lift Nothing = Nothing
lift (Just (ns, res)) = Just (ns, Global, res)

lift2 Nothing = []
lift2 (Just (actualPath, res)) = [(actualPath, Global, res)]

-- Utility functions
lmap :: (LocalEnvironment -> LocalEnvironment) -> Environment -> Environment
lmap f g@GE{local} = g{local=f local}

buildEnvironment :: Environment -> (Path, Bool) -> Program -> (Either TCError Environment, [TCWarning])
buildEnvironment g (name, self) p = (Right $ env, [])
  where
    localMEnv = buildModuleEnvironment p
    preEnv = g{current=name, mlocal=localMEnv, local=emptyLocalEnv}
    env = if self then squash preEnv (name, localMEnv) else preEnv
    
    buildModuleEnvironment :: Program -> ModuleEnvironment
    buildModuleEnvironment Program{typedefs, functions, classes, traits} =
            MEnv {
               typeSynonymTableME = Map.fromList [(getName (typedefdef t), t) | t <- typedefs],
               classTableME = Map.fromList [(getName (cname c), c) | c <- classes],
               traitTableME = Map.fromList [(getName (tname t), t) | t <- traits],
               functionTableME = Map.fromList $ map getFunctionType functions }
    getFunctionType f =
        let funname   = unsafeBase 7 $ functionName f
            funparams = functionParams f
            funtype   = functionType f
        in
            (funname, arrowType (map ptype funparams) funtype)


allNames :: ModuleEnvironment -> [Identifiers.Name]
allNames FilteredEnv{inner} = allNames inner
allNames HidingEnv{inner} = allNames inner
allNames MEnv{typeSynonymTableME, classTableME, traitTableME, functionTableME}
  = Map.keys typeSynonymTableME ++ Map.keys classTableME 
    ++ Map.keys traitTableME ++ Map.keys functionTableME

-- find a good place for this
-- TODO: this will not work well for multiple imports and qualified imports
applyImportRestrictions :: [ImportDecl] -> Environment -> Environment
applyImportRestrictions imports env@GE{modules} = env{modules=newModules}
  where
    newModules = concat $ map importOne imports

    importOne Import{itarget,iimports,ihiding,irename,iqualified} =
        let rawModule = fromJust $ lookup itarget modules
            qname = case irename of
              Nothing -> itarget
              Just name -> Path [name]
        in map (qualifyEnv iqualified) $ renameEnv (isJust irename) qname itarget $ 
                  hideEnv ihiding $ filterEnv iimports rawModule
                  
    hideEnv Nothing env = env
    hideEnv (Just names) env = HidingEnv{hiddenNames = names, inner = env}

    qualifyEnv False binding = binding
    qualifyEnv True  (path,env) = (path, QualifiedEnv{inner = env})

    renameEnv False _ path  env = [(path, env)]
    renameEnv True alias path env = [(alias, RenameEnv {fullName = path, inner=env}), (path, env)]
 
removeDuplicates :: [Result a] -> [Result a]
removeDuplicates = nubBy (\(a,_,_) (b,_,_) -> a == b)

filterEnv :: Maybe [Name] -> ModuleEnvironment -> ModuleEnvironment
filterEnv Nothing env = env
filterEnv (Just names) env = FilteredEnv{keptNames = names, inner = env}

applyExportRestrictions :: Maybe [Name] -> ModuleEnvironment -> ModuleEnvironment
applyExportRestrictions = filterEnv

squash :: Environment -> (Path, ModuleEnvironment) -> Environment
squash env@GE{modules} (path@(Path names), menv) = 
    if null names then env 
    else env{modules = (path, menv):modules}

pullLocal :: Environment -> (Path, ModuleEnvironment)
pullLocal GE{current, mlocal} = (current, mlocal)

allModules :: Environment -> [(Path, ModuleEnvironment)]
allModules GE{modules, current, mlocal} = (current, mlocal):modules

unQualifiedModules :: Environment -> [(Path, ModuleEnvironment)]
unQualifiedModules env = filter (not . isQualified . snd) (allModules env)

allLookup :: Path -> [(Path, ModuleEnvironment)] -> [(Path, ModuleEnvironment)] 
allLookup path menv = filter (\(p,e) -> p == path) menv

pushBT :: Pushable a => a -> Environment -> Environment
pushBT a = lmap (pushBTLE a)
  where 
    pushBTLE :: Pushable a => a -> LocalEnvironment -> LocalEnvironment
    pushBTLE x env@LEnv{bt} = env{bt = push x bt}

backtrace :: Environment -> Backtrace
backtrace = bt . local

currentModule :: Environment -> Path
currentModule = current

currentMethod :: Environment -> Maybe MethodDecl
currentMethod = currentMethodFromBacktrace . bt . local

fieldLookup :: Type -> Name -> Environment -> Maybe FieldDecl
fieldLookup ty f env
  | isTraitType ty = do
    trait <- convert $ traitLookup ty env
    fld <- find ((== f) . fname) $ requiredFields trait
    let bindings = formalBindings (tname trait) ty
        ftype' = replaceTypeVars bindings $ ftype fld
    return fld{ftype = ftype'}
  | isClassType ty = do
    cls <- convert $ classLookup ty env
    fld <- find ((== f) . fname) $ cfields cls
    let bindings = formalBindings (cname cls) ty
        ftype' = replaceTypeVars bindings $ ftype fld
    return fld{ftype = ftype'}
  | otherwise = error $ "Trying to lookup field in a non ref type " ++ show ty

traitMethodLookup :: Type -> Name -> Environment -> Maybe (FunctionHeader Name)
traitMethodLookup ty m env = do
  trait <- convert $ traitLookup ty env
  let headers = requiredMethods trait ++ map mheader (tmethods trait)
  header <- find (matchHeader m) headers
  let bindings = formalBindings (tname trait) ty
  return $ replaceHeaderTypes bindings header

classMethodLookup :: Type -> Name -> Environment -> Maybe (FunctionHeader Name)
classMethodLookup ty m env = do
  cls <- convert $ classLookup ty env
  traits <- mapM (\t -> convert $ traitLookup t env) $
    typesFromCapability $ ccapability cls
  let
    headers = map mheader $ cmethods cls ++ concatMap tmethods traits
  header <- find (matchHeader m) headers
  let bindings = formalBindings (cname cls) ty
  return $ replaceHeaderTypes bindings header

methodAndCalledTypeLookup ::
    Type -> Name -> Environment -> Maybe (FunctionHeader Name, Type)
methodAndCalledTypeLookup ty m env
    | isCapabilityType ty = do
        let traits = typesFromCapability ty
            results = map (\t -> (traitMethodLookup t m env, t)) traits
        (ret, ty') <- find (isJust . fst) results
        return (fromJust ret, ty')
    | isUnionType ty = do
        let members = unionMembers ty
        mapM_ (\t -> methodLookup t m env) members
        methodAndCalledTypeLookup (head members) m env
    | otherwise = do
        ret <- methodLookup ty m env
        return (ret, ty)

methodLookup :: Type -> Name -> Environment -> Maybe (FunctionHeader Name)
methodLookup ty m env
  | isClassType ty =
    classMethodLookup ty m env
  | isTraitType ty =
    traitMethodLookup ty m env
  | isCapabilityType ty = do
    let traits = typesFromCapability ty
        ms = map (\t -> traitMethodLookup t m env) traits
    ret <- find isJust ms
    return $ fromJust ret
  | otherwise = Nothing


capabilityLookup :: Type -> Environment -> [Result Type]
capabilityLookup = newLookup capabilityLookupLE
  where
    capabilityLookupLE :: Path -> Type -> ModuleEnvironment -> Maybe (Path, Type)
    capabilityLookupLE path ty env
        | isClassType ty = do
            (actualPath, cls) <- classTableLookup path ty env
            let bindings = formalBindings (cname cls) ty
            return $ (actualPath, replaceTypeVars bindings $ ccapability cls)
        | otherwise = error $ "Environment.hs: Tried to look up the capability " ++
                              "of non-class type " ++ show ty

-- TODO: move to places where it's used.
convert :: [Result a] -> Maybe a
convert = listToMaybe . map result

classLookup :: Type -> Environment -> Maybe ClassDecl
classLookup cls env
    | isRefAtomType cls = Map.lookup (getId cls) $ classTable env
    | isTypeSynonym cls = classLookup (unfoldTypeSynonyms cls) env -- TODO: remove!!
    | otherwise = error $
      "Tried to lookup the class of '" ++ show cls
      ++ "' which is not a reference type"

traitLookup :: Type -> Environment -> [Result TraitDecl]
traitLookup ty env = newLookup traitTableLookup ty env

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

refTypeLookup :: Type -> Environment -> [Result Type]
refTypeLookup = newLookup refTypeLookupLE
  where
    refTypeLookupLE :: Path -> Type -> ModuleEnvironment -> Maybe (Path, Type)
    refTypeLookupLE path t env =
      let
        typeSyn = fmap (second typedefdef) $ typeSynonymTableLookup path t env
        cls = fmap (second cname) $ classTableLookup path t env
        trait = fmap (second tname) $ traitTableLookup path t env 
      in
        cls <|> trait <|> typeSyn

varLookup :: Path -> Environment -> [Result Type]
varLookup p env =
  removeDuplicates $
  if qualified p then 
      let (Just path, base) = decompose p in
        do
          (actualPath, candidateModule) <- allLookup path $ allModules env
          lift2 $ functionTableLookup actualPath base candidateModule
  else
    let b = unsafeBase 10 p in
    case Map.lookup b (locals $ local env) of
      Just result -> [(Path [], Local, result)]
      Nothing -> catMaybes $
        map (\(path, lenv) -> lift $ functionTableLookup path b lenv) (unQualifiedModules env)

isLocal :: Name -> Environment -> Bool
isLocal n env = isJust $ Map.lookup n (locals $ local env)

typeVarLookup :: Type -> Environment -> Maybe Type
typeVarLookup t = typeVarLookupLE t . local
  where
    typeVarLookupLE :: Type -> LocalEnvironment -> Maybe Type
    typeVarLookupLE ty env
        | isTypeVar ty = lookup ty (bindingsLE env)
        | otherwise    = error
          "Tried to lookup the binding of something that was not a type variable"

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment pairs = lmap (extendEnvironmentLE pairs)

extendEnvironmentLE :: [(Name, Type)] -> LocalEnvironment -> LocalEnvironment
extendEnvironmentLE [] env = env
extendEnvironmentLE ((name, ty):newTypes) env = 
    extendEnvironmentLE newTypes $ env {locals = Map.insert name ty (locals env)}

-- | Convenience function for extending the environment with a
-- list of parameter declarations
addParams :: [ParamDecl] -> Environment -> Environment
addParams pd = lmap (addParamsLE pd)
  where
    addParamsLE :: [ParamDecl] -> LocalEnvironment -> LocalEnvironment
    addParamsLE = extendEnvironmentLE . map (\(Param {pname, ptype}) -> (pname, ptype))

typeParameters :: Environment -> [Type]
typeParameters = typeParametersLE . local

bindings :: Environment -> [(Type, Type)]
bindings = bindingsLE . local

addTypeParameters :: [Type] -> Environment -> Environment
addTypeParameters tl = lmap (addTypeParametersLE tl)
  where
    addTypeParametersLE :: [Type] -> LocalEnvironment -> LocalEnvironment
    addTypeParametersLE [] env = env
    addTypeParametersLE xs env@(LEnv{typeParametersLE}) =
      if all isTypeVar xs then
        env{typeParametersLE = xs ++ typeParametersLE}
      else
        error $ printf "'%s' is not a type parameter" $
          show $ head $ filter (not.isTypeVar) xs

bindTypes :: [(Type, Type)] -> Environment -> Environment
bindTypes bindings = lmap (bindTypesLE bindings)
  where
    bindTypesLE :: [(Type, Type)] -> LocalEnvironment -> LocalEnvironment
    bindTypesLE bindings env = foldr (\(tyVar, ty) env -> bindType tyVar ty env) env bindings

formalBindings :: Type -> Type -> [(Type, Type)]
formalBindings formal actual
    | isRefType formal && isRefType actual =
        let formals = getTypeParameters formal
            actuals = getTypeParameters actual
        in
          zip formals actuals
    | otherwise =
        error $ "Environment.hs: Tried to get bindings from " ++
                showWithKind formal ++ " and " ++ showWithKind actual

matchHeader :: Name -> (FunctionHeader Name) -> Bool
matchHeader m = (==m) . hname

classLookup :: Type -> Environment -> [Result ClassDecl]
classLookup = newLookup classLookupLE
  where
    classLookupLE :: Path -> Type -> ModuleEnvironment -> Maybe (Path, ClassDecl)
    classLookupLE path cls env
      | isRefType cls = classTableLookup path cls env
      | otherwise = error $
        "Tried to lookup the class of '" ++ show cls
        ++ "' which is not a reference type"

bindType :: Type -> Type -> LocalEnvironment -> LocalEnvironment
bindType var ty env
    | isTypeVar var = env {bindingsLE = (var, ty) : bindingsLE env}
    | otherwise     = error "Tried to bind something that was not a type variable"

-- TODO: find a good home for this
namesNotInEnv :: Maybe [Identifiers.Name] -> ModuleEnvironment -> [Identifiers.Name]
namesNotInEnv Nothing menv = []
namesNotInEnv (Just names) menv = names \\ allNames menv

checkImports :: [ImportDecl] -> Environment -> Maybe TCError
checkImports imports env = firstError $ map checkOneImport imports
  where
    checkOneImport Import{itarget,iimports,ihiding} = checkModule mod 
      where
        mod = fromJust $ lookup itarget $ modules env
        checkModule mod = 
            let danglingImports = namesNotInEnv iimports mod 
                danglingHidings = namesNotInEnv ihiding mod in
            if not $ null danglingImports then Just $ TCError (ImportedNamesNotFound danglingImports) emptyBT -- TODO: better information when right home is found
            else if not $ null danglingHidings then Just $ TCError (HiddenNamesNotFound danglingHidings) emptyBT -- TODO: better information when right home is found
            else if not $ null $ iimports `intersectM` ihiding then Just $ TCError (NamesBothImportedAndHidden $ iimports `intersectM` ihiding) emptyBT 
            else Nothing
    firstError [] = Nothing
    firstError (Nothing: l) = firstError l
    firstError (Just err: _) = Just err

intersectM :: Eq a => Maybe [a] -> Maybe [a] -> [a]
intersectM Nothing _ = []
intersectM _ Nothing = []
intersectM (Just a) (Just b) = a `intersect` b


checkExports :: Maybe [Name] -> ModuleEnvironment -> Maybe TCError
checkExports names menv =
  let missingNames = namesNotInEnv names menv in
  if not $ null missingNames 
  then Just $ TCError (ExportedNamesLackingBinding missingNames) emptyBT
  else Nothing
 
modulePaths :: Environment -> [Path]
modulePaths = map fst . modules

showMEnv :: ModuleEnvironment -> String
showMEnv MEnv { typeSynonymTableME, classTableME, traitTableME, functionTableME} = 
        "Type Synonyms\n" ++ show (Map.keys typeSynonymTableME) ++ 
        "\n\n Classes\n" ++ show (Map.keys classTableME) ++
        "\n\n Traits\n" ++ show (Map.keys traitTableME) ++
        "\n\n Functions\n" ++ show (Map.keys functionTableME)    
showMEnv FilteredEnv{inner} = showMEnv inner
showMEnv HidingEnv{inner} = showMEnv inner
showMEnv RenameEnv{fullName, inner} = "Renaming = " ++ show fullName ++ "\n" ++ showMEnv inner
showMEnv QualifiedEnv{inner} = showMEnv inner

showEnv :: Environment -> String
showEnv GE{modules} = concat (map showOne modules)
  where showOne (path, menv) = "\nModule Name: " ++ show path ++ "\n" ++ showMEnv menv ++ "\n"
