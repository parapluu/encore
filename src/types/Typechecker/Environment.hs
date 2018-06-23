{-|

The environment used by "Typechecker.Typechecker". Contains a
class table and a list of local name-type bindings and
typevar-type bindings for doing lookups, as well as the
'Typechecker.TypeError.Backtrace' used for error handling.

-}

module Typechecker.Environment where

import Data.List
import Data.Maybe
import Data.Tuple
import Control.Applicative ((<|>), (<$>))
import Control.Monad
import Control.Arrow(first, second, (***), (&&&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

import Debug.Trace

-- Module dependencies
import Identifiers
import AST.AST hiding(showWithKind)
import Types
import Typechecker.Backtrace

data LookupTable = LookupTable {
   sourceFile       :: FilePath
  ,isQualified      :: Bool
  ,allNames         :: [Name]
  ,selectiveExports :: Maybe [Name]
  ,typeSynonymTable :: Map String Typedef
  ,classTable       :: Map String ClassDecl
  ,traitTable       :: Map String TraitDecl
  ,functionTable    :: Map Name   Type
}

buildLookupTable :: Program -> LookupTable
buildLookupTable Program{source
                        ,moduledecl
                        ,typedefs
                        ,functions
                        ,classes
                        ,traits
                        } =
  let typeSynonymTable =
        Map.fromList [(getId (typedefdef t), t) | t <- typedefs]
      classTable =
        Map.fromList [(getId (cname c), c) | c <- classes]
      traitTable =
        Map.fromList [(getId (tname t), t) | t <- traits]
      functionTable =
        Map.fromList [(functionName f, getFunctionType f) | f <- functions]
      allNames = map Name (Map.keys typeSynonymTable ++
                           Map.keys classTable ++
                           Map.keys traitTable) ++
                 Map.keys functionTable
  in LookupTable {
           sourceFile = source,
           isQualified = False,
           allNames,
           selectiveExports = moduleExports moduledecl,
           typeSynonymTable,
           classTable,
           traitTable,
           functionTable
     }
  where
    getFunctionType f =
        let funparams = functionParams f
            funtypeparams = functionTypeParams f
            funtype   = functionType f
        in
          arrowWithTypeParam funtypeparams (map ptype funparams) funtype


type VarTable = [(Name, (Mutability, Type))]

data Environment = Env {
  defaultNamespace :: Namespace,
  lookupTables   :: Map Namespace LookupTable,
  abstractTraitTable :: Map String TraitDecl,
  namespaceTable :: Map FilePath Namespace,
  locals         :: VarTable,
  bindings       :: [(Type, Type)],
  typeParameters :: [Type],
  bt             :: Backtrace
}

emptyEnv = Env {
  defaultNamespace = emptyNamespace,
  lookupTables = Map.empty,
  namespaceTable = Map.empty,
  abstractTraitTable = Map.empty,
  locals = [],
  bindings = [],
  typeParameters = [],
  bt = emptyBT
}

buildEnvironment :: Map FilePath LookupTable -> Program -> Environment
buildEnvironment tables Program{source, imports, moduledecl} =
  let defaultNamespace =
        if moduledecl == NoModule
        then emptyNamespace
        else explicitNamespace [moduleName moduledecl]
      defaultTable = tables Map.! source
      defaultAssoc =
        (defaultNamespace, defaultTable{selectiveExports = Nothing})
      nonLocalLookupTables = filter ((/= source) . fst) $ Map.assocs tables
      importedLookupTables =
        defaultAssoc :
        concatMap (performImport imports) nonLocalLookupTables
      lookupTables =
        Map.fromList importedLookupTables
      namespaceTable =
        Map.fromList $ map (first sourceFile . swap) importedLookupTables
  in
  Env {
     defaultNamespace
    ,lookupTables
    ,namespaceTable
    ,abstractTraitTable = Map.empty
    ,locals = []
    ,bindings = []
    ,typeParameters = []
    ,bt = emptyBT
  }
  where
    performImport :: [ImportDecl] -> (FilePath, LookupTable) ->
                     [(Namespace, LookupTable)]
    performImport imports (source, table) =
      case filter ((Just source ==) . isource) imports of
        [] -> [(implicitNamespace source, table)]
        l -> concatMap performSingleImport l
      where
        performSingleImport Import{itarget
                                  ,iqualified
                                  ,ihiding
                                  ,iselect
                                  ,ialias
                                  } =
          let lookupTable@LookupTable{classTable
                                     ,traitTable
                                     ,typeSynonymTable
                                     ,functionTable} = table
              classTable' = selectiveImport iselect ihiding classTable
              traitTable' = selectiveImport iselect ihiding traitTable
              typeSynonymTable' =
                selectiveImport iselect ihiding typeSynonymTable
              functionTable' = selectiveImport' iselect ihiding functionTable
              lookupTable' = lookupTable{isQualified = iqualified
                                        ,classTable = classTable'
                                        ,traitTable = traitTable'
                                        ,typeSynonymTable = typeSynonymTable'
                                        ,functionTable = functionTable'
                                        }
          in (itarget, lookupTable'):
             case ialias of
               Just alias -> [(alias, lookupTable')]
               Nothing -> []
          where
            selectiveImport ::
              Maybe [Name] -> Maybe [Name] -> Map String a -> Map String a
            selectiveImport select hiding =
              Map.filterWithKey (\k _ -> importCond select hiding (Name k))

            selectiveImport' ::
              Maybe [Name] -> Maybe [Name] -> Map Name a -> Map Name a
            selectiveImport' select hiding =
              Map.filterWithKey (\k _ -> importCond select hiding k)

            importCond :: Maybe [Name] -> Maybe [Name] -> Name -> Bool
            importCond select hiding k =
              let selectCond = maybe True (k `elem`) select
                  hidingCond = maybe True (k `notElem`) hiding
              in selectCond && hidingCond

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env@Env{bt} = env{bt = push x bt}

backtrace = bt

currentExecutionContext :: Environment -> ExecutionContext
currentExecutionContext = currentContextFromBacktrace . bt

formalBindings :: Type -> Type -> [(Type, Type)]
formalBindings formal actual
    | isRefAtomType formal && isRefAtomType actual =
        let formals = getTypeParameters formal
            actuals = getTypeParameters actual
        in
          zip formals actuals
    | otherwise =
        error $ "Environment.hs: Tried to get bindings from " ++
                showWithKind formal ++ " and " ++ showWithKind actual

isKnownName :: Namespace -> Name -> Environment -> Bool
isKnownName ns name Env{lookupTables} =
  name `elem` allNames (lookupTables Map.! ns)

fieldLookup :: Type -> Name -> Environment -> Maybe FieldDecl
fieldLookup ty f env@Env{namespaceTable}
  | isTraitType ty =
    case traitLookup ty env of
      Just [trait] -> do
        fld <- find ((== f) . fname) $ requiredFields trait
        let bindings = formalBindings (tname trait) ty
            ftype' = translateTypeNamespace namespaceTable $
                     replaceTypeVars bindings $ ftype fld
        return fld{ftype = ftype'}
      Just l ->
        error $ "Environment.hs: Ambiguous target of field lookup. \n" ++
                "Possible targets are: " ++ show l
      Nothing ->
        error $
          printf "Environment.hs: Tried to lookup field %s in unresolved trait %s"
                 (show f) (show ty)
  | isClassType ty =
    case classLookup ty env of
      Just [cls] -> do
        fld <- find ((== f) . fname) $ cfields cls
        let bindings = formalBindings (cname cls) ty
            ftype' = translateTypeNamespace namespaceTable $
                     replaceTypeVars bindings $ ftype fld
        return fld{ftype = ftype'}
      Just l ->
        error $ "Environment.hs: Ambiguous target of field lookup. \n" ++
                "Possible targets are: " ++ show l
      Nothing ->
        error $
          printf "Environment.hs: Tried to lookup field %s in unresolved class %s"
                 (show f) (show ty)
  | otherwise = error $ "Trying to lookup field in a non ref type " ++ show ty

matchHeader :: Name -> FunctionHeader -> Bool
matchHeader m = (==m) . hname

traitMethodLookup :: Type -> Name -> Environment -> Maybe FunctionHeader
traitMethodLookup ty m env@Env{namespaceTable} =
  case traitLookup ty env of
    Just [trait] -> do
      let headers = requiredMethods trait ++ map mheader (tmethods trait)
          bindings = formalBindings (tname trait) ty
      header <- find (matchHeader m) headers
      return $ translateHeaderNamespace namespaceTable $
               replaceHeaderTypes bindings header
    Just l ->
      error $ "Environment.hs: Ambiguous target of trait method lookup. \n" ++
              "Possible targets are: " ++ show l
    Nothing ->
      error $
        printf "Environment.hs: Tried to lookup method %s in unresolved trait %s"
               (show m) (show ty)

classMethodLookup :: Type -> Name -> Environment -> Maybe FunctionHeader
classMethodLookup ty m env@Env{namespaceTable} =
  case classLookup ty env of
    Just [cls] -> do
      let headers = map mheader $ cmethods cls
          bindings = formalBindings (cname cls) ty
          tys = typesFromTraitComposition $ ccomposition cls
          classResults = find (matchHeader m) headers
          traitResults = msum (map (\ty -> traitMethodLookup ty m env) tys)
      header <- classResults <|> traitResults
      return $ translateHeaderNamespace namespaceTable $
               replaceHeaderTypes bindings header
    Just l ->
      error $ "Environment.hs: Ambiguous target of class method lookup. \n" ++
              "Possible targets are: " ++ show l
    Nothing ->
      error $
        printf "Environment.hs: Tried to lookup method %s in unresolved class %s"
               (show m) (show ty)

methodAndCalledTypeLookup ::
    Type -> Name -> Environment -> Maybe (FunctionHeader, Type)
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

methodLookup :: Type -> Name -> Environment -> Maybe FunctionHeader
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

capabilityLookup :: Type -> Environment -> Maybe Type
capabilityLookup ty env@Env{namespaceTable}
    | isClassType ty = do
        classes <- classLookup ty env
        case classes of
          [] -> return incapability -- Unimported class
          [cls] -> do
            let bindings = formalBindings (cname cls) ty
            return $ replaceTypeVars bindings $
                     capabilityFromTraitComposition (ccomposition cls)
          _ -> error $ "Environment.hs: Tried to look up the capability of " ++
                       "ambiguous type " ++ show ty
    | otherwise = error $ "Environment.hs: Tried to look up the capability " ++
                          "of non-class type " ++ show ty

fromSameSource :: Map Namespace LookupTable -> (Namespace, a) -> (Namespace, a)
                  -> Bool
fromSameSource tables (ns1, _) (ns2, _) =
  sourceFile (tables Map.! ns1) == sourceFile (tables Map.! ns2)

extractTables :: (LookupTable -> Map a b) -> Map Namespace LookupTable ->
                 [(Namespace, Map a b)]
extractTables extractMap =
  map (second extractMap) .
  filter (not . isQualified . snd) .
  filter (isExplicitNamespace . fst) .
  Map.assocs


traitLookup :: Type -> Environment -> Maybe [TraitDecl]
traitLookup t Env{defaultNamespace, lookupTables, abstractTraitTable}
    | isAbstractTraitType t = do
        result <- Map.lookup (getId t) abstractTraitTable
        return [result]
    | isRefAtomType t =
        case getRefNamespace t of
          Just ns -> do
            let key = if isEmptyNamespace ns
                      then defaultNamespace
                      else ns
            table <- Map.lookup key lookupTables
            let results = maybeToList $
                          Map.lookup (getId t) $
                          filterTraitTable table
            return $ map (setNamespace key) results
          Nothing -> do
            let traitTables = extractTables filterTraitTable lookupTables
                results = map (second (Map.lookup $ getId t)) traitTables
                hits = nubBy (fromSameSource lookupTables) $
                       map (second fromJust) $
                       filter (isJust . snd) results
            return $ map (uncurry setNamespace) hits
    | otherwise = error $
      "Tried to lookup the trait of '" ++ show t
      ++ "' which is not a reference type"
    where
      setNamespace ns t@Trait{tname} = t{tname = setRefNamespace ns tname}
      filterTraitTable LookupTable{traitTable
                                  ,selectiveExports = Nothing
                                  } =
        traitTable
      filterTraitTable LookupTable{traitTable
                                  ,selectiveExports = Just names
                                  } =
        Map.filterWithKey (\t _ -> Name t `elem` names) traitTable

abstractTraitLookup :: Type -> Environment -> Maybe TraitDecl
abstractTraitLookup t env =
  Map.lookup (getId t) $ abstractTraitTable env



classLookup :: Type -> Environment -> Maybe [ClassDecl]
classLookup cls Env{defaultNamespace, lookupTables, namespaceTable}
    | isRefAtomType cls =
        case getRefNamespace cls of
          Just ns -> do
            let key = if isEmptyNamespace ns
                      then defaultNamespace
                      else ns
            table <- Map.lookup key lookupTables
            let results = maybeToList $
                          Map.lookup (getId cls) $
                          filterClassTable table
            return $ map (setNamespace key) results
          Nothing -> do
            let classTables = extractTables filterClassTable lookupTables
                results = map (second (Map.lookup $ getId cls)) classTables
                hits = nubBy (fromSameSource lookupTables) $
                       map (second fromJust) $
                       filter (isJust . snd) results
            return $ map (uncurry setNamespace) hits
    | otherwise = error $
      "Tried to lookup the class of '" ++ show cls
      ++ "' which is not a reference type"
    where
      setNamespace ns c@Class{cname, ccomposition} =
          c{cname = setRefNamespace ns cname
           ,ccomposition = translateCompositionNamespace namespaceTable ccomposition}
      filterClassTable LookupTable{classTable
                                  ,selectiveExports = Nothing
                                  } =
        classTable
      filterClassTable LookupTable{classTable
                                  ,selectiveExports = Just names
                                  } =
        Map.filterWithKey (\c _ -> Name c `elem` names) classTable

typeSynonymLookup :: Type -> Environment -> Maybe [Typedef]
typeSynonymLookup t Env{defaultNamespace, lookupTables, namespaceTable}
    | isRefAtomType t =
        case getRefNamespace t of
          Just ns -> do
            let key = if isEmptyNamespace ns
                      then defaultNamespace
                      else ns
            table <- Map.lookup key lookupTables
            let results = maybeToList $
                          Map.lookup (getId t) $
                          filterTypeSynonymTable table
            return $ map (setNamespace key) results
          Nothing -> do
            let typeSynonymTables =
                  extractTables filterTypeSynonymTable lookupTables
                results = map (second (Map.lookup $ getId t)) typeSynonymTables
                hits = nubBy (fromSameSource lookupTables) $
                       map (second fromJust) $
                       filter (isJust . snd) results
            return $ map (uncurry setNamespace) hits
    | otherwise = error $
      "Tried to lookup the type synonym of '" ++ show t
      ++ "' which is not a reference type"
    where
      setNamespace ns t@Typedef{typedefdef} =
          t{typedefdef = translateTypeNamespace namespaceTable typedefdef}
      filterTypeSynonymTable LookupTable{typeSynonymTable
                                        ,selectiveExports = Nothing
                                        } =
        typeSynonymTable
      filterTypeSynonymTable LookupTable{typeSynonymTable
                                        ,selectiveExports = Just names
                                        } =
        Map.filterWithKey (\s _ -> Name s `elem` names) typeSynonymTable

refTypeLookup :: Type -> Environment -> Maybe [Type]
refTypeLookup t env = do
  cls <- map cname <$> classLookup t env
  trait <- map tname <$> traitLookup t env
  typeSyn <- map typedefdef <$> typeSynonymLookup t env
  return $ concat [cls, trait, typeSyn]

refTypeParameters :: Type -> Environment -> [Type]
refTypeParameters t env =
  let Just [t'] = refTypeLookup t env
  in getTypeParameters t'

varLookup :: QualifiedName -> Environment -> Maybe [(QualifiedName, Type)]
varLookup qname@QName{qnspace, qnlocal = x}
          Env{locals, defaultNamespace, lookupTables, namespaceTable} =
  case qnspace of
    Just ns -> do
      let key = if isEmptyNamespace ns
                then defaultNamespace
                else ns
      table <- Map.lookup key lookupTables
      let filteredTable = filterFunctionTable table
      result <- Map.lookup x filteredTable
      let source = sourceFile table
          result' = typeMap (translateTypeNamespace namespaceTable) result
      return [(setSourceFile source qname, result')]

    Nothing -> localLookup <|> globalSearch
  where
    localLookup = do
      (_, ty) <- lookup x locals
      return [(qname, ty)]

    globalSearch =
      let functionTables = extractTables filterFunctionTable lookupTables
          results = map (second (Map.lookup x)) functionTables
          hits = nubBy (fromSameSource lookupTables) $
                 map (second fromJust) $
                 filter (isJust . snd) results
          withTranslatedTypes =
            map (second (translateTypeNamespace namespaceTable)) hits
          withSources =
            map (first (sourceFile . (lookupTables Map.!))) withTranslatedTypes
          withNames = map (first resolveName) withSources
      in return withNames

    resolveName source =
      let ns = namespaceTable Map.! source
      in setSourceFile source $
         setNamespace ns qname

    filterFunctionTable LookupTable{functionTable
                                   ,selectiveExports = Nothing
                                   } =
      functionTable
    filterFunctionTable LookupTable{functionTable
                                   ,selectiveExports = Just names
                                   } =
      Map.filterWithKey (\f _ -> f `elem` names) functionTable

visibleFunctions :: Environment -> [(Name, Type)]
visibleFunctions Env{locals, lookupTables} =
  let
    ftable = extractTables filterFunctionTable lookupTables
    selfMadeFunc = filter (not . (`elem` ["Std", "String"]) . show . fst) ftable
    localFunc = map (\(x,(_,z)) -> (x,z)) $ filter (isArrowType . snd . snd) locals
  in
    localFunc ++ concatMap (Map.assocs . snd) selfMadeFunc

  where
    filterFunctionTable LookupTable{functionTable
                                   ,selectiveExports = Nothing
                                   } =
      functionTable
    filterFunctionTable LookupTable{functionTable
                                   ,selectiveExports = Just names
                                   } =
      Map.filterWithKey (\f _ -> f `elem` names) functionTable


isLocal :: QualifiedName -> Environment -> Bool
isLocal QName{qnspace = Nothing, qnlocal = x} Env{locals} =
    isJust $ lookup x locals
isLocal QName{qnspace = Just ns, qnlocal = x} Env{locals}
    | isEmptyNamespace ns = isJust $ lookup x locals
    | otherwise = False

isMutableLocal :: QualifiedName -> Environment -> Bool
isMutableLocal QName{qnspace = Nothing, qnlocal = x} Env{locals} =
  case lookup x locals of
    Just (mut, _) -> mut == Var
    Nothing -> False
isMutableLocal QName{qnspace = Just ns, qnlocal = x} Env{locals}
  | isEmptyNamespace ns =
      case lookup x locals of
        Just (mut, _) -> mut == Var
        Nothing -> False
  | otherwise = False

typeVarLookup :: Type -> Environment -> Maybe Type
typeVarLookup ty env
    | isTypeVar ty =
        lookup ty (bindings env) <|>
        find (==ty) (typeParameters env)
    | otherwise    = error
      "Tried to lookup the binding of something that was not a type variable"

extendEnvironment' :: Mutability -> [(Name, Type)] -> Environment -> Environment
extendEnvironment' mut [] env = env
extendEnvironment' mut ((name, ty):newTypes) env =
    extendEnvironment' mut newTypes $ env {locals = extend (locals env) name ty}
    where
      extend [] name' ty' = [(name', (mut, ty'))]
      extend ((name, tup):locals) name' ty'
          | name == name' = (name', (mut, ty')) : locals
          | otherwise     = (name, tup) : extend locals name' ty'

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment = extendEnvironment' Var

extendEnvironmentImmutable :: [(Name, Type)] -> Environment -> Environment
extendEnvironmentImmutable = extendEnvironment' Val

-- | Convenience function for extending the environment with a
-- list of parameter declarations
addParams :: [ParamDecl] -> Environment -> Environment
addParams params = extendEnvironmentImmutable vals .
                   extendEnvironment vars
    where
      (vals, vars) = map (pname &&& ptype) *** map (pname &&& ptype) $
                         partition ((== Val) . pmut) params

makeImmutable :: [Name] -> Environment -> Environment
makeImmutable names env@Env{locals} =
  env{locals = map (makeVarImmutable names) locals}
  where
    makeVarImmutable names (name, (mut, ty))
      | name `elem` names = (name, (Val, ty))
      | otherwise = (name, (mut, ty))

isLocalTypeParameter :: Type -> Environment -> Bool
isLocalTypeParameter ty Env{typeParameters}
  | isTypeVar ty = ty `elem` typeParameters
  | otherwise = error $ "Environment.hs: Asked if " ++ showWithKind ty ++
                        " is a type parameter"

dropLocal :: Name -> Environment -> Environment
dropLocal name env@Env{locals} = env{locals = filter ((/=name) .fst) locals}

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
    | isTypeVar var = env {bindings = (var, ty) : bindings env}
    | otherwise     = error "Tried to bind something that was not a type variable"

bindTypes :: [(Type, Type)] -> Environment -> Environment
bindTypes bindings env = foldr (\(tyVar, ty) env -> bindType tyVar ty env) env bindings

replaceLocals :: VarTable -> Environment -> Environment
replaceLocals newTypes env = env {locals = newTypes}

withAbstractTrait :: TraitDecl -> Environment -> Environment
withAbstractTrait tdecl env@Env{abstractTraitTable} =
    let key = getId (tname tdecl)
        abstractTraitTable' = Map.insert key tdecl abstractTraitTable
    in env{abstractTraitTable = abstractTraitTable'}

setTraitComposition :: Type -> Type -> Environment -> Environment
setTraitComposition c cap env@Env{defaultNamespace, lookupTables} =
  let table@LookupTable{classTable} = lookupTables Map.! defaultNamespace
      key = getId c
      cls = classTable Map.! key
      cls' = cls{ccomposition = Just $ traitCompositionFromCapability cap}
      table' = table{classTable = Map.insert key cls' classTable}
      lookupTables' = Map.insert defaultNamespace table' lookupTables
   in env{lookupTables = lookupTables'}
