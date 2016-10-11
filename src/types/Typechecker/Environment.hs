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
import Control.Arrow(first, second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Text.Parsec.Pos as P
import Debug.Trace

-- Module dependencies
import Identifiers
import AST.AST hiding(showWithKind)
import Types
import Typechecker.TypeError

data LookupTable = LookupTable {
   sourceFile       :: SourceName
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


type VarTable    = [(Name, Type)]

data Environment = Env {
  defaultNamespace :: Namespace,
  lookupTables   :: Map Namespace LookupTable,
  namespaceTable :: Map SourceName Namespace,
  locals         :: VarTable,
  bindings       :: [(Type, Type)],
  typeParameters :: [Type],
  bt             :: Backtrace
}

emptyEnv = Env {
  defaultNamespace = [],
  lookupTables = Map.empty,
  namespaceTable = Map.empty,
  locals = [],
  bindings = [],
  typeParameters = [],
  bt = emptyBT
}

buildEnvironment :: Map SourceName LookupTable -> Program -> Environment
buildEnvironment tables Program{source, imports, moduledecl} =
  let defaultNamespace =
        if moduledecl == NoModule
        then []
        else [moduleName moduledecl]
      defaultTable = tables Map.! source
      defaultAssoc =
        (defaultNamespace, defaultTable{selectiveExports = Nothing})
      assocList = defaultAssoc : concatMap (buildAssoc tables) imports
      lookupTables =
        Map.fromList assocList
      namespaceTable =
        Map.fromList (map (first sourceFile . swap) assocList)
  in
  Env {
     defaultNamespace
    ,lookupTables
    ,namespaceTable
    ,locals = []
    ,bindings = []
    ,typeParameters = []
    ,bt = emptyBT
  }
  where
    buildAssoc lookupTables Import{itarget
                                  ,iqualified
                                  ,ihiding
                                  ,iselect
                                  ,ialias
                                  ,isource = Just source} =
      let lookupTable@LookupTable{classTable
                                 ,traitTable
                                 ,typeSynonymTable
                                 ,functionTable} = lookupTables Map.! source
          classTable' = selectiveImport classTable
          traitTable' = selectiveImport traitTable
          typeSynonymTable' = selectiveImport typeSynonymTable
          functionTable' = selectiveImport' functionTable
          lookupTable' = lookupTable{isQualified = iqualified
                                    ,classTable = classTable'
                                    ,traitTable = traitTable'
                                    ,typeSynonymTable = typeSynonymTable'
                                    ,functionTable = functionTable'
                                    }
      in (itarget, lookupTable'):case ialias of
                                   Just alias -> [(alias, lookupTable')]
                                   Nothing -> []
      where
        selectiveImport :: Map String a -> Map String a
        selectiveImport = Map.filterWithKey importCond

        selectiveImport' :: Map Name a -> Map Name a
        selectiveImport' = Map.filterWithKey importCond'

        importCond :: String -> a -> Bool
        importCond k = importCond' (Name k)

        importCond' :: Name -> a -> Bool
        importCond' k _ =
          let selectCond = maybe True (k `elem`) iselect
              hidingCond = maybe True (k `notElem`) ihiding
          in selectCond && hidingCond

    buildAssoc _ Import{itarget} =
        error $ "Environment.hs: Import not resolved " ++ showNamespace itarget

pushBT :: Pushable a => a -> Environment -> Environment
pushBT x env@Env{bt} = env{bt = push x bt}

backtrace = bt

currentMethod :: Environment -> Maybe MethodDecl
currentMethod = currentMethodFromBacktrace . bt

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
fieldLookup ty f env
  | isTraitType ty =
    case traitLookup ty env of
      Just [trait] -> do
        fld <- find ((== f) . fname) $ requiredFields trait
        let bindings = formalBindings (tname trait) ty
            ftype' = replaceTypeVars bindings $ ftype fld
        return fld{ftype = ftype'}
      _ ->
        error "Environment.hs: Tried to do fieldLookup with unresolved type"
  | isClassType ty =
    case classLookup ty env of
      Just [cls] -> do
        fld <- find ((== f) . fname) $ cfields cls
        let bindings = formalBindings (cname cls) ty
            ftype' = replaceTypeVars bindings $ ftype fld
        return fld{ftype = ftype'}
      _ ->
        error "Environment.hs: Tried to do fieldLookup with unresolved type"
  | otherwise = error $ "Trying to lookup field in a non ref type " ++ show ty

matchHeader :: Name -> FunctionHeader -> Bool
matchHeader m = (==m) . hname

traitMethodLookup :: Type -> Name -> Environment -> Maybe FunctionHeader
traitMethodLookup ty m env =
  case traitLookup ty env of
    Just [trait] -> do
      let headers = requiredMethods trait ++ map mheader (tmethods trait)
          bindings = formalBindings (tname trait) ty
      header <- find (matchHeader m) headers
      return $ replaceHeaderTypes bindings header
    Just l ->
      error $ "Environment.hs: Ambiguous target of trait method lookup. \n" ++
              "Possible targets are: " ++ show l
    Nothing ->
      error $
        printf "Environment.hs: Tried to lookup method %s in unresolved trait %s"
               (show m) (show ty)

classMethodLookup :: Type -> Name -> Environment -> Maybe FunctionHeader
classMethodLookup ty m env =
  case classLookup ty env of
    Just [cls] -> do
      let headers = map mheader $ cmethods cls
          bindings = formalBindings (cname cls) ty
          tys = typesFromCapability $ ccapability cls
          classResults = find (matchHeader m) headers
          traitResults = msum (map (\ty -> traitMethodLookup ty m env) tys)
      header <- classResults <|> traitResults
      return $ replaceHeaderTypes bindings header
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
capabilityLookup ty env
    | isClassType ty = do
        let Just [cls] = classLookup ty env
            bindings = formalBindings (cname cls) ty
        return $ replaceTypeVars bindings $ ccapability cls
    | otherwise = error $ "Environment.hs: Tried to look up the capability " ++
                          "of non-class type " ++ show ty

fromSameSource :: Map Namespace LookupTable -> (Namespace, a) -> (Namespace, a)
                  -> Bool
fromSameSource tables (ns1, _) (ns2, _) =
  sourceFile (tables Map.! ns1) == sourceFile (tables Map.! ns2)

traitLookup :: Type -> Environment -> Maybe [TraitDecl]
traitLookup t Env{defaultNamespace, lookupTables}
    | isRefAtomType t =
        case getRefNamespace t of
          Just ns -> do
            let key = if null ns
                      then defaultNamespace
                      else ns
            table <- Map.lookup key lookupTables
            let results = maybeToList $
                          Map.lookup (getId t) $
                          filteredTraitTable table
            return $ map (setNamespace key) results
          Nothing -> do
            let tables = map (second filteredTraitTable) $
                         filter (not . isQualified . snd) $
                         Map.assocs lookupTables
                results = map (second (Map.lookup $ getId t)) tables
                hits = nubBy (fromSameSource lookupTables) $
                       map (second fromJust) $
                       filter (isJust . snd) results
            return $ map (uncurry setNamespace) hits
    | otherwise = error $
      "Tried to lookup the trait of '" ++ show t
      ++ "' which is not a reference type"
    where
      setNamespace ns t@Trait{tname} = t{tname = setRefNamespace ns tname}
      filteredTraitTable LookupTable{traitTable
                                    ,selectiveExports = Nothing
                                    } =
        traitTable
      filteredTraitTable LookupTable{traitTable
                                    ,selectiveExports = Just names
                                    } =
        Map.filterWithKey (\t _ -> Name t `elem` names) traitTable

classLookup :: Type -> Environment -> Maybe [ClassDecl]
classLookup cls Env{defaultNamespace, lookupTables, namespaceTable}
    | isRefAtomType cls =
        case getRefNamespace cls of
          Just ns -> do
            let key = if null ns
                      then defaultNamespace
                      else ns
            table <- Map.lookup key lookupTables
            let results = maybeToList $
                          Map.lookup (getId cls) $
                          filteredClassTable table
            return $ map (setNamespace key) results
          Nothing -> do
            let tables = map (second filteredClassTable) $
                         filter (not . isQualified . snd) $
                         Map.assocs lookupTables
                results = map (second (Map.lookup $ getId cls)) tables
                hits = nubBy (fromSameSource lookupTables) $
                       map (second fromJust) $
                       filter (isJust . snd) results
            return $ map (uncurry setNamespace) hits
    | otherwise = error $
      "Tried to lookup the class of '" ++ show cls
      ++ "' which is not a reference type"
    where
      setNamespace ns c@Class{cname, ccapability} =
          c{cname = setRefNamespace ns cname
           ,ccapability = translateTypeNamespace namespaceTable ccapability}
      filteredClassTable LookupTable{classTable
                                    ,selectiveExports = Nothing
                                    } =
        classTable
      filteredClassTable LookupTable{classTable
                                    ,selectiveExports = Just names
                                    } =
        Map.filterWithKey (\c _ -> Name c `elem` names) classTable

typeSynonymLookup :: Type -> Environment -> Maybe [Typedef]
typeSynonymLookup t Env{defaultNamespace, lookupTables, namespaceTable}
    | isRefAtomType t =
        case getRefNamespace t of
          Just ns -> do
            let key = if null ns
                      then defaultNamespace
                      else ns
            table <- Map.lookup key lookupTables
            let results = maybeToList $
                          Map.lookup (getId t) $
                          filteredTypeSynonymTable table
            return $ map (setNamespace key) results
          Nothing -> do
            let tables = map (second filteredTypeSynonymTable) $
                         filter (not . isQualified . snd) $
                         Map.assocs lookupTables
                results = map (second (Map.lookup $ getId t)) tables
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
      filteredTypeSynonymTable LookupTable{typeSynonymTable
                                          ,selectiveExports = Nothing
                                          } =
        typeSynonymTable
      filteredTypeSynonymTable LookupTable{typeSynonymTable
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
      let key = if null ns
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
      result <- lookup x locals
      return [(qname, result)]

    globalSearch =
      let tables = map (second filterFunctionTable) $
                   filter (not . isQualified . snd) $
                   Map.assocs lookupTables
          results = map (second (Map.lookup x)) tables
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

isLocal :: QualifiedName -> Environment -> Bool
isLocal QName{qnspace = Nothing, qnlocal = x} Env{locals} =
    isJust $ lookup x locals
isLocal QName{qnspace = Just [], qnlocal = x} Env{locals} =
    isJust $ lookup x locals
isLocal _ _ = False

typeVarLookup :: Type -> Environment -> Maybe Type
typeVarLookup ty env
    | isTypeVar ty =
        lookup ty (bindings env) <|>
        find (==ty) (typeParameters env)
    | otherwise    = error
      "Tried to lookup the binding of something that was not a type variable"

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) env =
    extendEnvironment newTypes $ env {locals = extend (locals env) name ty}
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):locals) name' ty'
          | name == name' = (name', ty') : locals
          | otherwise     = (name, ty) : extend locals name' ty'

-- | Convenience function for extending the environment with a
-- list of parameter declarations
addParams :: [ParamDecl] -> Environment -> Environment
addParams = extendEnvironment . map (\(Param {pname, ptype}) -> (pname, ptype))

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
