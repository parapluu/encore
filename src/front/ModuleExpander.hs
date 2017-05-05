module ModuleExpander(
                      ProgramTable
                     ,buildProgramTable
                     ,compressProgramTable
                     ,dirAndName
                     ) where

import Identifiers
import Utils
import AST.AST
import Parser.Parser
import Literate
import Typechecker.Environment
import AST.Meta
import Types(setRefSourceFile, setRefNamespace)

import SystemUtils
import Control.Monad
import Control.Arrow((&&&))
import System.Directory(doesFileExist, makeAbsolute)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.List
import Text.Megaparsec(parseErrorPretty, initialPos)
import Debug.Trace

type ProgramTable = Map FilePath Program

dirAndName = dirname' &&& basename
  where
    dirname' p =
        let dir = dirname p
        in if last dir == '/'
           then init dir
           else dir

buildProgramTable :: [FilePath] -> [FilePath] -> Program -> IO ProgramTable
buildProgramTable importDirs preludePaths p = do
  let (sourceDir, sourceName) = dirAndName (source p)
  findAndImportModules importDirs preludePaths sourceDir sourceName Map.empty p

shortenPrelude :: [FilePath] -> FilePath -> FilePath
shortenPrelude preludePaths source =
    if any (`isPrefixOf` source) preludePaths
    then basename source
    else source

stdLib source = [lib "String", lib "Std"]
    where
      lib s = Import{imeta = meta $ newPos (initialPos source)
                    ,itarget = explicitNamespace [Name s]
                    ,isource = Nothing
                    ,iqualified = False
                    ,iselect = Nothing
                    ,ialias  = Nothing
                    ,ihiding = Nothing
                    }

addStdLib :: FilePath -> ModuleDecl -> [ImportDecl] -> [ImportDecl]
addStdLib source NoModule imports = stdLib source ++ imports
addStdLib source Module{modname} imports =
  filter ((/= explicitNamespace [modname]) . itarget) (stdLib source) ++ imports


findAndImportModules :: [FilePath] -> [FilePath] -> FilePath -> FilePath ->
                        ProgramTable -> Program -> IO ProgramTable
findAndImportModules importDirs preludePaths sourceDir sourceName
                     table p@Program{moduledecl
                                    ,imports
                                    ,classes
                                    ,adts
                                    ,traits
                                    ,typedefs
                                    ,functions} = do
  let sourcePath = sourceDir </> sourceName
      shortSource = shortenPrelude preludePaths sourcePath
      withStdlib = addStdLib sourcePath moduledecl imports
  sources <- mapM (findSource importDirs sourceDir) withStdlib
  let imports'   = zipWith setImportSource sources withStdlib
      classes'   = map (setClassSource shortSource) classes
      adts'      = map (setADTSource shortSource) adts
      traits'    = map (setTraitSource shortSource) traits
      typedefs'  = map (setTypedefSource shortSource) typedefs
      functions' = map (setFunctionSource shortSource) functions
      p' = p{source    = shortSource
            ,imports   = imports'
            ,classes   = classes'
            ,adts      = adts'
            ,traits    = traits'
            ,typedefs  = typedefs'
            ,functions = functions'
            }
      newTable = Map.insert shortSource p' table
  foldM (importModule importDirs preludePaths) newTable sources
  where
    moduleNamespace = if moduledecl == NoModule
                      then emptyNamespace
                      else explicitNamespace [modname moduledecl]
    setImportSource source i =
        let shortPath = shortenPrelude preludePaths source
        in i{isource = Just shortPath}
    setADTSource source adt@ADT{aname, aconstructor} =
      adt{aname = setRefNamespace moduleNamespace $
                  setRefSourceFile source aname
         ,aconstructor = map (setADTConsSource source) aconstructor}
    setADTConsSource source cons@ADTcons{acname} =
      cons{acname = setRefNamespace moduleNamespace $
                  setRefSourceFile source acname}

    setClassSource source c@Class{cname} =
      c{cname = setRefNamespace moduleNamespace $
                setRefSourceFile source cname}
    setTraitSource source t@Trait{tname} =
      t{tname = setRefNamespace moduleNamespace $
                setRefSourceFile source tname}
    setTypedefSource source d@Typedef{typedefdef} =
      d{typedefdef = setRefNamespace moduleNamespace $
                     setRefSourceFile source typedefdef}
    setFunctionSource source f =
      f{funsource = source}

buildModulePath :: Namespace -> FilePath
buildModulePath (NSExplicit ns) =
  let prefix = init ns
      suffix = last ns
      moduleDir = foldl (</>) "" $ map show prefix
      moduleName = show suffix <> ".enc"
  in if null moduleDir
     then moduleName
     else moduleDir </> moduleName

findSource :: [FilePath] -> FilePath -> ImportDecl -> IO FilePath
findSource importDirs sourceDir Import{itarget} = do
  let modulePath = buildModulePath itarget
      imports = map (</> modulePath) importDirs
      sourceModulePath = sourceDir </> modulePath
  expandedSourceModulePath <- makeAbsolute $ sourceModulePath
  let sources = if expandedSourceModulePath `elem` imports then
                -- if directory of target is in imports, remove it to avoid ambiguous import error
                  nub $ imports
                else
                  nub $ sourceModulePath : imports
  candidates <- filterM doesFileExist sources
  case candidates of
    [] -> abort $ "Module " ++ show itarget ++
                  " cannot be found in imports. Directories searched:\n" ++
                  unlines (map (("  " ++) . dirname) sources) ++
                  "\nUse '-I PATH' to add additional import paths"
    [src] -> return src
    l -> do
      putStrLn $ "Module " ++ show itarget ++
                 " found in multiple places:"
      mapM_ (putStrLn . ("  " ++)) l
      abort "Unable to determine which one to use."

importModule :: [FilePath] -> [FilePath] -> ProgramTable -> FilePath
                -> IO ProgramTable
importModule importDirs preludePaths table source
  | shortSource <- shortenPrelude preludePaths source
  , Map.member shortSource table = return table
  | otherwise = do
      raw <- readFile source
      let code = if "#+literate\n" `isPrefixOf` raw
                 then getTangle raw
                 else raw
      ast <- case parseEncoreProgram source code of
               Right ast  -> return ast
               Left error -> abort $ parseErrorPretty error
      if moduledecl ast == NoModule
      then abort $ "No module in file " ++ source ++ ". Aborting!"
      else let (sourceDir, sourceName) = dirAndName source
           in findAndImportModules importDirs preludePaths
                                   sourceDir sourceName table ast

compressProgramTable :: ProgramTable -> Program
compressProgramTable = foldl1 joinTwo
  where
    joinTwo :: Program -> Program -> Program
    joinTwo p@Program{etl=etl,  functions=functions,  traits=traits,  classes=classes}
              Program{etl=etl', functions=functions', traits=traits', classes=classes'} =
                p{etl=etl ++ etl', functions=functions ++ functions',
                  traits=traits ++ traits', classes=classes ++ classes'}
