module ModuleExpander(importAndCheckModules,compressModules,ModuleMap) where


import Identifiers
import Utils
import AST.AST
import Control.Monad
import System.Directory(doesFileExist)
import Parser.Parser
import Data.Map (Map)
import Data.List
import Literate
import qualified Data.Map as Map
import Typechecker.Environment
import qualified AST.Meta as Meta
import Text.Parsec.Pos as P hiding(sourceName)

importAndCheckModules :: (FilePath -> Environment -> Path -> Program -> IO (Environment, Program)) ->
                                FilePath -> [FilePath] -> Program -> IO ModuleMap
importAndCheckModules checker sourceName importDirs p = do
    (modules, env) <- importAndCheckProgram sourceName [] Map.empty emptyEnv (Path []) p -- [] is Path of top-level module.
    return modules
  where
    importAndCheckProgram :: FilePath -> [Path] -> ModuleMap -> Environment -> Path -> Program -> IO (ModuleMap, Environment)
    importAndCheckProgram sourceName seen importTable env target prg = do
        when (target `elem` seen) $ abort $ "Module \"" ++ show target ++
                      "\" imports itself recursively! Aborting."
        let p@Program{imports} = addStdLib target prg
        (newImportTable, newEnv) <- foldM (performImport (target:seen)) (importTable, env) imports
        (newerEnv, checkedAST) <- checker sourceName newEnv target p
        return $ (Map.insert target checkedAST newImportTable, newerEnv)

    performImport :: [Path] -> (ModuleMap, Environment) -> ImportDecl -> IO (ModuleMap, Environment)
    performImport seen (importTable, env) i@Import{itarget} =
      if Map.member itarget importTable then
        return (importTable, env)
      else do
        (sourceName, impl) <- importOne importDirs i
        importAndCheckProgram sourceName seen importTable env itarget impl

importOne :: [FilePath] -> ImportDecl -> IO (FilePath, Program)
importOne importDirs Import{itarget} = do
  let sources = map (\dir -> tosrc dir itarget) importDirs
  candidates <- filterM doesFileExist sources
  sourceName <-
       case candidates of
         [] -> abort $ "Module \"" ++ show itarget ++
                      "\" cannot be found in imports! Aborting."
         [src] -> do { informImport itarget src; return src }
         l@(src:_) -> do 
             putStrLn $ "Error: Module " ++ show itarget ++ " found in multiple places:"
             mapM_ (\src -> putStrLn $ "-- " ++ src) l
             abort "Unable to determine which one to use."  
  raw <- readFile sourceName
  let code = if "#+literate\n" `isPrefixOf` raw
             then getTangle raw
             else raw
  ast <- case parseEncoreProgram sourceName code of
           Right ast  -> return ast
           Left error -> abort $ show error
  return (sourceName, ast)

addStdLib target ast@Program{imports = i} =
  if target == string2path "String" then ast  -- avoids importing String from String
  else ast{imports = i ++ stdLib}
    where
      stdLib = [strings]
      strings = Import{ imeta= Meta.meta (P.initialPos "String.enc"),
                        itarget = string2path "String", 
                        iimports = Nothing,
                        ihiding = Nothing,
                        irename = Nothing,
                        iqualified = False }

tosrc :: FilePath -> Path -> FilePath
tosrc dir (Path target) = dir ++ tosrc' target
  where
    tosrc' [(Name a)] = a ++ ".enc"
    tosrc' ((Name a) : as) = a ++ "/" ++ tosrc' as


compressModules :: ModuleMap -> Program
compressModules = foldl1 joinTwo
  where
    joinTwo :: Program -> Program -> Program
    joinTwo p@Program{etl=etl,  functions=functions,  traits=traits,  classes=classes}
              Program{etl=etl', functions=functions', traits=traits', classes=classes'} =
                p{etl=etl ++ etl', functions=functions ++ functions',
                  traits=traits ++ traits', classes=classes ++ classes'}


-- debugging code

printImports = False

informImport target src = 
    if printImports then 
        putStrLn $ "Importing module " ++ show target ++ " from " ++ src
    else 
        return ()  
