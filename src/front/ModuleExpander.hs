module ModuleExpander(importAndCheckModules,compressModules,ModuleMap) where

import Identifiers
import Utils
import AST.AST
import Control.Monad
import System.Directory(doesFileExist)
import Parser.Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Typechecker.Environment
import qualified AST.Meta as Meta
import Text.Parsec.Pos as P

type ModuleMap = Map QName Program

importAndCheckModules :: (Environment -> Program -> IO (Environment, Program)) ->
                                [FilePath] -> Program -> IO ModuleMap
importAndCheckModules checker importDirs p = do
    (modules, env) <- importAndCheckProgram Map.empty emptyEnv [] p -- [] is QName of top-level module.
    return modules
  where
    importAndCheckProgram :: ModuleMap -> Environment -> QName -> Program -> IO (ModuleMap, Environment)
    importAndCheckProgram importTable env target prg = do
      let p@Program{imports} = addStdLib target prg
      (newImportTable, newEnv) <- foldM performImport (importTable, env) imports
      (newerEnv, checkedAST) <- checker newEnv p
      return $ (Map.insert target checkedAST newImportTable, newerEnv)

    performImport :: (ModuleMap, Environment) -> ImportDecl -> IO (ModuleMap, Environment)
    performImport (importTable, env) i@(Import _ target) =
      if Map.member target importTable then
        return (importTable, env)
      else do
        (impl, _) <- importOne importDirs i
        importAndCheckProgram importTable env target impl

importOne :: [FilePath] -> ImportDecl -> IO (Program, FilePath)
importOne importDirs (Import meta target) = do
  let sources = map (\dir -> tosrc dir target) importDirs
  candidates <- filterM doesFileExist sources
  sourceName <-
       case candidates of
         [] -> abort $ "Module \"" ++ qname2string target ++
                      "\" cannot be found in imports! Aborting."
         [src] -> do { informImport target src; return src }
         l@(src:_) -> do { duplicateModuleWarning target l; return src }
  code <- readFile sourceName
  ast <- case parseEncoreProgram sourceName code of
           Right ast  -> return (ast, sourceName)
           Left error -> abort $ show error
  return ast

qname2string :: QName -> String
qname2string [] = ""
qname2string [(Name a)] = a
qname2string ((Name a):as) = a ++ "." ++ qname2string as

-- for printing imports
informImport target src =
        putStrLn $ "Importing module " ++ (qname2string target) ++ " from " ++ src

duplicateModuleWarning :: QName -> [FilePath] -> IO ()
duplicateModuleWarning target srcs =
    do putStrLn $ "Warning: Module " ++ (qname2string target) ++ " found in multiple places:"
       mapM_ (\src -> putStrLn $ "-- " ++ src) srcs


addStdLib target ast@Program{imports = i} =
  if qname2string target == "String" then ast  -- avoids importing String from String
  else ast{imports = i ++ stdLib}
    where
      stdLib = [Import (Meta.meta (P.initialPos "String.enc")) (Name "String" : [])]

tosrc :: FilePath -> QName -> FilePath
tosrc dir target = dir ++ tosrc' target
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

