module ModuleExpander(importAndCheckModules,compressModules) where

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
import Text.Parsec.Pos as P

type ModuleMap = Map QName Program

importAndCheckModules :: (Environment -> Program -> IO (Environment, Program)) ->
                                [FilePath] -> Program -> IO ModuleMap
importAndCheckModules checker importDirs p = do
    (modules, _) <- importAndCheckProgram [] Map.empty emptyEnv [] p -- [] is QName of top-level module.
    return modules
  where
    importAndCheckProgram :: [QName] -> ModuleMap -> Environment -> QName -> Program -> IO (ModuleMap, Environment)
    importAndCheckProgram seen importTable env target prg = do
        when (target `elem` seen) $ abort $ "Module \"" ++ qname2string target ++
                      "\" imports itself recursively! Aborting."
        let pp@Program{imports} = addStdLib target prg
        (newImportTable, newEnv) <- foldM (performImport (target:seen)) (importTable, env) imports
        (newerEnv, checkedAST) <- checker newEnv pp
        return $ (Map.insert target checkedAST newImportTable, newerEnv)

    performImport :: [QName] -> (ModuleMap, Environment) -> ImportDecl -> IO (ModuleMap, Environment)
    performImport seen (importTable, env) i@(Import _ target) =
      if Map.member target importTable then
        return (importTable, env)
      else do
        impl <- importOne importDirs i
        importAndCheckProgram seen importTable env target impl

importOne :: [FilePath] -> ImportDecl -> IO Program
importOne importDirs (Import _ target) = do
  let sources = map (\dir -> tosrc dir target) importDirs
  candidates <- filterM doesFileExist sources
  source <-
       case candidates of
         [] -> abort $ "Module \"" ++ qname2string target ++
                      "\" cannot be found in imports! Aborting."
         [src] -> do { informImport target src; return src }
         l@(src:_) -> do
             putStrLn $ "Error: Module " ++ (qname2string target) ++ " found in multiple places:"
             mapM_ (\src -> putStrLn $ "-- " ++ src) l
             abort "Unable to determine which one to use."
  raw <- readFile source
  let code = if "#+literate\n" `isPrefixOf` raw
             then getTangle raw
             else raw
  ast <- case parseEncoreProgram source code of
           Right ast  -> return ast
           Left error -> abort $ show error
  return ast

qname2string :: QName -> String
qname2string [] = ""
qname2string [(Name a)] = a
qname2string ((Name a):as) = a ++ "." ++ qname2string as

printImports = False

informImport target src =
    if printImports then
        putStrLn $ "Importing module " ++ (qname2string target) ++ " from " ++ src
    else
        return ()

addStdLib target ast@Program{imports = i} =
  if qname2string target == "String" then ast  -- avoids importing String from String
  else ast{imports = i ++ stdLib }
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
