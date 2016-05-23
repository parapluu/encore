module ModuleExpander(importAndCheckModules) where

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

-- for debugging
import Debug.Trace

type Modules = Map QName Program  -- where does this belong


tosrc :: FilePath -> QName -> FilePath
tosrc dir target = dir ++ tosrc' target
  where
    tosrc' [(Name a)] = a ++ ".enc"
    tosrc' ((Name a) : as) = a ++ "/" ++ tosrc' as

{-
expandModules :: [FilePath] -> Program -> IO Program
expandModules importDirs p = expandProgram p
    where
      expandProgram p@Program{imports} =
          do exImps <- mapM expandImport imports
             return $ p{imports = exImps}

      expandImport i@(Import meta target) =
          do (imp, src) <- importOne importDirs i
             expImp <- expandProgram imp
             return $ PulledImport meta target src expImp
-}

importOne :: [FilePath] -> ImportDecl -> IO (Program, FilePath)
importOne importDirs (Import meta target) = do
  let sources = map (\dir -> tosrc dir target) importDirs
  trace ("Looking here:" ++ (show sources)) (return 10)
  candidates <- filterM doesFileExist sources
  trace ("Possibilities:" ++ (show candidates)) (return 10)
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


-- TODO: this will replace expandModules above
-- TODO: better name
-- performs bottom up traversal. Needs to avoid rechecking
importAndCheckModules :: (Environment -> Program -> IO (Environment, Program)) -> [FilePath] -> Program -> IO Modules
importAndCheckModules checker importDirs p = do
    (modules, env) <- trace ("Import Dirs: " ++ (show importDirs)) $ importAndCheckProgram Map.empty emptyEnv [] p
    return modules
  -- [] is QName of top-level module. TODO: find better approach
    where
        importAndCheckProgram :: Modules -> Environment -> QName -> Program -> IO (Modules, Environment)
        importAndCheckProgram importTable env target prg = do
            let p@Program{imports} = addStdLib target prg
            (newImportTable, newEnv) <- foldM performImport (importTable, env) imports
            (newerEnv, checkedAST) <- checker newEnv p
            return $ (Map.insert target checkedAST newImportTable, newerEnv)
            
        performImport :: (Modules, Environment) -> ImportDecl -> IO (Modules, Environment)
        performImport (importTable, env) i@(Import _ target) =
            if Map.member target importTable then
                return (importTable, env)
            else do
                (impl, _) <- importOne importDirs i
                importAndCheckProgram importTable env target impl

addStdLib target ast@Program{imports = i} = 
    if qname2string target == "String" then ast  -- avoids importing String from String
    else ast{imports = i ++ stdLib}
 -- TODO: move this elsewhere
stdLib = [Import (Meta.meta (P.initialPos "String.enc")) (Name "String" : [])]
   