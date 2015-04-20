module ModuleExpander(expandModules) where

import Identifiers
import Utils
import AST.AST
import Control.Monad
import System.Directory(doesFileExist)
import Parser.Parser
-- for debugging
import Debug.Trace

tosrc :: FilePath -> QName -> FilePath
tosrc dir target = dir ++ tosrc' target
  where
    tosrc' [(Name a)] = a ++ ".enc"
    tosrc' ((Name a) : as) = a ++ "/" ++ tosrc' as

expandModules :: [FilePath] -> Program -> IO Program
expandModules importDirs p = expandProgram p
    where
      expandProgram p@(Program bundle etl imps funs cls) =
          do exImps <- mapM expandImport imps
             return $ Program bundle etl exImps funs cls
             
      expandImport i@(Import meta target) = 
          do (imp, src) <- importOne i
             expImp <- expandProgram imp         
             return $ PulledImport meta target src expImp

      importOne :: ImportDecl -> IO (Program, FilePath)
      importOne (Import meta target) =
          do let sources = map (\dir -> tosrc dir target) importDirs
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
qname2string [(Name a)] = a
qname2string ((Name a):as) = a ++ "." ++ qname2string as

informImport target src =
        putStrLn $ "Importing module " ++ (qname2string target) ++ " from " ++ src

duplicateModuleWarning :: QName -> [FilePath] -> IO ()
duplicateModuleWarning target srcs =
    do putStrLn $ "Warning: Module " ++ (qname2string target) ++ " found in multiple places:"
       mapM_ (\src -> putStrLn $ "-- " ++ src) srcs
