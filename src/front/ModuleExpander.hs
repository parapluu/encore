
module ModuleExpander(expandModules) where

import Identifiers
import Utils
import AST.AST
import Control.Monad
import System.Directory(doesFileExist)
import Parser.Parser
-- for debugging
import Debug.Trace

expandModules :: [FilePath] -> Program -> IO Program
expandModules importDirs p = expand p
  where
    expand p@(Program etl imps funs cls)  = 
 	  do 
	  	  impAsts <- mapM importOne imps
		  rimpAsts <- mapM expand impAsts 
		  return $ foldr merge p rimpAsts

    importOne :: ImportDecl -> IO Program
    importOne (Import meta (Name target)) = 
      do
          let sources = map (\dir -> dir ++ target ++ ".enc") importDirs
          maybeFirstSource <- firstsource sources
          sourceName <- case maybeFirstSource of
                          Just name -> return name
                          Nothing -> (abort $ "Module \"" ++ target ++ "\" cannot be found in imports! Aborting.")
          code <- readFile sourceName
          ast <- case parseEncoreProgram sourceName code of
               Right ast  -> return ast
               Left error -> abort $ show error
          return ast

merge (Program elt ims funs cls) (Program elt' ims' funs' cls') = Program (emjoin elt elt') (ims ++ ims') (funs ++ funs') (cls ++ cls')
  where emjoin (EmbedTL meta header body) (EmbedTL meta' header' body') = EmbedTL meta (header ++ header) (body ++ body') 
-- TODO how to join the two meta components?


firstsource [] = return Nothing
firstsource (src : srcs) = 
	do 
      sourceExists <- doesFileExist src 
      if sourceExists then return $ Just src
	  else firstsource srcs
	
	
