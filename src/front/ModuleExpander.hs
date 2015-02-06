
module ModuleExpander(expandModules) where

import Identifiers
import Utils
import AST.AST
import Control.Monad
import System.Directory(doesFileExist)
import Parser.Parser
-- import AST.PrettyPrinter
-- import AST.Util
-- import Types

data MError = MError deriving Show


expandModules :: Program -> IO Program
expandModules p@(Program etl imps funs cls) = 
 	do 
		impAsts <- mapM importOne imps
		rimpAsts <- mapM expandModules impAsts
		return $ foldr merge p rimpAsts

importOne :: ImportDecl -> IO Program
importOne (Import meta (Name target)) = 
	do
      let sourceName = target ++ ".enc"
      sourceExists <- doesFileExist sourceName
      when (not sourceExists)
          (abort $ "File \"" ++ sourceName ++ "\" does not exist! Aborting.") 
      code <- readFile sourceName
      ast <- case parseEncoreProgram sourceName code of
               Right ast  -> return ast
               Left error -> abort $ show error
      return ast

merge (Program elt ims funs cls) (Program elt' ims' funs' cls') = Program (emjoin elt elt') (ims ++ ims') (funs ++ funs') (cls ++ cls')
  where emjoin (EmbedTL meta header body) (EmbedTL meta' header' body') = EmbedTL meta (header ++ header) (body ++ body') 
-- TODO how should I join the two meta components?


