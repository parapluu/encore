
module ModuleExpander(expandModules) where

import Identifiers
import Utils
import AST.AST
import Control.Monad
import System.Directory             ( doesFileExist )
import Parser.Parser
-- import AST.PrettyPrinter
-- import AST.Util
-- import Types

data MError = MError deriving Show


{-
data Program = Program {etl :: EmbedTL, 
                        imports :: [ImportDecl], 
                        functions :: [Function], 
                        classes :: [ClassDecl]} deriving(Show)
-}

expandModules :: Program -> IO Program
expandModules p@(Program etl imps funs cls) = 
 	do 
		mapM importOne imps
		return p

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


expandModulesAux :: [Name] -> Program -> Either MError Program
expandModulesAux = undefined

