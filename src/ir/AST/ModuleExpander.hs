module AST.ModuleExpander(expandModules) where

import Identifiers
import AST.AST

data MError = MError deriving Show

expandModules :: Program -> Either MError Program
expandModules = return


expandModulesAux :: [Name] -> Program -> Either MError Program
expandModulesAux = undefined
