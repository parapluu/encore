
module AST.ModuleExpander(expandModules) where

import Identifiers
import AST.AST
-- import AST.PrettyPrinter
-- import AST.Util
-- import Types

data MError = MError deriving Show

expandModules :: Program -> Either MError Program
expandModules p = return p


expandModulesAux :: [Name] -> Program -> Either MError Program
expandModulesAux = undefined

