
module AST.ModuleExpander(expandModules) where

{-
import Identifiers
import AST.AST
import AST.PrettyPrinter
import AST.Util
import Types
-}

expandModules :: Program -> Either MError Program
expandModules p = return p


expandModulesAux :: [Names] -> Program -> Either MError Program
expandModulesAux = undefined

