module CCode (CCode (..),
              CVarSpec (..),
              Id (..),
              embedCType,
              CType,
              show) where

{- to be moved into a separate file later -}

import qualified AST
import Data.Char

newtype CType = CType String
type Id = String

instance Show CType where
  show (CType ct) = ct

embedCType :: String -> CType
embedCType = CType

newtype CVarSpec = CVarSpec (CType, Id)

data CCode = 
     Includes [String]
   | Decl CVarSpec
   | HashDefine String
   | Switch Id [(CCode, CCode)]
   | StructDecl Id [CVarSpec]
   | Record [CCode]
   | Static CCode
   | Assign CCode CCode
   | C [CCode]
   | Call Id [CCode]
   | TypeDef Id CCode
   | Var Id
   | SEMI CCode          -- need to get rid of this
   | Embed String  -- for C code that doesn't match other patterns
   | Function { fun_ret :: CType,
                fun_name :: String,
                fun_args :: [CVarSpec],
                fun_body :: [CCode] }

