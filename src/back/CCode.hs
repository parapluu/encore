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
   | Switch Id [(CCode, CCode)] CCode
   | StructDecl Id [CVarSpec]
   | Record [CCode]
   | Assign CCode CCode
   | Statement CCode -- for putting a semi-colon on the end.
   | C [CCode]
   | Enum [Id]
   | BracedBlock CCode
   | Call Id [CCode]
   | TypeDef Id CCode
   | Deref CCode
   | Dot CCode Id
   | Var Id
   | Embed String  -- for C code that doesn't match other patterns
   | Function { fun_ret :: CType,
                fun_name :: String,
                fun_args :: [CVarSpec],
                fun_body :: [CCode] }
   | FwdDecl CCode

