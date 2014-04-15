
module CCode (CCode (..),
              CVarDecl (..),
              Id (..),
              toCType,
              embedCType,
              CType) where

{- to be moved into a separate file later -}

import qualified AST
import Data.Char

newtype CType = CType String
type Id = String

instance Show CType where
  show (CType ct) = ct

toCType :: AST.Type -> CType
toCType aty = if isUpper $ head aty
              then CType $ "(" ++ aty++ "_data*" ++ ")"
              else CType $ aty

embedCType :: String -> CType
embedCType = CType

newtype CVarDecl = CVarDecl (CType, Id)

data CCode = 
     Includes [String]
   | HashDefine String
   | Switch String [CCode]
   | Record Id [CVarDecl]
   | C [CCode]
   | TypeDef String CCode
   | SEMI          -- need to get rid of this
   | Embed String  -- for C code that doesn't match other patterns
   | Function { fun_ret :: CType,
                fun_name :: String,
                fun_args :: [CVarDecl],
                fun_body :: [CCode] }

