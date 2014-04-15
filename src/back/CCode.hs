
module CCode (CCode (..),
              VarDecl (..),
              Id,
              Type) where

{- to be moved into a separate file later -}

type Type = String
type Id = String

newtype VarDecl = VarDecl (Type, Id)

data CCode = 
     Includes [String]
   | HashDefine String
   | Switch String [CCode]
   | Record Id [VarDecl]
   | C [CCode]
   | TypeDef String CCode
   | SEMI          -- need to get rid of this
   | Embed String  -- for C code that doesn't match other patterns
   | Function { fun_ret :: Type,
                fun_name :: String,
                fun_args :: [VarDecl],
                fun_body :: [CCode] }

