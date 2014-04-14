
module CCode (CCode (..),
              Funrec (..),
              Id,
              Type) where

{- to be moved into a separate file later -}

type Type = String
type Id = String

data Funrec = Funrec { fun_ret :: Type,
                       fun_name :: String,
                       fun_args :: [(Id,Type)],
                       fun_body :: [CCode] }

data CCode = 
     Includes [String]
   | HashDefine String
   | Switch String [CCode]
   | Record [CCode]
   | C [CCode]
   | TypeDef String CCode
   | SEMI          -- need to get rid of this
   | Embed String  -- for C code that doesn't match other patterns
   | Function Funrec

