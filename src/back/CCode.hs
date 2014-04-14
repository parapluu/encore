
module CCode where

{- to be moved into a separate file later -}

data CCode = 
     Includes [String]
   | HashDefine String
   | Switch String [CCode]
   | Record [CCode]
   | C [CCode]
   | TypeDef String CCode
   | SEMI          -- need to get rid of this
   | Embed String  -- for C code that doesn't match other patterns

