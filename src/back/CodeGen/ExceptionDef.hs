{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| Makes @ExceptionDef@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.ExceptionDef () where

import CodeGen.Typeclasses
import CCode.Main
import qualified AST.AST as A

instance Translatable A.ExceptionDef (CCode Toplevel) where
  translate exc@(A.ExceptionDef{A.excname, A.excsupername}) =
    let exportName       = Nam $ show excname
        exportSupername  = Nam $ show excsupername
    in  Exception exportName exportSupername
