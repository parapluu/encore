{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
Translating an Encore program to its CCode representation (see "CCode.Main")
-}

module CodeGen.Main (code_from_AST) where

import CodeGen.Program
import CCode.Main

import qualified AST as A

-- | Translates a full Encore Program to a CCode FIN (see "CCode.Main") value
code_from_AST :: A.Program -> CCode FIN
code_from_AST = translate
