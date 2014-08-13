{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NamedFieldPuns #-}

{-|
Translating an Encore program to its CCode representation (see "CCode.Main")
-}

module CodeGen.Main (compile_to_c) where

import CodeGen.Program
import CodeGen.Preprocessor
import CCode.Main

import qualified AST.AST as A

-- | Translates a full Encore Program to a CCode FIN (see "CCode.Main") value
compile_to_c :: A.Program -> ([(String, CCode FIN)], CCode FIN, CCode FIN)
compile_to_c = translate . preprocess