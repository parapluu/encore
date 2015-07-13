{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
Translating an Encore program to its CCode representation (see "CCode.Main")
-}

module CodeGen.Main (
  compile_to_c,
  Emitted,
  getClasses,
  getHeader,
  getShared
) where

import CodeGen.Program
import CodeGen.Preprocessor

import qualified AST.AST as A

getClasses = classes
getHeader = header
getShared = shared

compile_to_c :: A.Program -> Emitted
compile_to_c = translate . preprocess
