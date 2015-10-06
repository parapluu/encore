{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
Translating an Encore program to its CCode representation (see "CCode.Main")
-}

module CodeGen.Main (
  compileToC,
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

compileToC :: A.Program -> Emitted
compileToC = translate . preprocess
