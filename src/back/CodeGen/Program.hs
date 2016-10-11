{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| Translate an Encore program (see "AST") into a tuple
@(classes, header, shared)@, where @classes@ is a list of the
translated classes (one file per class) together with their names,
@header@ is the common header file of all the classes, and
@shared@ is the code for the file of shared C code. Each C file
(value of type @CCode FIN@) can be pretty-printed (see
"CCode.PrettyCCode"), and a C compiler might be able to compile
the result.-}

module CodeGen.Program (
  translate,
  Emitted,
  classes,
  header,
  shared
) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl ()
import CodeGen.Header
import CodeGen.Shared
import CodeGen.ClassTable

import CCode.Main
import qualified AST.AST as A
import qualified Types as Ty

data Emitted = Emitted {
  classes :: [(String, CCode FIN)],
  header ::  CCode FIN,
  shared ::  CCode FIN
} deriving (Show)

instance Translatable A.Program Emitted where
  translate prog =
    let
      table = buildProgramTable prog
      header = generateHeader prog
      shared = generateShared prog table
      classes = nameAndClass table prog
    in
      Emitted{classes, header, shared}
    where
      nameAndClass table A.Program{A.classes} =
        [(Ty.getId (A.cname c), translate c table) | c <- classes]
