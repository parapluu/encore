{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{-| Translate an Encore program (see "AST") into a tuple
@(classes, header, shared)@, where @classes@ is a list of the
translated classes (one file per class) together with their names,
@header@ is the common header file of all the classes, and
@shared@ is the code for the file of shared C code. Each C file
(value of type @CCode FIN@) can be pretty-printed (see
"CCode.PrettyCCode"), and a C compiler might be able to compile
the result.-}

module CodeGen.Program(translate) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl
import CodeGen.Header
import CodeGen.Shared

import CCode.Main
import qualified AST.AST as A
import qualified Types as Ty

instance Translatable A.Program ([(String, CCode FIN)], CCode FIN, CCode FIN) where
    translate prog@(A.Program{A.classes}) = (classList, header, shared)
      where
          classList = map name_and_class classes
          name_and_class cdecl@(A.Class{A.cname}) =
              (Ty.getId cname, translate cdecl)
          header = generate_header prog
          shared = generate_shared prog
