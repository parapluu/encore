{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NamedFieldPuns #-}

{-| Translate an Encore program (see "AST") into @CCode FIN@
You can pretty-print (see "CCode.PrettyCCode") this and a C compiler
might be able to compile the result. -}

module CodeGen.Program(translate) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl
import CodeGen.CCodeNames
import CodeGen.Header
import CodeGen.Shared
import qualified CodeGen.Context as Ctx

import CCode.Main
import qualified AST.AST as A
import Control.Monad.Reader hiding (void)
import qualified CodeGen.Context as Ctx

instance Translatable A.Program ([(String, CCode FIN)], CCode FIN, CCode FIN) where
    translate prog@(A.Program{A.classes = classes}) = (classList, header, shared)
        where
          classList = map (\cdecl@(A.Class{A.cname = cname}) -> 
                          (show cname, 
                           runReader (translate cdecl) (Ctx.mk prog))) classes
          header = generate_header prog
          shared = generate_shared prog