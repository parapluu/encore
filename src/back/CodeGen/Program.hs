{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

{-| Translate an Encore program (see "AST") into @CCode FIN@
You can pretty-print (see "CCode.PrettyCCode") this and a C compiler
might be able to compile the result. -}

module CodeGen.Program(translate) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx

import CCode.Main
import qualified AST.AST as A
import qualified AST.Util as Util
import Control.Monad.Reader hiding (void)
import qualified CodeGen.Context as Ctx

instance Translatable A.Program (CCode FIN) where
  translate prog@(A.Program cs) =
    Program $
    ConcatTL $
    (Includes ["pony/pony.h",
               "stdlib.h",
               "unistd.h", -- for sleep(..)
               "set.h",
               "closure.h",
               "future.h",
               --"string.h",
               --"inttypes.h",
               "assert.h",
               "stdio.h"
              ]) :
    (HashDefine "UNIT NULL - 1") :
    (fwd_decls prog) :
    (map (\cls -> runReader (fwd_decls cls) (Ctx.mk prog)) cs) ++
    (map translate_class_here cs) ++
    [(Function
      (Typ "int") (Nam "main")
      [(Typ "int", Var "argc"), (Ptr . Ptr $ char, Var "argv")]
      (Embed "return pony_start(argc, argv, pony_create(&Main_actor));"))]
    where
      translate_class_here :: A.ClassDecl -> CCode Toplevel
      translate_class_here cdecl = runReader (translate cdecl) $ Ctx.mk (A.Program cs)

instance FwdDeclaration A.Program (CCode Toplevel) where
  fwd_decls (A.Program cs) = ConcatTL $ [create_and_send_fn,
                                         msg_alloc_decl,
                                         msg_enum (A.Program cs),
                                         class_ids_enum (A.Program cs)]
    where
      msg_alloc_decl =
          Embed $ "static pony_msg_t m_MSG_alloc = {0, {PONY_NONE}};"
      create_and_send_fn =
          Embed $
                    "pony_actor_t* create_and_send(pony_actor_type_t* type, uint64_t msg_id) {\n" ++
--                    "  printf(\"creating:\\n\");\n" ++
                    "  pony_actor_t* ret = pony_create(type);\n" ++
                    "  pony_send(ret, msg_id);\n" ++
--                    "  printf(\"created and sent!\\n\");\n" ++
                    "  \n" ++
                    "  return ret;\n" ++
                    "}"
      msg_enum :: A.Program -> CCode Toplevel
      msg_enum (A.Program cs) =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (A.methods cdecl)) cs
          lines = map (\ (cname, mdecl) -> "MSG_" ++ show cname ++ "_" ++ (show $ A.mname mdecl)) meta
        in
         Enum $ map Nam $ "MSG_alloc":lines

      class_ids_enum :: A.Program -> CCode Toplevel
      class_ids_enum (A.Program cs) =
        let
          names = map (("ID_"++) . show . A.cname) cs
        in
         Enum $ map Nam $ names
