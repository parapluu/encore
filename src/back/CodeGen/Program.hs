{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Program(translate) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl

import CCode.Main
import qualified AST as A
import Control.Monad.Reader
import qualified CodeGen.Context as Ctx

instance FwdDeclaration A.Program CCode where
  fwd_decls (A.Program cs) = C $ [create_and_send_fn,
                                  msg_alloc_decl,
                                  msg_enum (A.Program cs),
                                  class_ids_enum (A.Program cs)]
    where
      msg_alloc_decl =
          Statement $ Embed $
                        "static pony_msg_t m_MSG_alloc = {0, {{NULL, 0, PONY_PRIMITIVE}}}"
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
      msg_enum :: A.Program -> CCode
      msg_enum (A.Program cs) =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (A.methods cdecl)) cs
          lines = map (\ (cname, mdecl) -> "MSG_" ++ show cname ++ "_" ++ (show $ A.mname mdecl)) meta
        in
         Statement $ Enum $ "MSG_alloc":lines

      class_ids_enum :: A.Program -> CCode
      class_ids_enum (A.Program cs) =
        let
          names = map (("ID_"++) . show . A.cname) cs
        in
         Statement $ Enum names

instance Translatable A.Program CCode where
  translate (A.Program cs) =
    C $
    (HashDefine "__STDC_FORMAT_MACROS") :
    (Includes ["pony/pony.h",
               "stdlib.h",
               "stdio.h",
               "string.h",
               "inttypes.h",
               "assert.h"]) :
    (fwd_decls (A.Program cs)) :
    (map fwd_decls cs) ++
    (map translate_class_here cs) ++
    [(Function
      (embedCType "static void") "dispatch"
      (map CVarSpec [(embedCType "pony_actor_t*", "this"),
                     (embedCType "void*", "p"),
                     (embedCType "uint64_t", "id"),
                     (embedCType "int", "argc"),
                     (embedCType "pony_arg_t*", "argv")])
      [Switch "id"
       [(Var "PONY_MAIN",
         C $ map Statement [Decl $ CVarSpec (embedCType "Main_data*", "d"),
                            Assign (Var "d") (Call "pony_alloc" [(Call "sizeof" [Var "Main_data"])]),
                            Call "pony_set" [Var "d"],
                            Call "Main_main" [Var "d"]])]
       (Embed "printf(\"error, got invalid id: %llu\",id);")]),
     (Function
      (embedCType "int") "main"
      [CVarSpec (embedCType "int","argc"), CVarSpec (embedCType "char**","argv")]
      [Embed "return pony_start(argc, argv, pony_create(&Main_actor));"])]
    where
      translate_class_here :: A.ClassDecl -> CCode
      translate_class_here cdecl = runReader (translate cdecl) $ Ctx.mk (A.Program cs)

