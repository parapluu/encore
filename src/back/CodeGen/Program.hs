{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

module CodeGen.Program(translate) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl

import CCode.Main
import qualified AST as A
import Control.Monad.Reader
import qualified CodeGen.Context as Ctx

instance FwdDeclaration A.Program (CCode Toplevel) where
  fwd_decls (A.Program cs) = ConcatTL $ [create_and_send_fn,
                                         msg_alloc_decl,
                                         msg_enum (A.Program cs),
                                         class_ids_enum (A.Program cs)]
    where
      msg_alloc_decl =
          Embed $ "static pony_msg_t m_MSG_alloc = {0, {{NULL, 0, PONY_PRIMITIVE}}};"
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
         Enum $ map Var $ "MSG_alloc":lines

      class_ids_enum :: A.Program -> CCode Toplevel
      class_ids_enum (A.Program cs) =
        let
          names = map (("ID_"++) . show . A.cname) cs
        in
         Enum $ map Var $ names

instance Translatable A.Program (CCode FIN) where
  translate (A.Program cs) =
    Program $ 
    ConcatTL $
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
      (Typ "static void") (Var "dispatch")
      [(Typ "pony_actor_t*", Var "this"),
       (Typ "void*", Var "p"),
       (Typ "uint64_t", Var "id"),
       (Typ "int", Var "argc"),
       (Typ "pony_arg_t*", Var "argv")]
      (Switch (Var "id")
       [(Var "PONY_MAIN",
         Concat $ map Statement
                    [Assign (Decl $ (Typ "Main_data*", Var "d"))
                                (Call 
                                 (Var $ "pony_alloc")
                                 [(Call (Var $ "sizeof") [AsExpr . Embed $ "Main_data"])]),
                     Call (Var $ "pony_set") [Var $ "d"],
                     Call (Var $ "Main_main") [Var $ "d"]])]
       (Embed "printf(\"error, got invalid id: %llu\",id);"))),
     (Function
      (Typ "int") (Var "main")
      [(Typ "int", Var "argc"), (Typ "char**", Var "argv")]
      (Embed "return pony_start(argc, argv, pony_create(&Main_actor));"))]
    where
      translate_class_here :: A.ClassDecl -> CCode Toplevel
      translate_class_here cdecl = runReader (translate cdecl) $ Ctx.mk (A.Program cs)

