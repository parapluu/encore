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

instance Translatable A.EmbedTL (CCode Toplevel) where
    translate (A.EmbedTL _ code) = Embed code

instance Translatable A.Program (CCode FIN) where
  translate prog@(A.Program etl imps cs) =
    Program $
    Concat $
    (Includes [
      "pony/pony.h",
      "stdlib.h",
      "set.h",
      "closure.h",
      "future.h",
      "string.h",
      "stdio.h"
     -- "unistd.h", -- for sleep(..)
     --"inttypes.h"
     -- "assert.h"
     ]) :
    (translate etl) :
    (HashDefine "UNIT NULL - 1") :
    (fwd_decls prog) :
    (map (\cls -> runReader (fwd_decls cls) (Ctx.mk prog)) cs) ++
    (map translate_class_here cs) ++
    [(Function
      (Typ "int") (Nam "main")
      [(Typ "int", Var "argc"), (Ptr . Ptr $ char, Var "argv")]
      (Seq [Statement (Call (Nam "init_futures") [Int 2, AsExpr $ Var "LAZY"]), -- TODO: Pass these as params to encorec
              Return $ Call (Nam "pony_start") [AsExpr $ Var "argc", AsExpr $ Var "argv", Call (Nam "pony_create") [Amp (Var "Main_actor")]]]))]
    where
      translate_class_here :: A.ClassDecl -> CCode Toplevel
      translate_class_here cdecl = runReader (translate cdecl) (Ctx.mk (A.Program etl imps cs))

instance FwdDeclaration A.Program (CCode Toplevel) where
  fwd_decls (A.Program etl imps cs) = Concat $ [create_and_send_fn,
                                             msg_alloc_decl,
                                             msg_fut_resume_decl,
                                             msg_fut_run_closure_decl,
                                             msg_enum cs,
                                             class_ids_enum cs,
                                             class_data_recs cs]
    where
      msg_alloc_decl =
          AssignTL (Decl (Static pony_msg_t, Var "m_MSG_alloc"))
                   (Record [Int 0, Record ([] :: [CCode Expr])])
      msg_fut_resume_decl =
          AssignTL (Decl (Static pony_msg_t, Var "m_resume_get"))
                   (Record [Int 1, Record [Var "PONY_NONE"]])
      msg_fut_run_closure_decl =
          AssignTL (Decl (Static pony_msg_t, Var "m_run_closure"))
                   (Record [Int 1, Record [Var "PONY_NONE"]])
      create_and_send_fn =
          Function (Ptr pony_actor_t) (Nam "create_and_send") 
                   [(Ptr pony_actor_type_t, Var "type"), (uint, Var "msg_id")]
                   (Seq [Assign (Decl (Ptr pony_actor_t, Var "ret")) (Call (Nam "pony_create") [Var "type"]),
                        Statement $ Call (Nam "pony_send") [Var "ret", Var "msg_id"],
                        Return $ Var "ret"])
      msg_enum :: [A.ClassDecl] -> CCode Toplevel
      msg_enum cs =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (map A.mname (A.methods cdecl))) cs
          method_msg_names = map (show . (uncurry method_msg_name)) meta
          one_way_msg_names = map (show . (uncurry one_way_send_msg_name)) meta
        in
         Enum $ map Nam $ "MSG_alloc":(method_msg_names ++ one_way_msg_names)

      class_ids_enum :: [A.ClassDecl] -> CCode Toplevel
      class_ids_enum cs =
        let
          names = map (("ID_"++) . show . A.cname) cs
        in
         Enum $ map Nam $ names

      class_data_recs :: [A.ClassDecl] -> CCode Toplevel
      class_data_recs = Concat . (map class_data_rec)
          where
            class_data_rec :: A.ClassDecl -> CCode Toplevel
            class_data_rec A.Class {A.cname = cname} =
                Typedef (Struct . Nam $ (show cname) ++ "_data") (Nam $ (show cname) ++ "_data")