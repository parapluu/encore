{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances #-}

module CodeGen.Shared(generate_shared) where

import CCode.Main
import CodeGen.CCodeNames
import qualified AST.AST as A


generate_shared :: A.Program -> CCode FIN
generate_shared A.Program{A.etl = A.EmbedTL{A.etlbody = etlbody}} = 
    Program $
    Concat $
      (LocalInclude "header.h") :
      [comment_section "Embedded Code"] ++
      [Embed etlbody] ++

      [comment_section "Shared functions"] ++
      [create_and_send] ++

      [comment_section "Shared messages"] ++
      shared_messages ++
      [main_function]
    where
      create_and_send =
          Function (Ptr pony_actor_t) (Nam "create_and_send") 
                   [(Ptr pony_actor_type_t, Var "type"), (uint, Var "msg_id")]
                   (Seq [Assign (Decl (Ptr pony_actor_t, Var "ret")) 
                                (Call (Nam "pony_create") [Var "type"]),
                         Statement $ Call (Nam "pony_send") [Var "ret", Var "msg_id"],
                         Return $ Var "ret"])

      shared_messages = [msg_alloc_decl, msg_fut_resume_decl, msg_fut_run_closure_decl]
          where
            msg_alloc_decl =
                AssignTL (Decl (pony_msg_t, Var "m_MSG_alloc"))
                         (Record [Int 0, Record ([] :: [CCode Expr])])
            msg_fut_resume_decl =
                AssignTL (Decl (pony_msg_t, Var "m_resume_get"))
                         (Record [Int 1, Record [Var "PONY_NONE"]])
            msg_fut_run_closure_decl =
                AssignTL (Decl (pony_msg_t, Var "m_run_closure"))
                         (Record [Int 1, Record [Var "PONY_NONE"]])
          
      main_function =
          (Function (Typ "int") (Nam "main")
                    [(Typ "int", Var "argc"), (Ptr . Ptr $ char, Var "argv")]
                    (Seq [Statement (Call (Nam "init_futures") [Int 2, AsExpr $ Var "LAZY"]), 
                          Return $ 
                          Call (Nam "pony_start") [AsExpr $ Var "argc", 
                                                   AsExpr $ Var "argv", 
                                                   Call (Nam "pony_create") [Amp (Var "Main_actor")]]]))

comment_section :: String -> CCode Toplevel
comment_section s = Embed $ (take (5 + length s) $ repeat '/') ++ "\n// " ++ s