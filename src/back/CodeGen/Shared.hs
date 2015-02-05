{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances #-}

module CodeGen.Shared(generate_shared) where

import CCode.Main
import CodeGen.CCodeNames
import CodeGen.Typeclasses
import CodeGen.Function
import qualified AST.AST as A

-- | Generates a file containing the shared (but not included) C
-- code of the translated program
generate_shared :: A.Program -> CCode FIN
generate_shared A.Program{A.etl = A.EmbedTL{A.etlbody}, A.functions} = 
    Program $
    Concat $
      (LocalInclude "header.h") :
      [comment_section "Embedded Code"] ++
      [Embed etlbody] ++

      [comment_section "Global functions"] ++
      global_functions ++

      -- [comment_section "Shared messages"] ++
      -- shared_messages ++
      [main_function]
    where
      shared_messages = [msg_alloc_decl, msg_fut_resume_decl, msg_fut_suspend_decl, msg_fut_await_decl, msg_fut_run_closure_decl]
          where
            msg_alloc_decl =
                AssignTL (Decl (pony_msg_t, Var "m_MSG_alloc"))
                         (Record [Int 0, Record ([] :: [CCode Expr])])
            msg_fut_resume_decl =
                AssignTL (Decl (pony_msg_t, Var "m_resume_get"))
                         (Record [Int 1, Record [Var "PONY_NONE"]])
            msg_fut_suspend_decl =
                AssignTL (Decl (pony_msg_t, Var "m_resume_suspend"))
                         (Record [Int 1, Record [Var "PONY_NONE"]])
            msg_fut_await_decl =
                AssignTL (Decl (pony_msg_t, Var "m_resume_await"))
                         (Record [Int 2, Record [Var "PONY_NONE", Var "PONY_NONE"]])
            msg_fut_run_closure_decl =
                AssignTL (Decl (pony_msg_t, Var "m_run_closure"))
                         (Record [Int 3, Record [Var "PONY_NONE", Var "PONY_NONE", Var "PONY_NONE"]])
      
      global_functions = map translate functions

      main_function =
          Function (Typ "int") (Nam "main")
                   [(Typ "int", Var "argc"), (Ptr . Ptr $ char, Var "argv")]
                   (Seq $ init_globals ++ [Return encore_start])
          where
            init_globals = map init_global functions
                where 
                  init_global A.Function{A.funname} = 
                      Assign (global_closure_name funname)
                             (Call (Nam "closure_mk") 
                                   [AsExpr $ AsLval $ global_function_name funname, 
                                    Null, 
                                    Null])
            encore_start =
                Call (Nam "encore_start") 
                     [AsExpr $ Var "argc", 
                      AsExpr $ Var "argv", 
                      Amp (Var "_enc__active_Main_type")]


comment_section :: String -> CCode Toplevel
comment_section s = Embed $ (take (5 + length s) $ repeat '/') ++ "\n// " ++ s
