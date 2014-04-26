{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.ClassDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.MethodDecl
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST as A

import Control.Monad.Reader

instance Translatable A.ClassDecl (Reader Ctx.Context CCode) where
  translate cdecl = do
    method_impls <- mapM (\mdecl -> (local (Ctx.with_class cdecl) (translate mdecl))) (A.methods cdecl)
    return $ C $ concat [
      [comment_section $ "Implementation of class " ++ (show $ A.cname cdecl)],
      [data_struct],
      [tracefun_decl],
      pony_msg_t_impls,
      [message_type_decl],
      [pony_actor_t_impl],
      method_impls,
      [dispatchfun_decl]
      ]
    where
      data_struct = Statement $
                    TypeDef (data_rec_name $ A.cname cdecl)
                    (StructDecl ((data_rec_name $ A.cname cdecl)) $ map CVarSpec $
                     zip (map (data_rec_pointer . A.ftype) (A.fields cdecl)) (map (show . A.fname) (A.fields cdecl)))
      mthd_dispatch_clause cdecl mdecl =
        (Var (method_msg_name (A.cname cdecl) (A.mname mdecl)),
         Statement (Call (method_impl_name (A.cname cdecl) (A.mname mdecl)) [Var "p"]
         -- fixme what about arguments?
          ))
        
      dispatchfun_decl =
          (Function (data_rec_pointer (A.Type "static void")) (class_dispatch_name $ A.cname cdecl)
           (map CVarSpec [(embedCType "pony_actor_t*", "this"),
                          (embedCType "void*", "p"),
                          (embedCType "uint64_t", "id"),
                          (embedCType "int", "argc"),
                          (embedCType "pony_arg_t*", "argv")])
           [--Decl (CVarSpec (embedCType "main_t*", "d")),
            --Assign () (Var "p"),
            Switch "id"
            ((Var "PONY_MAIN",
              C $ [alloc_instr,
                   (if (A.cname cdecl) == (A.Type "Main")
                    then Statement $ Call (method_impl_name (A.Type "Main") (A.Name "main")) [Var "p"]
                    else C [])]) :
             (Var "MSG_alloc",
                  alloc_instr) :
             map (mthd_dispatch_clause cdecl) (A.methods cdecl))
            (Embed "printf(\"error, got invalid id: %llu\",id);")])
          where
            alloc_instr = C $ map Statement $
                          [(Var "p") `Assign`
                           (Call "pony_alloc" [(Call "sizeof" [Var (data_rec_name $ A.cname cdecl)])]),
                           Call "pony_set" [Var "p"]]

      tracefun_decl = (Function
                       (embedCType "static void")
                       (class_trace_fn_name (A.cname cdecl))
                       [CVarSpec (embedCType "void*", "p")]
                       [])
      message_type_decl = Function (embedCType "static pony_msg_t*")
                          (class_message_type_name $ A.cname cdecl)
                          [CVarSpec (embedCType "uint64_t", "id")]
                          [(Switch "id"
                            ((Var "MSG_alloc", Statement $ Embed $ "return &m_MSG_alloc")
                             :(map (\mdecl -> message_type_clause (A.cname cdecl) (A.mname mdecl))
                                     (A.methods cdecl)))
                            (C [])),
                           Statement (Embed "return NULL")]
        where
          message_type_clause :: A.Type -> A.Name -> (CCode, CCode)
          message_type_clause cname mname =
            (Var (method_msg_name cname mname),
             Statement $ Embed $ "return &" ++ (method_message_type_name cname mname))

-- * implement the message types:
--      static pony_msg_t m_Other_init = {0, {{NULL, 0, PONY_PRIMITIVE}}};
--      static pony_msg_t m_Other_work = {2, {{NULL, 0, PONY_PRIMITIVE}}};


      pony_msg_t_impls = map pony_msg_t_impl (A.methods cdecl)

      pony_msg_t_impl mdecl = 
          Statement $
          Embed $ "static pony_msg_t " ++ method_message_type_name (A.cname cdecl) (A.mname mdecl) ++ "= {" ++ (show $ length (A.mparams mdecl)) ++ ", {{NULL, 0, PONY_PRIMITIVE}}}"
        
      pony_actor_t_impl = Statement (Assign (Embed $ "static pony_actor_type_t " ++ actor_rec_name (A.cname cdecl))
                                              (Record [Var ("ID_"++(show $ A.cname cdecl)),
                                                       tracefun_rec,
                                                       (Embed $ class_message_type_name (A.cname cdecl)),
                                                       (Embed $ class_dispatch_name $ A.cname cdecl)]))

      tracefun_rec = Record [Var (class_trace_fn_name $ A.cname cdecl),
                             Call "sizeof" [Var $ data_rec_name $ A.cname cdecl],
                             Var "PONY_ACTOR"]

comment_section :: String -> CCode
comment_section s = C $ [Embed $ take (5 + length s) $ repeat '/',
                         Embed $ "// " ++ s]

main_dispatch_clause = (Var "PONY_MAIN",
                        C $ map Statement [Decl $ CVarSpec (embedCType "Main_data*", "d"),
                                      Assign (Var "d") (Call "pony_alloc" [(Call "sizeof" [Var "Main_data"])]),
                                      Call "pony_set" [Var "d"],
                                      Call "Main_main" [Var "d"]])

instance FwdDeclaration A.ClassDecl CCode where
  fwd_decls cdecl =
      C $ (comment_section "Forward declarations") :
        map (Statement . Embed)
                ["static pony_actor_type_t " ++ (actor_rec_name $ A.cname cdecl),
                 "static void " ++ (show $ A.cname cdecl) ++
                 "_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*)"]

