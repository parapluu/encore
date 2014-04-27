{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.ClassDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.MethodDecl
import qualified CodeGen.Context as Ctx

import CCode.Main
import CCode.PrettyCCode

import qualified AST as A

import Control.Monad.Reader

instance Translatable A.ClassDecl (Reader Ctx.Context (CCode Toplevel)) where
  translate cdecl = do
    method_impls <- mapM (\mdecl -> (local (Ctx.with_class cdecl) (translate mdecl))) (A.methods cdecl)
    return $ ConcatTL $ concat [
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
      data_struct :: CCode Toplevel
      data_struct = TypeDef (data_rec_name $ A.cname cdecl)
                    (StructDecl ((data_rec_name $ A.cname cdecl)) $
                     zip
                     (map (Typ . show . A.ftype) (A.fields cdecl))
                     (map (Var . show . A.fname) (A.fields cdecl)))


      mthd_dispatch_clause :: A.ClassDecl -> A.MethodDecl -> (CCode Name, CCode Stat)
      mthd_dispatch_clause cdecl mdecl =
        ((method_msg_name (A.cname cdecl) (A.mname mdecl)),
         Statement
         (Call
          ((method_impl_name (A.cname cdecl) (A.mname mdecl)))
          [Var "p"]
         -- fixme what about arguments?
          ))
        
      dispatchfun_decl =
          (Function (Typ "static void") (class_dispatch_name $ A.cname cdecl)
           ([(Typ "pony_actor_t*", Var "this"),
             (Typ "void*", Var "p"),
             (Typ "uint64_t", Var "id"),
             (Typ "int", Var "argc"),
             (Typ "pony_arg_t*", Var "argv")])
           (Switch (Var "id")
            ((Nam "PONY_MAIN",

              Concat $ [alloc_instr,
                        (if (A.cname cdecl) == (A.Type "Main")
                         then Statement $ Call ((method_impl_name (A.Type "Main") (A.Name "main")))
                                                [Var $ "p"]
                         else Concat [])]) :

             (Nam "MSG_alloc", alloc_instr) :

             (map (mthd_dispatch_clause cdecl) (A.methods cdecl)))
             (Embed "printf(\"error, got invalid id: %llu\",id);")))
          where
            alloc_instr = Concat $ map Statement $
                          [(Var "p") `Assign`
                           (Call (Var "pony_alloc")
                                     [(Call
                                       (Var $ "sizeof")
                                       [AsExpr . Embed $ show (data_rec_name $ A.cname cdecl)])]),
                           Call (Var $ "pony_set")
                                    [Var "p"]]

      tracefun_decl = (Function
                       (Typ "static void")
                       (class_trace_fn_name (A.cname cdecl))
                       [(Typ "void*", Var "p")]
                       (Embed "//Todo!"))
      message_type_decl = Function (Typ "static pony_msg_t*")
                          (class_message_type_name $ A.cname cdecl)
                          [(Typ "uint64_t", Var "id")]
                          (Concat [(Switch (Var "id")
                                   ((Nam "MSG_alloc", Embed "return &m_MSG_alloc;")
                                    :(map (\mdecl -> message_type_clause (A.cname cdecl) (A.mname mdecl))
                                      (A.methods cdecl)))
                                   (Concat [])),
                                   (Embed "return NULL;")])
        where
          message_type_clause :: A.Type -> A.Name -> (CCode Name, CCode Stat)
          message_type_clause cname mname =
            (method_msg_name cname mname,
             Embed $ "return &" ++ show (method_message_type_name cname mname) ++ ";")

      pony_msg_t_impls :: [CCode Toplevel]
      pony_msg_t_impls = map pony_msg_t_impl (A.methods cdecl)

      pony_msg_t_impl :: A.MethodDecl -> CCode Toplevel
      pony_msg_t_impl mdecl = 
          Embed $ "static pony_msg_t " ++ 
          show (method_message_type_name
                (A.cname cdecl) 
                (A.mname mdecl)) ++
                   "= {" ++
                   (show $ length (A.mparams mdecl)) ++
                   ", {{NULL, 0, PONY_PRIMITIVE}}};"
        
      pony_actor_t_impl :: CCode Toplevel
      pony_actor_t_impl = EmbedC $
                          Statement (Assign
                                     ((Embed $ "static pony_actor_type_t " ++ show (actor_rec_name (A.cname cdecl))) :: CCode Lval)
                                     (Record [AsExpr . AsLval . Nam $ ("ID_"++(show $ A.cname cdecl)),
                                              tracefun_rec,
                                              (EmbedC $ class_message_type_name (A.cname cdecl)),
                                              (EmbedC $ class_dispatch_name $ A.cname cdecl)]))

      tracefun_rec :: CCode Expr
      tracefun_rec = Record [AsExpr . AsLval $ (class_trace_fn_name $ A.cname cdecl),
                             Call (Var "sizeof") [AsExpr . Embed $ show $ data_rec_name (A.cname cdecl)],
                             AsExpr . AsLval . Nam $ "PONY_ACTOR"]

comment_section :: String -> CCode a
comment_section s = EmbedC $ Concat $ [Embed $ take (5 + length s) $ repeat '/',
                         Embed $ "// " ++ s]

main_dispatch_clause = (Var "PONY_MAIN",
                        Concat $ map Statement [
                                      Assign
                                      (Decl $ (Typ "Main_data*", Var "d"))
                                      (Call (Var "pony_alloc") 
                                                [(Call (Var "sizeof") [Var $ "Main_data"])]),
                                      Call (Var "pony_set") [Var $ "d"],
                                      Call (Var "Main_main") [Var $ "d"]])

instance FwdDeclaration A.ClassDecl (CCode Toplevel) where
  fwd_decls cdecl =
      EmbedC $ Concat $ (comment_section "Forward declarations") :
        map Embed
                ["static pony_actor_type_t " ++ (show . actor_rec_name $ A.cname cdecl) ++ ";",
                 "static void " ++ (show $ A.cname cdecl) ++
                 "_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*);"]

