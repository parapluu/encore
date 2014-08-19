{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Header(generate_header) where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.MethodDecl
import CodeGen.Type
import qualified CodeGen.Context as Ctx

import CCode.Main
import CCode.PrettyCCode

import Data.List

import qualified AST.AST as A
import qualified Identifiers as ID
import qualified Types as Ty

-- | Generates the C header file for the translated program
generate_header :: A.Program -> CCode FIN
generate_header A.Program{A.etl = A.EmbedTL{A.etlheader}, A.functions, A.classes} = 
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
        ]) :
       (HashDefine "UNIT NULL - 1") :

       [comment_section "Embedded code"] ++
       [Embed etlheader] ++

       [comment_section "Shared functions"] ++
       [create_and_send_decl] ++

       [comment_section "Shared messages"] ++
       shared_messages ++

       [comment_section "Global functions"] ++
       global_function_decls ++

       [comment_section "Message IDs"] ++
       [message_enums] ++

       [comment_section "Class IDs"] ++
       [class_enums] ++

       [comment_section "Trace functions"] ++
       trace_fn_decls ++

       [comment_section "Data structs"] ++
       data_struct_decls ++ 

       [comment_section "Passive class data structs"] ++
       passive_data_structs ++ 

       [comment_section "Actor types"] ++
       actor_decls ++

       [comment_section "Methods"] ++
       concatMap method_fwds classes
    where
      create_and_send_decl = 
          FunctionDecl (Ptr pony_actor_t) (Nam "create_and_send") 
                       [Ptr pony_actor_type_t, uint]

      shared_messages = 
          [DeclTL (pony_msg_t, Var "m_MSG_alloc"),
           DeclTL (pony_msg_t, Var "m_resume_get"),
           DeclTL (pony_msg_t, Var "m_run_closure")]

      global_function_decls = map global_function_decl functions
          where
            global_function_decl A.Function{A.funname} = 
                DeclTL (closure, AsLval $ global_closure_name funname)


      message_enums =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (map A.mname (A.methods cdecl))) classes
          method_msg_names = map (show . (uncurry method_msg_name)) meta
          one_way_msg_names = map (show . (uncurry one_way_send_msg_name)) meta
        in
         Enum $ map Nam $ "MSG_alloc":(method_msg_names ++ one_way_msg_names)

      class_enums =
        let
          names = map (("ID_"++) . show . A.cname) classes
        in
         Enum $ map Nam $ names

      trace_fn_decls = map trace_fn_decl classes
          where
            trace_fn_decl A.Class{A.cname} =
                      FunctionDecl void (class_trace_fn_name cname) [Ptr void]

      data_struct_decls = map data_struct_decl classes
          where
            data_struct_decl A.Class{A.cname} = 
                Typedef (Struct $ data_rec_name cname) (data_rec_name cname)

      passive_data_structs = map passive_data_struct $ filter (not . A.isActive) classes
          where
            passive_data_struct A.Class{A.cname, A.fields} = 
                StructDecl (data_rec_type cname) 
                           (zip
                            (map (translate . A.ftype) fields)
                            (map (Var . show . A.fname) fields))

      actor_decls = map actor_decl $ filter A.isActive classes
          where
            actor_decl A.Class{A.cname} = DeclTL (pony_actor_type_t, AsLval $ actor_rec_name cname)

      method_fwds cdecl@(A.Class{A.cname, A.methods}) = map method_fwd methods
          where
            method_fwd A.Method{A.mtype, A.mname, A.mparams} =
              let params = if (A.isMainClass cdecl) && (mname == ID.Name "main")
                           then [data_rec_ptr cname, int, Ptr $ Ptr char]
                           else data_rec_ptr cname : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
              in
                FunctionDecl (translate mtype) (method_impl_name cname mname) params

comment_section :: String -> CCode Toplevel
comment_section s = Embed $ (take (5 + length s) $ repeat '/') ++ "\n// " ++ s
