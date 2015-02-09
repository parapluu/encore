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
       IfNDefine "HEADER_H" $
       Concat $ 
       HashDefine "HEADER_H" :
       (Includes [
         "pthread.h", -- Needed because of the use of locks in future code, remove if we choose to remove lock-based futures
         "pony/pony.h",
         "stdlib.h",
         "set.h",
         "closure.h",
         "stream.h",
         "array.h",
         "future.h",
         "string.h",
         "stdio.h"
        ]) :
       (HashDefine "UNIT ((void*) -1)") :

       [comment_section "Embedded code"] ++
       [Embed etlheader] ++

       [comment_section "Shared messages"] ++
       shared_messages ++

       [comment_section "Class types"] ++
       class_type_decls ++ 

       [comment_section "Passive class types"] ++
       passive_types ++ 

       [comment_section "Runtime types"] ++
       runtime_type_decls ++

       [comment_section "Message types"] ++
       pony_msg_t_typedefs ++
       pony_msg_t_impls ++

       [comment_section "Global functions"] ++
       global_function_decls ++

       [comment_section "Message IDs"] ++
       [message_enums] ++

       [comment_section "Class IDs"] ++
       [class_enums] ++

       [comment_section "Trace functions"] ++
       trace_fn_decls ++

       [comment_section "Methods"] ++
       concatMap method_fwds classes ++

       [comment_section "Main actor rtti"] ++
       [extern_main_rtti]
    where
      extern_main_rtti = DeclTL (Typ "extern pony_type_t", Var "_enc__active_Main_type")

      shared_messages = 
          [DeclTL (pony_msg_t, Var "m_MSG_alloc"),
           DeclTL (pony_msg_t, Var "m_resume_get"),
           DeclTL (pony_msg_t, Var "m_resume_suspend"),
           DeclTL (pony_msg_t, Var "m_resume_await"),
           DeclTL (pony_msg_t, Var "m_run_closure")]

      pony_msg_t_typedefs :: [CCode Toplevel]
      pony_msg_t_typedefs = map pony_msg_t_typedef_class classes
          where
            pony_msg_t_typedef_class cdecl@(A.Class{A.cname, A.methods}) = 
                Concat $ concatMap pony_msg_t_typedef methods
                where
                  pony_msg_t_typedef mdecl = 
                      [Typedef (Struct $ fut_msg_type_name cname (A.mname mdecl)) (fut_msg_type_name cname (A.mname mdecl)),
                       Typedef (Struct $ one_way_msg_type_name cname (A.mname mdecl)) (one_way_msg_type_name cname (A.mname mdecl))]

      pony_msg_t_impls :: [CCode Toplevel]
      pony_msg_t_impls = map pony_msg_t_impls_class classes
          where
            pony_msg_t_impls_class cdecl@(A.Class{A.cname, A.methods}) = 
                Concat $ map pony_msg_t_impl methods
                where
                  pony_msg_t_impl :: A.MethodDecl -> CCode Toplevel
                  pony_msg_t_impl mdecl =
                      let argrttys = map (translate . A.getType) (A.mparams mdecl)
                          argnames_w_comments = zipWith (\n name -> (Annotated (show name) (Var ("f"++show n)))) ([1..]:: [Int]) (map A.pname $ A.mparams mdecl)
                          argspecs = zip argrttys argnames_w_comments :: [CVarSpec]
                          encore_msg_t_spec = (enc_msg_t, Var "")
                          encore_msg_t_spec_oneway = (enc_oneway_msg_t, Var "msg")
                      in Concat [StructDecl (AsType $ fut_msg_type_name cname (A.mname mdecl)) (encore_msg_t_spec : argspecs)
                                ,StructDecl (AsType $ one_way_msg_type_name cname (A.mname mdecl)) (encore_msg_t_spec_oneway : argspecs)]

      global_function_decls = map global_function_decl functions
          where
            global_function_decl A.Function{A.funname} = 
                DeclTL (closure, AsLval $ global_closure_name funname)


      message_enums =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (map A.mname (A.methods cdecl))) classes
          method_msg_names = map (show . (uncurry fut_msg_id)) meta
          one_way_msg_names = map (show . (uncurry one_way_msg_id)) meta
        in
         Enum $ (Nam "__MSG_DUMMY__ = 1024") : map Nam (method_msg_names ++ one_way_msg_names)


      class_enums =
        let
          names = map (("ID_"++) . Ty.getId . A.cname) classes
        in
         Enum $ (Nam "__ID_DUMMY__ = 1024") : map Nam names

      trace_fn_decls = map trace_fn_decl classes
          where
            trace_fn_decl A.Class{A.cname} =
                      FunctionDecl void (class_trace_fn_name cname) [Ptr void]

      class_type_decls = map class_type_decl classes
          where
            class_type_decl A.Class{A.cname} = 
                Typedef (Struct $ class_type_name cname) (class_type_name cname)

      passive_types = map passive_type $ filter (not . A.isActive) classes
          where
            passive_type A.Class{A.cname, A.fields} = 
                StructDecl (AsType $ class_type_name cname) 
                           (zip
                            (map (translate . A.ftype) fields)
                            (map (Var . show . A.fname) fields))

      runtime_type_decls = map type_decl classes
          where
            type_decl A.Class{A.cname} = DeclTL (Extern pony_type_t, AsLval $ runtime_type_name cname)

      method_fwds cdecl@(A.Class{A.cname, A.methods}) = map method_fwd methods
          where
            method_fwd A.Method{A.mtype, A.mname, A.mparams} =
              let params = if (A.isMainClass cdecl) && (mname == ID.Name "main")
                           then [Ptr . AsType $ class_type_name cname, int, Ptr $ Ptr char]
                           else (Ptr . AsType $ class_type_name cname) : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
              in
                FunctionDecl (translate mtype) (method_impl_name cname mname) params
            method_fwd A.StreamMethod{A.mtype, A.mname, A.mparams} =
              let params = (Ptr . AsType $ class_type_name cname) : stream : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
              in
                FunctionDecl void (method_impl_name cname mname) params

comment_section :: String -> CCode Toplevel
comment_section s = Embed $ (take (5 + length s) $ repeat '/') ++ "\n// " ++ s
