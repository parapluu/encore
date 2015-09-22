module CodeGen.Header(generate_header) where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type ()

import CCode.Main
import CCode.PrettyCCode ()

import qualified AST.AST as A
import qualified Identifiers as ID
import qualified Types as Ty

-- | Generates the C header file for the translated program
-- | This function generates all the common code, generate_header_recurser generates class specific code
generate_header :: A.Program -> CCode FIN

generate_header p =
    Program $
    IfNDefine "HEADER_H" $
    Concat $
    HashDefine "HEADER_H" :
    HashDefine "_XOPEN_SOURCE 800" :
    (Includes [
      "pthread.h", -- Needed because of the use of locks in future code, remove if we choose to remove lock-based futures
      "pony.h",
      "stdlib.h",
      "closure.h",
      "stream.h",
      "array.h",
      "range.h",
      "future.h",
      "task.h",
      "string.h",
      "stdio.h",
      "stdarg.h"
     ]) :
    HashDefine "UNIT ((void*) -1)" :

    [comment_section "Shared messages"] ++
    shared_messages ++

    [comment_section "Embedded code"] ++
    map Embed allembedded ++

    [comment_section "Class type decls"] ++
    class_type_decls ++

    [comment_section "Trait type decls"] ++
    trait_type_decls ++

    [comment_section "Passive class types"] ++
    passive_types ++

    [comment_section "Runtime types"] ++
    runtime_type_decls ++

    [comment_section "Message IDs"] ++
    [message_enums] ++

    [comment_section "Message types"] ++
    pony_msg_t_typedefs ++
    pony_msg_t_impls ++

    [comment_section "Global functions"] ++
    global_function_decls ++

    [comment_section "Class IDs"] ++
    [class_enums] ++

    [comment_section "Trace functions"] ++
    trace_fn_decls ++

    [comment_section "Runtime type init functions"] ++
    runtime_type_fn_decls ++

    [comment_section "Methods"] ++
    concatMap method_fwds allclasses ++

    [comment_section "Main actor rtti"] ++
    [extern_main_rtti] ++

    [comment_section "Trait types"] ++
    [trait_method_enums] ++
    trait_types
   where
     extern_main_rtti = DeclTL (Typ "extern pony_type_t", Var "_enc__active_Main_type")

     shared_messages =
          [DeclTL (pony_msg_t, Var "m_MSG_alloc"),
           DeclTL (pony_msg_t, Var "m_resume_get"),
           DeclTL (pony_msg_t, Var "m_resume_suspend"),
           DeclTL (pony_msg_t, Var "m_resume_await"),
           DeclTL (pony_msg_t, Var "m_run_closure")
          ]

     allTraits = A.allTraits p
     allclasses = A.allClasses p
     allfunctions = A.allFunctions p
     allembedded = A.allEmbedded p

     pony_msg_t_typedefs :: [CCode Toplevel]
     pony_msg_t_typedefs = map pony_msg_t_typedef_class allclasses
             where
                 pony_msg_t_typedef_class cdecl@(A.Class{A.cname, A.cmethods}) =
                     Concat $ concatMap pony_msg_t_typedef cmethods
                     where
                         pony_msg_t_typedef mdecl =
                             [Typedef (Struct $ fut_msg_type_name cname (A.mname mdecl)) (fut_msg_type_name cname (A.mname mdecl)),
                              Typedef (Struct $ one_way_msg_type_name cname (A.mname mdecl)) (one_way_msg_type_name cname (A.mname mdecl))]

     pony_msg_t_impls :: [CCode Toplevel]
     pony_msg_t_impls = map pony_msg_t_impls_class allclasses
                 where
                   pony_msg_t_impls_class cdecl@(A.Class{A.cname, A.cmethods}) =
                       Concat $ map pony_msg_t_impl cmethods
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

     global_function_decls = map global_function_decl allfunctions
            where
                global_function_decl A.Function{A.funname} =
                   DeclTL (closure, AsLval $ global_closure_name funname)

     message_enums =
                 let
                     meta = concatMap (\cdecl -> zip (repeat $ A.cname cdecl) (map A.mname (A.cmethods cdecl))) allclasses
                     method_msg_names = map (show . (uncurry fut_msg_id)) meta
                     one_way_msg_names = map (show . (uncurry one_way_msg_id)) meta
                 in
                        Enum $ (Nam "__MSG_DUMMY__ = 1024") : map Nam (method_msg_names ++ one_way_msg_names)

     class_enums =
       let
        class_ids = map (ref_type_id . A.getType) allclasses
        trait_ids = map (ref_type_id . A.getType) allTraits
       in
        Enum $ (Nam "__ID_DUMMY__ = 1024") : class_ids ++ trait_ids

     trace_fn_decls = map trace_fn_decl allclasses
         where
           trace_fn_decl A.Class{A.cname} =
               FunctionDecl void (class_trace_fn_name cname) [Ptr void]

     runtime_type_fn_decls = map runtime_type_fn_decl allclasses
         where
           runtime_type_fn_decl A.Class{A.cname} =
               FunctionDecl void (runtime_type_init_fn_name cname) [Ptr . AsType $ class_type_name cname, Embed "..."]

     class_type_decls = map class_type_decl allclasses
                 where
                   class_type_decl A.Class{A.cname} =
                       Typedef (Struct $ class_type_name cname) (class_type_name cname)

     passive_types = map passive_type $ filter (not . A.isActive) allclasses
                 where
                   passive_type A.Class{A.cname, A.cfields} =
                       let typeParams = Ty.getTypeParameters cname in
                       StructDecl (AsType $ class_type_name cname)
                                  ((Ptr pony_type_t, AsLval $ self_type_field) :
                                   map (\ty -> (Ptr pony_type_t, AsLval $ type_var_ref_name ty)) typeParams ++
                                   zip
                                   (map (translate . A.ftype) cfields)
                                   (map (AsLval . field_name . A.fname) cfields))
     trait_method_enums =
       let
         dicts = map (\t -> (A.getType t, A.tmethods t)) allTraits
         pairs = concatMap (\(t, ms) -> zip (repeat t) (map A.mname ms)) dicts
         syncs = map (show . (uncurry one_way_msg_id)) pairs
       in Enum $ (Nam "__TRAIT_METHOD_DUMMY__ = 1024") : map Nam syncs

     trait_type_decls = map trait_type_decl allTraits
       where
         trait_type_decl A.Trait{A.tname} =
           let ty = ref_type_name tname in Typedef (Struct $ ty) ty

     trait_types = map trait_type allTraits
       where
         trait_type A.Trait{A.tname} =
           let
             formal = Ty.getTypeParameters tname
             self = (Ptr pony_type_t, AsLval $ self_type_field)
           in
             StructDecl (AsType $ ref_type_name tname) [self]

     runtime_type_decls = map type_decl allclasses ++ map type_decl allTraits
       where
         type_decl ref =
           let
             ty = A.getType ref
             runtime_ty = runtime_type_name ty
           in
             DeclTL (Extern pony_type_t, AsLval runtime_ty)

     method_fwds cdecl@(A.Class{A.cname, A.cmethods}) = map method_fwd cmethods
                 where
                   method_fwd A.Method{A.mtype, A.mname, A.mparams} =
                     let params = if (A.isMainClass cdecl) && (mname == ID.Name "main")
                                  then [Ptr . AsType $ class_type_name cname, array]
                                  else (Ptr . AsType $ class_type_name cname) : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
                     in
                       FunctionDecl (translate mtype) (method_impl_name cname mname) params
                   method_fwd A.StreamMethod{A.mtype, A.mname, A.mparams} =
                     let params = (Ptr . AsType $ class_type_name cname) : stream : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
                     in
                       FunctionDecl void (method_impl_name cname mname) params

comment_section :: String -> CCode Toplevel
comment_section s = Embed $ (replicate (5 + length s) '/') ++ "\n// " ++ s
