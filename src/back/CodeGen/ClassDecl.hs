{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs #-}

{-| Translate a @ClassDecl@ (see "AST") to its @CCode@ (see
"CCode.Main") equivalent. The contribution of this module are two instances:

  - @Translatable ClassDecl (Reader Context (CCode Toplevel))@ --
translates the class declaration into ccode, where the data are
represented as a record and the methods are represented as in
"CodeGen.MethodDecl".

  - @instance FwdDeclaration ClassDecl (CCode Toplevel)@ -- forward
declarations for all the parts of the implementations; we want all
@Encore@ classes to have the semantics of mutually recursive
definitions.

 -}

module CodeGen.ClassDecl () where

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

import Control.Monad.Reader hiding (void)

-- | A @ClassDecl@ is not translated to @CCode@ directly, but to a @Reader@ of @Context@ (see "CodeGen.Context")
instance Translatable A.ClassDecl (Reader Ctx.Context (CCode FIN)) where
  translate cdecl 
      | A.isActive cdecl = translateActiveClass cdecl
      | otherwise        = translatePassiveClass cdecl

translateActiveClass cdecl@(A.Class{A.cname = cname, A.fields = fields, A.methods = methods}) =
    do method_impls <- mapM (\mdecl -> (local (Ctx.with_class cdecl) (translate mdecl))) methods
       return $ Program $ Concat $
         (LocalInclude "header.h") :
         [data_struct] ++
         [tracefun_decl] ++
         pony_msg_t_impls ++
         [message_type_decl] ++
         method_impls ++
         [dispatchfun_decl] ++
         [pony_actor_t_impl]
    where
      data_struct :: CCode Toplevel
      data_struct = StructDecl (data_rec_type cname) $
                     ((Ptr $ pony_actor_t, Var "aref") :
                         zip
                         (map (translate  . A.ftype) fields)
                         (map (Var . show . A.fname) fields))

      tracefun_decl :: CCode Toplevel
      tracefun_decl = Function
                       (void) (class_trace_fn_name cname)
                       [(Ptr void, Var "p")]
                       (Seq $ map trace_field fields)
          where
            trace_field A.Field {A.ftype = ty, A.fname = f}
                | Ty.isActiveRefType ty = 
                    Call (Nam "pony_traceactor") [get_field f]
                | Ty.isPassiveRefType ty = 
                    Call (Nam "pony_traceobject") [get_field f, AsLval $ class_trace_fn_name ty]
                | otherwise = 
                    Embed $ "/* Not tracing field '" ++ show f ++ "' */"

            get_field f = Deref (Cast (data_rec_ptr cname) (Var "p")) `Dot` (Nam $ show f)

      pony_msg_t_impls :: [CCode Toplevel]
      pony_msg_t_impls = map pony_msg_t_impl methods
          where
            pony_msg_t_impl mdecl = 
                Concat
                  [AssignTL
                     (Decl (Static (Typ "pony_msg_t"), 
                           (method_message_type_name cname (A.mname mdecl))))
                     (if (A.isMainClass cdecl) && (A.mname mdecl == ID.Name "main") then
                          (Record 
                           [Int $ length (A.mparams mdecl), 
                            Record $ map (pony_mode . A.getType) (A.mparams mdecl)])
                      else
                          (Record
                           [Int $ length (A.mparams mdecl) + 1, -- plus 1 for future argument
                            Record $ Nam "PONY_ACTOR" : map (pony_mode . A.getType) (A.mparams mdecl)])), 
                   AssignTL 
                     (Decl (Static (Typ "pony_msg_t"), 
                            one_way_message_type_name cname (A.mname mdecl)))
                     (Record [Int $ length (A.mparams mdecl), 
                              Record $ map (pony_mode . A.getType) (A.mparams mdecl)])]

            pony_mode ty =
                case translate ty of
                  Ptr (Typ "pony_actor_t") -> Nam "PONY_ACTOR"
                  _other -> Nam "PONY_NONE"

      message_type_decl :: CCode Toplevel
      message_type_decl = 
          Function (Static . Ptr . Typ $ "pony_msg_t") 
                   (class_message_type_name cname)
                   [(Typ "uint64_t", Var "id")]
                   (Seq [Switch (Var "id")
                           ((Nam "MSG_alloc", Return $ Amp $ Var "m_MSG_alloc") :
                            (Nam "FUT_MSG_RESUME", Return $ Amp $ Var "m_resume_get") :
                            (Nam "FUT_MSG_RUN_CLOSURE", Return $ Amp $ Var "m_run_closure") :
                            (concatMap type_clause methods))
                            (Skip),
                         (Return Null)])
        where
          type_clause mdecl = 
              [message_type_clause cname (A.mname mdecl),
               one_way_message_type_clause cname (A.mname mdecl)]
          message_type_clause :: Ty.Type -> ID.Name -> (CCode Name, CCode Stat)
          message_type_clause cname mname =
              if mname == (ID.Name "main") then
                  (Nam "PONY_MAIN",
                   Return $ Amp (method_message_type_name cname mname))
              else
            (method_msg_name cname mname,
             Return $ Amp (method_message_type_name cname mname))

          one_way_message_type_clause :: Ty.Type -> ID.Name -> (CCode Name, CCode Stat)
          one_way_message_type_clause cname mname =
            (one_way_send_msg_name cname mname,
             Return $ Amp (one_way_message_type_name cname mname))
        
      dispatchfun_decl :: CCode Toplevel
      dispatchfun_decl =
          (Function (Static void) (class_dispatch_name cname)
           ([(Ptr . Typ $ "pony_actor_t", Var "this"),
             (Ptr void, Var "p"),
             (Typ "uint64_t", Var "id"),
             (Typ "int", Var "argc"),
             (Ptr . Typ $ "pony_arg_t", Var "argv")])
           (Switch (Var "id")
            ((Nam "MSG_alloc", alloc_instr) :
             (Nam "FUT_MSG_RESUME", fut_resume_instr) :
             (Nam "FUT_MSG_RUN_CLOSURE", fut_run_closure_instr) :
             (if (A.isMainClass cdecl)
              then pony_main_clause : (method_clauses $ filter ((/= ID.Name "main") . A.mname) methods)
              else method_clauses $ methods
             ))
             (Statement $ Call (Nam "printf") [String "error, got invalid id: %llu", AsExpr $ Var "id"])))
          where
            alloc_instr = let size = Call (Var "sizeof") [Var $ show (data_rec_name cname)]
                          in
                            Seq
                              [Assign (Var "p") 
                                      (Call (Nam "pony_alloc") [size]),
                               Statement $ Call (Nam "memset") [AsExpr $ Var "p", Int 0, size],
                               (Assign (Deref (Cast (data_rec_ptr cname) (Var "p") ) `Dot` Nam "aref")
                                       (Var "this")),
                               (Statement $ Call (Nam "pony_set") [Var "p"])]

            fut_resume_instr = 
                Seq 
                  [Assign (Decl (Ptr $ Typ "resumable_t", Var "r")) 
                          ((ArrAcc 0 (Var "argv")) `Dot` (Nam "p")),
                   Statement $ Call (Nam "future_resume") [Var "r"]]

            fut_run_closure_instr = 
                Seq
                  [Assign (Decl (closure, Var "closure"))
                          ((ArrAcc 0 (Var "argv")) `Dot` (Nam "p")),
                   Assign (Decl (Typ "value_t", Var "closure_arguments[]"))
                          (Record [UnionInst (Nam "p") (ArrAcc 1 (Var "argv") `Dot` (Nam "p"))]),
                   Statement $ Call (Nam "closure_call") [Var "closure", Var "closure_arguments"]]

            pony_main_clause =
                (Nam "PONY_MAIN",
                 Seq $ [alloc_instr,
                        Statement $ Call ((method_impl_name (Ty.refType "Main") (ID.Name "main")))
                                         [AsExpr $ Var "p",
                                          AsExpr $ (ArrAcc 0 (Var "argv")) `Dot` (Nam "i"),
                                          Cast (Ptr $ Ptr char) $ (ArrAcc 1 (Var "argv")) `Dot` (Nam "p")]])

            method_clauses :: [A.MethodDecl] -> [(CCode Name, CCode Stat)]
            method_clauses = concatMap method_clause

            method_clause m = [mthd_dispatch_clause m, one_way_send_dispatch_clause m]

            mthd_dispatch_clause mdecl =
                (method_msg_name cname (A.mname mdecl),
                 Seq [Assign (Decl (Ptr $ Typ "future_t", Var "fut"))
                      ((ArrAcc 0 ((Var "argv"))) `Dot` (Nam "p")),
                      Statement $ Call (Nam "future_fulfil") 
                                       [AsExpr $ Var "fut",
                                        Cast (Ptr void) 
                                             (Call (method_impl_name cname (A.mname mdecl))
                                              ((AsExpr . Var $ "p") : 
                                               (paramdecls_to_argv 1 $ A.mparams mdecl)))]])

            one_way_send_dispatch_clause mdecl =
                (one_way_send_msg_name cname (A.mname mdecl),
                 (Statement $
                  Call (method_impl_name cname (A.mname mdecl))
                       ((AsExpr . Var $ "p") : (paramdecls_to_argv 0 $ A.mparams mdecl))))

            paramdecls_to_argv :: Int -> [A.ParamDecl] -> [CCode Expr]
            paramdecls_to_argv start_idx = zipWith paramdecl_to_argv [start_idx..]

            paramdecl_to_argv argv_idx (A.Param {A.pname = na, A.ptype = ty}) =
                let arg_cell = ArrAcc argv_idx (Var "argv")
                in
                  AsExpr $ 
                  arg_cell `Dot`
                      (case translate ty of
                         (Typ "int64_t") -> (Nam "i")
                         (Typ "double")  -> (Nam "d")
                         (Ptr _)         -> (Nam "p")
                         other           ->
                             error $ "ClassDecl.hs: paramdecl_to_argv not implemented for "++show ty)

      pony_actor_t_impl =
                (AssignTL
                 (Decl (Typ "pony_actor_type_t", AsLval $ actor_rec_name cname))
                 (Record [AsExpr . AsLval . Nam $ ("ID_"++(show cname)),
                          tracefun_rec,
                          AsExpr . AsLval $ class_message_type_name cname,
                          AsExpr . AsLval $ class_dispatch_name cname]))
          where
            tracefun_rec = 
                Record [Call (Nam "sizeof") [Var . show $ data_rec_name cname],
                        AsExpr . AsLval $ (class_trace_fn_name cname),
                        Null,
                        Null]

translatePassiveClass cdecl@(A.Class{A.cname = cname, A.fields = fields, A.methods = methods}) = 
    do method_impls <- mapM (\mdecl -> (local (Ctx.with_class cdecl) (translate mdecl))) methods
       return $ Program $ Concat $
         (LocalInclude "header.h") :
         [tracefun_decl] ++
         method_impls
    where
      tracefun_decl :: CCode Toplevel
      tracefun_decl = 
          Function void (class_trace_fn_name cname)
                   [(Ptr void, Var "p")]
                   (Seq $ map trace_field fields)
          where
            trace_field A.Field {A.ftype = ty, A.fname = f}
                | Ty.isActiveRefType ty = Call (Nam "pony_traceactor") [get_field f]
                | Ty.isPassiveRefType ty = Call (Nam "pony_traceobject") [get_field f, AsLval $ class_trace_fn_name ty]
                | otherwise = Embed $ "/* Not tracing field '" ++ show f ++ "' */"
                where
                  get_field f = Deref (Cast (data_rec_ptr cname) (Var "p")) `Dot` (Nam $ show f)