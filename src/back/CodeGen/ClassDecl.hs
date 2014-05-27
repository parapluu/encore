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
      data_struct = StructDecl (data_rec_name $ A.cname cdecl) $
                     ((Ptr $ Embed "pony_actor_t", Var "aref") :
                         zip
                         (map (translate  . A.ftype) (A.fields cdecl))
                         (map (Var . show . A.fname) (A.fields cdecl)))


      mthd_dispatch_clause :: A.ClassDecl -> A.MethodDecl -> (CCode Name, CCode Stat)
      mthd_dispatch_clause cdecl mdecl =
        ((method_msg_name (A.cname cdecl) (A.mname mdecl)),
         Statement
         (Call
          ((method_impl_name (A.cname cdecl) (A.mname mdecl)))
          ((AsExpr . Var $ "p") : (paramdecls_to_argv $ A.mparams mdecl))
         -- fixme what about arguments?
          ))

      paramdecl_to_argv :: Int -> A.ParamDecl -> CCode Expr
      paramdecl_to_argv argv_idx (A.Param {A.pname = na, A.ptype = ty}) =
          let arg_cell = ArrAcc argv_idx (Var "argv")
          in
            AsExpr $ Dot arg_cell
                       (case translate ty of
                          (Typ "int64_t") -> (Nam "i")
                          (Typ "double")  -> (Nam "d")
                          (Ptr _)         -> (Nam "p")
                          other           ->
                              error $ "ClassDecl.hs: paramdecl_to_argv not implemented for "++show ty)

      paramdecls_to_argv :: [A.ParamDecl] -> [CCode Expr]
      paramdecls_to_argv = zipWith paramdecl_to_argv [0..]
        
      dispatchfun_decl :: CCode Toplevel
      dispatchfun_decl =
          (Function (Static void) (class_dispatch_name $ A.cname cdecl)
           ([(Ptr . Typ $ "pony_actor_t", Var "this"),
             (Ptr void, Var "p"),
             (Typ "uint64_t", Var "id"),
             (Typ "int", Var "argc"),
             (Ptr . Typ $ "pony_arg_t", Var "argv")])
           (Switch (Var "id")
            ((Nam "MSG_alloc", alloc_instr) :
             (if (A.cname cdecl == Ty.refType "Main")
              then [pony_main_clause]
              else []) ++
             (map (mthd_dispatch_clause cdecl) (A.methods cdecl)))
             (Embed "printf(\"error, got invalid id: %llu\",id);")))
          where
            pony_main_clause =
                (Nam "PONY_MAIN",
                     Concat $ [alloc_instr,
                               (if (A.cname cdecl) == (Ty.refType "Main")
                                then Statement $ Call ((method_impl_name (Ty.refType "Main") (ID.Name "main")))
                                         [Var "p"]
                                else Concat [])])
            
            alloc_instr = Concat $
                          [(Var "p") `Assign`
                           (Statement
                            (Call (Nam "pony_alloc")
                                      [(Call
                                        (Var "sizeof")
                                        [AsExpr . Embed $ show (data_rec_name $ A.cname cdecl)])])),
                           (Assign
                            (AsLval . Embed $ "((" ++ show (data_rec_ptr $ A.cname cdecl) ++ ")p)->aref")
                            (Var "this")
                           ),
                           (Statement
                            (Call (Nam "pony_set")
                                      [Var "p"]))]

      tracefun_decl :: CCode Toplevel
      tracefun_decl = (Function
                       (Static void)
                       (class_trace_fn_name (A.cname cdecl))
                       [(Ptr void, Var "p")]
                       (Embed "//Todo!"))

      message_type_decl :: CCode Toplevel
      message_type_decl = Function (Static . Ptr . Typ $ "pony_msg_t")
                          (class_message_type_name $ A.cname cdecl)
                          [(Typ "uint64_t", Var "id")]
                          (Concat [(Switch (Var "id")
                                   ((Nam "MSG_alloc", Embed "return &m_MSG_alloc;")
                                    :(map (\mdecl -> message_type_clause (A.cname cdecl) (A.mname mdecl))
                                      (A.methods cdecl)))
                                   (Concat [])),
                                   (Embed "return NULL;")])
        where
          message_type_clause :: Ty.Type -> ID.Name -> (CCode Name, CCode Stat)
          message_type_clause cname mname =
            (method_msg_name cname mname,
             Embed $ "return &" ++ show (method_message_type_name cname mname) ++ ";")

      pony_msg_t_impls :: [CCode Toplevel]
      pony_msg_t_impls = map pony_msg_t_impl (A.methods cdecl)

      pony_mode :: Ty.Type -> CCode Name
      pony_mode ty =
          case translate ty of
            Ptr (Typ "pony_actor_t") -> Nam "PONY_ACTOR"
            _other -> Nam "PONY_PRIMITIVE" --fixme how/when will we be
                                           --using the other modes?


      pony_msg_t_impl :: A.MethodDecl -> CCode Toplevel
      pony_msg_t_impl mdecl = 
          Embed $ "static pony_msg_t " ++ 
          show (method_message_type_name
                (A.cname cdecl) 
                (A.mname mdecl)) ++
                   "= {" ++
                   (show $ length (A.mparams mdecl)) ++
                   ", {" ++
                         param_descs (A.mparams mdecl)
                   ++"}};"
          where
            param_desc :: A.ParamDecl -> String --this should NOT be a String
            param_desc (A.Param {A.pname = na, A.ptype = ty}) = "{NULL, 0, " ++ show (pony_mode ty) ++ "}"

            param_descs :: [A.ParamDecl] -> String
            param_descs ps = concat $ intersperse ", " $ map param_desc ps

      pony_actor_t_impl :: CCode Toplevel
      pony_actor_t_impl = EmbedC $
                          Statement
                          (Assign
                           ((Embed $ "static pony_actor_type_t " ++ show (actor_rec_name (A.cname cdecl))) :: CCode Lval)
                           (Record [AsExpr . AsLval . Nam $ ("ID_"++(show $ A.cname cdecl)),
                                           tracefun_rec,
                                    (EmbedC $ class_message_type_name (A.cname cdecl)),
                                    (EmbedC $ class_dispatch_name $ A.cname cdecl)]))

      tracefun_rec :: CCode Expr
      tracefun_rec = Record [AsExpr . AsLval $ (class_trace_fn_name $ A.cname cdecl),
                             Call (Nam "sizeof") [AsExpr . Embed $ show $ data_rec_name (A.cname cdecl)],
                             AsExpr . AsLval . Nam $ "PONY_ACTOR"]

instance FwdDeclaration A.ClassDecl (Reader Ctx.Context (CCode Toplevel)) where
  fwd_decls cdecl = do
      let cname = show (A.cname cdecl)
      mthd_fwds <- mapM (\mdecl -> (local (Ctx.with_class cdecl) (fwd_decls mdecl))) (A.methods cdecl)
      return $ ConcatTL $
                 (comment_section $ "Forward declarations for " ++ show (A.cname cdecl)) :
                 [Embed $ "typedef struct ___"++cname++"_data "++cname++"_data;",
                  Embed $ "static pony_actor_type_t " ++ (show . actor_rec_name $ A.cname cdecl) ++ ";",
                  Embed $ "static void " ++cname++
                            "_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*);"] ++
                 mthd_fwds

comment_section :: String -> CCode a
comment_section s = EmbedC $ Concat $ [Embed $ take (5 + length s) $ repeat '/',
                         Embed $ "// " ++ s]
