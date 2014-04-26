{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CodeGen (code_from_AST) where

import Data.List
import Data.Char
import Data.Maybe
import CCode.CCode
import Control.Monad.Reader
import CodeGen.CCodeNames

import qualified AST as A
import qualified CodeGen.Context as Ctx

code_from_AST :: A.Program -> CCode
code_from_AST = translate

class Translatable a b where
  translate :: a -> b

class FwdDeclaration a b where
  fwd_decls :: a -> b

instance Translatable A.Op CCode where
  translate op = Embed $ case op of
    A.LT -> "<"
    A.GT -> ">"
    A.EQ -> "=="
    A.NEQ -> "!="
    A.PLUS -> "+"
    A.MINUS -> "-"

instance Translatable A.Lvar (Reader Ctx.Context CCode) where
  translate (A.LVar name) = return $ Embed $ show name
  translate (A.LField ex name) = do
    tex <- translate ex
    return $ (Deref tex) `Dot` (show name)

instance Translatable A.Expr (Reader Ctx.Context CCode) where
  translate (A.Skip) = return $ Embed "/* skip */"
  translate (A.Null) = return $ Embed "0"
  translate (A.Binop op e1 e2) = do
    te1 <- translate e1
    te2 <- translate e2
    return $ C [(Embed "("),
                te1,
                translate op,
                te2,
                (Embed ")")]
  translate (A.Print (A.StringLiteral s)) =
    return $ Embed $ "printf(\"%s\\n\", \"" ++ s ++ "\" );"
  translate (A.Print (A.FieldAccess (A.VarAccess var) name)) =
    return $ Embed $ "printf(\"%i\\n\", " ++ show var ++ "->" ++ show name ++ " );"
  translate (A.Seq es) = do
    tes <- mapM translate es
    return $ C (map Statement tes)
  translate (A.Assign lvar expr) = do
    texpr <- translate expr
    tlvar <- translate lvar
    return $ Assign tlvar texpr
  translate (A.VarAccess name) =
    return $ Embed $ show name
  translate (A.FieldAccess exp name) = do
    texp <- translate exp
    return $ Deref texp `Dot` (show name)
  translate (A.IntLiteral i) =
    return $ Embed $ show i
  translate (A.StringLiteral s) =
    return $ Embed $ show s
  translate (A.Let name ty e1 e2) = do
    te1 <- translate e1
    te2 <- local (Ctx.with_local (A.Param (ty, name))) $ translate e2
    return (BracedBlock $ C $
            map Statement
            ((Decl (CVarSpec (embedCType "pony_actor_t*", show name))) :
             [Assign (Var $ show name) te1,
              te2
             ]))
  translate (A.New ty) = return $ Embed $ "create_and_send(&"++show ty++"_actor, MSG_alloc)"
  translate (A.Call { A.target=expr, A.tmname=name, A.args=args } ) =
    case expr of
      (A.VarAccess (A.Name "this")) -> do
        -- call synchronously
        cname <- asks (A.cname . fromJust . Ctx.the_class)
        targs <- mapM translate args
        return $ Call (method_impl_name cname name) targs
      (A.VarAccess other) -> do
        -- send message
        -- fixme: how do we send arguments?
        other_ty <- asks (fromJust . (Ctx.type_of $ other))
        return $ Statement $ Call "pony_send" [Var $ show other, Var (method_msg_name other_ty name)]
      no_var_access -> error "calls are only implemented on variables for now"
  translate other = return $ Embed $ "/* missing: " ++ show other ++ "*/"

dataStructType :: A.Type -> CType
dataStructType ty =
    embedCType $
               case ty of
                 (A.Type "Object") -> "void*"
                 (A.Type other_ty) ->
                     if isLower $ head other_ty
                     then other_ty
                     else data_rec_ptr ty

--instance Translatable [FieldDecl] CCode where
--  translate fs = (StructDecl $ map VarDecl $ zip (map ftype fs) (map fname fs))

instance Translatable A.MethodDecl (Reader Ctx.Context CCode) where
  translate mdecl = do
    this_ty <- asks (A.cname . fromJust . Ctx.the_class)
    cdecl <- asks (fromJust . Ctx.the_class)
    tmbody <- local (Ctx.with_method mdecl) $ translate (A.mbody mdecl)
    return $ 
      (Function (dataStructType (A.rtype mdecl)) (method_impl_name (A.cname cdecl) (A.mname mdecl))
       (CVarSpec (dataStructType this_ty, "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
       [Statement tmbody])
    where
      mparam_to_cvardecl (A.Param (ty, na)) = CVarSpec (dataStructType ty, show na)

--      method_impl :: A.Type -> A.MethodDecl -> CCode
--      method_impl this_ty mdecl = (Function (dataStructType (A.rtype mdecl)) (A.cname cdecl ++ "_" ++ A.mname mdecl)
--                                   (CVarSpec (dataStructType this_ty, "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
--                                   [Statement $
--                                    --runReader (translate (A.mbody mdecl)) (ask { Ctx.the_class = Just cdecl })
--                                    C[]
--                                   ])

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
                     zip (map (dataStructType . A.ftype) (A.fields cdecl)) (map (show . A.fname) (A.fields cdecl)))
      mthd_dispatch_clause cdecl mdecl =
        (Var (method_msg_name (A.cname cdecl) (A.mname mdecl)),
         Statement (Call (method_impl_name (A.cname cdecl) (A.mname mdecl)) [Var "p"]
         -- fixme what about arguments?
          ))
        
      dispatchfun_decl =
          (Function (dataStructType (A.Type "static void")) (class_dispatch_name $ A.cname cdecl)
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

instance FwdDeclaration A.Program CCode where
  fwd_decls (A.Program cs) = C $ [create_and_send_fn,
                                  msg_alloc_decl,
                                  msg_enum (A.Program cs),
                                  class_ids_enum (A.Program cs)]
    where
      msg_alloc_decl =
          Statement $ Embed $
                        "static pony_msg_t m_MSG_alloc = {0, {{NULL, 0, PONY_PRIMITIVE}}}"
      create_and_send_fn =
          Embed $
                    "pony_actor_t* create_and_send(pony_actor_type_t* type, uint64_t msg_id) {\n" ++
--                    "  printf(\"creating:\\n\");\n" ++
                    "  pony_actor_t* ret = pony_create(type);\n" ++
                    "  pony_send(ret, msg_id);\n" ++
--                    "  printf(\"created and sent!\\n\");\n" ++
                    "  \n" ++
                    "  return ret;\n" ++
                    "}"
      msg_enum :: A.Program -> CCode
      msg_enum (A.Program cs) =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (A.methods cdecl)) cs
          lines = map (\ (cname, mdecl) -> "MSG_" ++ show cname ++ "_" ++ (show $ A.mname mdecl)) meta
        in
         Statement $ Enum $ "MSG_alloc":lines

      class_ids_enum :: A.Program -> CCode
      class_ids_enum (A.Program cs) =
        let
          names = map (("ID_"++) . show . A.cname) cs
        in
         Statement $ Enum names

instance Translatable A.Program CCode where
  translate (A.Program cs) =
    C $
    (HashDefine "__STDC_FORMAT_MACROS") :
    (Includes ["pony/pony.h",
               "stdlib.h",
               "stdio.h",
               "string.h",
               "inttypes.h",
               "assert.h"]) :
    (fwd_decls (A.Program cs)) :
    (map fwd_decls cs) ++
    (map translate_class_here cs) ++
    [(Function
      (embedCType "static void") "dispatch"
      (map CVarSpec [(embedCType "pony_actor_t*", "this"),
                     (embedCType "void*", "p"),
                     (embedCType "uint64_t", "id"),
                     (embedCType "int", "argc"),
                     (embedCType "pony_arg_t*", "argv")])
      [Switch "id"
       [(Var "PONY_MAIN",
         C $ map Statement [Decl $ CVarSpec (embedCType "Main_data*", "d"),
                            Assign (Var "d") (Call "pony_alloc" [(Call "sizeof" [Var "Main_data"])]),
                            Call "pony_set" [Var "d"],
                            Call "Main_main" [Var "d"]])]
       (Embed "printf(\"error, got invalid id: %llu\",id);")]),
     (Function
      (embedCType "int") "main"
      [CVarSpec (embedCType "int","argc"), CVarSpec (embedCType "char**","argv")]
      [Embed "return pony_start(argc, argv, pony_create(&Main_actor));"])]
    where
      translate_class_here :: A.ClassDecl -> CCode
      translate_class_here cdecl = runReader (translate cdecl) $ Ctx.mk (A.Program cs)
