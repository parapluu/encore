{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODOS
-- * implement the message_type functions ::192
-- * implement the message types:
--      static pony_msg_t m_Other_init = {0, {{NULL, 0, PONY_PRIMITIVE}}};
--      static pony_msg_t m_Other_work = {2, {{NULL, 0, PONY_PRIMITIVE}}};
-- * add this to MSG_Other_init:
--      p = pony_alloc(sizeof(Other_data));  
--      pony_set(p);


module CodeGen (code_from_AST) where
import Data.List
import Data.Char
import Data.Maybe
import CCode
import Control.Monad.Reader

import qualified AST as A
import qualified Context as Ctx

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
  translate (A.New ty) = return $ Embed $ "pony_create(&"++show ty++"_actor_t)" -- continue here
  translate (A.Call { A.target=expr, A.tmname=name, A.args=args } ) =
    case expr of
      (A.VarAccess (A.Name "this")) -> do
        -- call synchronously
        this_ty <- asks (A.cname . fromJust . Ctx.the_class)
        targs <- mapM translate args
        return $ Call (show this_ty ++ "_" ++ show name) targs
      (A.VarAccess other) -> do
        -- send message
        -- fixme: how do we send arguments?
        other_ty <- asks (fromJust . (Ctx.type_of $ other))
        return $ Statement $ Call "pony_send" [Var $ show other, Var ("MSG_" ++ show other_ty ++ "_" ++ show name)]
      no_var_access -> error "calls are only implemented on variables for now"
  translate other = return $ Embed $ "/* missing: " ++ show other ++ "*/"

dataStructType :: A.Type -> CType
dataStructType ty = case show ty of
  "Object" -> embedCType "void*"
  other_ty -> embedCType $ if isLower $ head other_ty
                           then other_ty
                           else other_ty++"_data*"

pony_actor_t_Type :: String -> CType
pony_actor_t_Type ty = case ty of
  other_ty -> embedCType $ if isLower $ head other_ty
                           then other_ty
                           else other_ty++"_actor_t*"

--instance Translatable [FieldDecl] CCode where
--  translate fs = (StructDecl $ map VarDecl $ zip (map ftype fs) (map fname fs))

instance Translatable A.MethodDecl (Reader Ctx.Context CCode) where
  translate mdecl = do
    this_ty <- asks (A.cname . fromJust . Ctx.the_class)
    cdecl <- asks (fromJust . Ctx.the_class)
    tmbody <- local (Ctx.with_method mdecl) $ translate (A.mbody mdecl)
    return $ 
      (Function (dataStructType (A.rtype mdecl)) ((show (A.cname cdecl)) ++ "_" ++ (show $ A.mname mdecl))
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
    return $ C $
      (comment_section $ "Implementation of class " ++ (show $ A.cname cdecl)) :
      data_struct :
      tracefun_decl :
      message_type_decl :
      pony_actor_t_impl :
      method_impls ++
      [dispatchfun_decl]
    where
      data_struct = Statement $
                    TypeDef data_struct_name
                    (StructDecl data_struct_name $ map CVarSpec $
                     zip (map (dataStructType . A.ftype) (A.fields cdecl)) (map (show . A.fname) (A.fields cdecl)))
      mthd_dispatch_clause cdecl mdecl =
        (Var ("MSG_"++(show $ A.cname cdecl)++"_"++(show $ A.mname mdecl)),
         Statement (Call ((show $ A.cname cdecl)++"_"++(show $ A.mname mdecl)) [Var "p"] 
         -- fixme what about arguments?
          ))
        
      dispatchfun_decl = (Function (dataStructType (A.Type "static void")) (show (A.cname cdecl) ++ "_dispatch")
                          (map CVarSpec [(embedCType "pony_actor_t*", "this"),
                                         (embedCType "void*", "p"),
                                         (embedCType "uint64_t", "id"),
                                         (embedCType "int", "argc"),
                                         (embedCType "pony_arg_t*", "argv")])
                          [--Decl (CVarSpec (embedCType "main_t*", "d")),
                           --Assign () (Var "p"),
                           Switch "id"
                           ((Var "PONY_MAIN",
                             C $ map Statement [--Decl $ CVarSpec (embedCType "Main_data*", "d"),
                                                Assign (Var "p") (Call "pony_alloc" [(Call "sizeof" [Var "Main_data"])]),
                                                Call "pony_set" [Var "p"],
                                                Call "Main_main" [Var "p"]]) :
                            map (mthd_dispatch_clause cdecl) (A.methods cdecl))
                           (Embed "printf(\"error, got invalid id: %llu\",id);")])


  -- (Embed $ "void " ++ (A.cname cdecl) ++ "_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {}")
                         

      tracefun_decl = (Function
                       (embedCType "static void")
                       tracefun_name
                       [CVarSpec (embedCType "void*", "p")]
                       [])

-- * implement the message_type functions
-- * implement the message types:
--      static pony_msg_t m_Other_init = {0, {{NULL, 0, PONY_PRIMITIVE}}};
--      static pony_msg_t m_Other_work = {2, {{NULL, 0, PONY_PRIMITIVE}}};

      message_type_decl = Function (embedCType "static pony_msg_t*")
                          ((show $ A.cname cdecl) ++ "_message_type")
                          [CVarSpec (embedCType "uint64_t", "id")]
                          [(Switch "id" (map (\m -> message_type_clause (A.cname cdecl) (A.mname m)) (A.methods cdecl)) (C [])),
                           Statement (Embed "return NULL")]
        where
          message_type_clause :: A.Type -> A.Name -> (CCode, CCode)
          message_type_clause cname mname =
            (Var ("MSG_"++(show cname)++"_"++(show mname)),
             Embed $ "return &" ++ (show cname) ++ "_" ++ (show mname) ++ ";")


--  switch(id)
--  {
--    case MSG_Other_init: return &m_Other_init;
--    case MSG_Other_work: return &m_Other_work;
--  }


      pony_actor_t_impl = Statement (Assign (Embed $ "static pony_actor_type_t " ++ pony_actor_t_name)
                                              (Record [Var ("ID_"++(show $ A.cname cdecl)),
                                                       tracefun_rec,
                                                       (Embed message_type_fn_name),
                                                       (Embed dispatch_fn_name)]))

      tracefun_rec = Record [Var tracefun_name,
                             Call "sizeof" [Var data_struct_name],
                             Var "PONY_ACTOR"]

      pony_actor_t_name    = (show $ A.cname cdecl) ++ "_actor_t"
      message_type_fn_name = (show $ A.cname cdecl) ++ "_message_type"
      tracefun_name        = (show $ A.cname cdecl) ++ "_trace"
      data_struct_name     = (show $ A.cname cdecl) ++ "_data" -- FIXME code duplication with CCode.hs
      dispatch_fn_name     = (show $ A.cname cdecl) ++ "_dispatch"

      mparam_to_cvardecl (A.Param (ty, na)) = CVarSpec (dataStructType ty, show na)

comment_section :: String -> CCode
comment_section s = C $ [Embed $ take (5 + length s) $ repeat '/',
                         Embed $ "// " ++ s]

main_dispatch_clause = (Var "PONY_MAIN",
                        C $ map Statement [Decl $ CVarSpec (embedCType "Main_data*", "d"),
                                      Assign (Var "d") (Call "pony_alloc" [(Call "sizeof" [Var "Main_data"])]),
                                      Call "pony_set" [Var "d"],
                                      Call "Main_main" [Var "d"]])

instance FwdDeclaration A.ClassDecl CCode where
  fwd_decls cdecl = C $ (comment_section "Forward declarations") :
                    map Embed ["static pony_actor_type_t " ++ (show $ A.cname cdecl) ++ "_actor_t;",
                               "struct " ++ (show $ A.cname cdecl) ++ "_data;",
                               "//todo fwd decl Main_message_type!!",
                               "static void " ++ (show $ A.cname cdecl) ++ "_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*);"]

instance FwdDeclaration A.Program CCode where
  fwd_decls (A.Program cs) = C $ [msg_enum (A.Program cs),
                                  class_ids_enum (A.Program cs)]
    where
      msg_enum :: A.Program -> CCode
      msg_enum (A.Program cs) =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (A.methods cdecl)) cs
          lines = map (\ (cname, mdecl) -> "MSG_" ++ show cname ++ "_" ++ (show $ A.mname mdecl)) meta
        in
         Statement $ Enum lines

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
      [Embed "return pony_start(argc, argv, pony_create(&Main_actor_t));"])]
    where
      translate_class_here :: A.ClassDecl -> CCode
      translate_class_here cdecl = runReader (translate cdecl) $ Ctx.mk (A.Program cs)
