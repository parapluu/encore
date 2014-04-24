{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen (code_from_AST) where
import Data.List
import Data.Char
import Data.Maybe
import CCode
import Control.Monad.Reader

import qualified AST as A

code_from_AST :: A.Program -> CCode
code_from_AST = translate

class Translatable a b where
  translate :: a -> b

data Context = Context { ctxt_prog :: A.Program,
                         ctxt_class :: Maybe A.ClassDecl}

ctxt_other_classes :: Context -> [A.ClassDecl]
ctxt_other_classes ctxt =
  let
    (A.Program the_classes) = (ctxt_prog ctxt)
    the_class = (ctxt_class ctxt)
  in
   map fromJust $ filter (/= the_class) (map Just the_classes)

instance Translatable A.Op CCode where
  translate A.LT = Embed "<"
  translate A.GT = Embed ">"
  translate A.EQ = Embed "=="
  translate A.NEQ = Embed "!="
  translate A.PLUS = Embed "+"
  translate A.MINUS = Embed "-"

instance Translatable A.Lvar (Reader Context CCode) where
  translate (A.LVar name) = return $ Embed name
  translate (A.LField ex name) = do
    tex <- translate ex
    return $ (Deref tex) `Dot` name

instance Translatable A.Expr (Reader Context CCode) where
  translate (A.Skip)                      = return $ Embed "/* skip */"
  translate (A.Null)                      = return $ Embed "0"
  translate (A.Binop op e1 e2)            = do
    te1 <- translate e1
    te2 <- translate e2
    return $ C [(Embed "("),
                te1,
                translate op,
                te2,
                (Embed ")")]
  translate (A.Print (A.StringLiteral s)) = return $ Embed $ "printf(\"%s\\n\", \"" ++ s ++ "\" );"
  translate (A.Print (A.FieldAccess (A.VarAccess var) name)) = return $ Embed $ "printf(\"%i\\n\", " ++ var ++ "->" ++ name ++ " );"
  translate (A.Seq es)                    = do
    tes <- mapM translate es
    return $ C (map Statement tes)
  translate (A.Assign lvar expr)          = do
    texpr <- translate expr
    tlvar <- translate lvar
    return $ Assign tlvar texpr
  translate (A.VarAccess name)            = return $ Embed name
  translate (A.FieldAccess exp name)      = do
    texp <- translate exp
    return $ Deref texp `Dot` name
  translate (A.IntLiteral i)              = return $ Embed $ show i
  translate (A.StringLiteral s)           = return $ Embed $ show s
  translate (A.Let name ty e1 e2)         = do
    te1 <- translate e1
    te2 <- translate e2
    return (BracedBlock $ C $
            map Statement
            ((Decl (CVarSpec (embedCType "pony_actor_t*", name))) :
             [Assign (Var name) te1,
              te2
             ]))
  translate (A.New ty) = return $ Embed $ "pony_create(&"++ty++"_actor_t)" -- continue here
--  translate (A.Call { target=expr, tmname=name, args=as } ) = case expr of
--    (VarAccess "this") -> Call 
  translate other = return $ Embed $ "/* missing: " ++ show other ++ "*/"



dataStructType :: String -> CType
dataStructType ty = case ty of
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

instance Translatable A.ClassDecl (Reader Context CCode) where
  translate cdecl = return $ C $
                    (comment_section $ "Implementation of class " ++ (A.cname cdecl)) :
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
                     zip (map (dataStructType . A.ftype) (A.fields cdecl)) (map A.fname (A.fields cdecl)))


      dispatchfun_decl = (Function (dataStructType "static void") (A.cname cdecl ++ "_dispatch")
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
                           (Embed "printf(\"error, got invalid id: %llu\",id);")])


  -- (Embed $ "void " ++ (A.cname cdecl) ++ "_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {}")
                         

      tracefun_decl = (Function
                       (embedCType "static void")
                       tracefun_name
                       [CVarSpec (embedCType "void*", "p")]
                       [])

      message_type_decl = (Function (embedCType "static pony_msg_t*")
                           (A.cname cdecl ++ "_message_type")
                           [CVarSpec (embedCType "uint64_t", "id")]
                           [Statement (Embed "return NULL")])

      pony_actor_t_impl = Statement (Assign (Embed $ "static pony_actor_type_t " ++ pony_actor_t_name)
                                              (Record [(Embed "1"), --FIXME: this can't always be '1', needs to be different per actor.
                                                       tracefun_rec,
                                                       (Embed message_type_fn_name),
                                                       (Embed dispatch_fn_name)]))

      method_impls = (map (method_impl (A.cname cdecl)) (A.methods cdecl))

      tracefun_rec = Record [Var tracefun_name,
                             Call "sizeof" [Var data_struct_name],
                             Var "PONY_ACTOR"]

      pony_actor_t_name    = (A.cname cdecl) ++ "_actor_t"
      message_type_fn_name = (A.cname cdecl) ++ "_message_type"
      tracefun_name        = (A.cname cdecl) ++ "_trace"
      data_struct_name     = (A.cname cdecl) ++ "_data" -- FIXME code duplication with CCode.hs
      dispatch_fn_name     = (A.cname cdecl) ++ "_dispatch"

      method_impl :: A.Type -> A.MethodDecl -> CCode
      method_impl this_ty mdecl = (Function (dataStructType (A.rtype mdecl)) (A.cname cdecl ++ "_" ++ A.mname mdecl)
                                   (CVarSpec (dataStructType this_ty, "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
                                   [Statement $ C[]
                                    --runReader (translate (A.mbody mdecl)) (ask { ctxt_class = Just cdecl })
                                   ])
      mparam_to_cvardecl (A.Param (ty, na)) = CVarSpec (dataStructType ty, na)

comment_section :: String -> CCode
comment_section s = C $ [Embed $ take (5 + length s) $ repeat '/',
                         Embed $ "// " ++ s]

main_dispatch_clause = (Var "PONY_MAIN",
                        C $ map Statement [Decl $ CVarSpec (embedCType "Main_data*", "d"),
                                      Assign (Var "d") (Call "pony_alloc" [(Call "sizeof" [Var "Main_data"])]),
                                      Call "pony_set" [Var "d"],
                                      Call "Main_main" [Var "d"]])

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
    (fwd_decls cs) ++
    (map (translate_class) cs) ++
    [(Function
      (embedCType "static void") "dispatch"
      [CVarSpec (embedCType "pony_actor_t*", "this"),
                CVarSpec (embedCType "void*", "p"),
                CVarSpec (embedCType "uint64_t", "id"),
                CVarSpec (embedCType "int", "argc"),
                CVarSpec (embedCType "pony_arg_t*", "argv")]
      [Switch "id" [(Var "PONY_MAIN",
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
      fwd_decls :: [A.ClassDecl] -> [CCode]
      fwd_decls cdecls = comment_section "Forward declarations" : map fwd_decl cdecls
        where
          fwd_decl cdecl = C $ [Embed $ "struct " ++ (A.cname cdecl) ++ "_data;",
                                Embed $ "//todo fwd decl Main_message_type!!",
                                Embed $ "static void " ++ (A.cname cdecl) ++ "_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*);"]
      the_context = Context { ctxt_prog = (A.Program cs),
                              ctxt_class = Nothing }
      translate_class :: A.ClassDecl -> CCode
      translate_class cdecl = runReader (translate cdecl) the_context
