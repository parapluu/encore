{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen (code_from_AST) where
import Data.List
import Data.Char
import CCode
import qualified AST as A

code_from_AST :: A.Program -> CCode
code_from_AST = translate

class Translatable a b where
  translate :: a -> b

instance Translatable A.Expr CCode where  
  translate (A.Skip)                      = Embed "/* skip */"
  translate (A.Call target name args)     = Embed $ "/* missing: " ++ (show (A.Call target name args) ) ++ "*/"
  translate (A.Let name e_init e_body)    = Embed $ "/* missing: " ++ (show (A.Let name e_init e_body) ) ++ "*/"
  translate (A.Seq es)                    = Embed $ "/* missing: " ++ (show (A.Seq es) ) ++ "*/"
  translate (A.IfThenElse e1 e2 e3)       = Embed $ "/* missing: " ++ (show (A.IfThenElse e1 e2 e3) ) ++ "*/"
  translate (A.Get e)                     = Embed $ "/* missing: " ++ (show (A.Get e) ) ++ "*/"
  translate (A.FieldAccess e name)        = Embed $ "/* missing: " ++ (show (A.FieldAccess e name) ) ++ "*/"
  translate (A.Assign lvar e)             = Embed $ "/* missing: " ++ (show (A.Assign lvar e) ) ++ "*/"
  translate (A.VarAccess name)            = Embed $ "/* missing: " ++ (show (A.VarAccess name) ) ++ "*/"
  translate (A.Null)                      = Embed "0"
  translate (A.New name)                  = Embed $ "/* missing: " ++ (show (A.New name) ) ++ "*/"
  translate (A.Print (A.StringLiteral s)) = Embed $ "printf(\"%s\"," ++ s ++ " );"
  translate (A.StringLiteral s)           = Embed $ "/* missing: " ++ (show (A.StringLiteral s) ) ++ "*/"
  translate (A.IntLiteral i)              = Embed $ "/* missing: " ++ (show (A.IntLiteral i) ) ++ "*/"
  translate (A.Binop op e1 e2)            = C [(Embed "("),
                                             translate e1,
                                             (Embed $ show op),
                                             translate e2,
                                             (Embed ")")]
instance Translatable A.ParamDecl CCode where
  translate _ = (Embed "//whatever a ParamDecl does")

--instance Translatable [FieldDecl] CCode where
--  translate fs = (StructDecl $ map VarDecl $ zip (map ftype fs) (map fname fs))

instance Translatable A.ClassDecl CCode where
  translate cdecl = C $
                    (Embed "//////////////////////////////////// ") :
                    (Embed $ "//Implementation of class " ++ (A.cname cdecl)) :
                    data_struct :
                    pony_actor_t_impl :
                    method_impls
    where
      data_struct =
        (StructDecl data_struct_name $ map CVarDecl $
         zip (map (toCType . A.ftype) (A.fields cdecl)) (map A.fname (A.fields cdecl)))

      pony_actor_t_impl = SEMI (Static (Assign (Embed pony_actor_t_name) (Record [(Embed "1"), --FIXME: this can't always be '1', needs to be different per actor.
                                                                                  tracefun_rec,
                                                                                  (Embed message_type_fn_name),
                                                                                  (Embed dispatch_fn_name)])))

      method_impls = (map (method_impl (A.cname cdecl)) (A.methods cdecl))

      tracefun_rec = Record [Embed tracefun_name,
                             Embed $ "sizeof(" ++ data_struct_name ++ ")",
                             Embed "PONY_ACTOR"]

      pony_actor_t_name    = (A.cname cdecl) ++ "_actor_t"
      message_type_fn_name = (A.cname cdecl) ++ "_message_type"
      tracefun_name        = (A.cname cdecl) ++ "_trace"
      data_struct_name     = (A.cname cdecl) ++ "_data" -- FIXME code duplication with CCode.hs
      dispatch_fn_name     = (A.cname cdecl) ++ "_dispatch"
      
      method_impl :: A.Type -> A.MethodDecl -> CCode
      method_impl this_ty mdecl = (Function (toCType $ A.rtype mdecl) (A.mname mdecl)
                                   (CVarDecl (toCType this_ty, "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
                                   [translate (A.mbody mdecl)])
      mparam_to_cvardecl (A.Param (ty, na)) = CVarDecl (toCType ty, na)

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
    map translate cs ++
    [(Function
      (embedCType "int") "main"
     [CVarDecl (embedCType "int","argc"), CVarDecl (embedCType "char**","argv")]
     [Embed "return pony_start(argc, argv, pony_create(&Main_actor_t));"])]



