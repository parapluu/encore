{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen (code_from_AST) where
import Data.List
import CCode

import AST

code_from_AST :: Program -> CCode
code_from_AST = translate

class Translatable a b where
  translate :: a -> b

instance Translatable Expr CCode where
  translate (Skip) = Embed ""
  translate (Call target name args) = Embed "//whatever a Call does"
  translate (Let name e_init e_body) = Embed "//whatever a Let does"
  translate (Seq es) = Embed "//whatever a Seq does"
  translate (IfThenElse e1 e2 e3) = Embed "//whatever a IfThenElse does"
  translate (Get e) = Embed "//whatever a Get does"
  translate (FieldAccess e name) = Embed "//whatever a FieldAccess does"
  translate (Assign lvar e) = Embed "//whatever a Assign does"
  translate (VarAccess name) = Embed "//whatever a VarAccess does"
  translate (Null) = Embed "//whatever a Null does"
  translate (New name) = Embed "//whatever a New does"
  translate (Print (StringLiteral s)) = Embed $ "printf(\"%s\"," ++ s ++ " );"
  translate (Print e) = Embed "//whatever a Print does"
  translate (StringLiteral s) = Embed "//whatever a StringLiteral does"
  translate (IntLiteral i) = Embed $ show i
  translate (Binop op e1 e2) = Embed "//whatever a Binop does"

instance Translatable ParamDecl CCode where
  translate _ = (Embed "//whatever a ParamDecl does")

--instance Translatable [FieldDecl] CCode where
--  translate fs = (Record $ map VarDecl $ zip (map ftype fs) (map fname fs))

instance Translatable ClassDecl CCode where
  translate cdecl = C $
                    (Embed "//whatever a class does...") :
                    struct :
                    method_impls
    where
      struct = TypeDef (struct_name cdecl) (Record (struct_name cdecl) $ map CVarDecl $
                zip (map (toCType.ftype) (fields cdecl)) (map fname (fields cdecl)))
      method_impls = map (method_impl (cname cdecl)) (methods cdecl)
               
      struct_name :: ClassDecl -> Id
      struct_name = (++ "_dataaaa") . cname
      
      method_impl :: Type -> MethodDecl -> CCode
      method_impl this_ty mdecl = Function (toCType $ rtype mdecl) (mname mdecl)
                                   (CVarDecl (toCType this_ty, "this"):(map mparam_to_cvardecl $ mparams mdecl))
                                   [translate (mbody mdecl)]
      mparam_to_cvardecl (Param (ty, na)) = CVarDecl (toCType ty, na)

instance Translatable Program CCode where
  translate (Program cs) =
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
     [Embed "return pony_start(argc, argv, pony_create(&type));"])]



