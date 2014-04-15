{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen (code_from_AST) where
import Data.List
import CCode

import AST

code_from_AST :: Program -> CCode
code_from_AST = translate

class Translatable a b where
  translate :: a -> b

instance Translatable Expr CCode where
  translate (Skip) = (Embed "undefined")
  translate (Call cr) = (Embed "undefined")
  translate (Let name e_init e_body) = (Embed "undefined")
  translate (Seq es) = (Embed "undefined")
  translate (IfThenElse e1 e2 e3) = (Embed "undefined")
  translate (Get e) = (Embed "undefined")
  translate (FieldAccess e name) = (Embed "undefined")
  translate (Assign lvar e) = (Embed "undefined")
  translate (VarAccess name) = (Embed "undefined")
  translate (Null) = (Embed "undefined")
  translate (New name) = (Embed "undefined")
  translate (Print e) = (Embed "undefined")
  translate (StringLiteral s) = (Embed "undefined")
  translate (IntLiteral i) = (Embed $ show i)
  translate (Binop op e1 e2) = (Embed "undefined")

instance Translatable ParamDecl CCode where
  translate _ = (Embed "undefined")

instance Translatable MethodDecl CCode where
  translate _ = (Embed "undefined")

instance Translatable FieldDecl CCode where
  translate _ = (Embed "undefined")

instance Translatable ClassDecl CCode where
  translate cdecl = C $
                    (Embed "//whatever a class does...") : 
                    (map translate (fields cdecl)) ++
                    (map translate (methods cdecl))

instance Translatable Program CCode where
  translate (Program cs) = C $
                           (HashDefine "__STDC_FORMAT_MACROS") :
                           (Includes ["pony/pony.h",
                                      "stdlib.h",
                                      "stdio.h",
                                      "string.h",
                                      "inttypes.h",
                                      "assert.h"]) :
                           map translate cs



