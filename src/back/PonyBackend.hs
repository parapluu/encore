{-# LANGUAGE MultiParamTypeClasses #-}

module PonyBackend (code_from_AST) where
import Data.List
import CCode

import AST

code_from_AST :: Program -> CCode
code_from_AST (Program cs) = translate (Program cs)

class Translatable a b where
  translate :: a -> b

instance Translatable Expr CCode where
  translate (Skip) = undefined
  translate (Call cr) = undefined
  translate (Let name e_init e_body) = undefined
  translate (Seq es) = undefined
  translate (IfThenElse e1 e2 e3) = undefined
  translate (Get e) = undefined
  translate (FieldAccess e name) = undefined
  translate (Assign lvar e) = undefined
  translate (VarAccess name) = undefined
  translate (Null) = undefined
  translate (New name) = undefined
  translate (Print e) = undefined
  translate (StringLiteral s) = undefined
  translate (IntLiteral i) = undefined
  translate (Binop op e1 e2) = undefined

instance Translatable ParamDecl CCode where
  translate = undefined

instance Translatable MethodDecl CCode where
  translate = undefined

instance Translatable FieldDecl CCode where
  translate = undefined

instance Translatable ClassDecl CCode where
  translate cdecl = C $
                    (Embed "//whatever a class does...") : 
                    (map translate (fields cdecl)) ++
                    (map translate (methods cdecl))

instance Translatable Program CCode where
  translate (Program cs) = C $ map translate cs


