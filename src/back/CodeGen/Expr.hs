{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.Expr where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST as A

import Control.Monad.Reader
import Data.Maybe

instance Translatable A.Op (CCode Id) where
  translate op = Var $ case op of
    A.LT -> "<"
    A.GT -> ">"
    A.EQ -> "=="
    A.NEQ -> "!="
    A.PLUS -> "+"
    A.MINUS -> "-"

instance Translatable A.Lvar (Reader Ctx.Context (CCode Lval)) where
  translate (A.LVar name) = return $ Embed $ show name
  translate (A.LField ex name) = do
    tex <- translate ex
    return $ (Deref tex) `Dot` (Var $ show name)

instance Translatable A.Expr (Reader Ctx.Context (CCode Expr)) where
  translate (A.Skip) = return $ Embed "/* skip */"
  translate (A.Null) = return $ Embed "0"
  translate (A.Binop op e1 e2) = do
    te1 <- translate e1
    te2 <- translate e2
    return $ BinOp (translate op) te1 te2
  translate (A.Print (A.StringLiteral s)) =
    return $ Embed $ "printf(\"%s\\n\", \"" ++ s ++ "\" );"
  translate (A.Print (A.FieldAccess (A.VarAccess var) name)) =
    return $ Embed $ "printf(\"%i\\n\", " ++ show var ++ "->" ++ show name ++ " );"
  translate (A.Seq es) = do
    tes <- mapM translate es
    return $ StoopidSeq tes
  translate (A.Assign lvar expr) = do
    texpr <- translate expr
    tlvar <- translate lvar
    return $ Assign tlvar texpr
  translate (A.VarAccess name) =
    return $ Embed $ show name
  translate (A.FieldAccess exp name) = do
    texp <- translate exp
    return $ AsExpr $ Deref texp `Dot` (Var $ show name)
  translate (A.IntLiteral i) =
    return $ Embed $ show i
  translate (A.StringLiteral s) =
    return $ Embed $ show s
  translate (A.Let name ty e1 e2) = do
    te1 <- translate e1
    te2 <- local (Ctx.with_local (A.Param (ty, name))) $ translate e2
    return (Braced . StoopidSeq $
                       [Assign (Decl ((embedCType "pony_actor_t*", Var $ show name))) te1,
                        te2])
  translate (A.New ty) = return $ Embed $ "create_and_send(&"++show ty++"_actor, MSG_alloc)"
  translate (A.Call { A.target=expr, A.tmname=name, A.args=args } ) =
    case expr of
      (A.VarAccess (A.Name "this")) -> do
        -- call synchronously
        cname <- asks (A.cname . fromJust . Ctx.the_class)
        targs <- mapM translate args
        return $ Call (AsExpr . AsLval $ (method_impl_name cname name)) targs
      (A.VarAccess other) -> do
        -- send message
        -- fixme: how do we send arguments?
        other_ty <- asks (fromJust . (Ctx.type_of $ other))
        return $
              Call (AsExpr . AsLval . Var $ "pony_send")
                       [AsExpr . AsLval . Var $ show other,
                        AsExpr . AsLval $ (method_msg_name other_ty name)]
      no_var_access -> error "calls are only implemented on variables for now"
  translate other = return $ Embed $ "/* missing: " ++ show other ++ "*/"
