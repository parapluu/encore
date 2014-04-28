{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module CodeGen.Expr where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST as A

import Control.Monad.Reader
import Data.Maybe

instance Translatable A.Op (CCode Name) where
  translate op = Nam $ case op of
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
    return $ (Deref (tex ::CCode Expr)) `Dot` (Nam $ show name)

type_to_printf_fstr :: A.Type -> String
type_to_printf_fstr (A.Type "int") = "%i"
type_to_printf_fstr (A.Type "string") = "%s"
type_to_printf_fstr other = error $ "Expr.hs: type_to_printf_fstr not defined for " ++ show other

instance Translatable A.Expr (Reader Ctx.Context (CCode Expr)) where
  translate (A.Skip) = return $ Embed "/* skip */"
  translate (A.Null) = return $ Embed "NULL"
  translate (A.Binop op e1 e2) = do
    te1 <- translate e1
    te2 <- translate e2
    return $ BinOp (translate op) te1 te2
  translate (A.Print ty e) =
      do
        te <- translate e
        return $ Call (Nam "printf") [Embed $ "\""++ type_to_printf_fstr ty++"\\n\"",
                                      te :: CCode Expr]
--        return $ Embed $ "printf(\""++ type_to_printf_fstr ty++"\\n\", " ++ (te::CCode Expr) ++ ")"
--  translate (A.Print (A.StringLiteral s)) =
--    return $ Embed $ "printf(\"%s\\n\", \"" ++ s ++ "\" )"
--  translate (A.Print ty (A.FieldAccess (A.VarAccess var) name)) =
--    return $ Embed $ "printf(\"%i\\n\", " ++ show var ++ "->" ++ show name ++ " )"
  translate (A.Seq es) = do
    tes <- mapM translate es
    return $ StoopidSeq tes
  translate (A.Assign lvar expr) = do
    texpr <- translate expr
    tlvar <- translate lvar
    return $ Assign (tlvar :: CCode Lval) texpr
  translate (A.VarAccess name) =
    return $ Embed $ show name
  translate (A.FieldAccess exp name) = do
    texp <- translate exp
    return $ AsExpr $ Deref (texp :: CCode Expr) `Dot` (Nam $ show name)
  translate (A.IntLiteral i) =
    return $ Embed $ show i
  translate (A.StringLiteral s) =
    return $ Embed $ show s
  translate (A.Let name ty e1 e2) = do
    te1 <- translate e1
    te2 <- local (Ctx.with_local (A.Param (ty, name))) $ translate e2
    return (Braced . StoopidSeq $
                       [Assign 
                        (Decl ((Ptr . Typ $ "pony_actor_t", Var $ show name))) te1,
                        te2])
  translate (A.New ty) = return $ Call (Nam "create_and_send") [Amp $ actor_rec_name ty,
                                                                AsExpr . AsLval . Nam $ "MSG_alloc"]
  translate (A.Call { A.target=expr, A.tmname=name, A.args=args }) =
    case expr of
      (A.VarAccess (A.Name "this")) -> do
        -- call synchronously
        cname <- asks (A.cname . fromJust . Ctx.the_class)
        targs <- mapM translate args
        return $ Call (method_impl_name cname name) (targs :: [CCode Expr])
      (A.VarAccess other) -> do
        -- send message
        -- fixme: how do we send arguments?
        targs <- mapM translate args
        other_ty <- asks (fromJust . (Ctx.type_of $ other))
        return $ args_to_call other other_ty name (targs :: [CCode Expr])
--              Call (Nam "pony_send")
--                       [Var $ show other,
--                        AsLval $ method_msg_name other_ty name]
      no_var_access -> error "calls are only implemented on variables for now"
          where

  translate other = return $ Embed $ "/* missing: " ++ show other ++ "*/"


args_to_call other other_ty name [] =
    Call (Nam "pony_send") [AsExpr . Var $ show other,
                            AsExpr . AsLval $ method_msg_name other_ty name]
args_to_call other other_ty name [arg] =
    Call (Nam "pony_sendi") [AsExpr . Var $ show other,
                             AsExpr . AsLval $ method_msg_name other_ty name,
                             arg]
