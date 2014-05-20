{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,GADTs #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type
import qualified CodeGen.Context as Ctx
import Data.List

import CCode.Main

import qualified EAST.EAST as A
import qualified Identifiers as ID

import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

instance Translatable ID.Op (CCode Name) where
  translate op = Nam $ case op of
    ID.LT -> "<"
    ID.GT -> ">"
    ID.EQ -> "=="
    ID.NEQ -> "!="
    ID.PLUS -> "+"
    ID.MINUS -> "-"
    ID.TIMES -> "*"
    ID.DIV -> "/"

instance Translatable A.LVal (State Ctx.Context (CCode Lval)) where
  translate (A.LVal ty name) = return $ Embed $ show name
  translate (A.LField ty ex name) = do
      tex <- translate ex
      return $ (Deref (tex ::CCode Expr)) `Dot` (Nam $ show name)

type_to_printf_fstr :: ID.Type -> String
type_to_printf_fstr (ID.Type "int") = "%i"
type_to_printf_fstr (ID.Type "string") = "%s"
type_to_printf_fstr other = case (translate other :: CCode Ty) of
                              Ptr something -> "%p"
                              _ -> "Expr.hs: type_to_printf_fstr not defined for " ++ show other

instance Translatable A.Expr (State Ctx.Context (CCode Expr)) where
  translate (A.Skip) = return $ Embed "/* skip */"
  translate (A.Null) = return $ Embed "NULL"
  translate (A.Binop {A.op = op, A.loper = e1, A.roper = e2}) = do
    te1 <- translate e1
    te2 <- translate e2
    return $ BinOp (translate op) te1 te2
  translate (A.Print _ e) =
      do
        te <- translate e
        return $ Call (Nam "printf") [Embed $ "\""++ type_to_printf_fstr (A.getType e)++"\\n\"",
                                      te :: CCode Expr]
  translate (A.Seq {A.seq = es}) = do
    tes <- mapM translate es
    return $ StoopidSeq tes
  translate (A.Assign {A.lhs = lvar, A.rhs = expr}) = do
    texpr <- translate expr
    tlvar <- translate lvar
    return $ Assign (tlvar :: CCode Lval) texpr
  translate (A.VarAccess {A.id = name}) =
    return $ Embed $ show name
  translate (A.FieldAccess {A.path = exp, A.field = name}) = do
    texp <- translate exp
    return $ AsExpr $ Deref (texp :: CCode Expr) `Dot` (Nam $ show name)
  translate (A.IntLiteral i) =
    return $ Embed $ show i
  translate (A.StringLiteral s) =
    return $ Embed $ show s
  translate (A.Let {A.id = name, A.ty = ty, A.val = e1, A.body = e2}) =
-- this uses an obscure feature of C: compound statements can form expressions!
-- http://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html
                   do
                     te1 <- translate e1
                     s <- get
                     put (Ctx.with_local (ID.Param (name, ty)) s)
                     te2 <- translate e2
                     put s
                     return (Parens . Braced . StoopidSeq $
                             [Assign 
                              (Decl ((Ptr . Typ $ "pony_actor_t", Var $ show name))) te1,
                                                                                     te2])
  translate (A.New ty) = return $ Call (Nam "create_and_send") [Amp $ actor_rec_name ty,
                                                                AsExpr . AsLval . Nam $ "MSG_alloc"]
  translate (A.Call { A.target=expr, A.tmname=name, A.args=args }) =
      do texpr <- translate expr
         targs <- mapM translate args
         let argtys = (map A.getType args)
         let targtys = map (translate . A.getType) args :: [CCode Ty]
         the_arg_name <- Ctx.gen_sym
         let the_arg_decl = Embed $ ("pony_arg_t " ++
                                     the_arg_name ++ "[" ++ show (length args) ++ "] = {" ++
                                             (concat $
                                              intersperse ", " $
                                              map (\(arg,ty) ->
                                                       "{"++pony_arg_t_tag ty ++ "=" ++ show arg ++ "}") $
                                                      (zip (targs :: [CCode Expr]) targtys)) ++
                                     "}")
         return $ StoopidSeq $
                    [the_arg_decl,
                     Call
                     (Nam "pony_sendv")
                     ([texpr :: CCode Expr,
                                AsExpr . AsLval $ method_msg_name (A.getType expr) name,
                                Embed . show . length $ args] ++
                      [Embed the_arg_name])]
             where
               pony_arg_t_tag :: CCode Ty -> String
               pony_arg_t_tag (Ptr _) = ".p"
               pony_arg_t_tag (Typ "int") = ".i"
               pony_arg_t_tag (Typ "double") = ".d"
               pony_arg_t_tag other =
                   error $ "Expr.hs: no pony_arg_t_tag for " ++ show other
  translate other = error $ "Expr.hs: can't translate: `" ++ show other ++ "`"
