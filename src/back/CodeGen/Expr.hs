{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,GADTs #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified EAST.EAST as A
import qualified Identifiers as ID

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

instance Translatable A.LVal (Reader Ctx.Context (CCode Lval)) where
  translate (A.LVal ty name) = return $ Embed $ show name
  translate (A.LField ty ex name) = do
    tex <- translate ex
    return $ (Deref (tex ::CCode Expr)) `Dot` (Nam $ show name)

type_to_printf_fstr :: ID.Type -> String
type_to_printf_fstr (ID.Type "int") = "%i"
type_to_printf_fstr (ID.Type "string") = "%s"
type_to_printf_fstr other = case (translate other :: CCode Ty) of
                              Ptr something -> "%p"
                              _ -> error $ "Expr.hs: type_to_printf_fstr not defined for " ++ show other

instance Translatable A.Expr (Reader Ctx.Context (CCode Expr)) where
  translate (A.Skip) = return $ Embed "/* skip */"
  translate (A.Null) = return $ Embed "NULL"
  translate (A.Binop {A.op = op, A.loper = e1, A.roper = e2}) = do
    te1 <- translate e1
    te2 <- translate e2
    return $ BinOp (translate op) te1 te2
  translate (A.Print ty e) =
      do
        te <- translate e
        return $ Call (Nam "printf") [Embed $ "\""++ type_to_printf_fstr ty++"\\n\"",
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
  translate (A.Let {A.id = name, A.ty = ty, A.val = e1, A.body = e2}) = do
    te1 <- translate e1
    te2 <- local (Ctx.with_local (ID.Param (name, ty))) $ translate e2
    return (Braced . StoopidSeq $
                       [Assign 
                        (Decl ((Ptr . Typ $ "pony_actor_t", Var $ show name))) te1,
                        te2])
  translate (A.New ty) = return $ Call (Nam "create_and_send") [Amp $ actor_rec_name ty,
                                                                AsExpr . AsLval . Nam $ "MSG_alloc"]
  translate (A.Call { A.target=expr, A.tmname=name, A.args=args }) =
    case expr of
      (A.VarAccess {A.id = ID.Name "this"}) -> do
        -- call synchronously
        cname <- asks (A.cname . fromJust . Ctx.the_class)
        targs <- mapM translate args
        return $ Call (method_impl_name cname name) (targs :: [CCode Expr])
      (A.VarAccess {A.id = other}) -> do
        -- send message
        -- fixme: how do we send arguments?
        targs <- mapM translate args
        other_ty <- asks (fromJust . (Ctx.type_of $ other))
        return $ args_to_call other other_ty name (targs :: [CCode Expr])
--              Call (Nam "pony_send")
--                       [Var $ show other,
--                        AsLval $ method_msg_name other_ty name]
      no_var_access -> error "Expr.hs: calls are only implemented on variables for now."

  translate other = return $ Embed $ "/* missing: " ++ show other ++ "*/"

--args_to_call :: ID.Name -> ID.Type -> ID.Name -> [CCode Expr]
args_to_call other other_ty name [] =
    Call (Nam "pony_send") [AsExpr . Var $ show other,
                            AsExpr . AsLval $ method_msg_name other_ty name]
args_to_call other other_ty name [arg] =
    case (translate other_ty :: CCode Ty) of
      (Typ "int") ->
          Call (Nam "pony_sendi") [AsExpr . Var $ show other,
                                   AsExpr . AsLval $ method_msg_name other_ty name,
                                   arg]
      (Ptr somekind) -> Call (Nam "pony_sendp") [AsExpr . Var $ show other,
                                   AsExpr . AsLval $ method_msg_name other_ty name,
                                   arg]
      other -> error $ "Expr.hs: don't know how to send `"++show other++"`"
args_to_call other other_ty name manyargs = (Embed "//Expr.hs: don't know how to call many args\n")



-- allocates 
--make_call_record :: A.MethodDecl -> [CCode Expr] -> CCode Stat
--make_call_record mdecl =
--    let
--        -- this assumes that everything has the same size. It's broken, need to fix later
--        sum_of_sizeofs = Embed $ ("sizeof(void*)*" ++ show (length (A.mparams mdecl)))
----        temp_ptr = temp_name "call_rec"
--    in (Assign (Decl temp_name

