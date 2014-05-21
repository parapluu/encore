{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type
import qualified CodeGen.Context as Ctx
import Data.List

import CCode.Main

import qualified AST.AST as A
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
  translate (A.Skip {}) = return $ Embed "/* skip */"
  translate (A.Null {}) = return $ Embed "NULL"
  translate (A.Binop {A.op = op, A.loper = e1, A.roper = e2}) = do
    te1 <- translate e1
    te2 <- translate e2
    return $ BinOp (translate op) te1 te2
  translate (A.Print {A.val = e}) =
      do
        te <- translate e
        return $ Call (Nam "printf") [Embed $ "\""++ type_to_printf_fstr (A.getType e)++"\\n\"",
                                      te :: CCode Expr]
  translate (A.Seq {A.eseq = es}) = do
    tes <- mapM translate es
    return $ StoopidSeq tes
  translate (A.Assign {A.lhs = lvar, A.rhs = expr}) = do
    texpr <- translate expr
    tlvar <- translate lvar
    return $ Assign (tlvar :: CCode Lval) texpr
  translate (A.VarAccess {A.name = name}) =
    return $ Embed $ show name
  translate (A.FieldAccess {A.target = exp, A.name = name}) = do
    texp <- translate exp
    return $ AsExpr $ Deref (texp :: CCode Expr) `Dot` (Nam $ show name)
  translate (A.IntLiteral {A.intLit = i}) =
    return $ Embed $ show i
  translate (A.StringLiteral {A.stringLit = s}) =
    return $ Embed $ show s
  translate l@(A.Let {A.name = name, A.val = e1, A.body = e2}) =
-- this uses an obscure feature of C: compound statements can form expressions!
-- http://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html
                   do
                     te1 <- translate e1
                     s <- get
                     put (Ctx.with_local (A.Param {A.pname = name, A.ptype = A.getType e1, A.pmeta = A.emeta e1}) s) 
                     te2 <- translate e2
                     put s
                     return (Braced . StoopidSeq $
                             [Assign 
                              (Decl (translate (A.getType e1), Var $ show name)) te1,
                                                                                 te2])
  translate (A.New {A.ty = ty}) = return $ Call (Nam "create_and_send") [Amp $ actor_rec_name ty,
                                                                AsExpr . AsLval . Nam $ "MSG_alloc"]
  translate (A.Call { A.target=target, A.name=name, A.args=args }) =
      (case target of
        (A.VarAccess { A.name = ID.Name "this"}) -> local_call
        _ -> remote_call)
          where
            local_call =
                do ctx <- get
                   ttarget <- translate target
                   targs <- mapM varaccess_this_to_aref args
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
                   return $ Call
                              (method_impl_name (A.cname . fromJust $ Ctx.the_class ctx) name)
                              (ttarget : targs)
                             
            remote_call :: State Ctx.Context (CCode Expr)
            remote_call =
                do ttarget <- varaccess_this_to_aref target
                   targs <- mapM varaccess_this_to_aref args
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
                               ([ttarget :: CCode Expr,
                                 AsExpr . AsLval $ method_msg_name (A.getType target) name,
                                 Embed . show . length $ args] ++
                                [Embed the_arg_name])]
            pony_arg_t_tag :: CCode Ty -> String
            pony_arg_t_tag (Ptr _)        = ".p"
            pony_arg_t_tag (Typ "int")    = ".i"
            pony_arg_t_tag (Typ "double") = ".d"
            pony_arg_t_tag other          =
                error $ "Expr.hs: no pony_arg_t_tag for " ++ show other

            varaccess_this_to_aref :: A.Expr -> State Ctx.Context (CCode Expr)
            varaccess_this_to_aref (A.VarAccess { A.name = ID.Name "this" }) = return $ AsExpr $ Deref (Var "this") `Dot` (Nam "aref")
            varaccess_this_to_aref other                                   = translate other

  translate w@(A.While {A.cond = cond, A.body = body}) = 
      do tcond <- translate cond
         tbody <- translate (body :: A.Expr)
         return (While tcond (Statement (tbody :: CCode Expr)))
  translate (A.IfThenElse { A.cond = cond, A.thn = thn, A.els = els }) =
      do tcond <- translate cond
         tthn <- translate thn
         tels <- translate els
         return (If tcond (Statement (tthn :: CCode Expr)) (Statement (tels :: CCode Expr)))
  translate other = error $ "Expr.hs: can't translate: `" ++ show other ++ "`"
