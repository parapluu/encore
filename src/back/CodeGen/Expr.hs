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
import qualified Types as Ty

import Control.Monad.State hiding (void)
import Control.Monad.Reader hiding (void)
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

instance Translatable A.LVal (State Ctx.Context (CCode Lval, CCode Stat)) where
  translate (A.LVal ty name) =
      do
        c <- get
        case Ctx.subst_lkp c name of
          Just subst_name ->
              return (subst_name, Skip)
          Nothing ->
              return $ (Embed $ show name, Skip)
  translate (A.LField ty ex name) = do
      (nex,tex) <- translate ex
      return (EmbedC $ Deref nex `Dot` (Nam $ show name),
              tex)

type_to_printf_fstr :: Ty.Type -> String
type_to_printf_fstr ty 
    | Ty.isIntType ty = "%lli"
    | Ty.isRealType ty = "%f"
    | Ty.isStringType ty = "%s"
    | otherwise = case translate ty of
                    Ptr something -> "%p"
                    _ -> "Expr.hs: type_to_printf_fstr not defined for " ++ show ty

-- | If the type is not void, create a variable to store it in.
tmp_var :: Ty.Type -> CCode Expr -> State Ctx.Context (CCode Lval, CCode Stat)
tmp_var ty cex = do
  if not $ Ty.isVoidType ty
  then do
    na <- Ctx.gen_sym
    return $ (Var na, Assign (Decl (translate ty, Var na)) cex)
  else return (error $ show cex ++ " is void",Statement cex)

substitute_var :: ID.Name -> CCode Lval -> State Ctx.Context ()
substitute_var na impl = do
  c <- get
  put $ Ctx.subst_add c na impl
  return ()

instance Translatable A.Expr (State Ctx.Context (CCode Lval, CCode Stat)) where
  translate (A.Skip {}) = return $ (error "it's void", Embed "/* skip */")
  translate (A.Null {}) = do
    tmp <- Ctx.gen_sym
    return $ (Var tmp, Seq [Assign (Decl (Ptr void, Var tmp)) (Embed "NULL"::CCode Expr)])
  translate (A.BTrue {}) = do
    tmp <- Ctx.gen_sym
    return $ (Var tmp, Seq [Assign (Decl (bool, Var tmp)) $ (Embed "1"::CCode Expr)])
  translate (A.BFalse {}) = do
    tmp <- Ctx.gen_sym
    return $ (Var tmp, Seq [Assign (Decl (bool, Var tmp)) $ (Embed "0"::CCode Expr)])
  translate bin@(A.Binop {A.op = op, A.loper = e1, A.roper = e2}) = do
    (ne1,ts1) <- translate (e1 :: A.Expr)
    (ne2,ts2) <- translate (e2 :: A.Expr)
    tmp <- Ctx.gen_sym
    return $ (Var tmp,
              Seq [ts1,
                   ts2,
                   Statement (Assign
                              (Decl (translate $ A.getType bin, Var tmp))
                              (BinOp (translate op)
                                         (ne1 :: CCode Lval)
                                         (ne2 :: CCode Lval)))])
  translate (A.Print {A.val = e}) =
      do
        (ne,te) <- translate e
        return $ (Var "NULL",
                  Seq [te,
                       (Statement
                        (Call (Nam "printf") -- TODO: weird Seq
                                  [Embed $ "\""++ type_to_printf_fstr (A.getType e)++"\\n\"", ne]))])
  translate seq@(A.Seq {A.eseq = es}) = do
    ntes <- mapM translate es
    let (nes, tes) = unzip ntes
    tmp <- Ctx.gen_sym
    let lastn = head $ reverse nes
    return (lastn, Seq $ (tes :: [CCode Stat]))
  translate (A.Assign {A.lhs = lvar, A.rhs = expr}) = do
    (nexpr,texpr) <- translate expr
    (nlvar, tlvar) <- translate lvar
    return (nlvar,
            Seq [texpr,
                 tlvar,
                 Seq[Assign nlvar nexpr]])
  translate (A.VarAccess {A.name = name}) = do
      c <- get
      case Ctx.subst_lkp c name of
        Just subst_name ->
            return (subst_name , Skip)
        Nothing ->
            return (Var $ show name, Skip)
  translate (A.FieldAccess {A.target = exp, A.name = name}) = do
    (nexp,texp) <- translate exp
    tmp <- Ctx.gen_sym
    return (EmbedC $ Deref nexp `Dot` (Nam $ show name),
            texp)
  translate lit@(A.IntLiteral {A.intLit = i}) = do
      tmp_var (A.getType lit) (Embed (show i))
  translate lit@(A.RealLiteral {A.realLit = r}) = do
      tmp_var (A.getType lit) (Embed (show r))
  translate lit@(A.StringLiteral {A.stringLit = s}) = do
      tmp_var (A.getType lit) (Embed (show s))
  translate l@(A.Let {A.name = name, A.val = e1, A.body = e2}) = do
                       -- TODO: this does not allow for nested lets with equal names yet
                       (ne1,te1) <- translate e1
                       substitute_var name ne1
                       (ne2,te2) <- translate e2
                       return (ne2,
                               Seq [te1,
                                    te2])
  translate new@(A.New {A.ty = ty}) = do
    tmp_var ty (Call (Nam "create_and_send")
                         [Amp $ actor_rec_name ty,
                          AsExpr . AsLval . Nam $ "MSG_alloc"])
  translate call@(A.MethodCall { A.target=target, A.name=name, A.args=args }) =
      (case target of
        (A.VarAccess { A.name = ID.Name "this"}) -> local_call
        _ -> remote_call)
          where
            local_call =
                do ctx <- get
                   tmp <- Ctx.gen_sym
                   (ntarget,_) <- translate target
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
                   tmp_var (A.getType call) (Call
                                             (method_impl_name (A.cname . fromJust $ Ctx.the_class ctx) name)
                                             ((EmbedC ntarget) : targs))
            remote_call :: State Ctx.Context (CCode Lval, CCode Stat)
            remote_call =
                do ttarget <- varaccess_this_to_aref target
                   tmp <- Ctx.gen_sym
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
                   (tmp_n, tmp_s) <- (tmp_var (A.getType call) (Call
                                               (Nam "pony_sendv")
                                               ([ttarget,
                                                 AsExpr . AsLval $ method_msg_name (A.getType target) name,
                                                 Embed . show . length $ args] ++
                                                [Embed the_arg_name])))
                   return (tmp_n,
                           Seq [the_arg_decl,
                                tmp_s])

            pony_arg_t_tag :: CCode Ty -> String
            pony_arg_t_tag (Ptr _)         = ".p"
            pony_arg_t_tag (Typ "int64_t") = ".i"
            pony_arg_t_tag (Typ "double")  = ".d"
            pony_arg_t_tag other           =
                error $ "Expr.hs: no pony_arg_t_tag for " ++ show other

            varaccess_this_to_aref :: A.Expr -> State Ctx.Context (CCode Expr)
            varaccess_this_to_aref (A.VarAccess { A.name = ID.Name "this" }) = return $ AsExpr $ Deref (Var "this") `Dot` (Nam "aref")
            varaccess_this_to_aref other                                     = do
                     (ntother, tother) <- translate other
                     return $ StatAsExpr ntother tother
  translate w@(A.While {A.cond = cond, A.body = body}) = 
      do (ncond,tcond) <- translate cond
         (nbody,tbody) <- translate body
         if not $ Ty.isVoidType (A.getType w)
         then do
           tmp <- Ctx.gen_sym;
           let export_body = Seq $ tbody : [EmbedC (Assign (Var tmp) nbody)]
           return (Var tmp,
                   Seq [Embed ((show (A.getType w)) ++ " " ++ tmp),
                        (While (StatAsExpr ncond tcond) (Statement export_body))])
         else do
           return (error $ show w ++ " is void",
                   (While (StatAsExpr ncond tcond) (Statement tbody)))

  translate ite@(A.IfThenElse { A.cond = cond, A.thn = thn, A.els = els }) =
      do 
--         tmp_var (A.getType ite) (If (StatAsExpr ncond tcond) (Statement tthn) (Statement tels))
        if not $ Ty.isVoidType (A.getType ite)
        then do tmp <- Ctx.gen_sym
                (ncond,tcond) <- translate cond
                (nthn, tthn) <- translate thn
                (nels, tels) <- translate els
                let export_thn = Seq $ tthn : [Assign (Var (tmp++"_ite")) nthn]
                let export_els = Seq $ tels : [Assign (Var (tmp++"_ite")) nels]
                return (Var (tmp++"_ite"),
                        Seq [Embed ((show $ translate (A.getType ite)) ++ " " ++ (tmp++"_ite")),
                             (If (StatAsExpr ncond tcond) (Statement export_thn) (Statement export_els))])
        else do (ncond,tcond) <- translate cond
                (nthn, tthn) <- translate thn
                (nels, tels) <- translate els
                return (error $ show ite ++ " is void",
                        (Statement $ If (StatAsExpr ncond tcond) (Statement tthn) (Statement tels)))

--          return (If tcond (Statement tthn) (Statement tels))
  translate e@(A.Embed {A.code=code}) = do
    tmp_var (A.getType e) (Embed code)
  translate other = error $ "Expr.hs: can't translate: `" ++ show other ++ "`"
