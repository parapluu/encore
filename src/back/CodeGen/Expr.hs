{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type
import qualified CodeGen.Context as Ctx
import Data.List

import qualified Parser.Parser as P -- for string interpolation in the embed expr
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as PString

import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
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
    ID.MOD -> "%"

instance Translatable A.LVal (State Ctx.Context (CCode Lval, CCode Stat)) where
  translate (A.LVal ty name) =
      do
        c <- get
        case Ctx.subst_lkp c name of
          Just subst_name ->
              return (subst_name, Skip)
          Nothing ->
              return $ (Var $ show name, Skip)
  translate (A.LField ty ex name) = do
      (nex,tex) <- translate ex
      return (EmbedC $ Deref nex `Dot` (Nam $ show name),
              tex)

type_to_printf_fstr :: Ty.Type -> String
type_to_printf_fstr ty 
    | Ty.isIntType ty = "%lli"
    | Ty.isRealType ty = "%f"
    | Ty.isStringType ty = "%s"
    | Ty.isBoolType ty = "bool(%d)"
    | otherwise = case translate ty of
                    Ptr something -> "%p"
                    _ -> "Expr.hs: type_to_printf_fstr not defined for " ++ show ty

-- | If the type is not void, create a variable to store it in. If it is void, return the lval UNIT
tmp_var :: Ty.Type -> CCode Expr -> State Ctx.Context (CCode Lval, CCode Stat)
tmp_var ty cex 
    | Ty.isVoidType ty = return $ (unit, Seq [cex])
    | otherwise     = do na <- Ctx.gen_sym
                         return $ (Var na, Assign (Decl (translate ty, Var na)) cex)

tmp_arr :: CCode Ty -> [CCode Expr] -> State Ctx.Context (CCode Lval, CCode Stat)
tmp_arr cty arr = do
    na <- Ctx.gen_sym
    return $ (Var na, Assign (Decl (cty, Var $ na ++ "[]")) (Record arr))

substitute_var :: ID.Name -> CCode Lval -> State Ctx.Context ()
substitute_var na impl = do
  c <- get
  put $ Ctx.subst_add c na impl
  return ()

-- these two are exclusively used for A.Embed translation:
type ParsedEmbed = [Either String VarLkp]
newtype VarLkp = VarLkp String

instance Translatable A.Expr (State Ctx.Context (CCode Lval, CCode Stat)) where
  translate skip@(A.Skip {}) = tmp_var (A.getType skip) (AsExpr unit)
  translate null@(A.Null {}) = tmp_var (A.getType null) Null
  translate true@(A.BTrue {}) = tmp_var (A.getType true) (Embed "1/*True*/"::CCode Expr)
  translate false@(A.BFalse {}) = tmp_var (A.getType false) (Embed "0/*False*/"::CCode Expr)
  translate lit@(A.IntLiteral {A.intLit = i}) = tmp_var (A.getType lit) (Embed (show i))
  translate lit@(A.RealLiteral {A.realLit = r}) = tmp_var (A.getType lit) (Embed (show r))
  translate lit@(A.StringLiteral {A.stringLit = s}) = tmp_var (A.getType lit) (Embed (show s))

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
        return $ (unit,
                  Seq [te,
                       (Statement
                        (Call (Nam "printf") 
                                  [Embed $ "\""++ type_to_printf_fstr (A.getType e)++"\\n\"", ne]))])

  translate seq@(A.Seq {A.eseq = es}) = do
    ntes <- mapM translate es
    let (nes, tes) = unzip ntes
    return (last nes, Seq tes)

  translate (A.Assign {A.lhs = lvar, A.rhs = expr}) = do
    (nexpr,texpr) <- translate expr
    (nlvar, tlvar) <- translate lvar
    return (unit,
            Seq [texpr,
                 tlvar,
                 if Ty.isVoidType $ A.getType lvar then Skip else Seq[Assign nlvar nexpr]])

  translate (A.VarAccess {A.name = name}) = do
      c <- get
      case Ctx.subst_lkp c name of
        Just subst_name ->
            return (subst_name , Skip)
        Nothing ->
            return (Var $ show name, Skip)

  translate acc@(A.FieldAccess {A.target = exp, A.name = name}) = do
    (nexp,texp) <- translate exp
    tmp <- Ctx.gen_sym
    return (Var tmp, Seq [texp,
                      (Assign (Decl (translate (A.getType acc), Var tmp)) (Deref nexp `Dot` (Nam $ show name)))])

  translate l@(A.Let {A.name = name, A.val = e1, A.body = e2}) = do
                       (ne1,te1) <- translate e1
                       tmp <- Ctx.gen_sym
                       substitute_var name (Var tmp)
                       (ne2,te2) <- translate e2
                       return (ne2,
                               Seq [te1,
                                    Assign (Decl (translate (A.getType e1), Var tmp)) (ne1),
                                    te2])

  translate new@(A.New {A.ty = ty}) 
      | Ty.isActiveRefType ty = tmp_var ty (Call (Nam "create_and_send")
                                                 [Amp $ actor_rec_name ty,
                                                  AsExpr . AsLval . Nam $ "MSG_alloc"])
      | otherwise = tmp_var ty (Call (Nam "pony_alloc") 
                                     [Sizeof $ Typ $ show (data_rec_name ty)])

  translate call@(A.MethodCall { A.target=target, A.name=name, A.args=args }) 
      | (A.isThisAccess target) ||
        (Ty.isPassiveRefType . A.getType) target = sync_call
      | otherwise = remote_call
          where
            sync_call =
                do (ntarget, ttarget) <- translate target
                   targs <- mapM varaccess_this_to_aref args
                   tmp <- Ctx.gen_sym
                   return (Var tmp, (Seq [ttarget,
                                          Assign (Decl (translate (A.getType call), Var tmp)) 
                                                 (Call (method_impl_name (A.getType target) name)
                                                  ((EmbedC ntarget) : targs))]))

            remote_call :: State Ctx.Context (CCode Lval, CCode Stat)
            remote_call =
                do ttarget <- varaccess_this_to_aref target
                   tmp <- Ctx.gen_sym
                   targs <- mapM varaccess_this_to_aref args
                   let argtys = (map A.getType args)
                   let targtys = map (translate . A.getType) args :: [CCode Ty]
                   the_fut_name <- Ctx.gen_sym
                   let the_fut_decl = Assign (Decl (Ptr $ Typ "future_t", Var the_fut_name)) (Call (Nam "future_mk") ([] :: [CCode Lval]))
                   the_arg_name <- Ctx.gen_sym
                   let the_arg_decl = Assign
                                        (Decl (Typ "pony_arg_t", ArrAcc (1 + length args) (Var the_arg_name)))
                                        (Record
                                          ((Embed $ "{.p = " ++ the_fut_name ++ "}") : 
                                          (map (\(arg, ty) -> Embed $ "{"++(pony_arg_t_tag ty) ++"="++ (show arg)++"}")
                                          (zip (targs) targtys)) :: [CCode Expr]))
                   the_call <- return (Call (Nam "pony_sendv")
                                               [ttarget,
                                                AsExpr . AsLval $ method_msg_name (A.getType target) name,
                                                Embed . show $ 1 + length args,
                                                AsExpr $ Var the_arg_name])
                   return (Var the_fut_name, 
                           Seq [the_fut_decl,
                                the_arg_decl,
                                Statement the_call])

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
         tmp <- Ctx.gen_sym;
         let export_body = Seq $ tbody : [EmbedC (Assign (Var tmp) nbody)]
         return (Var tmp,
                 Seq [EmbedC $ Decl ((translate (A.getType w)), Var tmp),
                      (While (StatAsExpr ncond tcond) (Statement export_body))])

  translate ite@(A.IfThenElse { A.cond = cond, A.thn = thn, A.els = els }) =
      do tmp <- Ctx.gen_sym
         (ncond,tcond) <- translate cond
         (nthn, tthn) <- translate thn
         (nels, tels) <- translate els
         let export_thn = Seq $ tthn : [Assign (Var (tmp++"_ite")) nthn]
             export_els = Seq $ tels : [Assign (Var (tmp++"_ite")) nels]
         return (Var (tmp++"_ite"),
                 Seq [AsExpr $ Decl (translate (A.getType ite), Var $ tmp++"_ite"),
                      If (StatAsExpr ncond tcond) (Statement export_thn) (Statement export_els)])

  translate e@(A.Embed {A.code=code}) = do
    interpolated <- interpolate code
    if Ty.isVoidType (A.getType e) then
        return (unit, Embed $ "({" ++ interpolated  ++ "})")
    else
        tmp_var (A.getType e) (Embed $ "({" ++ interpolated  ++ "})")
        where
          interpolate :: String -> State Ctx.Context String
          interpolate embedstr =
              case (Parsec.parse interpolate_parser "embed expression" embedstr) of
                (Right parsed) -> do
                  strs <- mapM to_looked_up_string parsed
                  return $ concat strs
                (Left err) -> error $ show err

          to_looked_up_string :: Either String VarLkp -> State Ctx.Context String
          to_looked_up_string e = case e of
                                    (Right (VarLkp var)) -> do
                                           ctx <- get
                                           case Ctx.subst_lkp ctx $ (ID.Name var) of
                                             (Just found) -> return $ show found
                                             Nothing      -> return var -- hope that it's a parameter,
                                                                        -- let clang handle the rest

                                    (Left str)           -> return str

          interpolate_parser :: PString.Parser [Either String VarLkp]
          interpolate_parser = do
            Parsec.many
                  (Parsec.try
                             (do
                               var <- varlkp_parser
                               return (Right (VarLkp var)))
                   Parsec.<|>
                         (do
                           c <- Parsec.anyChar
                           return (Left [c])))

          varlkp_parser :: PString.Parser String
          varlkp_parser = do
                            Parsec.string "#{"
                            id <- P.identifier_parser
                            Parsec.string "}"
                            return id

  translate get@(A.Get{A.val = val}) = 
      do (nval, tval) <- translate val
         let the_get = Statement $ Call (Nam "future_get") [nval, Var "this->aref"]
         tmp <- Ctx.gen_sym
         return (Var tmp, Seq [tval, Assign (Decl (translate (A.getType val), Var tmp)) the_get])

  translate clos@(A.Closure{A.eparams = params, A.body = body}) = 
      do let fun_name = closure_fun_name $ A.getMetaId clos
             env_name = closure_env_name $ A.getMetaId clos
             free_vars = Util.freeVariables (map A.pname params) body
         tmp <- Ctx.gen_sym
         fill_env <- mapM (insert_var env_name) free_vars
         return $ (Var tmp, Seq $ (mk_env env_name) : fill_env ++
                           [Assign (Decl (closure, Var tmp)) 
                                       (Call (Nam "mk_closure") [fun_name, env_name])])
      where
        mk_env name = 
            Assign (Decl (Ptr $ Struct name, AsLval name))
                    (Call (Nam "malloc") 
                          [Sizeof $ Struct name])
        insert_var env_name (name, _) = 
            do c <- get
               let tname = case Ctx.subst_lkp c name of
                              Just subst_name -> subst_name
                              Nothing -> Var $ show name
               return $ Assign ((Deref $ Var $ show env_name) `Dot` (Nam $ show name)) tname

  translate fcall@(A.FunctionCall{A.name = name, A.args = args}) = 
      do c <- get
         let clos = Var $ (case Ctx.subst_lkp c name of
                             Just subst_name -> show subst_name
                             Nothing -> show name)
             ty = A.getType fcall
         targs <- mapM translateArgument args
         (tmp_args, tmp_arg_decl) <- tmp_arr (Typ "value_t") targs
         (calln, the_call) <- tmp_var ty $ Call (getValFun ty) [Call (Nam "closure_call") [clos, tmp_args]]
         return (if Ty.isVoidType ty then unit else calln, Seq [tmp_arg_decl, the_call])
      where
        getValFun ty
            | Ty.isIntType  ty = Nam "val_to_int"
            | Ty.isRealType ty = Nam "val_to_dbl"
            | otherwise        = Nam "val_to_ptr"
        translateArgument arg = 
            do (ntother, tother) <- translate arg
               return $ Call (toValFun (A.getType arg)) [StatAsExpr ntother tother]
            where
              toValFun ty
                  | Ty.isIntType  ty = Nam "int_to_val"
                  | Ty.isRealType ty = Nam "dbl_to_val"
                  | otherwise        = Nam "ptr_to_val"


  translate other = error $ "Expr.hs: can't translate: '" ++ show other ++ "'"
