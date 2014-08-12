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
    ID.NOT -> "!"
    ID.AND -> "&&"
    ID.OR -> "||"
    ID.LT -> "<"
    ID.GT -> ">"
    ID.LTE -> "<="
    ID.GTE -> ">="
    ID.EQ -> "=="
    ID.NEQ -> "!="
    ID.PLUS -> "+"
    ID.MINUS -> "-"
    ID.TIMES -> "*"
    ID.DIV -> "/"
    ID.MOD -> "%"

type_to_printf_fstr :: Ty.Type -> String
type_to_printf_fstr ty 
    | Ty.isIntType ty    = "%lli"
    | Ty.isRealType ty   = "%f"
    | Ty.isStringType ty = "%s"
    | Ty.isBoolType ty   = "bool<%lld>"
    | Ty.isRefType ty    = show ty ++ "<%p>"
    | Ty.isFutureType ty = "fut<%p>"
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
  translate lit@(A.IntLiteral {A.intLit = i}) = tmp_var (A.getType lit) (Int i)
  translate lit@(A.RealLiteral {A.realLit = r}) = tmp_var (A.getType lit) (Double r)
  translate lit@(A.StringLiteral {A.stringLit = s}) = tmp_var (A.getType lit) (String s)

  translate unary@(A.Unary {A.op = op, A.operand = e}) = do
    (ne, ts) <- translate e
    tmp <- Ctx.gen_sym
    return $ (Var tmp,
              Seq [ts,
                   Statement (Assign
                              (Decl (translate $ A.getType unary, Var tmp))
                              (CUnary (translate op)
                                         (ne :: CCode Lval)))])

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

  translate (A.Print {A.stringLit = s, A.args = args}) = do
      targs <- mapM translate args
      let arg_names = map (AsExpr . fst) targs
      let arg_decls = map snd targs
      let arg_tys   = map A.getType args
      let fstring = format_string s arg_tys
      return $ (unit,
                Seq $ arg_decls ++
                     [Statement 
                      (Call (Nam "printf")
                       ((String fstring) : arg_names))])
      where
        format_string s [] = s
        format_string "" (ty:tys) = error "Wrong number of arguments to printf"
        format_string ('{':'}':s) (ty:tys) = (type_to_printf_fstr ty) ++ (format_string s tys)
        format_string (c:s) tys = c : (format_string s tys)

  translate exit@(A.Exit {A.args = [arg]}) = do
      (narg, targ) <- translate arg
      let exit_call = Call (Nam "exit") [narg]
      return (unit, Seq [Statement targ, Statement exit_call])

  translate seq@(A.Seq {A.eseq = es}) = do
    ntes <- mapM translate es
    let (nes, tes) = unzip ntes
    return (last nes, Seq tes)

  translate (A.Assign {A.lhs = lhs, A.rhs = rhs}) = do
    (nrhs, trhs) <- translate rhs
    (nlhs, tlhs) <- translate lhs
    lval <- mk_lval lhs
    return (unit, Seq [trhs, Assign lval nrhs])
        where
          mk_lval (A.VarAccess {A.name = name}) =
              do ctx <- get
                 case Ctx.subst_lkp ctx name of
                   Just subst_name -> return subst_name
                   Nothing -> return $ Var (show name)
          mk_lval (A.FieldAccess {A.target = target, A.name = name}) =
              do (ntarg, ttarg) <- translate target
                 return (Deref ntarg `Dot` (Nam $ show name))
          mk_lval e = error $ "Cannot translate '" ++ (show e) ++ "' to a valid lval"

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

  translate l@(A.Let {A.decls = decls, A.body = body}) = 
                     do
                       tdecls <- translate_decls decls
                       (nbody, tbody) <- translate body
                       return (nbody, Seq $ tdecls ++ [tbody])
                     where
                       translate_decls [] = return []
                       translate_decls ((name, expr):decls) = 
                           do (ne, te) <- translate expr
                              tmp <- Ctx.gen_sym
                              substitute_var name (Var tmp)
                              tdecls <- translate_decls decls
                              return $ [te, Assign (Decl (translate (A.getType expr), Var tmp)) ne] ++ tdecls

  translate new@(A.New {A.ty = ty}) 
      | Ty.isActiveRefType ty = tmp_var ty (Call (Nam "create_and_send")
                                                 [Amp $ actor_rec_name ty,
                                                  AsExpr . AsLval . Nam $ "MSG_alloc"])
      | otherwise = do na <- Ctx.gen_sym
                       let size = Sizeof $ Typ $ show (data_rec_name ty)
                       return $ (Var na, Seq $ 
                                         [Assign (Decl (translate ty, Var na))
                                          (Call (Nam "pony_alloc") [size]),
                                          Statement (Call (Nam "memset") [AsExpr $ Var na, Int 0, size])])

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
                                                  ((AsExpr ntarget) : targs))]))

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
                                          ((UnionInst (Nam "p") (Var the_fut_name)) : 
                                          (map (\(arg, ty) -> UnionInst (pony_arg_t_tag ty) arg)
                                               (zip (targs) targtys)) :: [CCode Expr]))
                   the_call <- return (Call (Nam "pony_sendv")
                                               [ttarget,
                                                AsExpr . AsLval $ method_msg_name (A.getType target) name,
                                                Int $ 1 + length args,
                                                AsExpr $ Var the_arg_name])
                   return (Var the_fut_name, 
                           Seq [the_fut_decl,
                                the_arg_decl,
                                Statement the_call])

            pony_arg_t_tag :: CCode Ty -> CCode Name
            pony_arg_t_tag (Ptr _)         = Nam "p"
            pony_arg_t_tag (Typ "int64_t") = Nam "i"
            pony_arg_t_tag (Typ "double")  = Nam "d"
            pony_arg_t_tag other           =
                error $ "Expr.hs: no pony_arg_t_tag for " ++ show other

            varaccess_this_to_aref :: A.Expr -> State Ctx.Context (CCode Expr)
            varaccess_this_to_aref (A.VarAccess { A.name = ID.Name "this" }) = return $ AsExpr $ Deref (Var "this") `Dot` (Nam "aref")
            varaccess_this_to_aref other                                     = do
                     (ntother, tother) <- translate other
                     return $ StatAsExpr ntother tother

  translate send@(A.MessageSend { A.target=target, A.name=name, A.args=args }) 
      | (Ty.isActiveRefType . A.getType) target = message_send
      | otherwise = error "Tried to send a message to something that was not an active reference"
          where
            message_send :: State Ctx.Context (CCode Lval, CCode Stat)
            message_send =
                do ttarget <- varaccess_this_to_aref target
                   tmp <- Ctx.gen_sym
                   targs <- mapM varaccess_this_to_aref args
                   let argtys = (map A.getType args)
                   let targtys = map (translate . A.getType) args :: [CCode Ty]
                   the_arg_name <- Ctx.gen_sym
                   let the_arg_decl = Assign
                                        (Decl (Typ "pony_arg_t", ArrAcc (length args) (Var the_arg_name)))
                                        (Record
                                          ((map (\(arg, ty) -> UnionInst (pony_arg_t_tag ty) arg)
                                          (zip (targs) targtys)) :: [CCode Expr]))
                   the_call <- return (Call (Nam "pony_sendv")
                                               [ttarget,
                                                AsExpr . AsLval $ one_way_send_msg_name (A.getType target) name,
                                                Int $ length args,
                                                AsExpr $ Var the_arg_name])
                   return (unit, 
                           Seq [the_arg_decl,
                                Statement the_call])

            pony_arg_t_tag :: CCode Ty -> CCode Name
            pony_arg_t_tag (Ptr _)         = Nam "p"
            pony_arg_t_tag (Typ "int64_t") = Nam "i"
            pony_arg_t_tag (Typ "double")  = Nam "d"
            pony_arg_t_tag other = error $ "Expr.hs: no pony_arg_t_tag for " ++ show other

            varaccess_this_to_aref :: A.Expr -> State Ctx.Context (CCode Expr)
            varaccess_this_to_aref (A.VarAccess { A.name = ID.Name "this" }) = return $ AsExpr $ Deref (Var "this") `Dot` (Nam "aref")
            varaccess_this_to_aref other                                     = do
                     (ntother, tother) <- translate other
                     return $ StatAsExpr ntother tother

  translate w@(A.While {A.cond = cond, A.body = body}) = 
      do (ncond,tcond) <- translate cond
         (nbody,tbody) <- translate body
         tmp <- Ctx.gen_sym;
         let export_body = Seq $ tbody : [Assign (Var tmp) nbody]
         return (Var tmp,
                 Seq [Statement $ Decl ((translate (A.getType w)), Var tmp),
                      (While (StatAsExpr ncond tcond) (Statement export_body))])

  translate ite@(A.IfThenElse { A.cond = cond, A.thn = thn, A.els = els }) =
      do tmp <- Ctx.gen_sym
         (ncond, tcond) <- translate cond
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
         let result_type = translate (Ty.getResultType $ A.getType val)
         let the_get = Cast (result_type) $ Call (Nam "future_get") [nval, Var "this->aref"]
         tmp <- Ctx.gen_sym
         return (Var tmp, Seq [tval, Assign (Decl (result_type, Var tmp)) the_get])

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
                    (Call (Nam "malloc") -- Use malloc until we figure out how to trace environments
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
         (calln, the_call) <- tmp_var ty $ arg_member ty (Call (Nam "closure_call") [clos, tmp_args])
         return (if Ty.isVoidType ty then unit else calln, Seq [tmp_arg_decl, the_call])
      where
        arg_member :: Ty.Type -> CCode Expr -> CCode Expr
        arg_member ty e
            | Ty.isVoidType ty = e
            | Ty.isIntType  ty = AsExpr $ e `Dot` Nam "i"
            | Ty.isRealType ty = AsExpr $ e `Dot` Nam "d"
            | otherwise        = AsExpr $ e `Dot` Nam "p"
        translateArgument arg = 
            do (ntother, tother) <- translate arg
               return $ UnionInst (arg_member $ A.getType arg) (StatAsExpr ntother tother)
            where
              arg_member ty
                  | Ty.isIntType  ty = Nam "i"
                  | Ty.isRealType ty = Nam "d"
                  | otherwise        = Nam "p"

  translate other = error $ "Expr.hs: can't translate: '" ++ show other ++ "'"
