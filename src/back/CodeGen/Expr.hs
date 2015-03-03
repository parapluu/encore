{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs, NamedFieldPuns #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type
import qualified CodeGen.Context as Ctx

import qualified Parser.Parser as P -- for string interpolation in the embed expr
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as PString

import CCode.Main
import CodeGen.ClassTable

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta
import qualified AST.PrettyPrinter as PP
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding (void)
import Data.List

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
    | Ty.isBoolType ty   = "bool<%zd>"
    | Ty.isRefType ty    = show ty ++ "<%p>"
    | Ty.isFutureType ty = "fut<%p>"
    | otherwise = case translate ty of
                    Ptr something -> "%p"
                    _ -> "Expr.hs: type_to_printf_fstr not defined for " ++ show ty

-- | If the type is not void, create a variable to store it in. If it is void, return the lval UNIT
named_tmp_var :: String -> Ty.Type -> CCode Expr -> State Ctx.Context (CCode Lval, CCode Stat)
named_tmp_var name ty cex
    | Ty.isVoidType ty = return $ (unit, Seq [cex])
    | otherwise     = do na <- Ctx.gen_named_sym name
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

unsubstitute_var :: ID.Name -> State Ctx.Context ()
unsubstitute_var na = do
  c <- get
  put $ Ctx.subst_rem c na
  return ()


-- these two are exclusively used for A.Embed translation:
type ParsedEmbed = [Either String VarLkp]
newtype VarLkp = VarLkp String

instance Translatable A.Expr (State Ctx.Context (CCode Lval, CCode Stat)) where
  -- | Translate an expression into the corresponding C code
  translate skip@(A.Skip {}) = named_tmp_var "skip" (A.getType skip) (AsExpr unit)
  translate breathe@(A.Breathe {}) =
    named_tmp_var "breathe"
                  (A.getType breathe)
                  (Call (Nam "call_respond_with_current_scheduler") ([] :: [CCode Expr]))
  translate null@(A.Null {}) = named_tmp_var "literal" (A.getType null) Null
  translate true@(A.BTrue {}) = named_tmp_var "literal"  (A.getType true) (Embed "1/*True*/"::CCode Expr)
  translate false@(A.BFalse {}) = named_tmp_var "literal" (A.getType false) (Embed "0/*False*/"::CCode Expr)
  translate lit@(A.IntLiteral {A.intLit = i}) = named_tmp_var "literal" (A.getType lit) (Int i)
  translate lit@(A.RealLiteral {A.realLit = r}) = named_tmp_var "literal" (A.getType lit) (Double r)
  translate lit@(A.StringLiteral {A.stringLit = s}) = named_tmp_var "literal" (A.getType lit) (String s)

  translate unary@(A.Unary {A.op, A.operand}) = do
    (noperand, toperand) <- translate operand
    tmp <- Ctx.gen_named_sym "unary"
    return $ (Var tmp,
              Seq [toperand,
                   Statement (Assign
                              (Decl (translate $ A.getType unary, Var tmp))
                              (CUnary (translate op) noperand))])

  translate bin@(A.Binop {A.op, A.loper, A.roper}) = do
    (nlo, tlo) <- translate (loper :: A.Expr)
    (nro, tro) <- translate (roper :: A.Expr)
    tmp <- Ctx.gen_named_sym "binop"
    return $ (Var tmp,
              Seq [tlo,
                   tro,
                   Statement (Assign
                              (Decl (translate $ A.getType bin, Var tmp))
                              (BinOp (translate op) nlo nro))])

  translate (A.Print {A.stringLit = s, A.args}) = do
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

  translate seq@(A.Seq {A.eseq}) = do
    ntes <- mapM translate eseq
    let (nes, tes) = unzip ntes
--    let comms = map (Comm . show . PP.ppExpr) eseq
--    map comment_and_te (zip eseq tes)
    return (last nes, Seq $ map comment_and_te (zip eseq tes))
           where
--             merge comms tes = concat $ zipWith (\(comm, te) -> (Seq [comm,te])) comms tes

             comment_for = (Comm . show . PP.ppExpr)

             comment_and_te (ast, te) = Seq [comment_for ast, te]

  translate (A.Assign {A.lhs = lhs@(A.ArrayAccess {A.target, A.index}), A.rhs}) = do
    (nrhs, trhs) <- if A.isThisAccess rhs && (Ty.isActiveRefType $ A.getType rhs)
                    then return (Deref (Var "this") `Dot` (Nam "aref"), Skip)
                    else translate rhs
    (ntarg, ttarg) <- translate target
    (nindex, tindex) <- translate index
    let ty = translate $ A.getType lhs
        the_set = 
            Statement $ 
            Call (Nam "array_set") 
                 [AsExpr ntarg, AsExpr nindex, as_encore_arg_t ty $ AsExpr nrhs]
    return (unit, Seq [trhs, ttarg, tindex, the_set])

  translate (A.Assign {A.lhs, A.rhs}) = do
    (nrhs, trhs) <- translate rhs
    cast_rhs <- case lhs of 
                  A.FieldAccess {A.name, A.target} -> 
                      do ctx <- get
                         let Just fld = lookup_field (Ctx.class_table ctx) (A.getType target) name
                         if Ty.isTypeVar (A.ftype fld) then
                             return $ as_encore_arg_t (translate . A.getType $ rhs) $ AsExpr nrhs
                         else
                             return $ AsExpr nrhs
                  _ -> return $ AsExpr nrhs

    lval <- mk_lval lhs
    return (unit, Seq [trhs, Assign lval cast_rhs])
        where
          mk_lval (A.VarAccess {A.name}) =
              do ctx <- get
                 case Ctx.subst_lkp ctx name of
                   Just subst_name -> return subst_name
                   Nothing -> return $ Var (show name)
          mk_lval (A.FieldAccess {A.target, A.name}) =
              do (ntarg, ttarg) <- translate target
                 return (Deref (StatAsExpr ntarg ttarg) `Dot` (Nam $ show name))
          mk_lval e = error $ "Cannot translate '" ++ (show e) ++ "' to a valid lval"

  translate (A.VarAccess {A.name}) = do
      c <- get
      case Ctx.subst_lkp c name of
        Just subst_name ->
            return (subst_name , Skip)
        Nothing ->
            return (Var $ show name, Skip)

  translate acc@(A.FieldAccess {A.target, A.name}) = do
    (ntarg,ttarg) <- translate target
    tmp <- Ctx.gen_named_sym "fieldacc"
    the_access <- do ctx <- get
                     let Just fld = lookup_field (Ctx.class_table ctx) (A.getType target) name
                     if Ty.isTypeVar (A.ftype fld) then
                         return $ from_encore_arg_t (translate . A.getType $ acc) $ AsExpr (Deref ntarg `Dot` (Nam $ show name))
                     else
                         return (Deref ntarg `Dot` (Nam $ show name))
    return (Var tmp, Seq [ttarg,
                      (Assign (Decl (translate (A.getType acc), Var tmp)) the_access)])

  translate (A.Let {A.decls, A.body}) = do
                     do
                       tmps_tdecls <- mapM translate_decl decls
                       let (tmps, tdecls) = unzip tmps_tdecls
                       (nbody, tbody) <- translate body
                       mapM_ unsubstitute_var (map fst decls)
                       return (nbody, Seq $ (concat tdecls) ++ [tbody])
                     where
                       translate_decl (name, expr) =
                           do (ne, te) <- translate expr
                              tmp <- Ctx.gen_named_sym (show name)
                              substitute_var name (Var tmp)
                              return $ (Var tmp
                                       , [ Comm ((show name) ++ " = " ++ (show $ PP.ppExpr expr))
                                         , te
                                         , Assign (Decl (translate (A.getType expr), Var tmp)) ne])

  translate (A.New {A.ty}) 
      | Ty.isActiveRefType ty = 
          named_tmp_var "new" ty $
                        Cast (Ptr . AsType $ class_type_name ty)
                        (Call (Nam "encore_create")
                              [Amp $ runtime_type_name ty])
      | otherwise = 
          do na <- Ctx.gen_named_sym "new"
             let size = Sizeof . AsType $ class_type_name ty
             return $ (Var na, Assign (Decl (translate ty, Var na))
                                      (Call (Nam "encore_alloc") [size]))

  translate (A.Peer {A.ty})
      | Ty.isActiveRefType ty =
          named_tmp_var "peer" ty $
                        Cast (Ptr . AsType $ class_type_name ty)
                        (Call (Nam "encore_peer_create")
                              [Amp $ runtime_type_name ty])

      | otherwise =
          error $  "can not have passive peer '"++show ty++"'"

  translate arrNew@(A.ArrayNew {A.ty, A.size}) =
      do arr_name <- Ctx.gen_named_sym "array"
         size_name <- Ctx.gen_named_sym "size"
         (nsize, tsize) <- translate size
         let the_array_decl = 
                 Assign (Decl (array, Var arr_name)) 
                        (Call (Nam "array_mk") [AsExpr nsize, runtime_type ty])
         return (Var arr_name, Seq [tsize, the_array_decl])

  translate arrAcc@(A.ArrayAccess {A.target, A.index}) =
      do (ntarg, ttarg) <- translate target
         (nindex, tindex) <- translate index
         access_name <- Ctx.gen_named_sym "access"
         let ty = translate $ A.getType arrAcc
             the_access = 
                 Assign (Decl (ty, Var access_name)) 
                        (Call (Nam "array_get") [ntarg, nindex]
                              `Dot` encore_arg_t_tag ty)
         return (Var access_name, Seq [ttarg, tindex, the_access])

  translate arrLit@(A.ArrayLiteral {A.args}) =
      do arr_name <- Ctx.gen_named_sym "array"
         targs <- mapM translate args
         let len = length args
             ty  = Ty.getResultType $ A.getType arrLit
             the_array_decl = 
                 Assign (Decl (array, Var arr_name)) 
                        (Call (Nam "array_mk") [Int len, runtime_type ty])
             the_array_content = Seq $ map (\(_, targ) -> targ) targs
             the_array_sets =
                 let (_, sets) = mapAccumL (array_set arr_name ty) 0 targs
                 in sets
         return (Var arr_name, Seq $ the_array_decl : the_array_content : the_array_sets)
      where
        array_set arr_name ty index (narg, _) = 
            (index + 1, 
             Statement $ Call (Nam "array_set") 
                              [AsExpr $ Var arr_name, 
                               Int index, 
                               as_encore_arg_t (translate ty) $ AsExpr narg])

  translate arrSize@(A.ArraySize {A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.gen_named_sym "size"
         let the_size = Assign (Decl (int, Var tmp)) 
                               (Call (Nam "array_size") [ntarg])
         return (Var tmp, Seq [ttarg, the_size])

  translate call@(A.MethodCall { A.target=target, A.name=name, A.args=args }) 
      | (A.isThisAccess target) ||
        (Ty.isPassiveRefType . A.getType) target = sync_call
      | otherwise = remote_call
          where
            sync_call =
                do (ntarget, ttarget) <- translate target
                   tmp <- Ctx.gen_named_sym "synccall"
                   targs <- mapM translate args
                   let targs_types = map (translate . A.getType) args
                   ctx <- get
                   let Just mtd = lookup_method (Ctx.class_table ctx) (A.getType target) name
                   let expected_types = map A.ptype (A.mparams mtd)
                   let (arg_names, arg_decls) = unzip targs
                   let casted_arguments = zipWith3 cast_arguments expected_types arg_names targs_types
                       the_call = if Ty.isTypeVar (A.mtype mtd) then 
                                      AsExpr $ from_encore_arg_t (translate (A.getType call))
                                                 (Call (method_impl_name (A.getType target) name)
                                                       (AsExpr ntarget : casted_arguments))
                                  else
                                      (Call (method_impl_name (A.getType target) name)
                                            (AsExpr ntarget : casted_arguments))
                       the_assign = Assign (Decl (translate (A.getType call), Var tmp)) the_call
                   return (Var tmp, Seq $ ttarget :
                                          arg_decls ++
                                          [the_assign])

            remote_call :: State Ctx.Context (CCode Lval, CCode Stat)
            remote_call =
                do (ntarget, ttarget) <- translate target
                   targs <- mapM translate args
                   the_fut_name <- if Ty.isStreamType $ A.getType call then
                                       Ctx.gen_named_sym "stream"
                                   else
                                       Ctx.gen_named_sym "fut"
                   let (arg_names, arg_decls) = unzip targs
                       the_fut_decl =
                           if Ty.isStreamType $ A.getType call then
                               Assign (Decl (Ptr $ Typ "stream_t", Var the_fut_name))
                                      (Call (Nam "stream_mk") ([] :: [CCode Expr]))
                           else
                               Assign (Decl (Ptr $ Typ "future_t", Var the_fut_name))
                                      (Call (Nam "future_mk") ([runtime_type . Ty.getResultType . A.getType $ call]))
                   the_arg_name <- Ctx.gen_named_sym "arg"
                   let no_args = length args
                   let the_arg_ty = Ptr . AsType $ fut_msg_type_name (A.getType target) name
                   let the_arg_decl = Assign (Decl (the_arg_ty, Var the_arg_name)) (Cast the_arg_ty (Call (Nam "pony_alloc_msg") [Int (calc_pool_size_for_msg (no_args + 1)), AsExpr $ AsLval $ fut_msg_id (A.getType target) name]))
                   let targs_types = map (translate . A.getType) args
                   ctx <- get
                   let Just mtd = lookup_method (Ctx.class_table ctx) (A.getType target) name
                   let expected_types = map A.ptype (A.mparams mtd)
                   let casted_arguments = zipWith3 cast_arguments expected_types arg_names targs_types
                   let arg_assignments = zipWith (\i tmp_expr -> Assign ((Var the_arg_name) `Arrow` (Nam $ "f"++show i)) tmp_expr) [1..no_args] casted_arguments

                   let args_types = zip (map (\i -> (Arrow (Var the_arg_name) (Nam $ "f"++show i))) [1..no_args]) (map A.getType args)
                   let install_future = Assign (Arrow (Var the_arg_name) (Nam "_fut")) (Var the_fut_name)
                   let the_arg_init = Seq $ (map Statement arg_assignments) ++ [install_future]
                   the_call <- return (Call (Nam "pony_sendv")
                                               [Cast (Ptr pony_actor_t) $ AsExpr ntarget,
                                                Cast (Ptr pony_msg_t) $ AsExpr $ Var the_arg_name])
                   return (Var the_fut_name,
                           Seq $ ttarget :
                                 arg_decls ++
                                 [the_fut_decl,
                                  the_arg_decl,
                                  the_arg_init] ++
                                  gc_send args_types expected_types (Statement $ Call (Nam "pony_traceobject") [Var the_fut_name, future_type_rec_name `Dot` Nam "trace"]) ++
                                 [Statement the_call])
            cast_arguments expected targ targ_type
                | Ty.isTypeVar expected = as_encore_arg_t targ_type $ AsExpr targ
                | otherwise = AsExpr targ

  translate (A.MessageSend { A.target, A.name, A.args })
      | (Ty.isActiveRefType . A.getType) target = message_send
      | otherwise = error "Tried to send a message to something that was not an active reference"
          where
            message_send :: State Ctx.Context (CCode Lval, CCode Stat)
            message_send =
                do (ntarg, ttarg) <- translate target
                   targs <- mapM translate args
                   the_msg_name <- Ctx.gen_named_sym "arg"
                   let (arg_names, arg_decls) = unzip targs
                       the_msg_ty = Ptr . AsType $ one_way_msg_type_name (A.getType target) name
                       no_args = length args
                       targs_types = map (translate . A.getType) args
                   ctx <- get
                   let Just mtd = lookup_method (Ctx.class_table ctx) (A.getType target) name
                       expected_types = map A.ptype (A.mparams mtd)
                       casted_arguments = zipWith3 cast_arguments expected_types arg_names targs_types
                       arg_assignments = zipWith (\i tmp_expr -> Assign (Arrow (Var the_msg_name) (Nam $ "f"++show i)) tmp_expr) [1..no_args] casted_arguments
                       the_arg_init = Seq $ map Statement arg_assignments
                       the_call = Call (Nam "pony_sendv")
                                       [Cast (Ptr pony_actor_t) ntarg,
                                        Cast (Ptr pony_msg_t) $ AsExpr $ Var the_msg_name]
                       the_msg_decl = Assign (Decl (the_msg_ty, Var the_msg_name)) (Cast the_msg_ty $ Call (Nam "pony_alloc_msg") [Int (calc_pool_size_for_msg no_args), AsExpr . AsLval $ one_way_msg_id (A.getType target) name])
                       args_types = zip (map (\i -> (Arrow (Var the_msg_name) (Nam $ "f" ++ show i))) [1..no_args]) (map A.getType args)
                   return (unit,
                           Seq ((Comm "message send") :
                                ttarg :
                                arg_decls ++
                                the_msg_decl :
                                the_arg_init :
                                gc_send args_types expected_types (Comm "Not tracing the future in a one_way send") ++
                                [Statement the_call]))
            cast_arguments expected targ targ_type
                | Ty.isTypeVar expected = as_encore_arg_t targ_type $ AsExpr targ
                | otherwise = AsExpr targ

  translate w@(A.While {A.cond, A.body}) =
      do (ncond,tcond) <- translate cond
         (nbody,tbody) <- translate body
         tmp <- Ctx.gen_named_sym "while";
         let export_body = Seq $ tbody : [Assign (Var tmp) nbody]
         return (Var tmp,
                 Seq [Statement $ Decl ((translate (A.getType w)), Var tmp),
                      (While (StatAsExpr ncond tcond) (Statement export_body))])

  translate ite@(A.IfThenElse { A.cond, A.thn, A.els }) =
      do tmp <- Ctx.gen_named_sym "ite"
         (ncond, tcond) <- translate cond
         (nthn, tthn) <- translate thn
         (nels, tels) <- translate els
         let export_thn = Seq $ tthn : [Assign (Var tmp) nthn]
             export_els = Seq $ tels : [Assign (Var tmp) nels]
         return (Var tmp,
                 Seq [AsExpr $ Decl (translate (A.getType ite), Var tmp),
                      If (StatAsExpr ncond tcond) (Statement export_thn) (Statement export_els)])

  translate e@(A.Embed {A.code=code}) = do
    interpolated <- interpolate code
    if Ty.isVoidType (A.getType e) then
        return (unit, Embed $ "({" ++ interpolated  ++ "})")
    else
        named_tmp_var "embed" (A.getType e) (Embed $ "({" ++ interpolated  ++ "})")
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

  translate get@(A.Get{A.val})
    | Ty.isFutureType $ A.getType val =
        do (nval, tval) <- translate val
           let result_type = translate (Ty.getResultType $ A.getType val)
               the_get = from_encore_arg_t result_type (Call (Nam "future_get_actor") [nval])
           tmp <- Ctx.gen_sym
           return (Var tmp, Seq [tval, Assign (Decl (result_type, Var tmp)) the_get])
    | Ty.isStreamType $ A.getType val =
        do (nval, tval) <- translate val
           let result_type = translate (Ty.getResultType $ A.getType val)
               the_get = from_encore_arg_t result_type (Call (Nam "stream_get") [nval])
           tmp <- Ctx.gen_sym
           return (Var tmp, Seq [tval, Assign (Decl (result_type, Var tmp)) the_get])
    | otherwise = error $ "Cannot translate get of " ++ show val


  translate yield@(A.Yield{A.val}) =
      do (nval, tval) <- translate val
         tmp <- Ctx.gen_sym
         let yield_arg = as_encore_arg_t (translate (A.getType val)) nval
             tmp_stream = Assign (Decl (stream, Var tmp)) stream_handle
             update_stream = Assign (stream_handle) (Call (Nam "stream_put")
                                                          [AsExpr stream_handle, yield_arg, runtime_type $ A.getType val])
         return (unit, Seq [tval, tmp_stream, update_stream])

  translate eos@(A.Eos{}) =
      let eos_call = Call (Nam "stream_close") [stream_handle]
      in return (unit, Statement eos_call)

  translate iseos@(A.IsEos{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.gen_sym
         let result_type = translate (A.getType target)
             the_call = Assign (Decl (bool, Var tmp)) (Call (Nam "stream_eos") [ntarg])
         return (Var tmp, Seq [ttarg, the_call])

  translate next@(A.StreamNext{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.gen_sym
         let the_call = Assign (Decl (stream, Var tmp)) (Call (Nam "stream_get_next") [ntarg])
         return (Var tmp, Seq [ttarg, the_call])

  translate await@(A.Await{A.val}) =
      do (nval, tval) <- translate val
         return (unit, Seq [tval, Statement $ Call (Nam "future_await") [nval]])

  translate suspend@(A.Suspend{}) = 
         return (unit, Seq [Call (Nam "future_suspend") ([] :: [CCode Expr])]) --TODO: Call should support 0-arity 

  translate futureChain@(A.FutureChain{A.future, A.chain}) =
      do (nfuture,tfuture) <- translate future
         (nchain, tchain)  <- translate chain
         fut_name <- Ctx.gen_sym
         let ty = runtime_type . Ty.getResultType . A.getType $ chain
             fut_decl = Assign (Decl (Ptr $ Typ "future_t", Var fut_name))
                               (Call (Nam "future_mk") [ty])
         result <- Ctx.gen_sym
         return $ (Var result, Seq [tfuture,
                                    tchain,
                                    fut_decl,
                                    (Assign (Decl (Ptr $ Typ "future_t", Var result)) (Call (Nam "future_chain_actor") [nfuture, (Var fut_name), nchain]))])

  translate clos@(A.Closure{A.eparams, A.body}) =
      do let meta_id    = Meta.getMetaId . A.getMeta $ clos
             fun_name   = closure_fun_name meta_id
             env_name   = closure_env_name meta_id
             trace_name = closure_trace_name meta_id
             free_vars  = Util.freeVariables (map A.pname eparams) body
         tmp <- Ctx.gen_sym
         fill_env <- mapM (insert_var env_name) free_vars
         return $ (Var tmp, Seq $ (mk_env env_name) : fill_env ++
                           [Assign (Decl (closure, Var tmp))
                                       (Call (Nam "closure_mk") [fun_name, env_name, trace_name])])
      where
        mk_env name =
            Assign (Decl (Ptr $ Struct name, AsLval name))
                    (Call (Nam "encore_alloc")
                          [Sizeof $ Struct name])
        insert_var env_name (name, _) =
            do c <- get
               let tname = case Ctx.subst_lkp c name of
                              Just subst_name -> subst_name
                              Nothing -> Var $ show name
               return $ Assign ((Deref $ Var $ show env_name) `Dot` (Nam $ show name)) tname

  translate fcall@(A.FunctionCall{A.name, A.args}) = do
    c <- get
    let clos = Var (case Ctx.subst_lkp c name of
                      Just subst_name -> show subst_name
                      Nothing -> show name)
    let ty = A.getType fcall
    targs <- mapM translateArgument args
    (tmp_args, tmp_arg_decl) <- tmp_arr (Typ "value_t") targs
    (calln, the_call) <- named_tmp_var "clos" ty $ AsExpr $ from_encore_arg_t (translate ty) (Call (Nam "closure_call") [clos, tmp_args])
    let comment = Comm ("fcall name: " ++ show name ++ " (" ++ show (Ctx.subst_lkp c name) ++ ")")
    return (if Ty.isVoidType ty then unit else calln, Seq [comment, tmp_arg_decl, the_call])
        where 
          translateArgument arg =
            do (ntother, tother) <- translate arg
               return $ as_encore_arg_t (translate $ A.getType arg) (StatAsExpr ntother tother) 

  translate other = error $ "Expr.hs: can't translate: '" ++ show other ++ "'"

gc_send as expected_types fut_trace = 
    [Embed $ "", 
     Embed $ "// --- GC on sending ----------------------------------------",
     Statement $ Call (Nam "pony_gc_send") ([] :: [CCode Expr]),
     fut_trace] ++
     (zipWith tracefun_call as expected_types) ++
    [Statement $ Call (Nam "pony_send_done") ([] :: [CCode Expr]),
     Embed $ "// --- GC on sending ----------------------------------------",
     Embed $ ""]

tracefun_call (a, t) expected_type
    | Ty.isActiveRefType  t = Statement $ Call (Nam "pony_traceactor")  
                              [Cast (Ptr pony_actor_t) (wrap a)]
    | Ty.isPassiveRefType t = Statement $ Call (Nam "pony_traceobject") [wrap a, AsLval $ class_trace_fn_name t]
    | Ty.isFutureType     t = Statement $ Call (Nam "pony_traceobject") [a, future_type_rec_name `Dot` Nam "trace"]
    | Ty.isArrowType      t = Statement $ Call (Nam "pony_traceobject") [a, AsLval $ Nam "closure_trace"]
    | Ty.isArrayType      t = Statement $ Call (Nam "pony_traceobject") [a, AsLval $ Nam "array_trace"]
    | Ty.isStreamType     t = Statement $ Call (Nam "pony_traceobject") [a, AsLval $ Nam "scons_trace"]
    | otherwise             = Embed $ "/* Not tracing '" ++ show a ++ "' */"
    where
      wrap 
          | Ty.isTypeVar expected_type = from_encore_arg_t (translate t) . AsExpr
          | otherwise = id
--TODO: add cases for future type, closure etc.  

-- Note: the 2 is for the 16 bytes of payload in pony_msg_t
-- If the size of this struct changes, so must this calculation
calc_pool_size_for_msg args = (args + 2) `div` 8

