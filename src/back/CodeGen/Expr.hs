{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs, FlexibleContexts #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Type
import CodeGen.Trace (traceVariable)
import qualified CodeGen.Context as Ctx

import qualified Parser.Parser as P -- for string interpolation in the embed expr
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as PString

import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta
import qualified AST.PrettyPrinter as PP
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding (void)
import Control.Applicative((<$>))
import Data.List

instance Translatable ID.BinaryOp (CCode Name) where
  translate op = Nam $ case op of
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

instance Translatable ID.UnaryOp (CCode Name) where
  translate op = Nam $ case op of
    ID.NOT -> "!"

typeToPrintfFstr :: Ty.Type -> String
typeToPrintfFstr ty
    | Ty.isIntType ty    = "%lli"
    | Ty.isRealType ty   = "%f"
    | Ty.isStringType ty = "%s"
    | Ty.isBoolType ty   = "bool<%zd>"
    | Ty.isRefType ty    = show ty ++ "<%p>"
    | Ty.isFutureType ty = "fut<%p>"
    | otherwise = case translate ty of
                    Ptr something -> "%p"
                    _ -> "Expr.hs: typeToPrintfFstr not defined for " ++ show ty

-- | If the type is not void, create a variable to store it in. If it is void, return the lval UNIT
namedTmpVar :: String -> Ty.Type -> CCode Expr -> State Ctx.Context (CCode Lval, CCode Stat)
namedTmpVar name ty cex
    | Ty.isVoidType ty = return $ (unit, Seq [cex])
    | otherwise     = do na <- Ctx.genNamedSym name
                         return $ (Var na, Assign (Decl (translate ty, Var na)) cex)

tmpArr :: CCode Ty -> [CCode Expr] -> State Ctx.Context (CCode Lval, CCode Stat)
tmpArr cty arr = do
   na <- Ctx.genSym
   return $ (Var na, Assign (Decl (cty, Var $ na ++ "[]")) (Record arr))

substituteVar :: ID.Name -> CCode Lval -> State Ctx.Context ()
substituteVar na impl = do
  c <- get
  put $ Ctx.substAdd c na impl
  return ()

unsubstituteVar :: ID.Name -> State Ctx.Context ()
unsubstituteVar na = do
  c <- get
  put $ Ctx.substRem c na
  return ()


-- these two are exclusively used for A.Embed translation:
type ParsedEmbed = [Either String VarLkp]
newtype VarLkp = VarLkp String

instance Translatable A.Expr (State Ctx.Context (CCode Lval, CCode Stat)) where
  -- | Translate an expression into the corresponding C code
  translate skip@(A.Skip {}) = namedTmpVar "skip" (A.getType skip) (AsExpr unit)
  translate breathe@(A.Breathe {}) =
    namedTmpVar "breathe"
                (A.getType breathe)
                (Call (Nam "call_respond_with_current_scheduler") ([] :: [CCode Expr]))
  translate null@(A.Null {}) = namedTmpVar "literal" (A.getType null) Null
  translate true@(A.BTrue {}) = namedTmpVar "literal"  (A.getType true) (Embed "1/*True*/"::CCode Expr)
  translate false@(A.BFalse {}) = namedTmpVar "literal" (A.getType false) (Embed "0/*False*/"::CCode Expr)
  translate lit@(A.IntLiteral {A.intLit = i}) = namedTmpVar "literal" (A.getType lit) (Int i)
  translate lit@(A.RealLiteral {A.realLit = r}) = namedTmpVar "literal" (A.getType lit) (Double r)
  translate lit@(A.StringLiteral {A.stringLit = s}) = namedTmpVar "literal" (A.getType lit) (String s)

  translate tye@(A.TypedExpr {A.body}) = do
    (nbody, tbody) <- translate body
    tmp <- Ctx.genNamedSym "cast"
    let ty = translate (A.getType tye)
        theCast = Assign (Decl (ty, Var tmp))
                         (Cast ty nbody)
    return $ (Var tmp,
              Seq [tbody, theCast])

  translate unary@(A.Unary {A.uop, A.operand}) = do
    (noperand, toperand) <- translate operand
    tmp <- Ctx.genNamedSym "unary"
    return $ (Var tmp,
              Seq [toperand,
                   Statement (Assign
                              (Decl (translate $ A.getType unary, Var tmp))
                              (CUnary (translate uop) noperand))])

  translate bin@(A.Binop {A.binop, A.loper, A.roper}) = do
    (nlo, tlo) <- translate (loper :: A.Expr)
    (nro, tro) <- translate (roper :: A.Expr)
    let ltype = A.getType loper
        rcast = if Ty.isRefType ltype
                then Cast (translate ltype) nro
                else AsExpr nro
    tmp <- Ctx.genNamedSym "binop"
    return $ (Var tmp,
              Seq [tlo,
                   tro,
                   Statement (Assign
                              (Decl (translate $ A.getType bin, Var tmp))
                              (BinOp (translate binop) (AsExpr nlo) rcast))])

  translate (A.Print {A.stringLit = s, A.args}) = do
      targs <- mapM translate args
      let argNames = map (AsExpr . fst) targs
      let argDecls = map snd targs
      let argTys   = map A.getType args
      let fstring = formatString s argTys
      return $ (unit,
                Seq $ argDecls ++
                     [Statement
                      (Call (Nam "printf")
                       ((String fstring) : argNames))])
      where
        formatString s [] = s
        formatString "" (ty:tys) = error "Wrong number of arguments to printf"
        formatString ('{':'}':s) (ty:tys) = (typeToPrintfFstr ty) ++ (formatString s tys)
        formatString (c:s) tys = c : (formatString s tys)

  translate exit@(A.Exit {A.args = [arg]}) = do
      (narg, targ) <- translate arg
      let exitCall = Call (Nam "exit") [narg]
      return (unit, Seq [Statement targ, Statement exitCall])

  translate seq@(A.Seq {A.eseq}) = do
    ntes <- mapM translate eseq
    let (nes, tes) = unzip ntes
--    let comms = map (Comm . show . PP.ppExpr) eseq
--    map commentAndTe (zip eseq tes)
    return (last nes, Seq $ map commentAndTe (zip eseq tes))
           where
--             merge comms tes = concat $ zipWith (\(comm, te) -> (Seq [comm,te])) comms tes

             commentFor = (Comm . show . PP.ppExpr)

             commentAndTe (ast, te) = Seq [commentFor ast, te]

  translate (A.Assign {A.lhs = lhs@(A.ArrayAccess {A.target, A.index}), A.rhs}) = do
    (nrhs, trhs) <- translate rhs
    (ntarg, ttarg) <- translate target
    (nindex, tindex) <- translate index
    let ty = translate $ A.getType lhs
        theSet =
           Statement $
           Call (Nam "array_set")
                [AsExpr ntarg, AsExpr nindex, asEncoreArgT ty $ AsExpr nrhs]
    return (unit, Seq [trhs, ttarg, tindex, theSet])

  translate (A.Assign {A.lhs, A.rhs}) = do
    (nrhs, trhs) <- translate rhs
    castRhs <- case lhs of
                 A.FieldAccess {A.name, A.target} ->
                     do fld <- gets $ Ctx.lookupField (A.getType target) name
                        if Ty.isTypeVar (A.ftype fld) then
                            return $ asEncoreArgT (translate . A.getType $ rhs) $ AsExpr nrhs
                        else
                            return $ upCast nrhs
                 _ -> return $ upCast nrhs

    lval <- mkLval lhs
    return (unit, Seq [trhs, Assign lval castRhs])
      where
        upCast e = if needUpCast then cast e else AsExpr e
          where
            lhsType = A.getType lhs
            rhsType = A.getType rhs
            needUpCast = rhsType /= lhsType
            cast = Cast (translate lhsType)
        mkLval (A.VarAccess {A.name}) =
           do ctx <- get
              case Ctx.substLkp ctx name of
                Just substName -> return substName
                Nothing -> return $ Var (show name)
        mkLval (A.FieldAccess {A.target, A.name}) =
           do (ntarg, ttarg) <- translate target
              return (Deref (StatAsExpr ntarg ttarg) `Dot` (fieldName name))
        mkLval e = error $ "Cannot translate '" ++ (show e) ++ "' to a valid lval"

  translate maybe@(A.MaybeValue _ (A.JustData e)) = do
    let createOption = Call (Nam "encore_alloc") [(Sizeof . AsType) (Nam "option_t")]
    (nalloc, talloc) <- namedTmpVar "option" (A.getType maybe) createOption
    let tag = Assign (Deref nalloc `Dot` (Nam "tag")) (Nam "JUST")
    (nE, tE) <- translate e
    let tJust = Assign (Deref nalloc `Dot` (Nam "val")) (asEncoreArgT (translate $ A.getType e) nE)
    return (nalloc, Seq [talloc, tag, tE, tJust])

  translate maybe@(A.MaybeValue _ (A.NothingData {})) = do
    let createOption = Amp (Nam "DEFAULT_NOTHING")
    (nalloc, talloc) <- namedTmpVar "option" (A.getType maybe) createOption
    return (nalloc, talloc)

  translate match@(A.MatchDecl {A.arg, A.matchbody}) = do
    (nArg, tArg) <- translate arg
    (resultVar, resultType) <- getResult match
    eMatch <- mapM (translateMatch (AsExpr nArg)) matchbody
    return (resultVar,
            Seq $ [Statement $ Decl (resultType, resultVar),
                   tArg,
                   Statement (convertToIf resultVar eMatch)])
    where
      getResult :: A.Expr -> State Ctx.Context (CCode Lval, CCode Ty)
      getResult match = do
        resultVarName <- Ctx.genNamedSym "match"
        let resultVar = Var resultVarName
            matchType = translate (A.getType match)
        return (resultVar, matchType)

      convertToIf :: CCode Lval -> [(CCode Expr, Maybe (CCode Lval, CCode Expr), (CCode Lval, CCode Stat))] -> CCode Expr
      convertToIf resultVar [] = StatAsExpr resultVar (Assign resultVar (Int 0))
      convertToIf resultVar ((cond, binding, body):rest) =
        let tBody = snd body
            nBody = fst body
            declBinding = case binding of
                            Nothing -> Comm "Nothing"
                            Just b -> Assign (fst b) (snd b)
        in
          If cond
            (Seq [declBinding, tBody, Assign resultVar nBody])
            (Statement $ convertToIf resultVar rest)

      translateMatch :: CCode Expr -> (A.Expr, A.Expr) -> State Ctx.Context (CCode Expr, Maybe (CCode Lval, CCode Expr), (CCode Lval, CCode Stat))
      translateMatch nArg (patternMatch, body)
        | null freeVars = do
            tCond <- translateCond nArg patternMatch
            tBody <- translate body
            return (tCond, Nothing, tBody)
        | otherwise = do
            let (freeVarName, freeVarType) = head freeVars
            tCond <- translateCond nArg patternMatch

            (tmpVar, tBinding) <- (do tmpVar <- getTmpVars freeVarName
                                      return (tmpVar, translateBind nArg patternMatch))
            let lhs = Decl (translate freeVarType, tmpVar)
            tBody <- evalWithContext (freeVarName, tmpVar) body
            return (tCond, Just (lhs, tBinding), tBody)
        where
          freeVars = Util.freeVariables [] patternMatch

          evalWithContext (freeVarName, tmpVar) body = do
            substituteVar freeVarName tmpVar
            tBody <- translate body
            unsubstituteVar freeVarName
            return tBody

      getTmpVars :: ID.Name -> State Ctx.Context (CCode Lval)
      getTmpVars name = do
        name' <- (Ctx.genNamedSym . show) name
        return (Var name')

      createCond :: A.Expr -> CCode Expr -> CCode Expr -> State Ctx.Context (CCode Expr)
      createCond e cond cast
        | A.isLval e = return cond
        | Ty.isMaybeType (A.getType e) || Ty.isPrimitive (A.getType e) = do
            transCond <- translateCond cast e
            return $ BinOp (Nam "&&") cond transCond
        | otherwise = return cond

      translateCond :: CCode Expr -> A.Expr -> State Ctx.Context (CCode Expr)
      translateCond nArg p@(A.MaybeValue meta (A.JustData e)) = do
        let cond = BinOp (Nam "==") (nArg `Arrow` Nam "tag") (AsLval $ Nam "JUST")
            expr = fromEncoreArgT (translate $ A.getType e) (AsExpr $ nArg `Arrow` Nam "val")
            cast = Cast option expr
            ext = if (Ty.isPrimitive (A.getType e)) then (AsExpr expr) else cast
        resultCond <- createCond e cond ext
        return resultCond

      translateCond nArg p@(A.MaybeValue meta (A.NothingData {})) =
        return $ BinOp (Nam "==") (nArg `Arrow` (Nam "tag")) (AsLval $ Nam "NOTHING")

      translateCond nArg v
        | A.isLval v = return nArg
        | Ty.isPrimitive (A.getType v) = do
          (nBody, tBody) <- translate v
          return $ BinOp (Nam "==") nArg (StatAsExpr nBody tBody)

      -- TODO: what happens with primitive types
      translateBind nArg p@(A.MaybeValue meta (A.JustData e)) =
        let cast = AsExpr $ fromEncoreArgT (translate (A.getType e)) (Deref nArg)
            containedType = A.getType e
        in
          if (Ty.isMaybeType containedType) then
            translateBind cast e
          else
            AsExpr $ fromEncoreArgT (translate containedType) (Deref (Cast option nArg))
      translateBind nArg v@(A.VarAccess {}) = nArg

  translate (A.VarAccess {A.name}) = do
      c <- get
      case Ctx.substLkp c name of
        Just substName ->
            return (substName , Skip)
        Nothing ->
            return (Var . show $ globalClosureName name, Skip)

  translate acc@(A.FieldAccess {A.target, A.name}) = do
    (ntarg,ttarg) <- translate target
    tmp <- Ctx.genNamedSym "fieldacc"
    theAccess <- do fld <- gets $ Ctx.lookupField (A.getType target) name
                    if Ty.isTypeVar (A.ftype fld) then
                        return $ fromEncoreArgT (translate . A.getType $ acc) $ AsExpr (Deref ntarg `Dot` (fieldName name))
                    else
                        return (Deref ntarg `Dot` (fieldName name))
    return (Var tmp, Seq [ttarg,
                      (Assign (Decl (translate (A.getType acc), Var tmp)) theAccess)])

  translate (A.Let {A.decls, A.body}) = do
                     do
                       tmpsTdecls <- mapM translateDecl decls
                       let (tmps, tdecls) = unzip tmpsTdecls
                       (nbody, tbody) <- translate body
                       mapM_ unsubstituteVar (map fst decls)
                       return (nbody, Seq $ (concat tdecls) ++ [tbody])
                     where
                       translateDecl (name, expr) =
                           do (ne, te) <- translate expr
                              tmp <- Ctx.genNamedSym (show name)
                              substituteVar name (Var tmp)
                              return $ (Var tmp
                                       , [ Comm ((show name) ++ " = " ++ (show $ PP.ppExpr expr))
                                         , te
                                         , Assign (Decl (translate (A.getType expr), Var tmp)) ne])

  translate (A.New {A.ty})
      | Ty.isActiveClassType ty =
          do (nnew, tnew) <- namedTmpVar "new" ty $ Cast (Ptr . AsType $ classTypeName ty)
                                                    (Call (Nam "encore_create")
                                                    [Amp $ runtimeTypeName ty])
             let typeParams = Ty.getTypeParameters ty
                 typeParamInit = Call (runtimeTypeInitFnName ty) (AsExpr nnew : map runtimeType typeParams)
             return (nnew, Seq [tnew, Statement typeParamInit])
      | Ty.isSharedClassType ty =
          let
            fName = constructorImplName ty
            args = [] :: [CCode Expr]
            call = Call fName args
          in
            namedTmpVar "new" ty call
      | otherwise =
          do na <- Ctx.genNamedSym "new"
             let size = Sizeof . AsType $ classTypeName ty
                 theNew = Assign (Decl (translate ty, Var na))
                                 (Call (Nam "encore_alloc") [size])
                 typeParams = Ty.getTypeParameters ty
                 init = [Assign (Var na `Arrow` selfTypeField) (Amp $ runtimeTypeName ty),
                         Statement $ Call (runtimeTypeInitFnName ty) (AsExpr (Var na) : map runtimeType typeParams)]
             return $ (Var na, Seq $ theNew : init)

  translate (A.Peer {A.ty})
      | Ty.isActiveClassType ty =
          namedTmpVar "peer" ty $
                      Cast (Ptr . AsType $ classTypeName ty)
                      (Call (Nam "encore_peer_create")
                            [Amp $ runtimeTypeName ty])

      | otherwise =
          error $  "can not have passive peer '"++show ty++"'"

  translate arrNew@(A.ArrayNew {A.ty, A.size}) =
      do arrName <- Ctx.genNamedSym "array"
         sizeName <- Ctx.genNamedSym "size"
         (nsize, tsize) <- translate size
         let theArrayDecl =
                Assign (Decl (array, Var arrName))
                       (Call (Nam "array_mk") [AsExpr nsize, runtimeType ty])
         return (Var arrName, Seq [tsize, theArrayDecl])

  translate rangeLit@(A.RangeLiteral {A.start = start, A.stop = stop, A.step = step}) = do
      (nstart, tstart) <- translate start
      (nstop, tstop)   <- translate stop
      (nstep, tstep)   <- translate step
      rangeLiteral    <- Ctx.genNamedSym "range_literal"
      let ty = translate $ A.getType rangeLit
      return (Var rangeLiteral, Seq [tstart, tstop, tstep,
                                Assign (Decl (ty, Var rangeLiteral))
                                       (Call (Nam "range_mk") [nstart, nstop, nstep])])

  translate arrAcc@(A.ArrayAccess {A.target, A.index}) =
      do (ntarg, ttarg) <- translate target
         (nindex, tindex) <- translate index
         accessName <- Ctx.genNamedSym "access"
         let ty = translate $ A.getType arrAcc
             theAccess =
                Assign (Decl (ty, Var accessName))
                       (Call (Nam "array_get") [ntarg, nindex]
                             `Dot` encoreArgTTag ty)
         return (Var accessName, Seq [ttarg, tindex, theAccess])

  translate arrLit@(A.ArrayLiteral {A.args}) =
      do arrName <- Ctx.genNamedSym "array"
         targs <- mapM translate args
         let len = length args
             ty  = Ty.getResultType $ A.getType arrLit
             theArrayDecl =
                Assign (Decl (array, Var arrName))
                       (Call (Nam "array_mk") [Int len, runtimeType ty])
             theArrayContent = Seq $ map (\(_, targ) -> targ) targs
             theArraySets =
                let (_, sets) = mapAccumL (arraySet arrName ty) 0 targs
                in sets
         return (Var arrName, Seq $ theArrayDecl : theArrayContent : theArraySets)
      where
        arraySet arrName ty index (narg, _) =
            (index + 1,
             Statement $ Call (Nam "array_set")
                              [AsExpr $ Var arrName,
                               Int index,
                               asEncoreArgT (translate ty) $ AsExpr narg])

  translate arrSize@(A.ArraySize {A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genNamedSym "size"
         let theSize = Assign (Decl (int, Var tmp))
                              (Call (Nam "array_size") [ntarg])
         return (Var tmp, Seq [ttarg, theSize])

  translate call@(A.MethodCall { A.target=target, A.name=name, A.args=args })
      | (Ty.isTraitType . A.getType) target = traitMethod call
      | syncAccess = syncCall
      | sharedAccess = sharedObjectMethodFut call
      | otherwise = remoteCall
          where
            syncAccess = (A.isThisAccess target)
              || (Ty.isPassiveClassType . A.getType) target
            sharedAccess = Ty.isSharedClassType $ A.getType target
            syncCall =
                do (ntarget, ttarget) <- translate target
                   tmp <- Ctx.genNamedSym "synccall"
                   targs <- mapM translate args
                   mtd <- gets $ Ctx.lookupMethod (A.getType target) name
                   let targsTypes = map A.getType args
                       expectedTypes = map A.ptype (A.mparams mtd)
                       (argNames, argDecls) = unzip targs
                       castedArguments = zipWith3 castArguments expectedTypes argNames targsTypes
                       theCall = if Ty.isTypeVar (A.mtype mtd) then
                                     AsExpr $ fromEncoreArgT (translate (A.getType call))
                                                (Call (methodImplName (A.getType target) name)
                                                      (AsExpr ntarget : castedArguments))
                                 else
                                     (Call (methodImplName (A.getType target) name)
                                           (AsExpr ntarget : castedArguments))
                       theAssign = Assign (Decl (translate (A.getType call), Var tmp)) theCall
                   return (Var tmp, Seq $ ttarget :
                                          argDecls ++
                                          [theAssign])

            remoteCall :: State Ctx.Context (CCode Lval, CCode Stat)
            remoteCall =
                do (ntarget, ttarget) <- translate target
                   targs <- mapM translate args
                   theFutName <- if Ty.isStreamType $ A.getType call then
                                     Ctx.genNamedSym "stream"
                                 else
                                     Ctx.genNamedSym "fut"
                   let (argNames, argDecls) = unzip targs
                       theFutDecl =
                          if Ty.isStreamType $ A.getType call then
                              Assign (Decl (Ptr $ Typ "stream_t", Var theFutName))
                                     (Call (Nam "stream_mk") ([] :: [CCode Expr]))
                          else
                              Assign (Decl (Ptr $ Typ "future_t", Var theFutName))
                                     (Call (Nam "future_mk") ([runtimeType . Ty.getResultType . A.getType $ call]))
                       theFutTrace = if Ty.isStreamType $ A.getType call then
                                         (Statement $ Call (Nam "pony_traceobject")
                                                           [Var theFutName, AsLval streamTraceFn])
                                     else
                                         (Statement $ Call (Nam "pony_traceobject")
                                                           [Var theFutName, futureTypeRecName `Dot` Nam "trace"])
                   theArgName <- Ctx.genNamedSym "arg"
                   mtd <- gets $ Ctx.lookupMethod (A.getType target) name
                   let noArgs = length args
                       theArgTy = Ptr . AsType $ futMsgTypeName (A.getType target) name
                       theArgDecl = Assign (Decl (theArgTy, Var theArgName)) (Cast theArgTy (Call (Nam "pony_alloc_msg") [Int (calcPoolSizeForMsg (noArgs + 1)), AsExpr $ AsLval $ futMsgId (A.getType target) name]))
                       targsTypes = map A.getType args
                       expectedTypes = map A.ptype (A.mparams mtd)
                       castedArguments = zipWith3 castArguments expectedTypes argNames targsTypes
                       argAssignments = zipWith (\i tmpExpr -> Assign ((Var theArgName) `Arrow` (Nam $ "f"++show i)) tmpExpr) [1..noArgs] castedArguments

                       argsTypes = zip (map (\i -> (Arrow (Var theArgName) (Nam $ "f"++show i))) [1..noArgs]) (map A.getType args)
                       installFuture = Assign (Arrow (Var theArgName) (Nam "_fut")) (Var theFutName)
                       theArgInit = Seq $ (map Statement argAssignments) ++ [installFuture]
                   theCall <- return (Call (Nam "pony_sendv")
                                              [Cast (Ptr ponyActorT) $ AsExpr ntarget,
                                               Cast (Ptr ponyMsgT) $ AsExpr $ Var theArgName])
                   return (Var theFutName,
                           Seq $ ttarget :
                                 argDecls ++
                                 [theFutDecl,
                                  theArgDecl,
                                  theArgInit] ++
                                  gcSend argsTypes expectedTypes theFutTrace ++
                                 [Statement theCall])

  translate call@(A.MessageSend { A.target, A.name, A.args })
      | (Ty.isActiveClassType . A.getType) target = messageSend
      | sharedAccess = sharedObjectMethodOneWay call
      | otherwise = error "Tried to send a message to something that was not an active reference"
          where
            sharedAccess = Ty.isSharedClassType $ A.getType target
            messageSend :: State Ctx.Context (CCode Lval, CCode Stat)
            messageSend =
                do (ntarg, ttarg) <- translate target
                   targs <- mapM translate args
                   theMsgName <- Ctx.genNamedSym "arg"
                   mtd <- gets $ Ctx.lookupMethod (A.getType target) name
                   let (argNames, argDecls) = unzip targs
                       theMsgTy = Ptr . AsType $ oneWayMsgTypeName (A.getType target) name
                       noArgs = length args
                       targsTypes = map A.getType args
                       expectedTypes = map A.ptype (A.mparams mtd)
                       castedArguments = zipWith3 castArguments expectedTypes argNames targsTypes
                       argAssignments = zipWith (\i tmpExpr -> Assign (Arrow (Var theMsgName) (Nam $ "f"++show i)) tmpExpr) [1..noArgs] castedArguments
                       theArgInit = Seq $ map Statement argAssignments
                       theCall = Call (Nam "pony_sendv")
                                      [Cast (Ptr ponyActorT) ntarg,
                                       Cast (Ptr ponyMsgT) $ AsExpr $ Var theMsgName]
                       theMsgDecl = Assign (Decl (theMsgTy, Var theMsgName)) (Cast theMsgTy $ Call (Nam "pony_alloc_msg") [Int (calcPoolSizeForMsg noArgs), AsExpr . AsLval $ oneWayMsgId (A.getType target) name])
                       argsTypes = zip (map (\i -> (Arrow (Var theMsgName) (Nam $ "f" ++ show i))) [1..noArgs]) (map A.getType args)
                   return (unit,
                           Seq ((Comm "message send") :
                                ttarg :
                                argDecls ++
                                theMsgDecl :
                                theArgInit :
                                gcSend argsTypes expectedTypes (Comm "Not tracing the future in a oneWay send") ++
                                [Statement theCall]))

  translate w@(A.While {A.cond, A.body}) =
      do (ncond,tcond) <- translate cond
         (nbody,tbody) <- translate body
         tmp <- Ctx.genNamedSym "while";
         let exportBody = Seq $ tbody : [Assign (Var tmp) nbody]
         return (Var tmp,
                 Seq [Statement $ Decl ((translate (A.getType w)), Var tmp),
                      While (StatAsExpr ncond tcond) (Statement exportBody)])

  translate for@(A.For {A.name, A.step, A.src, A.body}) = do
    tmpVar   <- Var <$> Ctx.genNamedSym "for";
    indexVar <- Var <$> Ctx.genNamedSym "index"
    eltVar   <- Var <$> Ctx.genNamedSym (show name)
    startVar <- Var <$> Ctx.genNamedSym "start"
    stopVar  <- Var <$> Ctx.genNamedSym "stop"
    stepVar  <- Var <$> Ctx.genNamedSym "step"
    srcStepVar <- Var <$> Ctx.genNamedSym "src_step"

    (srcN, srcT) <- if A.isRangeLiteral src
                    then return (undefined, Comm "Range not generated")
                    else translate src

    let srcType = A.getType src
        eltType = if Ty.isRangeType srcType
                  then int
                  else translate $ Ty.getResultType (A.getType src)
        srcStart = if Ty.isRangeType srcType
                   then Call (Nam "range_start") [srcN]
                   else Int 0 -- Arrays start at 0
        srcStop  = if Ty.isRangeType srcType
                   then Call (Nam "range_stop") [srcN]
                   else BinOp (translate ID.MINUS)
                              (Call (Nam "array_size") [srcN])
                              (Int 1)
        srcStep  = if Ty.isRangeType srcType
                   then Call (Nam "range_step") [srcN]
                   else Int 1

    (srcStartN, srcStartT) <- translateSrc src A.start startVar srcStart
    (srcStopN,  srcStopT)  <- translateSrc src A.stop stopVar srcStop
    (srcStepN,  srcStepT)  <- translateSrc src A.step srcStepVar srcStep

    (stepN, stepT) <- translate step
    substituteVar name eltVar
    (bodyN, bodyT) <- translate body
    unsubstituteVar name

    let stepDecl = Assign (Decl (int, stepVar))
                          (BinOp (translate ID.TIMES) stepN srcStepN)
        stepAssert = Statement $ Call (Nam "range_assert_step") [stepVar]
        indexDecl = Seq [AsExpr $ Decl (int, indexVar)
                        ,If (BinOp (translate ID.GT)
                                   (AsExpr stepVar) (Int 0))
                            (Assign indexVar srcStartN)
                            (Assign indexVar srcStopN)]
        cond = BinOp (translate ID.AND)
                     (BinOp (translate ID.GTE) indexVar srcStartN)
                     (BinOp (translate ID.LTE) indexVar srcStopN)
        eltDecl =
           Assign (Decl (eltType, eltVar))
                  (if Ty.isRangeType srcType
                   then AsExpr indexVar
                   else AsExpr $ Call (Nam "array_get") [srcN, indexVar]
                                 `Dot` encoreArgTTag eltType)
        inc = Assign indexVar (BinOp (translate ID.PLUS) indexVar stepVar)
        theBody = Seq [eltDecl
                      ,Statement bodyT
                ,Assign tmpVar bodyN
                    ,inc]
        theLoop = While cond theBody
        tmpDecl  = Statement $ Decl (translate (A.getType for), tmpVar)

    return (tmpVar, Seq [tmpDecl
                        ,srcT
                        ,srcStartT
                        ,srcStopT
                        ,srcStepT
                        ,stepT
                        ,stepDecl
                        ,stepAssert
                        ,indexDecl
                        ,theLoop])
    where
      translateSrc src selector var rhs
          | A.isRangeLiteral src = translate (selector src)
          | otherwise = return (var, Assign (Decl (int, var)) rhs)

  translate ite@(A.IfThenElse { A.cond, A.thn, A.els }) =
      do tmp <- Ctx.genNamedSym "ite"
         (ncond, tcond) <- translate cond
         (nthn, tthn) <- translate thn
         (nels, tels) <- translate els
         let exportThn = Seq $ tthn : [Assign (Var tmp) nthn]
             exportEls = Seq $ tels : [Assign (Var tmp) nels]
         return (Var tmp,
                 Seq [AsExpr $ Decl (translate (A.getType ite), Var tmp),
                      If (StatAsExpr ncond tcond) (Statement exportThn) (Statement exportEls)])

  translate e@(A.Embed {A.code=code}) = do
    interpolated <- interpolate code
    if Ty.isVoidType (A.getType e) then
        return (unit, Embed $ "({" ++ interpolated  ++ "})")
    else
        namedTmpVar "embed" (A.getType e) (Embed $ "({" ++ interpolated  ++ "})")
        where
          interpolate :: String -> State Ctx.Context String
          interpolate embedstr =
              case (Parsec.parse interpolateParser "embed expression" embedstr) of
                (Right parsed) -> do
                  strs <- mapM toLookedUpString parsed
                  return $ concat strs
                (Left err) -> error $ show err

          toLookedUpString :: Either String VarLkp -> State Ctx.Context String
          toLookedUpString e = case e of
                                 (Right (VarLkp var)) -> do
                                        ctx <- get
                                        case Ctx.substLkp ctx $ (ID.Name var) of
                                          (Just found) -> return $ show found
                                          Nothing      -> return var -- hope that it's a parameter,
                                                                     -- let clang handle the rest

                                 (Left str)           -> return str

          interpolateParser :: PString.Parser [Either String VarLkp]
          interpolateParser = do
            Parsec.many
                  (Parsec.try
                             (do
                               var <- varlkpParser
                               return (Right (VarLkp var)))
                   Parsec.<|>
                         (do
                           c <- Parsec.anyChar
                           return (Left [c])))

          varlkpParser :: PString.Parser String
          varlkpParser = do
                           Parsec.string "#{"
                           id <- P.identifierParser
                           Parsec.string "}"
                           return id

  translate get@(A.Get{A.val})
    | Ty.isFutureType $ A.getType val =
        do (nval, tval) <- translate val
           let resultType = translate (Ty.getResultType $ A.getType val)
               theGet = fromEncoreArgT resultType (Call (Nam "future_get_actor") [nval])
           tmp <- Ctx.genSym
           return (Var tmp, Seq [tval, Assign (Decl (resultType, Var tmp)) theGet])
    | Ty.isStreamType $ A.getType val =
        do (nval, tval) <- translate val
           let resultType = translate (Ty.getResultType $ A.getType val)
               theGet = fromEncoreArgT resultType (Call (Nam "stream_get") [nval])
           tmp <- Ctx.genSym
           return (Var tmp, Seq [tval, Assign (Decl (resultType, Var tmp)) theGet])
    | otherwise = error $ "Cannot translate get of " ++ show val


  translate yield@(A.Yield{A.val}) =
      do (nval, tval) <- translate val
         tmp <- Ctx.genSym
         let yieldArg = asEncoreArgT (translate (A.getType val)) nval
             tmpStream = Assign (Decl (stream, Var tmp)) streamHandle
             updateStream = Assign (streamHandle) (Call (Nam "stream_put")
                                                        [AsExpr streamHandle, yieldArg, runtimeType $ A.getType val])
         return (unit, Seq [tval, tmpStream, updateStream])

  translate eos@(A.Eos{}) =
      let eosCall = Call (Nam "stream_close") [streamHandle]
      in return (unit, Statement eosCall)

  translate iseos@(A.IsEos{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genSym
         let resultType = translate (A.getType target)
             theCall = Assign (Decl (bool, Var tmp)) (Call (Nam "stream_eos") [ntarg])
         return (Var tmp, Seq [ttarg, theCall])

  translate next@(A.StreamNext{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genSym
         let theCall = Assign (Decl (stream, Var tmp)) (Call (Nam "stream_get_next") [ntarg])
         return (Var tmp, Seq [ttarg, theCall])

  translate await@(A.Await{A.val}) =
      do (nval, tval) <- translate val
         return (unit, Seq [tval, Statement $ Call (Nam "future_await") [nval]])

  translate suspend@(A.Suspend{}) =
         return (unit, Seq [Call (Nam "actor_suspend") ([] :: [CCode Expr])]) --TODO: Call should support 0-arity

  translate futureChain@(A.FutureChain{A.future, A.chain}) =
      do (nfuture,tfuture) <- translate future
         (nchain, tchain)  <- translate chain
         futName <- Ctx.genSym
         let ty = runtimeType . Ty.getResultType . A.getType $ chain
             futDecl = Assign (Decl (Ptr $ Typ "future_t", Var futName))
                              (Call (Nam "future_mk") [ty])
         result <- Ctx.genSym
         return $ (Var result, Seq [tfuture,
                                    tchain,
                                    futDecl,
                                    (Assign (Decl (Ptr $ Typ "future_t", Var result)) (Call (Nam "future_chain_actor") [nfuture, (Var futName), nchain]))])

  translate async@(A.Async{A.body, A.emeta}) =
      do taskName <- Ctx.genNamedSym "task"
         futName <- Ctx.genNamedSym "fut"
         msgName <- Ctx.genNamedSym "arg"
         let metaId = Meta.getMetaId emeta
             funName = taskFunctionName metaId
             envName = taskEnvName metaId
             dependencyName = taskDependencyName metaId
             traceName = taskTraceName metaId
             freeVars = Util.freeVariables [] body
             taskMk = Assign (Decl (task, Var taskName))
                      (Call (Nam "task_mk") [funName, envName, dependencyName, traceName])
             traceFuture = Statement $ Call (Nam "pony_traceobject") [Var futName, futureTypeRecName `Dot` Nam "trace"]
             traceTask = Statement $ Call (Nam "pony_traceobject") [Var taskName, AsLval $ Nam "NULL" ]
             traceEnv =  Statement $ Call (Nam "pony_traceobject") [Var $ show envName, AsLval $ Nam "task_trace" ]
             traceDependency =  Statement $ Call (Nam "pony_traceobject") [Var $ show dependencyName, AsLval $ Nam "NULL" ]
         packedEnv <- mapM (packFreeVars envName) freeVars
         return $ (Var futName, Seq $ (encoreAlloc envName) : packedEnv ++
                                      [encoreAlloc dependencyName,
                                       taskRunner async futName,
                                       taskMk,
                                       Statement (Call (Nam "task_attach_fut") [Var taskName, Var futName]),
                                       Statement (Call (Nam "task_schedule") [Var taskName]),
                                       Embed $ "",
                                       Embed $ "// --- GC on sending ----------------------------------------",
                                       Statement $ Call (Nam "pony_gc_send") ([] :: [CCode Expr]),
                                       traceFuture,
                                       traceTask,
                                       traceEnv,
                                       traceDependency,
                                       Statement $ Call (Nam "pony_send_done") ([] :: [CCode Expr]),
                                       Embed $ "// --- GC on sending ----------------------------------------",
                                       Embed $ ""
                                       ])

      where
        encoreAlloc name = Assign (Decl (Ptr $ Struct name, AsLval name))
                               (Call (Nam "encore_alloc")
                                   [Sizeof $ Struct name])
        taskRunner async futName = Assign (Decl (Ptr $ Typ "future_t", Var futName))
                                             (Call (Nam "future_mk") ([runtimeType . Ty.getResultType . A.getType $ async]))
        packFreeVars envName (name, _) =
            do c <- get
               let tname = case Ctx.substLkp c name of
                              Just substName -> substName
                              Nothing -> AsLval $ globalClosureName name
               return $ Assign ((Var $ show envName) `Arrow` (fieldName name)) tname

  translate clos@(A.Closure{A.eparams, A.body}) =
      do let metaId    = Meta.getMetaId . A.getMeta $ clos
             funName   = closureFunName metaId
             envName   = closureEnvName metaId
             traceName = closureTraceName metaId
             freeVars  = Util.freeVariables (map A.pname eparams) body
         tmp <- Ctx.genSym
         fillEnv <- mapM (insertVar envName) freeVars
         return $ (Var tmp, Seq $ (mkEnv envName) : fillEnv ++
                           [Assign (Decl (closure, Var tmp))
                                       (Call (Nam "closure_mk") [funName, envName, traceName])])
      where
        mkEnv name =
           Assign (Decl (Ptr $ Struct name, AsLval name))
                   (Call (Nam "encore_alloc")
                         [Sizeof $ Struct name])
        insertVar envName (name, _) =
            do c <- get
               let tname = case Ctx.substLkp c name of
                              Just substName -> substName
                              Nothing -> AsLval $ globalClosureName name
               return $ Assign ((Deref $ Var $ show envName) `Dot` (fieldName name)) tname

  translate fcall@(A.FunctionCall{A.name, A.args}) = do
    c <- get
    let clos = Var (case Ctx.substLkp c name of
                      Just substName -> show substName
                      Nothing -> show $ globalClosureName name)
    let ty = A.getType fcall
    targs <- mapM translateArgument args
    (tmpArgs, tmpArgDecl) <- tmpArr (Typ "value_t") targs
    (calln, theCall) <- namedTmpVar "clos" ty $ AsExpr $ fromEncoreArgT (translate ty) (Call (Nam "closure_call") [clos, tmpArgs])
    let comment = Comm ("fcall name: " ++ show name ++ " (" ++ show (Ctx.substLkp c name) ++ ")")
    return (if Ty.isVoidType ty then unit else calln, Seq [comment, tmpArgDecl, theCall])
        where
          translateArgument arg =
            do (ntother, tother) <- translate arg
               return $ asEncoreArgT (translate $ A.getType arg) (StatAsExpr ntother tother)

  translate other = error $ "Expr.hs: can't translate: '" ++ show other ++ "'"

castArguments :: Ty.Type -> CCode Lval -> Ty.Type -> CCode Expr
castArguments expected targ targType
  | Ty.isTypeVar expected = asEncoreArgT (translate targType) $ AsExpr targ
  | targType /= expected = Cast (translate expected) targ
  | otherwise = AsExpr targ

sharedObjectMethodFut call@(A.MethodCall{A.target, A.name, A.args}) =
  let
    clazz = A.getType target
    f = methodImplFutureName clazz name
  in
    do
    (this, initThis) <- translate target
    result <- Ctx.genNamedSym "so_call"
    (args, initArgs) <- fmap unzip $ mapM translate args
    return (Var result,
      Seq $
        initThis:
        initArgs ++
        [ret result $ callF f this args]
      )
  where
    retType = translate $ A.getType call
    callF f this args = Call f $ this:args
    ret result fcall = Assign (Decl (retType, Var result)) fcall

sharedObjectMethodOneWay call@(A.MessageSend{A.target, A.name, A.args}) =
  let
    clazz = A.getType target
    f = methodImplOneWayName clazz name
  in
    do
    (this, initThis) <- translate target
    (args, initArgs) <- fmap unzip $ mapM translate args
    return (unit,
      Seq $
        initThis:
        initArgs ++
        [callF f this args]
      )
  where
    retType = translate $ A.getType call
    callF f this args = Statement $ Call f $ this:args

traitMethod call@(A.MethodCall{A.target, A.name, A.args}) =
  let
    ty = A.getType target
    id = oneWayMsgId ty name
    tyStr = Ty.getId ty
    nameStr = show name
  in
    do
      (this, initThis) <- translate target
      f <- Ctx.genNamedSym $ concat [tyStr, "_", nameStr]
      vtable <- Ctx.genNamedSym $ concat [tyStr, "_", "vtable"]
      tmp <- Ctx.genNamedSym "trait_method_call"
      (args, initArgs) <- fmap unzip $ mapM translate args
      return $ (Var tmp,
        Seq $
          initThis:
          initArgs ++
          [declF f] ++
          [declVtable vtable] ++
          [initVtable this vtable] ++
          [initF f vtable id] ++
          [ret tmp $ callF f this args]
        )
  where
    thisType = translate $ A.getType target
    argTypes = map (translate . A.getType) args
    retType = translate $ A.getType call
    declF f = FunPtrDecl retType (Nam f) $ thisType:argTypes
    declVtable vtable = FunPtrDecl (Ptr void) (Nam vtable) [Typ "int"]
    vtable this = ArrAcc 0 $ this `Arrow` selfTypeField `Arrow` Nam "vtable"
    initVtable this v = Assign (Var v) $ Cast (Ptr void) $ vtable this
    initF f vtable id = Assign (Var f) $ Call (Nam vtable) [id]
    callF f this args = Call (Nam f) $ this:args
    ret tmp fcall = Assign (Decl (retType, Var tmp)) fcall

gcSend as expectedTypes futTrace =
    [Embed $ "",
     Embed $ "// --- GC on sending ----------------------------------------",
     Statement $ Call (Nam "pony_gc_send") ([] :: [CCode Expr]),
     futTrace] ++
     (zipWith tracefunCall as expectedTypes) ++
    [Statement $ Call (Nam "pony_send_done") ([] :: [CCode Expr]),
     Embed $ "// --- GC on sending ----------------------------------------",
     Embed $ ""]

tracefunCall :: (CCode Lval, Ty.Type) -> Ty.Type -> CCode Stat
tracefunCall (a, t) expectedType =
  let
    needToUnwrap = Ty.isTypeVar expectedType && not (Ty.isTypeVar t)
    var = if needToUnwrap then a `Dot` Nam "p" else a
  in
    Statement $ traceVariable t var

-- Note: the 2 is for the 16 bytes of payload in pony_msg_t
-- If the size of this struct changes, so must this calculation
calcPoolSizeForMsg args = (args + 2) `div` 8
