{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, GADTs, FlexibleContexts #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import qualified CodeGen.CCodeNames as C
import CodeGen.Type
import CodeGen.Trace (traceVariable, tracefunCall)
import CodeGen.GC (gcSend)
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
import qualified Data.Set as Set
import Data.Maybe

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
    | Ty.isIntType ty          = "%lli"
    | Ty.isRealType ty         = "%f"
    | Ty.isStringObjectType ty = "%s"
    | Ty.isStringType ty       = "%s"
    | Ty.isCharType ty         = "%c"
    | Ty.isBoolType ty         = "bool<%zd>"
    | Ty.isRefType ty          = show ty ++ "<%p>"
    | Ty.isFutureType ty       = "fut<%p>"
    | otherwise = case translate ty of
                    Ptr something -> "%p"
                    _ -> error $ "Expr.hs: typeToPrintfFstr not defined for " ++ show ty

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

getRuntimeType = runtimeType . Ty.getResultType . A.getType

-- these two are exclusively used for A.Embed translation:
type ParsedEmbed = [Either String VarLkp]
newtype VarLkp = VarLkp String

newParty :: A.Expr -> CCode Name
newParty (A.Liftv {}) = partyNewParV
newParty (A.Liftf {}) = partyNewParF
newParty (A.PartyPar {}) = partyNewParP
newParty _ = error $ "ERROR in 'Expr.hs': node is different from 'Liftv', " ++
                     "'Liftf' or 'PartyPar'"

translateDecl (name, expr) = do
  (ne, te) <- translate expr
  tmp <- Ctx.genNamedSym (show name)
  substituteVar name (Var tmp)
  return (Var tmp,
          [Comm $ show name ++ " = " ++ show (PP.ppSugared expr)
          ,te
          ,Assign (Decl (translate (A.getType expr), Var tmp)) ne])

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
  translate lit@(A.CharLiteral {A.charLit = c}) = namedTmpVar "literal" (A.getType lit) (Char c)

  translate tye@(A.TypedExpr {A.body, A.ty}) = do
    (nbody, tbody) <- translate body
    tmp <- Ctx.genNamedSym "cast"
    let ty' = translate ty
        theCast = Assign (Decl (ty', Var tmp))
                         (Cast ty' nbody)
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

  translate l@(A.Liftf {A.val}) = do
    (nval, tval) <- translate val
    let runtimeT = (runtimeType . A.getType) val
    (nliftf, tliftf) <- namedTmpVar "par" (A.getType l) $
                        Call (newParty l) [AsExpr encoreCtxVar, AsExpr nval, runtimeT]
    return (nliftf, Seq [tval, tliftf])

  translate l@(A.Liftv {A.val}) = do
    (nval, tval) <- translate val
    let runtimeT = (runtimeType . A.getType) val
    (nliftv, tliftv) <- namedTmpVar "par" (A.getType l) $
                        Call (newParty l) [AsExpr encoreCtxVar,
                                           asEncoreArgT (translate $ A.getType val) (AsExpr nval), runtimeT]
    return (nliftv, Seq [tval, tliftv])

  translate p@(A.PartyJoin {A.val}) = do
    (nexpr, texpr) <- translate val
    let typ = A.getType p
    (nJoin, tJoin) <- namedTmpVar "par" typ $ Call partyJoin [encoreCtxVar, nexpr]
    return (nJoin, Seq [texpr, tJoin])

  translate p@(A.PartyExtract {A.val}) = do
    (nval, tval) <- translate val
    let runtimeT = (runtimeType . A.getType) p
    (nExtract, tExtract) <- namedTmpVar "arr" (A.getType p) $
                            Call partyExtract [AsExpr encoreCtxVar, AsExpr nval, runtimeT]
    return (nExtract, Seq [tval, tExtract])

  translate p@(A.PartyPar {A.parl, A.parr}) = do
    (nleft, tleft) <- translate parl
    (nright, tright) <- translate parr
    let runtimeT = (runtimeType . A.getType) p
    (npar, tpar) <- namedTmpVar "par" (A.getType p) $
                        Call (newParty p) [AsExpr encoreCtxVar, AsExpr nleft, AsExpr nright, runtimeT]
    return (npar, Seq [tleft, tright, tpar])

  translate ps@(A.PartySeq {A.par, A.seqfunc}) = do
    (npar, tpar) <- translate par
    (nseqfunc, tseqfunc) <- translate seqfunc
    let runtimeT = (runtimeType . A.getType) ps
    (nResultPar, tResultPar) <- namedTmpVar "par" (A.getType ps) $
                                Call partySequence [AsExpr encoreCtxVar,
                                                    AsExpr npar,
                                                    AsExpr nseqfunc,
                                                    runtimeT]
    return (nResultPar, Seq [tpar, tseqfunc, tResultPar])

  translate (A.Print {A.args}) = do
      let string = head args
          rest = tail args
      unless (Ty.isStringType $ A.getType string) $
             error $ "Expr.hs: Print expects first argument to be a string literal"
      targs <- mapM translate rest
      let argNames = map fst targs
      let argDecls = map snd targs
      let argTys   = map A.getType rest
      let fstring   = formatString (A.stringLit string) argTys
      let argNamesWithoutStringObjects = zipWith extractStringFromString rest argNames
      return $ (unit,
                Seq $ argDecls ++
                     [Statement
                      (Call (Nam "printf")
                       ((String fstring) : argNamesWithoutStringObjects))])
      where
        formatString s [] = s
        formatString "" (ty:tys) = error "Expr.hs: Wrong number of arguments to printf"
        formatString ('{':'}':s) (ty:tys) = (typeToPrintfFstr ty) ++ (formatString s tys)
        formatString (c:s) tys = c : (formatString s tys)

        extractStringFromString arg argName
          | Ty.isStringObjectType (A.getType arg) = AsExpr $ argName `Arrow` (fieldName $ ID.Name "data")
          | otherwise                             = AsExpr argName

  translate exit@(A.Exit {A.args = [arg]}) = do
      (narg, targ) <- translate arg
      let exitCall = Call (Nam "exit") [narg]
      return (unit, Seq [Statement targ, Statement exitCall])

  translate seq@(A.Seq {A.eseq}) = do
    ntes <- mapM translate eseq
    let (nes, tes) = unzip ntes
    return (last nes, Seq $ map commentAndTe (zip eseq tes))
           where
             commentFor = (Comm . show . PP.ppSugared)
             commentAndTe (ast, te) = Seq [commentFor ast, te]

  translate (A.Assign {A.lhs = lhs@(A.ArrayAccess {A.target, A.index}), A.rhs}) = do
    (nrhs, trhs) <- translate rhs
    (ntarg, ttarg) <- translate target
    (nindex, tindex) <- translate index
    let ty = translate $ A.getType lhs
        theSet =
           Statement $
           Call arraySet [AsExpr ntarg, AsExpr nindex, asEncoreArgT ty $ AsExpr nrhs]
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
    let createOption = Call encoreAllocName [AsExpr encoreCtxVar, (Sizeof . AsType) optionT]
    (nalloc, talloc) <- namedTmpVar "option" (A.getType maybe) createOption
    let tag = Assign (Deref nalloc `Dot` (Nam "tag")) (Nam "JUST")
    (nE, tE) <- translate e
    let tJust = Assign (Deref nalloc `Dot` (Nam "val")) (asEncoreArgT (translate $ A.getType e) nE)
    return (nalloc, Seq [talloc, tag, tE, tJust])

  translate maybe@(A.MaybeValue _ (A.NothingData {})) = do
    let createOption = Amp (Nam "DEFAULT_NOTHING")
    (nalloc, talloc) <- namedTmpVar "option" (A.getType maybe) createOption
    return (nalloc, talloc)

  translate tuple@(A.Tuple {A.args}) = do
    tupleName <- Ctx.genNamedSym "tuple"
    transArgs <- mapM translate args
    let elemTypes = map A.getType args
        realTypes = map runtimeType elemTypes
        tupLen = Int $ length args
        tupType = A.getType tuple
    let eAlloc = Call tupleMkFn [AsExpr encoreCtxVar, tupLen]
        theTupleDecl = Assign (Decl (translate tupType, Var tupleName)) eAlloc
        tSetTupleType = Seq $ zipWith (tupleSetType tupleName) [0..] realTypes
        (theTupleVars, theTupleContent) = unzip transArgs
        theTupleInfo = zip theTupleVars elemTypes
        theTupleSets = zipWith (tupleSet tupleName) [0..] theTupleInfo
    return (Var tupleName, Seq $ theTupleDecl : tSetTupleType :
                                 theTupleContent ++ theTupleSets)
      where
        tupleSetType name index ty =
          Statement $ Call C.tupleSetType
                           [AsExpr $ Var name, Int index, ty]
        tupleSet tupleName index (narg, ty) =
          Statement $ Call C.tupleSet
                           [AsExpr $ Var tupleName,
                            Int index,
                            asEncoreArgT (translate ty) $ AsExpr narg]

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

  translate (A.NewWithInit {A.ty, A.args})
    | Ty.isActiveClassType ty = delegateUse callTheMethodOneway
    | Ty.isSharedClassType ty = delegateUse callTheMethodOneway
    | otherwise = delegateUse callTheMethodSync
    where
      initName = ID.Name "_init"
      delegateUse methodCall =
        let
          fName = constructorImplName ty
          callCtor = Call fName [encoreCtxName]
          typeParams = Ty.getTypeParameters ty
          callTypeParamsInit args = Call (runtimeTypeInitFnName ty) args
          typeArgs = map runtimeType typeParams
        in
          do
            (nnew, ctorCall) <- namedTmpVar "new" ty callCtor
            (initArgs, result) <-
              methodCall nnew ty initName args ty
            return (nnew,
              Seq $
                [ctorCall] ++
                initArgs ++
                [ Statement $ callTypeParamsInit $ (AsExpr nnew):typeArgs
                , Statement result]
              )

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
                       (Call arrayMkFn [AsExpr encoreCtxVar, AsExpr nsize, runtimeType ty])
         return (Var arrName, Seq [tsize, theArrayDecl])

  translate rangeLit@(A.RangeLiteral {A.start = start, A.stop = stop, A.step = step}) = do
      (nstart, tstart) <- translate start
      (nstop, tstop)   <- translate stop
      (nstep, tstep)   <- translate step
      rangeLiteral    <- Ctx.genNamedSym "range_literal"
      let ty = translate $ A.getType rangeLit
      return (Var rangeLiteral, Seq [tstart, tstop, tstep,
                                Assign (Decl (ty, Var rangeLiteral))
                                       (Call rangeMkFn [encoreCtxVar, nstart, nstop, nstep])])

  translate arrAcc@(A.ArrayAccess {A.target, A.index}) =
      do (ntarg, ttarg) <- translate target
         (nindex, tindex) <- translate index
         accessName <- Ctx.genNamedSym "access"
         let ty = translate $ A.getType arrAcc
             theAccess =
                Assign (Decl (ty, Var accessName))
                       (fromEncoreArgT ty (Call arrayGet [ntarg, nindex]))
         return (Var accessName, Seq [ttarg, tindex, theAccess])

  translate arrLit@(A.ArrayLiteral {A.args}) =
      do arrName <- Ctx.genNamedSym "array"
         targs <- mapM translate args
         let len = length args
             ty  = Ty.getResultType $ A.getType arrLit
             theArrayDecl =
                Assign (Decl (array, Var arrName))
                       (Call arrayMkFn [AsExpr encoreCtxVar, Int len, runtimeType ty])
             theArrayContent = Seq $ map (\(_, targ) -> targ) targs
             theArraySets =
                let (_, sets) = mapAccumL (arraySet arrName ty) 0 targs
                in sets
         return (Var arrName, Seq $ theArrayDecl : theArrayContent : theArraySets)
      where
        arraySet arrName ty index (narg, _) =
            (index + 1,
             Statement $ Call C.arraySet
                              [AsExpr $ Var arrName,
                               Int index,
                               asEncoreArgT (translate ty) $ AsExpr narg])

  translate arrSize@(A.ArraySize {A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genNamedSym "size"
         let theSize = Assign (Decl (int, Var tmp))
                              (Call arraySize [ntarg])
         return (Var tmp, Seq [ttarg, theSize])

  translate call@(A.MethodCall { A.target, A.name, A.args})
    | (Ty.isTraitType . A.getType) target = do
        (ntarget, ttarget) <- translate target
        (nCall, tCall) <- traitMethod ntarget (A.getType target) name args
                                      (translate (A.getType call))
        return (nCall, Seq $ ttarget : [tCall])
    | syncAccess = delegateUse callTheMethodSync "sync_method_call"
    | sharedAccess = delegateUse callTheMethodFuture "shared_method_call"
    | isActive && isStream = delegateUse callTheMethodStream "stream"
    | isActive && isFuture = delegateUse callTheMethodFuture "fut"
    | otherwise = error $ "No match for " ++ show targetTy
        where
          targetTy = A.getType target
          retTy = A.getType call
          delegateUse methodCall sym = do
            result <- Ctx.genNamedSym sym
            (ntarget, ttarget) <- translate target
            (initArgs, resultExpr) <-
              methodCall ntarget targetTy name args retTy
            return (Var result,
              Seq $
                ttarget :
                initArgs ++
                [Assign (Decl (translate retTy, Var result)) resultExpr]
              )
          syncAccess = A.isThisAccess target ||
                       (Ty.isPassiveClassType . A.getType) target
          sharedAccess = Ty.isSharedClassType $ A.getType target
          isActive = Ty.isActiveClassType targetTy
          isStream = Ty.isStreamType retTy
          isFuture = Ty.isFutureType retTy

  translate call@(A.MessageSend { A.target, A.name, A.args })
      | (Ty.isActiveClassType . A.getType) target = messageSend
      | sharedAccess = sharedObjectMethodOneWay call
      | otherwise = error "Tried to send a message to something that was not an active reference"
          where
            sharedAccess = Ty.isSharedClassType $ A.getType target
            messageSend :: State Ctx.Context (CCode Lval, CCode Stat)
            messageSend =
                do (ntarg, ttarg) <- translate target
                   theSend <- activeMessageSend ntarg (A.getType target) name args
                   return (unit, Seq (Comm "message send" : ttarg : [theSend]))

  translate w@(A.While {A.cond, A.body}) =
      do (ncond,tcond) <- translate cond
         (nbody,tbody) <- translate body
         tmp <- Ctx.genNamedSym "while";
         let exportBody = Seq $ tbody : [Assign (Var tmp) nbody]
         return (Var tmp,
                 Seq [Statement $ Decl (translate (A.getType w), Var tmp),
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
                   then Call rangeStart [srcN]
                   else Int 0 -- Arrays start at 0
        srcStop  = if Ty.isRangeType srcType
                   then Call rangeStop [srcN]
                   else BinOp (translate ID.MINUS)
                              (Call arraySize [srcN])
                              (Int 1)
        srcStep  = if Ty.isRangeType srcType
                   then Call rangeStep [srcN]
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
        stepAssert = Statement $ Call rangeAssertStep [stepVar]
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
                   else AsExpr $ fromEncoreArgT eltType (Call arrayGet [srcN, indexVar]))
        inc = Assign indexVar (BinOp (translate ID.PLUS) indexVar stepVar)
        theBody = Seq [eltDecl, Statement bodyT, Assign tmpVar bodyN, inc]
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

  translate m@(A.Match {A.arg, A.clauses}) =
      do retTmp <- Ctx.genNamedSym "match"
         (narg, targ) <- translate arg
         let argty = A.getType arg
             mType = translate (A.getType m)
         tIfChain <- ifChain clauses narg argty retTmp mType
         let lRetDecl = Decl (mType, Var retTmp)
             eZeroInit = Cast mType (Int 0)
             tRetDecl = Assign lRetDecl eZeroInit
         return (Var retTmp, Seq [targ, Statement lRetDecl, tIfChain])
      where
        withPatDecls assocs expr = do
          mapM_ (\name -> substituteVar name $ lookupVar name) varNames
          (nexpr, texpr) <- translate expr
          mapM_ unsubstituteVar varNames
          return (nexpr, texpr)
            where
              varNames = map (ID.Name . fst) assocs
              lookupVar name = fromJust $ lookup (show name) assocs

        translateMaybePattern e derefedArg argty assocs usedVars = do
          optionVar <- Ctx.genNamedSym "optionVal"
          nCheck <- Ctx.genNamedSym "optionCheck"
          let eMaybeVal = AsExpr $ Dot derefedArg (Nam "val")
              valType = Ty.getResultType argty
              eMaybeField = fromEncoreArgT (translate valType) eMaybeVal
              tVal = Assign (Decl (translate valType, Var optionVar)) eMaybeField

          (nRest, tRest, newUsedVars) <- translatePattern e (Var optionVar) valType assocs usedVars

          let expectedTag = AsExpr $ AsLval $ Nam "JUST"
              actualTag = AsExpr $ Dot derefedArg $ Nam "tag"
              eTagsCheck = BinOp (translate ID.EQ) expectedTag actualTag
              intTy = translate Ty.intType
              tDecl = Statement $ Decl (intTy, nRest)
              tRestWithDecl = Seq [tDecl, tVal, tRest]
              eRest = StatAsExpr nRest tRestWithDecl
              eCheck = BinOp (translate ID.AND) eTagsCheck eRest
              tCheck = Assign (Var nCheck) eCheck

          return (Var nCheck, tCheck, newUsedVars)

        translateComparison e1 e2 ty
          | Ty.isStringType ty = do
              let strcmpCall = Call (Nam "strcmp") [e1, e2]
              return $ BinOp (translate ID.EQ) strcmpCall (Int 0)
          | Ty.isStringObjectType ty = do
              return $ Call (methodImplName Ty.stringObjectType (ID.Name "equals")) [AsExpr encoreCtxVar, e1, e2]
          | otherwise =
              return (BinOp (translate ID.EQ) e1 e2)

        translatePattern (A.FunctionCall {A.name, A.args}) narg argty assocs usedVars = do
          let eSelfArg = AsExpr narg
              eNullCheck = BinOp (translate ID.NEQ) eSelfArg Null
              innerExpr = head args -- args is known to only contain one element
              innerTy = A.getType innerExpr
              tmpTy = Ty.maybeType innerTy
              noArgs = [] :: [A.Expr]

          (nCall, tCall) <-
              if Ty.isTraitType argty || Ty.isCapabilityType argty
              then do
                calledType <- gets $ Ctx.lookupCalledType argty name
                traitMethod narg calledType name noArgs (translate tmpTy)
              else do
                tmp <- Ctx.genNamedSym "extractedOption"
                (argDecls, theCall) <-
                    passiveMethodCall narg argty name noArgs tmpTy
                let theAssign = Assign (Decl (translate tmpTy, Var tmp)) theCall
                return (Var tmp, Seq $ argDecls ++ [theAssign])

          let derefedCall = Deref nCall
          (nRest, tRest, newUsedVars) <-
              translateMaybePattern innerExpr derefedCall tmpTy assocs usedVars

          nCheck <- Ctx.genNamedSym "extractoCheck"
          let tDecl = Statement $ Decl (translate Ty.intType, nRest)
              tRestWithDecl = Seq [tDecl, tCall, tRest]
              eCheck = BinOp (translate ID.AND) eNullCheck $ StatAsExpr nRest tRestWithDecl
              tAssign = Assign (Var nCheck) eCheck

          return (Var nCheck, tAssign, newUsedVars)

        translatePattern (tuple@A.Tuple {A.args}) larg argty assocs usedVars = do
          tmp <- Ctx.genNamedSym "tupleCheck"

          let elemTypes = Ty.getArgTypes $ A.getType tuple
              elemInfo = zip elemTypes args
              theInit = Assign (Var tmp) (Int 1)

          (tChecks, newUsedVars) <- checkElems elemInfo (Var tmp) larg assocs usedVars 0
          return (Var tmp, Seq $ theInit:tChecks, newUsedVars)
            where
              checkElems [] _ _ _ usedVars _ = return ([], usedVars)
              checkElems ((ty, arg):rest) retVar larg assocs usedVars index = do
                accessName <- Ctx.genNamedSym "tupleAccess"
                let elemTy = translate ty
                    theDecl = Decl (elemTy, Var accessName)
                    theCall = fromEncoreArgT elemTy (Call tupleGet [AsExpr larg, Int index])
                    theCast = Cast elemTy theCall
                    theAssign = Assign theDecl theCall

                (ncheck, tcheck, newUsedVars) <- translatePattern arg (Var accessName) ty assocs usedVars
                (tRest, newNewUsedVars) <- checkElems rest retVar larg assocs newUsedVars (index + 1)

                let theAnd = BinOp (translate ID.AND) (AsExpr retVar) (AsExpr ncheck)
                    theRetAssign = Assign retVar theAnd
                    theHelpDecl = Statement $ Decl (translate Ty.intType, ncheck)
                return (theAssign : theHelpDecl : tcheck : theRetAssign : tRest, newNewUsedVars)

        translatePattern (A.MaybeValue {A.mdt=A.JustData{A.e}}) larg argty assocs usedVars = do
          let derefedArg = Deref larg
          translateMaybePattern e derefedArg argty assocs usedVars

        translatePattern (A.VarAccess{A.name}) larg argty assocs usedVars
          | Set.member name usedVars = do
              tmp <- Ctx.genNamedSym "varBinding"
              let eVar = AsExpr $ fromJust $ lookup (show name) assocs
                  eArg = AsExpr larg
              eComp <- translateComparison eVar eArg argty
              let tBindRet = Assign (Var tmp) eComp
              return (Var tmp, tBindRet, usedVars)
          | otherwise = do
              tmp <- Ctx.genNamedSym "varBinding"
              let lVar = fromJust $ lookup (show name) assocs
                  eArg = AsExpr larg
                  tBindVar = Assign lVar eArg
                  tBindRet = Assign (Var tmp) (Int 1)
                  newUsedVars = Set.insert name usedVars
                  tBindAll = Seq [tBindVar, tBindRet]
              return (Var tmp, tBindAll, newUsedVars)

        translatePattern value larg argty _ usedVars = do
          tmp <- Ctx.genNamedSym "valueCheck"
          (nvalue, tvalue) <- translate value
          let eValue = StatAsExpr nvalue tvalue
              eArg = AsExpr larg
          eComp <- translateComparison eValue eArg argty
          let tAssign = Assign (Var tmp) eComp
          return (Var tmp, tAssign, usedVars)

        translateIfCond (A.MatchClause {A.mcpattern, A.mcguard})
                        narg argty assocs = do
          (nPattern, tPatternNoDecl, _) <- translatePattern mcpattern narg argty assocs Set.empty

          -- The binding expression should evaluate to true,
          -- regardless of the values that are bound.
          let ty = translate Ty.intType
              eDeclPattern = AsExpr $ Decl (ty, nPattern)
              tPattern = Seq [Statement eDeclPattern, tPatternNoDecl]
              ePattern = StatAsExpr nPattern tPattern

          (nguard, tguard) <- withPatDecls assocs mcguard
          let eGuard = StatAsExpr nguard tguard
              eCond = BinOp (translate ID.AND) ePattern eGuard
          return eCond

        translateHandler clause handlerReturnVar assocs retTy = do
          (nexpr, texpr) <- withPatDecls assocs (A.mchandler clause)
          let eExpr = StatAsExpr nexpr texpr
              eCast = Cast retTy eExpr
              tAssign = Assign (Var handlerReturnVar) eCast
          return tAssign

        ifChain [] _ _ _ _ = do
          let errorCode = Int 1
              exitCall = Statement $ Call (Nam "exit") [errorCode]
              errorMsg = String "*** Runtime error: No matching clause was found ***\n"
              errorPrint = Statement $ Call (Nam "printf") [errorMsg]
          return $ Seq [errorPrint, exitCall]

        ifChain (clause:rest) narg argty retTmp retTy = do
          let freeVars = filter (not . Ty.isArrowType . snd) $
                                Util.freeVariables [] (A.mcpattern clause)
          assocs <- mapM createAssoc freeVars
          thenExpr <- translateHandler clause retTmp assocs retTy
          elseExpr <- ifChain rest narg argty retTmp retTy
          eCond <- translateIfCond clause narg argty assocs
          let tIf = Statement $ If eCond thenExpr elseExpr
              tDecls = Seq $ map (fwdDecl assocs) freeVars
          return $ Seq [tDecls, tIf]
            where
              createAssoc (name, _) = do
                let varName = show name
                tmp <- Ctx.genNamedSym varName
                return (varName, Var tmp)
              fwdDecl assocs (name, ty) =
                  let tname = fromJust $ lookup (show name) assocs
                  in Statement $ Decl (translate ty, tname)

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
               theGet = fromEncoreArgT resultType (Call futureGetActor [encoreCtxVar, nval])
           tmp <- Ctx.genSym
           return (Var tmp, Seq [tval, Assign (Decl (resultType, Var tmp)) theGet])
    | Ty.isStreamType $ A.getType val =
        do (nval, tval) <- translate val
           let resultType = translate (Ty.getResultType $ A.getType val)
               theGet = fromEncoreArgT resultType (Call streamGet [encoreCtxVar, nval])
           tmp <- Ctx.genSym
           return (Var tmp, Seq [tval, Assign (Decl (resultType, Var tmp)) theGet])
    | otherwise = error $ "Cannot translate get of " ++ show val


  translate yield@(A.Yield{A.val}) =
      do (nval, tval) <- translate val
         tmp <- Ctx.genSym
         let yieldArg = asEncoreArgT (translate (A.getType val)) nval
             tmpStream = Assign (Decl (stream, Var tmp)) streamHandle
             updateStream = Assign (streamHandle)
               (Call streamPut [AsExpr encoreCtxVar, AsExpr streamHandle,
                                yieldArg, runtimeType $ A.getType val])
         return (unit, Seq [tval, tmpStream, updateStream])

  translate eos@(A.Eos{}) =
      let eosCall = Call streamClose [encoreCtxVar, streamHandle]
      in return (unit, Statement eosCall)

  translate iseos@(A.IsEos{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genSym
         let resultType = translate (A.getType target)
             theCall = Assign (Decl (bool, Var tmp)) (Call streamEos [encoreCtxVar, ntarg])
         return (Var tmp, Seq [ttarg, theCall])

  translate next@(A.StreamNext{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genSym
         let theCall = Assign (Decl (stream, Var tmp)) (Call streamGetNext [encoreCtxVar, ntarg])
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
         let ty = getRuntimeType chain
             futDecl = Assign (Decl (C.future, Var futName))
                              (Call futureMkFn [AsExpr encoreCtxVar, ty])
         result <- Ctx.genSym
         return $ (Var result, Seq [tfuture,
                                    tchain,
                                    futDecl,
                                    (Assign (Decl (C.future, Var result))
                                            (Call futureChainActor
                                              [encoreCtxVar, nfuture, (Var futName), nchain]))])

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
                      (Call taskMkFn [encoreCtxName, funName, envName, dependencyName, traceName])
             -- TODO: (kiko) refactor to use traceVariable from Trace.hs
             traceFuture = Statement $ Call ponyTraceObject
                    [encoreCtxVar, Var futName, futureTypeRecName `Dot` Nam "trace"]
             traceTask = Statement $ Call ponyTraceObject
                    [encoreCtxVar, Var taskName, AsLval $ Nam "task_trace" ]
             traceEnv =  Statement $ Call ponyTraceObject
                    [encoreCtxVar, Var $ show envName, AsLval $ traceName ]
             traceDependency =  Statement $ Call ponyTraceObject [encoreCtxVar, Var $ show dependencyName, AsLval $ Nam "NULL" ]
         packedEnv <- mapM (packFreeVars envName) freeVars
         return $ (Var futName, Seq $ (encoreAlloc envName) : packedEnv ++
                                      [encoreAlloc dependencyName,
                                       taskRunner async futName,
                                       taskMk,
                                       Statement (Call taskAttachFut [Var taskName, Var futName]),
                                       Statement (Call taskSchedule [Var taskName]),
                                       -- TODO: (kiko) Refactor to use GC.hs
                                       Embed $ "",
                                       Embed $ "// --- GC on sending ----------------------------------------",
                                       Statement $ Call ponyGcSendName [encoreCtxVar],
                                       traceFuture,
                                       traceTask,
                                       traceEnv,
                                       traceDependency,
                                       Statement $ Call ponySendDoneName [encoreCtxVar],
                                       Embed $ "// --- GC on sending ----------------------------------------",
                                       Embed $ ""
                                       ])

      where
        encoreAlloc name = Assign (Decl (Ptr $ Struct name, AsLval name))
                               (Call C.encoreAllocName [AsExpr encoreCtxVar, Sizeof $ Struct name])
        taskRunner async futName = Assign (Decl (C.future, Var futName))
                                             (Call futureMkFn
                                                [AsExpr encoreCtxVar, getRuntimeType async])
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
                                       (Call closureMkFn [encoreCtxName, funName, envName, traceName])])
      where
        mkEnv name =
           Assign (Decl (Ptr $ Struct name, AsLval name))
                   (Call encoreAllocName [AsExpr encoreCtxVar, Sizeof $ Struct name])
        insertVar envName (name, _) =
            do c <- get
               let tname = case Ctx.substLkp c name of
                              Just substName -> substName
                              Nothing -> AsLval $ globalClosureName name
               return $ Assign ((Deref $ Var $ show envName) `Dot` (fieldName name)) tname

  translate fcall@(A.FunctionCall{A.name, A.args}) = do
    ctx <- get
    case Ctx.substLkp ctx name of
      Just clos -> closureCall clos fcall
      Nothing -> globalFunctionCall fcall

  translate other = error $ "Expr.hs: can't translate: '" ++ show other ++ "'"

closureCall :: CCode Lval -> A.Expr ->
  State Ctx.Context (CCode Lval, CCode Stat)
closureCall clos fcall@A.FunctionCall{A.name, A.args} = do
  targs <- mapM translateArgument args
  (tmpArgs, tmpArgDecl) <- tmpArr (Typ "value_t") targs
  (calln, theCall) <- namedTmpVar "clos" typ $
    AsExpr $
      fromEncoreArgT (translate typ) $
        Call closureCallName [encoreCtxVar, clos, tmpArgs]
  return (if Ty.isVoidType typ then unit else calln, Seq [tmpArgDecl, theCall])
    where
      typ = A.getType fcall
      translateArgument arg = do
        (ntother, tother) <- translate arg
        return $ asEncoreArgT (translate $ A.getType arg)
          (StatAsExpr ntother tother)

globalFunctionCall :: A.Expr -> State Ctx.Context (CCode Lval, CCode Stat)
globalFunctionCall fcall@A.FunctionCall{A.name, A.args} = do
  (args', initArgs) <- fmap unzip $ mapM translate args
  (callVar, call) <- namedTmpVar "global_f" typ $
    Call (globalFunctionName name) (encoreCtxVar:args')
  let ret = if Ty.isVoidType typ then unit else callVar
  return $ (ret, Seq $ initArgs ++ [call])
  where
    typ = A.getType fcall

indexArgument msgName i = Arrow msgName (Nam $ "f" ++ show i)

activeMessageSend targetName targetType name args = do
  targs <- mapM translate args
  theMsgName <- Var <$> Ctx.genNamedSym "arg"
  header <- gets $ Ctx.lookupMethod targetType name
  let (argNames, argDecls) = unzip targs
      msgType = AsType $ oneWayMsgTypeName targetType name
      msgTypePtr = Ptr msgType
      noArgs = length args
      targsTypes = map A.getType args
      expectedTypes = map A.ptype (A.hparams header)
      castedArguments = zipWith3 castArguments expectedTypes argNames targsTypes
      indexedArguments = map (indexArgument theMsgName) [1..]
      argAssignments = zipWith Assign indexedArguments castedArguments
      theArgInit = Seq $ map Statement argAssignments
      theCall = Call ponySendvName
                     [AsExpr encoreCtxVar,
                      Cast (Ptr ponyActorT) targetName,
                      Cast (Ptr ponyMsgT) $ AsExpr theMsgName]
      theMsgDecl =
          Assign (Decl (msgTypePtr, theMsgName)) $
                 Cast msgTypePtr $ Call ponyAllocMsgName
                                 [msgSize msgType
                                 ,AsExpr . AsLval $ oneWayMsgId targetType name]
      argsTypes = zip indexedArguments (map A.getType args)
      theTrace = gcSend argsTypes expectedTypes
                        [(Comm "Not tracing the future in a oneWay send")]
  return $ Seq $ argDecls ++
                 theMsgDecl :
                 theArgInit :
                 theTrace ++
                 [Statement theCall]

callTheMethodFuture = callTheMethodForName methodImplFutureName

callTheMethodOneway = callTheMethodForName methodImplOneWayName

callTheMethodStream = callTheMethodForName methodImplStreamName

callTheMethodSync targetName targetType methodName args resultType = do
  (initArgs, expr) <- callTheMethodForName methodImplName
    targetName targetType methodName args resultType
  header <- gets $ Ctx.lookupMethod targetType methodName
  return (initArgs, convertBack (A.htype header) expr)
  where
    convertBack retType
      | Ty.isTypeVar retType && (not . Ty.isTypeVar) resultType =
          AsExpr . fromEncoreArgT (translate resultType)
      | otherwise = id

callTheMethodForName ::
  (Ty.Type -> ID.Name -> CCode Name) ->
  CCode Lval -> Ty.Type -> ID.Name -> [A.Expr] -> Ty.Type
  -> State Ctx.Context ([CCode Stat], CCode CCode.Main.Expr)
callTheMethodForName
  genCMethodName targetName targetType methodName args resultType = do
  (args', initArgs) <- fmap unzip $ mapM translate args
  header <- gets $ Ctx.lookupMethod targetType methodName
  return (initArgs,
      Call cMethodName $
        map AsExpr [encoreCtxVar, targetName] ++
        doCast (map A.ptype (A.hparams header)) args'
    )
  where
    cMethodName = genCMethodName targetType methodName
    actualArgTypes = map A.getType args
    doCast expectedArgTypes args =
      zipWith3 castArguments expectedArgTypes args actualArgTypes
    convertBack retType
      | Ty.isTypeVar retType && (not . Ty.isTypeVar) resultType =
          AsExpr . fromEncoreArgT (translate resultType)
      | otherwise = id

passiveMethodCall :: CCode Lval -> Ty.Type -> ID.Name -> [A.Expr] -> Ty.Type
  -> State Ctx.Context ([CCode Stat], CCode CCode.Main.Expr)
passiveMethodCall targetName targetType name args resultType = do
  targs <- mapM translate args
  header <- gets $ Ctx.lookupMethod targetType name
  let targsTypes = map A.getType args
      expectedTypes = map A.ptype (A.hparams header)
      (argNames, argDecls) = unzip targs
      castedArguments = zipWith3 castArguments expectedTypes argNames targsTypes
      theCall =
          if Ty.isTypeVar (A.htype header) then
              AsExpr $ fromEncoreArgT (translate resultType)
                                      (Call (methodImplName targetType name)
                                      (AsExpr encoreCtxVar :
                                       AsExpr targetName : castedArguments))
          else
              Call (methodImplName targetType name)
                   (AsExpr encoreCtxVar : AsExpr targetName : castedArguments)
  return (argDecls, theCall)


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
    callF f this args = Call f $ encoreCtxVar:this:args
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
    callF f this args = Statement $ Call f $ encoreCtxVar:this:args

traitMethod this targetType name args resultType =
  let
    id = msgId targetType name
    tyStr = Ty.getId targetType
    nameStr = show name
  in
    do
      f <- Ctx.genNamedSym $ concat [tyStr, "_", nameStr]
      vtable <- Ctx.genNamedSym $ concat [tyStr, "_", "vtable"]
      tmp <- Ctx.genNamedSym "trait_method_call"
      (args, initArgs) <- fmap unzip $ mapM translate args
      return $ (Var tmp,
        Seq $
          initArgs ++
          [declF f] ++
          [declVtable vtable] ++
          [initVtable this vtable] ++
          [initF f vtable id] ++
          [ret tmp $ callF f this args]
        )
  where
    thisType = translate targetType
    argTypes = map (translate . A.getType) args
    declF f = FunPtrDecl resultType (Nam f) $ (Ptr encoreCtxT):thisType:argTypes
    declVtable vtable = FunPtrDecl (Ptr void) (Nam vtable) [Typ "int"]
    vtable this = this `Arrow` selfTypeField `Arrow` Nam "vtable"
    initVtable this v = Assign (Var v) $ Cast (Ptr void) $ vtable this
    initF f vtable id = Assign (Var f) $ Call (Nam vtable) [id]
    callF f this args = Call (Nam f) $ AsExpr encoreCtxVar : Cast thisType this : map AsExpr args
    ret tmp fcall = Assign (Decl (resultType, Var tmp)) fcall

msgSize :: CCode Ty -> CCode Expr
msgSize t = Call (Nam "POOL_INDEX") [Sizeof t]
