{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| Makes @Expr@ an instance of @Translatable@ (see "CodeGen.Typeclasses") -}

module CodeGen.Expr () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import qualified CodeGen.CCodeNames as C
import CodeGen.Type
import qualified CodeGen.Context as Ctx
import CodeGen.DTrace

import CCode.Main
import CCode.PrettyCCode

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta
import qualified AST.PrettyPrinter as PP
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding (void)
import Data.List
import Data.List.Utils(split)
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace

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
  translate op = Nam $
    case op of
      ID.NOT -> "!"
      ID.NEG -> "-"

typeToPrintfFstr :: Ty.Type -> String
typeToPrintfFstr ty
    | Ty.isIntType ty          = "%lli"
    | Ty.isUIntType ty         = "%llu"
    | Ty.isRealType ty         = "%f"
    | Ty.isStringObjectType ty = "%s"
    | Ty.isStringType ty       = "%s"
    | Ty.isCharType ty         = "%c"
    | Ty.isBoolType ty         = "%s"
    | Ty.isRefAtomType ty      = show ty ++ "@%p"
    | Ty.isCapabilityType ty   = "(" ++ show ty ++ ")@%p"
    | Ty.isUnionType ty        = "(" ++ show ty ++ ")@%p"
    | Ty.isFutureType ty       = "Fut@%p"
    | Ty.isStreamType ty       = "Stream@%p"
    | Ty.isParType ty          = "Par@%p"
    | Ty.isArrowType ty        = "(" ++ show ty ++ ")@%p"
    | Ty.isArrayType ty        = show ty ++ "@%p"
    | Ty.isRangeType ty        = "[%d..%d by %d]"
    | Ty.isTupleType ty        =
        let argFormats = map typeToPrintfFstr (Ty.getArgTypes ty) :: [String]
            formatString = intercalate ", " argFormats
        in "(" ++ formatString ++ ")"
    | Ty.isMaybeType ty        = "%s" -- A generated string
    | Ty.isUnitType ty         = "%s" -- Always "()"
    | otherwise = case translate ty of
                    Ptr _ -> "%p"
                    _ -> error $ "Expr.hs: typeToPrintfFstr not defined for " ++ show ty

-- | If the type is not void, create a variable to store it in. If it is void, return the lval UNIT
namedTmpVar :: String -> Ty.Type -> CCode Expr -> State Ctx.Context (CCode Lval, CCode Stat)
namedTmpVar name ty cex
    | Ty.isUnitType ty = return $ (unit, Seq [cex])
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

getRuntimeType :: A.Expr -> CCode Expr
getRuntimeType = runtimeType . Ty.getResultType . A.getType

newParty :: A.Expr -> CCode Name
newParty (A.PartyPar {}) = partyNewParP
newParty _ = error "Expr.hs: node is not 'PartyPar'"

translateDecl (vars, expr) = do
  (ne, te) <- translate expr
  let exprType = A.getType expr
  tmp <- Var <$> Ctx.genSym
  theAssigns <- mapM (assignDecl ne exprType) vars
  return (tmp,
          [Comm $ intercalate ", " (map (show . A.varName) vars) ++
                  " = " ++ show (PP.ppSugared expr)
          ,te
          ] ++ theAssigns)
  where
    assignDecl rhs rhsType var = do
      let x = A.varName var
      tmp <- Var <$> Ctx.genNamedSym (show x)
      substituteVar x tmp
      let (lhsType, theRhs) =
            case var of
              A.VarType{A.varType} ->
                (translate varType, Cast (translate varType) rhs)
              A.VarNoType{} ->
                (translate rhsType, AsExpr rhs)
      return $ Assign (Decl (lhsType, tmp)) theRhs

instance Translatable A.Expr (State Ctx.Context (CCode Lval, CCode Stat)) where
  -- | Translate an expression into the corresponding C code
  translate skip@(A.Skip {}) = namedTmpVar "skip" (A.getType skip) (AsExpr unit)
  translate A.Break{} = return (unit, Break{})
  translate A.Continue{} = return (unit, Continue{})
  translate null@(A.Null {}) = namedTmpVar "literal" (A.getType null) Null
  translate true@(A.BTrue {}) = namedTmpVar "literal"  (A.getType true) (Embed "1/*True*/"::CCode Expr)
  translate false@(A.BFalse {}) = namedTmpVar "literal" (A.getType false) (Embed "0/*False*/"::CCode Expr)
  translate lit@(A.IntLiteral {A.intLit = i}) = namedTmpVar "literal" (A.getType lit) (Int i)
  translate lit@(A.UIntLiteral {A.intLit = i}) = namedTmpVar "literal" (A.getType lit) (Int i)
  translate lit@(A.RealLiteral {A.realLit = r}) = namedTmpVar "literal" (A.getType lit) (Double r)
  translate lit@(A.StringLiteral {A.stringLit = s}) = namedTmpVar "literal" (A.getType lit) (String s)
  translate lit@(A.CharLiteral {A.charLit = c}) = namedTmpVar "literal" (A.getType lit) (Char c)

  translate A.TypedExpr {A.body, A.ty} = do
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
    (nlo, tlo) <- translate loper
    (nro, tro) <- translate roper
    tmp <- Ctx.genNamedSym "binop"
    let ltype = A.getType loper
        le = wrap ltype nlo tlo
        re' = wrap ltype nro tro
        re = if Ty.isRefType ltype
             then Cast (translate ltype) re'
             else re'
        theAssign = Assign (Decl (translate $ A.getType bin, Var tmp))
                           (BinOp (translate binop) le re)
    return (Var tmp, Statement theAssign)
    where
      wrap ty n t =
          StatAsExpr (if Ty.isTypeVar ty
                      then fromEncoreArgT (Ptr void) (AsExpr n)
                      else n) t

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
    let runtimeT = (runtimeType . Ty.getResultType . A.getType) ps
    (nResultPar, tResultPar) <- namedTmpVar "par" (A.getType ps) $
                                Call partySequence [AsExpr encoreCtxVar,
                                                    AsExpr npar,
                                                    AsExpr nseqfunc,
                                                    runtimeT]
    return (nResultPar, Seq [tpar, tseqfunc, tResultPar])

  translate ps@(A.PartyReduce {A.seqfun, A.pinit, A.par, A.runassoc}) = do
    (nseqfunc, tseqfunc) <- translate seqfun
    (npinit, tpinit) <- translate pinit
    (npar, tpar) <- translate par
    let npinit'  = asEncoreArgT (translate $ A.getType pinit) npinit
        runtimeT = (runtimeType . Ty.getResultType . A.getType) ps
        reduceFn = partyReduce runassoc
    (nResultPar, tResultPar) <- namedTmpVar "par" (A.getType ps) $
                                Call reduceFn [AsExpr encoreCtxVar
                                              ,AsExpr npar
                                              ,npinit'
                                              ,AsExpr nseqfunc
                                              ,runtimeT]
    return (nResultPar, Seq [tseqfunc, tpinit, tpar, tResultPar])

  translate (A.Print {A.args, A.file}) = do
      let string = head args
          rest = tail args
      unless (Ty.isStringType $ A.getType string) $
          error $ "Expr.hs: Print expects first argument to be a string literal"
      targs <- mapM translate rest
      let argNames = map (AsExpr . fst) targs
          argDecls = map snd targs
          argTys   = map A.getType rest
          fstring  = formatString (A.stringLit string) argTys
          expandedArgs = concat $ zipWith expandPrintfArg argTys argNames
          outputOn = case file of
                       A.Stdout -> C.stdout
                       A.Stderr -> C.stderr
      return (unit,
              Seq $ argDecls ++
                    [Statement
                       (Call (Nam "fprintf")
                       (AsExpr outputOn : String fstring : expandedArgs))])
      where
        formatString s [] = s
        formatString "" (ty:tys) =
            error "Expr.hs: Wrong number of arguments to printf"
        formatString ('{':'}':s) (ty:tys) =
            typeToPrintfFstr ty ++ formatString s tys
        formatString (c:s) tys =
            c : formatString s tys

        expandPrintfArg :: Ty.Type -> CCode Expr -> [CCode Expr]
        expandPrintfArg ty argName
          | Ty.isStringObjectType ty =
              let castArg = Cast (translate Ty.stringObjectType) argName
              in [AsExpr $ castArg `Arrow` fieldName (ID.Name "cstring")]
          | Ty.isBoolType ty =
              [Ternary argName (String "true") (String "false")]
          | Ty.isRangeType ty =
              map (`Call` [argName]) [rangeStart, rangeStop, rangeStep]
          | Ty.isTupleType ty =
              let argTypes = Ty.getArgTypes ty
                  args = zipWith (get argName) argTypes [0..]
              in
                concat $ zipWith expandPrintfArg argTypes args
          | Ty.isMaybeType ty =
              [Ternary (isNothing argName)
                        (String "Nothing") $
                        showJust argName (Ty.getResultType ty)]
          | Ty.isUnitType ty =
              [String "()"]
          | Ty.isNullType ty =
              [String "null"]
          | otherwise =
              [argName]
          where
            get tup ty i =
                AsExpr $ Call tupleGet [tup, Int i] `Dot`
                         encoreArgTTag (translate ty)
            isNothing arg =
                BinOp (Nam "==")
                      (Cast option arg `Arrow` Nam "tag")
                      (Var "NOTHING")
            showJust just resType =
                    let result = AsExpr $ Cast option just `Arrow`
                                 encoreArgTTag (translate resType)
                        len = BinOp (Nam "+") (Int 10)
                                    (approximateLength result resType)
                        resArgs = expandPrintfArg resType result
                        format = typeToPrintfFstr resType
                    in StatAsExpr (Var "tmp") $ Seq
                       [Assign (Decl (int, Var "len")) len,
                        Assign (Decl (Ptr char, Var "tmp")) $
                               Call encoreAllocName [Deref encoreCtxVar, Var "len"],
                        Statement . Call (Nam "sprintf") $
                             [AsExpr $ Var "tmp",
                              String $ "Just " ++ if Ty.isMaybeType resType
                                                  then "(" ++ format ++ ")"
                                                  else format] ++
                              resArgs]
                    where
                      approximateLength result ty
                          | Ty.isTupleType ty =
                              foldr1 (BinOp (Nam "+")) $
                              zipWith approximateLength
                                      (zipWith (get result)
                                               (Ty.getArgTypes ty) [0..])
                                      (Ty.getArgTypes ty)
                          | Ty.isStringObjectType ty =
                              AsExpr $
                              Cast (translate Ty.stringObjectType) result
                                   `Arrow` fieldName (ID.Name "length")
                          | otherwise =
                              Int $ length (show ty) + 30

  translate exit@(A.Exit {A.args = [arg]}) = do
      (narg, targ) <- translate arg
      let exitCall = Call (Nam "exit") [narg]
      return (unit, Seq [Statement targ, Statement exitCall])

  translate abort@(A.Abort {A.args = []}) = do
      let abortCall = Call (Nam "abort") ([]::[CCode Lval])
      return (unit, Statement abortCall)

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
    let theAssign = Assign lval castRhs
    return (unit, Seq [trhs, theAssign])
      where
        upCast e = if needUpCast then cast e else AsExpr e
          where
            lhsType = A.getType lhs
            rhsType = A.getType rhs
            needUpCast = rhsType /= lhsType
            cast = Cast (translate lhsType)
        mkLval (A.VarAccess {A.qname}) =
           do ctx <- get
              case Ctx.substLkp ctx qname of
                Just substName -> return substName
                Nothing -> error $ "Expr.hs: LVal is not assignable: " ++
                                   show qname
        mkLval (A.FieldAccess {A.target, A.name}) =
           do (ntarg, ttarg) <- translate target
              let ttargTrace = Seq [ttarg
                                   ,dtraceFieldWrite ntarg name
                                   ]
              return (Deref (StatAsExpr ntarg ttargTrace) `Dot` fieldName name)
        mkLval e = error $ "Cannot translate '" ++ show e ++ "' to a valid lval"

  translate mayb@(A.MaybeValue _ (A.JustData e)) = do
    (nE, tE) <- translate e
    let runtimeT = (runtimeType . A.getType) e
    let bodyDecl = asEncoreArgT (translate $ A.getType e) nE
        optionLval = Call optionMkFn [AsExpr encoreCtxVar, AsExpr just,
                                      bodyDecl, runtimeT]
    (nalloc, talloc) <- namedTmpVar "option" (A.getType mayb) optionLval
    return (nalloc, Seq [tE, talloc])

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
        eAlloc = Call tupleMkFn [AsExpr encoreCtxVar, tupLen]
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

  translate (A.VarAccess {A.qname}) = do
      c <- get
      case Ctx.substLkp c qname of
        Just substName ->
            return (substName , Skip)
        Nothing -> do
          (_, header) <- gets $ Ctx.lookupFunction qname
          let name = resolveFunctionSource header
          return (Var . show $ globalClosureName name, Skip)
      where
        resolveFunctionSource header =
          case ID.qnsource qname of
            Just source ->
                ID.setSourceFile source $
                ID.qLocal $ A.hname header
            Nothing ->
                ID.qLocal $ A.hname header

  translate fun@(A.FunctionAsValue {A.typeArgs}) = do
    tmp <- Var <$> Ctx.genSym
    let funName = functionAsValueWrapperNameOf fun
    (rtArray, rtArrayInit) <- runtimeTypeArguments typeArgs
    return (tmp,
            Seq $
             rtArrayInit:
              [Assign (Decl (closure, tmp))
              (Call closureMkFn [encoreCtxVar, AsLval funName,
                                 nullVar, nullVar, rtArray])])

  translate (A.Consume {A.target = arrAcc@A.ArrayAccess{A.target, A.index}}) = do
    (ntarg, ttarg) <- translate target
    (nindex, tindex) <- translate index
    accessName <- Ctx.genNamedSym "access"
    let ty = translate $ A.getType arrAcc
        theAccess =
            Assign (Decl (ty, Var accessName))
                   (Call (Nam "array_get") [ntarg, nindex]
                    `Dot` encoreArgTTag ty)
        theSet =
            Statement $
            Call (Nam "array_set")
                 [AsExpr ntarg, AsExpr nindex, asEncoreArgT ty Null]
    return (Var accessName, Seq [ttarg, tindex, theAccess, theSet])

  translate (A.Consume {A.target}) = do
    (ntarg, ttarg) <- translate target
    lval <- mkLval target
    accessName <- Ctx.genNamedSym "consume"
    let ty = translate $ A.getType target
        theRead = Assign (Decl (ty, Var accessName)) ntarg
        theConsume = if Ty.isMaybeType $ A.getType target
                     then Assign lval $ Amp (Nam "DEFAULT_NOTHING")
                     else Assign lval Null
    return (Var accessName, Seq [ttarg, theRead, theConsume])
         where
           mkLval (A.VarAccess {A.qname}) =
               do ctx <- get
                  case Ctx.substLkp ctx qname of
                    Just substName -> return substName
                    Nothing -> error $ "Expr.hs: Unbound variable: " ++ show qname
           mkLval (A.FieldAccess {A.target, A.name}) =
               do (ntarg, ttarg) <- translate target
                  return (Deref (StatAsExpr ntarg ttarg) `Dot` fieldName name)
           mkLval e = error $ "Cannot translate '" ++ show e ++ "' to a valid lval"

  translate acc@(A.FieldAccess {A.target, A.name}) = do
    (ntarg,ttarg) <- translate target
    tmp <- Ctx.genNamedSym "fieldacc"
    fld <- gets $ Ctx.lookupField (A.getType target) name
    let theAccess = if Ty.isTypeVar (A.ftype fld) then
                        fromEncoreArgT (translate . A.getType $ acc) $
                                       AsExpr (Deref ntarg `Dot` fieldName name)
                    else
                        Deref ntarg `Dot` fieldName name
        theAssign = Assign (Decl (translate (A.getType acc), Var tmp)) theAccess
    return (Var tmp, Seq [ttarg
                         ,dtraceFieldAccess ntarg name
                         ,theAssign
                         ])

  translate acc@(A.TupleAccess {A.target, A.compartment}) = do
    (ntarg,ttarg) <- translate target
    tmp <- Ctx.genNamedSym "tupleacc"
    let ty = translate $ (A.getType acc)
        theValue = fromEncoreArgT ty $ (Call tupleGet [AsExpr ntarg, Int compartment])
        theDecl = Decl (ty, Var tmp)
        theAssign = Assign theDecl theValue
    return (Var tmp, Seq [ttarg
--                         ,dtraceFieldAccess ntarg $ Int compartment
                         ,theAssign
                         ])

  translate (A.Let {A.decls, A.body}) = do
    tmpsTdecls <- mapM translateDecl decls
    let (tmps, tdecls) = unzip tmpsTdecls
    (nbody, tbody) <- translate body
    mapM_ (mapM_ (unsubstituteVar . A.varName) . fst) decls
    return (nbody, Seq $ concat tdecls ++ [tbody])

  translate new@(A.NewWithInit {A.ty, A.args})
    | Ty.isActiveSingleType ty = delegateUse callTheMethodOneway
    | Ty.isSharedSingleType ty = delegateUse callTheMethodOneway
    | otherwise = delegateUse callTheMethodSync
    where
      delegateUse methodCall =
        let
          fName = constructorImplName ty
          callCtor = Call fName [encoreCtxName, nullName]
          typeParams = Ty.getTypeParameters ty
          callTypeParamsInit args = Call (runtimeTypeInitFnName ty) args
        in
          do
            let typeArgs = map runtimeType typeParams
            (nnew, constructorCall) <- namedTmpVar "new" ty callCtor
            (initArgs, result) <-
              methodCall nnew ty ID.constructorName args [] ty
            return (nnew,
              Seq $
                [constructorCall] ++
                initArgs ++
                [ Statement $ callTypeParamsInit $ AsExpr nnew:typeArgs
                , Statement result]
              )

  translate arrNew@(A.ArrayNew {A.ty, A.size}) = do
    arrName <- Ctx.genNamedSym "array"
    (nsize, tsize) <- translate size
    let ty' = runtimeType ty
        theArrayDecl =
          Assign (Decl (array, Var arrName))
            (Call arrayMkFn [AsExpr encoreCtxVar, AsExpr nsize, ty'])
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
         let runtimeT = runtimeType ty
             theArrayDecl =
                Assign (Decl (array, Var arrName))
                       (Call arrayMkFn [AsExpr encoreCtxVar, Int len, runtimeT])
             theArrayContent = Seq $ map (\(_, targ) -> targ) targs
             theArraySets = snd $ mapAccumL (arraySet arrName ty) 0 targs
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

  translate call@(A.MethodCall { A.emeta, A.target, A.name, A.typeArguments, A.args})
    | (Ty.isTraitType . A.getType) target ||
      (Ty.isUnionType . A.getType) target ||
      (Ty.isTypeVar   . A.getType) target = do
        (ntarget, ttarget) <- translate target
        let ntarget' = if Ty.isTypeVar $ A.getType target
                       then Cast capability (ntarget `Dot` Nam "p")
                       else AsExpr ntarget
        (nCall, tCall) <- traitMethod msgId ntarget' (A.getType target) name
                              typeArguments args (translate (A.getType call))
        let recvNullCheck = targetNullCheck ntarget' target name emeta "."
        return (nCall, Seq [ttarget
                           ,recvNullCheck
                           ,tCall
                           ])
    | syncAccess = delegateUse callTheMethodSync "sync_method_call"
    | sharedAccess = delegateUse callTheMethodFuture "shared_method_call"
    | otherwise = error $ "Expr.hs: Don't know how to call target of type " ++
                          Ty.showWithKind targetTy ++
                          " at " ++ Meta.showPos (A.getMeta call)
        where
          targetTy = A.getType target
          retTy = A.getType call
          delegateUse methodCall sym = do
            result <- Ctx.genNamedSym sym
            (ntarget, ttarget) <- translate target
            (initArgs, resultExpr) <-
              methodCall ntarget targetTy name args typeArguments retTy
            return (Var result,
              Seq $
                ttarget :
                targetNullCheck (AsExpr ntarget) target name emeta "." :
                initArgs ++
                [Assign (Decl (translate retTy, Var result)) resultExpr]
              )
          syncAccess = A.isThisAccess target ||
                       (Ty.isPassiveRefType . A.getType) target
          sharedAccess = Ty.isSharedSingleType $ A.getType target

  translate call@A.MessageSend{A.emeta, A.target, A.name, A.args, A.typeArguments}
    | (Ty.isTraitType . A.getType) target ||
      (Ty.isUnionType . A.getType) target = do
        (ntarget, ttarget) <- translate target
        let idFun = if Util.isStatement call
                    then oneWayMsgId
                    else futMsgId
        (nCall, tCall) <- traitMethod idFun ntarget (A.getType target) name
                              typeArguments args (translate (A.getType call))
        let recvNullCheck = targetNullCheck (AsExpr ntarget) target name emeta "."
        return (nCall, Seq [ttarget
                           ,recvNullCheck
                           ,tCall
                           ])
    | Util.isStatement call = delegateUseM callTheMethodOneway Nothing
    | isActive && isStream = delegateUseM callTheMethodStream (Just "stream")
    | otherwise = delegateUseM callTheMethodFuture (Just "fut")
    where
      targetTy = A.getType target
      isActive = Ty.isActiveSingleType targetTy
      isStream = Ty.isStreamType $ A.getType call

      delegateUseM msgSend sym = do
        (ntarget, ttarget) <- translate target
        (initArgs, resultExpr) <-
          msgSend ntarget targetTy name args typeArguments retTy
        (resultVar, handleResult) <- returnValue
        return (resultVar,
                Seq $ ttarget : targetNullCheck (AsExpr ntarget) target name emeta " ! " :
                      initArgs ++ [handleResult resultExpr])
        where
          retTy | isNothing sym = Ty.unitType
                | otherwise = A.getType call
          returnValue | isNothing sym = return (unit, Statement)
                      | otherwise = do
                         result <- Ctx.genNamedSym (fromJust sym)
                         return (Var result, Assign (Decl (translate retTy, Var result)))

  translate w@(A.DoWhile {A.cond, A.body}) = do
    (ncond,tcond) <- translate cond
    (_,tbody) <- translate body
    return (unit, DoWhile (StatAsExpr ncond tcond) (Statement tbody))

  translate w@(A.While {A.cond, A.body}) = do
    (ncond,tcond) <- translate cond
    (_,tbody) <- translate body
    return (unit, While (StatAsExpr ncond tcond) (Statement tbody))

  translate for@(A.For {A.name, A.step, A.src, A.body}) = do
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
        theBody = Seq [eltDecl, Statement bodyT, inc]
        theLoop = While cond theBody

    return (unit, Seq [srcT
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
         let resultType = A.getType ite
             exportThn = Seq $ tthn :
                         [Assign (Var tmp) (Cast (translate resultType) nthn)]
             exportEls = Seq $ tels :
                         [Assign (Var tmp) (Cast (translate resultType) nels)]
         return (Var tmp,
                 Seq [AsExpr $ Decl (translate (A.getType ite), Var tmp),
                      If (StatAsExpr ncond tcond) (Statement exportThn) (Statement exportEls)])

  translate m@(A.Match {A.arg, A.clauses}) =
      do retTmp <- Ctx.genNamedSym "match"
         (narg, targ) <- translate arg
         let argty = A.getType arg
             mType = translate (A.getType m)
         tIfChain <- ifChain clauses narg argty retTmp mType (A.getPos m)
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

        translateMaybePattern e derefedArg argty assocs = do
          optionVar <- Ctx.genNamedSym "optionVal"
          nCheck <- Ctx.genNamedSym "optionCheck"
          let eMaybeVal = AsExpr $ Dot derefedArg (Nam "val")
              valType = Ty.getResultType argty
              eMaybeField = fromEncoreArgT (translate valType) eMaybeVal
              tVal = Assign (Decl (translate valType, Var optionVar)) eMaybeField

          (nRest, tRest) <- translatePattern e (Var optionVar) valType assocs

          let expectedTag = AsExpr $ AsLval $ Nam "JUST"
              actualTag = AsExpr $ Dot derefedArg $ Nam "tag"
              eTagsCheck = BinOp (translate ID.EQ) expectedTag actualTag
              intTy = translate Ty.intType
              tDecl = Statement $ Decl (intTy, nRest)
              tRestWithDecl = Seq [tDecl, tVal, tRest]
              eRest = StatAsExpr nRest tRestWithDecl
              eCheck = BinOp (translate ID.AND) eTagsCheck eRest
              tCheck = Assign (Var nCheck) eCheck

          return (Var nCheck, tCheck)

        translateComparison e1 e2 ty
          | Ty.isStringType ty = do
              let strcmpCall = Call (Nam "strcmp") [e1, e2]
              return $ BinOp (translate ID.EQ) strcmpCall (Int 0)
          | Ty.isStringObjectType ty =
              -- TODO: this code generation should be autogenerated
              return $ Call (methodImplName Ty.stringObjectType (ID.Name "eq"))
                            [AsExpr encoreCtxVar, e1, AsExpr nullVar, e2]
          | otherwise =
              return (BinOp (translate ID.EQ) e1 e2)

        translatePattern e@A.AdtExtractorPattern{A.name
                                                ,A.arg = A.Tuple{A.args}
                                                ,A.adtClassDecl =
                                                   c@A.Class{A.cname
                                                            ,A.cfields}}
                         argName argty assocs = do
          let eSelfArg = AsExpr argName
              tag = Ty.getAdtTag cname
              typeName = Ptr $ AsType (classTypeName cname)
              castArgName = Cast typeName argName

          fieldVar <- Var <$> Ctx.genNamedSym "adtFieldCheck"
          tmp <- Var <$> Ctx.genNamedSym "adtPatternResult"
          tFields <-
            zipWithM (translateAdtField castArgName assocs fieldVar)
                     args (map (\f -> (A.fname f, A.ftype f)) (drop 1 cfields))

          let nullCheck = BinOp (translate ID.NEQ) eSelfArg Null
              actualTag = Cast typeName eSelfArg `Arrow` Nam "_enc__field__ADT_tag"
              tagCheck  = BinOp (translate ID.EQ) (AsExpr actualTag) (Int tag)
              eCheck    = BinOp (translate ID.AND) nullCheck tagCheck
              fwdDecl   = Assign (Decl (translate Ty.intType, fieldVar)) (Int 1)
              result    = BinOp (translate ID.AND)
                                eCheck (StatAsExpr fieldVar (Seq tFields))
              theAssign = Assign tmp result

          return (tmp, Seq [fwdDecl, theAssign])
          where
            translateAdtField argName assocs retVar field (formalName, formalType) = do
              let fieldType = A.getType field
                  fieldCType = translate fieldType
                  fieldAccess = argName `Arrow` fieldName formalName
                  fieldLookup =
                    if Ty.isTypeVar formalType
                    then fromEncoreArgT fieldCType (AsExpr fieldAccess)
                    else fieldAccess
              (nField, tField) <-
                translatePattern field fieldLookup fieldType assocs
              let theAnd =
                    BinOp (translate ID.AND) (AsExpr retVar) (AsExpr nField)
                  theRetAssign = Assign retVar theAnd
                  theHelpDecl = Statement $ Decl (translate Ty.intType, nField)
              return $ Seq [theHelpDecl, tField, theRetAssign]

        translatePattern (A.ExtractorPattern {A.name, A.arg}) narg argty assocs = do
          let eSelfArg = AsExpr narg
              eNullCheck = BinOp (translate ID.NEQ) eSelfArg Null
              innerTy = A.getType arg
              tmpTy = Ty.maybeType innerTy
              noArgs = [] :: [A.Expr]

          (nCall, tCall) <-
              if Ty.isCapabilityType argty ||
                 Ty.isUnionType argty
              then do
                calledType <- gets $ Ctx.lookupCalledType argty name
                -- TODO: should we pass the typeArguments? I don't think so because
                -- pattern matching a function header works only on methods with
                -- no arguments, and there is no meaning in allowing this at
                -- the time being
                traitMethod msgId narg calledType name [] noArgs (translate tmpTy)
              else do
                tmp <- Ctx.genNamedSym "extractedOption"
                (argDecls, theCall) <-
                    passiveMethodCall narg argty name noArgs tmpTy
                let theAssign = Assign (Decl (translate tmpTy, Var tmp)) theCall
                return (Var tmp, Seq $ argDecls ++ [theAssign])

          let derefedCall = Deref nCall
          (nRest, tRest) <-
              translateMaybePattern arg derefedCall tmpTy assocs

          nCheck <- Ctx.genNamedSym "extractoCheck"
          let tDecl = Statement $ Decl (translate Ty.intType, nRest)
              tRestWithDecl = Seq [tDecl, tCall, tRest]
              eCheck = BinOp (translate ID.AND) eNullCheck $ StatAsExpr nRest tRestWithDecl
              tAssign = Assign (Var nCheck) eCheck

          return (Var nCheck, tAssign)

        translatePattern (tuple@A.Tuple {A.args}) larg argty assocs = do
          tmp <- Ctx.genNamedSym "tupleCheck"

          let elemTypes = Ty.getArgTypes $ A.getType tuple
              elemInfo = zip elemTypes args
              theInit = Assign (Var tmp) (Int 1)

          tChecks <- checkElems elemInfo (Var tmp) larg assocs 0
          return (Var tmp, Seq $ theInit:tChecks)
            where
              checkElems [] _ _ _ _ = return []
              checkElems ((ty, arg):rest) retVar larg assocs index = do
                accessName <- Ctx.genNamedSym "tupleAccess"
                let elemTy = translate ty
                    theDecl = Decl (elemTy, Var accessName)
                    theCall = fromEncoreArgT elemTy (Call tupleGet [AsExpr larg, Int index])
                    theCast = Cast elemTy theCall
                    theAssign = Assign theDecl theCall

                (ncheck, tcheck) <- translatePattern arg (Var accessName) ty assocs
                tRest <- checkElems rest retVar larg assocs (index + 1)

                let theAnd = BinOp (translate ID.AND) (AsExpr retVar) (AsExpr ncheck)
                    theRetAssign = Assign retVar theAnd
                    theHelpDecl = Statement $ Decl (translate Ty.intType, ncheck)
                return (theAssign : theHelpDecl : tcheck : theRetAssign : tRest)

        translatePattern (A.MaybeValue {A.mdt=A.JustData{A.e}}) larg argty assocs = do
          let derefedArg = Deref larg
          translateMaybePattern e derefedArg argty assocs

        translatePattern (A.VarAccess{A.qname}) larg argty assocs = do
          tmp <- Ctx.genNamedSym "varBinding"
          let name = ID.qnlocal qname
              lVar = fromJust $ lookup (show name) assocs
              eArg = AsExpr larg
              tBindVar = Assign lVar eArg
              tBindRet = Assign (Var tmp) (Int 1)
              tBindAll = Seq [tBindVar, tBindRet]
          return (Var tmp, tBindAll)

        translatePattern value larg argty _ = do
          tmp <- Ctx.genNamedSym "valueCheck"
          (nvalue, tvalue) <- translate value
          let eValue = StatAsExpr nvalue tvalue
              eArg = AsExpr larg
          eComp <- translateComparison eValue eArg argty
          let tAssign = Assign (Var tmp) eComp
          return (Var tmp, tAssign)

        translateIfCond (A.MatchClause {A.mcpattern, A.mcguard})
                        narg argty assocs = do
          (nPattern, tPatternNoDecl) <- translatePattern mcpattern narg argty assocs

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

        ifChain [] _ _ _ _ pos = do
          let errorCode = Int 1
              exitCall = Statement $ Call (Nam "exit") [errorCode]
              errorMsg = String $ "*** Runtime error: No matching clause was found at " ++ show pos ++ " ***\n"
              errorPrint = Statement $ Call (Nam "fprintf") [AsExpr C.stderr, errorMsg]
          return $ Seq [errorPrint, exitCall]

        ifChain (clause:rest) narg argty retTmp retTy pos = do
          let freeVars = Util.foldrExp (\e a -> getExprVars e ++ a) [] (A.mcpattern clause)
          assocs <- mapM createAssoc freeVars
          thenExpr <- translateHandler clause retTmp assocs retTy
          elseExpr <- ifChain rest narg argty retTmp retTy pos
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

              getExprVars var@(A.VarAccess {A.qname}) =
                  [(ID.qnlocal qname, A.getType var)]
              getExprVars _ =
                  []

  translate e@(A.Embed {A.embedded}) = do
    translated <- liftM concat $ mapM translatePair embedded
    if Ty.isUnitType (A.getType e) then
        return (unit, Embed $ "({" ++ translated ++ "});")
    else
        namedTmpVar "embed" (A.getType e) (Embed $ "({" ++ translated ++ "})")
    where
      translatePair (code, e) = do
        interpolated <- translateInterpolated e
        return $ code ++ pp interpolated
      translateInterpolated A.Skip{} =
        return (Embed "")
      translateInterpolated A.VarAccess{A.qname} = do
        result <- gets (`Ctx.substLkp` qname)
        let var = fromMaybe (AsLval $ globalClosureName qname) result
        return $ AsExpr var
      translateInterpolated A.FieldAccess{A.name, A.target} = do
        targ <- translateInterpolated target
        return $ AsExpr $ targ `Arrow` fieldName name
      translateInterpolated e = do
        (ne, te) <- translate e
        return $ StatAsExpr ne te

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

  translate A.Forward{A.forwardExpr = expr@A.MessageSend{A.emeta
                                                       ,A.target
                                                       ,A.name
                                                       ,A.typeArguments
                                                       ,A.args}} = do
    isAsyncForward <- gets Ctx.isAsyncForward
    eCtx <- gets Ctx.getExecCtx
    let dtraceExit = getDtraceExit eCtx
    if isAsyncForward
    then do
      (ntarget, ttarget) <- translate target
      let targetType = A.getType target

      (initArgs, forwardingCall) <-
        callTheMethodForward [futVar]
          ntarget targetType name args typeArguments Ty.unitType

      (initArgs1, oneWayMsg) <-
        callTheMethodOneway
          ntarget targetType name args typeArguments Ty.unitType

      let nullCheck = targetNullCheck (AsExpr ntarget) target name emeta "."
          result =
            case eCtx of
              Ctx.ClosureContext clos -> []
              _ -> [dtraceExit, Return Skip]

      return (unit, Seq $
                      ttarget : nullCheck :
                      [Statement $
                       If futVar
                         (Seq $ initArgs ++ [Statement forwardingCall])
                         (Seq $ initArgs1 ++ [Statement oneWayMsg])] ++
                       result
             )
    else do
      (sendn, sendt) <- translate A.MessageSend{A.emeta
                                               ,A.target
                                               ,A.name
                                               ,A.typeArguments
                                               ,A.args}
      tmp <- Ctx.genSym
      let resultType = translate (Ty.getResultType $ A.getType expr)
          theGet = fromEncoreArgT resultType (Call futureGetActor [encoreCtxVar, sendn])
          result =
            case eCtx of
              Ctx.MethodContext mdecl ->
                (unit, Seq [sendt, dtraceExit, Return theGet])
              Ctx.ClosureContext clos ->
                let ty = (Ty.getResultType $ A.getType clos)
                in (Var tmp, Seq [sendt, Assign (Decl (resultType, Var tmp)) theGet])
              _ -> error "Expr.hs: No context to forward"
      return result

  translate A.Forward{A.emeta, A.forwardExpr = fchain@A.FutureChain{A.future, A.chain}} = do
    (nfuture,tfuture) <- translate future
    (nchain, tchain)  <- translate chain
    eCtx <- gets $ Ctx.getExecCtx
    isAsyncForward <- gets Ctx.isAsyncForward
    let ty = getRuntimeType chain
        dtraceExit = getDtraceExit eCtx
        result = case eCtx of
                    Ctx.ClosureContext clos -> []
                    _ -> [dtraceExit, Return Skip]
        futureChain =
          if Util.isForwardInExpr chain
          then Call futureChainActor
                    [AsExpr encoreCtxVar, AsExpr nfuture, ty, AsExpr nchain]
          else Call futureChainWithFut
                    [AsExpr encoreCtxVar, AsExpr nfuture, ty, AsExpr nchain
                    ,AsExpr futVar, AsExpr $ AsLval $ Nam "false"]
    when (A.isVarAccess chain) $
      unless (A.isIdClosure chain) $
        error $ "Expr.hs: The closure that contains forward must be defined in chain."
    if isAsyncForward
    then do
      return (unit, Seq $
                      [tfuture,
                       tchain,
                       Statement futureChain] ++
                       result)
    else do
      tmp <- Ctx.genSym
      result <- Ctx.genSym
      let nfchain = Var result
          resultType = translate (Ty.getResultType $ A.getType fchain)
          theGet = fromEncoreArgT resultType (Call futureGetActor [encoreCtxVar, nfchain])
      return $ (Var tmp, Seq $
                      [tfuture,
                       tchain,
                       (Assign (Decl (C.future, Var result))
                               (Call futureChainActor
                                 [AsExpr encoreCtxVar, AsExpr nfuture, ty, AsExpr nchain]
                               )),
                       Assign (Decl (resultType, Var tmp)) theGet])

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
      in return (unit, Seq [Statement eosCall, Return Skip])

  translate ret@(A.Return{A.val}) =
      do (nval, tval) <- translate val
         eCtx <- gets Ctx.getExecCtx
         isAsyncForward <- gets Ctx.isAsyncForward
         let theReturn =
              if isAsyncForward then
                  case eCtx of
                    Ctx.FunctionContext fun ->
                      let ty = A.getType fun
                      in [dtraceFunctionExit (A.functionName fun)
                          ,Statement $ Call futureFulfil [AsExpr encoreCtxVar, AsExpr futVar
                                                ,asEncoreArgT (translate ty) nval]
                          ,Return Skip]
                    Ctx.MethodContext mdecl ->
                      let ty = A.getType mdecl
                      in [dtraceMethodExit thisVar (A.methodName mdecl)
                          ,Statement $ Call futureFulfil [AsExpr encoreCtxVar, AsExpr futVar
                                                ,asEncoreArgT (translate ty) nval]
                          ,Return Skip]
                    Ctx.ClosureContext clos ->
                      let ty = (Ty.getResultType $ A.getType clos)
                      in [dtraceClosureExit
                          ,Statement $ Call futureFulfil [AsExpr encoreCtxVar, AsExpr futVar
                                                ,asEncoreArgT (translate ty) nval]
                          ,Return Skip]
                    _ -> error "Expr.hs: No context to return from"
              else
                  case eCtx of
                    Ctx.FunctionContext fun ->
                      [dtraceFunctionExit (A.functionName fun), Return nval]
                    Ctx.MethodContext mdecl ->
                      [dtraceMethodExit thisVar (A.methodName mdecl), Return nval]
                    Ctx.ClosureContext clos ->
                      let ty = (Ty.getResultType $ A.getType clos)
                      in [dtraceClosureExit,
                         Return $ asEncoreArgT (translate ty) nval]
                    _ -> error "Expr.hs: No context to return from"
         return (unit, Seq $ tval:theReturn)

  translate iseos@(A.IsEos{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genSym
         let theCall = Assign (Decl (bool, Var tmp)) (Call streamEos [encoreCtxVar, ntarg])
         return (Var tmp, Seq [ttarg, theCall])

  translate next@(A.StreamNext{A.target}) =
      do (ntarg, ttarg) <- translate target
         tmp <- Ctx.genSym
         let theCall = Assign (Decl (stream, Var tmp)) (Call streamGetNext [encoreCtxVar, ntarg])
         return (Var tmp, Seq [ttarg, theCall])

  translate await@(A.Await{A.val}) =
      do (nval, tval) <- translate val
         return (unit, Seq [tval, Statement $ Call futureAwait [encoreCtxVar, nval]])

  translate suspend@(A.Suspend{}) =
         return (unit, Seq [Call actorSuspend [encoreCtxVar]])

  translate futureChain@(A.FutureChain{A.future, A.chain}) = do
    (nfuture,tfuture) <- translate future
    (nchain, tchain)  <- translate chain
    result <- Ctx.genSym
    isAsyncForward <- gets Ctx.isAsyncForward
    let ty = getRuntimeType chain
    return $ (Var result,
      Seq $ [tfuture,
             tchain,
              if (Util.isForwardInExpr chain && isAsyncForward)
              then Statement $
                      (Call futureChainWithFut
                            [AsExpr encoreCtxVar, AsExpr nfuture, ty, AsExpr nchain,
                             AsExpr ((Deref envName) `Dot` futNam), AsExpr $ AsLval $ Nam "true"])
              else Assign (Decl (C.future, Var result))
                          (Call futureChainActor [AsExpr encoreCtxVar, AsExpr nfuture, ty, AsExpr nchain])
            ] ++
            if (Util.isForwardInExpr chain && isAsyncForward)
            then [assignVar futNam (Decl (C.future, Var result))]
            else [])
    where
      metaId    = Meta.getMetaId . A.getMeta $ chain
      envName   = closureEnvName metaId
      assignVar lhs rhs = Assign rhs ((Deref envName) `Dot` lhs)

  translate clos@(A.Closure{A.eparams, A.body}) = do
    tmp <- Ctx.genSym
    futClos <- Ctx.genNamedSym "fut_closure"
    globalFunctionNames <- gets Ctx.getGlobalFunctionNames
    isAsyncForward <- gets Ctx.isAsyncForward
    let bound = map (ID.qLocal . A.pname) eparams
        freeVars = filter (ID.isLocalQName . fst) $
                   Util.freeVariables bound body
        ty = runtimeType . A.getType $ body
    fillEnv <- insertAllVars freeVars fTypeVars
    return
      (Var tmp,
       Seq $
          mkEnv envName : fillEnv ++
          if isAsyncForward then
              if forwardInBody
              then [Assign (Decl (future, Var futClos))
                           (Call futureMkFn [AsExpr encoreCtxVar, ty])
                    ,assignVar futNam (Var futClos)
                    ,Assign (Decl (closure, Var tmp))
                           (Call closureMkFn [encoreCtxName, funNameAsync, envName, traceNameAsync, nullName])]
              else [Assign (Decl (closure, Var tmp))
                     (Call closureMkFn [encoreCtxName, funName, envName, traceName, nullName])]
           else
               [Assign (Decl (closure, Var tmp))
                 (Call closureMkFn [encoreCtxName, funName, envName, traceName, nullName])])
    where
      forwardInBody  = Util.isForwardInExpr body
      metaIdAsync    = metaId ++ "_async"
      idClos         = A.isIdClosure body
      funNameAsync   = if idClos || not forwardInBody then funName
                       else closureFunName metaIdAsync
      traceNameAsync = if idClos || not forwardInBody then traceName
                       else closureTraceName metaIdAsync
      metaId    = Meta.getMetaId . A.getMeta $ clos
      funName   = closureFunName metaId
      envName   = closureEnvName metaId
      traceName = closureTraceName metaId
      fTypeVars  = Util.freeTypeVars body
      mkEnv name =
        Assign (Decl (Ptr $ Struct name, AsLval name))
          (Call encoreAllocName [AsExpr (Deref encoreCtxVar), Sizeof $ Struct name])
      insertAllVars vars typeVars =
        liftM2 (++)
          (mapM insertVar vars)
          (filterM localTypeVar typeVars >>= mapM insertTypeVar)

      insertVar (name, _) = do
        c <- get

        let tname = fromMaybe (AsLval $ globalClosureName name)
                              (Ctx.substLkp c name)
        return $ assignVar (fieldName (ID.qnlocal name)) tname
      insertTypeVar ty = do
        c <- get
        let
          Just tname = Ctx.substLkp c name
          fName = typeVarRefName ty
        return $ assignVar fName tname
        where
          name = ID.qName $ Ty.getId ty
      assignVar :: (UsableAs e Expr) => CCode Name -> CCode e -> CCode Stat
      assignVar lhs rhs = Assign ((Deref envName) `Dot` lhs) rhs
      localTypeVar ty = do
        c <- get
        return $ isJust $ Ctx.substLkp c name
        where
          name = ID.qName $ Ty.getId ty

  translate fcall@(A.FunctionCall{A.qname, A.args}) = do
    ctx <- get
    case Ctx.substLkp ctx qname of
      Just clos -> closureCall clos fcall
      Nothing -> functionCall fcall

  translate other = error $ "Expr.hs: can't translate: '" ++ show other ++ "'"

getDtraceExit eCtx =
  case eCtx of
    Ctx.FunctionContext fun ->
      dtraceFunctionExit (A.functionName fun)
    Ctx.MethodContext mdecl ->
      dtraceMethodExit thisVar (A.methodName mdecl)
    Ctx.ClosureContext clos ->
      dtraceClosureExit
    _ -> error "Expr.hs: No context to forward from"

closureCall :: CCode Lval -> A.Expr ->
  State Ctx.Context (CCode Lval, CCode Stat)
closureCall clos fcall@A.FunctionCall{A.qname, A.args} = do
  targs <- mapM translateArgument args
  (tmpArgs, tmpArgDecl) <- tmpArr (Typ "value_t") targs
  (calln, theCall) <- namedTmpVar "clos" typ $
    AsExpr $
      fromEncoreArgT (translate typ) $
        Call closureCallName [encoreCtxVar, clos, tmpArgs]
  return (if Ty.isUnitType typ then unit else calln
         ,Seq [tmpArgDecl
              ,dtraceClosureCall qname (extractArgs tmpArgs (length args))
              ,theCall
              ])
    where
      typ = A.getType fcall
      translateArgument arg = do
        (ntother, tother) <- translate arg
        return $ asEncoreArgT (translate $ A.getType arg)
          (StatAsExpr ntother tother)
      extractArgs arr n = map (\i -> ArrAcc i arr) [0..n-1]

functionCall :: A.Expr -> State Ctx.Context (CCode Lval, CCode Stat)
functionCall fcall@A.FunctionCall{A.typeArguments = typeArguments
                                 ,A.qname
                                 ,A.args} = do
  (argNames, initArgs) <- unzip <$> mapM translate args
  (callVar, call) <- buildFunctionCallExpr args argNames
  let ret = if Ty.isUnitType typ then unit else callVar

  return (ret, Seq $ initArgs ++ [dtraceFunctionCall qname argNames
                                 ,call
                                 ])
  where
    typ = A.getType fcall
    buildFunctionCallExpr args cArgs = do
      let functionType = A.getArrowType fcall
          expectedTypes = Ty.getArgTypes functionType
          actualTypes = map A.getType args
          castedArguments = zipWith3 castArguments expectedTypes cArgs actualTypes

      let runtimeTypes = map runtimeType typeArguments
      (tmpType, tmpTypeDecl) <- tmpArr (Ptr ponyTypeT) runtimeTypes
      (cname, header) <- gets (Ctx.lookupFunction qname)
      let runtimeTypeVar = if null runtimeTypes then nullVar else tmpType
          prototype = Call cname
                           (map AsExpr [encoreCtxVar, runtimeTypeVar] ++ castedArguments)
      rPrototype <- unwrapReturnType prototype
      (callVar, call) <- namedTmpVar "fun_call" typ rPrototype
      return (callVar, Seq [tmpTypeDecl, call])

    unwrapReturnType functionCall = do
      -- this function checks if the formal parameter return type is a type variable
      -- and, if so, unwraps it (adds the .i, .p or .d)
      (_, header) <- gets (Ctx.lookupFunction qname)
      let formalReturnType = A.htype header
      return (if Ty.isTypeVar formalReturnType then
               AsExpr (fromEncoreArgT (translate typ) functionCall)
              else functionCall)

indexArgument msgName i = Arrow msgName (Nam $ "f" ++ show i)

callTheMethodFuture = callTheMethodForName callMethodFutureName

callTheMethodForward extras =
  callTheMethodForNameWithExtraArguments extras methodImplForwardName

callTheMethodOneway = callTheMethodForName methodImplOneWayName

callTheMethodStream = callTheMethodForName methodImplStreamName

callTheMethodSync targetName targetType methodName args typeargs resultType = do
  (initArgs, expr) <- callTheMethodForName methodImplName
    targetName targetType methodName args typeargs resultType
  header <- gets $ Ctx.lookupMethod targetType methodName
  return (initArgs
         ,convertBack (A.htype header) expr)
  where
    convertBack retType
      | Ty.isTypeVar retType && (not . Ty.isTypeVar) resultType =
          AsExpr . fromEncoreArgT (translate resultType)
      | otherwise = id

callTheMethodForName ::
  (Ty.Type -> ID.Name -> CCode Name) ->
  CCode Lval -> Ty.Type -> ID.Name -> [A.Expr] -> [Ty.Type] -> Ty.Type
  -> State Ctx.Context ([CCode Stat], CCode CCode.Main.Expr)
callTheMethodForName = callTheMethodForName' []

callTheMethodForNameWithExtraArguments ::
  [CCode Lval] ->
  (Ty.Type -> ID.Name -> CCode Name) ->
  CCode Lval -> Ty.Type -> ID.Name -> [A.Expr] -> [Ty.Type] -> Ty.Type
  -> State Ctx.Context ([CCode Stat], CCode CCode.Main.Expr)
callTheMethodForNameWithExtraArguments = callTheMethodForName'

callTheMethodForName' ::
  [CCode Lval] ->
  (Ty.Type -> ID.Name -> CCode Name) ->
  CCode Lval -> Ty.Type -> ID.Name -> [A.Expr] -> [Ty.Type] -> Ty.Type
  -> State Ctx.Context ([CCode Stat], CCode CCode.Main.Expr)
callTheMethodForName'
  extraArguments
  genCMethodName targetName targetType methodName args typeargs resulttype = do
  (args', initArgs) <- unzip <$> mapM translate args
  header <- gets $ Ctx.lookupMethod targetType methodName

  -- translate actual method type variables
  let runtimeTypes = map runtimeType typeargs
  (tmpType, tmpTypeDecl) <- tmpArr (Ptr ponyTypeT) runtimeTypes
  let runtimeTypeVar = if null runtimeTypes then nullVar else tmpType
  return (initArgs ++ [tmpTypeDecl],
           Call cMethodName $
             map AsExpr [encoreCtxVar, targetName, runtimeTypeVar] ++
             doCast (map A.ptype (A.hparams header)) args' ++
             map AsExpr extraArguments
    )
  where
    cMethodName = genCMethodName targetType methodName
    actualArgTypes = map A.getType args
    doCast expectedArgTypes args =
      zipWith3 castArguments expectedArgTypes args actualArgTypes
    arrMethodTypeVars formaltys actualtys =
      let arrName = "methodTypeVars"
          arr = map (AsExpr . AsLval . typeVarRefName) actualtys
      in (Var arrName, Assign
                       (Decl (Ptr ponyTypeT, Var $ arrName ++ "[]"))
                       (Record arr))

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
                                       AsExpr targetName :
                                       AsExpr nullVar : castedArguments))
          else
              Call (methodImplName targetType name)
                   (AsExpr encoreCtxVar :
                      AsExpr targetName : AsExpr nullVar : castedArguments)
  return (argDecls, theCall)

castArguments :: Ty.Type -> CCode Lval -> Ty.Type -> CCode Expr
castArguments expected targ targType
  | Ty.isTypeVar expected = asEncoreArgT (translate targType) $ AsExpr targ
  | targType /= expected = Cast (translate expected) targ
  | otherwise = AsExpr targ

traitMethod idFun this targetType name typeargs args resultType =
  let
    id = idFun targetType name
    tyStr = Ty.getId targetType
    nameStr = show name
  in
    do
      f <- Ctx.genNamedSym $ concat [tyStr, "_", nameStr]
      vtable <- Ctx.genNamedSym $ concat [tyStr, "_", "vtable"]
      tmp <- Ctx.genNamedSym "trait_method_call"
      (args, initArgs) <- unzip <$> mapM translate args

      let runtimeTypes = map runtimeType typeargs
      (tmpType, tmpTypeDecl) <- tmpArr (Ptr ponyTypeT) runtimeTypes
      let tmpType' = if null runtimeTypes then nullVar else tmpType

      return (Var tmp,
        Seq $
          initArgs ++
          [declF f
          ,declVtable vtable
          ,initVtable this vtable
          ,initF f vtable id
          ,tmpTypeDecl
          ,ret tmp $ callF f this args tmpType'
          ]
        )
  where
    argTypes = map (translate . A.getType) args
    declF f = FunPtrDecl resultType (Nam f) $
                Ptr (Ptr encoreCtxT):capability: Ptr (Ptr ponyTypeT): argTypes
    declVtable vtable = FunPtrDecl (Ptr void) (Nam vtable) [Typ "int"]
    vtable this = this `Arrow` selfTypeField `Arrow` Nam "vtable"
    initVtable this v = Assign (Var v) $ Cast (Ptr void) $ vtable this
    initF f vtable id = Assign (Var f) $ Call (Nam vtable) [id]
    callF f this args typeArgs = Call (Nam f) $ AsExpr encoreCtxVar : Cast capability this :
                                       AsExpr typeArgs : map AsExpr args
    ret tmp fcall = Assign (Decl (resultType, Var tmp)) fcall

targetNullCheck ntarget target name meta op =
  Statement $
    Call (Nam "check_receiver")
      [ntarget,
       String op,
       String (show (PP.ppExpr target)),
       String (show name),
       String (Meta.showPos meta)]

runtimeTypeArguments [] = return (nullVar, Skip)
runtimeTypeArguments typeArgs = do
  tmpArray <- Var <$> Ctx.genNamedSym "rt_array"
  let runtimeTypes = map runtimeType typeArgs
      rtArraySize = BinOp (translate ID.TIMES)
                          (Int $ length typeArgs) (Sizeof $ Ptr ponyTypeT)
      rtArrayDecl = Decl (Ptr . Ptr $ ponyTypeT, tmpArray)
      rtArrayAlloc = Call encoreAllocName [AsExpr $ Deref encoreCtxVar
                                          ,rtArraySize]
      rtArrayAssign = Assign rtArrayDecl rtArrayAlloc
      rtArrayInit = Seq $ zipWith (\i t -> Assign (ArrAcc i tmpArray) t)
                                  [0..] runtimeTypes
  return (tmpArray, Seq [rtArrayAssign, rtArrayInit])
