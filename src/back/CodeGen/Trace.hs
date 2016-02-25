{-# LANGUAGE GADTs #-}

module CodeGen.Trace (
  traceVariable
  , traceFuture
  , ponyGcSend
  , ponySendDone
  , tracefunCall
) where

import CCode.Main
import CodeGen.CCodeNames
import qualified Types as Ty
import CCode.PrettyCCode ()

-- TODO (kiko): remove this and use GC.hs instead
ponyGcSend :: CCode Stat
ponyGcSend = Statement $ Call ponyGcSendName ([] :: [CCode Expr])

-- TODO (kiko): remove this and use GC.hs instead
ponySendDone :: CCode Stat
ponySendDone = Statement $ Call ponySendDoneName ([] :: [CCode Expr])

traceFuture :: CCode Lval -> CCode Stat
traceFuture var = ponyTraceobject var futureTraceFn

traceVariable :: Ty.Type -> CCode Lval -> CCode Stat
traceVariable t var
  | Ty.isActiveClassType  t = ponyTraceactor var
  | Ty.isSharedClassType  t = ponyTraceactor var
  | Ty.isPassiveClassType t = ponyTraceobject var $ classTraceFnName t
  | Ty.isCapabilityType t = traceCapability var
  | Ty.isTraitType      t = traceCapability var
  | Ty.isFutureType     t = ponyTraceobject var futureTraceFn
  | Ty.isArrowType      t = ponyTraceobject var closureTraceFn
  | Ty.isArrayType      t = ponyTraceobject var arrayTraceFn
  | Ty.isStreamType     t = ponyTraceobject var streamTraceFn
  | Ty.isTypeVar        t = traceTypeVar t var
  | otherwise =
    Embed $ "/* Not tracing field '" ++ show var ++ "' */"

ponyTraceactor :: CCode Lval -> CCode Stat
ponyTraceactor var =
  Statement $ Call ponyTraceActor  [AsExpr encoreCtxVar, Cast (Ptr ponyActorT) var]

ponyTraceobject :: (UsableAs e Expr) => CCode Lval -> CCode e -> CCode Stat
ponyTraceobject var f =
  let
    toExpr :: (UsableAs e Expr) => CCode e -> CCode Expr
    toExpr e@(Nam _) = AsExpr . AsLval $ e
    toExpr e@(AsExpr _) = e
    toExpr _ = undefined
  in
    Statement $ Call ponyTraceObject  [AsExpr encoreCtxVar, AsExpr var, toExpr f]

encoreTracePolymorphicVariable :: CCode Lval -> CCode Lval  -> CCode Stat
encoreTracePolymorphicVariable var t =
  Statement $ Call (Nam "encore_trace_polymorphic_variable")  [t, var]

traceTypeVar :: Ty.Type -> CCode Lval -> CCode Stat
traceTypeVar t var =
  let
    runtimeType = Var "this" `Arrow` typeVarRefName t
  in
    encoreTracePolymorphicVariable var runtimeType

traceCapability :: CCode Lval -> CCode Stat
traceCapability var =
  let
    cap = Cast capability var
    traceFunPath = cap `Arrow` selfTypeField `Arrow` Nam "trace"
  in
    Statement $ Call traceFunPath [var]

tracefunCall :: (CCode Lval, Ty.Type) -> Ty.Type -> CCode Stat
tracefunCall (a, t) expectedType =
  let
    needToUnwrap = Ty.isTypeVar expectedType && not (Ty.isTypeVar t)
    var = if needToUnwrap then a `Dot` Nam "p" else a
  in
    Statement $ traceVariable t var
