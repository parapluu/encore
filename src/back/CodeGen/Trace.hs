module CodeGen.Trace (
  traceVariable
  , traceFuture
  , traceStream
  , tracefunCall
) where

import CCode.Main
import CodeGen.CCodeNames
import qualified Types as Ty
import CCode.PrettyCCode ()

traceFuture :: CCode Lval -> CCode Stat
traceFuture var = traceObject var futureTraceFn

traceStream :: CCode Lval -> CCode Stat
traceStream var = traceObject var streamTraceFn

traceVariable :: Ty.Type -> CCode Lval -> CCode Stat
traceVariable t var
  | Ty.isActiveClassType t  = traceActor var
  | Ty.isSharedClassType t  = traceActor var
  | Ty.isPassiveClassType t = traceObject var $ classTraceFnName t
  | Ty.isCapabilityType t   = traceCapability var
  | Ty.isFutureType t       = traceObject var futureTraceFn
  | Ty.isArrowType t        = traceObject var closureTraceFn
  | Ty.isArrayType t        = traceObject var arrayTraceFn
  | Ty.isTupleType t        = traceObject var tupleTraceFn
  | Ty.isStreamType t       = traceObject var streamTraceFn
  | Ty.isMaybeType t        = traceObject var optionTraceFn
  | Ty.isRangeType t        = traceObject var rangeTraceFn
  | Ty.isTypeVar t          = traceTypeVar t var
  | Ty.isCType t            = trace var -- Assume C data contains no pointers
  | otherwise =
    Embed $ "/* Not tracing field '" ++ show var ++ "' */"

trace :: CCode Lval -> CCode Stat
trace var =
  Statement $ Call ponyTrace [Deref encoreCtxVar, var]

traceActor :: CCode Lval -> CCode Stat
traceActor var =
  Statement $ Call ponyTraceActor  [AsExpr (Deref encoreCtxVar), Cast (Ptr ponyActorT) var]

traceObject :: (UsableAs e Expr) => CCode Lval -> CCode e -> CCode Stat
traceObject var f =
  let
    toExpr :: (UsableAs e Expr) => CCode e -> CCode Expr
    toExpr e@(Nam _) = AsExpr . AsLval $ e
    toExpr e@(AsExpr _) = e
    toExpr _ = undefined
  in
    Statement $ Call ponyTraceObject [AsExpr (Deref encoreCtxVar), AsExpr var, toExpr f]

encoreTracePolymorphicVariable :: CCode Lval -> CCode Lval  -> CCode Stat
encoreTracePolymorphicVariable var t =
  Statement $ Call (Nam "encore_trace_polymorphic_variable")
    [Deref encoreCtxVar, t, var]

traceTypeVar :: Ty.Type -> CCode Lval -> CCode Stat
traceTypeVar t var =
  let
    runtimeType = Var "_this" `Arrow` typeVarRefName t
  in
    encoreTracePolymorphicVariable var runtimeType

traceCapability :: CCode Lval -> CCode Stat
traceCapability var =
  let
    cap = Cast capability var
    traceFunPath = cap `Arrow` selfTypeField `Arrow` Nam "trace"
  in
    Statement $ Call traceFunPath [Deref encoreCtxVar, var]

tracefunCall :: (CCode Lval, Ty.Type) -> Ty.Type -> CCode Stat
tracefunCall (a, t) expectedType =
  let
    needToUnwrap = Ty.isTypeVar expectedType && not (Ty.isTypeVar t)
    var = if needToUnwrap then a `Dot` Nam "p" else a
  in
    Statement $ traceVariable t var
