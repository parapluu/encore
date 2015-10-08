{-# LANGUAGE GADTs #-}

module CodeGen.Trace (
  traceVariable
  , traceFuture
  , ponyGcSend
  , ponySendDone
) where

import CCode.Main
import CodeGen.CCodeNames
import qualified Types as Ty
import CCode.PrettyCCode ()

ponyGcSend :: CCode Stat
ponyGcSend = Statement $ Call ponyGcSendName ([] :: [CCode Expr])

ponySendDone :: CCode Stat
ponySendDone = Statement $ Call ponySendDoneName ([] :: [CCode Expr])

traceFuture :: CCode Lval -> CCode Expr
traceFuture var = ponyTraceobject var futureTraceFn

-- TODO: Tracing of trait typed variables?
traceVariable :: Ty.Type -> CCode Lval -> CCode Expr
traceVariable t var
  | Ty.isActiveClassType  t = ponyTraceactor var
  | Ty.isSharedClassType  t = ponyTraceactor var
  | Ty.isPassiveClassType t = ponyTraceobject var $ classTraceFnName t
  | Ty.isFutureType     t = ponyTraceobject var futureTraceFn
  | Ty.isArrowType      t = ponyTraceobject var closureTraceFn
  | Ty.isArrayType      t = ponyTraceobject var arrayTraceFn
  | Ty.isStreamType     t = ponyTraceobject var streamTraceFn
  | Ty.isTypeVar        t = traceTypeVar t var
  | otherwise =
    Embed $ "/* Not tracing field '" ++ show var ++ "' */"

ponyTraceactor :: CCode Lval -> CCode Expr
ponyTraceactor var =
  Call (Nam "pony_traceactor")  [Cast (Ptr ponyActorT) var]

ponyTraceobject :: (UsableAs e Expr) => CCode Lval -> CCode e -> CCode Expr
ponyTraceobject var f =
  let
    toExpr :: (UsableAs e Expr) => CCode e -> CCode Expr
    toExpr e@(Nam _) = AsExpr . AsLval $ e
    toExpr e@(AsExpr _) = e
    toExpr _ = undefined
  in
    Call (Nam "pony_traceobject")  [AsExpr var, toExpr f]

encoreTracePolymorphicVariable :: CCode Lval -> CCode Lval  -> CCode Expr
encoreTracePolymorphicVariable var t =
  Call (Nam "encore_trace_polymorphic_variable")  [t, var]

traceTypeVar :: Ty.Type -> CCode Lval -> CCode Expr
traceTypeVar t var =
  let
    runtimeType = Var "this" `Arrow` typeVarRefName t
  in
    encoreTracePolymorphicVariable var runtimeType
