module CodeGen.GC (gcSend
                  ,gcRecv
                  ,ponyGcSendFuture
                  ,ponyGcSendStream
                  ,ponyGcSendOneway) where

import CodeGen.CCodeNames
import CCode.Main
import qualified AST.AST as A
import qualified Types as Ty
import CodeGen.Trace (traceVariable, tracefunCall, traceFuture, traceStream)

gcRecv params traceFun = [Embed $ "",
                          Embed $ "// --- GC on receive ----------------------------------------",
                          Statement $ Call ponyGcRecvName [Deref encoreCtxVar]] ++
                         (map traceEachParam params) ++
                         [traceFun,
                          Statement $ Call ponyRecvDoneName [Deref encoreCtxVar],
                          Embed $ "// --- GC on receive ----------------------------------------",
                          Embed $ ""]
  where
    traceEachParam A.Param{A.pname, A.ptype} =
      Statement $ traceVariable ptype $ AsLval $ argName pname

gcSend as expectedTypes traceFuns =
    [Embed $ "",
     Embed $ "// --- GC on sending ----------------------------------------",
     Statement $ Call ponyGcSendName [Deref encoreCtxVar]] ++
    traceFuns ++
    (zipWith tracefunCall as expectedTypes) ++
    [Statement $ Call ponySendDoneName [Deref encoreCtxVar],
     Embed $ "// --- GC on sending ----------------------------------------",
     Embed $ ""]

ponyGcSendFuture :: [(Ty.Type, CCode Lval)] -> [CCode Stat]
ponyGcSendFuture argPairs =
  [Statement $ Call ponyGcSendName [Deref encoreCtxVar]] ++
  (map (Statement . uncurry traceVariable) argPairs) ++
  [Statement . traceFuture $ Var "_fut"] ++
  [Statement $ Call ponySendDoneName [Deref encoreCtxVar]]

ponyGcSendStream :: [(Ty.Type, CCode Lval)] -> [CCode Stat]
ponyGcSendStream argPairs =
  [Statement $ Call ponyGcSendName [Deref encoreCtxVar]] ++
  (map (Statement . uncurry traceVariable) argPairs) ++
  [Statement . traceStream $ Var "_stream"] ++
  [Statement $ Call ponySendDoneName [Deref encoreCtxVar]]

ponyGcSendOneway :: [(Ty.Type, CCode Lval)] -> [CCode Stat]
ponyGcSendOneway argPairs =
  [Statement $ Call ponyGcSendName [Deref encoreCtxVar]] ++
  (map (Statement . uncurry traceVariable) argPairs) ++
  [Comm "No tracing future for oneway msg"] ++
  [Statement $ Call ponySendDoneName [Deref encoreCtxVar]]
