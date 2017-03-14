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

ponyGcSend :: [(Ty.Type, CCode Lval)] -> CCode Stat -> [CCode Stat]
ponyGcSend argPairs futTrace =
  [Statement $ Call ponyGcSendName [Deref encoreCtxVar]] ++
  (map (Statement . uncurry traceVariable) argPairs)     ++
  [Statement futTrace]                                   ++
  [Statement $ Call ponySendDoneName [Deref encoreCtxVar]]

ponyGcSendFuture :: [(Ty.Type, CCode Lval)] -> [CCode Stat]
ponyGcSendFuture argPairs =
  ponyGcSend argPairs (traceFuture $ futVar)

ponyGcSendStream :: [(Ty.Type, CCode Lval)] -> [CCode Stat]
ponyGcSendStream argPairs =
  ponyGcSend argPairs (traceStream $ Var "_stream")

ponyGcSendOneway :: [(Ty.Type, CCode Lval)] -> [CCode Stat]
ponyGcSendOneway argPairs =
  ponyGcSend argPairs (Comm "No tracing future for oneway msg")
