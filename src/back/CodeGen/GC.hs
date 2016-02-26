module CodeGen.GC (gcSend, gcRecv) where

import CodeGen.CCodeNames
import CCode.Main
import qualified AST.AST as A
import CodeGen.Trace (traceVariable, tracefunCall)

gcRecv params traceFun = [Embed $ "",
                          Embed $ "// --- GC on receive ----------------------------------------",
                          Statement $ Call ponyGcRecvName [encoreCtxVar]] ++
                         (map traceEachParam params) ++
                         [traceFun,
                          Statement $ Call ponyRecvDoneName [encoreCtxVar],
                          Embed $ "// --- GC on receive ----------------------------------------",
                          Embed $ ""]
  where
    traceEachParam A.Param{A.pname, A.ptype} =
      Statement $ traceVariable ptype $ AsLval $ argName pname

gcSend as expectedTypes traceFuns =
    [Embed $ "",
     Embed $ "// --- GC on sending ----------------------------------------",
     Statement $ Call ponyGcSendName [encoreCtxVar]] ++
    traceFuns ++
    (zipWith tracefunCall as expectedTypes) ++
    [Statement $ Call ponySendDoneName [encoreCtxVar],
     Embed $ "// --- GC on sending ----------------------------------------",
     Embed $ ""]
