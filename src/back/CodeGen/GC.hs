module CodeGen.GC (gcSend) where

import CodeGen.CCodeNames
import CCode.Main
import CodeGen.Trace (traceVariable, tracefunCall)

-- gcReceive params = gcRecv params
--                    (Statement $ Call ponyTraceObject
--                     [encoreCtxVar,
--                      Var "_fut",
--                      futureTypeRecName `Dot` Nam "trace"])

gcSend as expectedTypes trace =
    [Embed $ "",
     Embed $ "// --- GC on sending ----------------------------------------",
     Statement $ Call ponyGcSendName [encoreCtxVar],
     trace] ++
     (zipWith tracefunCall as expectedTypes) ++
    [Statement $ Call ponySendDoneName [encoreCtxVar],
     Embed $ "// --- GC on sending ----------------------------------------",
     Embed $ ""]
