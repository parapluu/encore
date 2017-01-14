module CodeGen.DTrace where

import CCode.Main
import CCode.PrettyCCode
import Identifiers as ID
import Data.List
import Text.Printf

dtrace :: String -> [String] -> CCode Stat
dtrace probe [] = Embed $ printf "ENC_DTRACE0(%s)" probe
dtrace probe args =
  let n = length args
  in
  Embed $ printf "ENC_DTRACE%d(%s, %s)"
          n probe (intercalate ", " args)

dtraceFieldAccess :: CCode Lval -> ID.Name -> CCode Stat
dtraceFieldAccess target name =
  dtrace "FIELD_ACCESS" [pp target, show $ show name]

dtraceFieldWrite :: CCode Lval -> ID.Name -> CCode Stat
dtraceFieldWrite target name =
  dtrace "FIELD_WRITE" [pp target, show $ show name]

dtraceMethodCall :: CCode Lval -> ID.Name -> [CCode Lval] -> CCode Stat
dtraceMethodCall target name args =
  dtrace "METHOD_CALL" $ [pp target, show $ show name]

dtraceMethodEntry :: CCode Lval -> ID.Name -> [CCode Lval] -> CCode Stat
dtraceMethodEntry this name args =
  dtrace "METHOD_ENTRY" $ [pp this, show $ show name]

dtraceMethodExit :: CCode Lval -> ID.Name -> CCode Stat
dtraceMethodExit this name =
  dtrace "METHOD_EXIT" [pp this, show $ show name]

-- dtraceFunctionCall :: ID.QualifiedName -> [CCode Lval] -> CCode Stat
-- dtraceFunctionCall name args =
--  dtrace "FUNTION_CALL" $ show (show name) : map pp args

-- dtraceFunctionCall :: CCode Stat
-- dtraceFunctionCall = dtrace "FUNTION_CALL" []

dtraceFunctionEntry :: ID.Name -> [CCode Lval] -> CCode Stat
dtraceFunctionEntry name args =
  dtrace "FUNCTION_ENTRY" $ show (show name) : map pp args

-- dtraceFunctionExit :: ID.Name -> CCode Stat
-- dtraceFunctionExit name =
--   dtrace "FUNTION_EXIT"  [show $ show name]
-- 
-- dtraceFunctionExit :: CCode Stat
-- dtraceFunctionExit = dtrace "FUNTION_EXIT"  []

dtraceClosureCall :: ID.QualifiedName -> [CCode Lval] -> CCode Stat
dtraceClosureCall name args =
  dtrace "CLOSURE_CALL" $ show (show name) : map pp args

dtraceClosureEntry :: [CCode Lval] -> CCode Stat
dtraceClosureEntry args =
  dtrace "CLOSURE_ENTRY" $ map pp args

dtraceClosureExit :: CCode Stat
dtraceClosureExit = dtrace "CLOSURE_EXIT" []
