module CodeGen.DTrace where

import CCode.Main
import CCode.PrettyCCode
import Identifiers as ID
import Data.List
import Text.Printf

dtrace :: String -> [String] -> CCode Stat
dtrace probe [] =
    Embed $ printf "ENC_DTRACE0(%s)" probe
dtrace probe args =
  let n = length args
  in
  Embed $ printf "ENC_DTRACE%d(%s, %s)"
          n probe (intercalate ", " args)

-- TODO: Args not used; can't use variable size of arguments

dtraceFieldAccess :: CCode Lval -> ID.Name -> CCode Stat
dtraceFieldAccess target name =
  dtrace "FIELD_ACCESS" ["(uintptr_t)*_ctx", "(uintptr_t)" ++ pp target, show $ show name]

dtraceFieldWrite :: CCode Lval -> ID.Name -> CCode Stat
dtraceFieldWrite target name =
  dtrace "FIELD_WRITE" ["(uintptr_t)*_ctx", "(uintptr_t)" ++ pp target, show $ show name]

dtraceMethodCall :: CCode Lval -> ID.Name -> [CCode Lval] -> CCode Stat
dtraceMethodCall target name args =
  dtrace "METHOD_CALL" $ ["(uintptr_t)*_ctx", "(uintptr_t)" ++ pp target, show $ show name] -- ++ map pp args

dtraceMethodEntry :: CCode Lval -> ID.Name -> [CCode Lval] -> CCode Stat
dtraceMethodEntry this name args =
  dtrace "METHOD_ENTRY" $ ["(uintptr_t)*_ctx", "(uintptr_t)" ++ pp this, show $ show name] -- ++ map pp args

dtraceMethodExit :: CCode Lval -> ID.Name -> CCode Stat
dtraceMethodExit this name =
  dtrace "METHOD_EXIT" ["(uintptr_t)*_ctx", "(uintptr_t)" ++ pp this, show $ show name]

dtraceFunctionCall :: ID.QualifiedName -> [CCode Lval] -> CCode Stat
dtraceFunctionCall name args =
  dtrace "FUNCTION_CALL" $ ["(uintptr_t)*_ctx", show $ show name ] -- : map pp args

dtraceFunctionEntry :: ID.Name -> [CCode Lval] -> CCode Stat
dtraceFunctionEntry name args =
  dtrace "FUNCTION_ENTRY" ["(uintptr_t)*_ctx", show $ show name] -- : map pp args

dtraceFunctionExit :: ID.Name -> CCode Stat
dtraceFunctionExit name =
  dtrace "FUNCTION_EXIT" ["(uintptr_t)*_ctx", show $ show name]

dtraceClosureCall :: ID.QualifiedName -> [CCode Lval] -> CCode Stat
dtraceClosureCall name args =
  dtrace "CLOSURE_CALL" ["(uintptr_t)*_ctx", show $ show name] -- : map pp args

dtraceClosureEntry :: [CCode Lval] -> CCode Stat
dtraceClosureEntry args =
  dtrace "CLOSURE_ENTRY" ["(uintptr_t)*_ctx"] -- $ map pp args

dtraceClosureExit :: CCode Stat
dtraceClosureExit = dtrace "CLOSURE_EXIT" ["(uintptr_t)*_ctx"]
