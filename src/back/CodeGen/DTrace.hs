module CodeGen.DTrace where

import CCode.Main
import CCode.PrettyCCode
import Identifiers as ID
import Data.List
import Text.Printf

dtrace :: String -> [String] -> CCode Stat
dtrace probe args =
  Embed $ printf "/* dtrace: Trigger %s(%s) */"
          probe (intercalate ", " args)

dtraceFieldAccess :: CCode Lval -> ID.Name -> CCode Stat
dtraceFieldAccess target name =
  dtrace "field_access_probe" [pp target, show $ show name]

dtraceFieldWrite :: CCode Lval -> ID.Name -> CCode Stat
dtraceFieldWrite target name =
  dtrace "field_write_probe" [pp target, show $ show name]

dtraceMethodCall :: CCode Lval -> ID.Name -> [CCode Lval] -> CCode Stat
dtraceMethodCall target name args =
  dtrace "method_call_probe" $ [pp target, show $ show name] ++ map pp args

dtraceMethodEntry :: CCode Lval -> ID.Name -> [CCode Lval] -> CCode Stat
dtraceMethodEntry this name args =
  dtrace "method_entry_probe" $ [pp this, show $ show name] ++ map pp args

dtraceMethodExit :: CCode Lval -> ID.Name -> CCode Stat
dtraceMethodExit this name =
  dtrace "method_exit_probe" [pp this, show $ show name]

dtraceFunctionCall :: ID.QualifiedName -> [CCode Lval] -> CCode Stat
dtraceFunctionCall name args =
  dtrace "function_call_probe" $ show (show name) : map pp args

dtraceFunctionEntry :: ID.Name -> [CCode Lval] -> CCode Stat
dtraceFunctionEntry name args =
  dtrace "function_entry_probe" $ show (show name) : map pp args

dtraceFunctionExit :: ID.Name -> CCode Stat
dtraceFunctionExit name =
  dtrace "function_exit_probe" [show $ show name]

dtraceClosureCall :: ID.QualifiedName -> [CCode Lval] -> CCode Stat
dtraceClosureCall name args =
  dtrace "closure_call_probe" $ show (show name) : map pp args

dtraceClosureEntry :: [CCode Lval] -> CCode Stat
dtraceClosureEntry args =
  dtrace "closure_entry_probe" $ map pp args

dtraceClosureExit :: CCode Stat
dtraceClosureExit = dtrace "closure_exit_probe" []
