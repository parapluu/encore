{-# LANGUAGE GADTs #-}

module CodeGen.Trace (trace_variable) where

import CCode.Main
import CodeGen.CCodeNames
import qualified Types as Ty
import CCode.PrettyCCode ()

-- TODO: Tracing of trait typed variables?
trace_variable :: Ty.Type -> CCode Lval -> CCode Expr
trace_variable t var
  | Ty.isActiveClassType  t = pony_traceactor var
  | Ty.isPassiveClassType t = pony_traceobject var $ class_trace_fn_name t
  | Ty.isFutureType     t = pony_traceobject var future_trace_fn
  | Ty.isArrowType      t = pony_traceobject var closure_trace_fn
  | Ty.isArrayType      t = pony_traceobject var array_trace_fn
  | Ty.isStreamType     t = pony_traceobject var stream_trace_fn
  | Ty.isTypeVar        t = trace_type_var t var
  | otherwise =
    Embed $ "/* Not tracing field '" ++ show var ++ "' */"

pony_traceactor :: CCode Lval -> CCode Expr
pony_traceactor var =
  Call (Nam "pony_traceactor")  [Cast (Ptr pony_actor_t) var]

pony_traceobject :: (UsableAs e Expr) => CCode Lval -> CCode e -> CCode Expr
pony_traceobject var f =
  let
    to_expr :: (UsableAs e Expr) => CCode e -> CCode Expr
    to_expr e@(Nam _) = AsExpr . AsLval $ e
    to_expr e@(AsExpr _) = e
    to_expr _ = undefined
  in
    Call (Nam "pony_traceobject")  [AsExpr var, to_expr f]

encore_trace_polymorphic_variable :: CCode Lval -> CCode Lval  -> CCode Expr
encore_trace_polymorphic_variable var t =
  Call (Nam "encore_trace_polymorphic_variable")  [t, var]

trace_type_var :: Ty.Type -> CCode Lval -> CCode Expr
trace_type_var t var =
  let
    runtime_type = Var "this" `Arrow` type_var_ref_name t
  in
    encore_trace_polymorphic_variable var runtime_type
