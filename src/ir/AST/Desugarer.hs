{-# LANGUAGE NamedFieldPuns #-}

module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types

import qualified Data.List as List

desugarProgram :: Program -> Program
desugarProgram p@(Program{classes, functions, imports}) = p{classes = map desugarClass classes,
                                                            functions = map desugarFunction functions,
                                                            imports = map desugarImports imports}
    where
      desugarImports f@(PulledImport{iprogram}) = f{iprogram = desugarProgram iprogram}
      desugarFunction f@(Function{funbody}) = f{funbody = desugarExpr funbody}
      desugarClass c@(Class{methods}) = c{methods = map desugarMethod methods}
      desugarMethod m
          | (mname m) == Name "init" = m{mname = Name "_init", mbody = desugarExpr (mbody m)}
          | otherwise = m{mbody = desugarExpr (mbody m)}
      desugarExpr = (extend desugar) . (extend (\e -> setSugared e e))

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = (Meta.meta (Meta.sourcePos m))

desugar :: Expr -> Expr

desugar FunctionCall{emeta, name = Name "exit", args} = Exit emeta args

desugar FunctionCall{emeta, name = Name "print", args = (string@(StringLiteral {stringLit = s})):args} =
    Print emeta s args

desugar FunctionCall{emeta, name = Name "print", args = [e]} =
    Print emeta "{}\n" [e]

desugar fCall@FunctionCall{emeta, name = Name "assertTrue", args = [cond]} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ (show $ ppExpr fCall) ++ "\n") [],
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar fCall@FunctionCall{emeta, name = Name "assertFalse", args = [cond]} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ (show $ ppExpr fCall) ++ "\n") [],
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar FunctionCall{emeta, name = Name "assertTrue", args = cond : lit@(StringLiteral {stringLit = s}) : rest} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ s ++ "\n") rest,
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar FunctionCall{emeta, name = Name "assertFalse", args = cond : lit@(StringLiteral {stringLit = s}) : rest} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta) [Print (cloneMeta emeta) ("Assertion failed: " ++ s ++ "\n") rest,
                       Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar IfThen{emeta, cond, thn} =
    IfThenElse emeta cond thn (Skip (Meta.meta (Meta.sourcePos (cloneMeta emeta))))

desugar Unless{emeta, cond, thn} =
    IfThenElse emeta (Unary (cloneMeta emeta) Identifiers.NOT cond) thn (Skip (cloneMeta emeta))

-- Desugars
--   repeat id <- e1 e2
-- into
--   let
--     id = 0
--     __ub__ = e1
--   in
--     while id < __ub__
--       {
--         e2;
--         id = id + 1;
--       }
desugar Repeat{emeta, name, times, body} =
    Let emeta
        [(name, (IntLiteral emeta 0)), (Name "__gub__", times)]
       (While emeta
             (Binop emeta
                   Identifiers.LT
                   (VarAccess emeta name)
                   (VarAccess emeta (Name "__gub__")))
             (Seq emeta
                  [body, (Assign emeta
                               (VarAccess emeta name)
                               (Binop emeta
                                     PLUS
                                     (VarAccess emeta name)
                                     (IntLiteral emeta 1)))]))


--   finish { f1 = async e1; f2 = async e2 }
-- into
--   f1 = async e1
--   f2 = async e2
--   get f1
--   get f2

desugar FinishAsync{emeta, body} =
    Seq emeta $ [desugar_body body]
  where
    isAsyncTask (Async _ _) = True
    isAsyncTask _ = False

    desugar_body seq@Seq{eseq, emeta} =
      let sizeSeq = (length eseq)
          bindings = [((Name $ "__seq__" ++ show i , eseq !! i), isAsyncTask $ eseq!!i) | i <- [0..sizeSeq-1]]
          stmts = map fst $ List.filter (\x -> (snd x) == True) bindings
      in
          Let emeta
              (map fst bindings)
              (Seq emeta $ [(Get emeta $ VarAccess emeta $ fst b) | b <- stmts])
    desugar_body a = a

-- foreach item in arr {
--   stmt using item
-- }

-- translates to

-- let __it__ = 0
--     __arr_size = |arr|
-- in
--   while (__it__ > __arr_size__) {
--     stmt where item is replaced by arr[__it__]
--     __it__ = __it__ +1
--   }
desugar Foreach{emeta, item, arr, body} =
  let it = Name "__it__"
      arr_size = Name "__arr_size__" in
   Let emeta [(arr_size, ArraySize emeta arr)]
    (IfThenElse emeta (Binop emeta Identifiers.EQ (VarAccess emeta arr_size) (IntLiteral emeta 0))
     (Skip (cloneMeta emeta))
     (Let emeta
        [(it, (IntLiteral emeta 0)),
         (item, ArrayAccess emeta arr (IntLiteral emeta 0))]
       (While emeta
             (Binop emeta
                   Identifiers.LT
                   (VarAccess emeta it)
                   (VarAccess emeta arr_size))
             (Seq emeta
                  [(Assign emeta (VarAccess emeta item) (ArrayAccess emeta arr (VarAccess emeta it))),
                   Async emeta body,
                   (Assign emeta
                      (VarAccess emeta it)
                      (Binop emeta
                       PLUS
                       (VarAccess emeta it)
                       (IntLiteral emeta 1)))
                  ]))))


desugar NewWithInit{emeta, ty, args}
    | isArrayType ty &&
      length args == 1 = ArrayNew emeta (getResultType ty) (head args)
    | otherwise =
        Let emeta
            [(Name "to_init", (New (cloneMeta emeta) ty))]
            (Seq (cloneMeta emeta)
                 [(MethodCall ((cloneMeta emeta))
                              (VarAccess (cloneMeta emeta) (Name "to_init"))
                              (Name "_init") (map desugar args)),
                  (VarAccess (cloneMeta emeta) (Name "to_init"))])

desugar e = e
