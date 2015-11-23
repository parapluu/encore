module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types

import qualified Data.List as List

desugarProgram :: Program -> Program
desugarProgram p@(Program{traits, classes, functions, imports}) =
  p{
    traits = map desugarTrait traits,
    classes = map desugarClass classes,
    functions = map desugarFunction functions,
    imports = map desugarImports imports
  }
  where
    desugarTrait t@Trait{tmethods}=
      t{tmethods = map desugarMethod tmethods}
    desugarImports f@(PulledImport{iprogram}) =
      f{iprogram = desugarProgram iprogram}
    desugarFunction f@(Function{funbody}) = f{funbody = desugarExpr funbody}
    desugarClass c@(Class{cmethods}) = c{cmethods = map desugarMethod cmethods}
    desugarMethod m
      | mname m == Name "init" =
        m{mname = Name "_init", mbody = desugarExpr (mbody m)}
      | otherwise = m{mbody = desugarExpr (mbody m)}
    desugarExpr = extend desugar . extend selfSugar

selfSugar :: Expr -> Expr
selfSugar e = setSugared e e

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = Meta.meta (Meta.sourcePos m)

desugar :: Expr -> Expr

desugar FunctionCall{emeta, name = Name "exit", args} = Exit emeta args

desugar FunctionCall{emeta, name = Name "print", args} =
    Print emeta args

desugar fCall@FunctionCall{emeta, name = Name "assertTrue", args = [cond]} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [StringLiteral (cloneMeta emeta) $
                                      "Assertion failed: " ++
                                      show (ppSugared fCall) ++ "\n"],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar fCall@FunctionCall{emeta, name = Name "assertFalse", args = [cond]} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [StringLiteral (cloneMeta emeta) $
                                      "Assertion failed: " ++
                                      show (ppSugared fCall) ++ "\n"],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar FunctionCall{emeta, name = Name "assertTrue", args = cond : rest} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [selfSugar $ StringLiteral (cloneMeta emeta)
                                                  "{}Assertion failed: ",
                        selfSugar $ StringLiteral (cloneMeta emeta) ""], -- Suppress newline
                 Print (cloneMeta emeta) rest,
                 if length rest > 1 then
                     Print (cloneMeta emeta) [] -- Add newline
                 else
                     Skip (cloneMeta emeta),
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar FunctionCall{emeta, name = Name "assertFalse", args = cond : rest} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [selfSugar $ StringLiteral (cloneMeta emeta)
                                                  "{}Assertion failed: ",
                        selfSugar $ StringLiteral (cloneMeta emeta) ""],
                 Print (cloneMeta emeta) rest,
                 if length rest > 1 then
                     Print (cloneMeta emeta) [] -- Add newline
                 else
                     Skip (cloneMeta emeta),
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
    Seq emeta $ [desugarBody body]
  where
    isAsyncTask (Async _ _) = True
    isAsyncTask _ = False

    desugarBody seq@Seq{eseq, emeta} =
      let sizeSeq = (length eseq)
          bindings = [((Name $ "__seq__" ++ show i , eseq !! i), isAsyncTask $ eseq!!i) | i <- [0..sizeSeq-1]]
          stmts = map fst $ List.filter (\x -> (snd x) == True) bindings
      in
          Let emeta
              (map fst bindings)
              (Seq emeta $ [(Get emeta $ VarAccess emeta $ fst b) | b <- stmts])
    desugarBody a = a

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
      arrSize = Name "__arr_size__" in
   Let emeta [(arrSize, ArraySize emeta arr)]
    (IfThenElse emeta (Binop emeta Identifiers.EQ (VarAccess emeta arrSize) (IntLiteral emeta 0))
     (Skip (cloneMeta emeta))
     (Let emeta
        [(it, IntLiteral emeta 0),
         (item, ArrayAccess emeta arr (IntLiteral emeta 0))]
       (While emeta
             (Binop emeta
                   Identifiers.LT
                   (VarAccess emeta it)
                   (VarAccess emeta arrSize))
             (Seq emeta
                  [Assign emeta (VarAccess emeta item) (ArrayAccess emeta arr (VarAccess emeta it)),
                   Async emeta body,
                   Assign emeta
                      (VarAccess emeta it)
                      (Binop emeta
                       PLUS
                       (VarAccess emeta it)
                       (IntLiteral emeta 1))
                  ]))))

desugar New{emeta, ty} = NewWithInit{emeta, ty, args = []}

desugar new@NewWithInit{emeta, ty, args}
    | isArrayType ty &&
      length args == 1 = ArrayNew emeta (getResultType ty) (head args)
    | otherwise = new

desugar s@StringLiteral{emeta, stringLit} =
    NewWithInit{emeta
               ,ty = stringObjectType
               ,args = [Embed emeta (ctype "char*") $ show stringLit ++ ";"]}

desugar e = e
