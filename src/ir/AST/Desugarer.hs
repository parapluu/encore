module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types

import qualified Data.List as List

desugarProgram :: Program -> Program
desugarProgram p@(Program{traits, classes, functions}) =
  p{
    traits = map desugarTrait traits,
    classes = map desugarClass classes,
    functions = map desugarFunction functions
  }
  where
    desugarFunctionHeadMatch headers bodies =
      let oldHeader = head headers
          pos = Meta.getPos $ getMeta $ head $ hpatterns oldHeader
          emeta = Meta.meta pos
          paramTypes = hparamtypes oldHeader
          maxLen = maximum $ map (length . hpatterns) headers
          paramNames = case List.find (isCatchAll maxLen) headers of
                         Just header -> map (qnlocal . qname) (hpatterns header)
                         Nothing -> map (Name . ("_match" ++) . show) [0..]

          accesses = take (length paramTypes) $
                          map (VarAccess emeta . qLocal) paramNames
          arg = Tuple{emeta, args = accesses}

          patterns = map getPattern headers
          handlers = bodies
          guards = map hguard headers
          clauses = zipWith3 makeClause patterns handlers guards

          newParams = zipWith (makeParam pos) paramNames paramTypes

          header = makeHeader oldHeader newParams

          body = Match{emeta, arg, clauses}
      in
        (header, desugarExpr body)
      where
        isCatchAll len header =
            let patterns = hpatterns header
            in length patterns == len && all isVarAccess patterns
        isVarAccess VarAccess{} = True
        isVarAccess _ = False

        getPattern header =
            let patterns = hpatterns header
                types = hparamtypes header
                typePattern body ty =
                    let emeta = Meta.meta . Meta.getPos . getMeta $ body
                    in TypedExpr{emeta, body, ty}
            in
              zipWith typePattern patterns types

        makeHeader (MatchingHeader{htypeparams, kind, hname, htype}) hparams =
          Header{kind, htypeparams, hname, htype, hparams}

        makeParam pos pname ptype =
          Param{pmeta = Meta.meta pos, pname, ptype}

        makeClause pattern mchandler mcguard =
          let pos = if null pattern
                    then Meta.getPos . getMeta $ mcguard
                    else Meta.getPos . getMeta . head $ pattern
              emeta = Meta.meta pos
              actualPattern = Tuple{emeta, args = pattern}
          in MatchClause{mcpattern = actualPattern,
                         mchandler,
                         mcguard}

    desugarTrait t@Trait{tmethods}=
      t{tmethods = map desugarMethod tmethods}
    desugarFunction f@(Function{funbody}) = f{funbody = desugarExpr funbody}
    desugarFunction f@(MatchingFunction{funmeta
                                       ,matchfunheaders
                                       ,matchfunbodies
                                       ,funsource
                                       }) =
      let (funheader, funbody) = desugarFunctionHeadMatch
                                   matchfunheaders matchfunbodies
      in Function{funmeta, funheader, funbody, funsource}

    desugarClass c@(Class{cmethods}) = c{cmethods = map desugarMethod cmethods}
    desugarMethod m@(Method {mmeta, mheader, mbody})
      | methodName m == Name "init" =
          let header  = mheader
              header' = header{hname = Name "_init"}
          in
        m{mheader = header', mbody = desugarExpr mbody}
      | otherwise = m{mbody = desugarExpr mbody}
    desugarMethod m@(MatchingMethod {mmeta, mheaders, mbodies}) =
      let (mheader, mbody) = desugarFunctionHeadMatch mheaders mbodies
      in Method{mmeta, mheader, mbody}

    desugarExpr = extend desugar . extend selfSugar


selfSugar :: Expr -> Expr
selfSugar e = setSugared e e

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = Meta.meta (Meta.sourcePos m)

desugar :: Expr -> Expr

desugar seq@Seq{eseq} = seq{eseq = expandMiniLets eseq}
    where
      expandMiniLets [] = []
      expandMiniLets (MiniLet{emeta, decl}:seq) =
          [Let{emeta
              ,decls = [decl]
              ,body = Seq emeta $ case expandMiniLets seq of
                                   [] -> [Skip emeta]
                                   seq' -> seq'
              }]
      expandMiniLets (e:seq) = e:expandMiniLets seq

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "exit"}
                    ,args} =
    Exit emeta args

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "println"}
                    ,args = []} =
    Print emeta [StringLiteral emeta "\n"]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "print"}
                    ,args = [arg]} =
    Print emeta [StringLiteral emeta "{}", arg]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "println"}
                    ,args = [arg]} =
    Print emeta [StringLiteral emeta "{}\n", arg]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "print"}
                    ,args} =
    Print emeta args

desugar FunctionCall{emeta = fmeta, qname = QName{qnlocal = Name "println"}
                    ,args} =
    let first = head args
        rest = tail args in
    case getSugared first of
      Just StringLiteral{emeta = smeta, stringLit} ->
        let stringWithNewline = stringLit ++ "\n"
            newString = selfSugar $ StringLiteral smeta stringWithNewline
            newHead = desugar newString
        in Print fmeta (newHead:rest)
      _ -> Print fmeta args

desugar fCall@FunctionCall{emeta, qname = QName{qnlocal = Name "assertTrue"}
                          ,args = [cond]} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [assertionFailed emeta (show (ppSugared fCall) ++ "\n")],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar fCall@FunctionCall{emeta, qname = QName{qnlocal = Name "assertFalse"}
                          ,args = [cond]} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [assertionFailed emeta (show (ppSugared fCall) ++ "\n")],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "assertTrue"}
                    ,args = cond : rest} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [selfSugar $ assertionFailed emeta ""],
                 Print (cloneMeta emeta) rest,
                 Print (cloneMeta emeta)
                       [selfSugar $ StringLiteral (cloneMeta emeta) "\n"],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "assertFalse"}
                    ,args = cond : rest} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       [selfSugar $ assertionFailed emeta ""],
                 Print (cloneMeta emeta) rest,
                 Print (cloneMeta emeta)
                       [selfSugar $ StringLiteral (cloneMeta emeta) "\n"],
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
                   (VarAccess emeta (qLocal name))
                   (VarAccess emeta (qLocal (Name "__gub__"))))
             (Seq emeta
                  [body, (Assign emeta
                               (VarAccess emeta (qLocal name))
                               (Binop emeta
                                     PLUS
                                     (VarAccess emeta (qLocal name))
                                     (IntLiteral emeta 1)))]))


--   finish { f1 = async e1; f2 = async e2 }
-- into
--   f1 = async e1
--   f2 = async e2
--   get f1
--   get f2

desugar FinishAsync{emeta, body} =
    Seq emeta [desugarBody body]
  where
    isAsyncTask (Async _ _) = True
    isAsyncTask _ = False

    desugarBody seq@Seq{eseq, emeta} =
      let sizeSeq = length eseq
          bindings = [((Name $ "__seq__" ++ show i, eseq !! i), isAsyncTask $ eseq!!i) | i <- [0..sizeSeq-1]]
          stmts = map fst $ List.filter snd bindings
      in
          Let emeta
              (map fst bindings)
              (Seq emeta [Get emeta $ VarAccess emeta $ qLocal (fst b) | b <- stmts])
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
    (IfThenElse emeta (Binop emeta Identifiers.EQ (VarAccess emeta (qLocal arrSize)) (IntLiteral emeta 0))
     (Skip (cloneMeta emeta))
     (Let emeta
        [(it, IntLiteral emeta 0),
         (item, ArrayAccess emeta arr (IntLiteral emeta 0))]
       (While emeta
             (Binop emeta
                   Identifiers.LT
                   (VarAccess emeta (qLocal it))
                   (VarAccess emeta (qLocal arrSize)))
             (Seq emeta
                  [Assign emeta (VarAccess emeta (qLocal item)) (ArrayAccess emeta arr (VarAccess emeta (qLocal it))),
                   Async emeta body,
                   Assign emeta
                      (VarAccess emeta (qLocal it))
                      (Binop emeta
                       PLUS
                       (VarAccess emeta (qLocal it))
                       (IntLiteral emeta 1))
                  ]))))

desugar New{emeta, ty} = NewWithInit{emeta, ty, args = []}

desugar new@NewWithInit{emeta, ty, args}
    | isArrayType ty &&
      length args == 1 = ArrayNew emeta (getResultType ty) (head args)
    | isRefType ty
    , "String" <- getId ty
    , [new'@NewWithInit{ty = ty', args = args'}] <- args
    , isStringObjectType ty'
    , length args' == 1 = new'
    | otherwise = new

desugar s@StringLiteral{emeta, stringLit} =
    NewWithInit{emeta
               ,ty = stringObjectType
               ,args = [Embed emeta (ctype "char*")
                              [(show stringLit ++ ";", Skip emeta)]
                       ]
               }

desugar e = e

assertionFailed emeta assert =
  StringLiteral (cloneMeta emeta) $
                "Assertion failed at " ++
                show (Meta.getPos emeta) ++ ":\n" ++ assert