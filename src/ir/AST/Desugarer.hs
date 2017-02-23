module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types
import Text.Megaparsec

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

        makeHeader (MatchingHeader{hmodifier,
                                   htypeparams,
                                   kind,
                                   hname,
                                   htype}) hparams =
          Header{hmodifier, kind, htypeparams, hname, htype, hparams}

        makeParam pos pname ptype =
          Param{pmeta = Meta.meta pos, pmut = Val, pname, ptype}

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
    desugarFunction f@(Function{funbody
                               ,funlocals}) =
      f{funbody = desugarExpr funbody
       ,funlocals = map desugarFunction funlocals}
    desugarFunction f@(MatchingFunction{funmeta
                                       ,matchfunheaders
                                       ,matchfunbodies
                                       ,funlocals
                                       ,funsource
                                       }) =
      let (funheader, funbody) = desugarFunctionHeadMatch
                                   matchfunheaders matchfunbodies
      in Function{funmeta
                 ,funheader
                 ,funbody
                 ,funlocals = map desugarFunction funlocals
                 ,funsource
                 }

    desugarClass c@(Class{cmethods}) = c{cmethods = map desugarMethod cmethods}
    desugarMethod m@(Method {mbody, mlocals}) =
      m{mbody = desugarExpr mbody
       ,mlocals = map desugarFunction mlocals}
    desugarMethod m@(MatchingMethod {mmeta, mheaders, mbodies, mlocals}) =
      let (mheader, mbody) = desugarFunctionHeadMatch mheaders mbodies
      in Method{mmeta
               ,mheader
               ,mbody
               ,mlocals = map desugarFunction mlocals}

    desugarExpr = extend removeDeadMiniLet . extend desugar . extend selfSugar

-- | Let an expression remember its sugared form.
selfSugar :: Expr -> Expr
selfSugar e = setSugared e e

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = Meta.meta (Meta.sourcePos m)

-- | A @MiniLet@ that has not been taken care of by @desugar@ is
-- dead and can be removed.
removeDeadMiniLet :: Expr -> Expr
removeDeadMiniLet MiniLet{emeta, decl = (_, e)} = e{emeta}
removeDeadMiniLet e = e

desugar :: Expr -> Expr

-- Unfold sequenced declarations into let-expressions
desugar seq@Seq{eseq} = seq{eseq = expandMiniLets eseq}
    where
      expandMiniLets [] = []
      expandMiniLets (MiniLet{emeta, mutability, decl}:seq) =
          [Let{emeta
              ,mutability
              ,decls = [decl]
              ,body = Seq emeta $ case expandMiniLets seq of
                                   [] -> [Skip emeta]
                                   seq' -> seq'
              }]
      expandMiniLets (e:seq) = e:expandMiniLets seq

-- Exit
desugar FunctionCall{emeta, qname = QName{qnlocal = Name "exit"}
                    ,args} =
    Exit emeta args

-- Print functions
desugar FunctionCall{emeta, qname = QName{qnlocal = Name "println"}
                    ,args = []} =
    Print emeta Stdout [StringLiteral emeta "\n"]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "print"}
                    ,args = [arg]} =
    Print emeta Stdout [StringLiteral emeta "{}", arg]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "println"}
                    ,args = [arg]} =
    Print emeta Stdout [StringLiteral emeta "{}\n", arg]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "print"}
                    ,args} =
    Print emeta Stdout args

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "perror"}
                    ,args = [arg]} =
    Print emeta Stderr [StringLiteral emeta "{}\n", arg]

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "perror"}
                    ,args} =
    Print emeta Stderr args

desugar FunctionCall{emeta = fmeta, qname = QName{qnlocal = Name "println"}
                    ,args} =
    let first = head args
        rest = tail args in
    case getSugared first of
      Just StringLiteral{emeta = smeta, stringLit} ->
        let stringWithNewline = stringLit ++ "\n"
            newString = selfSugar $ StringLiteral smeta stringWithNewline
            newHead = desugar newString
        in Print fmeta Stdout (newHead:rest)
      _ -> Print fmeta Stdout args

-- Assertions
desugar fCall@FunctionCall{emeta, qname = QName{qnlocal = Name "assertTrue"}
                          ,args = [cond]} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       Stderr
                       [assertionFailed emeta (show (ppSugared fCall) ++ "\n")],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar fCall@FunctionCall{emeta, qname = QName{qnlocal = Name "assertFalse"}
                          ,args = [cond]} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       Stderr
                       [assertionFailed emeta (show (ppSugared fCall) ++ "\n")],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "assertTrue"}
                    ,args = cond : rest} =
    IfThenElse emeta cond
           (Skip (cloneMeta emeta))
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       Stderr
                       [selfSugar $ assertionFailed emeta ""],
                 Print (cloneMeta emeta) Stderr rest,
                 Print (cloneMeta emeta)
                       Stderr
                       [selfSugar $ StringLiteral (cloneMeta emeta) "\n"],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])

desugar FunctionCall{emeta, qname = QName{qnlocal = Name "assertFalse"}
                    ,args = cond : rest} =
    IfThenElse emeta cond
           (Seq (cloneMeta emeta)
                [Print (cloneMeta emeta)
                       Stderr
                       [selfSugar $ assertionFailed emeta ""],
                 Print (cloneMeta emeta) Stderr rest,
                 Print (cloneMeta emeta)
                       Stderr
                       [selfSugar $ StringLiteral (cloneMeta emeta) "\n"],
                 Exit (cloneMeta emeta) [IntLiteral (cloneMeta emeta) 1]])
           (Skip (cloneMeta emeta))

-- If-expressions without else
desugar IfThen{emeta, cond, thn} =
    IfThenElse{emeta
              ,cond
              ,thn
              ,els = Skip (Meta.meta (Meta.sourcePos (cloneMeta emeta)))
              }

desugar Unless{emeta, cond = originalCond, thn} =
    IfThenElse{emeta
              ,cond = Unary (cloneMeta emeta) Identifiers.NOT originalCond
              ,thn
              ,els = Skip (cloneMeta emeta)
              }

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
    Let{emeta
       ,mutability = Var
       ,decls = [(name, IntLiteral emeta 0), (Name "__gub__", times)]
       ,body = While emeta
               (Binop emeta
                      Identifiers.LT
                      (VarAccess emeta (qLocal name))
                      (VarAccess emeta (qLocal (Name "__gub__"))))
               (Seq emeta
                    [body, Assign emeta
                                  (VarAccess emeta (qLocal name))
                                  (Binop emeta
                                        PLUS
                                        (VarAccess emeta (qLocal name))
                                        (IntLiteral emeta 1))])
       }

desugar Async{emeta, body} =
  FunctionCall {emeta, typeArguments=[], qname, args}
  where
    qname = QName{qnspace = Nothing, qnsource=Nothing, qnlocal = Name "spawn"}
    args = [lifted_body]
    lifted_body = Closure {emeta, eparams=[], mty=Nothing, body=body}

-- Constructor calls
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

-- Build String objects from literals
desugar s@StringLiteral{emeta, stringLit} =
    NewWithInit{emeta
               ,ty = stringObjectType
               ,args = [Embed emeta (ctype "char*")
                              [(show stringLit ++ ";", Skip emeta)]
                       ]
               }

-- Binary Operators
desugar b@Binop{emeta, binop=PLUS_EQUALS, loper, roper} = 
  Assign{emeta, lhs=loper, rhs=Binop{emeta, binop=PLUS, loper, roper}}

desugar b@Binop{emeta, binop=MINUS_EQUALS, loper, roper} = 
  Assign{emeta, lhs=loper, rhs=Binop{emeta, binop=MINUS, loper, roper}}

desugar b@Binop{emeta, binop=TIMES_EQUALS, loper, roper} = 
  Assign{emeta, lhs=loper, rhs=Binop{emeta, binop=TIMES, loper, roper}}

desugar b@Binop{emeta, binop=DIV_EQUALS, loper, roper} = 
  Assign{emeta, lhs=loper, rhs=Binop{emeta, binop=DIV, loper, roper}}

-- Operations on futures
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "get"}
                      ,args = [val]} = Get{emeta, val}
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "await"}
                      ,args = [val]} = Await{emeta, val}
desugar f@FunctionCall{emeta, qname = QName{qnlocal = Name "getNext"}
                      ,args = [target]} = StreamNext{emeta, target}

-- Operations on ParT
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "extract"}
                      ,args = [val]} = PartyExtract{emeta, val}
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "liftf"}
                      ,args = [val]} = Liftf{emeta, val}
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "liftv"}
                      ,args = [val]} = Liftv{emeta, val}
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "each"}
                      ,args = [val]} = PartyEach{emeta, val}
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "join"}
                      ,args = [val]} = PartyJoin{emeta, val}
desugar f@FunctionCall{emeta
                      ,qname = QName{qnlocal = Name "reduce"}
                      ,args = [seqfun, pinit, par]} =
  PartyReduce{emeta
             ,seqfun
             ,pinit
             ,par
             ,runassoc = False}

-- Maybe values
desugar x@VarAccess{emeta, qname = QName{qnlocal = Name "Nothing"}} =
  MaybeValue{emeta, mdt = NothingData}
desugar f@FunctionCall{emeta, qname = QName{qnlocal = Name "Just"}
                      ,args = [arg]} =
  MaybeValue{emeta, mdt = JustData arg}

desugar e = e

assertionFailed emeta assert =
  StringLiteral (cloneMeta emeta) $
                "Assertion failed at " ++
                Meta.showPos emeta ++ ":\n" ++ assert
