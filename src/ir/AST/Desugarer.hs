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
    desugarTrait t@Trait{tmethods}=
      t{tmethods = map desugarMethod tmethods}
    desugarFunction f@(Function{funbody
                               ,funlocals}) =
      f{funbody = desugarExpr funbody
       ,funlocals = map desugarFunction funlocals}

  -- Automatically give await and supend to active classes
  -- Then the Actor trait is in place, this desugaring step will be changed
  -- so that the Actor trait is included instead
    desugarClass c@(Class{cmeta, cmethods})
      | isActive c = c{cmethods = map desugarMethod (await:suspend:cmethods)}
      where
        await = Method{mmeta
                      ,mimplicit = True
                      ,mheader=awaitHeader
                      ,mlocals=[]
                      ,mbody=Await emeta $ VarAccess emeta (qName "f")}
        awaitHeader = Header{hmodifiers=[]
                            ,kind=NonStreaming
                            ,htypeparams=[typeVar "_t"]
                            ,hname=Name "await"
                            ,htype=unitType
                            ,hparams=[awaitParam]}
        awaitParam = Param{pmeta, pmut=Val, pname=Name "f", ptype=futureType $ typeVar "_t"}
        suspend = Method{mmeta
                        ,mimplicit = True
                        ,mheader = suspendHeader
                        ,mlocals = []
                        ,mbody = Suspend emeta}
        suspendHeader = Header{hmodifiers=[]
                              ,kind=NonStreaming
                              ,htypeparams=[]
                              ,hname=Name "suspend"
                              ,htype=unitType
                              ,hparams=[]}
        pmeta = Meta.meta (Meta.getPos cmeta)
        emeta = Meta.meta (Meta.getPos cmeta)
        mmeta = Meta.meta (Meta.getPos cmeta)

    desugarClass c@(Class{cmethods})
      | isPassive c || isShared c = c{cmethods = map desugarMethod cmethods}

    desugarMethod m@(Method {mbody, mlocals}) =
      m{mbody = desugarExpr mbody
       ,mlocals = map desugarFunction mlocals}

    -- NOTE:
    -- `selfSugar` should always be the first desugaring to run.
    -- otherwise the unsugared version is printed on typechecking errors
    desugarExpr = extend removeDeadMiniLet .
                  extend desugar .
                  extend optionalAccess .
                  extend selfSugar

-- | Desugars the notation `x?.foo()` and `actor?!bar()` into
--
--     borrow x as _tmp in
--       match _tmp with
--         case Just(_x) => Just(_x.foo())
--         case Nothing  => Nothing
--       end
--     end
--
-- Currently the support is only for Option types.
optionalAccess :: Expr -> Expr
optionalAccess Optional {emeta=em, optTag} =
  let (emeta, m, target) = getTemplate optTag
      handlerVar = VarAccess em (qName "_optAccess")
      borrowName = Name "_tmp"
      borrowVar = VarAccess em (qLocal borrowName)
      maybeVal = MaybeValue em $ JustData (m {target = handlerVar})
      match =
        Match emeta borrowVar
        [clauseNothing em,
         MatchClause {mcpattern = MaybeValue{emeta=em, mdt = JustData handlerVar}
                     ,mchandler = maybeVal
                     ,mcguard = BTrue em}]
  in Borrow{emeta = em
           ,target
           ,name = borrowName
           ,body = match
           }
  where
    getTemplate (QuestionBang m@MessageSend{emeta, target}) = (emeta, m, target)
    getTemplate (QuestionDot m@MethodCall{emeta, target}) = (emeta, m, target)
    getTemplate (QuestionDot f@FieldAccess{emeta, target}) = (emeta, f, target)
    getTemplate (QuestionBang e) = error $ "Desugarer.hs: error desugaring expression '" ++ show (ppExpr e) ++ "'"
    getTemplate (QuestionDot e) = error $ "Desugarer.hs: error desugaring expression '" ++ show (ppExpr e) ++ "'"
    clauseNothing emeta = MatchClause {mcpattern = MaybeValue{emeta, mdt = NothingData}
                                      ,mchandler = MaybeValue{emeta, mdt = NothingData}
                                      ,mcguard   = BTrue emeta}
optionalAccess e = e

-- | Let an expression remember its sugared form.
selfSugar :: Expr -> Expr
selfSugar e = setSugared e e

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = Meta.meta (Meta.getPos m)

-- | A @MiniLet@ that has not been taken care of by @desugar@ is
-- dead and can be removed.
removeDeadMiniLet :: Expr -> Expr
removeDeadMiniLet m@MiniLet{} = head $ expandMiniLets [m]
removeDeadMiniLet e = e

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

desugar :: Expr -> Expr

-- Unfold sequenced declarations into let-expressions
desugar seq@Seq{eseq} = seq{eseq = expandMiniLets eseq}

-- Exit
desugar FunctionCall{emeta, qname = QName{qnlocal = Name "exit"}
                    ,args} =
    Exit emeta args

-- Abort
desugar FunctionCall{emeta, qname=QName{qnlocal=Name "abort"} , args=[msg]} =
  Seq{emeta, eseq=[Print emeta Stderr [StringLiteral emeta "{}\n", msg]
                  ,Print emeta Stderr [StringLiteral emeta $ Meta.showPos emeta ++ "\n"]
                  ,Abort{emeta, args=[msg]}]}

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
              ,els = Skip (Meta.meta (Meta.getPos (cloneMeta emeta)))
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
--   do
--     val start = 0
--     val stop = e1
--     var step = start
--     while step < stop do
--       val i = step;
--       step = step + 1;    -- placed here because of continue
--       e2;
--     end
--  end
desugar Repeat{emeta, name, times, body} =
  desugar Seq{emeta ,eseq=[start, stop, step, loop]}
  where
    start = MiniLet{emeta, mutability=Val, decl = ([VarNoType $ Name "__start__"], IntLiteral{emeta, intLit=0})}
    stop = MiniLet{emeta, mutability=Val, decl = ([VarNoType $ Name "__stop__"], times)}
    step = MiniLet{emeta, mutability=Var, decl = ([VarNoType $ Name "__step__"], readVar "start")}
    loop = While{emeta
                ,cond=Binop{emeta
                           ,binop=Identifiers.LT
                           ,loper=readVar "step"
                           ,roper=readVar "stop"}
                ,body=Seq{emeta, eseq=[bindUserLoopVar Seq{emeta, eseq=[incStep, body]}]}}
    readVar name = VarAccess{emeta, qname=qName $ "__" ++ name ++ "__"}
    incStep = Assign{emeta
                    ,lhs=readVar "step"
                    ,rhs=Binop{emeta
                              ,binop=PLUS
                              ,loper=readVar "step"
                              ,roper=IntLiteral{emeta, intLit=1}}}
    bindUserLoopVar body = Let{emeta
                              ,mutability=Val
                              ,decls = [([VarNoType name], readVar "step")]
                              ,body=body}

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
