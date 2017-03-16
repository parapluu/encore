module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types
import Text.Megaparsec

import qualified Data.List as List

import Debug.Trace
import AST.PrettyPrinter

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
        suspend = Method{mmeta, mheader=suspendHeader, mlocals=[], mbody=Suspend emeta}
        suspendHeader = Header{hmodifiers=[]
                              ,kind=NonStreaming
                              ,htypeparams=[]
                              ,hname=Name "suspend"
                              ,htype=unitType
                              ,hparams=[]}
        pmeta = Meta.meta (Meta.sourcePos cmeta)
        emeta = Meta.meta (Meta.sourcePos cmeta)
        mmeta = Meta.meta (Meta.sourcePos cmeta)

    desugarClass c@(Class{cmethods})
      | isPassive c || isShared c = c{cmethods = map desugarMethod cmethods}

    desugarMethod m@(Method {mbody, mlocals}) =
      m{mbody = desugarExpr mbody
       ,mlocals = map desugarFunction mlocals}

    -- NOTE:
    -- `selfSugar` should always be the first thing.
    -- otherwise the unsugared version is printed on typechecking errors
    desugarExpr e = (extend removeDeadMiniLet . extend desugar . optionalAccess . extend selfSugar) e

optionalAccess :: Expr -> Expr
optionalAccess e@(Seq {eseq}) = e { eseq = map optionalAccess eseq}

optionalAccess e@(MiniLet {decl=(n, ex)}) =
  e {decl = (n, optionalAccess ex)}

optionalAccess e@(Let {decls, body}) =
  let (names, exprs) =  unzip decls
      bindings = zipWith (\nam expr -> (nam, optionalAccess expr)) names exprs
  in e { decls = bindings, body = optionalAccess body}

optionalAccess e@(Assign {lhs, rhs}) = e {lhs = optionalAccess lhs,
                                          rhs = optionalAccess rhs}

optionalAccess m@MessageSend {emeta, target, opt=True} =
  let result = Match emeta target
        [clauseNothing emeta,
         MatchClause {mcpattern = MaybeValue{emeta ,mdt = JustData handlerName}
                     ,mchandler = MaybeValue{emeta
                                            ,mdt = JustData (m {target = handlerName
                                                               ,opt=False})}
                     ,mcguard = BTrue emeta}]
  in result
  where
    varAccessFactory arg = VarAccess emeta QName{qnspace  = Nothing
                                                ,qnsource = Nothing
                                                ,qnlocal  = arg}
    handlerName = varAccessFactory $ Name . namePrefix . show $ ppExpr target
    namePrefix = ("_" ++)

optionalAccess f@FieldAccess{emeta, target, opt=True, name} =
  let matchFactory arg =
        let fAccess = FieldAccess emeta (handlerName arg) False name
            handlerBody = MaybeValue{emeta ,mdt = JustData fAccess}
        in desugarOptMatch (matcherName arg) (handlerName arg) handlerBody
  in desugarOptHelperBase target matchFactory
  where
    namePrefix = ("_" ++)
    letExpr decl body = Let {emeta
                            ,mutability = Val
                            ,decls = [decl]
                            ,body}
    varAccessFactory arg = VarAccess emeta QName{qnspace  = Nothing
                                                ,qnsource = Nothing
                                                ,qnlocal  = arg}
    matcherName arg = varAccessFactory arg
    handlerName arg = varAccessFactory $ Name . namePrefix $ show arg
    boundName arg = Name . (namePrefix . namePrefix) $ show arg
    desugarOptMatch matcherNam handlerNam handler =
      Match emeta matcherNam
           [clauseNothing emeta,
            MatchClause {mcpattern = MaybeValue{emeta ,mdt = JustData handlerNam}
                        ,mchandler = handler
                        ,mcguard = BTrue emeta}]
    desugarOptHelperBase :: Expr -> (Name -> Expr) -> Expr
    desugarOptHelperBase f@(VarAccess {qname}) fn = fn (qnlocal qname)
    desugarOptHelperBase MethodCall{emeta, typeArguments, target, opt, name, args} fn =
      let matchFactory :: Name -> Expr
          matchFactory arg =
            let mCall = MethodCall emeta typeArguments (handlerName arg) False name args
                maybeMethodC = MaybeValue emeta (JustData mCall)
            in if opt
               then desugarOptMatch (matcherName arg)
                                    (handlerName arg)
                                    (letExpr (boundName arg, maybeMethodC) (fn $ boundName arg))
               else maybeMethodC
      in desugarOptHelperBase target matchFactory
    desugarOptHelperBase f@(FieldAccess {emeta, target, opt, name}) fn =
      let matchFactory :: Name -> Expr
          matchFactory arg =
            let f = FieldAccess emeta (handlerName arg) False name
                maybeFieldAccess = MaybeValue {emeta, mdt = JustData(f)}
            in if opt
               then desugarOptMatch (matcherName arg)
                                    (handlerName arg)
                                    (letExpr (boundName arg, maybeFieldAccess) (fn $ boundName arg))
               else maybeFieldAccess
      in desugarOptHelperBase target matchFactory
    desugarOptHelperBase e _ = error $ "Desugarer.hs: error while desugaring '" ++
                                       show (ppExpr e) ++ "' using ?."
optionalAccess e = e

-- Helper
clauseNothing emeta = MatchClause {mcpattern = MaybeValue{emeta, mdt = NothingData}
                                  ,mchandler = MaybeValue{emeta, mdt = NothingData}
                                  ,mcguard   = BTrue emeta}


-- | Let an expression remember its sugared form.
selfSugar :: Expr -> Expr
selfSugar e = setSugared e e

cloneMeta :: Meta.Meta Expr -> Meta.Meta Expr
cloneMeta m = Meta.meta (Meta.sourcePos m)

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
--   do
--     val start = -1
--     val stop = e1 - 1
--     var step = start
--     while step < stop do
--       step = step + 1;    -- placed here because of continue
--       val i = step;
--       e2;
--     end
--  end
desugar Repeat{emeta, name, times, body} =
  desugar Seq{emeta ,eseq=[start, stop, step, loop]}
  where
    start = MiniLet{emeta, mutability=Val, decl = (Name "__start__", IntLiteral{emeta, intLit=(-1)})}
    stop = MiniLet{emeta, mutability=Val, decl = (Name "__stop__", Binop{emeta
                                                                        ,binop=MINUS
                                                                        ,loper=times
                                                                        ,roper=IntLiteral{emeta, intLit=1}})}
    step = MiniLet{emeta, mutability=Var, decl = (Name "__step__", readVar "start")}
    loop = While{emeta
                ,cond=Binop{emeta
                           ,binop=Identifiers.LT
                           ,loper=readVar "step"
                           ,roper=readVar "stop"}
                ,body=Seq{emeta, eseq=[incStep, bindUserLoopVar body]}}
    readVar name = VarAccess{emeta, qname=qName $ "__" ++ name ++ "__"}
    incStep = Assign{emeta
                    ,lhs=readVar "step"
                    ,rhs=Binop{emeta
                              ,binop=PLUS
                              ,loper=readVar "step"
                              ,roper=IntLiteral{emeta, intLit=1}}}
    bindUserLoopVar body = Let{emeta
                              ,mutability=Val
                              ,decls = [(name, readVar "step")]
                              ,body=body}


desugar Async{emeta, body} =
  FunctionCall {emeta, async = False, typeArguments=[], qname, args}
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
