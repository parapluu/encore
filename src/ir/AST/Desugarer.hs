module AST.Desugarer(desugarProgram) where

import Identifiers
import AST.AST
import qualified AST.Meta as Meta
import AST.PrettyPrinter
import AST.Util
import Types
import Text.Megaparsec
import Data.Maybe
import qualified Data.List as List


createFunction :: Function -> [Expr] -> Function
createFunction func@(Function{funheader}) defaultParams =
  func{funheader=Header{
        hmodifiers= hmodifiers funheader,
        kind= kind funheader,
        htypeparams= take usedParams (htypeparams funheader),
        hname=Name ("_" ++ show (hname funheader) ++ show (length defaultParams)),
        htype= htype funheader,
        hparams= params
    },
    funbody=Return {emeta= Meta.meta $ Meta.getPos $ funmeta func,
                    val= FunctionCall{
      emeta=Meta.meta $ Meta.getPos $ funmeta func,
      typeArguments=htypeparams funheader,
      qname= qName $ show $ hname funheader,
      args= map (\e -> VarAccess{emeta =  Meta.meta $ Meta.getPos $ pmeta e, qname = qName $ show $ pname e}) params ++ defaultParams
    }}
  }
  where
    usedParams = ((length (hparams funheader)) - (length defaultParams))
    params = take ((length (hparams funheader)) - (length defaultParams)) (hparams funheader)


desugarFunctionHeader :: Function -> [Expr] -> [Function]
desugarFunctionHeader f [] = []
desugarFunctionHeader f params@(_:xs) = desugarFunctionHeader f xs ++ [createFunction f params]


desugarDefaultParameters :: Function -> [Function]
desugarDefaultParameters func = desugarFunctionHeader func defaultParams
  where
    defaultParams = map (desugar . fromJust . pdefault) (List.filter (\p@(Param{pdefault}) -> isJust pdefault) (hparams (funheader func)))


-- create a method with default headers filled in
createMethod :: AST.AST.MethodDecl -> [Expr] -> AST.AST.MethodDecl
createMethod meth@(Method{mheader, mmeta}) defaultParams =
  meth{mheader=Header{
        hmodifiers= hmodifiers mheader,
        kind= kind mheader,
        htypeparams= take usedParams (htypeparams mheader),
        hname=Name ("_" ++ show (hname mheader) ++ show (length defaultParams)),
        htype= htype mheader,
        hparams= params
    },
    mbody=Return {emeta= Meta.meta $ Meta.getPos $ mmeta,
                    val= MethodCall{
      emeta=Meta.meta $ Meta.getPos $ mmeta,
      target=VarAccess {emeta=Meta.meta $ Meta.getPos $ mmeta, qname=qName "this"},
      typeArguments=htypeparams mheader,
      name= hname mheader,
      args= map (\e -> VarAccess{emeta =  Meta.meta $ Meta.getPos $ pmeta e, qname = qName $ show $ pname e}) params ++ defaultParams
    }}

  }
  where
    usedParams = ((length (hparams mheader)) - (length defaultParams))
    params = take ((length (hparams mheader)) - (length defaultParams)) (hparams mheader)



desugarDefaultParametersMethod :: MethodDecl -> [Expr] -> [MethodDecl]
desugarDefaultParametersMethod f [] = []
desugarDefaultParametersMethod f params@(_:xs) = desugarDefaultParametersMethod f xs ++ [createMethod f params]


desugarDefaultParametersM :: MethodDecl -> [MethodDecl]
desugarDefaultParametersM m = desugarDefaultParametersMethod m defaultParams
  where
    defaultParams = map (fromJust . pdefault) (List.filter (\p@(Param{pdefault}) -> isJust pdefault) (hparams (mheader m)))


desugarDefaultParametersClass :: Program -> ClassDecl -> ClassDecl
desugarDefaultParametersClass p c@(Class{cmethods}) = c{cmethods = cmethods ++ concat (map desugarDefaultParametersM cmethods) }


desugarProgram :: Program -> Program
desugarProgram p@(Program{traits, classes, functions, adts, adtCons}) =
  p{
    traits = map desugarTrait $ traits ++ adtTraits,
    classes = map (desugarClass . desugarClassParams . (desugarDefaultParametersClass p)) $ classes ++ adtClasses,
    functions = (map desugarFunction $ functions ++ adtFunctions) ++ concat (map desugarDefaultParameters functions)
  }
  where

    (adtTraits, adtClasses, adtFunctions) = partitionAdts [] [] [] (sortAdtCons adts adtCons)

    desugarTrait t@Trait{tmethods} = t{tmethods = map desugarMethod tmethods}

    desugarFunction f@(Function{funbody,funlocals}) =
      f{
        funbody = desugarExpr funbody
       ,funlocals = (map desugarFunction funlocals) ++ concat (map desugarDefaultParameters funlocals)}

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
        awaitParam = Param{pmeta, pmut=Val, pname=Name "f", ptype=futureType $ typeVar "_t", pdefault=Nothing}
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

      -- Desugar default Parameter fields into assignments in the construcor
    desugarClassParams c@(Class{cmethods, cfields}) = c{cmethods = map (desugarClassParamsMethod c) (cmethods ++ createConstructor c)}

    createConstructor c = if hasConstructor c then []
      else [emptyConstructor c]

    desugarClassParamsMethod  c@(Class{cmeta, cmethods, cfields}) m@(Method {mbody, mlocals})
      | isConstructor m = m{mbody = Seq{
          emeta= Meta.meta (Meta.getPos cmeta),
          eseq= (map paramFieldAssignment $ List.filter (isJust . fexpr) cfields) ++ [mbody]
        }}
        where
          paramFieldAssignment field = Assign {emeta=Meta.meta . Meta.getPos . fmeta $ field
                                      ,rhs=fromJust . fexpr $ field
                                      ,lhs=FieldAccess{emeta=Meta.meta. Meta.getPos . fmeta $ field
                                                      ,name=(fname field)
                                                      ,target=VarAccess {emeta=Meta.meta . Meta.getPos . fmeta $ field
                                                                        ,qname=qName "this"}}}
    desugarClassParamsMethod _ m = m


    desugarMethod m@(Method {mbody, mlocals}) =
      m{mbody = desugarExpr mbody
       ,mlocals = map desugarFunction mlocals ++ concat (map desugarDefaultParameters mlocals)}

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

sortAdtCons :: [AdtDecl] -> [AdtConstructor] -> [AdtDecl]
sortAdtCons adts [] = adts
sortAdtCons adts (con:cs) =
  sortAdtCons (insertConsInAdt adts con) cs
  where
    insertConsInAdt [] _ = error "you done did it now!"
    insertConsInAdt (adt@ADT{identity, aconstructor}:as) (cons@ADTcons{parentIdentity}) =
      if (identity == parentIdentity)
      then (adt{aconstructor = cons:aconstructor}:as)
      else adt:(insertConsInAdt as cons)

partitionAdts :: [TraitDecl] -> [ClassDecl] -> [Function] -> [AdtDecl] -> ([TraitDecl], [ClassDecl], [Function])
partitionAdts ts cs ms [] = (ts, cs, ms)
partitionAdts ts cs ms (ADT{ameta, aname, aconstructor, amethods}:rest) =
    partitionAdts (t:ts) (c ++ cs) (m ++ ms) rest
    where
      t = Trait{tmeta
               ,tname = makeRead traitName
               ,treqs = RequiredField{rfield = Field{fmeta, fmut = Val, fname = Name "_ADT_tag", ftype = intType}}:
                        RequiredMethod{rheader = getTagHeader}:
                        (map (\con -> RequiredMethod{rheader = headerFromCons con}) aconstructor)
               ,tmethods = amethods
               }

      c = map (\(a@ADTcons{acmeta, acname, acfields, acomposition, acmethods}, tag) ->
          let
            fields = Field{fmeta, fmut = Val, fname = Name "_ADT_tag", ftype = intType}:
              (map (\p@Param{pmut, pname, ptype} ->
                         Field{fmeta, fmut = pmut, fname = pname, ftype = ptype})) acfields
            traitExtensions = map (\p@Param{pname} -> FieldExtension{extname = pname}) acfields
          in
            Class{cmeta
                 ,cname = makeRead $ setRef acname $
                          adtClassType (reverse (stripName (showWithoutMode acname) [])) (getTypeParameters acname)
                 ,ccomposition = Just acomposition{tcext = traitExtensions}
                 ,cfields = fields
                 ,cmethods = (initMethod a tag):getTag:(extractorMethods a aconstructor)++amethods++acmethods
                 }
              ) $ zip aconstructor [1..length aconstructor]

      m = map (\x@ADTcons{acmeta, acname, acfields, acomposition} ->
              Function{funmeta
                      ,funheader = Header{hmodifiers = []
                                         ,kind = NonStreaming
                                         ,htypeparams = getTypeParameters acname
                                         ,hname = Name (reverse $ (stripName (showWithoutMode acname) []))
                                         ,htype = capabilityFromTraitComposition (Just acomposition)
                                         ,hparams = acfields
                                         }
                      ,funbody = NewWithInit{emeta
                                            ,ty = makeRead $ setRef acname $ classType (reverse $ stripName (showWithoutMode acname) []) (getTypeParameters acname)
                                            ,args = map (\x@Param{pname} -> VarAccess{emeta ,qname = qName $ show pname }) acfields
                                            }
                      ,funlocals = []
                      ,funsource = getRefSourceFile acname}) aconstructor
      getTag = Method{mmeta
                     ,mimplicit = False
                     ,mheader = getTagHeader
                     ,mlocals = []
                     ,mbody = FieldAccess{emeta ,target = VarAccess{emeta, qname = qLocal thisName} ,name = Name "_ADT_tag"}
                     }
      getTagHeader = Header{hmodifiers = []
                           ,kind = NonStreaming
                           ,htypeparams = []
                           ,hname = Name "_getTag"
                           ,htype = intType
                           ,hparams = []
                           }
      emeta = Meta.meta (Meta.sourcePos ameta)
      cmeta = Meta.meta (Meta.sourcePos ameta)
      mmeta = Meta.meta (Meta.sourcePos ameta)
      tmeta = Meta.meta (Meta.sourcePos ameta)
      fmeta = Meta.meta (Meta.sourcePos ameta)
      funmeta = Meta.meta (Meta.sourcePos ameta)
      stripName (c:str) res = if c == '['
                              then res
                              else stripName str (c:res)
      stripName [] res = res
      setRef name ty =
        let
          sourceFile = getRefSourceFile name
          namespace  = case getRefNamespace name of
                       Just(ns) -> ns
                       Nothing -> emptyNamespace
        in
          setRefSourceFile sourceFile $
                           setRefNamespace namespace ty
      typeParams = getTypeParameters aname
      traitName = setRef aname $ adtTraitType (reverse (stripName (showWithoutMode aname) [])) typeParams


      initMethod cons@ADTcons{acmeta, acname, acfields} tag =
        method header body mmeta
        where
          header = (headerFromCons cons){htype = unitType
                                        ,hparams = map (\p@Param{pname, ptype} -> p) acfields
                                        ,hname = Name "init"
                                        }
          body = Seq{emeta, eseq = (tagAssignment:map (\p@Param{pname} -> assignment pname) acfields)}
          tagAssignment = Assign{emeta
                                ,lhs = FieldAccess{emeta ,target = VarAccess{emeta ,qname = qLocal thisName} ,name = Name "_ADT_tag"}
                                ,rhs = IntLiteral{emeta, intLit = tag}
                                }
          assignment name = Assign{emeta
                                  ,lhs = FieldAccess{emeta ,target = VarAccess{emeta ,qname = qLocal thisName} ,name = name}
                                  ,rhs = VarAccess{emeta, qname = qName $ show name}
                                  }

      extractorMethods con@ADTcons{acmeta} cons =
        map (\c@ADTcons{acmeta, acname, acfields} ->
          method (header c) (body c) mmeta
        ) cons
        where
          header c@ADTcons{acmeta, acfields} = (headerFromCons c){
            htype = intType
          }
          returnType (x@Param{ptype}:[]) = maybeType ptype
          returnType list = maybeType $ tupleType $ map (\p@Param{ptype} -> ptype) list
          body c@ADTcons{acfields} =
            if (show c == show con)
            then justBody acfields
            else IntLiteral{emeta, intLit = 0}

          justBody fields = FieldAccess{emeta
                                        ,target = VarAccess{emeta, qname = qLocal thisName}
                                        ,name = Name "_ADT_tag"
                                        }
      method header body mmeta=
        Method{mmeta
              ,mimplicit = True
              ,mheader = header
              ,mlocals = []
              ,mbody = body
              }
      headerFromCons ADTcons{acname, acfields} =
        Header{
          hmodifiers = [],
          kind = NonStreaming,
          htypeparams = [],
          hname = Name (reverse $ stripName (showWithoutMode acname) []),
          htype = intType,
          hparams = []
        }
        where
          returnType (x@Param{ptype}:[]) = maybeType ptype
          returnType list = maybeType $ tupleType $ map (\p@Param{ptype} -> ptype) list


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
