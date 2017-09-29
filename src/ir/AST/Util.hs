{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-|
  Utility functions for "AST.AST".
-}

module AST.Util(
    foldrExp
    , filter
    , extend
    , extendAccum
    , extendAccumProgram
    , extendM
    , extractTypes
    , freeVariables
    , freeTypeVars
    , mapProgramClass
    , exprTypeMap
    , markStatsInBody
    , isStatement
    , isForwardMethod
    , isForwardInExpr
    ) where

import qualified Data.List as List
import Control.Arrow(first)
import Prelude hiding (foldr, filter)
import Control.Monad

import AST.AST
import Types
import Identifiers
import AST.Meta(isStat,makeStat)

-- | @getTypeChildren e@ returns all types that are part of @e@
-- _syntactically_ (i.e. part of the AST node of @e@)
getTypeChildren :: Expr -> [Type]
getTypeChildren NewWithInit{ty} = [ty]
getTypeChildren ArrayNew{ty} = [ty]
getTypeChildren TypedExpr{ty} = [ty]
getTypeChildren Embed{ty} = [ty]
getTypeChildren FunctionCall{typeArguments} = typeArguments
getTypeChildren MethodCall{typeArguments} = typeArguments
getTypeChildren FunctionAsValue{typeArgs} = typeArgs
getTypeChildren e = []


-- | @putTypeChildren tys e@ sets the syntactic types of @e@ to
-- @tys@. The expected invariant is that @putTypeChildren
-- (getTypeChildren e) e = e@
putTypeChildren :: [Type] -> Expr -> Expr
putTypeChildren [ty] e@NewWithInit{} = e{ty}
putTypeChildren [ty] e@ArrayNew{} = e{ty}
putTypeChildren [ty] e@TypedExpr{} = e{ty}
putTypeChildren [ty] e@Embed{} = e{ty}
putTypeChildren typeArguments e@FunctionCall{} = e{typeArguments}
putTypeChildren typeArguments e@MethodCall{} = e{typeArguments}
putTypeChildren typeArgs e@FunctionAsValue{} = e{typeArgs}
putTypeChildren [] e = e
putTypeChildren l e =
  error $ "Util.hs: Trying to give " ++ show (length l) ++
          " types to expression without syntactic types: " ++
          show e

-- | @getChildren e@ returns all children of @e@ that are Exprs themselves
getChildren :: Expr -> [Expr]
getChildren Skip{} = []
getChildren Break{} = []
getChildren Continue{} = []
getChildren TypedExpr {body} = [body]
getChildren Optional {optTag = QuestionDot e} = [e]
getChildren Optional {optTag = QuestionBang e} = [e]
getChildren MethodCall {target, args} = target : args
getChildren MessageSend {target, args} = target : args
getChildren ExtractorPattern {arg} = [arg]
getChildren FunctionCall {args} = args
getChildren FunctionAsValue {} = []
getChildren PartySeq {par, seqfunc} = [par, seqfunc]
getChildren PartyPar {parl, parr} = [parl, parr]
getChildren PartyReduce {seqfun, pinit, par} = [pinit, par, seqfun]
getChildren Closure {body} = [body]
getChildren (MaybeValue _ (JustData e)) = [e]
getChildren (MaybeValue _ NothingData) = []
getChildren Tuple {args} = args
getChildren Async {body} = [body]
getChildren Let {body, decls} = body : map snd decls
getChildren MiniLet {decl = (_, val)} = [val]
getChildren Seq {eseq} = eseq
getChildren IfThenElse {cond, thn, els} = [cond, thn, els]
getChildren IfThen {cond, thn} = [cond, thn]
getChildren Unless {cond, thn} = [cond, thn]
getChildren While {cond, body} = [cond, body]
getChildren DoWhile {cond, body} = [cond, body]
getChildren Repeat {name, times, body} = [times, body]
getChildren For {name, step, src, body} = [step, src, body]
getChildren Match {arg, clauses} = arg:getChildrenClauses clauses
  where
    getChildrenClauses = concatMap getChildrenClause

    getChildrenClause MatchClause {mcpattern, mchandler, mcguard} =
        [mcpattern, mchandler, mcguard]
getChildren Borrow {target, body} = [target, body]
getChildren Get {val} = [val]
getChildren Forward {forwardExpr} = [forwardExpr]
getChildren Yield {val} = [val]
getChildren Eos {} = []
getChildren IsEos {target} = [target]
getChildren StreamNext {target} = [target]
getChildren Await {val} = [val]
getChildren Return {val} = [val]
getChildren Suspend {} = []
getChildren FutureChain {future, chain} = [future, chain]
getChildren FieldAccess {target} = [target]
getChildren ArrayAccess {target, index} = [target, index]
getChildren ArraySize {target} = [target]
getChildren ArrayNew {size} = [size]
getChildren ArrayLiteral {args} = args
getChildren Assign {lhs, rhs} = [lhs, rhs]
getChildren VarAccess {} = []
getChildren TupleAccess {target} = [target]
getChildren Consume {target} = [target]
getChildren Null {} = []
getChildren BTrue {} = []
getChildren BFalse {} = []
getChildren NewWithInit {args} = args
getChildren New {} = []
getChildren Print {args} = args
getChildren Exit {args} = args
getChildren Abort {args} = args
getChildren StringLiteral {} = []
getChildren CharLiteral {} = []
getChildren IntLiteral {} = []
getChildren UIntLiteral {} = []
getChildren RealLiteral {} = []
getChildren RangeLiteral {start, stop, step} = [start, stop, step]
getChildren Embed {embedded} = map snd embedded
getChildren Unary {operand} = [operand]
getChildren Binop {loper, roper} = [loper, roper]

-- | @putChildren children e@ returns @e@ with it's children
-- replaced by the Exprs in @children@. The expected invariant is
-- that @putChildren (getChildren e) e == e@ and @getChildren (putChildren l e) == l@
putChildren :: [Expr] -> Expr -> Expr
putChildren args e@Abort{} = e{args=args}
putChildren [] e@Skip{} = e
putChildren [] e@Break{} = e
putChildren [] e@Continue{} = e
putChildren [] e@(FunctionAsValue {}) = e
putChildren [body] e@(TypedExpr {}) = e{body = body}
putChildren [body@MessageSend {}] e@(Optional {}) = e{optTag = QuestionBang body}
putChildren [body@MethodCall {}] e@(Optional {}) = e{optTag = QuestionDot body}
putChildren [body@FieldAccess {}] e@(Optional {}) = e{optTag = QuestionDot body}
putChildren (target : args) e@(MethodCall {}) = e{target = target, args = args}
putChildren (target : args) e@(MessageSend {}) = e{target = target, args = args}
putChildren [arg] e@(ExtractorPattern {}) = e{arg = arg}
putChildren args e@(FunctionCall {}) = e{args = args}
putChildren [par, seqfunc] e@(PartySeq {}) = e{par=par, seqfunc=seqfunc}
putChildren [l, r] e@(PartyPar {}) = e{parl=l, parr=r}
putChildren [pinit, par, seqfun] e@(PartyReduce {}) = e{par=par, seqfun=seqfun, pinit=pinit}
putChildren [body] e@(Closure {}) = e{body = body}
putChildren [body] e@(Async {}) = e{body = body}
putChildren [body] e@(MaybeValue _ (JustData _)) = e{mdt = JustData body}
putChildren [] e@(MaybeValue _ NothingData) = e
putChildren args e@(Tuple {}) = e{args = args}
putChildren (body : es) e@(Let{decls}) = e{body = body, decls = zipWith (\(name, _) e -> (name, e)) decls es}
putChildren [val] e@(MiniLet{decl = (x, _)}) = e{decl = (x, val)}
putChildren eseq e@(Seq {}) = e{eseq = eseq}
putChildren [cond, thn, els] e@(IfThenElse {}) = e{cond = cond, thn = thn, els = els}
putChildren [cond, thn] e@(IfThen {}) = e{cond = cond, thn = thn}
putChildren [cond, thn] e@(Unless {}) = e{cond = cond, thn = thn}
putChildren [cond, body] e@(While {}) = e{cond = cond, body = body}
putChildren [cond, body] e@(DoWhile {}) = e{cond = cond, body = body}
putChildren [times, body] e@(Repeat {}) = e{times = times, body = body}
putChildren [step, src, body] e@(For {}) = e{step = step, src = src, body = body}
putChildren (arg:clauseList) e@(Match {clauses}) =
    e{arg = arg, clauses=putClausesChildren clauseList clauses}
    where putClausesChildren [] [] = []
          putClausesChildren (pattern:handler:guard:rest) (mc:rClauses) =
              mc{mcpattern=pattern, mchandler=handler, mcguard=guard}:
                putClausesChildren rest rClauses
          putClausesChildren _ _ =
              error "Util.hs: Wrong number of children of of match clause"
putChildren [target, body] e@(Borrow {}) = e{target, body}
putChildren [val] e@(Get {}) = e{val = val}
putChildren [forwardExpr] e@(Forward {}) = e{forwardExpr = forwardExpr}
putChildren [val] e@(Yield {}) = e{val = val}
putChildren [] e@(Eos {}) = e
putChildren [target] e@(IsEos {}) = e{target = target}
putChildren [target] e@(StreamNext {}) = e{target = target}
putChildren [val] e@(Await {}) = e{val = val}
putChildren [val] e@(Return {}) = e{val = val}
putChildren [] e@(Suspend {}) = e
putChildren [future, chain] e@(FutureChain {}) = e{future = future, chain = chain}
putChildren [target] e@(FieldAccess {}) = e{target = target}
putChildren [target, index] e@(ArrayAccess {}) = e{target = target, index = index}
putChildren [target] e@(ArraySize {}) = e{target = target}
putChildren [size] e@(ArrayNew {}) = e{size = size}
putChildren args e@(ArrayLiteral {}) = e{args = args}
putChildren [lhs, rhs] e@(Assign {}) = e{lhs = lhs, rhs = rhs}
putChildren [] e@(VarAccess {}) = e
putChildren [target] e@(TupleAccess {}) = e{target = target}
putChildren [target] e@(Consume {}) = e{target = target}
putChildren [] e@(Null {}) = e
putChildren [] e@(BTrue {}) = e
putChildren [] e@(BFalse {}) = e
putChildren args e@(NewWithInit {}) = e{args = args}
putChildren [] e@(New {}) = e
putChildren args e@(Print {}) = e{args = args}
putChildren args e@(Exit {}) = e{args = args}
putChildren [start, stop, step] e@(RangeLiteral {emeta}) = e{start = start, stop = stop, step = step}
putChildren [] e@(StringLiteral {}) = e
putChildren [] e@(CharLiteral {}) = e
putChildren [] e@(IntLiteral {}) = e
putChildren [] e@(UIntLiteral {}) = e
putChildren [] e@(RealLiteral {}) = e
putChildren exprs e@(Embed {embedded}) = e{embedded = zipWith replace embedded exprs}
  where
    replace (code, _) e = (code, e)
putChildren [operand] e@(Unary {}) = e{operand = operand}
putChildren [loper, roper] e@(Binop {}) = e{loper = loper, roper = roper}
-- This very explicit error handling is there to make
-- -fwarn-incomplete-patterns help us find missing patterns
putChildren _ e@Skip{} = error "'putChildren l Skip' expects l to have 0 elements"
putChildren _ e@Break{} = error "'putChildren l Break' expects l to have 0 elements"
putChildren _ e@Continue{} = error "'putChildren l Continue' expects l to have 0 elements"
putChildren _ e@(TypedExpr {}) = error "'putChildren l TypedExpr' expects l to have 1 element"
putChildren _ e@(MaybeValue {}) = error "'putChildren l MaybeValue' expects l to have 1 element"
putChildren _ e@(Tuple {}) = error "'putChildren l Tuple' expects l to have 1 element"
putChildren _ e@(Optional {}) = error "'putChildren l Option' expects l to have 1 element"
putChildren _ e@(MethodCall {}) = error "'putChildren l MethodCall' expects l to have at least 1 element"
putChildren _ e@(MessageSend {}) = error "'putChildren l MessageSend' expects l to have at least 1 element"
putChildren _ e@(ExtractorPattern {}) = error "'putChildren l ExtractorPattern' expects l to have 1 element"
putChildren _ e@(FunctionAsValue {}) = error "'putChildren l FunctionAsValue' expects l to have 0 elements"
putChildren _ e@(PartySeq {}) = error "'putChildren l PartySeq' expects l to have 2 elements"
putChildren _ e@(PartyPar {}) = error "'putChildren l PartyPar' expects l to have 2 elements"
putChildren _ e@(PartyReduce {}) = error "'putChildren l PartyReduce' expects l to have 3 elements"
putChildren _ e@(Closure {}) = error "'putChildren l Closure' expects l to have 1 element"
putChildren _ e@(Async {}) = error "'putChildren l Async' expects l to have 1 element"
putChildren _ e@(Let{decls}) = error "'putChildren l Let' expects l to have at least 1 element"
putChildren _ e@(MiniLet{decl}) = error "'putChildren l MiniLet' expects l to have 1 element"
putChildren _ e@(IfThenElse {}) = error "'putChildren l IfThenElse' expects l to have 3 elements"
putChildren _ e@(IfThen {}) = error "'putChildren l IfThen' expects l to have 2 elements"
putChildren _ e@(Unless {}) = error "'putChildren l Unless' expects l to have 2 elements"
putChildren _ e@(While {}) = error "'putChildren l While' expects l to have 2 elements"
putChildren _ e@(DoWhile {}) = error "'putChildren l While' expects l to have 2 elements"
putChildren _ e@(Repeat {}) = error "'putChildren l Repeat' expects l to have 2 elements"
putChildren _ e@(For {}) = error "'putChildren l For' expects l to have 3 elements"
putChildren _ e@(Match {}) = error "'putChildren l Match' expects l to have at least 1 element"
putChildren _ e@(Borrow {}) = error "'putChildren l Borrow' expects l to have 2 element"
putChildren _ e@(Get {}) = error "'putChildren l Get' expects l to have 1 element"
putChildren _ e@(Forward {}) = error "'putChildren l Forward' expects l to have 1 element"
putChildren _ e@(Yield {}) = error "'putChildren l Yield' expects l to have 1 element"
putChildren _ e@(Eos {}) = error "'putChildren l Eos' expects l to have 0 elements"
putChildren _ e@(IsEos {}) = error "'putChildren l IsEos' expects l to have 1 element"
putChildren _ e@(StreamNext {}) = error "'putChildren l StreamNext' expects l to have 1 element"
putChildren _ e@(Await {}) = error "'putChildren l Await' expects l to have 1 element"
putChildren _ e@(Return {}) = error "'putChildren l Return' expects l to have 1 element"
putChildren _ e@(Suspend {}) = error "'putChildren l Suspend' expects l to have 0 elements"
putChildren _ e@(FutureChain {}) = error "'putChildren l FutureChain' expects l to have 2 elements"
putChildren _ e@(FieldAccess {}) = error "'putChildren l FieldAccess' expects l to have 1 element"
putChildren _ e@(ArrayAccess {}) = error "'putChildren l ArrayAccess' expects l to have 2 elements"
putChildren _ e@(ArraySize {}) = error "'putChildren l ArraySize' expects l to have 1 element"
putChildren _ e@(ArrayNew {}) = error "'putChildren l ArrayNew' expects l to have 1 element"
putChildren _ e@(Assign {}) = error "'putChildren l Assign' expects l to have 2 elements"
putChildren _ e@(VarAccess {}) = error "'putChildren l VarAccess' expects l to have 0 elements"
putChildren _ e@(TupleAccess {}) = error "'putChildren l TupleAccess' expects l to have 1 element"
putChildren _ e@(Consume {}) = error "'putChildren l Consume' expects l to have 1 element"
putChildren _ e@(Null {}) = error "'putChildren l Null' expects l to have 0 elements"
putChildren _ e@(BTrue {}) = error "'putChildren l BTrue' expects l to have 0 elements"
putChildren _ e@(BFalse {}) = error "'putChildren l BFalse' expects l to have 0 elements"
putChildren _ e@(New {}) = error "'putChildren l New' expects l to have 0 elements"
putChildren _ e@(StringLiteral {}) = error "'putChildren l StringLiteral' expects l to have 0 elements"
putChildren _ e@(CharLiteral {}) = error "'putChildren l CharLiteral' expects l to have 0 elements"
putChildren _ e@(IntLiteral {}) = error "'putChildren l IntLiteral' expects l to have 0 elements"
putChildren _ e@(UIntLiteral {}) = error "'putChildren l UIntLiteral' expects l to have 0 elements"
putChildren _ e@(RealLiteral {}) = error "'putChildren l RealLiteral' expects l to have 0 elements"
putChildren _ e@(RangeLiteral {}) = error "'putChildren l RangeLiteral' expects l to have 3 elements"
putChildren _ e@(Embed {}) = error "'putChildren l Embed' expects l to have 0 elements"
putChildren _ e@(Unary {}) = error "'putChildren l Unary' expects l to have 1 element"
putChildren _ e@(Binop {}) = error "'putChildren l Binop' expects l to have 2 elements"

--------------- The functions below this line depend only on the ones above --------------------

exprTypeMap :: (Type -> Type) -> Expr -> Expr
exprTypeMap f e =
    let tys = getTypeChildren e
        tys' = map f tys
    in putTypeChildren tys' e

foldrExp :: (Expr -> a -> a) -> a -> Expr -> a
foldrExp f l e =
    let childResult = List.foldr (\expr acc -> foldrExp f acc expr) l (getChildren e)
    in f e childResult

-- | Like a map, but where the function has access to the
-- substructure of each node, not only the element. For lists,
-- extend f [1,2,3,4] = [f [1,2,3,4], f [2,3,4], f [3,4], f [4]].
extend :: (Expr -> Expr) -> Expr -> Expr
extend f = snd . extendAccum (\acc e -> (undefined, f e)) undefined

extendAccum :: (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
extendAccum f acc0 e =
    let (acc1, childResults) =
            List.mapAccumL (extendAccum f) acc0 (getChildren e)
    in
      f acc1 (putChildren childResults e)

mapProgramClass :: Program -> (ClassDecl -> ClassDecl) -> Program
mapProgramClass p@Program{classes} f = p{classes = map f classes}

extendAccumProgram ::
    (acc -> Expr -> (acc, Expr)) -> acc -> Program -> (acc, Program)
extendAccumProgram f acc0 p@Program{functions, traits, classes} =
  (acc3, p{functions = funs', traits = traits', classes = classes'})
    where
      (acc1, funs') = List.mapAccumL (extendAccumFunction f) acc0 functions
      extendAccumFunction f acc fun@(Function{funbody, funlocals}) =
        (acc2, fun{funbody = funbody', funlocals = funlocals'})
        where
          (acc1, funbody') = extendAccum f acc funbody
          (acc2, funlocals') = List.mapAccumL (extendAccumFunction f)
                                              acc1 funlocals

      (acc2, traits') = List.mapAccumL (extendAccumTrait f) acc1 traits
      extendAccumTrait f acc trt@(Trait{tmethods}) =
        (acc', trt{tmethods = tmethods'})
        where
          (acc', tmethods') = List.mapAccumL (extendAccumMethod f) acc tmethods

      (acc3, classes') = List.mapAccumL (extendAccumClass f) acc2 classes
      extendAccumClass f acc cls@(Class{cmethods}) =
        (acc', cls{cmethods = cmethods'})
        where
          (acc', cmethods') = List.mapAccumL (extendAccumMethod f) acc cmethods

      extendAccumMethod f acc mtd@(Method{mbody, mlocals}) =
        (acc2, mtd{mbody = mbody', mlocals = mlocals'})
        where
          (acc1, mbody') = extendAccum f acc mbody
          (acc2, mlocals') = List.mapAccumL (extendAccumFunction f)
                                            acc1 mlocals

extendM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
extendM f e =
    do childResults <- mapM (extendM f) (getChildren e)
       f (putChildren childResults e)

-- | @filter cond e@ returns a list of all sub expressions @e'@ of
-- @e@ for which @cond e'@ returns @True@
filter :: (Expr -> Bool) -> Expr -> [Expr]
filter cond = foldrExp (\e acc -> if cond e then e:acc else acc) []

extractTypes :: Program -> [Type]
extractTypes (Program{functions, traits, classes}) =
    List.nub $ concatMap extractFunctionTypes functions ++
               concatMap extractTraitTypes traits ++
               concatMap extractClassTypes classes
    where
      extractHeaderTypes :: FunctionHeader -> [Type]
      extractHeaderTypes header =
          typeComponents (htype header) ++
          concatMap extractParamTypes (hparams header)

      extractFunctionTypes :: Function -> [Type]
      extractFunctionTypes Function{funheader, funbody} =
          extractHeaderTypes funheader ++
          extractExprTypes funbody

      extractTraitTypes :: TraitDecl -> [Type]
      extractTraitTypes Trait {tname, treqs, tmethods} =
          typeComponents tname ++
          concatMap extractReqType treqs ++
          concatMap extractMethodTypes tmethods

      extractReqType RequiredField {rfield} = extractFieldTypes rfield
      extractReqType RequiredMethod {rheader} = extractHeaderTypes rheader

      extractClassTypes :: ClassDecl -> [Type]
      extractClassTypes Class {cname, cfields, cmethods} =
          typeComponents cname ++
          concatMap extractFieldTypes cfields ++
          concatMap extractMethodTypes cmethods

      extractFieldTypes :: FieldDecl -> [Type]
      extractFieldTypes Field {ftype} = typeComponents ftype

      extractMethodTypes :: MethodDecl -> [Type]
      extractMethodTypes Method{mheader, mbody} =
          extractHeaderTypes mheader ++
          extractExprTypes mbody

      extractParamTypes :: ParamDecl -> [Type]
      extractParamTypes Param {ptype} = typeComponents ptype

extractExprTypes :: Expr -> [Type]
extractExprTypes = foldrExp collectTypes []
  where
    collectTypes e@(FunctionCall {typeArguments = typeArgs}) acc =
      typeArgs ++ (typeComponents . getType) e ++ acc
    collectTypes e acc = (typeComponents . getType) e ++ acc

freeTypeVars :: Expr -> [Type]
freeTypeVars = List.nub . List.filter isTypeVar . extractExprTypes

freeVariables :: [QualifiedName] -> Expr -> [(QualifiedName, Type)]
freeVariables bound expr = List.nub $ freeVariables' bound expr
  where
    freeVariables' :: [QualifiedName] -> Expr -> [(QualifiedName, Type)]
    freeVariables' bound Match {arg, clauses} =
        freeVariables' bound arg ++ clausesFreeVars
        where
          clausesFreeVars = concatMap clauseFreeVars clauses
          clauseFreeVars MatchClause{mcpattern, mcguard, mchandler} =
              let boundInPattern = map fst $ freeVariables' [] mcpattern
                  bound' = boundInPattern ++ bound
                  freeInGuard = freeVariables' bound' mcguard
                  freeInHandler = freeVariables' bound' mchandler
              in
                freeInGuard ++ freeInHandler
    freeVariables' bound var@(VarAccess {qname})
        | qname `elem` bound = []
        | otherwise = [(qname, getType var)]
    freeVariables' bound var@(FunctionAsValue {qname})
        | qname `elem` bound = []
        | otherwise = [(qname, getType var)]
    freeVariables' bound fCall@(FunctionCall {typeArguments, qname, args})
        | qname `elem` bound = concatMap (freeVariables' bound) args
        | otherwise = concatMap (freeVariables' bound) args ++
                      [(qname, arrType)]
        where
          arrType = arrowWithTypeParam typeArguments (map getType args) (getType fCall)
    freeVariables' bound Closure {eparams, body} =
        freeVariables' bound' body
        where
          bound' = bound ++ map (qLocal . pname) eparams
    freeVariables' bound Let {decls, body} =
        freeVars ++ freeVariables' bound' body
        where
          (freeVars, bound') = List.foldr fvDecls ([], bound) decls
          fvDecls (vars, expr) (free, bound) =
            let xs = map (qLocal . varName) vars
            in (freeVariables' bound expr ++ free, xs ++ bound)
    freeVariables' bound e@For{name, step, src, body} =
      freeVariables' (qLocal name:bound) =<< getChildren e
    freeVariables' bound e = concatMap (freeVariables' bound) (getChildren e)

markStatsInBody ty e
  | isUnitType ty = mark asStat e
  | otherwise     = mark asExpr e

asStat e = setMeta e $ makeStat $ getMeta e
asExpr e = e

isStatement :: Expr -> Bool
isStatement e = isStat (getMeta e)

markAsStat = mark asStat
markAsExpr = mark asExpr

-- Traverses an AST tree and marks nodes as statements or expressions
mark :: (Expr -> Expr) -> Expr -> Expr
mark asParent s@Seq{eseq} =
  asParent s{eseq=(map markAsStat $ init eseq) ++ [mark asParent $ last eseq]}
mark asParent s@IfThenElse{cond, thn, els} =
  asParent s{cond=markAsExpr cond, thn=mark asParent thn, els=mark asParent els}
mark asParent s@Async{body} = asParent s{body=mark asParent body}
mark asParent s@Assign {lhs, rhs} = asStat s{lhs=markAsExpr lhs, rhs=markAsExpr rhs}
mark asParent s@Print {args} = asStat s{args=map markAsExpr args}
mark asParent s@MaybeValue{mdt=d@JustData{e}} = asParent s{mdt=d{e=mark markAsExpr e}}
mark asParent s@Let{body, decls} =
  asParent s{body=mark asParent body, decls=map markDecl decls}
  where
    markDecl (n, e) = (n, markAsExpr e)
mark asParent s@While{cond, body} = asParent s{cond=markAsExpr cond, body=markAsStat body}
mark asParent s@For{step, src, body} =
  asParent s{step=markAsExpr step, src=markAsExpr src, body=markAsStat body}
mark asParent s =
  let
    children = AST.Util.getChildren s
    children' = map markAsExpr children
  in
    asParent $ AST.Util.putChildren children' s

isForwardMethod :: MethodDecl -> Bool
isForwardMethod mdecl = not . null . (filter isForward) . mbody $ mdecl

isForwardInExpr :: Expr -> Bool
isForwardInExpr e = not . null $ filter isForward e
