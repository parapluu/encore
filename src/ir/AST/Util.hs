{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -Werror #-}
{-|
  Utility functions for "AST.AST".
-}
module AST.Util(
    foldr
    , foldrAll
    , filter
    , extend
    , extendAccum
    , extendAccumProgram
    , extractTypes
    , freeVariables
    , mapProgramClass
    ) where

import qualified Data.List as List
import Prelude hiding (foldr, filter)

import AST.AST
import Types
import Identifiers

-- | @getChildren e@ returns all children of @e@ that are Exprs themselves
getChildren :: Expr -> [Expr]
getChildren Skip{} = []
getChildren Breathe{} = []
getChildren TypedExpr {body} = [body]
getChildren MethodCall {target, args} = target : args
getChildren MessageSend {target, args} = target : args
getChildren FunctionCall {args} = args
getChildren Liftf {val} = [val]
getChildren Liftv {val} = [val]
getChildren PartyJoin {val} = [val]
getChildren PartyExtract {val} = [val]
getChildren PartySeq {par, seqfunc} = [par, seqfunc]
getChildren PartyPar {parl, parr} = [parl, parr]
getChildren Closure {body} = [body]
getChildren (MaybeValue _ (JustData e)) = [e]
getChildren (MaybeValue _ NothingData) = []
getChildren Tuple {args} = args
getChildren Async {body} = [body]
getChildren FinishAsync {body} = [body]
getChildren Foreach {arr, body} = [arr, body]
getChildren Let {body, decls} = body : map snd decls
getChildren Seq {eseq} = eseq
getChildren IfThenElse {cond, thn, els} = [cond, thn, els]
getChildren IfThen {cond, thn} = [cond, thn]
getChildren Unless {cond, thn} = [cond, thn]
getChildren While {cond, body} = [cond, body]
getChildren Repeat {name, times, body} = [times, body]
getChildren For {name, step, src, body} = [step, src, body]
getChildren Match {arg, clauses} = arg:getChildrenClauses clauses
  where
    getChildrenClauses = concatMap getChildrenClause

    getChildrenClause MatchClause {mcpattern, mchandler, mcguard} = [mcpattern, mchandler, mcguard]
getChildren Get {val} = [val]
getChildren Yield {val} = [val]
getChildren Eos {} = []
getChildren IsEos {target} = [target]
getChildren StreamNext {target} = [target]
getChildren Await {val} = [val]
getChildren Suspend {} = []
getChildren FutureChain {future, chain} = [future, chain]
getChildren FieldAccess {target} = [target]
getChildren ArrayAccess {target, index} = [target, index]
getChildren ArraySize {target} = [target]
getChildren ArrayNew {size} = [size]
getChildren ArrayLiteral {args} = args
getChildren Assign {lhs, rhs} = [lhs, rhs]
getChildren VarAccess {} = []
getChildren Null {} = []
getChildren BTrue {} = []
getChildren BFalse {} = []
getChildren NewWithInit {args} = args
getChildren New {} = []
getChildren Peer {} = []
getChildren Print {args} = args
getChildren Exit {args} = args
getChildren StringLiteral {} = []
getChildren CharLiteral {} = []
getChildren IntLiteral {} = []
getChildren RealLiteral {} = []
getChildren RangeLiteral {start, stop, step} = [start, stop, step]
getChildren Embed {} = []
getChildren Unary {operand} = [operand]
getChildren Binop {loper, roper} = [loper, roper]

-- | @putChildren children e@ returns @e@ with it's children
-- replaced by the Exprs in @children@. The expected invariant is
-- that @putChildren (getChildren e) e == e@ and @getChildren (putChildren l e) == l@
putChildren :: [Expr] -> Expr -> Expr
putChildren [] e@Skip{} = e
putChildren [] e@Breathe{} = e
putChildren [body] e@(TypedExpr {}) = e{body = body}
putChildren (target : args) e@(MethodCall {}) = e{target = target, args = args}
putChildren (target : args) e@(MessageSend {}) = e{target = target, args = args}
putChildren args e@(FunctionCall {}) = e{args = args}
putChildren [val] e@(Liftf {}) = e{val}
putChildren [val] e@(Liftv {}) = e{val}
putChildren [val] e@(PartyJoin {}) = e{val}
putChildren [val] e@(PartyExtract {}) = e{val}
putChildren [par, seqfunc] e@(PartySeq {}) = e{par=par, seqfunc=seqfunc}
putChildren [l, r] e@(PartyPar {}) = e{parl=l, parr=r}
putChildren [body] e@(Closure {}) = e{body = body}
putChildren [body] e@(Async {}) = e{body = body}
putChildren [body] e@(FinishAsync {}) = e{body = body}
putChildren [body] e@(MaybeValue _ (JustData _)) = e{mdt = JustData body}
putChildren [] e@(MaybeValue _ NothingData) = e
putChildren args e@(Tuple {}) = e{args = args}
putChildren [arr, body] e@(Foreach {}) = e{arr = arr, body = body}
putChildren (body : es) e@(Let{decls}) = e{body = body, decls = zipWith (\(name, _) e -> (name, e)) decls es}
putChildren eseq e@(Seq {}) = e{eseq = eseq}
putChildren [cond, thn, els] e@(IfThenElse {}) = e{cond = cond, thn = thn, els = els}
putChildren [cond, thn] e@(IfThen {}) = e{cond = cond, thn = thn}
putChildren [cond, thn] e@(Unless {}) = e{cond = cond, thn = thn}
putChildren [cond, body] e@(While {}) = e{cond = cond, body = body}
putChildren [times, body] e@(Repeat {}) = e{times = times, body = body}
putChildren [step, src, body] e@(For {}) = e{step = step, src = src, body = body}
putChildren (arg:clauseList) e@(Match {clauses}) = e{arg = arg, clauses=putClausesChildren clauseList clauses}
  where putClausesChildren [] [] = []
        putClausesChildren (pattern:handler:guard:rest) (mc:rClauses) = (mc{mcpattern=pattern, mchandler=handler, mcguard=guard}):putClausesChildren rest rClauses
        putClausesChildren _ _ = error "Wrong number of children of of match clause"
putChildren [val] e@(Get {}) = e{val = val}
putChildren [val] e@(Yield {}) = e{val = val}
putChildren [] e@(Eos {}) = e
putChildren [target] e@(IsEos {}) = e{target = target}
putChildren [target] e@(StreamNext {}) = e{target = target}
putChildren [val] e@(Await {}) = e{val = val}
putChildren [] e@(Suspend {}) = e
putChildren [future, chain] e@(FutureChain {}) = e{future = future, chain = chain}
putChildren [target] e@(FieldAccess {}) = e{target = target}
putChildren [target, index] e@(ArrayAccess {}) = e{target = target, index = index}
putChildren [target] e@(ArraySize {}) = e{target = target}
putChildren [size] e@(ArrayNew {}) = e{size = size}
putChildren args e@(ArrayLiteral {}) = e{args = args}
putChildren [lhs, rhs] e@(Assign {}) = e{lhs = lhs, rhs = rhs}
putChildren [] e@(VarAccess {}) = e
putChildren [] e@(Null {}) = e
putChildren [] e@(BTrue {}) = e
putChildren [] e@(BFalse {}) = e
putChildren args e@(NewWithInit {}) = e{args = args}
putChildren [] e@(New {}) = e
putChildren [] e@(Peer {}) = e
putChildren args e@(Print {}) = e{args = args}
putChildren args e@(Exit {}) = e{args = args}
putChildren [start, stop, step] e@(RangeLiteral {emeta}) = e{start = start, stop = stop, step = step}
putChildren [] e@(StringLiteral {}) = e
putChildren [] e@(CharLiteral {}) = e
putChildren [] e@(IntLiteral {}) = e
putChildren [] e@(RealLiteral {}) = e
putChildren [] e@(Embed {}) = e
putChildren [operand] e@(Unary {}) = e{operand = operand}
putChildren [loper, roper] e@(Binop {}) = e{loper = loper, roper = roper}
-- This very explicit error handling is there to make
-- -fwarn-incomplete-patterns help us find missing patterns
putChildren _ e@Skip{} = error "'putChildren l Skip' expects l to have 0 elements"
putChildren _ e@Breathe{} = error "'putChildren l Breathe' expects l to have 0 elements"
putChildren _ e@(TypedExpr {}) = error "'putChildren l TypedExpr' expects l to have 1 element"
putChildren _ e@(MaybeValue {}) = error "'putChildren l MaybeValue' expects l to have 1 element"
putChildren _ e@(Tuple {}) = error "'putChildren l Tuple' expects l to have 1 element"
putChildren _ e@(MethodCall {}) = error "'putChildren l MethodCall' expects l to have at least 1 element"
putChildren _ e@(MessageSend {}) = error "'putChildren l MessageSend' expects l to have at least 1 element"
putChildren _ e@(FunctionCall {}) = error "'putChildren l FunctionCall' expects l to have at least 1 element"
putChildren _ e@(Liftf {}) = error "'putChildren l Liftf' expects l to have 1 element"
putChildren _ e@(Liftv {}) = error "'putChildren l Liftv' expects l to have 1 element"
putChildren _ e@(PartyJoin {}) = error "'putChildren l PartyJoin' expects l to have 1 element"
putChildren _ e@(PartyExtract {}) = error "'putChildren l PartyExtract' expects l to have 1 element"
putChildren _ e@(PartySeq {}) = error "'putChildren l PartySeq' expects l to have 2 elements"
putChildren _ e@(PartyPar {}) = error "'putChildren l PartyPar' expects l to have 2 elements"
putChildren _ e@(Closure {}) = error "'putChildren l Closure' expects l to have 1 element"
putChildren _ e@(Async {}) = error "'putChildren l Async' expects l to have 1 element"
putChildren _ e@(FinishAsync {}) = error "'putChildren l FinishAsync' expects l to have 1 element"
putChildren _ e@(Foreach {}) = error "'putChildren l Foreach' expects l to have 2 elements"
putChildren _ e@(Let{decls}) = error "'putChildren l Let' expects l to have at least 1 element"
putChildren _ e@(IfThenElse {}) = error "'putChildren l IfThenElse' expects l to have 3 elements"
putChildren _ e@(IfThen {}) = error "'putChildren l IfThen' expects l to have 2 elements"
putChildren _ e@(Unless {}) = error "'putChildren l Unless' expects l to have 2 elements"
putChildren _ e@(While {}) = error "'putChildren l While' expects l to have 2 elements"
putChildren _ e@(Repeat {}) = error "'putChildren l Repeat' expects l to have 2 elements"
putChildren _ e@(For {}) = error "'putChildren l For' expects l to have 3 elements"
putChildren _ e@(Match {}) = error "'putChildren l Case' expects l to have 1 element"
putChildren _ e@(Get {}) = error "'putChildren l Get' expects l to have 1 element"
putChildren _ e@(Yield {}) = error "'putChildren l Yield' expects l to have 1 element"
putChildren _ e@(Eos {}) = error "'putChildren l Eos' expects l to have 0 elements"
putChildren _ e@(IsEos {}) = error "'putChildren l IsEos' expects l to have 1 element"
putChildren _ e@(StreamNext {}) = error "'putChildren l StreamNext' expects l to have 1 element"
putChildren _ e@(Await {}) = error "'putChildren l Await' expects l to have 1 element"
putChildren _ e@(Suspend {}) = error "'putChildren l Suspend' expects l to have 0 elements"
putChildren _ e@(FutureChain {}) = error "'putChildren l FutureChain' expects l to have 2 elements"
putChildren _ e@(FieldAccess {}) = error "'putChildren l FieldAccess' expects l to have 1 element"
putChildren _ e@(ArrayAccess {}) = error "'putChildren l ArrayAccess' expects l to have 2 elements"
putChildren _ e@(ArraySize {}) = error "'putChildren l ArraySize' expects l to have 1 element"
putChildren _ e@(ArrayNew {}) = error "'putChildren l ArrayNew' expects l to have 1 element"
putChildren _ e@(Assign {}) = error "'putChildren l Assign' expects l to have 2 elements"
putChildren _ e@(VarAccess {}) = error "'putChildren l VarAccess' expects l to have 0 elements"
putChildren _ e@(Null {}) = error "'putChildren l Null' expects l to have 0 elements"
putChildren _ e@(BTrue {}) = error "'putChildren l BTrue' expects l to have 0 elements"
putChildren _ e@(BFalse {}) = error "'putChildren l BFalse' expects l to have 0 elements"
putChildren _ e@(New {}) = error "'putChildren l New' expects l to have 0 elements"
putChildren _ e@(Peer {}) = error "'putChildren l Peer' expects l to have 0 elements"
putChildren _ e@(StringLiteral {}) = error "'putChildren l StringLiteral' expects l to have 0 elements"
putChildren _ e@(CharLiteral {}) = error "'putChildren l CharLiteral' expects l to have 0 elements"
putChildren _ e@(IntLiteral {}) = error "'putChildren l IntLiteral' expects l to have 0 elements"
putChildren _ e@(RealLiteral {}) = error "'putChildren l RealLiteral' expects l to have 0 elements"
putChildren _ e@(RangeLiteral {}) = error "'putChildren l RangeLiteral' expects l to have 3 elements"
putChildren _ e@(Embed {}) = error "'putChildren l Embed' expects l to have 0 elements"
putChildren _ e@(Unary {}) = error "'putChildren l Unary' expects l to have 1 element"
putChildren _ e@(Binop {}) = error "'putChildren l Binop' expects l to have 2 elements"

--------------- The functions below this line depend only on the two above --------------------

foldr :: (Expr -> a -> a) -> a -> Expr -> a
foldr f acc e =
    let childResult = List.foldr (\e acc -> foldr f acc e) acc (getChildren e)
    in f e childResult

foldrAll :: (Expr -> a -> a) -> a -> Program -> [a]
foldrAll f e Program{functions, traits, classes} =
  map (foldFunction f e) functions ++
  concatMap (foldTrait f e) traits ++
  concatMap (foldClass f e) classes
    where
      foldFunction f e (Function {funbody}) = foldr f e funbody
      foldClass f e (Class {cmethods}) = map (foldMethod f e) cmethods
      foldTrait f e (Trait {tmethods}) = map (foldMethod f e) tmethods
      foldMethod f e m = foldr f e (mbody m)

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
mapProgramClass p@Program{classes, imports} f =
  p{classes = map f classes, imports = map i2i imports}
  where
    i2i i@PulledImport{iprogram} = i{iprogram = p2p iprogram}
    i2i _ = error "Non desugared imports"
    p2p p@Program{classes} = p{classes = map f classes}

extendAccumProgram ::
    (acc -> Expr -> (acc, Expr)) -> acc -> Program -> (acc, Program)
extendAccumProgram f acc0 p@Program{functions, traits, classes} =
  (acc3, p{functions = funs', traits = traits', classes = classes'})
    where
      (acc1, funs') = List.mapAccumL (extendAccumFunction f) acc0 functions
      extendAccumFunction f acc fun@(Function{funbody}) =
        (acc', fun{funbody = funbody'})
        where
          (acc', funbody') = extendAccum f acc funbody

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

      extendAccumMethod f acc mtd =
        (acc', mtd{mbody = mbody'})
        where
          (acc', mbody') = extendAccum f acc (mbody mtd)

-- | @filter cond e@ returns a list of all sub expressions @e'@ of
-- @e@ for which @cond e'@ returns @True@
filter :: (Expr -> Bool) -> Expr -> [Expr]
filter cond = foldr (\e acc -> if cond e then e:acc else acc) []

extractTypes :: Program -> [Type]
extractTypes (Program{functions, traits, classes}) =
    List.nub $ concat $ concatMap extractFunctionTypes functions ++
                        concatMap extractTraitTypes traits ++
                        concatMap extractClassTypes classes
    where
      extractFunctionTypes Function {funtype, funparams, funbody} =
          typeComponents funtype :
          map extractParamTypes funparams ++
          [extractExprTypes funbody]

      extractTraitTypes Trait {tname, tfields, tmethods} =
          typeComponents tname :
          map extractFieldTypes tfields ++
          concatMap extractMethodTypes tmethods

      extractClassTypes Class {cname, cfields, cmethods} =
          typeComponents cname :
          map extractFieldTypes cfields ++
          concatMap extractMethodTypes cmethods
      extractFieldTypes Field {ftype} = typeComponents ftype

      extractMethodTypes m =
          typeComponents (mtype m) :
          map extractParamTypes (mparams m) ++
          [extractExprTypes (mbody m)]

      extractParamTypes Param {ptype} = typeComponents ptype

      extractExprTypes = foldr collectTypes []
          where
            collectTypes e acc = (typeComponents . getType) e ++ acc

freeVariables :: [Name] -> Expr -> [(Name, Type)]
freeVariables bound expr = List.nub $ freeVariables' bound expr
    where
      freeVariables' bound var@(VarAccess {name})
          | name `elem` bound = []
          | otherwise = [(name, getType var)]
      freeVariables' bound fCall@(FunctionCall {name, args})
          | name `elem` bound = concatMap (freeVariables' bound) args
          | otherwise = concatMap (freeVariables' bound) args ++ [(name, arrType)]
          where
            arrType = arrowType (map getType args) (getType fCall)
      freeVariables' bound Closure {eparams, body} =
          freeVariables' bound' body
          where
            bound' = bound ++ map pname eparams
      freeVariables' bound Let {decls, body} =
          freeVars ++ freeVariables' bound' body
          where
            (freeVars, bound') = List.foldr fvDecls ([], bound) decls
            fvDecls (x, expr) (free, bound) = (freeVariables' (x:bound) expr ++ free, x:bound)
      freeVariables' bound e = concatMap (freeVariables' bound) (getChildren e)
