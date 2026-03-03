{-# LANGUAGE ConstrainedClassMethods #-}
{-|

Capturechecks an "AST.AST" and produces the same tree, extended
with information of which expression nodes are 'free', i.e. have
no references to it directly after evaluation. It throws an
exception with a meaningful error message if it fails.

-}

module Typechecker.Capturechecker(capturecheckProgram) where

import Data.List as List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Debug.Trace

-- Module dependencies
import AST.AST as AST
import AST.PrettyPrinter
import AST.Util
import Types as Ty
import Identifiers
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

capturecheckProgram :: Map FilePath LookupTable -> Program ->
                       (Either TCError (Environment, Program), [TCWarning])
capturecheckProgram table p = do
  let env = buildEnvironment table p
  let reader = (\p -> (env, p)) <$> runReaderT (doCapturecheck p) env
  runState (runExceptT reader) []

class CaptureCheckable a where
    -- | 'capturecheck' checks that linear references are actually
    -- handled linearly. A node that after evaluation has at least
    -- one reference to the resulting value is called captured,
    -- otherwise it is called free. For each node, 'capturecheck'
    -- conservatively decides if that node is captured or free,
    -- and which of its children (if any) it captures.
    capturecheck :: Pushable a => a -> TypecheckM a
    capturecheck x = local (pushBT x) $ doCapturecheck x
    doCapturecheck :: a -> TypecheckM a

instance CaptureCheckable Program where
    doCapturecheck p@Program{classes, traits, functions} =
        do functions' <- mapM capturecheck functions
           traits' <- mapM capturecheck traits
           classes' <- mapM capturecheck classes
           return p{classes = classes'
                   ,traits = traits'
                   ,functions = functions'}

instance CaptureCheckable FunctionHeader where
    doCapturecheck = return

instance CaptureCheckable Function where
    doCapturecheck fun@Function{funbody} =
        do let funtype = functionType fun
           when (any isStackboundType (typeComponents funtype)) $
                tcError ReverseBorrowingError
           funbody' <- local (pushBT funbody) $
                       extendM capturecheck funbody
           unless (isUnitType funtype) $
             local (pushBT funbody') $ do
               capture funbody'
               matchStackBoundedness (getType funbody') funtype
           return fun{funbody = funbody'}

instance CaptureCheckable FieldDecl where
    doCapturecheck f@Field{ftype} = do
        when (isStackboundType ftype) $
             tcError $ BorrowedFieldError ftype
        return f

instance CaptureCheckable ClassDecl where
    doCapturecheck c@Class{cname, cmethods, cfields} = do
      cfields' <- mapM (local addThis . capturecheck) cfields
      cmethods' <- mapM (local addThis . capturecheck) cmethods
      return c{cfields = cfields'
              ,cmethods = cmethods'}
        where
          addThis = extendEnvironment [(thisName, cname)]

instance CaptureCheckable TraitDecl where
    doCapturecheck t@Trait{tname, treqs, tmethods} = do
      treqs' <- mapM (local addThis . doCapturecheck) treqs
      tmethods' <- mapM (local addThis . capturecheck) tmethods
      return t{treqs = treqs'
              ,tmethods = tmethods'}
        where
          addThis = extendEnvironment [(thisName, tname)]

instance CaptureCheckable Requirement where
    doCapturecheck req
        | isRequiredField req = do
            rfield' <- capturecheck $ rfield req
            return req{rfield = rfield'}
        | isRequiredMethod req = do
            rheader' <- doCapturecheck $ rheader req
            return req{rheader = rheader'}
        | otherwise =
            error $ "Capturechecker.hs: requirement '" ++ show req ++
                    "' is neither a field, nor a method"


-- TODO: Find all smallest result expressions for better error
-- messages. resultOf (let x = 42 in x + 1) = x + 1
instance CaptureCheckable MethodDecl where
    doCapturecheck m@Method{mbody} =
        do let mtype = methodType m
           when (any isStackboundType (typeComponents mtype)) $
                tcError ReverseBorrowingError
           mbody' <- local (pushBT mbody) $
                     extendM capturecheck mbody
           unless (isUnitType mtype) $
             local (pushBT mbody') $ do
               capture mbody'
               matchStackBoundedness (getType mbody') mtype
           return m{mbody = mbody'}

instance CaptureCheckable Expr where
    doCapturecheck e@Null{} =
        free e

    doCapturecheck e@Embed{} =
        free e

    doCapturecheck e@NewWithInit{args} =
        do let funType = getArrowType e
               paramTypes = getArgTypes funType
           zipWithM_ (sendArgument e) args paramTypes
           assertNoDuplicateBorrow args paramTypes
           free e

    doCapturecheck e@VarAccess{} =
        return $ makeCaptured e

    doCapturecheck e@FieldAccess{} =
        return $ makeCaptured e

    doCapturecheck e@Consume{} =
        free e

    doCapturecheck e@For{body} =
        return $ makeCaptured e

    doCapturecheck e@Assign{lhs, rhs} =
        do let lType = getType lhs
               rType = getType rhs
           matchStackBoundedness rType lType
           capture rhs
           return e

    doCapturecheck e@Let{decls, body} =
        do mapM_ (capture . snd) decls
           e `returns` body

    doCapturecheck e@TypedExpr{body, ty} =
        do let bodyType = getType body
           matchStackBoundedness bodyType ty
           capture body
           e `returns` body

    doCapturecheck e@MethodCall{target, name, args} =
        do let funType = getArrowType e
               paramTypes = getArgTypes funType
           zipWithM_ (sendArgument target) args paramTypes
           assertNoDuplicateBorrow args paramTypes
           free e

    doCapturecheck e@MessageSend{target, name, args} =
        do let funType = getArrowType e
               paramTypes = getArgTypes funType
           zipWithM_ (sendArgument target) args paramTypes
           assertNoDuplicateBorrow args paramTypes
           free e

    doCapturecheck e@FunctionCall{args} =
        do let funType = getArrowType e
               paramTypes = getArgTypes funType
           zipWithM_ captureOrBorrow args paramTypes
           assertNoDuplicateBorrow args paramTypes
           free e

    doCapturecheck e@Closure{eparams, body} =
        do let freeVars = freeVariables (map (qLocal . pname) eparams) body
           freeLins <- filterM (isLinearType . snd) freeVars
           let nonBorrowed = List.filter (not . isStackboundType . snd) freeLins
               firstLin = head nonBorrowed
           unless (null nonBorrowed) $
             tcError $ uncurry LinearClosureError firstLin
           capture body
           when (any isStackboundType (typeComponents (getType body))) $
                tcError ReverseBorrowingError
           free e

    doCapturecheck e@Seq{eseq} =
        e `returns` last eseq

    doCapturecheck e@MaybeValue{mdt = JustData{e = body}} =
        e `returns` body

    doCapturecheck e@MaybeValue{mdt = NothingData} =
        free e

    doCapturecheck e@Tuple{args} = do
      unless (isPattern e) $
             mapM_ capture args
      free e

    doCapturecheck e@Match{arg, clauses} = do
        capture arg
        isLin <- isLinearType (getType e)
        if isLin && all (isFree . mchandler) clauses
        then return $ makeFree e
        else return $ makeCaptured e

    doCapturecheck e@Borrow{target, body} = do
        captureOrBorrow target (getType target)
        e `returns` body

    doCapturecheck e@IfThenElse{thn, els} = do
        isLin <- isLinearType (getType e)
        if isLin && isFree thn && isFree els
        then return $ makeFree e
        else return $ makeCaptured e

    doCapturecheck e@While{body} =
        e `returns` body

    doCapturecheck e@Get{val} =
        e `returns` val

    doCapturecheck e@Yield{val} =
        do capture val
           return e

    doCapturecheck e@StreamNext{target} =
        e `returns` target

    doCapturecheck e@FutureChain{future} =
        do capture future
           e `returns` future

    doCapturecheck e@PartySeq{par} =
        do capture par
           e `returns` par

    doCapturecheck e@PartyPar{parl, parr} =
        do capture parl
           capture parr
           return $ if isFree parl && isFree parr
                    then makeFree e
                    else makeCaptured e

    doCapturecheck e@ArrayNew{} =
        free e

    doCapturecheck e@ArrayAccess{target} =
        return $ makeCaptured e

    doCapturecheck e@ArrayLiteral{args} =
        do mapM_ capture args
           free e

    doCapturecheck e = return e

-- | An expression of linear type can only be 'capture'd if it is
-- also free (see 'capturecheck') or stackbound. A non-linear
-- expression is trivially captured. Successful capturing has no
-- effect.
capture :: Expr -> TypecheckM ()
capture e = do
    let ty = getType e
    whenM (isLinearType ty) $
      unless (isFree e || isStackboundType ty) $
        pushError e $ LinearCaptureError e ty
--    when (isStackboundType ty && not (isNull e)) $
--      pushError e $
--        "Cannot alias borrowed expression '" ++ show (ppExpr e) ++ "'"

-- | An expression of linear type can be marked 'free' to signal
-- that it is safe to 'capture' its result
free :: Expr -> TypecheckM Expr
free e = do
    isLin <- isLinearType (getType e)
    return $ if isLin
             then makeFree e
             else makeCaptured e

captureOrBorrow :: Expr -> Type -> TypecheckM ()
captureOrBorrow e ty
    | isStackboundType ty = unless (isFree e) (assertBorrowable e)
    | otherwise =
        do when (isStackboundType (getType e)) $
             tcError $ BorrowedLeakError e
           capture e
    where
      assertBorrowable VarAccess{} = return ()
      assertBorrowable e@FieldAccess{target, name} =
          whenM (isLinearType (getType e)) $
            unlessM (linearAllTheWay target) $
              tcError $ NonBorrowableError e
      assertBorrowable e@ArrayAccess{target} =
          whenM (isLinearType (getType e)) $
            unlessM (linearAllTheWay target) $
              tcError $ NonBorrowableError e
      assertBorrowable e@TypedExpr{body} =
          assertBorrowable body
      assertBorrowable e = return ()

      linearAllTheWay e@FieldAccess{} = linearPath e
      linearAllTheWay e@MethodCall{}  = linearPath e
      linearAllTheWay e@MessageSend{} = linearPath e
      linearAllTheWay e@ArrayAccess{} = linearPath e
      linearAllTheWay e@Consume{}     = linearPath e
      linearAllTheWay e@TupleAccess{} = linearPath e
      linearAllTheWay e = linearAndNonBorrowed (getType e)

      linearPath e =
          liftM2 (&&)
                 (linearAndNonBorrowed (getType e))
                 (linearAllTheWay (target e))

      linearAndNonBorrowed ty =
          liftM (&& not (isStackboundType ty)) (isLinearType ty)

sendArgument :: Expr -> Expr -> Type -> TypecheckM ()
sendArgument target arg paramType = do
  argIsLinear <- isLinearType (getType arg)
  targetIsActive <- isActiveType targetType
  when (isStackboundType paramType &&
        targetIsActive && not (isThisAccess target)) $ do
       when (argIsLinear && not (isFree arg)) $
           tcError $ ActiveBorrowError arg targetType
       when (isStackboundType (getType arg)) $
           tcError $ ActiveBorrowSendError arg targetType
  captureOrBorrow arg paramType
    where
      targetType = getType target

assertNoDuplicateBorrow :: [Expr] -> [Type] -> TypecheckM ()
assertNoDuplicateBorrow args paramTypes = do
  let posts = zip3 [0..(length args)] args paramTypes
      borrowedPosts = List.filter isBorrowed posts
  mapM_ (noDuplicateBorrow posts) borrowedPosts
    where
      noDuplicateBorrow posts (i, arg, _) =
        when (any (isDuplicateBorrow i arg) posts) $
             tcError $ DuplicateBorrowError (rootOf arg)

      isDuplicateBorrow i borrowed (j, arg, ty)
          | i == j = False

          | VarAccess{qname} <- borrowed =
              qname `elem` map fst (freeVariables [] arg)

          | FieldAccess{target} <- borrowed =
              case rootOf target of
                VarAccess{qname} ->
                  qname `elem` map fst (freeVariables [] arg)
                _ -> False

          | ArrayAccess{target} <- borrowed =
              case rootOf target of
                VarAccess{qname} ->
                  qname `elem` map fst (freeVariables [] arg)
                _ -> False

          | otherwise =
              error $ "Capturechecker.hs: Expression '" ++ show (ppExpr arg) ++
                      "' should not be borrowable"

      rootOf FieldAccess{target} = rootOf target
      rootOf MethodCall{target}  = rootOf target
      rootOf MessageSend{target} = rootOf target
      rootOf ArrayAccess{target} = rootOf target
      rootOf Consume{target}     = rootOf target
      rootOf e = e

      isBorrowed (_, VarAccess{}, ty) =
          isStackboundType ty
      isBorrowed (_, FieldAccess{}, ty) =
          isStackboundType ty
      isBorrowed (_, ArrayAccess{}, ty) =
          isStackboundType ty
      isBorrowed _ = False

matchStackBoundedness :: Type -> Type -> TypecheckM ()
matchStackBoundedness ty expected
    |  isStackboundType expected &&
       not (isStackboundType ty)
    || not (isStackboundType expected) &&
       isStackboundType ty =
          tcError $ StackboundednessMismatchError ty expected
    | otherwise = return ()

-- | Convenience function for when a node inherits the same
-- captivitiy status as its child. For example @let x = e in e'@
-- is free iff @e'@ is free.
returns :: Expr -> Expr -> TypecheckM Expr
returns parent child =
    return $ if isFree child
             then makeFree parent
             else makeCaptured parent
