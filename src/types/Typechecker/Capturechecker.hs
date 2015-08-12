{-|

Capturechecks an "AST.AST" and produces the same tree, extended
with information of which expression nodes are 'free', i.e. have
no references to it directly after evaluation. It throws an
exception with a meaningful error message if it fails.

-}

module Typechecker.Capturechecker(capturecheckEncoreProgram) where

import Data.List as List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except

-- Module dependencies
import AST.AST as AST
import AST.PrettyPrinter
import AST.Util
import Types as Ty
import Typechecker.Environment
import Typechecker.TypeError

capturecheckEncoreProgram ::
    Program -> Environment -> Either CCError Program
capturecheckEncoreProgram p env =
  runReader (runExceptT (doCapturecheck p)) env

ccError msg = do bt <- asks backtrace
                 throwError $ CCError (msg, bt)

pushError x msg = local (pushBT x) $ ccError msg

class CaptureCheckable a where
    -- | 'capturecheck' checks that linear references are actually
    -- handled linearly. A node that after evaluation has at least
    -- one reference to the resulting value is called captured,
    -- otherwise it is called free. For each node, 'capturecheck'
    -- conservatively decides if that node is captured or free,
    -- and which of its children (if any) it captures.
    capturecheck ::
        Pushable a => a -> ExceptT CCError (Reader Environment) a
    capturecheck x = local (pushBT x) $ doCapturecheck x
    doCapturecheck ::
        a -> ExceptT CCError (Reader Environment) a

instance CaptureCheckable Program where
    doCapturecheck p@Program{classes, functions} =
        do functions' <- mapM capturecheck functions
           classes' <- mapM capturecheck classes
           return p{classes = classes', functions = functions'}

instance CaptureCheckable Function where
    doCapturecheck fun@Function{funbody} =
        do funbody' <- local (pushBT funbody) $
                       extendM capturecheck funbody
           local (pushBT funbody') $ capture funbody'
           return fun{funbody = funbody'}

instance CaptureCheckable ClassDecl where
    doCapturecheck c@Class{cmethods, cfields} =
        do cmethods' <- mapM capturecheck cmethods
           mapM_ notStackbound cfields
           return c{cmethods = cmethods'}
        where
          notStackbound f@Field{ftype} =
              local (pushBT f) $
              when (isStackboundType ftype) $
                   ccError $ "Cannot have field of stackbound type '" ++
                             show ftype ++ "'"

-- TODO: Find all smallest result expressions for better error
-- messages. resultOf (let x = 42 in x + 1) = x + 1
instance CaptureCheckable MethodDecl where
    doCapturecheck m@Method{mbody, mtype} =
        do mbody' <- local (pushBT mbody) $
                     extendM capturecheck mbody
           unless (isVoidType mtype) $
             local (pushBT mbody') $ capture mbody'
           return m{mbody = mbody'}
    doCapturecheck m@StreamMethod{mbody} =
        do mbody' <- local (pushBT mbody) $
                     extendM capturecheck mbody
           return m{mbody = mbody'}

instance CaptureCheckable Expr where
    doCapturecheck e@Null{} =
        free e

    doCapturecheck e@New{} =
        free e

    doCapturecheck e@Peer{} =
        free e

    doCapturecheck e@VarAccess{} =
        return $ makeCaptured e

    doCapturecheck e@FieldAccess{} =
        return $ makeCaptured e

    doCapturecheck e@Consume{} =
        free e

    doCapturecheck e@Assign{lhs, rhs} =
        do let lType = getType lhs
               rType = getType rhs
           matchStackBoundedness lType rType
           capture rhs
           return e

    doCapturecheck e@Let{decls, body} =
        do mapM_ (capture . snd) decls
           e `returns` body

    doCapturecheck e@TypedExpr{body, ty} =
        do let bodyType = getType body
           matchStackBoundedness bodyType ty
           e `returns` body

    doCapturecheck e@MethodCall{target, name, args} =
        do let funType = getArrowType e
               argTypes = getArgTypes funType
           zipWithM_ (sendArgument target) args argTypes
           free e

    doCapturecheck e@MessageSend{target, name, args} =
        do let funType = getArrowType e
               argTypes = getArgTypes funType
           zipWithM_ (sendArgument target) args argTypes
           return e

    doCapturecheck e@FunctionCall{args} =
        do let funType = getArrowType e
               argTypes = getArgTypes funType
           zipWithM_ captureOrBorrow args argTypes
           free e

    doCapturecheck e@Closure{eparams, body} =
        do let freeVars = freeVariables (map pname eparams) body
               freeLins = List.filter (isLinearType . snd) freeVars
               firstLin = head freeLins
           unless (null freeLins) $
             ccError $ "Cannot capture variable '" ++ show (fst firstLin) ++
                       "' of linear type '" ++ show (snd firstLin) ++
                       "' in a closure"
           capture body
           free e

    doCapturecheck e@Seq{eseq} =
        e `returns` last eseq

    doCapturecheck e@IfThenElse{thn, els} =
        if isLinearType (getType e) && isFree thn && isFree els
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

    doCapturecheck e@ArrayNew{} =
        free e

    doCapturecheck e@ArrayAccess{target} =
        return $ makeCaptured e

    doCapturecheck e@ArrayLiteral{args} =
        do mapM_ capture args
           free e

    doCapturecheck e = return e

-- | An expression of linear type can only be 'capture'd if it is
-- also free (see 'capturecheck'). A non-linear expression is
-- trivially captured. Successful capturing has no effect.
capture :: Expr -> ExceptT CCError (Reader Environment) ()
capture e = do
    let ty = getType e
    when (isLinearType ty) $
      unless (isFree e) $
        pushError e $
          "Cannot capture expression '" ++ show (ppExpr e) ++
          if isClassType ty
          then "' of linear type '" ++ show ty ++ "'"
          else "' of type '" ++ show ty ++ "'"
    when (isStackboundType ty) $
      pushError e $
        "Cannot alias borrowed expression '" ++ show (ppExpr e) ++ "'"

-- | An expression of linear type can be marked 'free' to signal
-- that it is safe to 'capture' its result
free :: Expr -> ExceptT CCError (Reader Environment) Expr
free e =
    return $ if isLinearType (getType e)
             then makeFree e
             else makeCaptured e

captureOrBorrow ::
    Expr -> Type -> ExceptT CCError (Reader Environment) ()
captureOrBorrow e ty
    | isStackboundType ty = return ()
    | otherwise =
        do when (isStackboundType (getType e)) $
             ccError $ "Cannot pass stack-bound expression '" ++
                       show (ppExpr e) ++ "' as non-stackbound parameter"
           capture e

sendArgument ::
    Expr -> Expr -> Type -> ExceptT CCError (Reader Environment) ()
sendArgument target arg paramType
    | isStackboundType paramType
    , isLinearType (getType arg)
    , not (isFree arg) =
        unless isSyncCall $
             ccError $ "Expression '" ++ show (ppExpr arg) ++
                       "' cannot be borrowed by active object " ++
                       "of type '" ++ show targetType ++ "'"
    | isStackboundType (getType arg) =
        unless isSyncCall $
             ccError $ "Cannot send stack-bound expression '" ++
                       show (ppExpr arg) ++ "' to active object " ++
                       "of type '" ++ show targetType ++ "'"
    | otherwise = captureOrBorrow arg paramType
    where
      targetType = getType target
      isSyncCall = isThisAccess target ||
                   isPassiveClassType targetType

matchStackBoundedness :: Type -> Type -> ExceptT CCError (Reader Environment) ()
matchStackBoundedness expected ty
    |  isStackboundType expected &&
       not (isStackboundType ty)
    || not (isStackboundType expected) &&
       isStackboundType ty =
          ccError $ kindOf ty ++ " does not match " ++ kindOf' expected
    | otherwise = return ()
    where
      kindOf ty
          | isStackboundType ty = "Stack-bound type '" ++ show ty ++ "'"
          | otherwise = "Non stack-bound type '" ++ show ty ++ "'"
      kindOf' ty
          | isStackboundType ty = "stack-bound type '" ++ show ty ++ "'"
          | otherwise = "non stack-bound type '" ++ show ty ++ "'"


-- | Convenience function for when a node inherits the same
-- captivitiy status as its child. For example @let x = e in e'@
-- is free iff @e'@ is free.
returns :: Expr -> Expr -> ExceptT CCError (Reader Environment) Expr
returns parent child =
    return $ if isFree child
             then makeFree parent
             else makeCaptured parent
