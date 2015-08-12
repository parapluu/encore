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
import Control.Monad

-- Module dependencies
import AST.AST as AST
import AST.PrettyPrinter
import AST.Util
import Types as Ty
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util

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
        Pushable a => a -> CapturecheckM a
    capturecheck x = local (pushBT x) $ doCapturecheck x
    doCapturecheck ::
        a -> CapturecheckM a

instance CaptureCheckable Program where
    doCapturecheck p@Program{classes, functions} =
        do functions' <- mapM capturecheck functions
           classes' <- mapM capturecheck classes
           return p{classes = classes', functions = functions'}

instance CaptureCheckable Function where
    doCapturecheck fun@Function{funbody, funheader} =
        do let fType = htype funheader
           when (any isStackboundType (typeComponents fType)) $
                ccError $ "Reverse borrowing (returning stackbounds) " ++
                          "is currently not supported"
           funbody' <- local (pushBT funbody) $
                       extendM capturecheck funbody
           unless (isVoidType fType) $
             local (pushBT funbody') $ do
               capture funbody'
               matchStackBoundedness (getType funbody') fType
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
    doCapturecheck m@Method{mbody} =
        do let mType = methodType m
           when (any isStackboundType (typeComponents mType)) $
                ccError $ "Reverse borrowing (returning stackbounds) " ++
                          "is currently not supported"
           mbody' <- local (pushBT mbody) $
                     extendM capturecheck mbody
           unless (isVoidType mType) $
             local (pushBT mbody') $ do
               capture mbody'
               matchStackBoundedness (getType mbody') mType
           return m{mbody = mbody'}

instance CaptureCheckable Expr where
    doCapturecheck e@Null{} =
        free e

    doCapturecheck e@NewWithInit{args} =
        do mapM_ capture args
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

    doCapturecheck e@Match{arg, clauses} =
        do isLin <- isLinearType (getType arg)
           when False $
                ccError "Matching on linear values is currently not supported"
           if all isFree $ map mchandler clauses
           then return $ makeFree e
           else return $ makeCaptured e

    doCapturecheck e@TypedExpr{body, ty} =
        do let bodyType = getType body
           matchStackBoundedness bodyType ty
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
           return e

    doCapturecheck e@FunctionCall{args} =
        do let funType = getArrowType e
               paramTypes = getArgTypes funType
           zipWithM_ captureOrBorrow args paramTypes
           assertNoDuplicateBorrow args paramTypes
           free e

    doCapturecheck e@Closure{eparams, body} =
        do let freeVars = freeVariables (map pname eparams) body
           freeLins <- filterM (isLinearType . snd) freeVars
           let firstLin = head freeLins
           unless (null freeLins) $
             ccError $ "Cannot capture variable '" ++ show (fst firstLin) ++
                       "' of linear type '" ++ show (snd firstLin) ++
                       "' in a closure"
           capture body
           when (any isStackboundType (typeComponents (getType body))) $
                ccError $ "Reverse borrowing (returning stackbounds) " ++
                          "is currently not supported"
           free e

    doCapturecheck e@Seq{eseq} =
        e `returns` last eseq

    doCapturecheck e@IfThenElse{thn, els} = do
        isLin <- isLinearType (getType e)
        if isLin && isFree thn && isFree els
        then return $ makeFree e
        else return $ makeCaptured e

    doCapturecheck e@While{body} =
        e `returns` body

    doCapturecheck e@For{body} =
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

    doCapturecheck e@Tuple{args} =
        do mapM_ capture args
           free e

    doCapturecheck e@MaybeValue {mdt = JustData{e = contents}} =
        do capture contents
           free e

    doCapturecheck e = return e

-- | An expression of linear type can only be 'capture'd if it is
-- also free (see 'capturecheck') or stackbound. A non-linear
-- expression is trivially captured. Successful capturing has no
-- effect.
capture :: Expr -> CapturecheckM ()
capture e = do
    let ty = getType e
    whenM (isLinearType ty) $
      unless (isFree e || isStackboundType ty) $
        pushError e $
          "Cannot capture expression '" ++ show (ppExpr e) ++
          if isModeless ty
          then "' of linear type '" ++ show ty ++ "'"
          else "' of type '" ++ show ty ++ "'"
--    when (isStackboundType ty && not (isNull e)) $
--      pushError e $
--        "Cannot alias borrowed expression '" ++ show (ppExpr e) ++ "'"

-- | An expression of linear type can be marked 'free' to signal
-- that it is safe to 'capture' its result
free :: Expr -> CapturecheckM Expr
free e = do
    isLin <- isLinearType (getType e)
    return $ if isLin
             then makeFree e
             else makeCaptured e

captureOrBorrow :: Expr -> Type -> CapturecheckM ()
captureOrBorrow e ty
    | isStackboundType ty = unless (isFree e) (assertBorrowable e)
    | otherwise =
        do when (isStackboundType (getType e)) $
             ccError $ "Cannot pass stack-bound expression '" ++
                       show (ppExpr e) ++ "' as non-stackbound parameter"
           capture e
    where
      assertBorrowable VarAccess{} = return ()
      assertBorrowable e@FieldAccess{target, name} =
          whenM (isLinearType (getType e)) $
            unlessM (linearAllTheWay target) $
              ccError $ "Cannot borrow linear field '" ++ show name ++
                        "' from non-linear path '" ++
                        show (ppExpr target) ++ "'"
      assertBorrowable e@ArrayAccess{target} =
          whenM (isLinearType (getType e)) $
            unlessM (linearAllTheWay target) $
              ccError $ "Cannot borrow linear array value from " ++
                        "non-linear path '" ++ show (ppExpr target) ++ "'"
      assertBorrowable e =
          ccError $ "Expression '" ++ show (ppExpr e) ++
                    "' cannot be borrowed. Go ask Elias why."

      linearAllTheWay e@FieldAccess{} = linearPath e
      linearAllTheWay e@MethodCall{}  = linearPath e
      linearAllTheWay e@MessageSend{} = linearPath e
      linearAllTheWay e@ArrayAccess{} = linearPath e
      linearAllTheWay e@Consume{}     = linearPath e
      linearAllTheWay e = isLinearType (getType e)

      linearPath e =
          liftM2 (&&)
                 (isLinearType (getType e))
                 (linearAllTheWay (target e))


sendArgument :: Expr -> Expr -> Type -> CapturecheckM ()
sendArgument target arg paramType = do
  argIsLinear <- isLinearType (getType arg)
  when (isStackboundType paramType && not isSyncCall) $ do
       when (argIsLinear && not (isFree arg)) $
           ccError $ "Expression '" ++ show (ppExpr arg) ++
                     "' cannot be borrowed by active object " ++
                     "of type '" ++ show targetType ++ "'"
       when (isStackboundType (getType arg)) $
           ccError $ "Cannot send stack-bound expression '" ++
                     show (ppExpr arg) ++ "' to active object " ++
                     "of type '" ++ show targetType ++ "'"
  captureOrBorrow arg paramType
    where
      targetType = getType target
      isSyncCall = isThisAccess target ||
                   isPassiveClassType targetType

assertNoDuplicateBorrow :: [Expr] -> [Type] -> CapturecheckM ()
assertNoDuplicateBorrow args paramTypes = do
  let posts = zip3 [0..(length args)] args paramTypes
      borrowedPosts = List.filter isBorrowed posts
  mapM_ (noDuplicateBorrow posts) borrowedPosts
    where
      noDuplicateBorrow posts (i, arg, _) =
        when (any (isDuplicateBorrow i arg) posts) $
             ccError $ "Borrowed variable '" ++ show (ppExpr $ rootOf arg) ++
                       "' cannot be used more than once in an argument list"

      isDuplicateBorrow i borrowed (j, arg, ty)
          | i == j = False

          | VarAccess{name} <- borrowed =
              name `elem` map fst (freeVariables [] arg)

          | FieldAccess{target} <- borrowed =
              case rootOf target of
                VarAccess{name} -> name `elem` map fst (freeVariables [] arg)
                _ -> False

          | ArrayAccess{target} <- borrowed =
              case rootOf target of
                VarAccess{name} -> name `elem` map fst (freeVariables [] arg)
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

matchStackBoundedness :: Type -> Type -> CapturecheckM ()
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
returns :: Expr -> Expr -> CapturecheckM Expr
returns parent child =
    return $ if isFree child
             then makeFree parent
             else makeCaptured parent

isLinearType :: Type -> CapturecheckM Bool
isLinearType = isLinearType' []
    where
      isLinearType' :: [Type] -> Type -> CapturecheckM Bool
      isLinearType' checked ty = do
        let components = typeComponents (dropArrows ty)
            unchecked = components \\ checked
            classes = List.filter isClassType unchecked
        capabilities <- mapM findCapability classes
        liftM2 (||)
              (anyM isDirectlyLinear unchecked)
              (anyM (isLinearType' (checked ++ unchecked)) capabilities)

      isDirectlyLinear :: Type -> CapturecheckM Bool
      isDirectlyLinear ty
          | isClassType ty = do
              cap <- findCapability ty
              let components = typeComponents (dropArrows ty) ++
                               typeComponents (dropArrows cap)
              return $ any isLinearRefType components
          | otherwise = do
              let components = typeComponents (dropArrows ty)
              return $ any isLinearRefType components

      dropArrows = typeMap dropArrow
      dropArrow ty
          | isArrowType ty = voidType
          | otherwise = ty