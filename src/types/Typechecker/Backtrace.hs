{-|

The backtrace of the typechecker passes, used for tracking the
current position of the typechecker.

-}

module Typechecker.Backtrace(Backtrace
                            ,emptyBT
                            ,reduceBT
                            ,Pushable(push)
                            ,ExecutionContext(..)
                            ,currentContextFromBacktrace
                            ,validUseOfBreak
                            ,validUseOfContinue
                            ) where

import Data.Maybe
import Data.List
import Text.PrettyPrint

import Identifiers
import AST.Meta(Position)
import AST.AST
import AST.PrettyPrinter
import Types

data BacktraceNode = BTFunction Name Type
                   | BTTrait Type
                   | BTClass Type
                   | BTParam ParamDecl
                   | BTField FieldDecl
                   | BTMethod MethodDecl
                   | BTExpr Expr
                   | BTTypedef Type
                   | BTModule Name
                   | BTImport Namespace
                     deriving(Eq)

isBTExpr :: BacktraceNode -> Bool
isBTExpr (BTExpr _) = True
isBTExpr _ = False

instance Show BacktraceNode where
  show (BTFunction n ty) =
    concat ["In function '", show n, "' of type '", show ty, "'"]
  show (BTClass ty) = concat ["In class '", show ty, "'"]
  show (BTTrait ty) = concat ["In trait '", show ty, "'"]
  show (BTParam p) = concat ["In parameter '", show (ppParamDecl p), "'"]
  show (BTField f) =  concat ["In field '", show (ppFieldDecl f), "'"]
  show (BTMethod m) =
      let name = hname $ mheader m
          ty   = htype $ mheader m
          method | isStreamMethod m = "stream method"
                 | otherwise = "method"
      in
        concat ["In ", method, " '", show name, "' of type '", show ty, "'"]
  show (BTExpr expr)
    | (isNothing . getSugared) expr = ""
    | otherwise =
      let str = show $ nest 2 $ ppSugared expr
      in "In expression: \n" ++ str
  show (BTTypedef tl) =
     concat ["In typedef '", show tl, "'"]
  show (BTModule m) =
     concat ["In declaration of module '", show m, "'"]
  show (BTImport ns) =
     concat ["In import of module '", show ns, "'"]

type Backtrace = [(Position, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

reduceBT :: Backtrace -> Backtrace
reduceBT = truncateExprs . dropMiniLets . mergeBlocks . nub
  where
    mergeBlocks ((pos1, BTExpr seq@Seq{}):(pos2, BTExpr e2):bt) =
      if hasBody e2
      then mergeBlocks $ (pos2, BTExpr e2):bt
      else (pos1, BTExpr seq) : mergeBlocks ((pos2, BTExpr e2) : bt)
    mergeBlocks (node:bt) = node:mergeBlocks bt
    mergeBlocks [] = []

    dropMiniLets :: Backtrace -> Backtrace
    dropMiniLets = filter (not . isMiniLetNode . snd)
    isMiniLetNode node
      | BTExpr e <- node
      , Just MiniLet{} <- getSugared e = True
      | otherwise = False

    truncateExprs ((pos1, BTExpr e1):(pos2, BTExpr e2):bt) =
      (pos1, BTExpr e1):(pos2, BTExpr e2):
      filter (not . isBTExpr . snd) bt
    truncateExprs bt = bt

data ExecutionContext = MethodContext MethodDecl
                      | ClosureContext (Maybe Type)
                      | FunctionContext Name Type

currentContextFromBacktrace :: Backtrace -> ExecutionContext
currentContextFromBacktrace [] = error "TypeError.hs: No execution context"
currentContextFromBacktrace ((_, BTExpr Closure{mty}):_) = ClosureContext mty
currentContextFromBacktrace ((_, BTMethod m):_) =  MethodContext m
currentContextFromBacktrace ((_, BTFunction f t):_) =  FunctionContext f t
currentContextFromBacktrace (_:bt) = currentContextFromBacktrace bt

validUseOfBreak :: Backtrace -> Bool
validUseOfBreak [] = False
validUseOfBreak ((_, BTExpr l@For{}):_) = True
validUseOfBreak ((_, BTExpr l@While{}):_) = True
validUseOfBreak ((_, BTExpr l@Repeat{}):_) = True
validUseOfBreak ((_, BTExpr c@Closure{}):_) = False
validUseOfBreak (_:bt) = validUseOfBreak bt

validUseOfContinue :: Backtrace -> Bool
validUseOfContinue [] = False
validUseOfContinue ((_, BTExpr l@For{}):_) = False
validUseOfContinue ((_, BTExpr l@While{}):_) = True
validUseOfContinue ((_, BTExpr l@DoWhile{}):_) = True
validUseOfContinue ((_, BTExpr l@Repeat{}):_) = True
validUseOfContinue ((_, BTExpr c@Closure{}):_) = False
validUseOfContinue (_:bt) = validUseOfContinue bt

-- | A type class for unifying the syntactic elements that can be pushed to the
-- backtrace stack.

class Pushable a where
    push :: a -> Backtrace -> Backtrace
    pushMeta ::  HasMeta a => a -> BacktraceNode -> Backtrace -> Backtrace
    pushMeta m n bt = (getPos m, n) : bt

instance Pushable Function where
  push fun =
    pushMeta fun (BTFunction (functionName fun) (functionType fun))

instance Pushable TraitDecl where
  push t = pushMeta t (BTTrait (tname t))

instance Pushable ClassDecl where
    push c = pushMeta c (BTClass (cname c))

instance Pushable FieldDecl where
    push f = pushMeta f (BTField f)

instance Pushable ParamDecl where
    push p = pushMeta p (BTParam p)

instance Pushable MethodDecl where
    push m = pushMeta m (BTMethod m)

instance Pushable Expr where
    push expr = pushMeta expr (BTExpr expr)

instance Pushable Typedef where
    push t@(Typedef {typedefdef}) = pushMeta t (BTTypedef typedefdef)

instance Pushable ModuleDecl where
    push m@(Module{modname}) = pushMeta m (BTModule modname)

instance Pushable ImportDecl where
    push i@(Import{itarget}) = pushMeta i (BTImport itarget)
