{-|

The machinery used by "Typechecker.Typechecker" for handling errors and backtracing.

-}

module Typechecker.TypeError (Backtrace, emptyBT, Pushable(push), TCError(TCError), currentMethod) where

import Text.PrettyPrint
import Text.Parsec(SourcePos)
import Data.Maybe

import Identifiers
import Types
import AST.AST
import AST.PrettyPrinter

data BacktraceNode = BTFunction Name Type
                   | BTImplementTrait ImplementTrait
                   | BTTrait Trait
                   | BTClass Type
                   | BTParam ParamDecl
                   | BTField FieldDecl
                   | BTMethod MethodDecl
                   | BTExpr Expr
                   | BTType Type

instance Show BacktraceNode where
  show (BTFunction n ty) =
    concat ["In function '", show n, "' of type '", show ty, "'"]
  show (BTClass ty) = concat ["In class '", show ty, "'"]
  show (BTImplementTrait t) = concat ["In trait implementation '", show t, "'"]
  show (BTTrait t) = concat ["In trait '", show t, "'"]
  show (BTParam p) = concat ["In parameter '", show (ppParamDecl p), "'"]
  show (BTField f) =  concat ["In field '", show (ppFieldDecl f), "'"]
  show (BTMethod Method{mname, mtype}) =
    concat ["In method '", show mname, "' of type '", show mtype, "'"]
  show (BTMethod StreamMethod{mname, mtype}) =
    concat ["In stream method '", show mname, "' of type '", show mtype, "'"]
  show (BTExpr expr)
    | (isNothing . getSugared) expr = ""
    | otherwise =
      let str = show $ nest 2 $ ppSugared expr
      in concat ["In expression: \n", str]

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

currentMethod :: Backtrace -> MethodDecl
currentMethod [] =
  let
    err = unlines
      [
        "*** Internal error ***",
        "to get current method when not in a method"
      ]
  in error err
currentMethod ((_, BTMethod m):_) = m
currentMethod (_:bt) = currentMethod bt

-- | A type class for unifying the syntactic elements that can be pushed to the
-- backtrace stack.

class Pushable a where
    push :: a -> Backtrace -> Backtrace
    pushMeta ::  HasMeta a => a -> BacktraceNode -> Backtrace -> Backtrace
    pushMeta m n bt = (getPos m, n) : bt

instance Pushable Function where
    push fun@(Function {funname, funtype}) bt = (getPos fun, BTFunction funname funtype) : bt

instance Pushable Type where
  push t bt = bt

instance Pushable ImplementTrait where
  push t bt = pushMeta t (BTImplementTrait t) bt

instance Pushable Trait where
  push t bt = pushMeta t (BTTrait t) bt

instance Pushable ClassDecl where
    push c bt = (getPos c, BTClass (cname c)) : bt

instance Pushable FieldDecl where
    push f bt = (getPos f, BTField f) : bt

instance Pushable ParamDecl where
    push p bt = (getPos p, BTParam p) : bt

instance Pushable MethodDecl where
    push m bt = (getPos m, BTMethod m) : bt

instance Pushable Expr where
    push expr bt = (getPos expr, BTExpr expr) : bt


-- | The data type for a type checking error. Showing it will
-- produce an error message and print the backtrace.
newtype TCError = TCError (String, Backtrace)
instance Show TCError where
    show (TCError (msg, [])) =
        " *** Error during typechecking *** \n" ++
        msg ++ "\n"
    show (TCError (msg, bt@((pos, _):_))) =
        " *** Error during typechecking *** \n" ++
        show pos ++ "\n" ++
        msg ++ "\n" ++
        (concat $ map showBT bt)
        where
          showBT (pos, node) =
              case (show node) of
                "" -> ""
                s  -> s ++ "\n"
