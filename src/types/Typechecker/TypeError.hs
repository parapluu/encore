{-|

The machinery used by "Typechecker.Typechecker" and
"Typechecker.Capturechecker" for handling errors and backtracing.

-}

module Typechecker.TypeError (Backtrace
                             ,emptyBT
                             ,Pushable(push)
                             ,TCError(TCError)
                             ,CCError(CCError)
                             ,currentMethod
                             ) where

import Text.PrettyPrint
import Text.Parsec(SourcePos)
import Data.Maybe
import Data.List

import Identifiers
import Types
import AST.AST
import AST.PrettyPrinter

data BacktraceNode = BTPulledImport QName
                   | BTFunction Name Type
                   | BTTrait Type
                   | BTClass Type
                   | BTParam ParamDecl
                   | BTField FieldDecl
                   | BTMethod MethodDecl
                   | BTExpr Expr

instance Show BacktraceNode where
  show (BTPulledImport qname) =
    concat ["In imported module '", intercalate "." (map show qname), "'"]
  show (BTFunction n ty) =
    concat ["In function '", show n, "' of type '", show ty, "'"]
  show (BTClass ty) = concat ["In class '", show ty, "'"]
  show (BTTrait ty) = concat ["In trait '", show ty, "'"]
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
      in "In expression: \n" ++ str

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

instance Pushable ImportDecl where
  push i@(PulledImport {qname}) =
    pushMeta i (BTPulledImport qname)

instance Pushable Function where
  push fun@(Function {funname, funtype}) =
    pushMeta fun (BTFunction funname funtype)

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
        concatMap showBT bt
        where
          showBT (pos, node) =
              case show node of
                "" -> ""
                s  -> s ++ "\n"

newtype CCError = CCError (String, Backtrace)
instance Show CCError where
    show (CCError (msg, [])) =
        " *** Error during capturechecking *** \n" ++
        msg ++ "\n"
    show (CCError (msg, bt@((pos, _):_))) =
        " *** Error during capturechecking *** \n" ++
        show pos ++ "\n" ++
        msg ++ "\n" ++
        (concat $ map showBT bt)
        where
          showBT (pos, node) =
              case (show node) of
                "" -> ""
                s  -> s ++ "\n"