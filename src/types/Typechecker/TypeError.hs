{-|

The machinery used by "Typechecker.Typechecker" and
"Typechecker.Capturechecker" for handling errors and backtracing.

-}

module Typechecker.TypeError (Backtrace
                             ,emptyBT
                             ,Pushable(push)
                             ,TCError(TCError)
                             ,TCWarning(TCWarning)
                             ,Warning(..)
                             ,CCError(CCError)
                             ,currentMethodFromBacktrace
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
                   | BTRequirement Requirement
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
  show (BTMethod m) =
      let name = hname $ mheader m
          ty   = htype $ mheader m
          method | isStreamMethod m = "stream method"
                 | otherwise = "method"
      in
        concat ["In ", method, " '", show name, "' of type '", show ty, "'"]
  show (BTRequirement req)
      | isRequiredField req =
          concat ["In required field '", show . ppFieldDecl . rfield $ req, "'"]
      | isRequiredMethod req =
          concat ["In required method '"
                 ,show . ppFunctionHeader . rheader $ req
                 , "'"]
  show (BTExpr expr)
    | (isNothing . getSugared) expr = ""
    | otherwise =
      let str = show $ nest 2 $ ppSugared expr
      in "In expression: \n" ++ str

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

currentMethodFromBacktrace :: Backtrace -> Maybe MethodDecl
currentMethodFromBacktrace [] = Nothing
currentMethodFromBacktrace ((_, BTExpr Closure{}):_) = Nothing
currentMethodFromBacktrace ((_, BTExpr Async{}):_) = Nothing
currentMethodFromBacktrace ((_, BTMethod m):_) = Just m
currentMethodFromBacktrace (_:bt) = currentMethodFromBacktrace bt

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

instance Pushable Requirement where
    push m = pushMeta m (BTRequirement m)

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


data TCWarning = TCWarning Backtrace Warning
instance Show TCWarning where
    show (TCWarning [] w) =
        "Warning:\n" ++
        show w
    show (TCWarning ((pos, _):_) w) =
        "Warning at " ++ show pos ++ ":\n" ++
        show w

data Warning = StringDeprecatedWarning
             | StringIdentityWarning
instance Show Warning where
    show StringDeprecatedWarning =
        "Type 'string' is deprecated. Use 'String' instead."
    show StringIdentityWarning =
        "Comparing String identity. Equality should be compared using 'equals'"

newtype CCError = CCError (String, Backtrace)
instance Show CCError where
    show (CCError (msg, [])) =
        " *** Error during capturechecking *** \n" ++
        msg ++ "\n"
    show (CCError (msg, bt@((pos, _):_))) =
        " *** Error during capturechecking *** \n" ++
        show pos ++ "\n" ++
        msg ++ "\n" ++
        concatMap showBT bt
        where
          showBT (pos, node) =
              case show node of
                "" -> ""
                s  -> s ++ "\n"
