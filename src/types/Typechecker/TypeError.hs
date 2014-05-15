{-|

The machinery used by "Typechecker.Typechecker" for handling errors and backtracing. 

-}

module Typechecker.TypeError (Backtrace, emptyBT, Pushable(push), TCError(TCError)) where

import Text.PrettyPrint
import Control.Monad.Error

import Identifiers
import Typechecker.Types
import AST.AST
import AST.PrettyPrinter

data BacktraceNode = BTClass Type | BTParam ParamDecl | BTField FieldDecl | BTMethod Name Type | BTExpr Expr | BTLVal LVal
instance Show BacktraceNode where
    show (BTClass (Type t))            = "In class '"          ++ t                      ++ "'"
    show (BTParam p)                   = "In parameter '"      ++ (show $ ppParamDecl p) ++ "'"
    show (BTField f)                   = "In field '"          ++ (show $ ppFieldDecl f) ++ "'"
    show (BTMethod (Name n) (Type ty)) = "In method '"         ++ n                      ++ "' of type '" ++ ty ++ "'"
    show (BTExpr expr)                 = "In expression: \n"   ++ (show $ nest 2 $ ppExpr expr)
    show (BTLVal lval)                 = "In left hand side '" ++ (show $ ppLVal lval) ++ "'"

type Backtrace = [BacktraceNode]
emptyBT :: Backtrace
emptyBT = []

-- | A type class for unifying the syntactic elements that can be pushed to the backtrace stack.
class Pushable a where
    push :: a -> Backtrace -> Backtrace

instance Pushable ClassDecl where
    push (Class cname _ _) bt = (BTClass cname) : bt

instance Pushable FieldDecl where
    push f bt = (BTField f) : bt

instance Pushable ParamDecl where
    push p bt = (BTParam p) : bt

instance Pushable MethodDecl where
    push (Method name ty _ _) bt = (BTMethod name ty) : bt

instance Pushable Expr where
    push expr bt = (BTExpr expr) : bt

instance Pushable LVal where
    push lval bt = (BTLVal lval) : bt

-- | The data type for a type checking error. Showing it will
-- produce an error message and print the backtrace.
newtype TCError = TCError (String, Backtrace)
instance Error TCError
instance Show TCError where
    show (TCError (msg, bt)) = 
        "*** Error during typechecking ***\n" ++
        msg ++ "\n" ++
        (concat $ map ((++"\n") . show) bt)