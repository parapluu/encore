{-# LANGUAGE NamedFieldPuns #-}

{-|

The machinery used by "Typechecker.Typechecker" for handling errors and backtracing. 

-}

module Typechecker.TypeError (Backtrace, emptyBT, Pushable(push), TCError(TCError)) where

import Text.PrettyPrint
import Text.Parsec(SourcePos)
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

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

-- | A type class for unifying the syntactic elements that can be pushed to the backtrace stack.
class Pushable a where
    push :: a -> Backtrace -> Backtrace

instance Pushable ClassDecl where
    push c bt = (getPos c, BTClass (cname c)) : bt

instance Pushable FieldDecl where
    push f bt = (getPos f, BTField f) : bt

instance Pushable ParamDecl where
    push p bt = (getPos p, BTParam p) : bt

instance Pushable MethodDecl where
    push m@(Method {mname, mtype}) bt = (getPos m, BTMethod mname mtype) : bt

instance Pushable Expr where
    push expr bt = (getPos expr, BTExpr expr) : bt

instance Pushable LVal where
    push lval bt = (getPos lval, BTLVal lval) : bt

-- | The data type for a type checking error. Showing it will
-- produce an error message and print the backtrace.
newtype TCError = TCError (String, Backtrace)
instance Error TCError
instance Show TCError where
    show (TCError (msg, [])) = 
        "*** Error during typechecking ***\n" ++
        msg ++ "\n"
    show (TCError (msg, bt@((pos, _):_))) = 
        "*** Error during typechecking ***\n" ++
        show pos ++ "\n" ++
        msg ++ "\n" ++
        (concat $ map showBT bt)
        where
          showBT (pos, node) = (show node) ++ "\n"