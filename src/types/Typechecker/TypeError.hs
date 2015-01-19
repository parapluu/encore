{-# LANGUAGE NamedFieldPuns #-}

{-|

The machinery used by "Typechecker.Typechecker" for handling errors and backtracing. 

-}

module Typechecker.TypeError (Backtrace, emptyBT, Pushable(push), TCError(TCError), currentMethod) where

import Text.PrettyPrint
import Text.Parsec(SourcePos)
-- import Control.Monad.Error
import Control.Monad.Except
import Data.Maybe

import Identifiers
import Types
import AST.AST
import AST.PrettyPrinter

data BacktraceNode = BTFunction Name Type | BTClass Type | BTParam ParamDecl | BTField FieldDecl | BTMethod MethodDecl | BTExpr Expr
instance Show BacktraceNode where
    show (BTFunction n ty) = "In function '"       ++ show n                 ++ "' of type '" ++ show ty ++ "'"
    show (BTClass ty)      = "In class '"          ++ show ty                ++ "'"
    show (BTParam p)       = "In parameter '"      ++ (show $ ppParamDecl p) ++ "'"
    show (BTField f)       = "In field '"          ++ (show $ ppFieldDecl f) ++ "'"
    show (BTMethod Method{mname, mtype}) = "In method '" ++ show mname ++ "' of type '" ++ show mtype ++ "'"
    show (BTMethod StreamMethod{mname, mtype}) = "In stream method '" ++ show mname ++ "' of type '" ++ show mtype ++ "'"
    show (BTExpr expr)     
        | (isNothing . getSugared) expr = ""
        | otherwise = "In expression: \n"   ++ (show $ nest 2 $ ppSugared expr)

type Backtrace = [(SourcePos, BacktraceNode)]
emptyBT :: Backtrace
emptyBT = []

currentMethod :: Backtrace -> MethodDecl
currentMethod [] = error "*** Internal error ***\nTried to get current method when not in a method"
currentMethod ((_, BTMethod m):_) = m
currentMethod (_:bt) = currentMethod bt

-- | A type class for unifying the syntactic elements that can be pushed to the backtrace stack.
class Pushable a where
    push :: a -> Backtrace -> Backtrace

instance Pushable Function where
    push fun@(Function {funname, funtype}) bt = (getPos fun, BTFunction funname funtype) : bt

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