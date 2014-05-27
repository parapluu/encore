{-# LANGUAGE NamedFieldPuns #-}
{-| 
  Utility functions for "AST.AST".
-}
module AST.Util(filterAST) where

import qualified Data.List as List
import Prelude hiding (foldr)

import AST.AST

foldr :: (Expr -> a -> a) -> a -> Expr -> a
foldr f acc e@(MethodCall {target, args}) = let argResults = List.foldr f acc args in
                                            f e (foldr f argResults target)
foldr f acc e@(FunctionCall {args}) = f e (List.foldr f acc args)
foldr f acc e@(Closure {body}) = f e (foldr f acc body)
foldr f acc e@(Let {val, body}) = let valResults = foldr f acc val in
                                  f e (foldr f valResults body)
foldr f acc e@(Seq {eseq}) = f e (List.foldr f acc eseq)
foldr f acc e@(IfThenElse {cond, thn, els}) = let condResults = foldr f acc cond
                                                  thnResults = foldr f condResults thn in
                                              f e (foldr f thnResults els)
foldr f acc e@(While {cond, body}) = let condResults = foldr f acc cond in
                                     f e (foldr f condResults body)
foldr f acc e@(Get {val}) = f e (foldr f acc val)
foldr f acc e@(FieldAccess {target, name}) = f e (foldr f acc target)
foldr f acc e@(Assign {rhs}) = f e (foldr f acc rhs)
foldr f acc e@(Print {val}) = f e (foldr f acc val)
foldr f acc e@(Binop {loper, roper}) = let lResults = foldr f acc loper in
                                       f e (foldr f lResults roper)
foldr f acc e = f e acc

-- | @filterAST cond e@ returns a list of all sub expressions @e'@ of @e@ for which @cond e'@ returns @True@
filterAST :: (Expr -> Bool) -> Expr -> [Expr]
filterAST cond = foldr (\e acc -> if cond e then e:acc else acc) []