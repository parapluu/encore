{-# LANGUAGE NamedFieldPuns #-}
{-| 
  Utility functions for "AST.AST".
-}
module AST.Util(foldr, foldrAll, filter, extend, extendAccum, extendAccumProgram) where

import qualified Data.List as List
import Prelude hiding (foldr, filter)

import AST.AST

foldr :: (Expr -> a -> a) -> a -> Expr -> a
foldr f acc e@(MethodCall {target, args}) = 
    let argResults = List.foldr f acc args in
    f e (foldr f argResults target)
foldr f acc e@(Let {val, body}) = 
    let valResults = foldr f acc val in
    f e (foldr f valResults body)
foldr f acc e@(IfThenElse {cond, thn, els}) = 
    let condResults = foldr f acc cond
        thnResults = foldr f condResults thn in
    f e (foldr f thnResults els)
foldr f acc e@(While {cond, body}) = 
    let condResults = foldr f acc cond in
    f e (foldr f condResults body)
foldr f acc e@(Binop {loper, roper}) = 
    let lResults = foldr f acc loper in
    f e (foldr f lResults roper)
foldr f acc e@(FunctionCall {args}) = f e (List.foldr f acc args)
foldr f acc e@(Closure {body}) = f e (foldr f acc body)
foldr f acc e@(Seq {eseq}) = f e (List.foldr f acc eseq)
foldr f acc e@(Get {val}) = f e (foldr f acc val)
foldr f acc e@(FieldAccess {target, name}) = f e (foldr f acc target)
foldr f acc e@(Assign {rhs}) = f e (foldr f acc rhs)
foldr f acc e@(Print {val}) = f e (foldr f acc val)
foldr f acc e = f e acc

foldrAll :: (Expr -> a -> a) -> a -> Program -> [[a]]
foldrAll f e (Program classes) = map (foldClass f e) classes
    where
      foldClass f e (Class {methods}) = map (foldMethod f e) methods
      foldMethod f e (Method {mbody}) = foldr f e mbody

extend :: (Expr -> Expr) -> Expr -> Expr
extend f e@(MethodCall {target, args}) = 
    let e'@(MethodCall {target = t, args = a}) = f e
        t' = f t
        a' = map f a
    in
      e'{target = t', args = a'}
extend f e@(Let {val, body}) = 
    let e'@(Let {val = v, body = b}) = f e
        v' = f v
        b' = f b
    in
      e'{val = v', body = b'}
extend f e@(IfThenElse {cond, thn, els}) = 
    let e'@(IfThenElse {cond = c, thn = t, els = el}) = f e
        c' = f c
        t' = f t
        el' = f el
    in
      e'{cond = c', thn = t', els = el'}
extend f e@(While {cond, body}) = 
    let e'@(While {cond = c, body = b}) = f e
        c' = f c
        b' = f b
    in
      e'{cond = c', body = b'}
extend f e@(Binop {loper, roper}) = 
    let e'@(Binop {loper = l, roper = r}) = f e
        l' = f l
        r' = f r
    in
      e'{loper = l', roper = r'}
extend f e@(FunctionCall {args}) = 
    let e'@(FunctionCall {args = a}) = f e
        a' = map f a
    in
      e'{args = a'}
extend f e@(Closure {body}) = 
    let e'@(Closure {body = b}) = f e
        b' = f b
    in
      e'{body = b'}
extend f e@(Seq {eseq}) = 
    let e'@(Seq {eseq = es}) = f e
        es' = map f es
    in
      e'{eseq = es'}
extend f e@(Get {val}) = 
    let e'@(Get {val = v}) = f e
        v' = f v
    in
      e'{val = v'}
extend f e@(FieldAccess {target}) = 
    let e'@(FieldAccess {target = t}) = f e
        t' = f t
    in
      e'{target = t'}
extend f e@(Assign {rhs}) = 
    let e'@(Assign {rhs = r}) = f e
        r' = f r
    in
      e'{rhs = r'}
extend f e@(Print {val}) = 
    let e'@(Print {val = v}) = f e
        v' = f v
    in
      e'{val = v'}
extend f e = f e

extendAccum :: (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
extendAccum f acc e@(MethodCall {target, args}) = 
    let (acc0, e'@(MethodCall {target = t, args = a})) = f acc e
        (acc1, t') = f acc0 t
        (acc2, a') = List.mapAccumL f acc1 a
    in
      (acc2, e'{target = t', args = a'})
extendAccum f acc e@(Let {val, body}) = 
    let (acc0, e'@(Let {val = v, body = b})) = f acc e
        (acc1, v') = f acc0 v
        (acc2, b') = f acc1 b
    in
      (acc2, e'{val = v', body = b'})
extendAccum f acc e@(IfThenElse {cond, thn, els}) = 
    let (acc0, e'@(IfThenElse {cond = c, thn = t, els = el})) = f acc e
        (acc1, c') = f acc0 c
        (acc2, t') = f acc1 t
        (acc3, el') = f acc2 el
    in
      (acc3, e'{cond = c', thn = t', els = el'})
extendAccum f acc e@(While {cond, body}) = 
    let (acc0, e'@(While {cond = c, body = b})) = f acc e
        (acc1, c') = f acc0 c
        (acc2, b') = f acc1 b
    in
      (acc2, e'{cond = c', body = b'})
extendAccum f acc e@(Binop {loper, roper}) = 
    let (acc0, e'@(Binop {loper = l, roper = r})) = f acc e
        (acc1, l') = f acc0 l
        (acc2, r') = f acc1 r
    in
      (acc2, e'{loper = l', roper = r'})
extendAccum f acc e@(FunctionCall {args}) = 
    let (acc0, e'@(FunctionCall{args = a})) = f acc e 
        (acc1, a') = List.mapAccumL f acc0 a
    in
      (acc1, e'{args = a'})
extendAccum f acc e@(Closure {body}) = 
    let (acc0, e'@(Closure {body = b})) = f acc e
        (acc1, b') = f acc0 b
    in
      (acc1, e'{body = b'})
extendAccum f acc e@(Seq {eseq}) = 
    let (acc0, e'@(Seq {eseq = es})) = f acc e
        (acc1, es') = List.mapAccumL f acc0 es
    in
      (acc1, e'{eseq = es'})
extendAccum f acc e@(Get {val}) = 
    let (acc0, e'@(Get {val = v})) = f acc e
        (acc1, v') = f acc0 v
    in
      (acc1, e'{val = v'})
extendAccum f acc e@(FieldAccess {target}) = 
    let (acc0, e'@(FieldAccess {target = t})) = f acc e
        (acc1, t') = f acc0 t
    in
      (acc1, e'{target = t'})
extendAccum f acc e@(Assign {rhs}) = 
    let (acc0, e'@(Assign {rhs = r})) = f acc e
        (acc1, r') = f acc0 r
    in
      (acc1, e'{rhs = r'})
extendAccum f acc e@(Print {val}) = 
    let (acc0, e'@(Print {val = v})) = f acc e
        (acc1, v') = f acc0 v
    in
      (acc1, e'{val = v'})
extendAccum f acc e = f acc e

extendAccumProgram :: (acc -> Expr -> (acc, Expr)) -> acc -> Program -> (acc, Program)
extendAccumProgram f acc (Program classes) = (acc0, Program program')
    where 
      (acc0, program') = List.mapAccumL (extendAccumClass f) acc classes
      extendAccumClass f acc cls@(Class{methods}) = (acc1, cls{methods = methods'})
          where
            (acc1, methods') = List.mapAccumL (extendAccumMethod f) acc methods
            extendAccumMethod f acc mtd@(Method{mbody}) = (acc2, mtd{mbody = mbody'})
                where
                  (acc2, mbody') = extendAccum f acc mbody

-- | @filter cond e@ returns a list of all sub expressions @e'@ of @e@ for which @cond e'@ returns @True@
filter :: (Expr -> Bool) -> Expr -> [Expr]
filter cond = foldr (\e acc -> if cond e then e:acc else acc) []