{-# LANGUAGE NamedFieldPuns #-}
{-| 
  Utility functions for "AST.AST".
-}
module AST.Util(foldr, foldrAll, filter, extend, extendAccum, extendAccumProgram, extractTypes, freeVariables) where

import qualified Data.List as List
import Prelude hiding (foldr, filter)

import AST.AST
import Types
import Identifiers

foldr :: (Expr -> a -> a) -> a -> Expr -> a
foldr f acc e@(MethodCall {target, args}) = 
    let argResults = List.foldr (\e acc -> foldr f acc e) acc args in
    f e (foldr f argResults target)
foldr f acc e@(MessageSend {target, args}) = 
    let argResults = List.foldr (\e acc -> foldr f acc e) acc args in
    f e (foldr f argResults target)
foldr f acc e@(Let {decls, body}) = 
    let declsResults = List.foldr (\(name, val) acc' -> foldr f acc' val) acc decls in
    f e (foldr f declsResults body)
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
foldr f acc e@(FunctionCall {args}) = f e (List.foldr (\e acc -> foldr f acc e) acc args)
foldr f acc e@(Closure {body}) = f e (foldr f acc body)
foldr f acc e@(Seq {eseq}) = f e (List.foldr (\e acc -> foldr f acc e) acc eseq)
foldr f acc e@(Get {val}) = f e (foldr f acc val)
foldr f acc e@(FieldAccess {target, name}) = f e (foldr f acc target)
foldr f acc e@(Assign {rhs}) = f e (foldr f acc rhs)
foldr f acc e@(Print {args}) = f e (List.foldr (\e acc -> foldr f acc e) acc args) --f e (foldr f acc val)
foldr f acc e = f e acc

foldrAll :: (Expr -> a -> a) -> a -> Program -> [[a]]
foldrAll f e (Program _ classes) = map (foldClass f e) classes
    where
      foldClass f e (Class {methods}) = map (foldMethod f e) methods
      foldMethod f e (Method {mbody}) = foldr f e mbody

extend :: (Expr -> Expr) -> Expr -> Expr
extend f e@(MethodCall {target, args}) = 
    let e'@(MethodCall {target = t, args = a}) = f e
        t' = f t
        a' = map (\x -> extend f x) a
    in
      e'{target = t', args = a'}
extend f e@(MessageSend {target, args}) = 
    let e'@(MessageSend {target = t, args = a}) = f e
        t' = f t
        a' = map (\x -> extend f x) a
    in
      e'{target = t', args = a'}
extend f e@(Let {decls, body}) = 
    let e'@(Let {decls = d, body = b}) = f e
        d' = map (\(name, val) -> (name, extend f val)) d
        b' = f b
    in
      e'{decls = d', body = b'}
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
        a' = map (\x -> extend f x) a
    in
      e'{args = a'}
extend f e@(Closure {body}) = 
    let e'@(Closure {body = b}) = f e
        b' = f b
    in
      e'{body = b'}
extend f e@(Seq {eseq}) = 
    let e'@(Seq {eseq = es}) = f e
        es' = map (\x -> extend f x) es
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
extend f e@(Print {args}) = 
    let e'@(Print {args = a}) = f e
        a' = map (\x -> extend f x) a
    in
      e'{args = a'}
extend f e = f e

extendAccum :: (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
extendAccum f acc e@(MethodCall {}) = 
    let (acc0, e'@(MethodCall {target = t, args = a})) = f acc e
        (acc1, t') = extendAccum f acc0 t
        (acc2, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc1 a
    in
      (acc2, e'{target = t', args = a'})
extendAccum f acc e@(MessageSend {}) = 
    let (acc0, e'@(MessageSend {target = t, args = a})) = f acc e
        (acc1, t') = extendAccum f acc0 t
        (acc2, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc1 a
    in
      (acc2, e'{target = t', args = a'})
extendAccum f acc e@(Let {}) = 
    let (acc0, e'@(Let {decls = d, body = b})) = f acc e
        (acc1, d') = List.mapAccumL accumDecls acc0 d
        (acc2, b') = extendAccum f acc1 b
    in
      (acc2, e'{decls = d', body = b'})
    where
      accumDecls acc (name, expr) = let (acc', expr') = extendAccum f acc expr 
                                    in (acc', (name, expr'))
extendAccum f acc e@(IfThenElse {}) = 
    let (acc0, e'@(IfThenElse {cond = c, thn = t, els = el})) = f acc e
        (acc1, c') = extendAccum f acc0 c
        (acc2, t') = extendAccum f acc1 t
        (acc3, el') = extendAccum f acc2 el
    in
      (acc3, e'{cond = c', thn = t', els = el'})
extendAccum f acc e@(While {}) = 
    let (acc0, e'@(While {cond = c, body = b})) = f acc e
        (acc1, c') = extendAccum f acc0 c
        (acc2, b') = extendAccum f acc1 b
    in
      (acc2, e'{cond = c', body = b'})
extendAccum f acc e@(Binop {}) = 
    let (acc0, e'@(Binop {loper = l, roper = r})) = f acc e
        (acc1, l') = extendAccum f acc0 l
        (acc2, r') = extendAccum f acc1 r
    in
      (acc2, e'{loper = l', roper = r'})
extendAccum f acc e@(FunctionCall {}) = 
    let (acc0, e'@(FunctionCall{args = a})) = f acc e 
        (acc1, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 a
    in
      (acc1, e'{args = a'})
extendAccum f acc e@(Closure {}) = 
    let (acc0, e'@(Closure {body = b})) = f acc e
        (acc1, b') = extendAccum f acc0 b
    in
      (acc1, e'{body = b'})
extendAccum f acc e@(Seq {}) = 
    let (acc0, e'@(Seq {eseq = es})) = f acc e
        (acc1, es') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 es
    in
      (acc1, e'{eseq = es'})
extendAccum f acc e@(Get {}) = 
    let (acc0, e'@(Get {val = v})) = f acc e
        (acc1, v') = extendAccum f acc0 v
    in
      (acc1, e'{val = v'})
extendAccum f acc e@(FieldAccess {}) = 
    let (acc0, e'@(FieldAccess {target = t})) = f acc e
        (acc1, t') = extendAccum f acc0 t
    in
      (acc1, e'{target = t'})
extendAccum f acc e@(Assign {}) = 
    let (acc0, e'@(Assign {rhs = r})) = f acc e
        (acc1, r') = extendAccum f acc0 r
    in
      (acc1, e'{rhs = r'})
extendAccum f acc e@(Print {}) = 
    let (acc0, e'@(Print {args = a})) = f acc e
        (acc1, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 a
    in
      (acc1, e'{args = a'})
extendAccum f acc e = f acc e

extendAccumProgram :: (acc -> Expr -> (acc, Expr)) -> acc -> Program -> (acc, Program)
extendAccumProgram f acc (Program etl classes) = (acc', Program etl program')
    where 
      (acc', program') = List.mapAccumL (extendAccumClass f) acc classes
      extendAccumClass f acc cls@(Class{methods}) = (acc', cls{methods = methods'})
          where
            (acc', methods') = List.mapAccumL (extendAccumMethod f) acc methods
            extendAccumMethod f acc mtd@(Method{mbody}) = (acc', mtd{mbody = mbody'})
                where
                  (acc', mbody') = extendAccum f acc mbody

-- | @filter cond e@ returns a list of all sub expressions @e'@ of @e@ for which @cond e'@ returns @True@
filter :: (Expr -> Bool) -> Expr -> [Expr]
filter cond = foldr (\e acc -> if cond e then e:acc else acc) []

extractTypes :: Program -> [Type]
extractTypes (Program _ classes) = List.nub $ concat $ concatMap extractClassTypes classes
    where
      extractClassTypes Class {cname, fields, methods} = [cname] : (map extractFieldTypes fields) ++ (concatMap extractMethodTypes methods)
      extractFieldTypes Field {ftype} = typeComponents ftype
      extractMethodTypes Method {mtype, mparams, mbody} = (typeComponents mtype) : (map extractParamTypes mparams) ++ [extractExprTypes mbody]
      extractParamTypes Param {ptype} = typeComponents ptype
      extractExprTypes e = foldr (\e acc -> (typeComponents . getType) e ++ acc) [voidType] e

freeVariables :: [Name] -> Expr -> [(Name, Type)]
freeVariables bound expr = List.nub $ freeVariables' bound expr
    where
      freeVariables' bound TypedExpr {body} = 
          freeVariables' bound body
      freeVariables' bound MethodCall {target, args} = 
          concatMap (freeVariables' bound) args ++ freeVariables' bound target
      freeVariables' bound MessageSend {target, args} = 
          concatMap (freeVariables' bound) args ++ freeVariables' bound target
      freeVariables' bound fCall@(FunctionCall {name, args})
          | name `elem` bound = concatMap (freeVariables' bound) args
          | otherwise = concatMap (freeVariables' bound) args ++ [(name, arrType)]
          where
            arrType = arrowType (map getType args) (getType fCall)
      freeVariables' bound Closure {eparams, body} =
          freeVariables' bound' body
          where
            bound' = bound ++ map pname eparams
      freeVariables' bound Let {decls, body} =
          freeVars ++ freeVariables' bound' body
          where
            (freeVars, bound') = List.foldr fvDecls ([], bound) decls
            fvDecls (x, expr) (free, bound) = (freeVariables' (x:bound) expr ++ free, x:bound)
      freeVariables' bound Seq {eseq} = 
          concatMap (freeVariables' bound) eseq
      freeVariables' bound IfThenElse {cond, thn, els} =
          freeVariables' bound cond ++ 
          freeVariables' bound thn ++ freeVariables' bound els
      freeVariables' bound While {cond, body} =
          freeVariables' bound cond ++ freeVariables' bound body
      freeVariables' bound Get {val} = 
          freeVariables' bound val
      freeVariables' bound FieldAccess {target} = 
          freeVariables' bound target
      freeVariables' bound Assign {lhs, rhs} = 
          freeLVal bound lhs ++ freeVariables' bound rhs
          where
            freeLVal bound lval@(LVal {lname})
                | lname `elem` bound = []
                | otherwise = [(lname, getType lval)]
            freeLVal bound LField {ltarget} = freeVariables' bound ltarget
      freeVariables' bound var@(VarAccess {name})
          | name `elem` bound = []
          | otherwise = [(name, getType var)]
      freeVariables' bound Print {args} = 
          concatMap (freeVariables' bound) args
      freeVariables' bound Binop {loper, roper} = 
          freeVariables' bound loper ++ freeVariables' bound roper
      freeVariables' bound _ = []