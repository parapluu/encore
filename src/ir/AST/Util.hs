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
foldr f acc e@(Print {args}) = f e (List.foldr (\e acc -> foldr f acc e) acc args)
foldr f acc e@(Exit {args}) = f e (List.foldr (\e acc -> foldr f acc e) acc args)
foldr f acc e = f e acc

foldrAll :: (Expr -> a -> a) -> a -> Program -> [[a]]
foldrAll f e (Program _ classes) = map (foldClass f e) classes
    where
      foldClass f e (Class {methods}) = map (foldMethod f e) methods
      foldMethod f e (Method {mbody}) = foldr f e mbody

extend :: (Expr -> Expr) -> Expr -> Expr
extend f = snd . (extendAccum (\acc e -> (undefined, f e)) undefined)

extendAccum :: (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
extendAccum f acc0 e@(MethodCall {target, args}) = 
    let (acc1, t') = extendAccum f acc0 target
        (acc2, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc1 args
    in
      (acc2, e{target = t', args = a'})
extendAccum f acc0 e@(MessageSend {target, args}) = 
    let (acc1, t') = extendAccum f acc0 target
        (acc2, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc1 args
    in
      f acc2 e{target = t', args = a'}
extendAccum f acc0 e@(Let {decls, body}) = 
    let (acc1, d') = List.mapAccumL accumDecls acc0 decls
        (acc2, b') = extendAccum f acc1 body
    in
      f acc2 e{decls = d', body = b'}
    where
      accumDecls acc (name, expr) = let (acc', expr') = extendAccum f acc expr 
                                    in (acc', (name, expr'))
extendAccum f acc0 e@(IfThenElse {cond, thn, els}) = 
    let (acc1, c') = extendAccum f acc0 cond
        (acc2, t') = extendAccum f acc1 thn
        (acc3, el') = extendAccum f acc2 els
    in
      f acc3 e{cond = c', thn = t', els = el'}
extendAccum f acc0 e@(While {cond, body}) = 
    let (acc1, c') = extendAccum f acc0 cond
        (acc2, b') = extendAccum f acc1 body
    in
      f acc2 e{cond = c', body = b'}
extendAccum f acc0 e@(Binop {loper, roper}) = 
    let (acc1, l') = extendAccum f acc0 loper
        (acc2, r') = extendAccum f acc1 roper
    in
      f acc2 e{loper = l', roper = r'}
extendAccum f acc0 e@(FunctionCall {args}) = 
    let (acc1, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 args
    in
      f acc1 e{args = a'}
extendAccum f acc0 e@(Closure {body}) = 
    let (acc1, b') = extendAccum f acc0 body
    in
      f acc1 e{body = b'}
extendAccum f acc0 e@(Seq {eseq}) = 
    let (acc1, es') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 eseq
    in
      f acc1 e{eseq = es'}
extendAccum f acc0 e@(Get {val}) = 
    let (acc1, v') = extendAccum f acc0 val
    in
      f acc1 e{val = v'}
extendAccum f acc0 e@(FieldAccess {target}) = 
    let (acc1, t') = extendAccum f acc0 target
    in
      f acc1 e{target = t'}
extendAccum f acc0 e@(Assign {rhs}) = 
    let (acc1, r') = extendAccum f acc0 rhs
    in
      f acc1 e{rhs = r'}
extendAccum f acc0 e@(Print {args}) = 
    let (acc1, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 args
    in
      f acc1 e{args = a'}
extendAccum f acc0 e@(Exit {args}) = 
    let (acc1, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 args
    in
      f acc1 e{args = a'}
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
      freeVariables' bound Exit {args} = 
          concatMap (freeVariables' bound) args
      freeVariables' bound Binop {loper, roper} = 
          freeVariables' bound loper ++ freeVariables' bound roper
      freeVariables' bound _ = []