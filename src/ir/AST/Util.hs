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

-- | @getChildren e@ returns all children of @e@ that are Exprs themselves
getChildren :: Expr -> [Expr]
getChildren TypedExpr {body} = [body]
getChildren MethodCall {target, args} = target : args
getChildren MessageSend {target, args} = target : args
getChildren FunctionCall {args} = args
getChildren Closure {body} = [body]
getChildren Let {body, decls} = body : map snd decls
getChildren Seq {eseq} = eseq
getChildren IfThenElse {cond, thn, els} = [cond, thn, els]
getChildren IfThen {cond, thn} = [cond, thn]
getChildren Unless {cond, thn} = [cond, thn]
getChildren While {cond, body} = [cond, body]
getChildren Repeat {name, times, body} = [times, body]
getChildren Get {val} = [val]
getChildren FieldAccess {target} = [target]
getChildren Assign {lhs, rhs} = [lhs, rhs]
getChildren NewWithInit {args} = args
getChildren Print {args} = args
getChildren Exit {args} = args
getChildren Unary {operand} = [operand]
getChildren Binop {loper, roper} = [loper, roper]
getChildren e = []

foldr :: (Expr -> a -> a) -> a -> Expr -> a
foldr f acc e = 
    let childResult = List.foldr (\e acc -> foldr f acc e) acc (getChildren e)
    in f e childResult

foldrAll :: (Expr -> a -> a) -> a -> Program -> [[a]]
foldrAll f e (Program _ _ funs classes) = [map (foldFunction f e) funs] ++ (map (foldClass f e) classes)
    where
      foldFunction f e (Function {funbody}) = foldr f e funbody
      foldClass f e (Class {methods}) = map (foldMethod f e) methods
      foldMethod f e (Method {mbody}) = foldr f e mbody

extend :: (Expr -> Expr) -> Expr -> Expr
extend f = snd . (extendAccum (\acc e -> (undefined, f e)) undefined)

extendAccum :: (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
extendAccum f acc0 e@(MethodCall {target, args}) = 
    let (acc1, t') = extendAccum f acc0 target
        (acc2, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc1 args
    in
      f acc2 e{target = t', args = a'}
extendAccum f acc0 e@(MessageSend {target, args}) = 
    let (acc1, t') = extendAccum f acc0 target
        (acc2, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc1 args
    in
      f acc2 e{target = t', args = a'}
extendAccum f acc0 e@(NewWithInit {args}) = 
    let (acc1, a') = List.mapAccumL (\acc e -> extendAccum f acc e) acc0 args
    in
      f acc1 e{args = a'}
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
extendAccum f acc0 e@(IfThen {cond, thn}) = 
    let (acc1, c') = extendAccum f acc0 cond
        (acc2, t') = extendAccum f acc1 thn
    in
      f acc2 e{cond = c', thn = t'}
extendAccum f acc0 e@(Unless {cond, thn}) = 
    let (acc1, c') = extendAccum f acc0 cond
        (acc2, t') = extendAccum f acc1 thn
    in
      f acc2 e{cond = c', thn = t'}
extendAccum f acc0 e@(While {cond, body}) = 
    let (acc1, c') = extendAccum f acc0 cond
        (acc2, b') = extendAccum f acc1 body
    in
      f acc2 e{cond = c', body = b'}
extendAccum f acc0 e@(Repeat {times, body}) = 
    let (acc1, t') = extendAccum f acc0 times
        (acc2, b') = extendAccum f acc1 body
    in
      f acc2 e{times = t', body = b'}
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
extendAccumProgram f acc0 (Program etl imps funs classes) = (acc2, Program etl imps funs' classes')
    where 
      (acc1, funs') = List.mapAccumL (extendAccumFunction f) acc0 funs
      extendAccumFunction f acc fun@(Function{funbody}) = (acc', fun{funbody = funbody'})
          where
            (acc', funbody') = extendAccum f acc funbody
      (acc2, classes') = List.mapAccumL (extendAccumClass f) acc1 classes
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
extractTypes (Program _ _ funs classes) = 
    List.nub $ concat $ concatMap extractFunctionTypes funs ++ concatMap extractClassTypes classes
    where
      extractFunctionTypes Function {funtype, funparams, funbody} = (typeComponents funtype) : (map extractParamTypes funparams) ++ [extractExprTypes funbody]
      extractClassTypes Class {cname, fields, methods} = [cname] : (map extractFieldTypes fields) ++ (concatMap extractMethodTypes methods)
      extractFieldTypes Field {ftype} = typeComponents ftype
      extractMethodTypes Method {mtype, mparams, mbody} = (typeComponents mtype) : (map extractParamTypes mparams) ++ [extractExprTypes mbody]
      extractParamTypes Param {ptype} = typeComponents ptype
      extractExprTypes e = foldr (\e acc -> (typeComponents . getType) e ++ acc) [] e

freeVariables :: [Name] -> Expr -> [(Name, Type)]
freeVariables bound expr = List.nub $ freeVariables' bound expr
    where
      freeVariables' bound var@(VarAccess {name})
          | name `elem` bound = []
          | otherwise = [(name, getType var)]
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
      freeVariables' bound e = concatMap (freeVariables' bound) (getChildren e)
