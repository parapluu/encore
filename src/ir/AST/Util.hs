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
getChildren Yield {val} = [val]
getChildren IsEos {target} = [target]
getChildren StreamNext {target} = [target]
getChildren FutureChain {future, chain} = [future, chain]
getChildren FieldAccess {target} = [target]
getChildren Assign {lhs, rhs} = [lhs, rhs]
getChildren NewWithInit {args} = args
getChildren Print {args} = args
getChildren Exit {args} = args
getChildren Unary {operand} = [operand]
getChildren Binop {loper, roper} = [loper, roper]
getChildren e = []

-- | @putChildren children e@ returns @e@ with it's children
-- replaced by the Exprs in @children@. The expected invariant is
-- that @putChildren (getChildren e) e == e@ and @getChildren (putChildren l e) == l@
putChildren :: [Expr] -> Expr -> Expr
putChildren [body] e@(TypedExpr {}) = e{body = body}
putChildren (target : args) e@(MethodCall {}) = e{target = target, args = args}
putChildren (target : args) e@(MessageSend {}) = e{target = target, args = args}
putChildren args e@(FunctionCall {}) = e{args = args}
putChildren [body] e@(Closure {}) = e{body = body}
putChildren (body : es) e@(Let{decls}) = e{body = body, decls = zipWith (\(name, _) e -> (name, e)) decls es}
putChildren eseq e@(Seq {}) = e{eseq = eseq}
putChildren [cond, thn, els] e@(IfThenElse {}) = e{cond = cond, thn = thn, els = els}
putChildren [cond, thn] e@(IfThen {}) = e{cond = cond, thn = thn}
putChildren [cond, thn] e@(Unless {}) = e{cond = cond, thn = thn}
putChildren [cond, body] e@(While {}) = e{cond = cond, body = body}
putChildren [times, body] e@(Repeat {}) = e{times = times, body = body}
putChildren [val] e@(Get {}) = e{val = val}
putChildren [val] e@(Yield {}) = e{val = val}
putChildren [target] e@(IsEos {}) = e{target = target}
putChildren [target] e@(StreamNext {}) = e{target = target}
putChildren [future, chain] e@(FutureChain {}) = e{future = future, chain = chain}
putChildren [target] e@(FieldAccess {}) = e{target = target}
putChildren [lhs, rhs] e@(Assign {}) = e{lhs = lhs, rhs = rhs}
putChildren args e@(NewWithInit {}) = e{args = args}
putChildren args e@(Print {}) = e{args = args}
putChildren args e@(Exit {}) = e{args = args}
putChildren [operand] e@(Unary {}) = e{operand = operand}
putChildren [loper, roper] e@(Binop {}) = e{loper = loper, roper = roper}
putChildren _ e = e

--------------- The functions below this line depend only on the two above --------------------

foldr :: (Expr -> a -> a) -> a -> Expr -> a
foldr f acc e = 
    let childResult = List.foldr (\e acc -> foldr f acc e) acc (getChildren e)
    in f e childResult

foldrAll :: (Expr -> a -> a) -> a -> Program -> [[a]]
foldrAll f e (Program _ _ _ funs classes) = [map (foldFunction f e) funs] ++ (map (foldClass f e) classes)
    where
      foldFunction f e (Function {funbody}) = foldr f e funbody
      foldClass f e (Class {methods}) = map (foldMethod f e) methods
      foldMethod f e m = foldr f e (mbody m)

extend :: (Expr -> Expr) -> Expr -> Expr
extend f = snd . (extendAccum (\acc e -> (undefined, f e)) undefined)

extendAccum :: (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
extendAccum f acc0 e =
    let (acc1, childResults) = 
            List.mapAccumL (\acc e -> extendAccum f acc e) acc0 (getChildren e)
    in 
      f acc1 (putChildren childResults e)

extendAccumProgram :: (acc -> Expr -> (acc, Expr)) -> acc -> Program -> (acc, Program)
extendAccumProgram f acc0 (Program bundle etl imps funs classes) = (acc2, Program bundle etl imps funs' classes')
    where 
      (acc1, funs') = List.mapAccumL (extendAccumFunction f) acc0 funs
      extendAccumFunction f acc fun@(Function{funbody}) = (acc', fun{funbody = funbody'})
          where
            (acc', funbody') = extendAccum f acc funbody
      (acc2, classes') = List.mapAccumL (extendAccumClass f) acc1 classes
      extendAccumClass f acc cls@(Class{methods}) = (acc', cls{methods = methods'})
          where
            (acc', methods') = List.mapAccumL (extendAccumMethod f) acc methods
            extendAccumMethod f acc mtd = (acc', mtd{mbody = mbody'})
                where
                  (acc', mbody') = extendAccum f acc (mbody mtd)

-- | @filter cond e@ returns a list of all sub expressions @e'@ of @e@ for which @cond e'@ returns @True@
filter :: (Expr -> Bool) -> Expr -> [Expr]
filter cond = foldr (\e acc -> if cond e then e:acc else acc) []

extractTypes :: Program -> [Type]
extractTypes (Program _ _ _ funs classes) = 
    List.nub $ concat $ concatMap extractFunctionTypes funs ++ concatMap extractClassTypes classes
    where
      extractFunctionTypes Function {funtype, funparams, funbody} = (typeComponents funtype) : (map extractParamTypes funparams) ++ [extractExprTypes funbody]
      extractClassTypes Class {cname, fields, methods} = [cname] : (map extractFieldTypes fields) ++ (concatMap extractMethodTypes methods)
      extractFieldTypes Field {ftype} = typeComponents ftype
      extractMethodTypes m = (typeComponents (mtype m)) : (map extractParamTypes (mparams m)) ++ [extractExprTypes (mbody m)]
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
