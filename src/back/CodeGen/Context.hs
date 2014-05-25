{-# LANGUAGE NamedFieldPuns #-}

{-| The context that several of the Translatable typeclasses use for
compiling. For instance, it's needed for looking up the type of local
variables / fields on the class. The need for the context would go
away if we had runtime type information and/or type information from
the typechecking process. -}

module CodeGen.Context (
  Context,
  mk,
  the_prog,
  the_class,
  the_method,
  the_locals,
  type_of,
  with_class,
  with_method,
  gen_sym,
  with_local,
  other_classes) where

import AST.AST
import Identifiers
import Data.Maybe
import Control.Monad.State

import qualified CCode.Main as C

type NextSym = Int

type ProgramLoc = (Program, Maybe (ClassDecl, Maybe (MethodDecl, [ParamDecl])))

type VarSubTable = [(Name, C.CCode C.Name)] -- variable substitutions (for supporting, for instance, nested var decls)

data Context = Context VarSubTable ProgramLoc NextSym

mk :: Program -> Context
mk p = Context [] (p, Nothing) 0

with_class :: ClassDecl -> Context -> Context
with_class c (Context s (p, Nothing) n)       = Context s (p, (Just (c, Nothing))) n
with_class c (Context s (p, (Just (_, m))) n) = Context s (p, (Just (c, m))) n

with_method :: MethodDecl -> Context -> Context
with_method m (Context s (p, (Just (c, _))) n) = Context s (p, (Just (c, Just (m,[])))) n

gen_sym :: State Context String
gen_sym = do
  c <- get
  case c of
    Context s lo n ->
        do
          put $ Context s lo (n+1)
          return $ "_tmp" ++ show n
  
with_local :: ParamDecl -> Context -> Context
with_local d (Context s (p, (Just (c, Just (m, ds)))) n) =
    Context s (p, (Just (c, Just (m, d:ds)))) n
with_local d _ =
    error $ "with_local: invalid input" ++ show d

the_prog :: Context -> Program
the_prog (Context _ (prog, _) _) = prog

the_class :: Context -> Maybe ClassDecl
the_class (Context _ (_, x) _) = x >>= Just . fst

the_method :: Context -> Maybe MethodDecl
the_method (Context _ (_, x) _) = x >>= snd >>= return . fst

the_locals :: Context -> Maybe [ParamDecl]
the_locals (Context _ (_, x) _) = x >>= snd >>= return . snd

type_of :: Name -> Context -> Maybe Type
type_of n c = do
  mthd <- the_method c
  locals <- the_locals c
  return $ head $ only_tys $ filter_name_matches n $ (mparams mthd ++ locals)
    where
      filter_name_matches :: Name -> [ParamDecl] -> [ParamDecl]
      filter_name_matches n = filter (\(Param {pname}) -> (n==pname))
      
      only_tys :: [ParamDecl] -> [Type]
      only_tys = map (\(Param {ptype}) -> ptype)

other_classes :: Context -> [ClassDecl]
other_classes ctx =
  let
    (Program clss) = (the_prog ctx)
    cls = (the_class ctx)
  in
   map fromJust $ filter (/= cls) (map Just clss)

