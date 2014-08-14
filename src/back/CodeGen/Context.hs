{-# LANGUAGE NamedFieldPuns #-}

{-| The context that several of the Translatable typeclasses use
for compiling. It is used to generate new symbols for temporary
variables, store the mappings from encore variables to c variables
and to keep track of which class we're translating at the
moment. -}

module CodeGen.Context (
  Context,
  mk,
  subst_add,
  subst_lkp,
  the_class,
  gen_sym) where

import AST.AST
import Identifiers
import Types
import Data.Maybe
import Control.Monad.State

import qualified CCode.Main as C

type NextSym = Int

type CurrentClass = ClassDecl

type VarSubTable = [(Name, C.CCode C.Lval)] -- variable substitutions (for supporting, for instance, nested var decls)

data Context = Context VarSubTable CurrentClass NextSym

mk :: ClassDecl -> Context
mk cdecl = Context [] cdecl 0

gen_sym :: State Context String
gen_sym = do
  c <- get
  case c of
    Context s lo n ->
        do
          put $ Context s lo (n+1)
          return $ "_tmp" ++ show n
  
subst_add :: Context -> Name -> C.CCode C.Lval -> Context
subst_add (Context s loc nxt) na lv = Context ((na,lv):s) loc nxt

subst_lkp :: Context -> Name -> Maybe (C.CCode C.Lval)
subst_lkp (Context s _ _) n = lookup n s

the_class :: Context -> ClassDecl
the_class (Context _ c _) = c