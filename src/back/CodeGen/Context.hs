{-| The context that several of the Translatable typeclasses use
for compiling. It is used to generate new symbols for temporary
variables, store the mappings from encore variables to c variables
and to keep track of which class we're translating at the
moment. -}

module CodeGen.Context (
  Context,
  class_table,
  empty,
  new,
  subst_add,
  subst_lkp,
  subst_rem,
  gen_named_sym,
  gen_sym,
  lookup_field,
  lookup_method
) where

import Identifiers
import Types
import AST.AST
import Control.Monad.State
import qualified CodeGen.ClassTable as Tbl

import qualified CCode.Main as C

type NextSym = Int

type VarSubTable = [(Name, C.CCode C.Lval)] -- variable substitutions (for supporting, for instance, nested var decls)

data Context = Context VarSubTable NextSym Tbl.ClassTable

class_table :: Context -> Tbl.ClassTable
class_table (Context _ _ ctable) = ctable

empty :: Tbl.ClassTable -> Context
empty ctable = new [] ctable

new :: VarSubTable -> Tbl.ClassTable -> Context
new subs ctable = Context subs 0 ctable

gen_named_sym :: String -> State Context String
gen_named_sym name = do
  c <- get
  case c of
    Context s n t ->
        do put $ Context s (n+1) t
           return $ "_" ++ name ++ "_" ++ show n

gen_sym :: State Context String
gen_sym = gen_named_sym "tmp"

subst_add :: Context -> Name -> C.CCode C.Lval -> Context
subst_add c@(Context s nxt ctable) na lv = Context ((na,lv):s) nxt ctable

subst_rem :: Context -> Name -> Context
subst_rem (Context [] nxt ctable) na = Context [] nxt ctable
subst_rem (Context ((na, lv):s) nxt ctable) na'
     | na == na'  = Context s nxt ctable
     | na /= na'  = subst_add (subst_rem (Context s nxt ctable) na') na lv

subst_lkp :: Context -> Name -> Maybe (C.CCode C.Lval)
subst_lkp (Context s _ _) n = lookup n s

lookup_field :: Type -> Name -> Context -> FieldDecl
lookup_field ty f = Tbl.lookup_field ty f . class_table

lookup_method :: Type -> Name -> Context -> MethodDecl
lookup_method ty m = Tbl.lookup_method ty m . class_table