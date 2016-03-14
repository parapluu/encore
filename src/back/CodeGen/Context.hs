{-| The context that several of the Translatable typeclasses use
for compiling. It is used to generate new symbols for temporary
variables, store the mappings from encore variables to c variables
and to keep track of which class we're translating at the
moment. -}

module CodeGen.Context (
  Context,
  classTable,
  empty,
  new,
  substAdd,
  substLkp,
  substRem,
  genNamedSym,
  genSym,
  lookupField,
  lookupMethod,
  lookupCalledType,
  lookupFunction,
) where

import Identifiers
import Types
import AST.AST
import Control.Monad.State
import qualified CodeGen.ClassTable as Tbl

import qualified CCode.Main as C

type NextSym = Int

type VarSubTable = [(Name, C.CCode C.Lval)] -- variable substitutions (for supporting, for instance, nested var decls)

data Context = Context VarSubTable NextSym Tbl.NamespaceTable

classTable :: Context -> Tbl.ClassTable
classTable (Context _ _ ntable) = snd ntable

functionTable :: Context -> Tbl.FunctionTable
functionTable (Context _ _ ntable) = fst ntable

empty :: Tbl.NamespaceTable -> Context
empty ntable = new [] ntable

new :: VarSubTable -> Tbl.NamespaceTable -> Context
new subs ntable = Context subs 0 ntable

genNamedSym :: String -> State Context String
genNamedSym name = do
  c <- get
  case c of
    Context s n t ->
        do put $ Context s (n+1) t
           return $ "_" ++ name ++ "_" ++ show n

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd c@(Context s nxt ntable) na lv = Context ((na,lv):s) nxt ntable

substRem :: Context -> Name -> Context
substRem (Context [] nxt ntable) na = Context [] nxt ntable
substRem (Context ((na, lv):s) nxt ntable) na'
     | na == na'  = Context s nxt ntable
     | na /= na'  = substAdd (substRem (Context s nxt ntable) na') na lv

substLkp :: Context -> Name -> Maybe (C.CCode C.Lval)
substLkp (Context s _ _) n = lookup n s

lookupField :: Type -> Name -> Context -> FieldDecl
lookupField ty f = Tbl.lookupField ty f . classTable

lookupMethod :: Type -> Name -> Context -> FunctionHeader
lookupMethod ty m = Tbl.lookupMethod ty m . classTable

lookupCalledType :: Type -> Name -> Context -> Type
lookupCalledType ty m = Tbl.lookupCalledType ty m . classTable

lookupFunction :: Type -> Name -> Context -> FunctionHeader
lookupFunction ty f = Tbl.lookupFunction ty f . functionTable
