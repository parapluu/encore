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
  LexicalContext(..),
  lexicalContext,
  classCtx,
  functionCtx,
) where

import Identifiers
import Types
import AST.AST
import Control.Monad.State
import qualified CodeGen.ClassTable as Tbl

import qualified CCode.Main as C

data LexicalContext = ClassContext | GlobalFunctionContext

classCtx :: LexicalContext
classCtx = ClassContext

functionCtx :: LexicalContext
functionCtx = GlobalFunctionContext

type NextSym = Int

type VarSubTable = [(Name, C.CCode C.Lval)] -- variable substitutions (for supporting, for instance, nested var decls)

data Context = Context VarSubTable NextSym Tbl.NamespaceTable LexicalContext

classTable :: Context -> Tbl.ClassTable
classTable (Context _ _ ntable _) = snd ntable

functionTable :: Context -> Tbl.FunctionTable
functionTable (Context _ _ ntable _) = fst ntable

lexicalContext :: Context -> LexicalContext
lexicalContext (Context _ _ _ ctx)= ctx

empty :: Tbl.NamespaceTable -> Context
empty ntable = new [] ntable ClassContext

new :: VarSubTable -> Tbl.NamespaceTable -> LexicalContext -> Context
new subs ntable ctx = Context subs 0 ntable ctx

genNamedSym :: String -> State Context String
genNamedSym name = do
  c <- get
  case c of
    Context s n t ctx ->
        do put $ Context s (n+1) t ctx
           return $ "_" ++ name ++ "_" ++ show n

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd c@(Context s nxt ntable ctx) na lv = Context ((na,lv):s) nxt ntable ctx

substRem :: Context -> Name -> Context
substRem (Context [] nxt ntable ctx) na = Context [] nxt ntable ctx
substRem (Context ((na, lv):s) nxt ntable ctx) na'
     | na == na'  = Context s nxt ntable ctx
     | na /= na'  = substAdd (substRem (Context s nxt ntable ctx) na') na lv

substLkp :: Context -> Name -> Maybe (C.CCode C.Lval)
substLkp (Context s _ _ _) n = lookup n s

lookupField :: Type -> Name -> Context -> FieldDecl
lookupField ty f = Tbl.lookupField ty f . classTable

lookupMethod :: Type -> Name -> Context -> FunctionHeader
lookupMethod ty m = Tbl.lookupMethod ty m . classTable

lookupCalledType :: Type -> Name -> Context -> Type
lookupCalledType ty m = Tbl.lookupCalledType ty m . classTable

lookupFunction :: Type -> Name -> Context -> FunctionHeader
lookupFunction ty f = Tbl.lookupFunction ty f . functionTable
