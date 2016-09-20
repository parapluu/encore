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
  getGlobalFunctionNames,
  lookupFunction,
  lookupField,
  lookupMethod,
  lookupCalledType,
) where

import Identifiers
import Types
import AST.AST
import Control.Monad.State
import qualified CodeGen.ClassTable as Tbl

import qualified CCode.Main as C
import CodeGen.CCodeNames

type NextSym = Int

type VarSubTable = [(Name, C.CCode C.Lval)] -- variable substitutions (for supporting, for instance, nested var decls)

data Context = Context VarSubTable NextSym Tbl.ClassTable Tbl.FunctionTable

classTable :: Context -> Tbl.ClassTable
classTable (Context _ _ ctable _) = ctable

functionTable :: Context -> Tbl.FunctionTable
functionTable (Context _ _ _ ftable) = ftable

empty :: Tbl.ClassTable -> Tbl.FunctionTable -> Context
empty ctable ftable = new [] ctable ftable

new :: VarSubTable -> Tbl.ClassTable -> Tbl.FunctionTable -> Context
new subs ctable ftable = Context subs 0 ctable ftable

genNamedSym :: String -> State Context String
genNamedSym name = do
  let (_, name') = fixPrimes name
  c <- get
  case c of
    Context s n t f ->
        do put $ Context s (n+1) t f
           return $ "_" ++ name' ++ "_" ++ show n

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd c@(Context s nxt ctable ftable) na lv = Context ((na,lv):s) nxt ctable ftable

substRem :: Context -> Name -> Context
substRem (Context [] nxt ctable ftable) na = Context [] nxt ctable ftable
substRem (Context ((na, lv):s) nxt ctable ftable) na'
     | na == na'  = Context s nxt ctable ftable
     | na /= na'  = substAdd (substRem (Context s nxt ctable ftable) na') na lv

substLkp :: Context -> Name -> Maybe (C.CCode C.Lval)
substLkp (Context s _ _ _) n = lookup n s

lookupField :: Type -> Name -> Context -> FieldDecl
lookupField ty f = Tbl.lookupField ty f . classTable

lookupMethod :: Type -> Name -> Context -> FunctionHeader
lookupMethod ty m = Tbl.lookupMethod ty m . classTable

lookupCalledType :: Type -> Name -> Context -> Type
lookupCalledType ty m = Tbl.lookupCalledType ty m . classTable

lookupFunction :: Name -> Context -> FunctionHeader
lookupFunction fname = Tbl.lookupFunction fname . functionTable

getGlobalFunctionNames :: Context -> [Name]
getGlobalFunctionNames = Tbl.getGlobalFunctionNames . functionTable
