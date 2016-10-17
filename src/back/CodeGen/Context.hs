{-| The context that several of the Translatable typeclasses use
for compiling. It is used to generate new symbols for temporary
variables, store the mappings from encore variables to c variables
and to keep track of which class we're translating at the
moment. -}

module CodeGen.Context (
  Context,
  new,
  substAdd,
  substLkp,
  substRem,
  genNamedSym,
  genSym,
  getGlobalFunctionNames,
  getMethodName,
  putMethodName,
  lookupFunction,
  lookupField,
  lookupMethod,
  lookupMethodDecl,
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

type MethodName = String

data Context = Context VarSubTable NextSym MethodName Tbl.ProgramTable

programTable :: Context -> Tbl.ProgramTable
programTable (Context _ _ _ table) = table

new :: VarSubTable -> Tbl.ProgramTable -> Context
new subs = Context subs 0 ""

genNamedSym :: String -> State Context String
genNamedSym name = do
  let (_, name') = fixPrimes name
  c <- get
  case c of
    Context s n m t ->
        do put $ Context s (n+1) m t
           return $ "_" ++ name' ++ "_" ++ show n

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd c@(Context s nxt m table) na lv = Context ((na,lv):s) nxt m table

substRem :: Context -> Name -> Context
substRem (Context [] nxt m table) na = Context [] nxt m table
substRem (Context ((na, lv):s) nxt m table) na'
     | na == na'  = Context s nxt m table
     | na /= na'  = substAdd (substRem (Context s nxt m table) na') na lv

-- <<<<<<< 65432d6eaa5484e8c0b82de739f175cfec9b53dd
substLkp :: Context -> QualifiedName -> Maybe (C.CCode C.Lval)
substLkp (Context s _ _ _) QName{qnspace = Nothing, qnlocal} = lookup qnlocal s
substLkp (Context s _ _ _) QName{qnspace = Just [], qnlocal} = lookup qnlocal s
substLkp _ _ = Nothing
-- =======
-- substLkp :: Context -> Name -> Maybe (C.CCode C.Lval)
-- substLkp (Context s _ _ _) n = lookup n s

putMethodName :: Context -> String -> Context
putMethodName c@(Context s next name table) m = Context s next m table

getMethodName :: Context -> String
getMethodName c@(Context s nxt m table) = m
-- >>>>>>> Fix finding method name from inside of its body

lookupField :: Type -> Name -> Context -> FieldDecl
lookupField ty f = Tbl.lookupField ty f . programTable

lookupMethod :: Type -> Name -> Context -> FunctionHeader
lookupMethod ty m = Tbl.lookupMethod ty m . programTable

lookupMethodDecl :: Name -> Context -> MethodDecl
lookupMethodDecl m = Tbl.lookupMethodDecl m . programTable

lookupCalledType :: Type -> Name -> Context -> Type
lookupCalledType ty m = Tbl.lookupCalledType ty m . programTable

lookupFunction :: QualifiedName -> Context -> FunctionHeader
lookupFunction fname = Tbl.lookupFunction fname . programTable

getGlobalFunctionNames :: Context -> [QualifiedName]
getGlobalFunctionNames = Tbl.getGlobalFunctionNames . programTable
