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
  lookupFunction,
  lookupField,
  lookupMethod,
  lookupCalledType,
  putMethodName,
  getMethodName,
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

data Context = Context VarSubTable NextSym Name Tbl.ProgramTable

programTable :: Context -> Tbl.ProgramTable
programTable (Context _ _ _ table) = table

new :: VarSubTable -> Tbl.ProgramTable -> Context
new subs = Context subs 0 (Name "")

genNamedSym :: String -> State Context String
genNamedSym name = do
  let (_, name') = fixPrimes name
  c <- get
  case c of
    Context s n fname t ->
        do put $ Context s (n+1) fname t
           return $ "_" ++ name' ++ "_" ++ show n

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd c@(Context s nxt fname table) na lv = Context ((na,lv):s) nxt fname table

substRem :: Context -> Name -> Context
substRem (Context [] nxt fname table) na = Context [] nxt fname table
substRem (Context ((na, lv):s) nxt fname table) na'
     | na == na'  = Context s nxt fname table
     | na /= na'  = substAdd (substRem (Context s nxt fname table) na') na lv

substLkp :: Context -> QualifiedName -> Maybe (C.CCode C.Lval)
substLkp (Context s _ _ _) QName{qnspace = Nothing, qnlocal} = lookup qnlocal s
substLkp (Context s _ _ _) QName{qnspace = Just ns, qnlocal}
     | isEmptyNamespace ns = lookup qnlocal s
     | otherwise = Nothing

putMethodName :: Context -> Name -> Context
putMethodName c@(Context s next fname table) m = Context s next m table

getMethodName :: Context -> Name
getMethodName c@(Context s nxt fname table) = fname

lookupField :: Type -> Name -> Context -> FieldDecl
lookupField ty f = Tbl.lookupField ty f . programTable

lookupMethod :: Type -> Name -> Context -> FunctionHeader
lookupMethod ty m = Tbl.lookupMethod ty m . programTable

lookupCalledType :: Type -> Name -> Context -> Type
lookupCalledType ty m = Tbl.lookupCalledType ty m . programTable

lookupFunction :: QualifiedName -> Context -> (C.CCode C.Name, FunctionHeader)
lookupFunction fname = Tbl.lookupFunction fname . programTable

getGlobalFunctionNames :: Context -> [QualifiedName]
getGlobalFunctionNames = Tbl.getGlobalFunctionNames . programTable
