{-| The context that several of the Translatable typeclasses use
for compiling. It is used to generate new symbols for temporary
variables, store the mappings from encore variables to c variables
and to keep track of which class we're translating at the
moment. -}

module CodeGen.Context (
  Context,
  ExecContext,
  new,
  newWithForwarding,
  substAdd,
  substLkp,
  substRem,
  genNamedSym,
  genSym,
  getGlobalFunctionNames,
  lookupFunction,
  lookupFunctionContext,
  lookupField,
  lookupMethod,
  lookupMethodContext,
  lookupCalledType,
  setExecCtx,
  setMtdCtx,
  setFunCtx,
  getExecCtx,
  withForwarding
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

data ExecContext =
    FunctionContext{fun :: Function}
  | MethodContext  {mdecl :: MethodDecl}
  | ClosureContext {cls :: Expr} -- for checking closure in the future.
  | Empty
    deriving(Show)

data Context = Context VarSubTable NextSym ExecContext Tbl.ProgramTable Bool

programTable :: Context -> Tbl.ProgramTable
programTable (Context _ _ _ table _) = table

new :: VarSubTable -> Tbl.ProgramTable -> Context
new subs table = Context subs 0 Empty table False

newWithForwarding subs table = Context subs 0 Empty table True

withForwarding :: Context -> Bool
withForwarding (Context _ _ _ _ b) = b

genNamedSym :: String -> State Context String
genNamedSym name = do
  let (_, name') = fixPrimes name
  c <- get
  case c of
    Context s n eCtx t b ->
        do put $ Context s (n+1) eCtx t b
           return $ "_" ++ name' ++ "_" ++ show n

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd c@(Context s nxt eCtx table b) na lv = Context ((na,lv):s) nxt eCtx table b

substRem :: Context -> Name -> Context
substRem (Context [] nxt eCtx table b) na = Context [] nxt eCtx table b
substRem (Context ((na, lv):s) nxt eCtx table b) na'
     | na == na'  = Context s nxt eCtx table b
     | na /= na'  = substAdd (substRem (Context s nxt eCtx table b) na') na lv

substLkp :: Context -> QualifiedName -> Maybe (C.CCode C.Lval)
substLkp (Context s _ _ _ _) QName{qnspace = Nothing, qnlocal} = lookup qnlocal s
substLkp (Context s _ _ _ _) QName{qnspace = Just ns, qnlocal}
     | isEmptyNamespace ns = lookup qnlocal s
     | otherwise = Nothing

setExecCtx :: Context -> ExecContext -> Context
setExecCtx c@(Context s next eCtx table b) eCtx' = Context s next eCtx' table b

setFunCtx :: Context -> Function -> Context
setFunCtx c@(Context s next eCtx table b) eCtx' = Context s next (FunctionContext{fun = eCtx'}) table b

setMtdCtx :: Context -> MethodDecl -> Context
setMtdCtx c@(Context s next eCtx table b) eCtx' = Context s next (MethodContext{mdecl = eCtx'}) table b

getExecCtx :: Context -> ExecContext
getExecCtx c@(Context s nxt eCtx table _) = eCtx

lookupField :: Type -> Name -> Context -> FieldDecl
lookupField ty f = Tbl.lookupField ty f . programTable

lookupMethod :: Type -> Name -> Context -> FunctionHeader
lookupMethod ty m = Tbl.lookupMethod ty m . programTable

lookupCalledType :: Type -> Name -> Context -> Type
lookupCalledType ty m = Tbl.lookupCalledType ty m . programTable

lookupFunction :: QualifiedName -> Context -> (C.CCode C.Name, FunctionHeader)
lookupFunction fname = Tbl.lookupFunction fname . programTable

lookupFunctionContext :: ExecContext -> [Function]
lookupFunctionContext FunctionContext{fun} = [fun]
lookupFunctionContext _ = []

lookupMethodContext :: ExecContext -> [MethodDecl]
lookupMethodContext MethodContext{mdecl} = [mdecl]
lookupMethodContext _ = []

getGlobalFunctionNames :: Context -> [QualifiedName]
getGlobalFunctionNames = Tbl.getGlobalFunctionNames . programTable
