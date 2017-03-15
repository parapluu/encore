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
  lookupClosureContext,
  lookupCalledType,
  setExecCtx,
  setMtdCtx,
  setFunCtx,
  setClsCtx,
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

data Context = Context {
  varSubTable  :: VarSubTable,
  nextSym      :: NextSym,
  execContext  :: ExecContext,
  programTbl   :: Tbl.ProgramTable,
  withForward  :: Bool
}

programTable :: Context -> Tbl.ProgramTable
programTable Context{programTbl} = programTbl

new :: VarSubTable -> Tbl.ProgramTable -> Context
new subs table = Context {
    varSubTable = subs
    ,nextSym = 0
    ,execContext = Empty
    ,programTbl = table
    ,withForward = False
  }

newWithForwarding subs table = Context {
    varSubTable = subs
    ,nextSym = 0
    ,execContext = Empty
    ,programTbl = table
    ,withForward = True
  }

withForwarding :: Context -> Bool
withForwarding Context{withForward} = withForward

genNamedSym :: String -> State Context String
genNamedSym name = do
  let (_, name') = fixPrimes name
  c <- get
  case c of
    ctx@Context{nextSym} ->
        do put $ ctx{nextSym = nextSym + 1}
           return $ "_" ++ name' ++ "_" ++ show nextSym

genSym :: State Context String
genSym = genNamedSym "tmp"

substAdd :: Context -> Name -> C.CCode C.Lval -> Context
substAdd ctx@Context{varSubTable} na lv = ctx{varSubTable = ((na,lv):varSubTable)}

substRem :: Context -> Name -> Context
substRem ctx@Context{varSubTable = []} na = ctx
substRem ctx@Context{varSubTable = ((na, lv):s)} na'
     | na == na'  = ctx{varSubTable = s}
     | na /= na'  = substAdd (substRem ctx{varSubTable = s} na') na lv

substLkp :: Context -> QualifiedName -> Maybe (C.CCode C.Lval)
substLkp ctx@Context{varSubTable} QName{qnspace = Nothing, qnlocal} = lookup qnlocal varSubTable
substLkp ctx@Context{varSubTable} QName{qnspace = Just ns, qnlocal}
     | isEmptyNamespace ns = lookup qnlocal varSubTable
     | otherwise = Nothing

setExecCtx :: Context -> ExecContext -> Context
setExecCtx ctx@Context{execContext} execContext' = ctx{execContext = execContext'}

setFunCtx :: Context -> Function -> Context
setFunCtx ctx@Context{execContext} execContext' = ctx{execContext = FunctionContext{fun = execContext'}}

setMtdCtx :: Context -> MethodDecl -> Context
setMtdCtx ctx@Context{execContext} execContext' = ctx{execContext = MethodContext{mdecl = execContext'}}

setClsCtx :: Context -> Expr -> Context
setClsCtx ctx@Context{execContext} execContext' = ctx{execContext = ClosureContext{cls = execContext'}}

getExecCtx :: Context -> ExecContext
getExecCtx ctx@Context{execContext} = execContext

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

lookupClosureContext :: ExecContext -> [Expr]
lookupClosureContext ClosureContext{cls} = [cls]
lookupClosureContext _ = []

getGlobalFunctionNames :: Context -> [QualifiedName]
getGlobalFunctionNames = Tbl.getGlobalFunctionNames . programTable
