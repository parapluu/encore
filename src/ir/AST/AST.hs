{-|

The abstract syntax tree produced by the parser. Each node carries
meta-information about its type (filled in by
"Typechecker.Typechecker") and its position in the source file
(filled in by "Parser.Parser")

-}

module AST.AST where

import Data.List
import Data.Maybe
import Text.Parsec(SourcePos, SourceName)

import Identifiers
import Types
import AST.Meta hiding(Closure, Async)

data Program = Program {
  source :: SourceName,
  bundle :: BundleDecl,
  etl :: EmbedTL,
  imports :: [ImportDecl],
  functions :: [Function],
  traits :: [TraitDecl],
  classes :: [ClassDecl]
} deriving (Show)

class Show a => HasMeta a where
    getMeta :: a -> Meta a

    setMeta :: a -> Meta a -> a

    getPos :: a -> SourcePos
    getPos = AST.Meta.getPos . getMeta

    getType :: a -> Type
    getType = AST.Meta.getType . getMeta

    setType :: Type -> a -> a

    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

    getMetaInfo :: a -> MetaInfo
    getMetaInfo = AST.Meta.metaInfo . getMeta

    showWithKind :: a -> String
    showWithKind = show

data EmbedTL = EmbedTL {etlmeta   :: Meta EmbedTL,
                        etlheader :: String,
                        etlbody   :: String } deriving (Show)

data BundleDecl = Bundle { bmeta :: Meta BundleDecl,
                           bname :: QName }
                | NoBundle
                deriving Show

data ImportDecl = Import {imeta   :: Meta ImportDecl,
                          itarget :: QName }
                | PulledImport {imeta :: Meta ImportDecl,
                                qname :: QName,
                                isrc :: FilePath,
                                iprogram :: Program }
                  deriving (Show)

instance HasMeta ImportDecl where
    getMeta = imeta

    setMeta i m = i{imeta = m}

    setType ty i =
        error "AST.hs: Cannot set the type of an ImportDecl"

data Function = Function {
  funmeta   :: Meta Function,
  funname   :: Name,
  funtype   :: Type,
  funparams :: [ParamDecl],
  funbody   :: Expr
} deriving (Show)

instance Eq Function where
  a == b = funname a == funname b

instance HasMeta Function where
  getMeta = funmeta
  setMeta f m = f{funmeta = m}
  setType ty f@(Function {funmeta, funtype}) = f {funmeta = AST.Meta.setType ty funmeta, funtype = ty}
  showWithKind Function{funname, funtype} = "function '" ++ show funname ++ "'"

data ClassDecl = Class {
  cmeta       :: Meta ClassDecl,
  cname       :: Type,
  ccapability :: Type,
  cfields     :: [FieldDecl],
  cmethods    :: [MethodDecl]
} deriving (Show)

instance Eq ClassDecl where
  a == b = getId (cname a) == getId (cname b)

isActive :: ClassDecl -> Bool
isActive = isActiveClassType . cname

isShared :: ClassDecl -> Bool
isShared = isSharedClassType . cname

isPassive :: ClassDecl -> Bool
isPassive = isPassiveClassType . cname

isMainClass :: ClassDecl -> Bool
isMainClass cdecl = (== "Main") . getId . cname $ cdecl

instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) =
      c {cmeta = AST.Meta.setType ty cmeta, cname = ty}
    showWithKind Class{cname} = "class '" ++ getId cname ++ "'"

data TraitDecl = Trait {
  tmeta :: Meta TraitDecl,
  tname :: Type,
  tfields :: [FieldDecl],
  tmethods :: [MethodDecl]
} deriving (Show)

instance Eq TraitDecl where
  a == b = getId (tname a) == getId (tname b)

instance HasMeta TraitDecl where
  getMeta = tmeta
  setMeta t m = t{tmeta = m}
  setType ty t@Trait{tmeta, tname} =
    t{tmeta = AST.Meta.setType ty tmeta, tname = ty}
  showWithKind Trait{tname} = "trait '" ++ getId tname ++ "'"

data Modifier = Val
                deriving(Eq)

instance Show Modifier where
    show Val = "val"

data FieldDecl = Field {
  fmeta :: Meta FieldDecl,
  fmods :: [Modifier],
  fname :: Name,
  ftype :: Type
}

instance Show FieldDecl where
  show f@Field{fmods,fname,ftype} =
      smods ++ show fname ++ " : " ++ show ftype
    where
      smods = concatMap ((++ " ") . show) fmods

instance Eq FieldDecl where
  a == b = fname a == fname b

instance HasMeta FieldDecl where
    getMeta = fmeta
    setMeta f m = f{fmeta = m}
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = AST.Meta.setType ty fmeta, ftype = ty}
    showWithKind Field{fname} = "field '" ++ show fname ++ "'"

isValField :: FieldDecl -> Bool
isValField = (Val `elem`) . fmods

data ParamDecl = Param {
  pmeta :: Meta ParamDecl,
  pname :: Name,
  ptype :: Type
} deriving (Show, Eq)

instance HasMeta ParamDecl where
    getMeta = pmeta
    setMeta p m = p{pmeta = m}
    setType ty p@(Param {pmeta, ptype}) = p {pmeta = AST.Meta.setType ty pmeta, ptype = ty}
    showWithKind Param{pname} = "parameter '" ++ show pname ++ "'"

data MethodDecl = Method {mmeta   :: Meta MethodDecl,
                          mname   :: Name,
                          mtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr}
                | StreamMethod {mmeta   :: Meta MethodDecl,
                                mname   :: Name,
                                mtype   :: Type,
                                mparams :: [ParamDecl],
                                mbody   :: Expr} deriving (Show)

isStreamMethod StreamMethod{} = True
isStreamMethod _ = False

isMainMethod :: Type -> Name -> Bool
isMainMethod ty name = isMainType ty && (name == Name "main")

isConstructor :: MethodDecl -> Bool
isConstructor m = mname m == Name "_init"

replaceMethodTypes :: [(Type, Type)] -> MethodDecl -> MethodDecl
replaceMethodTypes bindings m =
    let mparams' = map (replaceParamType bindings) (mparams m)
        mtype' = replaceTypeVars bindings (mtype m)
    in
      m{mparams = mparams', mtype = mtype'}
    where
      replaceParamType bindings p@Param{ptype} =
          p{ptype = replaceTypeVars bindings ptype}

instance Eq MethodDecl where
  a == b = mname a == mname b

instance HasMeta MethodDecl where
  getMeta = mmeta
  setMeta mtd m = mtd{mmeta = m}
  setType ty m@(Method {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}
  setType ty m@(StreamMethod {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}
  showWithKind Method {mname} = "method '" ++ show mname ++ "'"
  showWithKind StreamMethod {mname} = "streaming method '" ++ show mname ++ "'"

data MatchClause = MatchClause {mcmeta    :: Meta MatchClause,
                                mcpattern :: Expr,
                                mchandler :: Expr,
                                mcguard   :: Expr} deriving (Show, Eq)
instance HasMeta MatchClause where
    getMeta = mcmeta
    setMeta mc m = mc{mcmeta = m}
    setType ty mc@MatchClause{mchandler} = mc {mchandler = AST.AST.setType ty mchandler}

type Arguments = [Expr]

data MaybeContainer = JustData { e :: Expr}
                    | NothingData deriving(Eq, Show)

data Expr = Skip {emeta :: Meta Expr}
          | Breathe {emeta :: Meta Expr}
          | TypedExpr {emeta :: Meta Expr,
                       body :: Expr,
                       ty   :: Type}
          | MethodCall {emeta :: Meta Expr,
                        target :: Expr,
                        name :: Name,
                        args :: Arguments}
          | MessageSend {emeta :: Meta Expr,
                         target :: Expr,
                         name :: Name,
                         args :: Arguments}
          | FunctionCall {emeta :: Meta Expr,
                          name :: Name,
                          args :: Arguments}
          | Closure {emeta :: Meta Expr,
                     eparams :: [ParamDecl],
                     body :: Expr}
          | Liftf {emeta :: Meta Expr,
                   val :: Expr}
          | Liftv {emeta :: Meta Expr,
                   val :: Expr}
          | PartyJoin {emeta :: Meta Expr,
                       val :: Expr}
          | PartyExtract {emeta :: Meta Expr,
                          val :: Expr}
          | PartySeq {emeta :: Meta Expr,
                      par :: Expr,
                      seqfunc :: Expr}
          | PartyPar {emeta :: Meta Expr,
                      parl :: Expr,
                      parr :: Expr}
          | Async {emeta :: Meta Expr,
                   body :: Expr}
          | MaybeValue {emeta :: Meta Expr,
                        mdt :: MaybeContainer }
          | Tuple {emeta :: Meta Expr,
                   args :: [Expr]}
          | Foreach {emeta :: Meta Expr,
                     item :: Name,
                     arr :: Expr,
                     body :: Expr}
          | FinishAsync {emeta :: Meta Expr,
                         body :: Expr}
          | Let {emeta :: Meta Expr,
                 decls :: [(Name, Expr)],
                 body :: Expr}
          | Seq {emeta :: Meta Expr,
                 eseq :: [Expr]}
          | IfThenElse {emeta :: Meta Expr,
                        cond :: Expr,
                        thn :: Expr,
                        els :: Expr}
          | IfThen {emeta :: Meta Expr,
                    cond :: Expr,
                    thn :: Expr}
          | Unless {emeta :: Meta Expr,
                    cond :: Expr,
                    thn :: Expr}
          | While {emeta :: Meta Expr,
                   cond :: Expr,
                   body :: Expr}
          | Repeat {emeta :: Meta Expr,
                    name :: Name,
                    times :: Expr,
                    body :: Expr}
          | For {emeta  :: Meta Expr,
                 name   :: Name,
                 step   :: Expr,
                 src    :: Expr,
                 body   :: Expr}
          | Match {emeta :: Meta Expr,
                   arg :: Expr,
                   clauses :: [MatchClause]}
          | Get {emeta :: Meta Expr,
                 val :: Expr}
          | Yield {emeta :: Meta Expr,
                   val :: Expr}
          | Eos {emeta :: Meta Expr}
          | IsEos {emeta :: Meta Expr,
                   target :: Expr}
          | StreamNext {emeta :: Meta Expr,
                        target :: Expr}
          | Await {emeta :: Meta Expr,
                   val :: Expr}
          | Suspend {emeta :: Meta Expr}
          | FutureChain {emeta :: Meta Expr,
                        future :: Expr,
                         chain :: Expr}
          | FieldAccess {emeta :: Meta Expr,
                         target :: Expr,
                         name :: Name}
          | ArrayAccess {emeta :: Meta Expr,
                         target :: Expr,
                         index :: Expr}
          | ArraySize {emeta :: Meta Expr,
                       target :: Expr}
          | ArrayNew {emeta :: Meta Expr,
                      ty :: Type,
                      size :: Expr}
          | ArrayLiteral {emeta :: Meta Expr,
                          args :: [Expr]}
          | Assign {emeta :: Meta Expr,
                    lhs :: Expr,
                    rhs :: Expr}
          | VarAccess {emeta :: Meta Expr,
                       name :: Name}
          | Null {emeta :: Meta Expr}
          | BTrue {emeta :: Meta Expr}
          | BFalse {emeta :: Meta Expr}
          | NewWithInit {emeta :: Meta Expr,
                         ty ::Type,
                         args :: Arguments}
          | New {emeta :: Meta Expr,
                 ty ::Type}
          | Peer {emeta :: Meta Expr,
                  ty ::Type}
          | Print {emeta :: Meta Expr,
                   args :: [Expr]}
          | Exit {emeta :: Meta Expr,
                  args :: [Expr]}
          | StringLiteral {emeta :: Meta Expr,
                           stringLit :: String}
          | CharLiteral {emeta :: Meta Expr,
                         charLit :: Char}
          | RangeLiteral {emeta :: Meta Expr,
                          start  :: Expr,
                          stop   :: Expr,
                          step   :: Expr}
          | IntLiteral {emeta :: Meta Expr,
                        intLit :: Int}
          | RealLiteral {emeta :: Meta Expr,
                         realLit :: Double}
          | Embed {emeta :: Meta Expr,
                   ty    :: Type,
                   code  :: String}
          | Unary {emeta :: Meta Expr,
                   uop   :: UnaryOp,
                   operand  :: Expr }
          | Binop {emeta :: Meta Expr,
                   binop :: BinaryOp,
                   loper :: Expr,
                   roper :: Expr} deriving(Show, Eq)

isLval :: Expr -> Bool
isLval VarAccess {} = True
isLval FieldAccess {} = True
isLval ArrayAccess {} = True
isLval _ = False

isThisAccess :: Expr -> Bool
isThisAccess VarAccess {name = Name "this"} = True
isThisAccess _ = False

isClosure :: Expr -> Bool
isClosure Closure {} = True
isClosure _ = False

isTask :: Expr -> Bool
isTask Async {} = True
isTask _ = False

isRangeLiteral :: Expr -> Bool
isRangeLiteral RangeLiteral {} = True
isRangeLiteral _ = False

isCallable :: Expr -> Bool
isCallable e = isArrowType (AST.AST.getType e)

isStringLiteral :: Expr -> Bool
isStringLiteral StringLiteral {} = True
isStringLiteral _ = False

instance HasMeta Expr where
    getMeta = emeta
    setMeta e m = e{emeta = m}

    hasType (Null {}) ty = not . isPrimitive $ ty
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

    setType ty expr = expr {emeta = AST.Meta.setType ty (emeta expr)}

setSugared :: Expr -> Expr -> Expr
setSugared e sugared = e {emeta = AST.Meta.setSugared sugared (emeta e)}

getSugared :: Expr -> Maybe Expr
getSugared e = AST.Meta.getSugared (emeta e)

traverseProgram :: (Program -> [a]) -> Program -> [a]
traverseProgram f program =
  let
    programs = flattenImports program
  in
    concatMap f programs
  where
    flattenImports :: Program -> [Program]
    flattenImports program@Program{imports} =
      let
        programs = map iprogram imports
      in
        program : concatMap flattenImports programs

getTrait :: Type -> Program -> TraitDecl
getTrait t p =
  let
    traits = allTraits p
    match t trait = getId t == getId (tname trait)
  in
    fromJust $ find (match t) traits

allClasses = traverseProgram classes

allTraits = traverseProgram traits

allFunctions = traverseProgram functions

allEmbedded = traverseProgram ((:[]) . etlheader . etl)
