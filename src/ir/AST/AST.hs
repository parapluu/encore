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
import AST.Meta as Meta hiding(Closure, Async)

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
    getPos = Meta.getPos . getMeta

    getType :: a -> Type
    getType = Meta.getType . getMeta

    setType :: Type -> a -> a

    hasType :: a -> Type -> Bool
    hasType x ty = if ty == nullType then
                       not $ isPrimitive ty'
                   else
                       ty == ty'
                   where
                     ty' = AST.AST.getType x

    isFree :: a -> Bool
    isFree = Meta.isFree . getMeta

    isCaptured :: a -> Bool
    isCaptured = Meta.isCaptured . getMeta

    makeFree :: a -> a
    makeFree x = let meta = Meta.makeFree (getMeta x)
                 in setMeta x meta

    makeCaptured :: a -> a
    makeCaptured x = let meta = Meta.makeCaptured (getMeta x)
                     in setMeta x meta

    getArrowType :: a -> Type
    getArrowType = Meta.getMetaArrowType . getMeta

    setArrowType :: Type -> a -> a
    setArrowType ty x = let meta = getMeta x
                        in setMeta x $ Meta.setMetaArrowType ty meta

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
  setType ty f@(Function {funmeta, funtype}) = f {funmeta = Meta.setType ty funmeta, funtype = ty}
  showWithKind Function{funname, funtype} = "function '" ++ show funname ++ "'"

data ClassDecl = Class {
  cmeta   :: Meta ClassDecl,
  cname   :: Type,
  cfields  :: [FieldDecl],
  cmethods :: [MethodDecl]
} deriving (Show)

instance Eq ClassDecl where
  a == b = getId (cname a) == getId (cname b)

isActive :: ClassDecl -> Bool
isActive = isActiveClassType . cname

isMainClass :: ClassDecl -> Bool
isMainClass cdecl = (== "Main") . getId . cname $ cdecl

instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) =
      c {cmeta = Meta.setType ty cmeta, cname = ty}
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
    t{tmeta = Meta.setType ty tmeta, tname = ty}
  showWithKind Trait{tname} = "trait '" ++ getId tname ++ "'"

data FieldDecl = Field {
  fmeta :: Meta FieldDecl,
  fname :: Name,
  ftype :: Type
}

instance Show FieldDecl where
  show f@Field{fname,ftype} = show fname ++ " : " ++ show ftype

instance Eq FieldDecl where
  a == b = fname a == fname b

instance HasMeta FieldDecl where
    getMeta = fmeta
    setMeta f m = f{fmeta = m}
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = Meta.setType ty fmeta, ftype = ty}
    showWithKind Field{fname} = "field '" ++ show fname ++ "'"

data ParamDecl = Param {
  pmeta :: Meta ParamDecl,
  pname :: Name,
  ptype :: Type
} deriving (Show, Eq)

instance HasMeta ParamDecl where
    getMeta = pmeta
    setMeta p m = p{pmeta = m}
    setType ty p@(Param {pmeta, ptype}) = p {pmeta = Meta.setType ty pmeta, ptype = ty}
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

instance Eq MethodDecl where
  a == b = mname a == mname b

instance HasMeta MethodDecl where
  getMeta = mmeta
  setMeta mtd m = mtd{mmeta = m}
  setType ty m@(Method {mmeta, mtype}) = m {mmeta = Meta.setType ty mmeta, mtype = ty}
  setType ty m@(StreamMethod {mmeta, mtype}) = m {mmeta = Meta.setType ty mmeta, mtype = ty}
  showWithKind Method {mname} = "method '" ++ show mname ++ "'"
  showWithKind StreamMethod {mname} = "streaming method '" ++ show mname ++ "'"

type Arguments = [Expr]

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
          | Async {emeta :: Meta Expr,
                   body :: Expr}
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
          | Consume {emeta :: Meta Expr,
                     target :: Expr}
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
                   stringLit :: String,
                   args :: [Expr]}
          | Exit {emeta :: Meta Expr,
                  args :: [Expr]}
          | StringLiteral {emeta :: Meta Expr,
                           stringLit :: String}
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

    setType ty expr = expr {emeta = Meta.setType ty (emeta expr)}

setSugared :: Expr -> Expr -> Expr
setSugared e sugared = e {emeta = Meta.setSugared sugared (emeta e)}

getSugared :: Expr -> Maybe Expr
getSugared e = Meta.getSugared (emeta e)


traverseProgram :: (Program -> [a]) -> Program -> [a]
traverseProgram f program =
  let
    programs = flatten_imports program
  in
    concatMap f programs
  where
    flatten_imports :: Program -> [Program]
    flatten_imports program@Program{imports} =
      let
        programs = map iprogram imports
      in
        program : concatMap flatten_imports programs

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
