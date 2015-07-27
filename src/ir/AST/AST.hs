{-# OPTIONS_GHC -fno-warn-missing-fields#-}
{-|

The abstract syntax tree produced by the parser. Each node carries
meta-information about its type (filled in by
"Typechecker.Typechecker") and its position in the source file
(filled in by "Parser.Parser")

-}

module AST.AST where

import Data.List
import Data.Maybe
import Text.Parsec(SourcePos)

import Identifiers
import Types
import AST.Meta hiding(Closure, Async)

data Program = Program {
  bundle :: BundleDecl,
  etl :: EmbedTL,
  imports :: [ImportDecl],
  functions :: [Function],
  traits :: [Trait],
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
                | PulledImport {pimeta :: Meta ImportDecl,
                                qname :: QName,
                                isrc :: FilePath,
                                iprogram :: Program }
                  deriving (Show)

data Function = Function {
  funmeta   :: Meta Function,
  funname   :: Name,
  funtype   :: Type,
  funparams :: [ParamDecl],
  funbody   :: Expr
} deriving (Show)

instance Eq Function where
  a == b = (funname a) == (funname b)

instance HasMeta Function where
  getMeta = funmeta
  setMeta f m = f{funmeta = m}
  setType ty f@(Function {funmeta, funtype}) = f {funmeta = AST.Meta.setType ty funmeta, funtype = ty}
  showWithKind Function{funname, funtype} = "function '" ++ show funname ++ "'"

data ClassDecl = Class {
  cmeta   :: Meta ClassDecl,
  cname   :: Type,
  ctraits  :: [ImplementedTrait],
  fields  :: [FieldDecl],
  methods :: [MethodDecl]
} deriving (Show)

makeClassDecl refKind name params ctraits =
  refKind name params (map (traitName . itrait) ctraits)

instance Eq ClassDecl where
  a == b = (cname a) == (cname b)

isActive :: ClassDecl -> Bool
isActive = isActiveRefType . cname

isMainClass :: ClassDecl -> Bool
isMainClass cdecl = (== "Main") . getId . cname $ cdecl

instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) =
      c {cmeta = AST.Meta.setType ty cmeta, cname = ty}
    showWithKind Class{cname} = "class '" ++ show cname ++ "'"

data Trait = Trait {
  traitMeta :: Meta Trait,
  traitName :: Type,
  traitFields :: [FieldDecl],
  traitMethods :: [MethodDecl]
}

instance Show Trait where
  show t@Trait{traitName} = show traitName

instance Eq Trait where
  a == b = (traitName a) == (traitName b)

instance HasMeta Trait where
  getMeta = traitMeta
  setMeta t m = t{traitMeta = m}
  setType ty t@Trait{traitMeta, traitName} =
    t{traitMeta = AST.Meta.setType ty traitMeta, traitName = ty}
  showWithKind Trait{traitName} = "trait '" ++ show traitName ++ "'"

data ImplementedTrait = ImplementedTrait {
  itraitMeta :: Meta ImplementedTrait,
  itrait :: Trait
}

itraitMethods :: ImplementedTrait -> [MethodDecl]
itraitMethods ImplementedTrait{itrait} = traitMethods itrait

implementTrait :: Meta ImplementedTrait -> Type -> ImplementedTrait
implementTrait itraitMeta ty =
  let
    params = (getTypeParameters ty)
    itrait = Trait{traitName = traitRefType (getId ty) params}
  in
    ImplementedTrait{itraitMeta, itrait}

instance HasMeta ImplementedTrait where
  getMeta = itraitMeta
  setMeta t m = t{itraitMeta = m}
  setType ty t@ImplementedTrait{itrait} =
    let itrait' = AST.AST.setType ty itrait
    in t{itrait = itrait'}
  showWithKind ImplementedTrait{itrait = Trait{traitName}} =
    "implemented trait '" ++ show traitName ++ "'"

instance Show ImplementedTrait where
  show ImplementedTrait{itrait} = show itrait

instance Eq ImplementedTrait where
  a == b = (id a) == (id b)
    where id = getId . traitName . itrait

data FieldDecl = Field {
  fmeta :: Meta FieldDecl,
  fname :: Name,
  ftype :: Type
}

instance Show FieldDecl where
  show f@Field{fname,ftype} = show fname ++ " : " ++ show ftype

instance Eq FieldDecl where
  a == b = (fname a) == (fname b)

instance HasMeta FieldDecl where
    getMeta = fmeta
    setMeta f m = f{fmeta = m}
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = AST.Meta.setType ty fmeta, ftype = ty}
    showWithKind Field{fname} = "field '" ++ show fname ++ "'"

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

instance Eq MethodDecl where
  a == b = (mname a) == (mname b)

instance HasMeta MethodDecl where
  getMeta = mmeta
  setMeta mtd m = mtd{mmeta = m}
  setType ty m@(Method {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}
  setType ty m@(StreamMethod {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}
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

    setType ty expr = expr {emeta = AST.Meta.setType ty (emeta expr)}

setSugared :: Expr -> Expr -> Expr
setSugared e sugared = e {emeta = AST.Meta.setSugared sugared (emeta e)}

getSugared :: Expr -> Maybe Expr
getSugared e = AST.Meta.getSugared (emeta e)


-- | program_traverse (needs better name) traverse a program and its imports collecting data
-- traverseProgram f g p takes traverses p, applying f and g to collect values
-- f applies to one level program, ignoring imports
-- g takes the results of recursing on imports plus the current level and combines them
traverseProgram :: (Program -> t) -> (t -> [b] -> b) -> Program -> b
traverseProgram f g p@(Program{imports}) =
  g (f p) (map (lift (traverseProgram f g)) imports)
    where
      lift :: (Program -> b) -> ImportDecl -> b
      lift h (PulledImport{iprogram}) = h iprogram

mapProgram :: Program -> (Program -> [a]) -> [a]
mapProgram program f =
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

getTrait :: Type -> Program -> Trait
getTrait t p =
  let
    traits = allTraits p
    match t trait = (getId t) == (getId $ traitName trait)
  in
    fromJust $ find (match t) traits

allTraits :: Program -> [Trait]
allTraits p = mapProgram p traits

merge a b = a ++ concat b

allClasses p = traverseProgram f merge p
  where f Program{classes} = classes

allFunctions p = traverseProgram f merge p
    where f Program{functions} = functions

allEmbedded p = traverseProgram f merge p
    where f Program{etl = EmbedTL{etlheader}} = [etlheader]
