{-# LANGUAGE NamedFieldPuns #-}

{-|

The abstract syntax tree produced by the parser. Each node carries
meta-information about its type (filled in by
"Typechecker.Typechecker") and its position in the source file
(filled in by "Parser.Parser")

-}

module AST.AST where

import Text.Parsec(SourcePos)

import Identifiers
import Types
import AST.Meta hiding(Closure)

data Program = Program {etl :: EmbedTL, 
                        imports :: [ImportDecl], 
                        functions :: [Function], 
                        classes :: [ClassDecl]} deriving(Show)

class HasMeta a where
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

data EmbedTL = EmbedTL {etlmeta   :: Meta EmbedTL,
                        etlheader :: String,
                        etlbody   :: String} deriving (Show)

data ImportDecl = Import {imeta   :: Meta ImportDecl,
                          itarget :: QName } 
                | PulledImport {pimeta :: Meta ImportDecl,
                                qname :: QName,
                                isrc :: FilePath,
                                iprogram :: Program }
                  deriving (Show)

data Function = Function {funmeta   :: Meta Function,
                          funname   :: Name,
                          funtype   :: Type,
                          funparams :: [ParamDecl],
                          funbody   :: Expr} deriving (Show, Eq)

instance HasMeta Function where
    getMeta = funmeta
    setMeta f m = f{funmeta = m}
    setType ty f@(Function {funmeta, funtype}) = f {funmeta = AST.Meta.setType ty funmeta, funtype = ty}

data ClassDecl = Class {cmeta   :: Meta ClassDecl,
                        cname   :: Type,
                        fields  :: [FieldDecl], 
                        methods :: [MethodDecl]} deriving(Show, Eq)

isActive :: ClassDecl -> Bool
isActive = isActiveRefType . cname

isMainClass :: ClassDecl -> Bool
isMainClass cdecl = (== "Main") . getId . cname $ cdecl

instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) = c {cmeta = AST.Meta.setType ty cmeta, cname = ty}

data FieldDecl = Field {fmeta :: Meta FieldDecl, 
                        fname :: Name, 
                        ftype :: Type} deriving(Show, Eq)

instance HasMeta FieldDecl where
    getMeta = fmeta
    setMeta f m = f{fmeta = m}
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = AST.Meta.setType ty fmeta, ftype = ty}

data ParamDecl = Param {pmeta :: Meta ParamDecl, pname :: Name, ptype :: Type} deriving(Show, Eq)

instance HasMeta ParamDecl where
    getMeta = pmeta
    setMeta p m = p{pmeta = m}
    setType ty p@(Param {pmeta, ptype}) = p {pmeta = AST.Meta.setType ty pmeta, ptype = ty}

data MethodDecl = Method {mmeta   :: Meta MethodDecl,
                          mname   :: Name,
                          mtype   :: Type,
                          mparams :: [ParamDecl],
                          mbody   :: Expr} 
                | StreamMethod {mmeta   :: Meta MethodDecl,
                                mname   :: Name,
                                mtype   :: Type,
                                mparams :: [ParamDecl],
                                mbody   :: Expr} deriving (Show, Eq)

isStreamMethod StreamMethod{} = True
isStreamMethod _ = False

instance HasMeta MethodDecl where
    getMeta = mmeta
    setMeta mtd m = mtd{mmeta = m}
    setType ty m@(Method {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}
    setType ty m@(StreamMethod {mmeta, mtype}) = m {mmeta = AST.Meta.setType ty mmeta, mtype = ty}

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
                   op    :: Op,
                   operand  :: Expr }
          | Binop {emeta :: Meta Expr,
                   op :: Op,
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

    setType ty' expr@(TypedExpr {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(New {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(Peer {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty' expr@(Embed {ty}) = expr {emeta = AST.Meta.setType ty' (emeta expr), ty = ty'}
    setType ty expr = expr {emeta = AST.Meta.setType ty (emeta expr)}

setSugared :: Expr -> Expr -> Expr
setSugared e sugared = e {emeta = AST.Meta.setSugared sugared (emeta e)}

getSugared :: Expr -> Maybe Expr
getSugared e = AST.Meta.getSugared (emeta e)

