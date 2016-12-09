{-|

The abstract syntax tree produced by the parser. Each node carries
meta-information about its type (filled in by
"Typechecker.Typechecker") and its position in the source file
(filled in by "Parser.Parser")

-}

module AST.AST where

import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parsec(SourcePos, SourceName)

import Identifiers
import Types
import AST.Meta hiding(Closure, Async)

data Program = Program {
  source :: SourceName,
  moduledecl :: ModuleDecl,
  etl :: [EmbedTL],
  imports :: [ImportDecl],
  typedefs :: [Typedef],
  functions :: [Function],
  traits :: [TraitDecl],
  classes :: [ClassDecl]
} deriving (Show)

setProgramSource source p = p{source}

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

data EmbedTL = EmbedTL {
      etlmeta   :: Meta EmbedTL,
      etlheader :: String,
      etlbody   :: String
    } deriving (Show)

data ModuleDecl = Module {
      modmeta :: Meta ModuleDecl,
      modname :: Name,
      modexports :: Maybe [Name]
    }
  | NoModule deriving(Show, Eq)

instance HasMeta ModuleDecl where
    getMeta = modmeta

    setMeta NoModule _ = error "AST.hs: Cannot set meta of NoModule"
    setMeta m modmeta = m{modmeta}

    setType ty i =
        error "AST.hs: Cannot set the type of a ModuleDecl"

moduleName NoModule = Name "<default>"
moduleName Module{modname} = modname

moduleExports NoModule = Nothing
moduleExports Module{modexports} = modexports

data ImportDecl = Import {
      imeta   :: Meta ImportDecl,
      itarget :: Namespace,
      iqualified :: Bool,
      ihiding :: Maybe [Name],
      iselect :: Maybe [Name],
      ialias :: Maybe Namespace,
      isource :: Maybe SourceName
    } deriving (Show)

instance HasMeta ImportDecl where
    getMeta = imeta

    setMeta i m = i{imeta = m}

    setType ty i =
        error "AST.hs: Cannot set the type of an ImportDecl"

data Typedef = Typedef {
   typedefmeta :: Meta Typedef,
   typedefdef  :: Type  -- will be a TypeSynonym, with left and right hand side of definition built in
} deriving (Show)

instance HasMeta Typedef where
    getMeta = typedefmeta

    setMeta i m = i{typedefmeta = m}

    setType ty i =
        error "AST.hs: Cannot set the type of an Typedef"


data HeaderKind = Streaming
                | NonStreaming
                  deriving(Eq, Show)

data FunctionHeader =
    Header {
        kind        :: HeaderKind,
        htypeparams :: [Type],
        hname       :: Name,
        htype       :: Type,
        hparams     :: [ParamDecl]
    }
    | MatchingHeader {
        kind        :: HeaderKind,
        htypeparams :: [Type],
        hname       :: Name,
        htype       :: Type,
        hparamtypes :: [Type],
        hpatterns   :: [Expr],
        hguard      :: Expr
    }deriving(Eq, Show)


setHeaderType ty h = h{htype = ty}

isStreamMethodHeader h = kind h == Streaming

-- MatchingFunction instances should be replaced by regular
-- functions after desugaring
data Function =
    Function {
      funmeta   :: Meta Function,
      funheader :: FunctionHeader,
      funbody   :: Expr,
      funsource :: SourceName
    }
  | MatchingFunction {
      funmeta         :: Meta Function,
      matchfunheaders :: [FunctionHeader],
      matchfunbodies  :: [Expr],
      funsource       :: SourceName
    } deriving (Show)

functionName = hname . funheader
functionParams = hparams . funheader
functionTypeParams = htypeparams . funheader
functionType = htype . funheader

instance Eq Function where
  a == b = (hname . funheader $ a) == (hname . funheader $ b)

instance HasMeta Function where
  getMeta = funmeta
  setMeta f m = f{funmeta = m}
  setType ty f@(Function {funmeta}) =
      f{funmeta = AST.Meta.setType ty funmeta}
  setType ty f@(MatchingFunction {funmeta}) =
      f{funmeta = AST.Meta.setType ty funmeta}
  showWithKind Function{funheader} =
      "function '" ++ show (hname funheader) ++ "'"
  showWithKind MatchingFunction{matchfunheaders} =
      "function '" ++ show (hname $ head matchfunheaders) ++ "'"

data ClassDecl = Class {
  cmeta       :: Meta ClassDecl,
  cname       :: Type,
  ccomposition :: Maybe TraitComposition,
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
isMainClass cdecl =
    let ty = cname cdecl
    in getId ty == "Main" && isActiveClassType ty


instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) =
      c {cmeta = AST.Meta.setType ty cmeta, cname = ty}
    showWithKind Class{cname} = "class '" ++ getId cname ++ "'"

data Requirement =
    RequiredField {
      rfield :: FieldDecl
    }
  | RequiredMethod {
      rheader :: FunctionHeader
    } deriving(Show)

isRequiredField RequiredField{} = True
isRequiredField _ = False

isRequiredMethod RequiredMethod{} = True
isRequiredMethod _ = False

instance Eq Requirement where
    a == b
        | isRequiredField a
        , isRequiredField b =
            rfield a == rfield b
        | isRequiredMethod a
        , isRequiredMethod b =
            rheader a == rheader b
        | otherwise = False

data TraitDecl = Trait {
  tmeta :: Meta TraitDecl,
  tname :: Type,
  treqs :: [Requirement],
  tmethods :: [MethodDecl]
} deriving (Show)

requiredFields :: TraitDecl -> [FieldDecl]
requiredFields Trait{treqs} =
    map rfield $ filter isRequiredField treqs

requiredMethods :: TraitDecl -> [FunctionHeader]
requiredMethods Trait{treqs} =
    map rheader $ filter isRequiredMethod treqs

traitInterface :: TraitDecl -> [FunctionHeader]
traitInterface t@Trait{tmethods} =
    requiredMethods t ++ map mheader tmethods

instance Eq TraitDecl where
  a == b = getId (tname a) == getId (tname b)

instance HasMeta TraitDecl where
  getMeta = tmeta
  setMeta t m = t{tmeta = m}
  setType ty t@Trait{tmeta, tname} =
    t{tmeta = AST.Meta.setType ty tmeta, tname = ty}
  showWithKind Trait{tname} = "trait '" ++ getId tname ++ "'"

-- | A @TraitComposition@ is the (possibly extended) capability of
-- a class, i.e. what is after the colon in @class C : T(f) + S@.
-- It is not a type per se (@T(f)@ is not allowed anywhere else),
-- but it can be used to obtain a capability type without the
-- extensions.
data TraitComposition =
    Conjunction{tcleft  :: TraitComposition
               ,tcright :: TraitComposition
               }
  | Disjunction{tcleft  :: TraitComposition
               ,tcright :: TraitComposition
               }
  | TraitLeaf{tcname :: Type
             ,tcext  :: [TraitExtension]
             } deriving(Show, Eq)

data TraitExtension =
    FieldExtension{extname :: Name}
  | MethodExtension{extname :: Name}
    deriving(Show, Eq)

type ExtendedTrait = (Type, [TraitExtension])

-- | Partitions a list of trait extensions into its field and
-- method extensions.
partitionTraitExtensions :: [TraitExtension] -> ([Name], [Name])
partitionTraitExtensions l =
  let (fields, methods) = partition isField l
  in (map extname fields, map extname methods)
  where
    isField FieldExtension{} = True
    isField _ = False

-- | Turns the included traits of a class (if any) into a
-- capability type.
capabilityFromTraitComposition :: Maybe TraitComposition -> Type
capabilityFromTraitComposition (Just Conjunction{tcleft, tcright}) =
    capabilityFromTraitComposition (Just tcleft) `conjunctiveType`
    capabilityFromTraitComposition (Just tcright)
capabilityFromTraitComposition (Just Disjunction{tcleft, tcright}) =
    capabilityFromTraitComposition (Just tcleft) `disjunctiveType`
    capabilityFromTraitComposition (Just tcright)
capabilityFromTraitComposition (Just TraitLeaf{tcname}) = tcname
capabilityFromTraitComposition Nothing = incapability

-- | Takes the included traits of a class (if any) and returns
-- a list of these traits without their extensions
typesFromTraitComposition :: Maybe TraitComposition -> [Type]
typesFromTraitComposition =
  typesFromCapability . capabilityFromTraitComposition

-- | Takes the included traits of a class (if any) and returns a
-- list of these traits together with their extensions. For
-- example, calling this function on the capability of
-- @
-- class C : T(f) + S
-- @
-- returns the list @[(T, [f]), (S, [])]@.
extendedTraitsFromComposition :: Maybe TraitComposition -> [ExtendedTrait]
extendedTraitsFromComposition (Just TraitLeaf{tcname, tcext}) =
  [(tcname, tcext)]
extendedTraitsFromComposition (Just tc) =
  extendedTraitsFromComposition (Just $ tcleft tc) ++
  extendedTraitsFromComposition (Just $ tcright tc)
extendedTraitsFromComposition Nothing = []

-- | Takes the included traits of a class (if any) and returns a
-- list of tuples @(ts, ts')@ where @ts@ and @ts'@ are the
-- (extended) traits that respectively appear to the left and
-- right of a @*@ in the composition.
conjunctiveTypesFromComposition ::
  Maybe TraitComposition -> [([ExtendedTrait], [ExtendedTrait])]
conjunctiveTypesFromComposition (Just Conjunction{tcleft, tcright}) =
  (extendedTraitsFromComposition (Just tcleft)
  ,extendedTraitsFromComposition (Just tcright)):
  conjunctiveTypesFromComposition (Just tcleft) ++
  conjunctiveTypesFromComposition (Just tcright)
conjunctiveTypesFromComposition (Just Disjunction{tcleft, tcright}) =
  conjunctiveTypesFromComposition (Just tcleft) ++
  conjunctiveTypesFromComposition (Just tcright)
conjunctiveTypesFromComposition _ = []

-- | @translateCompositionNamespace table@ gives the included
-- traits of a class (if any) the namespace specified by @table@.
translateCompositionNamespace ::
  Map SourceName Namespace -> Maybe TraitComposition -> Maybe TraitComposition
translateCompositionNamespace table Nothing = Nothing
translateCompositionNamespace table (Just tc@TraitLeaf{tcname}) =
  let source = getRefSourceFile tcname
      ns = table Map.! source
  in Just tc{tcname = setRefNamespace ns tcname}
translateCompositionNamespace table (Just tc) =
  let Just tcleft' = translateCompositionNamespace table (Just $ tcleft tc)
      Just tcright' = translateCompositionNamespace table (Just $ tcright tc)
  in Just tc{tcleft = tcleft', tcright = tcright'}

data Modifier = MVal
                deriving(Eq)

instance Show Modifier where
    show MVal = "val"

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
isValField = (MVal `elem`) . fmods

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

data MethodDecl =
    Method {
      mmeta   :: Meta MethodDecl,
      mheader :: FunctionHeader,
      mbody   :: Expr}
  | MatchingMethod {
      mmeta    :: Meta MethodDecl,
      mheaders :: [FunctionHeader],
      mbodies  :: [Expr]
    } deriving (Show)

methodName = hname . mheader
methodParams = hparams . mheader
methodType = htype . mheader

isStreamMethod Method{mheader} = isStreamMethodHeader mheader

isMainMethod :: Type -> Name -> Bool
isMainMethod ty name = isMainType ty && (name == Name "main")

isConstructor :: MethodDecl -> Bool
isConstructor m = methodName m == Name "_init"

emptyConstructor :: ClassDecl -> MethodDecl
emptyConstructor cdecl =
    let pos = AST.AST.getPos cdecl
    in Method{mmeta = meta pos
             ,mheader = Header{kind = NonStreaming
                              ,htypeparams = []
                              ,hname = Name "_init"
                              ,hparams = []
                              ,htype = voidType
                              }
             ,mbody = Skip (meta pos)}

replaceHeaderTypes :: [(Type, Type)] -> FunctionHeader -> FunctionHeader
replaceHeaderTypes bindings header =
    let hparams' = map (replaceParamType bindings) (hparams header)
        htype' = replaceTypeVars bindings (htype header)
    in
      header{hparams = hparams', htype = htype'}
    where
      replaceParamType bindings p@Param{ptype} =
          p{ptype = replaceTypeVars bindings ptype}

instance Eq MethodDecl where
  a == b = methodName a == methodName b

instance HasMeta MethodDecl where
  getMeta = mmeta
  setMeta mtd m = mtd{mmeta = m}
  setType ty m =
      let header = mheader m
          meta = mmeta m
      in
        m{mmeta = AST.Meta.setType ty meta
         ,mheader = setHeaderType ty header}
  showWithKind m
      | isStreamMethod m = "streaming method '" ++ show (methodName m) ++ "'"
      | otherwise = "method '" ++ show (methodName m) ++ "'"

data MatchClause =
    MatchClause {
      mcpattern :: Expr,
      mchandler :: Expr,
      mcguard   :: Expr
    } deriving (Show, Eq)

type Arguments = [Expr]

data MaybeContainer = JustData { e :: Expr}
                    | NothingData deriving(Eq, Show)

data Mutability = Var
                | Val deriving(Eq, Show)

data Expr = Skip {emeta :: Meta Expr}
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
                          typeArguments :: Maybe [Type],
                          qname :: QualifiedName,
                          args :: Arguments}
          | FunctionAsValue {emeta :: Meta Expr,
                             typeArgs :: [Type],
                             qname :: QualifiedName}
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
          | PartyEach {emeta :: Meta Expr,
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
                 mutability :: Mutability,
                 decls :: [(Name, Expr)],
                 body :: Expr}
          | MiniLet {emeta :: Meta Expr,
                     mutability :: Mutability,
                     decl :: (Name, Expr)}
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
                       qname :: QualifiedName}
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
          | UIntLiteral {emeta :: Meta Expr,
                         intLit :: Int}
          | RealLiteral {emeta :: Meta Expr,
                         realLit :: Double}
          | Embed {emeta :: Meta Expr,
                   ty    :: Type,
                   embedded :: [(String, Expr)]}
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
isThisAccess VarAccess {qname = QName{qnlocal}} = qnlocal == Name "this"
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

isNullLiteral :: Expr -> Bool
isNullLiteral Null{} = True
isNullLiteral TypedExpr{body} = isNullLiteral body
isNullLiteral _ = False

isPrimitiveLiteral :: Expr -> Bool
isPrimitiveLiteral Skip{}          = True
isPrimitiveLiteral BTrue{}         = True
isPrimitiveLiteral BFalse{}        = True
isPrimitiveLiteral StringLiteral{} = True
isPrimitiveLiteral NewWithInit{ty} = isStringObjectType ty
isPrimitiveLiteral CharLiteral{}   = True
isPrimitiveLiteral IntLiteral{}    = True
isPrimitiveLiteral RealLiteral{}   = True
isPrimitiveLiteral Unary{uop = NEG, operand} = isPrimitiveLiteral operand
isPrimitiveLiteral _ = False

isPattern :: Expr -> Bool
isPattern TypedExpr{body} = isPattern body
isPattern FunctionCall{} = True
isPattern MaybeValue{mdt = JustData{e}} = isPattern e
isPattern MaybeValue{mdt = NothingData} = True
isPattern Tuple{args} = all isPattern args
isPattern VarAccess{} = True
isPattern Null{} = True
isPattern e
    | isPrimitiveLiteral e = True
    | otherwise = False

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

getTrait :: Type -> Program -> TraitDecl
getTrait t p =
  let
    match t trait = getId t == getId (tname trait)
  in
    fromJust $ find (match t) (traits p)

allEmbedded = map etlheader . etl
