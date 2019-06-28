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
import Text.Megaparsec(SourcePos)

import Identifiers
import Types
import AST.Meta as Meta hiding(Closure, Async)


data FileDescriptor = Stdout | Stderr
  deriving (Show, Eq)

data Program = Program {
  source :: FilePath,
  moduledecl :: ModuleDecl,
  etl :: [EmbedTL],
  imports :: [ImportDecl],
  typedefs :: [Typedef],
  functions :: [Function],
  traits :: [TraitDecl],
  classes :: [ClassDecl],
  adts :: [AdtDecl],
  adtCases :: [AdtCase]
} deriving (Show)

setProgramSource source p = p{source}

class Show a => HasMeta a where
    getMeta :: a -> Meta a

    setMeta :: a -> Meta a -> a

    getPos :: a -> Position
    getPos = Meta.getPos . getMeta

    setEndPos :: SourcePos -> a -> a
    setEndPos end x =
      let oldMeta = getMeta x
          newMeta = Meta.setEndPos end oldMeta
      in setMeta x newMeta

    getType :: a -> Type
    getType = Meta.getType . getMeta

    setType :: Type -> a -> a

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

    makePattern :: a -> a
    makePattern x = let meta = getMeta x
                    in setMeta x $ Meta.makePattern meta

    isPattern :: a -> Bool
    isPattern = Meta.isPattern . getMeta

data EmbedTL = EmbedTL {
      etlmeta   :: Meta EmbedTL,
      etlheader :: String,
      etlbody   :: String
    } deriving (Show)

instance HasMeta EmbedTL where
    getMeta = etlmeta

    setMeta etl etlmeta = etl{etlmeta}

    setType ty i =
        error "AST.hs: Cannot set the type of a EmbedTL"

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
      isource :: Maybe FilePath
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

data Modifier = ModPrivate
              | ModMatch deriving (Eq, Ord)

instance Show Modifier where
  show ModPrivate = "private"
  show ModMatch   = "match"

data FunctionHeader =
    Header {
        hmodifiers  :: [Modifier],
        kind        :: HeaderKind,
        htypeparams :: [Type],
        hname       :: Name,
        htype       :: Type,
        hparams     :: [ParamDecl]
    } deriving(Eq, Show)

simpleHeader hname hparams htype =
  Header{hmodifiers = []
        ,kind = NonStreaming
        ,htypeparams = []
        ,hname
        ,hparams
        ,htype
        }

setHeaderType ty h = h{htype = ty}

setHeaderModifier :: [Modifier] -> FunctionHeader -> FunctionHeader
setHeaderModifier mod h = h {hmodifiers = nub mod}

isPrivateMethodHeader :: FunctionHeader -> Bool
isPrivateMethodHeader Header{hmodifiers} = ModPrivate `elem` hmodifiers

isMatchMethodHeader :: FunctionHeader -> Bool
isMatchMethodHeader Header{hmodifiers} = ModMatch `elem` hmodifiers

isMatchMethod :: MethodDecl -> Bool
isMatchMethod = isMatchMethodHeader . mheader

isStreamMethodHeader h = kind h == Streaming

data Function =
    Function {
      funmeta   :: Meta Function,
      funheader :: FunctionHeader,
      funbody   :: Expr,
      funlocals :: [Function],
      funsource :: FilePath
    } deriving (Show)

functionName = hname . funheader
functionParams = hparams . funheader
functionTypeParams = htypeparams . funheader
functionType = htype . funheader

setFunctionName name fun@Function{funheader} =
  fun{funheader = funheader{hname = name}}

instance Eq Function where
  a == b = (hname . funheader $ a) == (hname . funheader $ b)

instance HasMeta Function where
  getMeta = funmeta
  setMeta f m = f{funmeta = m}
  setType ty f@(Function {funmeta, funheader}) =
      f{funmeta = Meta.setType ty funmeta
       ,funheader = setHeaderType ty funheader}
  showWithKind Function{funheader} =
      "function '" ++ show (hname funheader) ++ "'"

data ClassDecl = Class {
  cmeta       :: Meta ClassDecl,
  cname       :: Type,
  ccomposition :: Maybe TraitComposition,
  cfields     :: [FieldDecl],
  cmethods    :: [MethodDecl]
} deriving (Show)

data AdtDecl = ADT {
  ameta        :: Meta AdtDecl,
  aname        :: Type,
  amethods     :: [MethodDecl]
} deriving (Show)

data AdtCase = ADTCase {
  acmeta         :: Meta AdtCase,
  acname         :: Type,
  acfields       :: [ParamDecl],
  acparent       :: TraitComposition,
  acmethods      :: [MethodDecl]
} deriving (Show)

instance Eq ClassDecl where
  a == b = getId (cname a) == getId (cname b)

isActive :: ClassDecl -> Bool
isActive Class{cname, ccomposition} =
  isActiveSingleType cname ||
  isModeless cname &&
  all isActiveSingleType (typesFromTraitComposition ccomposition)

isPassive :: ClassDecl -> Bool
isPassive cls = not (isActive cls) && not (isShared cls)

isShared :: ClassDecl -> Bool
isShared = isSharedSingleType . cname

isMainClass :: ClassDecl -> Bool
isMainClass cdecl =
    let ty = cname cdecl
    in getId ty == "Main" && isActive cdecl

instance HasMeta ClassDecl where
    getMeta = cmeta
    setMeta c m = c{cmeta = m}
    setType ty c@(Class {cmeta, cname}) =
      c {cmeta = Meta.setType ty cmeta, cname = ty}
    showWithKind Class{cname} = "class '" ++ getId cname ++ "'"

instance HasMeta AdtDecl where
    getMeta = ameta
    setMeta a m = a{ameta = m}
    setType _ _ =
        error "AST.hs: Cannot set the type of an ADT"
    showWithKind ADT{aname} = "data '" ++ getId aname ++ "'"

instance HasMeta AdtCase where
    getMeta = acmeta
    setMeta ac m = ac{acmeta = m}
    setType _ _ =
        error "AST.hs: Cannot set the type of an ADT"
    showWithKind ADTCase{acname} = "case '" ++ getId acname ++ "'"

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
    t{tmeta = Meta.setType ty tmeta, tname = ty}
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
  Map FilePath Namespace -> Maybe TraitComposition -> Maybe TraitComposition
translateCompositionNamespace table Nothing = Nothing
translateCompositionNamespace table (Just tc@TraitLeaf{tcname}) =
  let source = getRefSourceFile tcname
      ns = table Map.! source
  in Just tc{tcname = setRefNamespace ns tcname}
translateCompositionNamespace table (Just tc) =
  let Just tcleft' = translateCompositionNamespace table (Just $ tcleft tc)
      Just tcright' = translateCompositionNamespace table (Just $ tcright tc)
  in Just tc{tcleft = tcleft', tcright = tcright'}

-- | The inversion of 'capabilityFromTraitComposition'. The
-- resulting trait composition will have no extensions, meaning that
-- @c == traitCompositionFromCapability (capabilityFromTraitComposition c)@
-- will hold if and only if @c@ has no extensions.
traitCompositionFromCapability :: Type -> TraitComposition
traitCompositionFromCapability cap
  | isConjunctiveType cap =
      let (left, right) = getTypeOperands cap
      in Conjunction{tcleft = traitCompositionFromCapability left
                    ,tcright = traitCompositionFromCapability right}
  | isDisjunctiveType cap =
      let (left, right) = getTypeOperands cap
      in Disjunction{tcleft = traitCompositionFromCapability left
                    ,tcright = traitCompositionFromCapability right}
  | otherwise = TraitLeaf{tcname = cap, tcext = []}

data FieldDecl = Field {
  fmeta :: Meta FieldDecl,
  fmut  :: Mutability,
  fname :: Name,
  ftype :: Type,
  fexpr :: Maybe Expr
}

instance Show FieldDecl where
  show f@Field{fmut, fname, ftype} =
      show fmut ++ " " ++ show fname ++ " : " ++ show ftype

instance Eq FieldDecl where
  a == b = fname a == fname b

instance HasMeta FieldDecl where
    getMeta = fmeta
    setMeta f m = f{fmeta = m}
    setType ty f@(Field {fmeta, ftype}) = f {fmeta = Meta.setType ty fmeta, ftype = ty}
    showWithKind Field{fname} = "field '" ++ show fname ++ "'"

isValField :: FieldDecl -> Bool
isValField = (== Val) . fmut

isVarField :: FieldDecl -> Bool
isVarField = (== Var) . fmut

data ParamDecl = Param {
  pmeta :: Meta ParamDecl,
  pmut  :: Mutability,
  pname :: Name,
  ptype :: Type,
  pdefault :: Maybe Expr
} deriving (Show, Eq)

instance HasMeta ParamDecl where
    getMeta = pmeta
    setMeta p m = p{pmeta = m}
    setType ty p@(Param {pmeta, ptype}) = p {pmeta = Meta.setType ty pmeta, ptype = ty}
    showWithKind Param{pname} = "parameter '" ++ show pname ++ "'"

data MethodDecl =
    Method {
      mmeta   :: Meta MethodDecl,
      mimplicit :: Bool,
      mheader :: FunctionHeader,
      mlocals :: [Function],
      mbody   :: Expr
    } deriving (Show)

methodName = hname . mheader
methodParams = hparams . mheader
methodTypeParams = htypeparams . mheader
methodType = htype . mheader

isStreamMethod Method{mheader} = isStreamMethodHeader mheader

isMainMethod :: Type -> Name -> Bool
isMainMethod ty name = isMainType ty && (name == Name "main")

isConstructor :: MethodDecl -> Bool
isConstructor m = methodName m == constructorName

isImplicitMethod = mimplicit

hasConstructor :: ClassDecl -> Bool
hasConstructor Class{cmethods} = filter isConstructor cmethods /= []

emptyConstructor :: ClassDecl -> MethodDecl
emptyConstructor cdecl =
    let pos = AST.AST.getPos cdecl
    in Method{mmeta = meta pos
             ,mimplicit = True
             ,mheader = simpleHeader constructorName [] unitType
             ,mbody = Skip (meta pos)
             ,mlocals = []}

replaceHeaderTypes :: [(Type, Type)] -> FunctionHeader -> FunctionHeader
replaceHeaderTypes bindings header =
    let hparams' = map (replaceParamType bindings) (hparams header)
        htype' = replaceTypeVars bindings (htype header)
    in
      header{hparams = hparams', htype = htype'}
    where
      replaceParamType bindings p@Param{ptype} =
          p{ptype = replaceTypeVars bindings ptype}

translateHeaderNamespace ::
  Map FilePath Namespace -> FunctionHeader -> FunctionHeader
translateHeaderNamespace table header =
    let hparams' = map (translateParamType table) (hparams header)
        htype' = translateTypeNamespace table (htype header)
    in
      header{hparams = hparams', htype = htype'}
    where
      translateParamType table p@Param{ptype} =
          p{ptype = translateTypeNamespace table ptype}

instance Eq MethodDecl where
  a == b = methodName a == methodName b

instance HasMeta MethodDecl where
  getMeta = mmeta
  setMeta mtd m = mtd{mmeta = m}
  setType ty m =
      let header = mheader m
          meta = mmeta m
      in
        m{mmeta = Meta.setType ty meta
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
                | Val deriving(Eq)

instance Show Mutability where
    show Var = "var"
    show Val = "val"

data OptionalPathComponent = QuestionDot Expr | QuestionBang Expr deriving (Show, Eq)

data VarDecl =
    VarNoType {varName :: Name}
  | VarType {varName :: Name,
             varType :: Type}
  deriving(Eq, Show)

data ForSource =
     ForSource { fsName :: Name,
                 fsTy :: Maybe Type,
                 collection :: Expr}
     deriving(Eq, Show)

data Expr = Skip {emeta :: Meta Expr}
          | Break {emeta :: Meta Expr}
          | Continue {emeta :: Meta Expr}
          | TypedExpr {emeta :: Meta Expr,
                       body :: Expr,
                       ty   :: Type}
          | MethodCall {emeta :: Meta Expr,
                        typeArguments :: [Type],
                        target :: Expr,
                        name :: Name,
                        args :: Arguments}
          | MessageSend {emeta :: Meta Expr,
                         typeArguments :: [Type],
                         target :: Expr,
                         name :: Name,
                         args :: Arguments}
          | Optional {emeta :: Meta Expr,
                      optTag :: OptionalPathComponent}
          | AdtExtractorPattern {emeta :: Meta Expr,
                                 name :: Name,
                                 arg :: Expr,
                                 adtClassDecl :: ClassDecl}
          | ExtractorPattern {emeta :: Meta Expr,
                              name :: Name,
                              arg :: Expr}
          | FunctionCall {emeta :: Meta Expr,
                          typeArguments :: [Type],
                          qname :: QualifiedName,
                          args :: Arguments}
          | FunctionAsValue {emeta :: Meta Expr,
                             typeArgs :: [Type],
                             qname :: QualifiedName}
          | Closure {emeta :: Meta Expr,
                     eparams :: [ParamDecl],
                     mty :: Maybe Type,
                     body :: Expr}
          | PartySeq {emeta :: Meta Expr,
                      par :: Expr,
                      seqfunc :: Expr}
          | PartyPar {emeta :: Meta Expr,
                      parl :: Expr,
                      parr :: Expr}
          | PartyReduce {emeta    :: Meta Expr,
                         seqfun   :: Expr,
                         pinit    :: Expr,
                         par      :: Expr,
                         runassoc :: Bool}
          | Async {emeta :: Meta Expr,
                   body :: Expr}
          | Return {emeta :: Meta Expr,
                    val :: Expr}
          | MaybeValue {emeta :: Meta Expr,
                        mdt :: MaybeContainer }
          | Tuple {emeta :: Meta Expr,
                   args :: [Expr]}
          | Let {emeta :: Meta Expr,
                 mutability :: Mutability,
                 decls :: [([VarDecl], Expr)],
                 body :: Expr}
          | MiniLet {emeta :: Meta Expr,
                     mutability :: Mutability,
                     decl :: ([VarDecl], Expr)}
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
          | DoWhile {emeta :: Meta Expr,
                     cond :: Expr,
                     body :: Expr}
          | Repeat {emeta :: Meta Expr,
                    name :: Name,
                    times :: Expr,
                    body :: Expr}
          | For {emeta  :: Meta Expr,
                sources :: [ForSource],
                  body   :: Expr}
          | Match {emeta :: Meta Expr,
                   arg :: Expr,
                   clauses :: [MatchClause]}
          | Borrow {emeta  :: Meta Expr,
                    target :: Expr,
                    name   :: Name,
                    body   :: Expr}
          | Get {emeta :: Meta Expr,
                 val :: Expr}
          | Forward {emeta :: Meta Expr,
                     forwardExpr :: Expr}
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
          | TupleAccess {emeta :: Meta Expr,
                         target :: Expr,
                         compartment :: Int}
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
          | Print {emeta :: Meta Expr,
                   file :: FileDescriptor,
                   args :: [Expr]}
          | Exit {emeta :: Meta Expr,
                  args :: [Expr]}
          | Abort {emeta :: Meta Expr,
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

isMethodCallOrMessageSend e = isMethodCall e || isMessageSend e

isMethodCall :: Expr -> Bool
isMethodCall MethodCall {} = True
isMethodCall _ = False

isMessageSend :: Expr -> Bool
isMessageSend MessageSend {} = True
isMessageSend _ = False

isFunctionCall :: Expr -> Bool
isFunctionCall FunctionCall {} = True
isFunctionCall _ = False

isThisAccess :: Expr -> Bool
isThisAccess VarAccess {qname = QName{qnlocal}} = qnlocal == Name "this"
isThisAccess _ = False

isIdClos :: Expr -> Bool
isIdClos VarAccess{qname = QName{qnlocal}} = qnlocal == Name "_id_fun_tmp"
isIdClos _ = False

isClosure :: Expr -> Bool
isClosure Closure {} = True
isClosure _ = False

isIdClosure :: Expr -> Bool
isIdClosure VarAccess{qname = QName{qnlocal}} = qnlocal == Name "_id_fun_tmp"
isIdClosure _ = False

isForward :: Expr -> Bool
isForward Forward {} = True
isForward _ = False

isAssign :: Expr -> Bool
isAssign Assign {} = True
isAssign _ = False

isVarAccess :: Expr -> Bool
isVarAccess VarAccess{} = True
isVarAccess _ = False

isNull :: Expr -> Bool
isNull Null{} = True
isNull _ = False

isTask :: Expr -> Bool
isTask Async {} = True
isTask _ = False

isBreak :: Expr -> Bool
isBreak Break{} = True
isBreak _ = False

isFor :: Expr -> Bool
isFor For{} = True
isFor _ = False

isRangeLiteral :: Expr -> Bool
isRangeLiteral RangeLiteral {} = True
isRangeLiteral _ = False

isCallable :: Expr -> Bool
isCallable e = isArrowType (AST.AST.getType e)

isStringLiteral :: Expr -> Bool
isStringLiteral StringLiteral {} = True
isStringLiteral _ = False

isReturn :: Expr -> Bool
isReturn Return{} = True
isReturn _ = False

isArrayLiteral :: Expr -> Bool
isArrayLiteral ArrayLiteral{} = True
isArrayLiteral _ = False

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

isValidPattern :: Expr -> Bool
isValidPattern TypedExpr{body} = isValidPattern body
isValidPattern FunctionCall{} =  True
isValidPattern MaybeValue{mdt = JustData{e}} =   isValidPattern e
isValidPattern MaybeValue{mdt = NothingData} =    True
isValidPattern Tuple{args} =  all isValidPattern args
isValidPattern VarAccess{} =  True
isValidPattern Null{} =  True
isValidPattern ExtractorPattern{} =  True
isValidPattern AdtExtractorPattern{} =  True
isValidPattern e
    | isPrimitiveLiteral e =  True
    | otherwise = False

isExtractorPattern :: Expr -> Bool
isExtractorPattern ExtractorPattern{} = True
isExtractorPattern _ = False

isImpure :: Expr -> Bool
isImpure MethodCall {} = True
isImpure MessageSend {} = True
isImpure FunctionCall {} = True
isImpure While {} = True
isImpure StreamNext {} = True
isImpure Get {} = True
isImpure Await {} = True
isImpure Suspend {} = True
isImpure Assign {} = True
isImpure NewWithInit {} = True
isImpure New {} = True
isImpure Match {} = True
isImpure _ = False

hasBody :: Expr -> Bool
hasBody Closure {} = True
hasBody Async {} = True
hasBody Let {} = True
hasBody IfThenElse {} = True
hasBody IfThen {} = True
hasBody Unless {} = True
hasBody While {} = True
hasBody DoWhile {} = True
hasBody Repeat {} = True
hasBody For {} = True
hasBody Match {} = True
hasBody _ = False

findRoot :: Expr -> Expr
findRoot FieldAccess{target} = findRoot target
findRoot MethodCall{target} = findRoot target
findRoot MessageSend{target} = findRoot target
findRoot TupleAccess{target} = findRoot target
findRoot e = e

instance HasMeta Expr where
    getMeta = emeta
    setMeta e m = e{emeta = m}

    setType ty expr = expr {emeta = Meta.setType ty (emeta expr)}

setSugared :: Expr -> Expr -> Expr
setSugared e sugared = e {emeta = Meta.setSugared sugared (emeta e)}

getSugared :: Expr -> Maybe Expr
getSugared e = Meta.getSugared (emeta e)

getTrait :: Type -> Program -> TraitDecl
getTrait t p =
  let
    match t trait = getId t == getId (tname trait)
  in
    fromJust $ find (match t) (traits p)

allEmbedded = map etlheader . etl
