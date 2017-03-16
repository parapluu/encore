{-# LANGUAGE TupleSections #-}

{-|

Produces an "AST.AST" (or an error) of a @Program@ built from the
grammar found in @doc/encore/@

-}

module Parser.Parser(parseEncoreProgram) where

-- Library dependencies
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Expr
import Data.List((\\))
import Data.Char(isUpper)
import Data.Maybe(fromMaybe, isJust, fromJust)
import Control.Monad(void, foldM, unless, when, liftM)
import Control.Monad.Reader hiding(guard)
import Control.Applicative ((<$>), empty)
import Control.Arrow (first)

import Debug.Trace
import AST.PrettyPrinter

-- Module dependencies
import Identifiers hiding(namespace)
import Types hiding(refType)
import AST.AST
import AST.Meta hiding(Closure, Async, getPos)

-- | 'parseEncoreProgram' @path@ @code@ assumes @path@ is the path
-- to the file being parsed and will produce an AST for @code@,
-- unless a parse error occurs.
parseEncoreProgram :: FilePath -> String -> Either (ParseError Char Dec) Program
parseEncoreProgram = parse (runReaderT program sc)

lineComment = L.skipLineComment "--"
blockComment = L.skipBlockCommentNested "{-" "-}"

-- | A "space consumer", used for parsing non-linebreaking white-space.
sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment blockComment

-- | A "space consumer", used for parsing white-space, including line breaks.
scn :: Parser ()
scn = L.space (void spaceChar) lineComment blockComment

-- | The Parser monad wrapped in a Reader that carries the current
-- space consumer (if newlines may be consumed or not)
type EncParser = ReaderT (Parser ()) Parser
currentSpaceConsumer = lift <$> ask

-- | The 'EncParser' equivalent of 'sc'
hspace :: EncParser ()
hspace = lift sc

-- | The 'EncParser' equivalent of 'scn'
vspace :: EncParser ()
vspace = lift scn

-- | Parse a section of code allowing line breaks
withLinebreaks :: EncParser a -> EncParser a
withLinebreaks = local (const scn)

-- | Parse a section of code disallowing line breaks
withoutLinebreaks :: EncParser a -> EncParser a
withoutLinebreaks = local (const sc)

-- | Parse a newline and any whitespace following it (including
-- additional newlines)
nl :: EncParser ()
nl = newline >> vspace

lexeme :: EncParser a -> EncParser a
lexeme p = do
  sc <- currentSpaceConsumer
  L.lexeme sc p

symbol :: String -> EncParser String
symbol s = do
  sc <- currentSpaceConsumer
  L.symbol sc s

charLiteral :: EncParser Char
charLiteral = do
  sc <- currentSpaceConsumer
  char '\'' *> L.charLiteral <* char '\'' <* sc

stringLiteral :: EncParser String
stringLiteral = do
  sc <- currentSpaceConsumer
  char '"' >> manyTill L.charLiteral (char '"') <* sc

float :: EncParser Double
float = lexeme L.float

-- | @atLevel ind p@ parses @p@ if the current indentation level
-- is @ind@, and fails otherwise. Note that only the start of @p@
-- is checked; if @p@ can consume newlines, subsequent lines are
-- not controlled.
atLevel :: Pos -> EncParser a -> EncParser a
atLevel expectedIndent p = do
  currentIndent <- L.indentLevel
  unless (currentIndent == expectedIndent) $
         L.incorrectIndent Prelude.EQ expectedIndent currentIndent
  p

-- | @indented ind p@ parses @p@ if the current level is greater
-- than @ind@, and fails otherwise. Note that only the start of
-- @p@ is checked; if @p@ can consume newlines, subsequent lines
-- are not controlled.
indented :: Pos -> EncParser a -> EncParser a
indented refIndent p = do
  currentIndent <- L.indentLevel
  unless (currentIndent > refIndent) $
         L.incorrectIndent Prelude.GT refIndent currentIndent
  p

-- | @atLeast ind p@ parses @p@ if the current level is greater or
-- equal to @ind@, and fails otherwise. Note that only the start
-- of @p@ is checked; if @p@ can consume newlines, subsequent
-- lines are not controlled.
atLeast :: Pos -> EncParser a -> EncParser a
atLeast refIndent = indented (unsafePos $ unPos refIndent - 1)

-- | Parse a token in column one
nonIndented :: EncParser a -> EncParser a
nonIndented = L.nonIndented hspace

-- | See 'L.indentBlock'. Used to parse indented blocks of code.
indentBlock = L.indentBlock vspace

-- | See 'L.lineFold'. Used to fold lines, i.e. allow linebreaks
-- but require some indentation
lineFold :: (EncParser () -> EncParser a) -> EncParser a
lineFold = L.lineFold vspace

-- | @folded delimiters fold p@ parses @delimiters p@ folded with
-- the space consumer @fold@. Inteded for use inside a @lineFold@.
folded :: (EncParser a -> EncParser a) -> EncParser () -> EncParser a -> EncParser a
folded delimiters fold p =
  delimiters (fold >> foldWith fold p)
  where
    foldWith fold = local (const $ runReaderT fold undefined)

alignedExpressions :: ([Expr] -> EncParser a) -> EncParser (L.IndentOpt EncParser a Expr)
alignedExpressions f =
  return $ L.IndentSome Nothing f exprBreak
  where
    exprBreak = do
      e <- expression
      try $ lookAhead nl
      return e

-- | @parseBody c@ is inteded for use in the end of an
-- `indentBlock` construct. It parses the body of a loop or
-- conditional, which is a list of indented expressions @c@ is a
-- function that takes the loop body as the argument and builds a
-- (possibly partial) expression.
parseBody :: (Expr -> a) -> EncParser (L.IndentOpt EncParser a Expr)
parseBody constructor =
  alignedExpressions (return . constructor . makeBody)

-- | @blockedConstruct p@ parses a construct whose header is
-- parsed by @p@, and whose body is a block ended by "end".
blockedConstruct header = do
  indent <- L.indentLevel
  block <- indentBlock $ do
    constructor <- header
    parseBody constructor
  atLevel indent $ reserved "end"
  return block

-- | These parsers use the lexer above and are the smallest
-- building blocks of the whole parser.
reservedNames =
    ["EMBED"
    ,"END"
    ,"Fut"
    ,"Maybe"
    ,"Par"
    ,"Stream"
    ,"and"
    ,"bool"
    ,"break"
    ,"case"
    ,"char"
    ,"class"
    ,"continue"
    ,"def"
    ,"do"
    ,"else"
    ,"end"
    ,"eos"
    ,"false"
    ,"for"
    ,"fun"
    ,"forward"
    ,"if"
    ,"import"
    ,"in"
    ,"int"
    ,"let"
    ,"match"
    ,"module"
    ,"new"
    ,"not"
    ,"null"
    ,"or"
    ,"passive"
    ,"qualified"
    ,"real"
    ,"repeat"
    ,"return"
    ,"require"
    ,"shared"
    ,"stream"
    ,"then"
    ,"this"
    ,"trait"
    ,"true"
    ,"typedef"
    ,"uint"
    ,"unless"
    ,"val"
    ,"var"
    ,"unit"
    ,"when"
    ,"where"
    ,"while"
    ,"with"
    ,"yield"
   ]

reservedOps =
    ["="
    ,"=="
    ,"!="
    ,"+="
    ,"-="
    ,"*="
    ,"/="
    ,"<="
    ,">="
    ,"<"
    ,">"
    ,"=>"
    ,"~~>"
    ,"||"
    ,"&&"
    ,"|||"
    ,">>"
    ,"?!"
    ,"?."
    ,"+"
    ,"-"
    ,"*"
    ,"/"
    ,"%"
    ,":"
    ,"."
    ,"!"
    ]

validIdentifierChar :: EncParser Char
validIdentifierChar = alphaNumChar <|> char '_' <|> char '\''

validOpChar :: EncParser Char
validOpChar = oneOf ".:=!<>+-*/%~|"

reserved :: String -> EncParser ()
reserved w = do
  sc <- currentSpaceConsumer
  try $ string w *> notFollowedBy validIdentifierChar *> sc

reservedOp :: String -> EncParser ()
reservedOp op = do
  sc <- currentSpaceConsumer
  let otherOps = foldr1 (<|>) (map string $ reservedOps \\ [op])
  notFollowedBy (otherOps >> notFollowedBy validOpChar)
  try $ string op *> sc

identifier :: EncParser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many validIdentifierChar
    check x = if x `elem` reservedNames
              then fail $ "Reserved keyword " ++ show x ++
                          " cannot be used as an identifier"
              else return x

dot        = symbol "."
bang       = symbol "!"
bar        = symbol "|"
dotdot     = symbol ".."
colon      = symbol ":"
semi       = symbol ";"
comma      = symbol ","
commaSep   = (`sepBy` comma)
commaSep1  = (`sepBy1` comma)
semiSep    = (`sepBy` semi)
parens     = between (symbol "(") (symbol ")")
brackets   = between (symbol "[") (symbol "]")
braces     = between (symbol "{") (symbol "}")
maybeBraces p = braces p <|> p

encoreEscapeStart = symbol "#{"
encoreEscapeEnd = symbol "}"
encoreEscaped p = do
  encoreEscapeStart
  x <- p
  encoreEscapeEnd
  return x

modulePath :: EncParser [Name]
modulePath =
    (Name <$> (lookAhead upperChar >> identifier)) `sepBy1`
    try (dot >> lookAhead upperChar)

typ :: EncParser Type
typ = makeExprParser singleType opTable
    where
      opTable = [
                 [typeOp "*" conjunctiveType],
                 [typeOp "+" disjunctiveType],
                 [arrow]
                ]
      typeOp op constructor =
          InfixL (do withLinebreaks $ reservedOp op
                     return constructor)
      arrow =
          InfixR (do withLinebreaks $ reservedOp "->"
                     return (arrowType . unfoldArgs))
          where
            unfoldArgs ty
                | isTupleType ty = getArgTypes ty
                | otherwise = [ty]

      singleType =
            try tuple
        <|> array
        <|> embed
        <|> range
        <|> builtin
        <|> refType
        <|> primitive
        <|> typeVariable
        <|> parenthesized
        <?> "type"
      tuple = do
        types <- (reservedOp "()" >> return [])
             <|> parens (typ `sepBy2` comma)
        return $ tupleType types
      parenthesized = do
        ty <- parens typ
        (notFollowedBy (reservedOp "->") >> return ty)
         <|> return (tupleType [ty]) -- Allows for ((t, t')) -> t''
      array = do
        ty <- brackets typ
        return $ arrayType ty
      embed = do
        reserved "EMBED"
        ty <- manyTill anyChar $ try $ do {spaceChar; reserved "END"}
        return $ ctype ty
      range = do
        reserved "Range"
        return rangeType
      builtin = maybe <|> fut <|> par <|> stream
        where
          builtin' t r = liftM t (reserved r >> brackets typ)
          maybe  = builtin' maybeType "Maybe"
          fut    = builtin' futureType "Fut"
          par    = builtin' parType "Par"
          stream = builtin' streamType "Stream"
      refType = do
        full <- modulePath
        let ns = explicitNamespace $ init full
            refId = show $ last full
        parameters <- option [] $ brackets (commaSep1 typ)
        if isEmptyNamespace ns
        then return $ refTypeWithParams refId parameters
        else return $ setRefNamespace ns $
                      refTypeWithParams refId parameters
      primitive =
        do {reserved "int"; return intType} <|>
        do {reserved "uint"; return uintType} <|>
        do {reserved "bool"; return boolType} <|>
        do {reserved "string"; return stringType} <|>
        do {reserved "char"; return charType} <|>
        do {reserved "real"; return realType} <|>
        do {reserved "unit"; return unitType}

typeVariable :: EncParser Type
typeVariable = do
  notFollowedBy upperChar
  id <- identifier
  return $ typeVar id
  <?> "lower case type variable"

data ADecl = CDecl{cdecl :: ClassDecl} | TDecl{tdecl :: TraitDecl} | TDef{tdef :: Typedef} | FDecl{fdecl :: Function}

partitionDecls :: [ADecl] -> ([ClassDecl], [TraitDecl], [Typedef], [Function])
partitionDecls = partitionDecls' [] [] [] []
  where
    partitionDecls' cs ts tds fds [] = (cs, ts, tds, fds)
    partitionDecls' cs ts tds fds (CDecl{cdecl}:ds) = partitionDecls' (cdecl:cs) ts tds fds ds
    partitionDecls' cs ts tds fds (TDecl{tdecl}:ds) = partitionDecls' cs (tdecl:ts) tds fds ds
    partitionDecls' cs ts tds fds (TDef{tdef}:ds) = partitionDecls' cs ts (tdef:tds) fds ds
    partitionDecls' cs ts tds fds (FDecl{fdecl}:ds) = partitionDecls' cs ts tds (fdecl:fds) ds

program :: EncParser Program
program = do
  source <- sourceName <$> getPosition
  optional hashbang
  vspace

  moduledecl <- nonIndented moduleDecl
  unless (moduledecl == NoModule) nl

  imports <- nonIndented importdecl `endBy` nl
  etls <- nonIndented embedTL
  let etl = [etls]
  vspace

  decls <- (`sepEndBy` nl) . foldr1 (<|>) $
           map nonIndented
                   [CDecl <$> classDecl
                   ,TDecl <$> traitDecl
                   ,TDef <$> typedef
                   ,FDecl <$> globalFunction
                   ]
  let (classes, traits, typedefs, functions) = partitionDecls decls
  eof
  return Program{source
                ,moduledecl
                ,etl
                ,imports
                ,typedefs
                ,functions
                ,traits
                ,classes
                }
    where
      hashbang = do string "#!"
                    many (noneOf "\n\r")

moduleDecl :: EncParser ModuleDecl
moduleDecl = option NoModule $
  lineFold $ \sc' -> do
    modmeta <- meta <$> getPosition
    reserved "module"
    lookAhead upperChar
    modname <- Name <$> identifier
    modexports <- optional $
                  folded parens sc' ((Name <$> identifier) `sepEndBy` comma)
    return Module{modmeta
                 ,modname
                 ,modexports
                 }

importdecl :: EncParser ImportDecl
importdecl =
  lineFold $ \sc' -> do
    indent <- L.indentLevel
    imeta <- meta <$> getPosition
    reserved "import"
    iqualified <- option False $ reserved "qualified" >> return True
    itarget <- explicitNamespace <$> modulePath
    iselect <- optional $
               folded parens sc' ((Name <$> identifier) `sepEndBy` comma)
    ialias <- optional $ do
                try sc'
                reserved "as"
                explicitNamespace <$> modulePath
    ihiding <- optional $ do
                 try sc'
                 reserved "hiding"
                 folded parens sc' ((Name <$> identifier) `sepEndBy` comma)
    return Import{imeta
                 ,itarget
                 ,iqualified
                 ,iselect
                 ,ihiding
                 ,ialias
                 ,isource = Nothing
                 }

embedTL :: EncParser EmbedTL
embedTL = do
  -- TODO: Make sure BODY and END are not indented
  pos <- getPosition
  (try (do string "EMBED"
           header <- manyTill anyChar $ try $ do {spaceChar; string "BODY"}
           code <- manyTill anyChar $ try $ do {spaceChar; reserved "END"}
           return $ EmbedTL (meta pos) header code
       ) <|>
   try (do string "EMBED"
           header <- manyTill anyChar $ try $ do {spaceChar; reserved "END"}
           return $ EmbedTL (meta pos) header ""
       ) <|>
   (return $ EmbedTL (meta pos) "" ""))

optionalTypeParameters = option [] (brackets $ commaSep1 typ)

typedef :: EncParser Typedef
typedef = do
  typedefmeta <- meta <$> getPosition
  indent <- L.indentLevel
  reserved "typedef"
  name <- lookAhead upperChar >> identifier
  params <- optionalTypeParameters
  reservedOp "="
  typedeftype <- typ <|> (hidden nl >> indented indent typ)
  let typedefdef = setRefNamespace emptyNamespace $
                   typeSynonym name params typedeftype
  return Typedef{typedefmeta, typedefdef}

functionHeader :: EncParser FunctionHeader
functionHeader =
  lineFold $ \sc' -> do
    hname <- Name <$> identifier
    htypeparams <- optionalTypeParameters
    hparams <- folded parens sc' (commaSep paramDecl)
    colon
    htype <- typ
    return Header{hmodifiers = []
                 ,kind = NonStreaming
                 ,htypeparams
                 ,hname
                 ,hparams
                 ,htype
                 }

streamMethodHeader :: EncParser FunctionHeader
streamMethodHeader = do
  header <- functionHeader
  return header{kind = Streaming}

guard :: EncParser Expr
guard = do
  reserved "when"
  expression

whereClause :: EncParser [Function]
whereClause =
  indentBlock $ do
    reserved "where"
    return $ L.IndentSome Nothing return localFunction

localFunction :: EncParser Function
localFunction = do
  funIndent <- L.indentLevel
  fun <- funHeaderAndBody
  atLevel funIndent $ reserved "end"
  return fun

globalFunction :: EncParser Function
globalFunction = do
  funIndent <- L.indentLevel
  fun <- funHeaderAndBody

  funlocals <- option [] $ atLevel funIndent whereClause

  atLevel funIndent $ reserved "end"

  return fun{funlocals}

funHeaderAndBody =
  indentBlock $ do
    funmeta <- meta <$> getPosition
    reserved "fun"
    funheader <- functionHeader
    alignedExpressions (buildFun funmeta funheader)
  where
    buildFun funmeta funheader block =
      return Function{funmeta
                     ,funheader
                     ,funbody = makeBody block
                     ,funlocals = []
                     ,funsource = ""
                     }

makeBody :: [Expr] -> Expr
makeBody es =
  case es of
    [] -> error "Parser.hs: Cannot make body from empty list"
    [e] -> e
    es -> Seq (meta (getPos (last es))) es

data TraitAttribute = TReqAttribute {treq :: Requirement}
                    | TMethodAttribute {tmdecl :: MethodDecl}

partitionTraitAttributes :: [TraitAttribute] -> ([Requirement], [MethodDecl])
partitionTraitAttributes = partitionTraitAttributes' [] []
  where
    partitionTraitAttributes' rs ms [] = (rs, ms)
    partitionTraitAttributes' rs ms (TReqAttribute{treq}:as) =
      partitionTraitAttributes' (treq:rs) ms as
    partitionTraitAttributes' rs ms (TMethodAttribute{tmdecl}:as) =
      partitionTraitAttributes' rs (tmdecl:ms) as


traitDecl :: EncParser TraitDecl
traitDecl = do
  tIndent <- L.indentLevel
  tdecl <- indentBlock $ do
    tmeta <- meta <$> getPosition
    reserved "trait"
    ident <- lookAhead upperChar >> identifier
    params <- optionalTypeParameters
    return $ L.IndentMany
               Nothing
               (buildTrait tmeta ident params)
               traitAttribute
  -- TODO: tlocals <- option [] $ atLevel tIndent whereClause
  atLevel tIndent $ reserved "end"
  return tdecl
  where
    traitAttribute = label "requirement"
                     (TReqAttribute <$> requirement)
                 <|> (TMethodAttribute <$> methodDecl)
    requirement = do
      reserved "require"
      reqMethod <|> reqField
    reqMethod = do
      reserved "def"
      rheader <- functionHeader
      return RequiredMethod{rheader}
    reqField = do
      rfield <- fieldDecl
      return RequiredField{rfield}
    buildTrait tmeta ident params attributes =
      let (treqs, tmethods) = partitionTraitAttributes attributes
      in
        return Trait{tmeta
                    ,tname = setRefNamespace emptyNamespace $
                             traitTypeFromRefType $
                             refTypeWithParams ident params
                    ,treqs
                    ,tmethods
                    }

traitComposition :: EncParser TraitComposition
traitComposition = makeExprParser includedTrait opTable
    where
      opTable = [
                 [compositionOp "*" Conjunction],
                 [compositionOp "+" Disjunction]
                ]
      compositionOp op constructor =
          InfixL (do withLinebreaks $ reservedOp op
                     return constructor)

      includedTrait =
            trait
        <|> parens traitComposition
        <?> "trait-inclusion"
      trait = do
        notFollowedBy lowerChar
        full <- modulePath
        let ns = explicitNamespace $ init full
            refId = show $ last full
        parameters <- option [] $ brackets (commaSep1 typ)
        tcext <- option [] $ parens (commaSep1 extension)
        let tcname = if isEmptyNamespace ns
                     then traitTypeFromRefType $
                          refTypeWithParams refId parameters
                     else setRefNamespace ns $
                          traitTypeFromRefType $
                          refTypeWithParams refId parameters
        return TraitLeaf{tcname, tcext}
        where
          extension = do
            name <- Name <$> identifier

            methodExtension name <|> return (FieldExtension name)
          methodExtension name = do
            reservedOp "()"
            return $ MethodExtension name

data ClassAttribute = FieldAttribute {flddecl :: FieldDecl}
                    | MethodAttribute {mdecl :: MethodDecl}

partitionClassAttributes :: [ClassAttribute] -> ([FieldDecl], [MethodDecl])
partitionClassAttributes = partitionClassAttributes' [] []
  where
    partitionClassAttributes' fs ms [] = (fs, ms)
    partitionClassAttributes' fs ms (FieldAttribute{flddecl}:as) =
      partitionClassAttributes' (flddecl:fs) ms as
    partitionClassAttributes' fs ms (MethodAttribute{mdecl}:as) =
      partitionClassAttributes' fs (mdecl:ms) as

-- TODO: Allow linebreak (with indent) for trait inclusion
classDecl :: EncParser ClassDecl
classDecl = do
  cIndent <- L.indentLevel
  cdecl <- indentBlock $ do
    cmeta <- meta <$> getPosition
    activity <- parseActivity
    reserved "class"
    name <- lookAhead upperChar >> identifier
    params <- optionalTypeParameters
    ccomposition <- optional (do{colon; traitComposition})
    return $ L.IndentMany
               Nothing
               (buildClass cmeta activity name params ccomposition)
               classAttribute
  -- TODO: clocals <- option [] $ atLevel cIndent whereClause
  atLevel cIndent $ reserved "end"
  return cdecl
  where
    parseActivity = (reserved "shared" >> return Shared)
      <|> (reserved "passive" >> return Passive)
      <|> return Active
    classAttribute = (FieldAttribute <$> fieldDecl)
                 <|> (MethodAttribute <$> methodDecl)
    buildClass cmeta activity name params ccomposition attributes =
      let (cfields, cmethods) = partitionClassAttributes attributes
      in
        return Class{cmeta
                    ,cname = setRefNamespace emptyNamespace $
                             classType activity name params
                    ,ccomposition
                    ,cfields
                    ,cmethods
                    }

mutModifier :: EncParser Mutability
mutModifier = (reserved "var" >> return Var)
          <|> (reserved "val" >> return Val)

fieldDecl :: EncParser FieldDecl
fieldDecl = do fmeta <- meta <$> getPosition
               fmut  <- mutModifier
               fname <- Name <$> identifier
               colon
               ftype <- typ
               return Field{fmeta
                           ,fmut
                           ,fname
                           ,ftype}

paramDecl :: EncParser ParamDecl
paramDecl = do
  pmeta <- meta <$> getPosition
  pmut <- option Val $
              (reserved "var" >> return Var)
          <|> (reserved "val" >> return Val)
  pname <- Name <$> identifier
  colon
  ptype <- typ
  return Param{pmeta, pmut, pname, ptype}

patternParamDecl :: EncParser (Expr, Type)
patternParamDecl = do
  x <- expr
  colon
  ty <- typ
  return (x, ty)

methodDecl :: EncParser MethodDecl
methodDecl = do
  mIndent <- L.indentLevel
  mtd <- methodHeaderAndBody

  mlocals <- option [] $ atLevel mIndent whereClause
  atLevel mIndent $ reserved "end"
  return mtd{mlocals}
  where
    methodHeaderAndBody =
      indentBlock $ do
        mmeta <- meta <$> getPosition
        mheader <- do reserved "def"
                      modifiers <- many modifier
                      setHeaderModifier modifiers <$> functionHeader
               <|> do reserved "stream"
                      streamMethodHeader
        alignedExpressions (buildMethod mmeta mheader)
    buildMethod mmeta mheader block =
      return Method{mmeta
                   ,mheader
                   ,mbody = makeBody block
                   ,mlocals = []
                   }

modifier :: EncParser Modifier
modifier = (reserved "private" >> return ModPrivate)
       <|> (reserved "match" >> return ModMatch)

arguments :: EncParser Arguments
arguments = do
  indent <- L.indentLevel
  atLeast indent expression `sepBy` withLinebreaks comma

sepBy2 p sep = do
  first <- p
  sep
  last <- p `sepBy1` sep
  return $ first:last

matchClause :: EncParser MatchClause
matchClause = do
  indent <- L.indentLevel
  (needsEnd, clause) <- indentBlock $ do
    reserved "case"
    mcpattern <- expression <|> dontCare
    guardMeta <- meta <$> getPosition
    mcguard <- option (BTrue guardMeta) guard
    reservedOp "=>"
    lineClause mcpattern mcguard <|> blockClause mcpattern mcguard
  when needsEnd $
       atLevel indent $ reserved "end"
  return clause
  where
    lineClause mcpattern mcguard = do
      notFollowedBy nl
      mchandler <- expression
      return $ L.IndentNone (False, MatchClause{mcpattern, mcguard, mchandler})
    blockClause mcpattern mcguard =
      alignedExpressions
        (\body -> return (True, MatchClause{mcpattern
                                            ,mcguard
                                            ,mchandler = makeBody body
                                           }))
    dontCare = do
      emeta <- meta <$> getPosition
      symbol "_"
      return VarAccess{emeta, qname = qName "_"}

expression :: EncParser Expr
expression = makeExprParser expr opTable
    where
      opTable = [
                 [prefix "-" NEG],
                 [op "*" TIMES, op "/" DIV, op "%" MOD],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<=" Identifiers.LTE, op ">=" Identifiers.GTE,
                  op "<" Identifiers.LT, op ">" Identifiers.GT
                 ],
                 [op "==" Identifiers.EQ, op "!=" NEQ],
                 [textualPrefix "not" Identifiers.NOT],
                 [partySequence, partyParallel],
                 [op "&&" Identifiers.AND,
                  op "||" Identifiers.OR],
                 [arrayAccess],
                 [messageSend],
                 [typedExpression],
                 [singleLineTask],
                 [chain],
                 [assignment,
                  op "+=" PLUS_EQUALS,
                  op "-=" MINUS_EQUALS,
                  op "*=" TIMES_EQUALS,
                  op "/=" DIV_EQUALS]
                ]

      textualPrefix s operator =
          Prefix (try(do pos <- getPosition
                         reserved s
                         return (Unary (meta pos) operator)))
      prefix s operator =
          Prefix (do pos <- getPosition
                     reservedOp s
                     return (Unary (meta pos) operator))
      op s binop =
          InfixL (do pos <- getPosition
                     withLinebreaks $ reservedOp s
                     return (Binop (meta pos) binop))

      arrayAccess =
          Postfix (do pos <- getPosition
                      index <- parens expression
                      return (\target -> ArrayAccess{emeta = meta pos
                                                    ,target
                                                    ,index
                                                    }))

      typedExpression =
          Postfix (do pos <- getPosition
                      withLinebreaks colon
                      t <- typ
                      return (\e -> TypedExpr (meta pos) e t))
      messageSend =
          Postfix (do pos <- getPosition
                      withLinebreaks bang
                      name <- Name <$> identifier
                      typeArguments <- option [] (try . brackets $ commaSep typ)
                      args <- parens arguments
                      let msgSend opt target = MessageSend {emeta = meta pos
                                                           ,typeArguments
                                                           ,target
                                                           ,name
                                                           ,args}
                      return $ msgSend False)

      singleLineTask =
        Prefix (do notFollowedBy (reserved "async" >> nl)
                   emeta <- meta <$> getPosition
                   reserved "async"
                   return (Async emeta))

      chain =

          InfixL (do pos <- getPosition
                     withLinebreaks $ reservedOp "~~>"
                     return (FutureChain (meta pos)))
      partySequence =
          InfixL (do pos <- getPosition ;
                     reservedOp ">>" ;
                     return (PartySeq (meta pos)))
      partyParallel =
          InfixL (do pos <- getPosition ;
                     reservedOp "|||" ;
                     return (PartyPar (meta pos)))
      assignment =
          InfixR (do pos <- getPosition ;
                     reservedOp "=" ;
                     return (Assign (meta pos)))

-- Elias: I don't know why the first 'notFollowedBy nl' needed,
-- but it improves error messages
expr :: EncParser Expr
expr = notFollowedBy nl >>
        (embed
     <|> break
     <|> continue
     <|> closure
     <|> match
     <|> blockedTask
     <|> for
     <|> while
     <|> repeat
     <|> arraySize
     <|> bracketed
     <|> letExpression
     <|> ifExpression
     <|> unlessIf
     <|> explicitReturn
     <|> forward
     <|> yield
     <|> try isEos
     <|> eos
     <|> new
     <|> sequence
     <|> miniLet
     <|> path
     --- literals ---
     <|> nullLiteral
     <|> true
     <|> false
     <|> stringLit
     <|> charLit
     <|> try real
     <|> int)
     <?> "expression"
    where
      embed = do
        indent <- L.indentLevel
        startLine <- sourceLine <$> getPosition
        emeta <- meta <$> getPosition
        reserved "EMBED"
        ty <- label "parenthesized type" $
                    parens typ
        vspace
        embedded <- many cAndEncore
        vspace
        endLine <- sourceLine <$> getPosition
        if endLine == startLine
        then hspace >> reserved "END"
        else atLevel indent $ reserved "END"
        when (null embedded) $
             fail "EMBED block cannot be empty"
        return Embed{emeta, ty, embedded}
        where
          cAndEncore :: EncParser (String, Expr)
          cAndEncore = (do
            notFollowedBy $ reserved "END"
            code <- c
            emeta <- meta <$> getPosition
            e <- option Skip{emeta}
                 (try $ encoreEscaped expression)
            return (code, e))
            <|> liftM ("",) (encoreEscaped expression)
          c = do
            notFollowedBy (embedEnd <|> void encoreEscapeStart)
            first <- anyChar
            rest <-
              manyTill anyChar (try $ lookAhead (embedEnd <|>
                                                 void encoreEscapeStart))
            return (first:rest)
          embedEnd = vspace >> reserved "END"

      path = do
        pos <- getPosition
        root <- tupled <|>
                stringLit <|>
                try qualifiedVarOrFun <|>
                varOrFun
        longerPath pos root <|> return root
        where
          tupled = do
            pos <- getPosition
            args <- parens (expression `sepBy` comma)
            case args of
              [] -> return $ Skip (meta pos)
              [e] -> return e
              _ -> return $ Tuple (meta pos) args

          qualifiedVarOrFun = do
            qx <- qualifiedVarAccess
            functionOrCall qx <|> return qx

          varOrFun = do
            x <- varAccess
            functionOrCall x <|> return x

          qualifiedVarAccess = do
            pos <- getPosition
            ns <- explicitNamespace <$> modulePath
            dot
            x <- identifier
            let qx = setNamespace ns (qName x)
            return $ VarAccess (meta pos) qx

          varAccess = do
            pos <- getPosition
            id <- (do reserved "this"; return "this") <|> identifier
            return $ VarAccess (meta pos) (qName id)

          functionOrCall VarAccess{emeta, qname} = do
            optTypeArgs <- option [] (try . brackets $ commaSep typ)
            if null optTypeArgs then
              call emeta optTypeArgs qname
            else
              call emeta optTypeArgs qname <|>
              return (FunctionAsValue emeta optTypeArgs qname)

          call emeta typeArgs name = do
            args <- parens arguments
            return $ FunctionCall emeta typeArgs name args

          longerPath pos root = do
            first <- pathComponent
            rest <- many $ try pathComponent
            return $ foldl (buildPath pos) root (first:rest)

          pathComponent = do
            emeta <- meta <$> getPosition
            try comparmentAcc <|> try varOrCallFunction <|>
              optionalAccessBang emeta <|> optionalAccessDot emeta
            where
              optionalAccessBang emeta = do
                reservedOp "?!"
                m <- varAccess >>= functionCall
                return $ Optional emeta (QuestionBang m)
              optionalAccessDot emeta = do
                reservedOp "?."
                var <- do v <- varAccess
                          functionCall v <|> return v
                return $ Optional emeta (QuestionDot var)
              comparmentAcc = dot >> compartmentAccess
              varOrCallFunction = dot >> varOrCall

          compartmentAccess = do
            pos <-  getPosition
            n <- L.integer
            return $ IntLiteral (meta pos) (fromInteger n)

          varOrCall = do
            x <- varAccess
            functionCall x <|> return x

          functionCall VarAccess{emeta, qname} = do
            typeParams <- option [] (try . brackets $ commaSep typ)
            args <- parens arguments
            return $ FunctionCall emeta typeParams qname args

          buildPath _ target o@Optional {emeta, optTag = QuestionBang f@(FunctionCall {})} =
            o {optTag = QuestionBang $ MessageSend emeta (typeArguments f) target (qnlocal $ qname f) (args f)}

          buildPath _ target o@Optional {emeta, optTag = QuestionDot f@(FunctionCall {})} =
            o {optTag = QuestionDot $ MethodCall emeta (typeArguments f) target (qnlocal $ qname f) (args f) }

          buildPath _ target o@Optional {emeta, optTag = QuestionDot (VarAccess {qname})} =
            o { optTag = QuestionDot $ FieldAccess emeta target (qnlocal qname) }

          buildPath pos target (VarAccess{qname}) =
            FieldAccess (meta pos) target (qnlocal qname)

          buildPath pos target (FunctionCall{qname, args, typeArguments}) =
            MethodCall (meta pos) typeArguments target (qnlocal qname) args

          buildPath pos target (IntLiteral {intLit}) =
            TupleAccess (meta pos) target intLit

      letExpression = do
        indent <- L.indentLevel
        letLine <- sourceLine <$> getPosition
        (needsEnd, letExpr) <- indentBlock $ do
          emeta <- meta <$> getPosition
          decls <- indentBlock $ do
            reserved "let"
            inlineDecls indent <|> indentedDecls indent
          inLine <- sourceLine <$> getPosition
          if inLine == letLine
          then do -- let ... in
            reserved "in"
            inlineLet emeta decls <|> nonInlineLet indent emeta decls
          else do -- let
                  --   ...
                  -- in
            atLevel indent $ reserved "in"
            nonInlineLet indent emeta decls
        when needsEnd $
             atLevel indent $ reserved "end"
        return letExpr
        where
          inlineDecls letIndent = do
            notFollowedBy nl
            varIndent <- L.indentLevel
            first <- varDecl letIndent
            return $ L.IndentMany (Just varIndent) (return . (first:))
                                                   (varDecl letIndent)
          indentedDecls indent =
            return $ L.IndentSome Nothing return (varDecl indent)
          inlineLet emeta decls = do
            notFollowedBy nl
            body <- expression
            reserved "end"
            return $ L.IndentNone (False, Let{emeta
                                             ,mutability = Val
                                             ,decls
                                             ,body
                                             })
          nonInlineLet indent emeta decls =
            parseBody $ \body -> (True, Let{emeta
                                           ,mutability = Val
                                           ,decls
                                           ,body
                                           })
      varDecl indent = do
        x <- Name <$> identifier
        withLinebreaks $ reservedOp "="
        val <- indented indent expression
        return (x, val)

      sequence = singleLineBlock <|> multiLineBlock
      singleLineBlock = do
        emeta <- meta <$> getPosition
        eseq <- braces (expression `sepEndBy1` semi)
        return Seq{emeta, eseq}
      multiLineBlock = do
        indent <- L.indentLevel
        block <- indentBlock $ do
          emeta <- meta <$> getPosition
          reserved "do"
          alignedExpressions (return . Seq emeta)
        doBlock indent block <|> doWhile indent block
      doBlock indent block = do
        atLevel indent $ reserved "end"
        return block
      doWhile indent body = do
        emeta <- meta <$> getPosition
        atLevel indent $ reserved "while"
        cond <- expression
        return DoWhile{emeta, cond, body}

      miniLet = do
        indent <- L.indentLevel
        emeta <- meta <$> getPosition
        mutability <- mutModifier
        (x, val) <- varDecl indent
        return MiniLet{emeta, mutability, decl = (x, val)}

      ifExpression = do
        indent <- L.indentLevel
        ifLine <- sourceLine <$> getPosition
        ifThen <-
          ifWithSimpleCond indent ifLine (reserved "if") <|>
          ifWithComplexCond indent (reserved "if")
        ifThenNoElse indent ifLine ifThen
         <|> ifThenElse indent ifLine ifThen

      ifWithSimpleCond indent ifLine head = do
        notFollowedBy (head >> nl)
        indentBlock $ do
          emeta <- meta <$> getPosition
          atLevel indent head
          cond <- expression
          thenLine <- sourceLine <$> getPosition
          unless (thenLine == ifLine) $
                 fail "Expected 'then' on same line or column as 'if'"
          reserved "then"
          inlineIfThen emeta cond <|> nonInlineIfThen indent emeta cond

      ifWithComplexCond indent head = do
        emeta <- meta <$> getPosition
        head
        nl
        cond <- indented indent expression
        indentBlock $ do
          atLevel indent $ reserved "then"
          parseBody $ \thn -> IfThen{emeta, cond, thn}

      inlineIfThen emeta cond  = do
        notFollowedBy nl
        thn <- expression
        return $ L.IndentNone IfThen{emeta, cond, thn}

      nonInlineIfThen indent emeta cond =
        parseBody $ \thn -> IfThen{emeta, cond, thn}

      ifThenNoElse indent ifLine ifThen = do
        notFollowedBy (reserved "else" <|> (nl >> reserved "else"))
        endLine <- sourceLine <$> getPosition
        if endLine == ifLine
        then reserved "end"
        else atLevel indent $ reserved "end"
        return ifThen

      ifThenElse indent ifLine ifThen = do
        elseLine <- sourceLine <$> getPosition
        if ifLine == elseLine -- Single line if
        then do
          reserved "else"
          els <- expression
          reserved "end"
          return $ extendIfThen ifThen els
        else finalElse indent ifThen <|>
             elseIf indent ifThen

      elseIf indent ifThen = do
        let head = reserved "else" >> reserved "if"
        ifLine <- sourceLine <$> getPosition
        nestedIfThen <-
          ifWithSimpleCond indent ifLine head <|>
          ifWithComplexCond indent head
        nestedIfThenElse <-
          ifThenNoElse indent ifLine nestedIfThen <|>
          finalElse indent nestedIfThen <|>
          elseIf indent nestedIfThen
        return $ extendIfThen ifThen nestedIfThenElse

      finalElse indent ifThen = do
        notFollowedBy (reserved "else" >> reserved "if")
        result <-
          indentBlock $ do
            atLevel indent $ reserved "else"
            parseBody (extendIfThen ifThen)
        atLevel indent $ reserved "end"
        return result

      extendIfThen IfThen{emeta, cond, thn} els =
        IfThenElse{emeta, cond, thn, els}

      unlessIf = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "unless"
        cond <- expression
        reserved "then"
        return $ \thn -> Unless{emeta, cond, thn}

      for = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "for"
        name <- Name <$> identifier
        reservedOp "<-"
        src <- expression
        stepMeta <- meta <$> getPosition
        step <- option (IntLiteral stepMeta 1)
                       (do {reserved "by"; expression})
        reserved "do"
        return $ \body -> For{emeta, name, src, step, body}

      while = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "while"
        cond <- expression
        reserved "do"
        return $ \body -> While{emeta, cond, body}

      repeat = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "repeat"
        name <- Name <$> identifier
        reservedOp "<-"
        times <- expression
        reserved "do"
        return $ \body -> Repeat{emeta, name, times, body}

      match = do
        indent <- L.indentLevel
        theMatch <- indentBlock $ do
          emeta <- meta <$> getPosition
          reserved "match"
          arg <- expression
          reserved "with"
          return $ L.IndentSome Nothing (return . Match emeta arg) matchClause
        atLevel indent $ reserved "end"
        return theMatch

      yield = do
        emeta <- meta <$> getPosition
        reserved "yield"
        val <- expression
        return Yield{emeta, val}

      isEos = do
        emeta <- meta <$> getPosition
        reserved "eos"
        target <- expression
        return IsEos{emeta, target}

      eos = do
        emeta <- meta <$> getPosition
        reserved "eos"
        return Eos{emeta}

      break = do
        pos <- getPosition
        reserved "break"
        return $ Break (meta pos)

      continue = do
        pos <- getPosition
        reserved "continue"
        return $ Continue (meta pos)

      forward = do
        pos <- getPosition
        reserved "forward"
        expr <- parens expression
        return $ Forward (meta pos) expr

      closure = do
        indent <- L.indentLevel
        funLine <- sourceLine <$> getPosition
        (withEnd, clos) <- indentBlock $ do
          emeta <- meta <$> getPosition
          reserved "fun"
          eparams <- parens (commaSep paramDecl)
          mty <- optional (colon >> typ)
          singleLineClosure emeta eparams mty <|>
            blockClosure emeta eparams mty
        when withEnd $
             atLevel indent $ reserved "end"
        return clos
      singleLineClosure emeta eparams mty = do
        reservedOp "=>"
        body <- expression
        return $ L.IndentNone (False, Closure{emeta, eparams, mty, body})
      blockClosure emeta eparams mty =
        alignedExpressions (buildClosure emeta eparams mty)
      buildClosure emeta eparams mty block =
        return (True, Closure{emeta
                             ,eparams
                             ,mty
                             ,body = makeBody block
                             })

      blockedTask = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "async"
        return $ \body -> Async{emeta, body}

      arraySize = do
        emeta <- meta <$> getPosition
        bar
        target <- expression
        bar
        return ArraySize{emeta, target}

      nullLiteral = do
        emeta <- meta <$> getPosition
        reserved "null"
        return Null{emeta}

      true = do
        emeta <- meta <$> getPosition
        reserved "true"
        return BTrue{emeta}

      false = do
        emeta <- meta <$> getPosition
        reserved "false"
        return BFalse{emeta}

      new = do
        emeta <- meta <$> getPosition
        reserved "new"
        ty <- typ
        newWithoutInit emeta ty <|> newWithInit emeta ty
        where
          newWithoutInit emeta ty = do
            notFollowedBy (symbol "(")
            return New{emeta, ty}
          newWithInit emeta ty = do
            args <- parens arguments
            return NewWithInit{emeta, ty, args}

      stringLit = do
        emeta <- meta <$> getPosition
        stringLit <- stringLiteral
        return StringLiteral{emeta, stringLit}

      charLit = do
        emeta <- meta <$> getPosition
        charLit <- charLiteral
        return CharLiteral{emeta, charLit}

      int = do
        emeta <- meta <$> getPosition
        n <- L.integer
        kind <- do hidden (symbol "u") <|> hidden (symbol "U")
                   return UIntLiteral
               <|> (hspace >> return IntLiteral)
        return $ kind emeta (fromInteger n)

      real = do
        emeta <- meta <$> getPosition
        realLit <- float
        return RealLiteral{emeta, realLit}

      explicitReturn = do pos <- getPosition
                          reserved "return"
                          expr <- option (Skip (meta pos)) (do {expression})
                          return $ Return (meta pos) expr

      bracketed =
          lineFold $ \sc' ->
            folded brackets sc' (rangeOrArray <|> empty)
          where
            empty = do
              emeta <- meta <$> getPosition
              lookAhead (symbol "]")
              return ArrayLiteral{emeta, args = []}
            rangeOrArray = do
              emeta <- meta <$> getPosition
              first <- expression
              range emeta first
               <|> arrayLit emeta first
            range emeta start = do
              dotdot
              stop <- expression
              stepMeta <- meta <$> getPosition
              step <- option (IntLiteral stepMeta 1)
                             (reserved "by" >> expression)
              return RangeLiteral{emeta, start, stop, step}
            arrayLit emeta first = longerArray <|> singletonArray
              where
                singletonArray =
                  return ArrayLiteral{emeta, args = [first]}
                longerArray = do
                  notFollowedBy (symbol "]")
                  comma
                  rest <- commaSep1 expression
                  return ArrayLiteral{emeta, args = first:rest}
