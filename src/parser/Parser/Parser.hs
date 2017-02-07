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
import Data.Char(isUpper)
import Data.Maybe(fromMaybe)
import Control.Monad(void, foldM, unless, when, liftM)
import Control.Monad.Reader hiding(guard)
import Control.Applicative ((<$>), empty)
import Control.Arrow (first)

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
blockComment = L.skipBlockComment "{-" "-}"

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

-- TODO: What about escape sequences? e.g. \n
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

-- | Parse a token in column one
nonIndented :: EncParser a -> EncParser a
nonIndented = L.nonIndented hspace

-- | See 'L.indentBlock'. Used to parse indented blocks of code.
indentBlock = L.indentBlock vspace

-- | See 'L.lineFold'. Used to fold lines, i.e. allow linebreaks
-- but require proper indentation
lineFold :: (EncParser () -> EncParser a) -> EncParser a
lineFold = L.lineFold vspace

-- | @parseBody ind c@ is inteded for use in the end of an
-- `indentBlock` construct. It parses the body of a loop or
-- conditional, which is either a "do", a newline and a list of
-- indented expressions, or a single expression at an indentation
-- level greater than @ind@. In the first case, the returned
-- boolean is @True@ to signal that some terminating token is
-- needed (e.g. "end"). @c@ is a function that takes the loop body
-- as the argument and builds a (possibly partial) expression.
parseBody :: Pos -> (Expr -> a) -> EncParser (L.IndentOpt EncParser (Bool, a) Expr)
parseBody indent constructor = blockBody <|> shortBody
  where
    blockBody = do
      reserved "do"
      return $ L.IndentSome Nothing
               (return . (True,) . constructor . makeBody) expression
    shortBody = do
      nl
      body <- indented indent expression
      return $ L.IndentNone (False, constructor body)

-- | @blockedConstruct p@ parses a construct whose header is
-- parsed by @p@, and whose body is either a single indented
-- expression, or a block between "do"/"end".
blockedConstruct header = do
  indent <- L.indentLevel
  (needsEnd, loop) <- indentBlock $ do
    constructor <- header
    parseBody indent constructor
  when needsEnd $
       atLevel indent $ reserved "end"
  return loop

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
    ,"await"
    ,"body"
    ,"bool"
    ,"case"
    ,"char"
    ,"class"
    ,"def"
    ,"do"
    ,"each"
    ,"else"
    ,"end"
    ,"eos"
    ,"extract"
    ,"false"
    ,"for"
    ,"fun"
    ,"if"
    ,"import"
    ,"in"
    ,"int"
    ,"let"
    ,"liftf"
    ,"liftv"
    ,"match"
    ,"module"
    ,"new"
    ,"not"
    ,"null"
    ,"or"
    ,"passive"
    ,"peer"
    ,"qualified"
    ,"real"
    ,"reduce"
    ,"repeat"
    ,"require"
    ,"shared"
    ,"stream"
    ,"suspend"
    ,"then"
    ,"this"
    ,"trait"
    ,"true"
    ,"typedef"
    ,"uint"
    ,"unless"
    ,"val"
    ,"var"
    ,"void"
    ,"when"
    ,"where"
    ,"while"
    ,"with"
    ,"yield"
   ]

validIdentifierChar :: EncParser Char
validIdentifierChar = alphaNumChar <|> char '_' <|> char '\''

validOpChar :: EncParser Char
validOpChar = oneOf ".:=!<>+-*/%\\~|"

reserved :: String -> EncParser ()
reserved w = do
  sc <- currentSpaceConsumer
  try $ string w *> notFollowedBy validIdentifierChar *> sc

reservedOp :: String -> EncParser ()
reservedOp op = do
  sc <- currentSpaceConsumer
  try $ string op <* notFollowedBy validOpChar *> sc

identifier :: EncParser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many validIdentifierChar
    check x = if x `elem` reservedNames
              -- TODO: Change from tutorial default
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
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
                 [typeConstructor "Maybe" maybeType
                 ,typeConstructor "Fut" futureType
                 ,typeConstructor "Par" parType
                 ,typeConstructor "Stream" streamType
                 ],
                 [arrow]
                ]
      typeOp op constructor =
          InfixL (do reservedOp op
                     return constructor)
      typeConstructor op constructor =
          Prefix (do reserved op
                     return constructor)
      arrow =
          InfixR (do reservedOp "->"
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
        do {reserved "void"; return voidType}

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
moduleDecl = option NoModule $ do
  modmeta <- meta <$> getPosition
  reserved "module"
  lookAhead upperChar
  modname <- Name <$> identifier
  -- TODO: Allow linebreaks
  modexports <- optional $ parens ((Name <$> identifier) `sepEndBy` comma)
  return Module{modmeta
               ,modname
               ,modexports
               }

importdecl :: EncParser ImportDecl
importdecl = do
  imeta <- meta <$> getPosition
  reserved "import"
  iqualified <- option False $ reserved "qualified" >> return True
  itarget <- explicitNamespace <$> modulePath
  -- TODO: Allow linebreaks
  iselect <- optional $ parens ((Name <$> identifier) `sepEndBy` comma)
  ialias <- optional $ reserved "as" >> (explicitNamespace <$> modulePath)
  ihiding <- optional $
             reserved "hiding" >>
             parens ((Name <$> identifier) `sepEndBy` comma)
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
  reserved "typedef"
  name <- lookAhead upperChar >> identifier
  params <- optionalTypeParameters
  reservedOp "="
  -- TODO: Allow linebreaks
  typedeftype <- typ
  let typedefdef = setRefNamespace emptyNamespace $
                   typeSynonym name params typedeftype
  return Typedef{typedefmeta, typedefdef}

functionHeader :: EncParser FunctionHeader
functionHeader = do
  hname <- Name <$> identifier
  htypeparams <- optionalTypeParameters
  -- TODO: Allow linebreaks
  hparams <- parens (commaSep paramDecl)
  colon
  htype <- typ
  return Header{hmodifier = [Public]
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

matchingHeader = do
   hname <- Name <$> identifier
   htypeparams <- optionalTypeParameters
   args <- parens (commaSep patternParamDecl)
   colon
   htype <- typ
   posGuard <- getPosition
   hguard <- option (BTrue (meta posGuard)) guard
   let (hpatterns, hparamtypes) = unzip  args
   return MatchingHeader{hmodifier = [Public]
                        ,kind = NonStreaming
                        ,htypeparams
                        ,hname
                        ,hpatterns
                        ,hparamtypes
                        ,htype
                        ,hguard
                        }

matchingStreamHeader :: EncParser FunctionHeader
matchingStreamHeader = do
  header <- matchingHeader
  return header{kind = Streaming}

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
  -- TODO: Re-add matching functions
  indentBlock $ do
    funmeta <- meta <$> getPosition
    reserved "fun"
    funheader <- functionHeader
    return $ L.IndentSome Nothing (buildFun funmeta funheader) expression
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
    es@(e:_) -> Seq (meta (getPos e)) es

matchingFunction = do
  funmeta <- meta <$> getPosition
  reserved "def"
  clauses <- functionClause `sepBy1` reservedOp "|"
  let matchfunheaders = map fst clauses
      matchfunbodies = map snd clauses
  return MatchingFunction{funmeta
                         ,matchfunheaders
                         ,matchfunbodies
                         ,funlocals = []
                         ,funsource = ""
                         }
  where
    functionClause = do
      funheader <- matchingHeader
      funbody <- expression
      return (funheader, funbody)

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
                     (TReqAttribute <$> (try reqField <|>
                                         reqMethod))
                 <|> (TMethodAttribute <$> methodDecl)
    reqField = do
      reserved "require"
      rfield <- fieldDecl
      return RequiredField{rfield}
    reqMethod = do
      reserved "require"
      rheader <- functionHeader
      return RequiredMethod{rheader}
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
          InfixL (do reservedOp op
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


classDecl :: EncParser ClassDecl
classDecl = do
  cIndent <- L.indentLevel
  cdecl <- indentBlock $ do
    cmeta <- meta <$> getPosition
    activity <- parseActivity
    reserved "class"
    name <- lookAhead upperChar >> identifier
    params <- optionalTypeParameters
    ccomposition <- optional (do{reservedOp ":"; traitComposition})
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

modifier :: EncParser Modifier
modifier = val
           <?>
           "modifier"
    where
      val = do
        reserved "val"
        return MVal

fieldDecl :: EncParser FieldDecl
fieldDecl = do fmeta <- meta <$> getPosition
               fmods <- many modifier
               fname <- Name <$> identifier
               colon
               ftype <- typ
               return Field{fmeta
                           ,fmods
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
  -- TODO: Re-add support for matching methods
  mtd <- methodHeaderAndBody -- <|> matchingMethod

  mlocals <- option [] $ atLevel mIndent whereClause
  atLevel mIndent $ reserved "end"
  return mtd{mlocals}
  where
    methodHeaderAndBody =
      indentBlock $ do
        mmeta <- meta <$> getPosition
        mheader <- do reserved "def"
                      modifiers <- option [Public] $ many modifiersDecl
                      setHeaderModifier modifiers <$> functionHeader
               <|> do reserved "stream"
                      streamMethodHeader
        return $ L.IndentSome Nothing (buildMethod mmeta mheader) expression
    buildMethod mmeta mheader block =
      return Method{mmeta
                   ,mheader
                   ,mbody = makeBody block
                   ,mlocals = []
                   }
    matchingMethod = do
      mmeta <- meta <$> getPosition
      clauses <- do reserved "def"
                    modifiers <- option [Public] $ many modifiersDecl
                    map (first (setHeaderModifier modifiers)) <$>
                      methodClause matchingHeader `sepBy1` reservedOp "|"
             <|> do reserved "stream"
                    methodClause matchingStreamHeader `sepBy1` reservedOp "|"
      let (mheaders, mbodies) = unzip clauses

      return MatchingMethod{mmeta
                           ,mheaders
                           ,mbodies
                           ,mlocals = []
                           }
      where
        methodClause headerParser= do
          mheader <- headerParser
          mbody <- expression
          return (mheader, mbody)

modifiersDecl :: EncParser AccessModifier
modifiersDecl = reserved "private" >> return Private


arguments :: EncParser Arguments
arguments = expression `sepBy` comma

sepBy2 p sep = do first <- p
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
    lineClause mcpattern mcguard <|> blockClause mcpattern mcguard
  when needsEnd $
       atLevel indent $ reserved "end"
  return clause
  where
    lineClause mcpattern mcguard = do
      reservedOp "=>"
      mchandler <- expression
      return $ L.IndentNone (False, MatchClause{mcpattern, mcguard, mchandler})
    blockClause mcpattern mcguard = do
      reservedOp "do"
      return $ L.IndentSome Nothing
               (\body -> return (True, MatchClause{mcpattern
                                                  ,mcguard
                                                  ,mchandler = makeBody body
                                                  })) expression
    dontCare = do
      emeta <- meta <$> getPosition
      symbol "_"
      return VarAccess{emeta, qname = qName "_"}

expression :: EncParser Expr
expression = makeExprParser expr opTable
    where
      opTable = [
                 [arrayAccess],
                 [prefix "-" NEG],
                 [op "*" TIMES, op "/" DIV, op "%" MOD],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" Identifiers.LT, op ">" Identifiers.GT,
                  op "<=" Identifiers.LTE, op ">=" Identifiers.GTE,
                  op "==" Identifiers.EQ, op "!=" NEQ],
                 [textualPrefix "not" Identifiers.NOT],
                 [op "&&" Identifiers.AND,
                  op "||" Identifiers.OR],
                 [messageSend],
                 [partyLiftf, partyLiftv, partyEach],
                 [typedExpression],
                 [chain],
                 [partySequence],
                 [partyParallel],
                 [partyJoin],
                 [assignment]
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
                     reservedOp s
                     return (Binop (meta pos) binop))

      typedExpression =
          Postfix (do pos <- getPosition
                      reservedOp ":"
                      t <- typ
                      return (\e -> TypedExpr (meta pos) e t))
      arrayAccess =
          Postfix (try (do pos <- getPosition
                           index <- brackets expression
                           return (\e -> ArrayAccess (meta pos) e index)))
      messageSend =
          Postfix (do pos <- getPosition
                      bang
                      name <- identifier
                      optTypeArgs <- option [] (try . brackets $ commaSep typ)
                      args <- parens arguments
                      return (\target -> MessageSend { emeta = meta pos,
                                                       typeArguments=optTypeArgs,
                                                       target,
                                                       name=(Name name),
                                                       args }))
      chain =
          InfixL (do pos <- getPosition ;
                     reservedOp "~~>" ;
                     return (FutureChain (meta pos)))
      -- TODO: What is the correct syntax here?
      partyLiftf =
          Prefix (do pos <- getPosition
                     reserved "liftf"
                     return (Liftf (meta pos)))
      partyLiftv =
          Prefix (do pos <- getPosition
                     reserved "liftv"
                     return (Liftv (meta pos)))
      partyEach =
          Prefix (do pos <- getPosition
                     reserved "each"
                     return $ PartyEach (meta pos))
      partySequence =
          InfixL (do pos <- getPosition ;
                     reservedOp ">>" ;
                     return (PartySeq (meta pos)))
      partyParallel =
          InfixL (do pos <- getPosition ;
                     reservedOp "||" ;
                     return (PartyPar (meta pos)))
      partyJoin =
          Prefix (do pos <- getPosition
                     reserved "join"
                     return (PartyJoin (meta pos)))
      assignment =
          InfixR (do pos <- getPosition ;
                     reservedOp "=" ;
                     return (Assign (meta pos)))


expr :: EncParser Expr
expr  =  embed
     <|> closure
     <|> reduce
     <|> match
     <|> task
     <|> for
     <|> while
     <|> repeat
     <|> extract
     <|> arraySize
     <|> bracketed
     <|> letExpression
     <|> ifExpression
     <|> unlessIf
     <|> yield
     <|> try isEos
     <|> eos
     <|> suspend
     <|> yield
     <|> new
     <|> peer
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
     <|> int
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

          pathComponent = dot >> varOrCall

          varOrCall = do
            x <- varAccess
            functionCall x <|> return x

          functionCall VarAccess{emeta, qname} = do
            typeParams <- option [] (try . brackets $ commaSep typ)
            args <- parens arguments
            return $ FunctionCall emeta typeParams qname args

          buildPath pos target (VarAccess{qname}) =
            FieldAccess (meta pos) target (qnlocal qname)

          buildPath pos target (FunctionCall{qname, args, typeArguments}) =
            MethodCall (meta pos) typeArguments target (qnlocal qname) args

      letExpression = do
        indent <- L.indentLevel
        letLine <- sourceLine <$> getPosition
        (withDo, letExpr) <- indentBlock $ do
          emeta <- meta <$> getPosition
          decls <- indentBlock $ do
            reserved "let"
            singleLineDecl <|> multiLineDecl
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
        when withDo $
             atLevel indent $ reserved "end"
        return letExpr
        where
          singleLineDecl = do
            notFollowedBy nl
            decl <- varDecl
            return $ L.IndentNone [decl]
          multiLineDecl =
            return $ L.IndentSome Nothing return varDecl
          inlineLet emeta decls = do
            notFollowedBy (reserved "do" <|> nl)
            body <- expression
            return $ L.IndentNone (False, Let{emeta
                                             ,mutability = Val
                                             ,decls
                                             ,body
                                             })
          nonInlineLet indent emeta decls =
            parseBody indent $ \body -> Let{emeta
                                           ,mutability = Val
                                           ,decls
                                           ,body
                                           }
      varDecl = do
        x <- Name <$> identifier
        reservedOp "="
        val <- expression
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
          return $ L.IndentSome Nothing (return . Seq emeta) expression
        atLevel indent $ reserved "end"
        return block

      miniLet = do
        indent <- L.indentLevel
        emeta <- meta <$> getPosition
        mutability <- (reserved "var" >> return Var)
                  <|> (reserved "val" >> return Val)
        (x, val) <- varDecl
        return MiniLet{emeta, mutability, decl = (x, val)}

      ifExpression = do
        indent <- L.indentLevel
        ifLine <- sourceLine <$> getPosition
        (withDo, ifThen) <- indentBlock $ do
          emeta <- meta <$> getPosition
          reserved "if"
          cond <- expression
          reserved "then"
          inlineIfThen emeta cond <|> nonInlineIfThen indent emeta cond
        ifThenNoElse indent withDo ifThen
         <|> ifThenElse indent ifLine withDo ifThen

      inlineIfThen emeta cond  = do
        notFollowedBy (reserved "do" <|> nl)
        thn <- expression
        return $ L.IndentNone (False, IfThen{emeta, cond, thn})
      nonInlineIfThen indent emeta cond =
        parseBody indent $ \thn -> IfThen{emeta, cond, thn}
      ifThenNoElse indent withDo ifThen = do
        notFollowedBy (reserved "else" <|> (nl >> reserved "else"))
        when withDo $
             atLevel indent $ reserved "end"
        return ifThen
      ifThenElse indent ifLine withDo ifThen = do
        elseLine <- sourceLine <$> getPosition
        if ifLine == elseLine -- Single line if
        then do
          reserved "else"
          els <- expression
          return $ extendIfThen ifThen els
        else if withDo -- if b then do
        then do
          ifThenElse <- indentBlock $ do
            atLevel indent $ reserved "else"
            reserved "do"
            return $ L.IndentSome Nothing
                     (return . extendIfThen ifThen . makeBody) expression
          atLevel indent $ reserved "end"
          return ifThenElse
        else do -- if b then
          nl
          atLevel indent $ reserved "else"
          nl
          els <- indented indent expression
          return $ extendIfThen ifThen els

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
        reserved "in"
        src <- expression
        stepMeta <- meta <$> getPosition
        step <- option (IntLiteral stepMeta 1)
                       (do {reserved "by"; expression})
        return $ \body -> For{emeta, name, src, step, body}

      while = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "while"
        cond <- expression
        return $ \body -> While{emeta, cond, body}

      repeat = blockedConstruct $ do
        emeta <- meta <$> getPosition
        reserved "repeat"
        name <- Name <$> identifier
        symbol "<-"
        times <- expression
        return $ \body -> Repeat{emeta, name, times, body}

      reduce = do
        pos <- getPosition
        reserved "reduce"
        (f, i, p) <- parens (do
                             seqfun <- expression
                             comma
                             pinit <- expression
                             comma
                             par <- expression
                             return (seqfun, pinit, par))
        return $ PartyReduce (meta pos) f i p False

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

      -- TODO: What is the correct syntax here?
      extract = do
        emeta <- meta <$> getPosition
        reserved "extract"
        val <- expression
        return PartyExtract{emeta, val}

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

      suspend = do
        emeta <- meta <$> getPosition
        reserved "suspend"
        return Suspend{emeta}

      closure = do
        indent <- L.indentLevel
        funLine <- sourceLine <$> getPosition
        clos <- indentBlock $ do
          emeta <- meta <$> getPosition
          reserved "fun"
          eparams <- parens (commaSep paramDecl)
          -- TODO: Add optional type annotation
          singleLineClosure emeta eparams <|>
            blockClosure emeta eparams
        endLine <- sourceLine <$> getPosition
        unless (endLine == funLine) $
               atLevel indent $ reserved "end"
        return clos
      singleLineClosure emeta eparams = do
        reservedOp "=>"
        body <- expression
        return $ L.IndentNone Closure{emeta, eparams, body}
      blockClosure emeta eparams =
        return $ L.IndentSome Nothing (buildClosure emeta eparams) expression
      buildClosure emeta eparams block =
        return Closure{emeta
                      ,eparams
                      ,body = makeBody block
                      }

      -- TODO: Should we keep these three?
      task = do
        emeta <- meta <$> getPosition
        reserved "async"
        body <- expression
        return Async{emeta, body}

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

      peer = do
        emeta <- meta <$> getPosition
        reserved "peer"
        ty <- typ
        return Peer{emeta, ty}

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
        kind <- do symbol "u" <|> symbol "U"
                   return UIntLiteral
               <|> (hspace >> return IntLiteral)
        return $ kind emeta (fromInteger n)
      real = do
        emeta <- meta <$> getPosition
        realLit <- float
        return RealLiteral{emeta, realLit}

      bracketed = brackets (rangeOrArray <|> empty)
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
            arrayLit emeta first = singletonArray <|> longerArray
              where
                singletonArray = do
                  lookAhead (symbol "]")
                  return ArrayLiteral{emeta, args = [first]}
                longerArray = do
                  comma
                  rest <- commaSep1 expression
                  return ArrayLiteral{emeta, args = first:rest}