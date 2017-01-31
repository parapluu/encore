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
import Control.Monad(void, foldM, unless)
import Control.Applicative ((<$>))
import Control.Arrow (first)

-- Module dependencies
import Identifiers hiding(namespace)
import Types hiding(refType)
import AST.AST
import AST.Meta hiding(Closure, Async, getPos)

-- | 'parseEncoreProgram' @path@ @code@ assumes @path@ is the path
-- to the file being parsed and will produce an AST for @code@,
-- unless a parse error occurs.
parseEncoreProgram = parse program

lineComment = L.skipLineComment "--"
blockComment = L.skipBlockComment "{-" "-}"

-- | A "space consumer", used for parsing non-linebreaking white-space.
sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment blockComment

-- | A "space consumer", used for parsing white-space, including line breaks.
scn :: Parser ()
scn = L.space (void spaceChar) lineComment blockComment

-- | Parse a newline and any whitespace following it (including
-- additional newlines)
nl :: Parser ()
nl = newline >> scn

-- | @atLevel ind p@ parses @p@ if the current indentation level
-- is @lvl@, and fails otherwise
atLevel :: Pos -> Parser a -> Parser a
atLevel expectedIndent p = do
  currentIndent <- L.indentLevel
  unless (currentIndent == expectedIndent) $
         L.incorrectIndent Prelude.EQ expectedIndent currentIndent
  p

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = char '\'' *> L.charLiteral <* char '\'' <* sc

-- TODO: What about escape sequences? e.g. \n
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* sc

-- TODO: Maybe we want to read negative numbers directly (instead
-- of going via unary minus)
natural :: Parser Integer
natural = lexeme L.integer

float :: Parser Double
float = lexeme L.float

-- | These parsers use the lexer above and are the smallest
-- building blocks of the whole parser.
reservedNames =
    ["Fut"
    ,"Just"
    ,"Maybe"
    ,"Nothing"
    ,"Par"
    ,"Stream"
    ,"and"
    ,"await"
    ,"body"
    ,"bool"
    ,"char"
    ,"class"
    ,"def"
    ,"each"
    ,"else"
    ,"EMBED"
    ,"END"
    ,"end"
    ,"eos"
    ,"extract"
    ,"false"
    ,"for"
    ,"get"
    ,"getNext"
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

validIdentifierChar :: Parser Char
validIdentifierChar = alphaNumChar <|> char '_' <|> char '\''

validOpChar :: Parser Char
validOpChar = oneOf ".:=!<>+-*/%\\~|"

reserved :: String -> Parser ()
reserved w = try $ string w *> notFollowedBy validIdentifierChar *> sc

reservedOp :: String -> Parser ()
reservedOp op = try $ string op <* notFollowedBy validOpChar *> sc

identifier :: Parser String
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
angles     = between (symbol "<") (symbol ">")
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

modulePath :: Parser [Name]
modulePath =
    (Name <$> (lookAhead upperChar >> identifier)) `sepBy1`
    try (dot >> lookAhead upperChar)

typ :: Parser Type
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
        parameters <- option [] $ angles (commaSep1 typ)
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

typeVariable :: Parser Type
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

program :: Parser Program
program = do
  source <- sourceName <$> getPosition
  optional hashbang
  scn

  moduledecl <- L.nonIndented sc moduleDecl
  unless (moduledecl == NoModule) nl

  imports <- L.nonIndented sc importdecl `endBy` nl
  etls <- L.nonIndented sc embedTL
  let etl = [etls]
  scn

  decls <- (`sepEndBy` nl) . foldr1 (<|>) $
           map (L.nonIndented sc)
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

moduleDecl :: Parser ModuleDecl
moduleDecl = option NoModule $ do
  modmeta <- meta <$> getPosition
  reserved "module"
  lookAhead upperChar
  modname <- Name <$> identifier
  -- TODO: Allow linebreaks
  modexports <- optional (parens ((Name <$> identifier) `sepEndBy` comma))
  return Module{modmeta
               ,modname
               ,modexports
               }

importdecl :: Parser ImportDecl
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

embedTL :: Parser EmbedTL
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

optionalTypeParameters = option [] (angles $ commaSep1 typ)

typedef :: Parser Typedef
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

functionHeader :: Parser FunctionHeader
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

streamMethodHeader :: Parser FunctionHeader
streamMethodHeader = do
  header <- functionHeader
  return header{kind = Streaming}

guard :: Parser Expr
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

matchingStreamHeader :: Parser FunctionHeader
matchingStreamHeader = do
  header <- matchingHeader
  return header{kind = Streaming}

whereClause :: Parser [Function]
whereClause =
  L.indentBlock scn $ do
    reserved "where"
    return $ L.IndentSome Nothing return localFunction

localFunction :: Parser Function
localFunction = do
  funIndent <- L.indentLevel
  fun <- funHeaderAndBody
  atLevel funIndent $ reserved "end"
  return fun

globalFunction :: Parser Function
globalFunction = do
  funIndent <- L.indentLevel
  fun <- funHeaderAndBody

  funlocals <- option [] $ atLevel funIndent whereClause

  atLevel funIndent $ reserved "end"

  return fun{funlocals}

funHeaderAndBody =
  -- TODO: Re-add matching functions
  L.indentBlock scn $ do
    funmeta <- meta <$> getPosition
    reserved "fun"
    funheader <- functionHeader
    return $ L.IndentSome Nothing (buildFun funmeta funheader) expression
  where
    buildFun funmeta funheader bodyblock =
      let funbody =
            case bodyblock of
              [e] -> e
              e:es -> Seq (meta (getPos e)) (e:es)
      in return Function{funmeta
                        ,funheader
                        ,funbody
                        ,funlocals = []
                        ,funsource = ""
                        }

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


traitDecl :: Parser TraitDecl
traitDecl = do
  tIndent <- L.indentLevel
  tdecl <- L.indentBlock scn $ do
    tmeta <- meta <$> getPosition
    reserved "trait"
    ident <- lookAhead upperChar >> identifier
    params <- optionalTypeParameters
    return $ L.IndentMany
               Nothing
               (buildTrait tmeta ident params)
               traitAttribute
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

traitComposition :: Parser TraitComposition
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
        parameters <- option [] $ angles (commaSep1 typ)
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


classDecl :: Parser ClassDecl
classDecl = do
  cIndent <- L.indentLevel
  cdecl <- L.indentBlock scn $ do
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

modifier :: Parser Modifier
modifier = val
           <?>
           "modifier"
    where
      val = do
        reserved "val"
        return MVal

fieldDecl :: Parser FieldDecl
fieldDecl = do fmeta <- meta <$> getPosition
               fmods <- many modifier
               fname <- Name <$> identifier
               colon
               ftype <- typ
               return Field{fmeta
                           ,fmods
                           ,fname
                           ,ftype}

paramDecl :: Parser ParamDecl
paramDecl = do
  pmeta <- meta <$> getPosition
  pmut <- option Val $
              (reserved "var" >> return Var)
          <|> (reserved "val" >> return Val)
  pname <- Name <$> identifier
  colon
  ptype <- typ
  return Param{pmeta, pmut, pname, ptype}

patternParamDecl :: Parser (Expr, Type)
patternParamDecl = do
  x <- highOrderExpr
  colon
  ty <- typ
  return (x, ty)

methodDecl :: Parser MethodDecl
methodDecl = do
  mIndent <- L.indentLevel
  -- TODO: Re-add support for matching methods
  mtd <- methodHeaderAndBody -- <|> matchingMethod

  mlocals <- option [] $ atLevel mIndent whereClause
  atLevel mIndent $ reserved "end"
  return mtd{mlocals}
  where
    methodHeaderAndBody =
      L.indentBlock scn $ do
        mmeta <- meta <$> getPosition
        mheader <- do reserved "def"
                      modifiers <- option [Public] $ many modifiersDecl
                      setHeaderModifier modifiers <$> functionHeader
               <|> do reserved "stream"
                      streamMethodHeader
        return $ L.IndentSome Nothing (buildMethod mmeta mheader) expression
    buildMethod mmeta mheader bodyblock =
      let mbody =
            case bodyblock of
              [e] -> e
              e:es -> Seq (meta (getPos e)) (e:es)
      in return Method{mmeta
                      ,mheader
                      ,mbody
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

modifiersDecl :: Parser AccessModifier
modifiersDecl = reserved "private" >> return Private


arguments :: Parser Arguments
arguments = expression `sepBy` comma

sepBy2 p sep = do first <- p
                  sep
                  last <- p `sepBy1` sep
                  return $ first:last

matchClause :: Parser MatchClause
matchClause = do
  pos <- getPosition
  mcpattern <- expression <|> dontCare
  posGuard <- getPosition
  mcguard <- option (BTrue (meta posGuard)) guard
  reservedOp "=>"
  mchandler <- expression
  return MatchClause{mcpattern, mcguard, mchandler}
    where
      dontCare = do pos <- getPosition
                    symbol "_"
                    return (VarAccess (meta pos) (qName "_"))

expression :: Parser Expr
expression = makeExprParser highOrderExpr opTable
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
                 [textualOperator "and" Identifiers.AND,
                  textualOperator "or" Identifiers.OR],
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
      textualOperator s binop =
          InfixL (try(do pos <- getPosition
                         reserved s
                         return (Binop (meta pos) binop)))
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
                      optTypeArgs <- option [] (try . angles $ commaSep typ)
                      args <- parens arguments
                      return (\target -> MessageSend { emeta = (meta pos),
                                                       typeArguments=optTypeArgs,
                                                       target,
                                                       name=(Name name),
                                                       args }))
      chain =
          InfixL (do pos <- getPosition ;
                     reservedOp "~~>" ;
                     return (FutureChain (meta pos)))
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


highOrderExpr :: Parser Expr
highOrderExpr = adtExpr
                <|> expr
  where
    adtExpr = justExpr
              <|> nothingExpr
    justExpr = do
      pos <- getPosition
      reserved "Just"
      body <- expr <|> nothingExpr
      return $ MaybeValue (meta pos) (JustData body)
    nothingExpr = do
      pos <- getPosition
      reserved "Nothing"
      return $ MaybeValue (meta pos) NothingData


expr :: Parser Expr
expr  =  embed
     <|> try print
     <|> closure
     <|> reduce
     <|> match
     <|> task
     <|> for
     <|> foreach
     <|> extract
     <|> arraySize
     <|> bracketed
     <|> letExpression
     <|> ifExpression
     <|> unless
     <|> repeat
     <|> while
     <|> get
     <|> yield
     <|> try isEos
     <|> eos
     <|> getNext
     <|> await
     <|> suspend
     <|> yield
     <|> new
     <|> peer
     <|> sequence
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
      embed = do pos <- getPosition
                 reserved "EMBED"
                 ty <- label "parenthesized type" $
                       parens typ
                 embedded <- many cAndEncore
                 embedEnd
                 return $ Embed (meta pos) ty embedded
              where
                cAndEncore :: Parser (String, Expr)
                cAndEncore = (do
                  code <- c
                  pos <- getPosition
                  e <- option (Skip (meta pos))
                       (try $ encoreEscaped expression)
                  return (code, e))
                  <|> (do
                        e <- encoreEscaped expression
                        return ("", e))
                c = do
                  notFollowedBy (embedEnd <|> encoreEscapeStart)
                  first <- anyChar
                  rest <-
                    manyTill anyChar (try $ lookAhead (embedEnd <|>
                                                       encoreEscapeStart))
                  return (first:rest)
                embedEnd = spaceChar >> reserved "END" >> return "END"

      path = do pos <- getPosition
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
                 optTypeArgs <- option [] (try . angles $ commaSep typ)
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
                 typeParams <- option [] (try . angles $ commaSep typ)
                 args <- parens arguments
                 return $ FunctionCall emeta typeParams qname args

               buildPath pos target (VarAccess{qname}) =
                   FieldAccess (meta pos) target (qnlocal qname)

               buildPath pos target (FunctionCall{qname, args, typeArguments}) =
                   MethodCall (meta pos) typeArguments target (qnlocal qname) args

      letExpression = do pos <- getPosition
                         reserved "let"
                         decls <- many varDecl
                         reserved "in"
                         expr <- expression
                         return $ Let (meta pos) Val decls expr
                      where
                        varDecl = do x <- identifier
                                     reservedOp "="
                                     val <- expression
                                     return (Name x, val)
      sequence = do pos <- getPosition
                    seq <- braces ((miniLet <|> expression) `sepEndBy1` semi)
                    return $ Seq (meta pos) seq
          where
            miniLet = do
              emeta <- meta <$> getPosition
              mutability <- (reserved "var" >> return Var)
                        <|> (reserved "val" >> return Val)
              x <- Name <$> identifier
              reservedOp "="
              val <- expression
              lookAhead semi
              return MiniLet{emeta, mutability, decl = (x, val)}
      ifExpression = do pos <- getPosition
                        reserved "if"
                        cond <- expression
                        reserved "then"
                        thn <- expression
                        (do reserved "else"
                            els <- expression
                            return $ IfThenElse (meta pos) cond thn els
                         ) <|>
                         (return $ IfThen (meta pos) cond thn)
      repeat = do pos <- getPosition
                  reserved "repeat"
                  name <- identifier
                  symbol "<-"
                  times <- expression
                  body <- expression
                  return $ Repeat (meta pos) (Name name) times body
      unless = do pos <- getPosition
                  reserved "unless"
                  cond <- expression
                  reserved "then"
                  thn <- expression
                  return $ Unless (meta pos) cond thn
      while = do pos <- getPosition
                 reserved "while"
                 cond <- expression
                 expr <- expression
                 return $ While (meta pos) cond expr
      extract = do pos <- getPosition
                   reserved "extract"
                   expr <- expression
                   return $ PartyExtract (meta pos) expr
      reduce = do pos <- getPosition
                  reserved "reduce"
                  (f, i, p) <- parens (do
                                       seqfun <- expression
                                       comma
                                       pinit <- expression
                                       comma
                                       par <- expression
                                       return (seqfun, pinit, par))
                  return $ PartyReduce (meta pos) f i p False
      match = do pos <- getPosition
                 reserved "match"
                 arg <- expression
                 reserved "with"
                 clauses <- maybeBraces $ many matchClause
                 return $ Match (meta pos) arg clauses
      get = do pos <- getPosition
               reserved "get"
               expr <- expression
               return $ Get (meta pos) expr
      getNext = do pos <- getPosition
                   reserved "getNext"
                   expr <- expression
                   return $ StreamNext (meta pos) expr
      yield = do pos <- getPosition
                 reserved "yield"
                 expr <- expression
                 return $ Yield (meta pos) expr
      isEos = do pos <- getPosition
                 reserved "eos"
                 expr <- expression
                 return $ IsEos (meta pos) expr
      eos = do pos <- getPosition
               reserved "eos"
               return $ Eos (meta pos)
      await = do pos <- getPosition
                 reserved "await"
                 expr <- expression
                 return $ Await (meta pos) expr
      suspend = do pos <- getPosition
                   reserved "suspend"
                   return $ Suspend (meta pos)
      closure = do pos <- getPosition
                   reservedOp "\\"
                   params <- parens (commaSep paramDecl)
                   reservedOp "->"
                   body <- expression
                   return $ Closure (meta pos) params body
      task = do pos <- getPosition
                reserved "async"
                body <- expression
                return $ Async (meta pos) body
      foreach = do pos <- getPosition
                   reserved "foreach"
                   item <- identifier
                   reserved "in"
                   arr <- expression
                   body <- expression
                   return $ Foreach (meta pos) (Name item) arr body
      arraySize = do pos <- getPosition
                     bar
                     arr <- expression
                     bar
                     return $ ArraySize (meta pos) arr
      nullLiteral = do pos <- getPosition
                       reserved "null"
                       return $ Null (meta pos)
      true = do pos <- getPosition
                reserved "true"
                return $ BTrue (meta pos)
      false = do pos <- getPosition
                 reserved "false"
                 return $ BFalse (meta pos)
      new = do pos <- getPosition
               reserved "new"
               ty <- typ
               (do notFollowedBy (symbol "(")
                   return $ New (meta pos) ty
                ) <|>
                do args <- parens arguments
                   return $ NewWithInit (meta pos) ty args
      peer = do pos <- getPosition
                reserved "peer"
                ty <- typ
                return $ Peer (meta pos) ty
      print = do pos <- getPosition
                 reserved "print"
                 notFollowedBy (symbol "(" >> symbol "\"")
                 arg <- option [] ((:[]) <$> expression)
                 return $ FunctionCall (meta pos) []
                                       (qName "println") arg
      stringLit = do pos <- getPosition
                     string <- stringLiteral
                     return $ StringLiteral (meta pos) string
      charLit = do pos <- getPosition
                   char <- charLiteral
                   return $ CharLiteral (meta pos) char
      int = do pos <- getPosition
               n <- natural
               kind <- do symbol "u" <|> symbol "U"
                          return UIntLiteral
                    <|> return IntLiteral
               return $ kind (meta pos) (fromInteger n)
      real = do pos <- getPosition
                r <- float
                return $ RealLiteral (meta pos) r
      for = do pos <- getPosition
               reserved "for"
               name <- identifier
               reserved "in"
               src <- expression
               step <- option (IntLiteral (meta pos) 1)
                              (do {reserved "by"; expression})
               body <- expression
               return $ For (meta pos) (Name name) step src body
      bracketed = brackets (rangeOrArray <|> empty)
          where
            empty = do
              pos <- getPosition
              lookAhead (symbol "]")
              return $ ArrayLiteral (meta pos) []
            rangeOrArray = do
              pos <- getPosition
              first <- expression
              range pos first
               <|> arrayLit pos first
            range pos start = do
              dotdot
              stop <- expression
              step <- option (IntLiteral (meta pos) 1)
                             (reserved "by" >> expression)
              return $ RangeLiteral (meta pos) start stop step
            arrayLit pos first = (do
              lookAhead (symbol "]")
              return (ArrayLiteral (meta pos) [first])
              ) <|> do
              comma
              rest <- commaSep1 expression
              return $ ArrayLiteral (meta pos) (first:rest)
