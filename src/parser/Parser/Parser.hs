{-|

Produces an "AST.AST" (or an error) of a @Program@ built from the
grammar found in @doc/encore/@

-}

module Parser.Parser(
                      parseEncoreProgram
                    , identifierParser
                    ) where

-- Library dependencies
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.Char(isUpper)
import Control.Applicative ((<$>))

-- Module dependencies
import Identifiers
import Types hiding(refType)
import AST.AST
import AST.Meta hiding(Closure, Async)

-- | 'parseEncoreProgram' @path@ @code@ assumes @path@ is the path
-- to the file being parsed and will produce an AST for @code@,
-- unless a parse error occurs.
parseEncoreProgram :: FilePath -> String -> Either ParseError Program
parseEncoreProgram = parse program

identifierParser = identifier

-- | This creates a tokenizer that reads a language derived from
-- the empty language definition 'emptyDef' extended as shown.
lexer =
 P.makeTokenParser $
 emptyDef {
   P.commentStart = "{-",
   P.commentEnd = "-}",
   P.commentLine = "--",
   P.identStart = letter,
   P.reservedNames = [
     "shared"
    ,"passive"
    ,"class"
    ,"def"
    ,"stream"
    ,"int"
    ,"string"
    ,"char"
    ,"real"
    ,"bool"
    ,"void"
    ,"let"
    ,"in"
    ,"if"
    ,"unless"
    ,"then"
    ,"else"
    ,"repeat"
    ,"for"
    ,"while"
    ,"get"
    ,"yield"
    ,"eos"
    ,"getNext"
    ,"new"
    ,"this"
    ,"await"
    ,"suspend"
    ,"and"
    ,"or"
    ,"not"
    ,"true"
    ,"false"
    ,"null"
    ,"embed"
    ,"body"
    ,"end"
    ,"where"
    ,"Fut"
    ,"Par"
    ,"Stream"
    ,"import"
    ,"qualified"
    ,"bundle"
    ,"peer"
    ,"finish"
    ,"trait"
    ,"require"
    ,"val"
    ,"Maybe"
    ,"Just"
    ,"Nothing"
    ,"match"
    ,"with"
    ,"when"
    ,"liftf"
    ,"liftv"
    ,"extract"
    ,"each"
    ,"typedef"
   ],
   P.reservedOpNames = [
     ":"
    ,"="
    ,"=="
    ,"!="
    ,"<"
    ,">"
    ,"<="
    ,">="
    ,"+"
    ,"-"
    ,"*"
    ,"/"
    ,"%"
    ,"->"
    ,".."
    ,"\\"
    ,"()"
    ,"~~>"
    ,"=>"
    ,"|"
    ]
  }

-- | These parsers use the lexer above and are the smallest
-- building blocks of the whole parser.
identifier = P.identifier lexer
symbol     = P.symbol lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
operator   = P.operator lexer
dot        = P.dot lexer
bang       = symbol "!"
bar        = symbol "|"
dotdot     = symbol ".."
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
colon      = P.colon lexer
semi       = P.semi lexer
semiSep    = P.semiSep lexer
comma      = P.comma lexer
parens     = P.parens lexer
angles     = P.angles lexer
brackets   = P.brackets lexer
braces     = P.braces lexer
maybeBraces p = braces p <|> p

stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer
natural = P.natural lexer
float = P.float lexer
whiteSpace = P.whiteSpace lexer

-- ! For parsing qualified names such as A.B.C
longidentifier :: Parser QName
longidentifier = do
    id <- identifier
    rest <- option [] (do { dot ; longidentifier })
    return $ Name id : rest

typ :: Parser Type
typ = buildExpressionParser opTable singleType
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
          Infix (do reservedOp op
                    return constructor) AssocLeft
      typeConstructor op constructor =
          Prefix (do reserved op
                     return constructor)
      arrow =
          Infix (do reservedOp "->"
                    return (arrowType . unfoldArgs)) AssocRight
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
        <|> parens typ
        <?> "type"
      tuple = do
        types <- (reservedOp "()" >> return [])
             <|> parens (typ `sepBy2` comma)
        return $ tupleType types
      array = do
        ty <- brackets typ
        return $ arrayType ty
      embed = do
        reserved "embed"
        ty <- manyTill anyChar $ try $ do {space; reserved "end"}
        return $ ctype ty
      range = do
        reserved "Range"
        return rangeType
      refType = do
        notFollowedBy lower
        refId <- identifier
        parameters <- option [] $ angles (commaSep1 typ)
        return $ refTypeWithParams refId parameters
      primitive =
        do {reserved "int"; return intType} <|>
        do {reserved "bool"; return boolType} <|>
        do {reserved "string"; return stringType} <|>
        do {reserved "char"; return charType} <|>
        do {reserved "real"; return realType} <|>
        do {reserved "void"; return voidType}

typeVariable :: Parser Type
typeVariable = do
  notFollowedBy upper
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
  whiteSpace
  bundle <- bundledecl
  imports <- many importdecl
  etls <- embedTL
  let etl = [etls]
  decls <- many $ (CDecl <$> classDecl) <|> (TDecl <$> traitDecl) <|> (TDef <$> typedef) <|> (FDecl <$> function) 
  let (classes, traits, typedefs, functions) = partitionDecls decls
  eof
  return Program{source, bundle, etl, imports, typedefs, functions, traits, classes}
    where
      hashbang = do string "#!"
                    many (noneOf "\n\r")

bundledecl :: Parser BundleDecl
bundledecl = option NoBundle $ do
  pos <- getPosition
  reserved "bundle"
  bname <- longidentifier
  reserved "where"
  return $ Bundle (meta pos) bname

importdecl :: Parser ImportDecl
importdecl = do
  pos <- getPosition
  reserved "import"
--  qualified <- option $ reserved "qualified"
  iname <- longidentifier
--  {(Name,...)}?
--  stringliteral "as"; qname <- name
--  {as Name}
  return $ Import (meta pos) iname

embedTL :: Parser EmbedTL
embedTL = do
  pos <- getPosition
  (try (do string "embed"
           header <- manyTill anyChar $ try $ do {space; string "body"}
           code <- manyTill anyChar $ try $ do {space; reserved "end"}
           return $ EmbedTL (meta pos) header code
       ) <|>
   try (do string "embed"
           header <- manyTill anyChar $ try $ do {space; reserved "end"}
           return $ EmbedTL (meta pos) header ""
       ) <|>
   (return $ EmbedTL (meta pos) "" ""))

typedef :: Parser Typedef
typedef = do
  typedefmeta <- meta <$> getPosition
  reserved "typedef"
  name <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  reservedOp "="
  typedeftype <- typ
  let typedefdef = typeSynonym name params typedeftype
  return Typedef{typedefmeta, typedefdef}

functionHeader :: Parser FunctionHeader
functionHeader = do
  hname <- Name <$> identifier
  hparams <- parens (commaSep paramDecl)
  colon
  htype <- typ
  return Header{kind = NonStreaming
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
   args <- parens (commaSep patternParamDecl)
   colon
   htype <- typ
   posGuard <- getPosition
   hguard <- option (BTrue (meta posGuard)) guard
   let hpatterns = map fst args
       hparamtypes = map snd args
   return MatchingHeader{kind = NonStreaming
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

function :: Parser Function
function =  try regularFunction <|> matchingFunction
  where
    regularFunction = do
      funmeta <- meta <$> getPosition
      reserved "def"
      funheader <- functionHeader
      funbody <- expression
      return Function{funmeta
                     ,funheader
                     ,funbody}
    matchingFunction = do
      funmeta <- meta <$> getPosition
      reserved "def"
      clauses <- functionClause `sepBy1` reservedOp "|"
      let matchfunheaders = map fst clauses
          matchfunbodies = map snd clauses
      return MatchingFunction{funmeta
                             ,matchfunheaders
                             ,matchfunbodies}
      where
        functionClause = do
          funheader <- matchingHeader
          funbody <- expression
          return (funheader, funbody)

traitDecl :: Parser TraitDecl
traitDecl = do
  tmeta <- meta <$> getPosition
  reserved "trait"
  ident <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  (treqs, tmethods) <- maybeBraces traitBody
  return Trait{tmeta
              ,tname = traitTypeFromRefType $
                       refTypeWithParams ident params
              ,treqs
              ,tmethods
              }
  where
    traitBody = do
      reqs    <- many (try reqField <|> reqMethod <?> "requirement")
      methods <- many methodDecl
      return (reqs, methods)
    reqField = do
      rmeta <- meta <$> getPosition
      reserved "require"
      rfield <- fieldDecl
      return RequiredField{rmeta
                          ,rfield
                          }
    reqMethod = do
      rmeta <- meta <$> getPosition
      reserved "require"
      rheader <- functionHeader
      return RequiredMethod{rmeta
                           ,rheader
                           }

classDecl :: Parser ClassDecl
classDecl = do
  cmeta <- meta <$> getPosition
  activity <- parseActivity
  reserved "class"
  name <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  ccapability <- option incapability (do{reservedOp ":"; typ})
  (cfields, cmethods) <- maybeBraces classBody
  return Class{cmeta
              ,cname = classType activity name params
              ,ccapability
              ,cfields
              ,cmethods
              }
  where
    parseActivity = (reserved "shared" >> return Shared)
      <|> (reserved "passive" >> return Passive)
      <|> return Active
    classBody = do
              fields <- many fieldDecl
              methods <- many methodDecl
              return (fields, methods)

modifier :: Parser Modifier
modifier = val
           <?>
           "modifier"
    where
      val = do
        reserved "val"
        return Val

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
paramDecl = do pos <- getPosition
               x <- identifier
               colon
               ty <- typ
               return $ Param (meta pos) (Name x) ty

patternParamDecl :: Parser (Expr, Type)
patternParamDecl = do
  x <- highOrderExpr
  colon
  ty <- typ
  return (x, ty)

methodDecl :: Parser MethodDecl
methodDecl = try regularMethod <|> matchingMethod
  where
    regularMethod = do
      mmeta <- meta <$> getPosition
      mheader <- do reserved "def"
                    functionHeader
             <|> do reserved "stream"
                    streamMethodHeader
      mbody <- expression
      return Method{mmeta
                   ,mheader
                   ,mbody
                   }
    matchingMethod = do
      mmeta <- meta <$> getPosition
      clauses <- do reserved "def"
                    methodClause matchingHeader `sepBy1` reservedOp "|"
             <|> do reserved "stream"
                    methodClause matchingStreamHeader `sepBy1` reservedOp "|"
      let mheaders = map fst clauses
          mbodies = map snd clauses
      return MatchingMethod{mmeta
                           ,mheaders
                           ,mbodies
                           }
      where
        methodClause headerParser= do
          mheader <- headerParser
          mbody <- expression
          return (mheader, mbody)

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
  reserved "=>"
  mchandler <- expression
  return MatchClause{mcpattern, mcguard, mchandler}
    where
      dontCare = do pos <- getPosition
                    symbol "_"
                    return (VarAccess (meta pos) (Name "_"))

expression :: Parser Expr
expression = buildExpressionParser opTable highOrderExpr
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
          Infix (try(do pos <- getPosition
                        reserved s
                        return (Binop (meta pos) binop))) AssocLeft
      prefix s operator =
          Prefix (do pos <- getPosition
                     reservedOp s
                     return (Unary (meta pos) operator))
      op s binop =
          Infix (do pos <- getPosition
                    reservedOp s
                    return (Binop (meta pos) binop)) AssocLeft

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
                      args <- parens arguments
                      return (\target -> MessageSend (meta pos) target
                                                     (Name name) args))
      chain =
          Infix (do pos <- getPosition ;
                    reservedOp "~~>" ;
                    return (FutureChain (meta pos))) AssocLeft
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
          Infix (do pos <- getPosition ;
                    reservedOp ">>" ;
                    return (PartySeq (meta pos))) AssocLeft
      partyParallel =
          Infix (do pos <- getPosition ;
                    reservedOp "||" ;
                    return (PartyPar (meta pos))) AssocLeft
      partyJoin =
          Prefix (do pos <- getPosition
                     reserved "join"
                     return (PartyJoin (meta pos)))
      assignment =
          Infix (do pos <- getPosition ;
                    reservedOp "=" ;
                    return (Assign (meta pos))) AssocRight


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
expr  =  unit
     <|> try embed
     <|> try path
     <|> try functionCall
     <|> try print
     <|> try closure
     <|> try tupleLit
     <|> match
     <|> task
     <|> finishTask
     <|> for
     <|> foreach
     <|> extract
     <|> parens expression
     <|> varAccess
     <|> arraySize
     <|> try rangeLit
     <|> arrayLit
     <|> letExpression
     <|> try ifThenElse
     <|> ifThen
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
     <|> try newWithInit
     <|> new
     <|> peer
     <|> null
     <|> true
     <|> false
     <|> sequence
     <|> stringLit
     <|> charLit
     <|> try real
     <|> int
     <?> "expression"
    where
      embed = do pos <- getPosition
                 reserved "embed"
                 ty <- typ
                 code <- manyTill anyChar $ try $ do {space; reserved "end"}
                 return $ Embed (meta pos) ty code
      unit = do pos <- getPosition
                reservedOp "()"
                return $ Skip (meta pos)
      path = do pos <- getPosition
                root <- parens expression <|> try functionCall <|> varAccess <|> stringLit
                first <- pathComponent
                rest <- many $ try pathComponent
                return $ foldl (buildPath pos) root (first:rest)
             where
               pathComponent = do {dot; (try functionCall <|> varAccess)}
               buildPath pos target (VarAccess{name}) =
                   FieldAccess (meta pos) target name
               buildPath pos target (FunctionCall{name, args}) =
                   MethodCall (meta pos) target name args
      letExpression = do pos <- getPosition
                         reserved "let"
                         decls <- many varDecl
                         reserved "in"
                         expr <- expression
                         return $ Let (meta pos) decls expr
                      where
                        varDecl = do x <- identifier
                                     reservedOp "="
                                     val <- expression
                                     return (Name x, val)
      sequence = do pos <- getPosition
                    seq <- braces ((try expression <|> miniLet) `sepEndBy1` semi)
                    return $ Seq (meta pos) seq
          where
            miniLet = do
              emeta <- meta <$> getPosition
              reserved "let"
              x <- Name <$> identifier
              reservedOp "="
              val <- expression
              return MiniLet{emeta, decl = (x, val)}
      ifThenElse = do pos <- getPosition
                      reserved "if"
                      cond <- expression
                      reserved "then"
                      thn <- expression
                      reserved "else"
                      els <- expression
                      return $ IfThenElse (meta pos) cond thn els
      ifThen = do pos <- getPosition
                  reserved "if"
                  cond <- expression
                  reserved "then"
                  thn <- expression
                  return $ IfThen (meta pos) cond thn
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
      match = do pos <- getPosition
                 reserved "match"
                 arg <- expression
                 reserved "with"
                 clauses <- maybeBraces $ many matchClause
                 return $ Match (meta pos) arg clauses
      tupleLit = do pos <- getPosition
                    args <- parens (expression `sepBy2` comma)
                    return $ Tuple (meta pos) args
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
      functionCall = do pos <- getPosition
                        fun <- identifier
                        args <- parens arguments
                        return $ FunctionCall (meta pos) (Name fun) args
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
      finishTask = do pos <- getPosition
                      reserved "finish"
                      body <- expression
                      return $ FinishAsync (meta pos) body
      varAccess = do pos <- getPosition
                     id <- (do reserved "this"; return "this") <|> identifier
                     return $ VarAccess (meta pos) $ Name id
      arraySize = do pos <- getPosition
                     bar
                     arr <- expression
                     bar
                     return $ ArraySize (meta pos) arr
      arrayLit = do pos <- getPosition
                    args <- brackets $ commaSep expression
                    return $ ArrayLiteral (meta pos) args
      null = do pos <- getPosition
                reserved "null"
                return $ Null (meta pos)
      true = do pos <- getPosition
                reserved "true"
                return $ BTrue (meta pos)
      false = do pos <- getPosition
                 reserved "false"
                 return $ BFalse (meta pos)
      newWithInit = do pos <- getPosition
                       reserved "new"
                       ty <- typ
                       args <- parens arguments
                       return $ NewWithInit (meta pos) ty args
      new = do pos <- getPosition
               reserved "new"
               ty <- typ
               return $ New (meta pos) ty
      peer = do pos <- getPosition
                reserved "peer"
                ty <- typ
                return $ Peer (meta pos) ty
      print = do pos <- getPosition
                 reserved "print"
                 arg <- option [] ((:[]) <$> expression)
                 return $ FunctionCall (meta pos) (Name "print") arg
      stringLit = do pos <- getPosition
                     string <- stringLiteral
                     return $ StringLiteral (meta pos) string
      charLit = do pos <- getPosition
                   char <- charLiteral
                   return $ CharLiteral (meta pos) char
      int = do pos <- getPosition
               n <- natural
               return $ IntLiteral (meta pos) (fromInteger n)
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
      rangeLit = brackets range
      range = do pos <- getPosition
                 start <- expression
                 dotdot
                 stop <- expression
                 step <- option (IntLiteral (meta pos) 1)
                                (do {reserved "by"; expression})
                 return $ RangeLiteral (meta pos) start stop step
