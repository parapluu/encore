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
import Data.Either (partitionEithers)
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
     "shared", "passive", "class", "def", "stream", "breathe", "int", "string",
     "real", "bool", "void", "let", "in", "if", "unless", "then", "else",
     "repeat", "for", "while", "get", "yield", "eos", "getNext", "new", "this",
     "await", "suspend", "and", "or", "not", "true", "false", "null", "embed",
     "body", "end", "where", "Fut", "Par", "Stream", "import", "qualified",
     "bundle", "peer", "async", "finish", "foreach", "trait", "require", "val",
     "Maybe", "Just", "Nothing", "match", "with", "when","liftf", "liftv", "linear",
     "extract", "consume", "unsafe", "S", "break"
   ],
   P.reservedOpNames = [
     ":", "=", "==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "->", "..",
     "\\", "()", "~~>", "=>"
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
integer = P.integer lexer
float = P.float lexer
whiteSpace = P.whiteSpace lexer

-- ! For parsing qualified names such as A.B.C
longidentifier :: Parser QName
longidentifier = do
    id <- identifier
    rest <- option [] (do { dot ; longidentifier })
    return $ Name id : rest

-- Note, when we have product types (i.e. tuples), we could make
-- an expressionParser for types (with arrow as the only infix
-- operator.)
typ :: Parser Type
typ  = adtTypes
       <|> firstOrderTyp
    where
      adtTypes = maybe
      firstOrderTyp = try arrow
                   <|> try tuple
                   <|> parens typ
                   <|> nonArrow
                   <?> "type"
      nonArrow =  fut
              <|> par
              <|> stream
              <|> array
              <|> embed
              <|> primitive
              <|> range
              <|> capabilityOrRef
              <|> stackbound
              <|> typeVariable
              <|> parens nonArrow
      arrow = do lhs <- parens (commaSep typ)
                     <|> do {ty <- nonArrow ; return [ty]}
                 reservedOp "->"
                 rhs <- nonArrow
                 return $ arrowType lhs rhs
      fut = do reserved "Fut"
               ty <- typ
               return $ futureType ty
      maybe = do
         reserved "Maybe"
         ty <- firstOrderTyp
         return $ maybeType ty
      tuple = do
        ty <- parens (typ `sepBy2` comma)
        return $ tupleType ty
      par = do reserved "Par"
               ty <- typ
               return $ parType ty
      stream = do reserved "Stream"
                  ty <- typ
                  return $ streamType ty
      array = do ty <- brackets typ
                 return $ arrayType ty
      range = do reserved "Range"
                 return rangeType
      embed = do reserved "embed"
                 ty <- manyTill anyChar $ try $ do {space; reserved "end"}
                 return $ ctype ty
      stackbound = do reserved "borrowed"
                      ty <- parens typ
                      return $ makeStackbound ty
      capabilityOrRef = do
        cap <- capability
        if isSingleCapability cap
        then return $ refTypeFromSingletonCapability cap
        else return cap
      primitive = do {reserved "int"; return intType} <|>
                  do {reserved "bool"; return boolType} <|>
                  do {reserved "string"; return stringType} <|>
                  do {reserved "char"; return charType} <|>
                  do {reserved "real"; return realType} <|>
                  do {reserved "void"; return voidType}

typeVariable :: Parser Type
typeVariable = do
  mode <- option id mode
  notFollowedBy upper
  id <- identifier
  return $ mode (typeVar id)
  <?> "lower case type variable"

program :: Parser Program
program = do
  source <- sourceName <$> getPosition
  optional hashbang
  whiteSpace
  bundle <- bundledecl
  imports <- many importdecl
  etl <- embedTL
  functions <- many function
  decls <- many $ (Left <$> traitDecl) <|> (Right <$> classDecl)
  let (traits, classes) = partitionEithers decls
  eof
  return Program{source, bundle, etl, imports, functions, traits, classes}
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
  try (embedWithBody pos) <|>
      try (embedWithoutBody pos) <|>
      return (EmbedTL (meta pos) "" "")
    where
      embedWithBody pos = do
            string "embed"
            header <- manyTill anyChar $ try $ do {space; string "body"}
            code <- manyTill anyChar $ try $ do {space; reserved "end"}
            return $ EmbedTL (meta pos) header code
      embedWithoutBody pos = do
            string "embed"
            header <- manyTill anyChar $ try $ do {space; reserved "end"}
            return $ EmbedTL (meta pos) header ""

functionHeader :: Parser FunctionHeader
functionHeader = do
  hname <- Name <$> identifier
  hparams <- parens (commaSep paramDecl)
  colon
  htype <- typ
  return FunctionHeader{hname, hparams, htype}

methodHeader :: Parser FunctionHeader
methodHeader = do
  FunctionHeader{hname, hparams, htype} <- functionHeader
  return MethodHeader{hname, hparams, htype}

streamMethodHeader :: Parser FunctionHeader
streamMethodHeader = do
  FunctionHeader{hname, hparams, htype} <- functionHeader
  return StreamMethodHeader{hname, hparams, htype}

function :: Parser Function
function = do
  funmeta <- meta <$> getPosition
  reserved "def"
  funheader <- functionHeader
  funbody <- expression
  return Function{funmeta
                 ,funheader
                 ,funbody
                 }

mode :: Parser (Type -> Type)
mode = linear
       <|>
       unsafe
       <|>
       read
       <|>
       safe
       <?> "mode"
    where
      linear = do reserved "linear"
                  return makeLinear
      unsafe = do reserved "unsafe"
                  return makeUnsafe
      read   = do reserved "read"
                  return makeRead
      safe   = do reserved "safe"
                  return makeSafe

traitDecl :: Parser TraitDecl
traitDecl = do
  tmeta <- meta <$> getPosition
  mode <- option id mode
  reserved "trait"
  ident <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  (treqs, tmethods) <- maybeBraces traitBody
  return Trait{tmeta
              ,tname = mode $
                       traitTypeFromRefType $
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
      rheader <- methodHeader
      return RequiredMethod{rmeta
                           ,rheader
                           }

capability :: Parser Type
capability = do
  tree <- typeTree
  return $ capabilityType tree
  where
    typeTree :: Parser TypeTree
    typeTree = do
     ts <- productType `sepBy1` reservedOp "+"
     return $ RoseTree Addition ts

    productType :: Parser TypeTree
    productType = do
     ts <- term `sepBy1` reservedOp "*"
     return $ RoseTree Product ts

    term :: Parser TypeTree
    term = parens typeTree
        <|> leafType

    leafType :: Parser TypeTree
    leafType = do
      mode <- option id mode
      notFollowedBy lower
      refId <- identifier
      parameters <- option [] $ angles (commaSep1 typ)
      return . typeTreeFromRefType . mode $
          refTypeWithParams refId parameters
      <?> "capability atom"

classDecl :: Parser ClassDecl
classDecl = do
  cmeta <- meta <$> getPosition
  activity <- parseActivity
  reserved "class"
  name <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  ccapability <- option incapability (do{reservedOp ":"; capability})
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

methodDecl :: Parser MethodDecl
methodDecl = do mmeta <- meta <$> getPosition
                mheader <- do reserved "def"
                              methodHeader
                       <|> do reserved "stream"
                              streamMethodHeader
                mbody <- expression
                return Method{mmeta
                             ,mheader
                             ,mbody
                             }

arguments :: Parser Arguments
arguments = expression `sepBy` comma

sepBy2 p sep = do first <- p
                  sep
                  last <- p `sepBy1` sep
                  return $ first:last

matchClause :: Parser MatchClause
matchClause = do pos <- getPosition
                 lhs <- expression <|> dontCare
                 posGuard <- getPosition
                 grd <- option (BTrue (meta posGuard)) guard
                 reserved "=>"
                 rhs <- expression
                 return $ MatchClause (meta pos) lhs rhs grd
    where
      guard = do reserved "when"
                 expression
      dontCare = do pos <- getPosition
                    symbol "_"
                    return (VarAccess (meta pos) (Name "_"))

expression :: Parser Expr
expression = buildExpressionParser opTable highOrderExpr
    where
      opTable = [
                 [arrayAccess],
                 [textualPrefix "not" Identifiers.NOT],
                 [op "*" TIMES, op "/" DIV, op "%" MOD],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" Identifiers.LT, op ">" Identifiers.GT,
                  op "<=" Identifiers.LTE, op ">=" Identifiers.GTE,
                  op "==" Identifiers.EQ, op "!=" NEQ],
                 [textualOperator "and" Identifiers.AND,
                  textualOperator "or" Identifiers.OR],
                 [messageSend],
                 [consume],
                 [partyLiftf, partyLiftv],
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
      consume =
          Prefix (do pos <- getPosition
                     reserved "consume"
                     return (Consume (meta pos)))
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
     <|> breathe
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
      breathe = do pos <- getPosition
                   reserved "breathe"
                   return $ Breathe (meta pos)
      path = do pos <- getPosition
                root <- parens expression <|> try functionCall <|> varAccess <|> stringLit
                dot
                path <- (try functionCall <|> varAccess) `sepBy1` dot
                return $ foldl (buildPath pos) root path
             where
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
                 return $ Print (meta pos) arg
      stringLit = do pos <- getPosition
                     string <- stringLiteral
                     return $ StringLiteral (meta pos) string
      charLit = do pos <- getPosition
                   char <- charLiteral
                   return $ CharLiteral (meta pos) char
      int = do pos <- getPosition
               n <- integer
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
