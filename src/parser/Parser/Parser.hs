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
     "passive", "class", "def", "stream", "breathe", "int", "string", "real",
     "bool", "void", "let", "in", "if", "unless", "then", "else", "repeat", "for",
     "while", "get", "yield", "eos", "getNext", "new", "this", "await",
     "suspend", "and", "or", "not", "true", "false", "null", "embed", "body",
     "end", "where", "Fut", "Par", "Stream", "import", "qualified", "bundle",
     "peer", "async", "finish", "foreach", "trait", "require", "val",
     "Maybe", "Just", "Nothing", "match", "with"
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
-- operator)
typ :: Parser Type
typ  = adtTypes
       <|> firstOrderTyp
    where
      adtTypes = maybe
      firstOrderTyp = try arrow
                   <|> parens typ
                   <|> nonArrow
                   <?> "type"
      nonArrow =  fut
              <|> par
              <|> stream
              <|> array
              <|> primitive
              <|> range
              <|> capability
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
      primitive = do {reserved "int"; return intType} <|>
                  do {reserved "bool"; return boolType} <|>
                  do {reserved "string"; return stringType} <|>
                  do {reserved "real"; return realType} <|>
                  do {reserved "void"; return voidType}

typeVariable :: Parser Type
typeVariable = do
  notFollowedBy upper
  id <- identifier
  return $ typeVar id
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

function :: Parser Function
function = do
  funmeta <- meta <$> getPosition
  reserved "def"
  funname <- Name <$> identifier
  funparams <- parens (commaSep paramDecl)
  colon
  funtype <- typ
  funbody <- expression
  return Function{funmeta
                 ,funname
                 ,funparams
                 ,funtype
                 ,funbody
                 }

traitDecl :: Parser TraitDecl
traitDecl = do
  tmeta <- meta <$> getPosition
  reserved "trait"
  ident <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  (tfields, tmethods) <- maybeBraces traitBody
  return Trait{tmeta
              ,tname = traitTypeFromRefType $
                       refTypeWithParams ident params
              ,tfields
              ,tmethods
              }
  where
    traitBody = do
      fields <- many traitField
      methods <- many methodDecl
      return (fields, methods)

traitField :: Parser FieldDecl
traitField = do
  reserved "require"
  fieldDecl

capability :: Parser Type
capability = do
  tree <- type_tree
  return $ fromTypeTree tree
  where
    type_tree :: Parser TypeTree
    type_tree = do
      ts <- product_type `sepBy1` reservedOp "+"
      return $ RoseTree Addition ts

    product_type :: Parser TypeTree
    product_type = do
      ts <- term `sepBy1` reservedOp "*"
      return $ RoseTree Product ts

    term :: Parser TypeTree
    term = Leaf <$> refInfo
        <|> parens type_tree

    refInfo :: Parser RefInfo
    refInfo = do
      notFollowedBy lower
      refId <- identifier
      parameters <- option [] $ angles (commaSep1 typ)
      return $ RefInfo{refId, parameters}
      <?> "upper case ref type"

classDecl :: Parser ClassDecl
classDecl = do
  cmeta <- meta <$> getPosition
  refKind <- option activeClassTypeFromRefType
             (reserved "passive" >> return passiveClassTypeFromRefType)
  reserved "class"
  name <- identifier
  params <- option [] (angles $ commaSep1 typeVariable)
  ccapability <- option incapability (do{reservedOp ":"; capability})
  (cfields, cmethods) <- maybeBraces classBody
  return Class{cmeta
              ,cname = refKind (refTypeWithParams name params)
              ,ccapability
              ,cfields
              ,cmethods
              }
  where
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
methodDecl = do pos <- getPosition
                reserved "def"
                name <- identifier
                params <- parens (commaSep paramDecl)
                colon
                ty <- typ
                body <- expression
                return $ Method (meta pos) (Name name) ty params body
             <|>
             do pos <- getPosition
                reserved "stream"
                name <- identifier
                params <- parens (commaSep paramDecl)
                colon
                ty <- typ
                body <- expression
                return $ StreamMethod (meta pos) (Name name) ty params body

arguments :: Parser Arguments
arguments = expression `sepBy` comma

expression :: Parser Expr
expression = buildExpressionParser opTable highOrderExpr
    where
      opTable = [
                 [arrayAccess],
                 [textualPrefix "not" Identifiers.NOT],
                 [textualOperator "and" Identifiers.AND,
                  textualOperator "or" Identifiers.OR],
                 [op "*" TIMES, op "/" DIV, op "%" MOD],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" Identifiers.LT, op ">" Identifiers.GT,
                  op "<=" Identifiers.LTE, op ">=" Identifiers.GTE,
                  op "==" Identifiers.EQ, op "!=" NEQ],
                 [messageSend],
                 [typedExpression],
                 [chain],
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
     <|> closure
     <|> match
     <|> task
     <|> finishTask
     <|> for
     <|> foreach
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
                root <- parens expression <|> try functionCall <|> varAccess
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
                    seq <- braces (expression `sepEndBy1` semi)
                    return $ Seq (meta pos) seq
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
      match = do pos <- getPosition
                 reserved "match"
                 argDecl <- expression
                 reserved "with"
                 body <- maybeBraces $ many matchingExpr
                 return $ MatchDecl (meta pos) argDecl body
             where
               matchingExpr = do
                 patternMatching <- expression
                 reservedOp "=>"
                 body <- expression
                 return (patternMatching, body)
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
                 val <- expression
                 return $ Print (meta pos) "{}\n" [val]
      stringLit = do pos <- getPosition
                     string <- stringLiteral
                     return $ StringLiteral (meta pos) string
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
