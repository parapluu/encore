{-# LANGUAGE NamedFieldPuns #-}

{-| 

Produces an "AST.AST" (or an error) of a @Program@ built from the
following grammar:

@
    Program ::= {Imports}* {EmbedTL}? ClassDecl Program | eps
    Imports ::= import {qualified}? Name {(Name,...)}? {as Name}?
    EmbedTL ::= embed .* body .* end | embed .* end
  ClassDecl ::= {passive}? class Name { FieldDecls MethodDecls }
 FieldDecls ::= Name : Type FieldDecl | eps
 ParamDecls ::= Name : Type , ParamDecl | eps
MethodDecls ::= def Name ( ParamDecls ) : Type Expr
   Sequence ::= Expr Seq | eps
        Seq ::= ; Expr Seq | ; | eps
  Arguments ::= Expr Args | eps
       Args ::= , Expr Args | eps
   LetDecls ::= Name = Expr LetDecls | eps
       Expr ::= ()
              | embed Type .* end
              | Expr . Name
              | Expr . Name ( Arguments )
              | Expr ! Name ( Arguments )
              | print Expr
              | Name ( Arguments )
              | ( Expr )
              | Name
              | let LetDecls in Expr
              | Expr = Expr
              | { Sequence }
              | if Expr then Expr else Expr
              | if Expr then Expr
              | unless Expr then Expr
              | while Expr Expr
              | repeat Name <- Expr Expr
              | get Expr
              | await Expr
              | suspend
              | new Type ( Arguments )
              | new Type
              | null
              | true
              | false
              | \" String \"
              | Int
              | Real
              | Expr Op Expr
              | not Expr
              | \\ ( ParamDecls ) -> Expr
        Op ::= \< | \> | == | != | + | - | * | / | % | and | or
      Name ::= [a-zA-Z][a-zA-Z0-9_]*
       Int ::= [0-9]+
      Real ::= Int . Int
    String ::= ([^\"]|\\\")*
      Type ::= Arrow | NonArrow
     Arrow ::= (Types) -> NonArrow | NonArrow -> NonArrow
  NonArrow ::= string | int | bool | void | RefType
             | Fut Type | Par Type | (Type)
     Types ::= Type Tys | eps
       Tys ::= , Type Tys | eps
   RefType ::= [A-Z][a-zA-Z0-9_]*
@

Keywords: @ class def embed body end Fut let in passive if then else while await suspend get null new Par print @

-}

module Parser.Parser(parseEncoreProgram ,identifier_parser) where

-- Library dependencies
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.Char(isUpper)

-- Module dependencies
import Identifiers
import Types
import AST.AST
import AST.Meta hiding(Closure)

-- | 'parseEncoreProgram' @path@ @code@ assumes @path@ is the path
-- to the file being parsed and will produce an AST for @code@,
-- unless a parse error occurs.
parseEncoreProgram :: FilePath -> String -> Either ParseError Program
parseEncoreProgram = parse program

identifier_parser = identifier

-- | This creates a tokenizer that reads a language derived from
-- the empty language definition 'emptyDef' extended as shown.
lexer = 
    P.makeTokenParser $ 
    emptyDef { P.commentStart = "{-",
               P.commentEnd = "-}",
               P.commentLine = "--",
               P.identStart = letter,
               P.reservedNames = ["passive", "class", "def", "stream", 
                                  "let", "in", "if", "unless", "then", "else", "repeat", "while", 
                                  "get", "yield", "eos", "getNext", "new", "this", "await", "suspend",
				  "and", "or", "not", "true", "false", "null", "embed", "body", "end", 
                                  "Fut", "Par", "Stream", "import", "qualified", "module"],
               P.reservedOpNames = [":", "=", "==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "->", "\\", "()", "~~>"]
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
commaSep   = P.commaSep lexer
colon      = P.colon lexer
semi       = P.semi lexer
semiSep    = P.semiSep lexer
comma      = P.comma lexer
parens     = P.parens lexer
angles     = P.angles lexer
lparen     = symbol "("
rparen     = symbol ")"
braces     = P.braces lexer
stringLiteral = P.stringLiteral lexer
natural = P.integer lexer
float = P.float lexer
whiteSpace = P.whiteSpace lexer

-- Note, when we have product types (i.e. tuples), we could make
-- an expressionParser for types (with arrow as the only infix
-- operator)
typ :: Parser Type
typ  =  try arrow
    <|> parens typ
    <|> nonArrow
    <?> "type"
    where
      nonArrow =  fut
              <|> par
              <|> try paramType
              <|> stream
              <|> singleType
              <|> parens nonArrow
      arrow = do lhs <- parens (commaSep typ) <|> do {ty <- nonArrow ; return [ty]}
                 reservedOp "->"
                 rhs <- nonArrow
                 return $ arrowType lhs rhs
      fut = do reserved "Fut" 
               ty <- typ 
               return $ futureType ty
      par = do reserved "Par" 
               ty <- typ 
               return $ parType ty
      paramType = do id <- identifier
                     if (isUpper . head $ id) 
                     then do params <- angles (commaSep typ)
                             return $ refTypeWithParams id params
                     else fail "Type with parameters must start with upper-case letter"
      stream = do reserved "Stream" 
                  ty <- typ 
                  return $ streamType ty
      singleType = do ty <- identifier
                      return $ read ty
          where read "void"   = voidType
                read "string" = stringType
                read "int"    = intType
                read "real"   = realType
                read "bool"   = boolType
                read id 
                    | isUpper . head $ id = refType id
                    | otherwise           = typeVar id

program :: Parser Program
program = do optional hashbang
             whiteSpace
             importdecls <- many importdecl
             embedtl <- embedTL
             functions <- many function
             classes <- many classDecl
             eof
             return $ Program embedtl importdecls functions classes
    where
      hashbang = do string "#!"
                    many (noneOf "\n\r")

importdecl :: Parser ImportDecl
importdecl = do 
  pos <- getPosition 
  reserved "import"
--  qualified <- option $ reserved "qualified"
  iname <- identifier
--  {(Name,...)}? 
--  stringliteral "as"; qname <- name
--  {as Name}
  return $ Import (meta pos) (Name iname)

embedTL :: Parser EmbedTL
embedTL = do pos <- getPosition
             (try (do string "embed"
                      header <- manyTill anyChar $ try $ do {space; string "body"}
                      code <- manyTill anyChar $ try $ do {space; reserved "end"}
                      return $ EmbedTL (meta pos) header code
                 )
              <|>
              try (do string "embed"
                      header <- manyTill anyChar $ try $ do {space; reserved "end"}
                      return $ EmbedTL (meta pos) header ""
                 ) 
              <|>
              (return $ EmbedTL (meta pos) "" ""))

function :: Parser Function
function = do pos <- getPosition
              reserved "def"
              name <- identifier
              params <- parens (commaSep paramDecl)
              colon
              ty <- typ
              body <- expression
              return $ Function (meta pos) (Name name) ty params body

classDecl :: Parser ClassDecl
classDecl = do pos <- getPosition
               refKind <- option activeRefTypeWithParams 
                          (do {reserved "passive" ; return passiveRefTypeWithParams})
               reserved "class"
               cname <- identifier
               params <- option [] 
                         (do ids <- angles $ commaSep identifier
                             return $ map typeVar ids)
               (fields, methods) <- braces classBody <|> classBody
               return $ Class (meta pos) (refKind cname params) fields methods
            where
              classBody = do fields <- many fieldDecl
                             methods <- many methodDecl
                             return (fields, methods)
                                     

fieldDecl :: Parser FieldDecl
fieldDecl = do pos <- getPosition
               f <- identifier
               colon
               ty <- typ
               return $ Field (meta pos) (Name f) ty

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
expression = buildExpressionParser opTable expr
    where
      opTable = [
                 [prefix "not" Identifiers.NOT],
                 [op "*" TIMES, op "/" DIV, op "%" MOD],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" Identifiers.LT, op ">" Identifiers.GT, op "<=" Identifiers.LTE, op ">=" Identifiers.GTE, op "==" Identifiers.EQ, op "!=" NEQ],
                 [op "and" Identifiers.AND, op "or" Identifiers.OR],
                 [messageSend],
                 [typedExpression],
                 [chain],
                 [assignment]
                ]
      prefix s operator = 
          Prefix (do pos <- getPosition
                     reservedOp s
                     return (\x -> Unary (meta pos) operator x))
      op s binop = 
          Infix (do pos <- getPosition
                    reservedOp s
                    return (\e1 e2 -> Binop (meta pos) binop e1 e2)) AssocLeft
      typedExpression = 
          Postfix (do pos <- getPosition
                      reservedOp ":"
                      t <- typ
                      return (\e -> TypedExpr (meta pos) e t))
      messageSend = 
          Postfix (do pos <- getPosition
                      bang
                      name <- identifier
                      args <- parens arguments
                      return (\target -> MessageSend (meta pos) target (Name name) args))
      chain = 
          Infix (do pos <- getPosition ;
                    reservedOp "~~>" ;
                    return $ (\lhs rhs -> FutureChain (meta pos) lhs rhs)) AssocLeft
      assignment = 
          Infix (do pos <- getPosition ;
                    reservedOp "=" ;
                    return (\lhs rhs -> Assign (meta pos) lhs rhs)) AssocRight



expr :: Parser Expr
expr  =  unit
     <|> try embed
     <|> try path
     <|> try functionCall
     <|> try print
     <|> closure
     <|> parens expression
     <|> varAccess
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
      path = do pos <- getPosition
                root <- parens expression <|> try functionCall <|> varAccess
                dot
                path <- (try functionCall <|> varAccess) `sepBy1` dot
                return $ foldl (buildPath pos) root path
             where
               buildPath pos target (VarAccess{name}) = FieldAccess (meta pos) target name
               buildPath pos target (FunctionCall{name, args}) = MethodCall (meta pos) target name args
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
                    seq <- braces (do {seq <- expression `sepEndBy1` semi; return seq})
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
      varAccess = do pos <- getPosition
                     id <- (do reserved "this"; return "this") <|> identifier
                     return $ VarAccess (meta pos) $ Name id 
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
      print = do pos <- getPosition
                 reserved "print"
                 val <- expression
                 return $ Print (meta pos) "{}\n" [val]
      stringLit = do pos <- getPosition
                     string <- stringLiteral
                     return $ StringLiteral (meta pos) string
      int = do pos <- getPosition
               n <- natural
               return $ IntLiteral (meta pos) (fromInteger n)
      real = do pos <- getPosition
                r <- float
                return $ RealLiteral (meta pos) r
