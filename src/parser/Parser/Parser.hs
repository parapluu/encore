{-| 

Produces an "AST.AST" (or an error) of a @Program@ built from the
following grammar:

@
    Program ::= {EmbedTL}? ClassDecl Program | eps
    EmbedTL ::= embed .*end
  ClassDecl ::= {passive}? class Name { FieldDecls MethodDecls }
 FieldDecls ::= Name : Type FieldDecl | eps
 ParamDecls ::= Name : Type , ParamDecl | eps
MethodDecls ::= def Name ( ParamDecls ) : Type Expr
   Sequence ::= Expr Seq | eps
        Seq ::= ; Expr Seq | eps
  Arguments ::= Expr Args | eps
       Args ::= , Expr Args | eps
       Path ::= Name FieldAccess | eps
FieldAccess ::= . Name FieldAccess | eps
       Expr ::= ()
              | Path = Expr
              | Path . Name ( Arguments )
              | let Name = Expr in Expr
              | { Sequence }
              | if Expr then Expr else Expr
              | while Expr Expr
              | get Expr
              | Path
              | null
              | true
              | false
              | new Type
              | print Expr
              | \" String \"
              | Int
              | Expr Op Expr
              | embed Type .* end
              | (Expr)
              | \\ ( ParamDecls ) -> Expr
        Op ::= \< | \> | == | != | + | - | * | /
      Name ::= [a-zA-Z][a-zA-Z0-9_]*
       Int ::= [0-9]+
    String ::= ([^\"]|\\\")*
      Type ::= Arrow | NonArrow
     Arrow ::= (Types) -> NonArrow | NonArrow -> NonArrow
  NonArrow ::= string | int | bool | void | RefType
             | Fut Type | Par Type | (Type)
     Types ::= Type Tys | eps
       Tys ::= , Type Tys | eps
   RefType ::= [A-Z][a-zA-Z0-9_]*
@

Keywords: @ class def embed end Fut let in passive if then else while get null new Par print @

-}

module Parser.Parser(parseEncoreProgram
                    ,identifier_parser
                    ) where

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
import AST.Meta

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
               P.reservedNames = ["passive", "class", "def", "let", "in", "if", "then", "else", "while", "get", "null", "true", "false", "new", "print", "embed", "end", "Fut", "Par"],
               P.reservedOpNames = [":", "=", "==", "!=", "<", ">", "+", "-", "*", "/", "%", "->", "\\", "()"]
             }

-- | These parsers use the lexer above and are the smallest
-- building blocks of the whole parser.
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
operator   = P.operator lexer
dot        = P.dot lexer
commaSep   = P.commaSep lexer
colon      = P.colon lexer
semi       = P.semi lexer
semiSep    = P.semiSep lexer
comma      = P.comma lexer
parens     = P.parens lexer
braces     = P.braces lexer
stringLiteral = P.stringLiteral lexer
natural = P.natural lexer
float = P.float lexer
whiteSpace = P.whiteSpace lexer

typ :: Parser Type
typ  =  try arrow
    <|> parens typ
    <|> nonArrow
    <?> "type"
    where
      nonArrow =  fut
              <|> par
              <|> singleType
              <|> parens nonArrow
      arrow = do {lhs <- parens (commaSep typ) <|> do {ty <- nonArrow ; return [ty]} ;
                  reservedOp "->" ;
                  rhs <- nonArrow ;
                  return $ arrowType lhs rhs}
      fut = do {reserved "Fut" ; 
                ty <- typ ;
                return $ futureType ty}
      par = do {reserved "Par" ; 
                ty <- typ ;
                return $ parType ty}
      singleType = do {ty <- identifier ;
                       return $ case ty of
                                  "void" -> voidType
                                  "string" -> stringType
                                  "int" -> intType
                                  "real" -> realType
                                  "bool" -> boolType
                                  id -> if isUpper . head $ id then refType id else typeVar id}

program :: Parser Program
program = do {popHashbang ;
              whiteSpace ;
              embedtl <- embedTL ;
              whiteSpace ;
              classes <- many classDecl ;
              eof ;
              return $ Program embedtl classes}

popHashbang :: Parser ()
popHashbang = do
  try (do
    string "#!"
    many (noneOf "\n\r")
    whiteSpace
    return ()) <|> return ()

embedTL :: Parser EmbedTL
embedTL = do
  pos <- getPosition
  try (do
        reserved "embed"
        code <- manyTill anyChar $ (try $ reserved "end")
        return $ EmbedTL (meta pos) code) <|>
       (return $ EmbedTL (meta pos) "")

classDecl :: Parser ClassDecl
classDecl = do {pos <- getPosition ;
                activity <- do {reserved "passive" ; return Passive} <|> do {return Active} ;
                reserved "class" ;
                cname <- identifier ;
                (fields, methods) <- braces classBody <|> classBody ;
                let {ctype = case activity of Passive -> passiveRefType cname ; Active -> activeRefType cname} ;
                return $ Class (meta pos) activity ctype fields methods}
            where
              classBody = do {fields <- many fieldDecl ;
                              methods <- many methodDecl ;
                              return (fields, methods)}
                                     

fieldDecl :: Parser FieldDecl
fieldDecl = do {pos <- getPosition ;
                f <- identifier ;
                colon ;
                ty <- typ ;
                return $ Field (meta pos) (Name f) ty}

paramDecl :: Parser ParamDecl
paramDecl = do {pos <- getPosition ;
                x <- identifier ; 
                colon ; 
                ty <- typ ; 
                return $ Param (meta pos) (Name x) ty}

methodDecl :: Parser MethodDecl
methodDecl = do {pos <- getPosition ;
                 reserved "def" ; 
                 name <- identifier ;
                 params <- parens (commaSep paramDecl) ;
                 colon ;
                 ty <- typ ;
                 body <- expression ; 
                 return $ Method (meta pos) (Name name) ty params body}

lval :: Parser LVal
lval  =  try (do {pos <- getPosition ;
                  x <- identifier ;
                  dot ;
                  path <- identifier `sepBy` dot ;
                  return $ fieldAccessLVal path (VarAccess (meta pos) (Name x))})
     <|> do {pos <- getPosition ;
             x <- identifier ; return $ LVal (meta pos) (Name x)}
         where
           fieldAccessLVal :: [String] -> Expr -> LVal
           fieldAccessLVal [f] acc = LField (emeta acc) acc (Name f)
           fieldAccessLVal (f:path) acc = fieldAccessLVal path (FieldAccess (emeta acc) acc (Name f))

methodPath :: Parser (Expr, Name)
methodPath = do {pos <- getPosition ;
                 root <- identifier ;
                 dot ;
                 path <- identifier `sepBy` (skipMany1 dot) ;
                 return (pathToExpr (init path) (VarAccess (meta pos) (Name root)), Name $ last path )}

pathToExpr :: [String] -> Expr -> Expr
pathToExpr [] acc = acc
pathToExpr (f:path) acc = pathToExpr path (FieldAccess (emeta acc) acc (Name f))

arguments :: Parser Arguments
arguments = expression `sepBy` comma

expression :: Parser Expr
expression = buildExpressionParser opTable expr
    where
      opTable = [[op "*" TIMES, op "/" DIV, op "%" MOD],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" Identifiers.LT, op ">" Identifiers.GT, op "==" Identifiers.EQ, op "!=" NEQ],
                 [typedExpression]]
      op s binop = Infix (do{pos <- getPosition ; 
                             reservedOp s ; 
                             return (\e1 e2 -> Binop (meta pos) binop e1 e2)}) AssocLeft
      typedExpression = Postfix (do{pos <- getPosition ; 
                                    reservedOp ":" ; 
                                    t <- typ ; 
                                    return (\e -> TypedExpr (meta pos) e t)})

expr :: Parser Expr
expr  =  unit
     <|> try embed
     <|> try assignment
     <|> try methodCall
     <|> try fieldAccess
     <|> try functionCall
     <|> closure
     <|> parens expression
     <|> varAccess
     <|> letExpression
     <|> ifThenElse
     <|> while
     <|> get
     <|> new
     <|> null
     <|> true
     <|> false
     <|> sequence
     <|> print
     <|> string
     <|> try real
     <|> int
     <?> "expression"
    where
      embed = do {pos <- getPosition ;
                  reserved "embed" ; 
                  ty <- typ ;
                  code <- manyTill anyChar $ try $ reserved "end" ;
                  return $ Embed (meta pos) ty code}
      unit = do {pos <- getPosition ; reservedOp "()" ; return $ Skip (meta pos) }
      assignment = do {pos <- getPosition; 
                       lhs <- lval ; reservedOp "=" ; 
                       expr <- expression ; 
                       return $ Assign (meta pos) lhs expr}
      methodCall = do {pos <- getPosition ;
                       (target, tmname) <- methodPath ; 
                       args <- parens arguments ; 
                       return $ MethodCall (meta pos) target tmname args}
      letExpression = do {pos <- getPosition ;
                          reserved "let" ;
                          x <- identifier ;
                          reservedOp "=" ;
                          val <- expression ;
                          reserved "in" ;
                          expr <- expression ;
                          return $ Let (meta pos) (Name x) val expr}
      sequence = do {pos <- getPosition ;
                     seq <- braces (do {seq <- expression `sepEndBy` semi; return seq}) ;
                     return $ Seq (meta pos) seq}
      ifThenElse = do {pos <- getPosition ;
                       reserved "if" ; 
                       cond <- expression ;
                       reserved "then" ;
                       thn <- expression ;
                       reserved "else" ;
                       els <- expression ;
                       return $ IfThenElse (meta pos) cond thn els}
      while = do {pos <- getPosition ;
                  reserved "while" ; 
                  cond <- expression ;
                  expr <- expression ;
                  return $ While (meta pos) cond expr}
      get = do {pos <- getPosition ;
                reserved "get" ; 
                expr <- expression ; 
                return $ Get (meta pos) expr }
      fieldAccess = do {pos <- getPosition ;
                        root <- identifier ;
                        dot ;
                        path <- identifier `sepBy1` (skipMany1 dot) ;
                        return $ pathToExpr path (VarAccess (meta pos) (Name root)) }
      functionCall = do {pos <- getPosition ;
                         fun <- identifier ;
                         args <- parens arguments ;
                         return $ FunctionCall (meta pos) (Name fun) args}
      closure = do {pos <- getPosition ;
                    reservedOp "\\" ;
                    params <- parens (commaSep paramDecl) ;
                    reservedOp "->" ;
                    body <- expression ;
                    return $ Closure (meta pos) params body}
      varAccess = do {pos <- getPosition ;
                      id <- identifier ; 
                      return $ VarAccess (meta pos) $ Name id }
      null = do {pos <- getPosition ;
                 reserved "null" ; 
                 return $ Null (meta pos)}
      true = do {pos <- getPosition ;
                 reserved "true" ; 
                 return $ BTrue (meta pos)}
      false = do {pos <- getPosition ;
                  reserved "false" ; 
                  return $ BFalse (meta pos)}
      new = do {pos <- getPosition ;
                reserved "new" ;
                ty <- typ ;
                return $ New (meta pos) ty}
      print = do {pos <- getPosition ;
                  reserved "print" ;
                  expr <- expression ;
                  return $ Print (meta pos) expr}
      string = do {pos <- getPosition ;
                   string <- stringLiteral ; 
                   return $ StringLiteral (meta pos) string}
      int = do {pos <- getPosition ;
                n <- natural ; 
                return $ IntLiteral (meta pos) (fromInteger n)}
      real = do {pos <- getPosition ;
                 r <- float ; 
                 return $ RealLiteral (meta pos) r}
