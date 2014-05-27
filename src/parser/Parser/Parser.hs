{-| 

Produces an "AST.AST" (or an error) of a @Program@ built from the
following grammar:

@
    Program ::= ClassDecl Program | eps
  ClassDecl ::= class Name { FieldDecls MethodDecls }
 FieldDecls ::= Name : Name FieldDecl | eps
 ParamDecls ::= Name : Name , ParamDecl | eps
MethodDecls ::= def Name ( ParamDecls ) : Name Expr
   Sequence ::= Expr Seq | eps
        Seq ::= ; Expr Seq | eps
  Arguments ::= Expr Args | eps
       Args ::= , Expr Args | eps
       Path ::= Name FieldAccess | eps
FieldAccess ::= . Name FieldAccess | eps
       Expr ::= skip
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
              | new Name
              | print Expr
              | \" String \"
              | Int
              | Expr Op Expr
              | embed Name \" String \"
              | (Expr)
        Op ::= \< | \> | == | != | + | - | * | /
      Name ::= [a-zA-Z][a-zA-Z0-9_]*
       Int ::= [0-9]+
    String ::= ([^\"]|\\\")*
      Type ::= string | int | bool | void | String
             | Fut Type | Par Type | (Types) -> Type
     Types ::= Type Tys | eps
       Tys ::= , Type Tys | eps
@

Keywords: @ class def embed let in if then else while get null new print @

-}

module Parser.Parser(parseEncoreProgram) where

-- Library dependencies
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

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

-- | This creates a tokenizer that reads a language derived from
-- the empty language definition 'emptyDef' extended as shown.
lexer = 
    P.makeTokenParser $ 
    emptyDef { P.commentStart = "{-",
               P.commentEnd = "-}",
               P.commentLine = "--",
               P.identStart = letter,
               P.reservedNames = ["class", "def", "skip", "let", "in", "if", "then", "else", "while", "get", "null", "true", "false", "new", "print", "embed", "Fut", "Par"],
               P.reservedOpNames = [":", "=", "==", "!=", "<", ">", "+", "-", "*", "/", "->"]
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
    <|> fut
    <|> par
    <|> do {ty <- identifier ;
            return $ case ty of
                       "void" -> voidType
                       "string" -> stringType
                       "int" -> intType
                       "bool" -> boolType
                       id -> refType id}
    <?> "type"
    where
      arrow = do {lhs <- parens (commaSep typ) ;
                  reservedOp "->" ;
                  rhs <- typ ;
                  return $ arrowType lhs rhs}
      fut = do {reserved "Fut" ; 
                ty <- typ ;
                return $ futureType ty}
      par = do {reserved "Par" ; 
                ty <- typ ;
                return $ parType ty}

program :: Parser Program
program = do {whiteSpace ;
              classes <- many classDecl ;
              eof ;
              return $ Program classes}

classDecl :: Parser ClassDecl
classDecl = do {pos <- getPosition ;
                reserved "class" ;
                cname <- identifier ;
                (fields, methods) <- do {braces classBody} <|> do {classBody} ;
                return $ Class (meta pos) (refType cname) fields methods}
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
      opTable = [[op "*" TIMES, op "/" DIV],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" Identifiers.LT, op ">" Identifiers.GT, op "==" Identifiers.EQ, op "!=" NEQ]]
      op s binop = Infix (do{pos <- getPosition ; 
                             reservedOp s ; 
                             return (\e1 e2 -> Binop (meta pos) binop e1 e2)}) AssocLeft

expr :: Parser Expr
expr  =  skip
     <|> try embed
     <|> try assignment
     <|> try methodCall
     <|> try fieldAccess
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
                  code <- stringLiteral ; 
                  return $ Embed (meta pos) ty code}
      skip = do {pos <- getPosition ; reserved "skip" ; return $ Skip (meta pos) }
      assignment = do {pos <- getPosition; 
                       lhs <- lval ; reservedOp "=" ; 
                       expr <- expression ; 
                       return $ Assign (meta pos) lhs expr}
      methodCall = do {pos <- getPosition ;
                       (target, tmname) <- methodPath ; 
                       args <- parens arguments ; 
                       return $ Call (meta pos) target tmname args}
      letExpression = do {pos <- getPosition ;
                          reserved "let" ;
                          x <- identifier ;
                          reservedOp "=" ;
                          val <- expression ;
                          reserved "in" ;
                          expr <- expression ;
                          return $ Let (meta pos) (Name x) val expr}
      sequence = do {pos <- getPosition ;
                     seq <- braces (semiSep expression) ;
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