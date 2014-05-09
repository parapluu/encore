module Parser(parseEncoreProgram) where
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

import AST

-- Program ::= ClassDecl Program | eps
-- ClassDecl ::= class Name { FieldDecls MethodDecls }
-- FieldDecls ::= Name : Name FieldDecl | eps
-- ParamDecls ::= Name : Name , ParamDecl | eps
-- MethodDecls = def Name ( ParamDecls ) : Name Expr
-- Sequence ::= Expr Seq | eps
-- Seq ::= ; Expr Seq | eps
-- Arguments ::= Expr Args | eps
-- Args ::= , Expr Args | eps
-- Path ::= Name FieldAccess | eps
-- FieldAccess ::= . Name FieldAccess | eps
-- Expr ::= skip
--        | Path = Expr
--        | Path . Name ( Arguments )
--        | let Name :: Name = Expr in Expr
--        | { Sequence }
--        | if Expr then Expr else Expr
--        | get Expr
--        | Path
--        | null
--        | new Name
--        | print Name Name Expr
--        | " String "
--        | Int
--        | ( Expr Op Expr )
-- Op     ::= < | > | == | != | + | - | * | /
-- Name   ::= [a-zA-Z][a-zA-Z0-9_]*
-- Int    ::= [0-9]+
-- String ::= ([^"]|\")*

-- Keywords: class def let in if then else get null new print

lexer = 
    P.makeTokenParser $ 
    emptyDef { P.commentStart = "{-",
               P.commentEnd = "-}",
               P.commentLine = "--",
               P.identStart = letter,
               P.reservedNames = ["class", "def", "skip", "let", "in", "if", "then", "else", "while", "get", "null", "new", "print"],
               P.reservedOpNames = [":", "=", "==", "!=", "<", ">", "+", "-", "*", "/"]
             }

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

program :: Parser Program
program = do {classes <- many classDecl ;
              eof ;
              return $ Program classes}

classDecl :: Parser ClassDecl
classDecl = do {reserved "class" ;
                cname <- identifier ;
                fields <- many fieldDecl ;
                methods <- many methodDecl ;
                return $ Class (Type cname) fields methods}

fieldDecl :: Parser FieldDecl
fieldDecl = do {f <- identifier ;
                colon ;
                ty <- identifier ;
                return $ Field (Name f) (Type ty)}

paramDecl :: Parser ParamDecl
paramDecl = do {x <- identifier ; 
                colon ; 
                ty <- identifier ; 
                return $ Param (Name x, Type ty)}

methodDecl :: Parser MethodDecl
methodDecl = do {reserved "def" ; 
                 name <- identifier ;
                 params <- parens (commaSep paramDecl) ;
                 colon ;
                 ty <- identifier ;
                 body <- expression ; 
                 return $ Method (Name name) (Type ty) params body}

lval :: Parser LVal
lval  =  try (do {x <- identifier ;
                  dot ;
                  path <- identifier `sepBy` dot ;
                  return $ fieldAccessLVal path (VarAccess (Name x))})
     <|> do {x <- identifier ; return $ LVal (Name x)}
         where
           fieldAccessLVal :: [String] -> Expr -> LVal
           fieldAccessLVal [f] acc = LField acc (Name f)
           fieldAccessLVal (f:path) acc = fieldAccessLVal path (FieldAccess acc (Name f))

methodPath :: Parser (Expr, Name)
methodPath = do {root <- identifier ;
                 dot ;
                 path <- identifier `sepBy` (skipMany1 dot) ;
                 return (pathToExpr (init path) (VarAccess (Name root)), Name $ last path )}

pathToExpr :: [String] -> Expr -> Expr
pathToExpr [] acc = acc
pathToExpr (f:path) acc = pathToExpr path (FieldAccess acc (Name f))

arguments :: Parser Arguments
arguments = expression `sepBy` comma

expression :: Parser Expr
expression = buildExpressionParser opTable expr
    where
      opTable = [[op "*" TIMES, op "/" DIV],
                 [op "+" PLUS, op "-" MINUS],
                 [op "<" AST.LT, op ">" AST.GT, op "==" AST.EQ, op "!=" AST.NEQ]]
      op s binop = Infix (do{reservedOp s ; return (\e1 e2 -> Binop binop e1 e2)}) AssocLeft

expr :: Parser Expr
expr  =  skip
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
     <|> sequence
     <|> print
     <|> string
     <|> int
     <?> "expression"
    where
      skip = do {reserved "skip" ; return Skip }
      assignment = do {lhs <- lval ; reservedOp "=" ; 
                       expr <- expression ; 
                       return $ Assign lhs expr}
      methodCall = do {(target, tmname) <- methodPath ; 
                       args <- parens arguments ; 
                       return $ Call target tmname args}
      letExpression = do {reserved "let" ;
                          x <- identifier ;
                          reservedOp ":" ;
                          ty <- identifier ;
                          reservedOp "=" ;
                          val <- expression ;
                          reserved "in" ;
                          expr <- expression ;
                          return $ Let (Name x) (Type ty) val expr}
      sequence = do { seq <- braces (semiSep expression) ;
                      return $ Seq seq}
      ifThenElse = do {reserved "if" ; 
                       cond <- expression ;
                       reserved "then" ;
                       thn <- expression ;
                       reserved "else" ;
                       els <- expression ;
                       return $ IfThenElse cond thn els}
      while = do {reserved "while" ; 
                  cond <- expression ;
                  expr <- expression ;
                  return $ While cond expr}
      get = do {reserved "get" ; 
                expr <- expression ; 
                return $ Get expr }
      fieldAccess = do {root <- identifier ;
                        dot ;
                        path <- identifier `sepBy1` (skipMany1 dot) ;
                        return $ pathToExpr path (VarAccess (Name root)) }
      varAccess = do {id <- identifier ; 
                      return $ VarAccess $ Name id }
      null = do {reserved "null" ; 
                 return Null}
      new = do {reserved "new" ;
                ty <- identifier ;
                return $ New (Type ty)}
      print = do {reserved "print" ;
                  ty <- identifier ;
                  expr <- expression ;
                  return $ Print (Type ty) expr}
      string = do {string <- stringLiteral ; 
                   return $ StringLiteral string}
      int = do {n <- natural ; 
                return $ IntLiteral (fromInteger n)}

parseEncoreProgram :: FilePath -> String -> Either ParseError Program
parseEncoreProgram = parse program