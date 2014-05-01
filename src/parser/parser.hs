-- Program ::= ClassDecl Program | eps
-- ClassDecl ::= class Name { FieldDecls MethodDecls }
-- FieldDecls ::= Name : Name ; FieldDecl | eps
-- ParamDecls ::= Name : Name , ParamDecl | eps
-- MethodDecls = def Name ( ParamDecls ) : Name { Expr }
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
-- Op     ::= < | > | == | != | + | -
-- Name   ::= [a-zA-Z][a-zA-Z0-9]*
-- Int    ::= [0-9]+
-- String ::= ([^"]|\")*

-- Keywords: class def let in if then else get null new print

module Main where
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

import AST

lexer = 
    P.makeTokenParser $ 
    emptyDef { P.commentStart = "/*",
               P.commentEnd = "*/",
               P.commentLine = "//",
               P.reservedNames = ["class", "def", "skip", "let", "call", "in", "if", "then", "else", "lookup", "get", "null", "new", "print"],
               P.reservedOpNames = ["::", "=", "==", "!=", "<", ">", "+", "-"]
             }

identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
operator   = P.operator lexer
dot        = P.dot lexer
semi       = P.semi lexer
semiSep    = P.semiSep lexer
comma      = P.comma lexer
parens     = P.parens lexer
braces     = P.braces lexer
stringLiteral = P.stringLiteral lexer
natural = P.natural lexer

lval :: Parser LVal
lval  =  try (do {x <- identifier ;
                  dot ;
                  path <- sepBy1 identifier (skipMany1 dot) ;
                  return $ fieldAccessLVal path (VarAccess (Name x))})
     <|> do {x <- identifier ; return $ LVal (Name x)}
         where
           fieldAccessLVal :: [String] -> Expr -> LVal
           fieldAccessLVal [f] acc = LField acc (Name f)
           fieldAccessLVal (f:path) acc = fieldAccessLVal path (FieldAccess acc (Name f))

methodPath :: Parser (Expr, Name)
methodPath = do {root <- identifier ;
                 dot ;
                 path <- sepBy1 identifier (skipMany1 dot) ;
                 return (pathToExpr (init path) (VarAccess (Name root)), Name $ last path )}

pathToExpr :: [String] -> Expr -> Expr
pathToExpr [] acc = acc
pathToExpr (f:path) acc = pathToExpr path (FieldAccess acc (Name f))

arguments :: Parser Arguments
arguments = sepBy1 expression (skipMany1 comma)

-- TODO: Fix this so there is no left-recursion (and no parens)
binExpr :: Parser Expr
binExpr = buildExpressionParser binTable expression

binTable = [[op "+" PLUS, op "-" MINUS],
            [op "<" AST.LT, op ">" AST.GT, op "==" AST.EQ, op "!=" AST.NEQ]]
    where
      op s binop = Infix (do{reservedOp s ; return (\e1 e2 -> Binop binop e1 e2)}) AssocLeft

expression :: Parser Expr
expression  =  try (do {reserved "skip" ; return Skip })
           <|> try (do {lhs <- lval ; reservedOp "=" ; 
                        expr <- expression ; 
                        return $ Assign lhs expr})
           <|> try (do {(target, tmname) <- methodPath ; 
                        args <- parens arguments ; 
                        return $ Call target tmname args})
           <|> do {reserved "let" ;
                   x <- identifier ;
                   reservedOp "::" ;
                   ty <- identifier ;
                   reservedOp "=" ;
                   val <- expression ;
                   reserved "in" ;
                   expr <- expression ;
                   return $ Let (Name x) (Type ty) val expr}
           <|> do { seq <- braces (semiSep expression) ;
                    return $ Seq seq}
           <|> do {reserved "if" ; 
                   cond <- expression ;
                   reserved "then" ;
                   thn <- expression ;
                   reserved "else" ;
                   els <- expression ;
                   return $ IfThenElse cond thn els}
           <|> do {reserved "get" ; 
                   expr <- expression ; 
                   return $ Get expr }
           <|> try (do {root <- identifier ;
                        dot ;
                        path <- sepBy1 identifier (skipMany1 dot) ;
                        return $ pathToExpr path (VarAccess (Name root))
                       })
           <|> try (parens binExpr) -- FIXME: No parens
           <|> try (do {id <- identifier ; 
                   return $ VarAccess $ Name id })
           <|> do {reserved "null" ; 
                   return Null}
           <|> do {reserved "new" ;
                   ty <- identifier ;
                   return $ New (Type ty)}
           <|> do {reserved "print" ;
                   ty <- identifier ;
                   expr <- expression ;
                   return $ Print (Type ty) expr}
           <|> do {string <- stringLiteral ; 
                   return $ StringLiteral string}
           <|> do {n <- natural ; 
                   return $ IntLiteral (fromInteger n)}
           <?> "expression"

main = do putStrLn ""

-- Usage (for now): parse expression "" "x.f = skip"