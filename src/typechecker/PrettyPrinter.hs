module PrettyPrinter (ppExpr,ppProgram) where
import AST
import Text.PrettyPrint

ppClass = text "class"
ppSkip = text "skip"
ppLet = text "let"
ppIn = text "in"
ppIf = text "if"
ppThen = text "then"
ppElse = text "else"
ppWhile = text "while"
ppGet = text "get"
ppNull = text "null"
ppTrue = text "true"
ppFalse = text "false"
ppNew = text "new"
ppPrint = text "print"
ppDot = text "."
ppColon = text ":"
ppComma = text ","
ppSemicolon = text ";"
ppEquals = text "="
ppSpace = text " "

ppName :: Name -> Doc
ppName (Name x) = text x

ppType :: Type -> Doc
ppType (Type t) = text t

ppProgram :: Program -> Doc
ppProgram (Program classDecls) = vcat (map ppClassDecl classDecls)

ppClassDecl :: ClassDecl -> Doc
ppClassDecl (Class name fields methods) = 
    ppClass <+> ppType name $+$
             (nest 2 $
                   vcat (map ppFieldDecl fields) $$
                   vcat (map ppMethodDecl methods))

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl (Field f t) = ppName f <+> ppColon <+> ppType t

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param (x, t)) =  ppName x <+> text ":" <+> ppType t

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl (Method mn rt params body) = 
    text "def" <+>
    ppName mn <> 
    parens (cat (punctuate (ppComma <> ppSpace) (map ppParamDecl params))) <+>
    text ":" <+> ppType rt $+$
    (nest 2 (ppExpr body))

isSimple :: Expr -> Bool
isSimple (VarAccess _) = True
isSimple (FieldAccess e _) = isSimple e
isSimple (Call e _ _) = isSimple e
isSimple _ = False

maybeParens :: Expr -> Doc
maybeParens e 
    | isSimple e = ppExpr e
    | otherwise  = parens $ ppExpr e

ppExpr :: Expr -> Doc
ppExpr Skip = ppSkip
ppExpr (Call e m args) = 
    maybeParens e <> ppDot <> ppName m <> 
      parens (cat (punctuate (ppComma <> ppSpace) (map ppExpr args)))
ppExpr (Let (Name x) (Type ty) e1 e2) = 
    ppLet <+> text x <+> ppColon <+> text ty <+> equals <+> ppExpr e1 <+> ppIn $+$ 
      nest 2 (ppExpr e2)
ppExpr (Seq es) = braces $ vcat $ punctuate ppSemicolon (map ppExpr es)
ppExpr (IfThenElse cond thn els) = 
    ppIf <+> ppExpr cond <+> ppThen $+$
         nest 2 (ppExpr thn) $+$
    ppElse $+$
         nest 2 (ppExpr els)
ppExpr (While cond expr) = 
    ppWhile <+> ppExpr cond $+$
         nest 2 (ppExpr expr)
ppExpr (Get e) = ppGet <+> ppExpr e
ppExpr (FieldAccess e f) = maybeParens e <> ppDot <> ppName f
ppExpr (VarAccess x) = ppName x
ppExpr (Assign lval e) = ppLVal lval <+> ppEquals <+> ppExpr e
ppExpr (Null) = ppNull
ppExpr (BTrue) = ppTrue
ppExpr (BFalse) = ppFalse
ppExpr (New ty) = ppNew <+> ppType ty
ppExpr (Print ty e) = ppPrint <+> ppType ty <+> ppExpr e
ppExpr (StringLiteral s) = doubleQuotes (text s)
ppExpr (IntLiteral n) = int n
ppExpr (Binop op e1 e2) = ppExpr e1 <+> ppBinop op <+> ppExpr e2

ppBinop :: Op -> Doc
ppBinop AST.LT  = text "<"
ppBinop AST.GT  = text ">"
ppBinop AST.EQ  = text "=="
ppBinop AST.NEQ = text "!="
ppBinop AST.PLUS  = text "+"
ppBinop AST.MINUS = text "-"
ppBinop AST.TIMES  = text "*"
ppBinop AST.DIV = text "/"

ppLVal :: LVal -> Doc
ppLVal (LVal (Name x))  = text x
ppLVal (LField e (Name f)) = maybeParens e <> ppDot <> text f
ppLVal (LThisField (Name f)) = text f
