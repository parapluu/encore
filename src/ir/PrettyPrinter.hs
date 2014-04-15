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
ppGet = text "get"
ppNull = text "null"
ppNew = text "new"
ppPrint = text "print"
ppDot = text "."
ppColon = text ":"
ppComma = text ","
ppSemicolon = text ";"
ppEquals = text "="
ppSpace = text " "

ppName :: Name -> Doc
ppName x = text x

ppType :: Type -> Doc
ppType t = text t

ppProgram :: Program -> Doc
ppProgram (Program classDecls) = vcat (map ppClassDecl classDecls)

ppClassDecl :: ClassDecl -> Doc
ppClassDecl (Class name fields methods) = 
    ppClass <+> ppName name <+> lbrace $+$
             (nest 2 $
                   vcat (map ppFieldDecl fields) $$
                   vcat (map ppMethodDecl methods))
             $+$ rbrace

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl (Field f t) = ppName f <+> ppColon <+> ppType t

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param (t, x)) = ppType t <+> ppName x

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl (Method mn rt params body) = 
    ppType rt <+>
    ppName mn <> 
    parens (cat (punctuate (ppComma <> ppSpace) (map ppParamDecl params))) $+$
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
ppExpr (Let x e1 e2) = 
    ppLet <+> ppName x <+> equals <+> ppExpr e1 <+> ppIn $+$ 
      nest 2 (ppExpr e2)
ppExpr (Seq es) = braces $ vcat $ punctuate ppSemicolon (map ppExpr es)
ppExpr (IfThenElse cond thn els) = 
    ppIf <+> ppExpr cond <+> ppThen $+$
         nest 2 (ppExpr thn) $+$
    ppElse $+$
         nest 2 (ppExpr els)
ppExpr (Get e) = ppGet <+> ppExpr e
ppExpr (FieldAccess e f) = maybeParens e <> ppDot <> ppName f
ppExpr (VarAccess x) = ppName x
ppExpr (Assign lvar e) = ppLvar lvar <+> ppEquals <+> ppExpr e
ppExpr (Null) = ppNull
ppExpr (New c) = ppNew <+> ppName c
ppExpr (Print e) = ppPrint <+> ppExpr e
ppExpr (StringLiteral s) = doubleQuotes (ppName s)
ppExpr (IntLiteral n) = int n
ppExpr (Binop op e1 e2) = ppExpr e1 <+> ppBinop op <+> ppExpr e2

ppBinop :: Op -> Doc
ppBinop AST.LT  = text "<"
ppBinop AST.GT  = text ">"
ppBinop AST.EQ  = text "=="
ppBinop AST.NEQ = text "!="
ppBinop AST.PLUS  = text "+"
ppBinop AST.MINUS = text "-"

ppLvar :: Lvar -> Doc
ppLvar (LVar x)  = text x
ppLvar (LField e f) = maybeParens e <> ppDot <> text f
ppLvar (LThisField f) = text f
