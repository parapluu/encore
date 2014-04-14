module PrettyPrinter (prettyPrint) where
import AST
import Text.PrettyPrint

ppSkip = text "skip"
ppLet = text "let"
ppIf = text "if"
ppThen = text "then"
ppElse = text "else"
ppGet = text "get"
ppNull = text "null"
ppNew = text "new"
ppPrint = text "print"
ppDot = text "."
ppComma = text ","
ppEquals = text "="
ppSpace = text " "

isSimple :: Expr -> Bool
isSimple (VarAccess _) = True
isSimple (FieldAccess e f) = isSimple e
isSimple (Call e m f) = isSimple e
isSimple _ = False

maybeParens :: Expr -> Doc
maybeParens e 
    | isSimple e = prettyPrint e
    | otherwise  = parens $ prettyPrint e

prettyPrint :: Expr -> Doc
prettyPrint Skip = ppSkip
prettyPrint (Call e m args) = maybeParens e <> ppDot <> text m <> 
                              parens (cat (punctuate (ppComma <> ppSpace)
                                           (map prettyPrint args)))
prettyPrint (Let x e1 e2) = ppLet <+> text x <+> equals <+> prettyPrint e1
prettyPrint (IfThenElse cond thn els) = ppIf <+> prettyPrint cond <+> ppThen $+$
                                        nest 2 (prettyPrint thn) $+$
                                        ppElse $+$
                                        nest 2 (prettyPrint els)
prettyPrint (Get e) = ppGet <+> prettyPrint e
prettyPrint (FieldAccess e f) = maybeParens e <> ppDot <> text f
prettyPrint (FieldAssign e1 f e2) = maybeParens e1 <> ppDot <> text f <+> 
                                    ppEquals <+> prettyPrint e2
prettyPrint (VarAccess x) = text x
prettyPrint (VarAssign x e) = text x <+> ppEquals <+> prettyPrint e
prettyPrint (Null) = ppNull
prettyPrint (New c) = ppNew <+> text c
prettyPrint (Print e) = ppPrint <+> prettyPrint e
prettyPrint (StringLiteral s) = text s
prettyPrint (IntLiteral n) = int n
prettyPrint (Binop op e1 e2) = prettyPrint e1 <+> prettyPrintBinop op <+> prettyPrint e2

prettyPrintBinop :: Op -> Doc
prettyPrintBinop LT_  = text "<"
prettyPrintBinop GT_  = text ">"
prettyPrintBinop EQ_  = text "=="
prettyPrintBinop NEQ_ = text "!="