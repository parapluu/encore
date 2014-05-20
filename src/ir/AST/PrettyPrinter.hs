{-|

Prints the source code that an "AST.AST" represents. Each node in
the abstract syntax tree has a corresponding pretty-print function
(although not all are exported)

-}

module AST.PrettyPrinter (ppExpr,ppProgram,ppParamDecl, ppFieldDecl, ppLVal, indent) where

-- Library dependencies
import Text.PrettyPrint

-- Module dependencies
import Identifiers
import AST.AST

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

indent = nest 2

ppName :: Name -> Doc
ppName (Name x) = text x

ppType :: Type -> Doc
ppType (Type t) = text t

ppProgram :: Program -> Doc
ppProgram (Program classDecls) = vcat (map ppClassDecl classDecls)

ppClassDecl :: ClassDecl -> Doc
ppClassDecl Class {cname = cname, fields = fields, methods = methods} = 
    ppClass <+> ppType cname $+$
             (indent $
                   vcat (map ppFieldDecl fields) $$
                   vcat (map ppMethodDecl methods))

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl Field {fname = f, ftype = t} = ppName f <+> ppColon <+> ppType t

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param (x, t)) =  ppName x <+> text ":" <+> ppType t

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl Method {mname = mn, rtype = rt, mparams = params, mbody = body} = 
    text "def" <+>
    ppName mn <> 
    parens (cat (punctuate (ppComma <> ppSpace) (map ppParamDecl params))) <+>
    text ":" <+> ppType rt $+$
    (indent (ppExpr body))

isSimple :: Expr -> Bool
isSimple VarAccess {} = True
isSimple FieldAccess {path = p} = isSimple p
isSimple Call {target = targ} = isSimple targ
isSimple _ = False

maybeParens :: Expr -> Doc
maybeParens e 
    | isSimple e = ppExpr e
    | otherwise  = parens $ ppExpr e

ppExpr :: Expr -> Doc
ppExpr Skip {} = ppSkip
ppExpr Call {target = e, tmname = m, args = args} = 
    maybeParens e <> ppDot <> ppName m <> 
      parens (cat (punctuate (ppComma <> ppSpace) (map ppExpr args)))
ppExpr Let {eid = Name x, ty = Type ty, val = val, body = body} = 
    ppLet <+> text x <+> ppColon <+> text ty <+> equals <+> ppExpr val <+> ppIn $+$ 
      indent (ppExpr body)
ppExpr Seq {eseq = es} = braces $ vcat $ punctuate ppSemicolon (map ppExpr es)
ppExpr IfThenElse {cond = cond, thn = thn, els = els} = 
    ppIf <+> ppExpr cond <+> ppThen $+$
         indent (ppExpr thn) $+$
    ppElse $+$
         indent (ppExpr els)
ppExpr While {cond = cond, body = expr} = 
    ppWhile <+> ppExpr cond $+$
         indent (ppExpr expr)
ppExpr Get {fut = e} = ppGet <+> ppExpr e
ppExpr FieldAccess {path = p, field = f} = maybeParens p <> ppDot <> ppName f
ppExpr VarAccess {eid = x} = ppName x
ppExpr Assign {lhs = lval, rhs = e} = ppLVal lval <+> ppEquals <+> ppExpr e
ppExpr Null {} = ppNull
ppExpr BTrue {} = ppTrue
ppExpr BFalse {} = ppFalse
ppExpr New {ty = ty} = ppNew <+> ppType ty
ppExpr Print {ty = ty, val = e} = ppPrint <+> ppType ty <+> ppExpr e
ppExpr StringLiteral {stringLit = s} = doubleQuotes (text s)
ppExpr IntLiteral {intLit = n} = int n
ppExpr Binop {op = op, loper = e1, roper = e2} = ppExpr e1 <+> ppBinop op <+> ppExpr e2

ppBinop :: Op -> Doc
ppBinop Identifiers.LT  = text "<"
ppBinop Identifiers.GT  = text ">"
ppBinop Identifiers.EQ  = text "=="
ppBinop Identifiers.NEQ = text "!="
ppBinop Identifiers.PLUS  = text "+"
ppBinop Identifiers.MINUS = text "-"
ppBinop Identifiers.TIMES  = text "*"
ppBinop Identifiers.DIV = text "/"

ppLVal :: LVal -> Doc
ppLVal LVal {lid = (Name x)}  = text x
ppLVal LField {lpath = p, lid = Name f} = maybeParens p <> ppDot <> text f
