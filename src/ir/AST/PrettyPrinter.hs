{-# LANGUAGE NamedFieldPuns #-}

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
ppClassDecl Class {cname, fields, methods} = 
    ppClass <+> ppType cname $+$
             (indent $
                   vcat (map ppFieldDecl fields) $$
                   vcat (map ppMethodDecl methods))

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl Field {fname, ftype} = ppName fname <+> ppColon <+> ppType ftype

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param {pname, ptype}) =  ppName pname <+> text ":" <+> ppType ptype

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl Method {mname, mtype, mparams, mbody} = 
    text "def" <+>
    ppName mname <> 
    parens (cat (punctuate (ppComma <> ppSpace) (map ppParamDecl mparams))) <+>
    text ":" <+> ppType mtype $+$
    (indent (ppExpr mbody))

isSimple :: Expr -> Bool
isSimple VarAccess {} = True
isSimple FieldAccess {target} = isSimple target
isSimple Call {target} = isSimple target
isSimple _ = False

maybeParens :: Expr -> Doc
maybeParens e 
    | isSimple e = ppExpr e
    | otherwise  = parens $ ppExpr e

ppExpr :: Expr -> Doc
ppExpr Skip {} = ppSkip
ppExpr Call {target, name, args} = 
    maybeParens target <> ppDot <> ppName name <> 
      parens (cat (punctuate (ppComma <> ppSpace) (map ppExpr args)))
ppExpr Let {name = Name x, ty = Type ty, val, body} = 
    ppLet <+> text x <+> ppColon <+> text ty <+> equals <+> ppExpr val <+> ppIn $+$ 
      indent (ppExpr body)
ppExpr Seq {eseq} = braces $ vcat $ punctuate ppSemicolon (map ppExpr eseq)
ppExpr IfThenElse {cond, thn, els} = 
    ppIf <+> ppExpr cond <+> ppThen $+$
         indent (ppExpr thn) $+$
    ppElse $+$
         indent (ppExpr els)
ppExpr While {cond, body} = 
    ppWhile <+> ppExpr cond $+$
         indent (ppExpr body)
ppExpr Get {val} = ppGet <+> ppExpr val
ppExpr FieldAccess {target, name} = maybeParens target <> ppDot <> ppName name
ppExpr VarAccess {name} = ppName name
ppExpr Assign {lhs, rhs} = ppLVal lhs <+> ppEquals <+> ppExpr rhs
ppExpr Null {} = ppNull
ppExpr BTrue {} = ppTrue
ppExpr BFalse {} = ppFalse
ppExpr New {ty} = ppNew <+> ppType ty
ppExpr Print {ty, val} = ppPrint <+> ppType ty <+> ppExpr val
ppExpr StringLiteral {stringLit} = doubleQuotes (text stringLit)
ppExpr IntLiteral {intLit} = int intLit
ppExpr Binop {op, loper, roper} = ppExpr loper <+> ppBinop op <+> ppExpr roper

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
ppLVal LVal {lname = (Name x)}  = text x
ppLVal LField {ltarget, lname = Name f} = maybeParens ltarget <> ppDot <> text f
