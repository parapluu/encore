{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|

Prints the source code that an "AST.AST" represents. Each node in
the abstract syntax tree has a corresponding pretty-print function
(although not all are exported)

-}

module AST.PrettyPrinter (ppExpr, ppProgram, ppParamDecl,
                          ppFieldDecl, indent, ppSugared) where

-- Library dependencies
import Text.PrettyPrint

-- Module dependencies
import Identifiers
import Types
import AST.AST

ppClass = text "class"
ppSkip = text "()"
ppBreathe = text "breathe"
ppLet = text "let"
ppIn = text "in"
ppIf = text "if"
ppThen = text "then"
ppElse = text "else"
ppUnless = text "unless"
ppWhile = text "while"
ppRepeat = text "repeat"
ppGet = text "get"
ppYield = text "yield"
ppEos = text "eos"
ppAwait = text "await"
ppSuspend = text "suspend"
ppNull = text "null"
ppTrue = text "true"
ppFalse = text "false"
ppNew = text "new"
ppPeer = text "peer"
ppPrint = text "print"
ppExit = text "exit"
ppEmbed = text "embed"
ppDot = text "."
ppBang = text "!"
ppColon = text ":"
ppComma = text ","
ppSemicolon = text ";"
ppEquals = text "="
ppSpace = text " "
ppLambda = text "\\"
ppArrow = text "->"
ppTask = text "async"
ppBar = text "|"

indent = nest 2

commaSep l = cat $ punctuate (ppComma <> ppSpace) l

ppName :: Name -> Doc
ppName (Name x) = text x

ppQName :: QName -> Doc
ppQName [a] = ppName a
ppQName (a:as) = ppName a <+> text "." <+> ppQName as

ppType :: Type -> Doc
ppType = text . show

ppProgram :: Program -> Doc
ppProgram (Program bundle (EmbedTL _ header code) importDecls _ functions classDecls) =
    ppBundleDecl bundle $+$
         text "embed" $+$ text header $+$ text "body" $+$ text code $+$ text "end" $+$
         vcat (map ppImportDecl importDecls) $+$
         vcat (map ppFunction functions) $+$
         vcat (map ppClassDecl classDecls)

ppBundleDecl :: BundleDecl -> Doc
ppBundleDecl NoBundle = empty
ppBundleDecl Bundle{bname} = text "bundle" <+> ppQName bname

ppImportDecl :: ImportDecl -> Doc
ppImportDecl Import {itarget} = text "import" <+> ppQName itarget

ppFunction :: Function -> Doc
ppFunction Function {funname, funtype, funparams, funbody} =
    text "def" <+>
    ppName funname <>
    parens (commaSep (map ppParamDecl funparams)) <+>
    text ":" <+> ppType funtype $+$
    (indent (ppExpr funbody))

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
    parens (commaSep (map ppParamDecl mparams)) <+>
    text ":" <+> ppType mtype $+$
    (indent (ppExpr mbody))
ppMethodDecl StreamMethod {mname, mtype, mparams, mbody} =
    text "stream" <+>
    ppName mname <>
    parens (commaSep (map ppParamDecl mparams)) <+>
    text ":" <+> ppType mtype $+$
    (indent (ppExpr mbody))

isSimple :: Expr -> Bool
isSimple VarAccess {} = True
isSimple FieldAccess {target} = isSimple target
isSimple MethodCall {target} = isSimple target
isSimple MessageSend {target} = isSimple target
isSimple FunctionCall {} = True
isSimple _ = False

maybeParens :: Expr -> Doc
maybeParens e
    | isSimple e = ppExpr e
    | otherwise  = parens $ ppExpr e

ppSugared :: Expr -> Doc
ppSugared e = case getSugared e of
                Just e' -> ppExpr e'
                Nothing -> ppExpr e

ppExpr :: Expr -> Doc
ppExpr Skip {} = ppSkip
ppExpr Breathe {} = ppBreathe
ppExpr MethodCall {target, name, args} =
    maybeParens target <> ppDot <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr MessageSend {target, name, args} =
    maybeParens target <> ppBang <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr FunctionCall {name, args} =
    ppName name <> parens (commaSep (map ppExpr args))
ppExpr Closure {eparams, body} =
    ppLambda <> parens (commaSep (map ppParamDecl eparams)) <+> ppArrow <+> ppExpr body
ppExpr Async {body} =
    ppTask <> parens (ppExpr body)
ppExpr Let {decls, body} =
    ppLet <+> vcat (map (\(Name x, e) -> text x <+> equals <+> ppExpr e) decls) $+$ ppIn $+$
      indent (ppExpr body)
ppExpr Seq {eseq} = braces $ vcat $ punctuate ppSemicolon (map ppExpr eseq)
ppExpr IfThenElse {cond, thn, els} =
    ppIf <+> ppExpr cond <+> ppThen $+$
         indent (ppExpr thn) $+$
    ppElse $+$
         indent (ppExpr els)
ppExpr IfThen {cond, thn} =
    ppIf <+> ppExpr cond <+> ppThen $+$
         indent (ppExpr thn)
ppExpr Unless {cond, thn} =
    ppUnless <+> ppExpr cond <+> ppThen $+$
         indent (ppExpr thn)
ppExpr While {cond, body} =
    ppWhile <+> ppExpr cond $+$
         indent (ppExpr body)
ppExpr Repeat {name, times, body} =
    ppRepeat <+> (ppName name) <+> (text "<-") <+> (ppExpr times) $+$
         indent (ppExpr body)
ppExpr FutureChain {future, chain} =
    ppExpr future <+> (text "~~>") <+> ppExpr chain
ppExpr Get {val} = ppGet <+> ppExpr val
ppExpr Yield {val} = ppYield <+> ppExpr val
ppExpr Eos {} = ppEos <> parens empty
ppExpr Await {val} = ppAwait <+> ppExpr val
ppExpr IsEos {target} = ppExpr target <> ppDot <> ppEos <> parens empty
ppExpr StreamNext {target} = ppExpr target <> ppDot <> text "next" <> parens empty
ppExpr Suspend {} = ppSuspend
ppExpr FieldAccess {target, name} = maybeParens target <> ppDot <> ppName name
ppExpr ArrayAccess {target, index} = ppExpr target <> (brackets $ ppExpr index)
ppExpr ArraySize {target} = ppBar <> ppExpr target <> ppBar
ppExpr ArrayNew {ty, size} = (brackets $ ppType ty) <> parens (ppExpr size)
ppExpr ArrayLiteral {args} = brackets $ commaSep (map ppExpr args)
ppExpr VarAccess {name} = ppName name
ppExpr Assign {lhs, rhs} = ppExpr lhs <+> ppEquals <+> ppExpr rhs
ppExpr Null {} = ppNull
ppExpr BTrue {} = ppTrue
ppExpr BFalse {} = ppFalse
ppExpr NewWithInit {ty, args} = ppNew <+> ppType ty <> parens (commaSep (map ppExpr args))
ppExpr New {ty} = ppNew <+> ppType ty
ppExpr Peer {ty} = ppPeer <+> ppType ty
ppExpr Print {stringLit, args} = ppPrint <> parens (doubleQuotes (text stringLit) <> comma <+> commaSep (map ppExpr args))
ppExpr Exit {args} = ppExit <> parens (commaSep (map ppExpr args))
ppExpr StringLiteral {stringLit} = doubleQuotes (text stringLit)
ppExpr IntLiteral {intLit} = int intLit
ppExpr RealLiteral {realLit} = double realLit
ppExpr Embed {ty, code} = ppEmbed <+> ppType ty <+> doubleQuotes (text code)
ppExpr Unary {op, operand} = ppUnary op <+> ppExpr operand
ppExpr Binop {op, loper, roper} = ppExpr loper <+> ppBinop op <+> ppExpr roper
ppExpr TypedExpr {body, ty} = ppExpr body <+> ppColon <+> ppType ty

ppUnary :: Op -> Doc
ppUnary Identifiers.NOT = text "not"

ppBinop :: Op -> Doc
ppBinop Identifiers.AND = text "and"
ppBinop Identifiers.OR = text "or"
ppBinop Identifiers.LT  = text "<"
ppBinop Identifiers.GT  = text ">"
ppBinop Identifiers.LTE  = text "<="
ppBinop Identifiers.GTE  = text ">="
ppBinop Identifiers.EQ  = text "=="
ppBinop Identifiers.NEQ = text "!="
ppBinop Identifiers.PLUS  = text "+"
ppBinop Identifiers.MINUS = text "-"
ppBinop Identifiers.TIMES  = text "*"
ppBinop Identifiers.DIV = text "/"
ppBinop Identifiers.MOD = text "%"
