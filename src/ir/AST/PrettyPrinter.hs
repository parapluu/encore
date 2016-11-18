{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

Prints the source code that an "AST.AST" represents. Each node in
the abstract syntax tree has a corresponding pretty-print function
(although not all are exported)

-}

module AST.PrettyPrinter (ppExpr
                         ,ppProgram
                         ,ppParamDecl
                         ,ppFieldDecl
                         ,indent
                         ,ppSugared
                         ,ppFunctionHeader
                         ) where

-- Library dependencies
import Text.PrettyPrint

-- Module dependencies
import Identifiers
import Types
import AST.AST

indent = nest 2

commaSep l = cat $ punctuate ", " l
angles s = cat ["<", s, ">"]

ppName :: Name -> Doc
ppName = text . show

ppNamespace :: Namespace -> Doc
ppNamespace = text . showNamespace

ppQName :: QualifiedName -> Doc
ppQName = text . show

ppType :: Type -> Doc
ppType = text . show

ppProgram :: Program -> Doc
ppProgram Program{moduledecl, etl, imports, typedefs, functions, classes} =
    ppModuleDecl moduledecl $+$
    vcat (map ppEmbedded etl) <+>
    vcat (map ppImportDecl imports) $+$
    vcat (map ppTypedef typedefs) $+$
    vcat (map ppFunction functions) $+$
    vcat (map ppClassDecl classes) $+$
    "" -- new line at end of file

ppEmbedded EmbedTL{etlheader=header, etlbody=code} =
  ppHeader header code

ppHeader header code =
  if null header && null code
  then empty
  else "embed" $+$ text header $+$ "body" $+$ text code $+$ "end\n"

ppModuleDecl :: ModuleDecl -> Doc
ppModuleDecl NoModule = empty
ppModuleDecl Module{modname} = "module" <+> ppName modname

ppImportDecl :: ImportDecl -> Doc
ppImportDecl Import {itarget} = "import" <+> ppNamespace itarget

ppTypedef :: Typedef -> Doc
ppTypedef Typedef { typedefdef=t } =
    "typedef" <+>
    ppType t <+>
    "=" <+>
    ppType (typeSynonymRHS t)

ppFunctionHeader :: FunctionHeader -> Doc
ppFunctionHeader header =
    ppName (hname header) <>
    parens (commaSep (map ppParamDecl (hparams header))) <+>
    ":" <+> ppType (htype header)

ppFunctionHelper :: FunctionHeader -> Expr -> Doc
ppFunctionHelper funheader funbody =
    "def" <+> ppFunctionHeader funheader $+$
        indent (ppExpr funbody)

ppFunction :: Function -> Doc
ppFunction Function {funheader, funbody} =
    ppFunctionHelper funheader funbody
ppFunction MatchingFunction {matchfunheaders, matchfunbodies} =
    foldr ($+$) "" (zipWith ppFunctionHelper matchfunheaders matchfunbodies)

ppClassDecl :: ClassDecl -> Doc
ppClassDecl Class {cname, cfields, cmethods} =
    "class" <+> ppType cname $+$
        indent (vcat (map ppFieldDecl cfields) $$
                vcat (map ppMethodDecl cmethods))

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl = text . show

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param {pname, ptype}) =  ppName pname <+> ":" <+> ppType ptype

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl m =
    let header = mheader m
        body = mbody m
        def | isStreamMethod m = "stream"
            | otherwise = "def"
    in
      def <+> ppFunctionHeader header $+$
          indent (ppExpr body)

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
ppExpr Skip {} = "()"
ppExpr MethodCall {target, name, args} =
    maybeParens target <> "." <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr MessageSend {target, name, args} =
    maybeParens target <> "!" <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr Liftf {val} = "liftf" <+> ppExpr val
ppExpr Liftv {val} = "liftv" <+> ppExpr val
ppExpr PartyJoin {val} = "join" <+> ppExpr val
ppExpr PartyExtract {val} = "extract" <+> ppExpr val
ppExpr PartyEach {val} = "each" <+> ppExpr val
ppExpr PartySeq {par, seqfunc} = ppExpr par <+> ">>" <+> ppExpr seqfunc
ppExpr PartyPar {parl, parr} = ppExpr parl <+> "||" <+> ppExpr parr
ppExpr FunctionCall {qname, args} =
    ppQName qname <> parens (commaSep (map ppExpr args))
ppExpr FunctionAsValue {qname, typeArgs} =
  ppQName qname <> angles (commaSep (map ppType typeArgs))
ppExpr Closure {eparams, body} =
    "\\" <> parens (commaSep (map ppParamDecl eparams)) <+> "->" <+> ppExpr body
ppExpr Async {body} =
    "async" <> parens (ppExpr body)
ppExpr (MaybeValue _ (JustData a)) = "Just" <+> ppExpr a
ppExpr (MaybeValue _ NothingData) = "Nothing"
ppExpr Tuple {args} = parens (commaSep (map ppExpr args))
ppExpr Let {decls, body} =
  "let" <+> vcat (map (\(Name x, e) -> text x <+> "=" <+> ppExpr e) decls) $+$
  "in" $+$ indent (ppExpr body)
ppExpr MiniLet {decl = (x, val)} =
    "let" <+> ppName x <+> "=" <+> ppExpr val
ppExpr Seq {eseq = [expr]} =
    ppExpr expr
ppExpr Seq {eseq} =
    braces $ vcat $ punctuate ";" (map ppExpr eseq)
ppExpr IfThenElse {cond, thn, els} =
    "if" <+> ppExpr cond <+> "then" $+$
         indent (ppExpr thn) $+$
    "else" $+$
         indent (ppExpr els)
ppExpr IfThen {cond, thn} =
    "if" <+> ppExpr cond <+> "then" $+$
         indent (ppExpr thn)
ppExpr Unless {cond, thn} =
    "unless" <+> ppExpr cond <+> "then" $+$
         indent (ppExpr thn)
ppExpr While {cond, body} =
    "while" <+> ppExpr cond $+$
         indent (ppExpr body)
ppExpr Repeat {name, times, body} =
    "repeat" <+> ppName name <+> "<-" <+> ppExpr times $+$
         indent (ppExpr body)
ppExpr For {name, step = IntLiteral{intLit = 1}, src, body} =
    "for" <+> ppName name <+> "in" <+> ppExpr src $+$
         indent (ppExpr body)
ppExpr For {name, step, src, body} =
    "for" <+> ppName name <+> "in" <+> ppExpr src <+> "by" <+> ppExpr step $+$
         indent (ppExpr body)
ppExpr Match {arg, clauses} =
    "match" <+> ppExpr arg <+> "with" $+$
         ppMatchClauses clauses
    where
      ppClause (MatchClause {mcpattern, mchandler, mcguard = BTrue{}}) =
        indent (ppExpr mcpattern <+> "=>" <+> ppExpr mchandler)
      ppClause (MatchClause {mcpattern, mchandler, mcguard}) =
        indent (ppExpr mcpattern <+> "when" <+> ppExpr mcguard <+>
                       "=>" <+> ppExpr mchandler)
      ppMatchClauses = foldr (($+$) . ppClause) ""
ppExpr FutureChain {future, chain} =
    ppExpr future <+> "~~>" <+> ppExpr chain
ppExpr Get {val} = "get" <+> ppExpr val
ppExpr Yield {val} = "yield" <+> ppExpr val
ppExpr Eos {} = "eos"
ppExpr Await {val} = "await" <+> ppExpr val
ppExpr IsEos {target} = ppExpr target <> "." <> "eos" <> parens empty
ppExpr StreamNext {target} = ppExpr target <> "." <> "next" <> parens empty
ppExpr Suspend {} = "suspend"
ppExpr FieldAccess {target, name} = maybeParens target <> "." <> ppName name
ppExpr ArrayAccess {target, index} = ppExpr target <> brackets (ppExpr index)
ppExpr ArraySize {target} = "|" <> ppExpr target <> "|"
ppExpr ArrayNew {ty, size} = brackets (ppType ty) <> parens (ppExpr size)
ppExpr ArrayLiteral {args} = brackets $ commaSep (map ppExpr args)
ppExpr VarAccess {qname} = ppQName qname
ppExpr Assign {lhs, rhs} = ppExpr lhs <+> "=" <+> ppExpr rhs
ppExpr Null {} = "null"
ppExpr BTrue {} = "true"
ppExpr BFalse {} = "false"
ppExpr NewWithInit {ty, args} =
  "new" <+> ppType ty <> parens (commaSep (map ppExpr args))
ppExpr New {ty} = "new" <+> ppType ty
ppExpr Peer {ty} = "peer" <+> ppType ty
ppExpr Print {args} = "print" <> parens (commaSep (map ppExpr args))
ppExpr Exit {args} = "exit" <> parens (commaSep (map ppExpr args))
ppExpr StringLiteral {stringLit} = text $ show stringLit
ppExpr CharLiteral {charLit} = text $ show charLit
ppExpr UIntLiteral {intLit} = int intLit <> "u"
ppExpr IntLiteral {intLit} = int intLit
ppExpr RealLiteral {realLit} = double realLit
ppExpr RangeLiteral {start, stop, step} =
  "[" <+> ppExpr start <+> "," <+> ppExpr stop <+> "by" <+> ppExpr step <+> "]"
ppExpr Embed {ty, embedded} =
  "embed" <+> ppType ty <+>
          hcat (map (uncurry ppPair) embedded) <+> "end"
  where
    ppPair code Skip{} = text code
    ppPair code expr = text code <> "#{" <> ppExpr expr <> "}#"
ppExpr Unary {uop, operand} = ppUnary uop <+> ppExpr operand
ppExpr Binop {binop, loper, roper} =
  ppExpr loper <+> ppBinop binop <+> ppExpr roper
ppExpr TypedExpr {body, ty} = ppExpr body <+> ":" <+> ppType ty
ppExpr Foreach {item, arr, body} =
  "foreach" <+> ppName item <+> "in" <+> ppExpr arr <>
                braces (ppExpr body)
ppExpr FinishAsync {body} = "finish" <+> ppExpr body

ppUnary :: UnaryOp -> Doc
ppUnary Identifiers.NOT = "not"
ppUnary Identifiers.NEG = "-"

ppBinop :: BinaryOp -> Doc
ppBinop Identifiers.AND   = "and"
ppBinop Identifiers.OR    = "or"
ppBinop Identifiers.LT    = "<"
ppBinop Identifiers.GT    = ">"
ppBinop Identifiers.LTE   = "<="
ppBinop Identifiers.GTE   = ">="
ppBinop Identifiers.EQ    = "=="
ppBinop Identifiers.NEQ   = "!="
ppBinop Identifiers.PLUS  = "+"
ppBinop Identifiers.MINUS = "-"
ppBinop Identifiers.TIMES = "*"
ppBinop Identifiers.DIV   = "/"
ppBinop Identifiers.MOD   = "%"
