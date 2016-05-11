{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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
import Data.List(intercalate)

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
ppFor = text "for"
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
ppEnd = text "end"
ppForeach = text "foreach"
ppFinish = text "finish"
ppDot = text "."
ppBang = text "!"
ppColon = text ":"
ppComma = text ","
ppSemicolon = text ";"
ppEquals = text "="
ppSpace = text " "
ppLambda = text "\\"
ppArrow = text "->"
ppSeq = text ">>"
ppLiftf = text "liftf"
ppLiftv = text "liftv"
ppJoin = text "join"
ppPartyExtract = text "extract"
ppPartyEach = text "each"
ppTask = text "async"
ppPar = text "||"
ppBar = text "|"
ppJust = text "Just"
ppNothing = text "Nothing"
ppMatch = text "match"
ppWith = text "with"
ppTypeDef = text "typedef"
ppMatchArrow = text "=>"

indent = nest 2

commaSep l = cat $ punctuate (ppComma <> ppSpace) l

ppName :: Name -> Doc
ppName (Name x) = text x

ppQName :: QName -> Doc
ppQName q = cat $ punctuate ppDot (map ppName q)

ppType :: Type -> Doc
ppType = text . show

ppProgram :: Program -> Doc
ppProgram Program{bundle, etl=EmbedTL{etlheader=header, etlbody=code},
  imports, typedefs, functions, classes} =
    ppBundleDecl bundle $+$
    ppHeader header code <+>
    vcat (map ppImportDecl imports) $+$
    vcat (map ppTypedef typedefs) $+$
    vcat (map ppFunction functions) $+$
    vcat (map ppClassDecl classes) $+$
    text "" -- new line at end of file

ppHeader header code =
  if null header && null code
  then empty
  else text "embed" $+$ text header $+$ text "body" $+$ text code $+$ text "end\n"

ppBundleDecl :: BundleDecl -> Doc
ppBundleDecl NoBundle = empty
ppBundleDecl Bundle{bname} = text "bundle" <+> ppQName bname

ppImportDecl :: ImportDecl -> Doc
ppImportDecl Import {itarget} = text "import" <+> ppQName itarget
ppImportDecl PulledImport {} = error "Cannot pretty-print a pulled import"

ppTypedef :: Typedef -> Doc
ppTypedef Typedef { typedefdef=t } =
    text "typedef" <+>
    ppType t <+>
    text "=" <+>
    ppType (typeSynonymRHS t) 

ppFunctionHeader :: FunctionHeader -> Doc
ppFunctionHeader header =
    ppName (hname header) <>
    parens (commaSep (map ppParamDecl (hparams header))) <+>
    text ":" <+> ppType (htype header)

ppFunctionHelper :: FunctionHeader -> Expr -> Doc
ppFunctionHelper funheader funbody =
    text "def" <+> ppFunctionHeader funheader $+$
        indent (ppExpr funbody)

ppFunction :: Function -> Doc
ppFunction Function {funheader, funbody} =
    ppFunctionHelper funheader funbody
ppFunction MatchingFunction {matchfunheaders, matchfunbodies} =
    foldr ($+$) (text "") (zipWith ppFunctionHelper matchfunheaders matchfunbodies)

ppClassDecl :: ClassDecl -> Doc
ppClassDecl Class {cname, cfields, cmethods} =
    ppClass <+> ppType cname $+$
        (indent $
                vcat (map ppFieldDecl cfields) $$
                vcat (map ppMethodDecl cmethods))

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl = text . show

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param {pname, ptype}) =  ppName pname <+> text ":" <+> ppType ptype

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl m =
    let header = mheader m
        body = mbody m
        def | isStreamMethod m = text "stream"
            | otherwise = text "def"
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
ppExpr Skip {} = ppSkip
ppExpr Breathe {} = ppBreathe
ppExpr MethodCall {target, name, args} =
    maybeParens target <> ppDot <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr MessageSend {target, name, args} =
    maybeParens target <> ppBang <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr Liftf {val} = ppLiftf <+> ppExpr val
ppExpr Liftv {val} = ppLiftv <+> ppExpr val
ppExpr PartyJoin {val} = ppJoin <+> ppExpr val
ppExpr PartyExtract {val} = ppPartyExtract <+> ppExpr val
ppExpr PartyEach {val} = ppPartyEach <+> ppExpr val
ppExpr PartySeq {par, seqfunc} = ppExpr par <+> ppSeq <+> ppExpr seqfunc
ppExpr PartyPar {parl, parr} = ppExpr parl <+> ppPar <+> ppExpr parr
ppExpr FunctionCall {name, args} =
    ppName name <> parens (commaSep (map ppExpr args))
ppExpr Closure {eparams, body} =
    ppLambda <> parens (commaSep (map ppParamDecl eparams)) <+> ppArrow <+> ppExpr body
ppExpr Async {body} =
    ppTask <> parens (ppExpr body)
ppExpr (MaybeValue _ (JustData a)) = ppJust <+> ppExpr a
ppExpr (MaybeValue _ NothingData) = ppNothing
ppExpr Tuple {args} = parens (commaSep (map ppExpr args))
ppExpr Let {decls, body} =
    ppLet <+> vcat (map (\(Name x, e) -> text x <+> equals <+> ppExpr e) decls) $+$ ppIn $+$
      indent (ppExpr body)
ppExpr MiniLet {decl = (x, val)} =
    ppLet <+> ppName x <+> equals <+> ppExpr val
ppExpr Seq {eseq = [expr]} =
    ppExpr expr
ppExpr Seq {eseq} =
    braces $ vcat $ punctuate ppSemicolon (map ppExpr eseq)
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
    ppRepeat <+> ppName name <+> text "<-" <+> ppExpr times $+$
         indent (ppExpr body)
ppExpr For {name, step = IntLiteral{intLit = 1}, src, body} =
    ppFor <+> ppName name <+> ppIn <+> ppExpr src $+$
         indent (ppExpr body)
ppExpr For {name, step, src, body} =
    ppFor <+> ppName name <+> ppIn <+> ppExpr src <+> text "by" <+> ppExpr step $+$
         indent (ppExpr body)
ppExpr Match {arg, clauses} =
    ppMatch <+> ppExpr arg <+> text "with" $+$
         ppMatchClauses clauses
    where
      ppClause (MatchClause {mcpattern, mchandler, mcguard = BTrue{}}) =
        indent (ppExpr mcpattern <+> text "=>" <+> ppExpr mchandler)
      ppClause (MatchClause {mcpattern, mchandler, mcguard}) =
        indent (ppExpr mcpattern <+> text "when" <+> ppExpr mcguard <+>
                       text "=>" <+> ppExpr mchandler)
      ppMatchClauses = foldr (($+$) . ppClause) (text "")
ppExpr FutureChain {future, chain} =
    ppExpr future <+> text "~~>" <+> ppExpr chain
ppExpr Get {val} = ppGet <+> ppExpr val
ppExpr Yield {val} = ppYield <+> ppExpr val
ppExpr Eos {} = ppEos <> parens empty
ppExpr Await {val} = ppAwait <+> ppExpr val
ppExpr IsEos {target} = ppExpr target <> ppDot <> ppEos <> parens empty
ppExpr StreamNext {target} = ppExpr target <> ppDot <> text "next" <> parens empty
ppExpr Suspend {} = ppSuspend
ppExpr FieldAccess {target, name} = maybeParens target <> ppDot <> ppName name
ppExpr ArrayAccess {target, index} = ppExpr target <> brackets (ppExpr index)
ppExpr ArraySize {target} = ppBar <> ppExpr target <> ppBar
ppExpr ArrayNew {ty, size} = brackets (ppType ty) <> parens (ppExpr size)
ppExpr ArrayLiteral {args} = brackets $ commaSep (map ppExpr args)
ppExpr VarAccess {name} = ppName name
ppExpr Assign {lhs, rhs} = ppExpr lhs <+> ppEquals <+> ppExpr rhs
ppExpr Null {} = ppNull
ppExpr BTrue {} = ppTrue
ppExpr BFalse {} = ppFalse
ppExpr NewWithInit {ty, args} = ppNew <+> ppType ty <> parens (commaSep (map ppExpr args))
ppExpr New {ty} = ppNew <+> ppType ty
ppExpr Peer {ty} = ppPeer <+> ppType ty
ppExpr Print {args} = ppPrint <> parens (commaSep (map ppExpr args))
ppExpr Exit {args} = ppExit <> parens (commaSep (map ppExpr args))
ppExpr StringLiteral {stringLit} = text (show stringLit)
ppExpr CharLiteral {charLit} = text $ show charLit
ppExpr IntLiteral {intLit} = int intLit
ppExpr RealLiteral {realLit} = double realLit
ppExpr RangeLiteral {start, stop, step} = text "[" <+> ppExpr start <+> text "," <+> ppExpr stop <+> text " by " <+> ppExpr step <+> text"]"
ppExpr Embed {ty, code} = ppEmbed <+> ppType ty <+> text code <+> ppEnd
ppExpr Unary {uop, operand} = ppUnary uop <+> ppExpr operand
ppExpr Binop {binop, loper, roper} = ppExpr loper <+> ppBinop binop <+> ppExpr roper
ppExpr TypedExpr {body, ty} = ppExpr body <+> ppColon <+> ppType ty
ppExpr Foreach {item, arr, body} = ppForeach <+> ppName item <+> ppIn <+> ppExpr arr <>
                                   braces (ppExpr body)
ppExpr FinishAsync {body} = ppFinish <+> ppExpr body

ppUnary :: UnaryOp -> Doc
ppUnary Identifiers.NOT = text "not"
ppUnary Identifiers.NEG = text "-"

ppBinop :: BinaryOp -> Doc
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
