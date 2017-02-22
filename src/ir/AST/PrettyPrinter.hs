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
import qualified Text.PrettyPrint as P
import Text.PrettyPrint hiding(brackets)

-- Module dependencies
import Identifiers
import Types
import AST.AST

indent = nest 2

commaSep l = cat $ punctuate ", " l
brackets s = cat ["[", s, "]"]

ppMut :: Mutability -> Doc
ppMut Val = "val"
ppMut Var = "var"

ppName :: Name -> Doc
ppName = text . show

ppNamespace :: Namespace -> Doc
ppNamespace = text . show

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
    ppTypeParams (htypeparams header) <>
    parens (commaSep $ map ppParamDecl $ hparams header) <+>
    ":" <+> ppType (htype header)

ppTypeParams :: [Type] -> Doc
ppTypeParams params =
  if null params
  then empty
  else P.brackets (commaSep $ map ppType params)

ppFunctionHelper :: FunctionHeader -> Expr -> Doc
ppFunctionHelper funheader funbody =
    "fun" <+> ppFunctionHeader funheader $+$
        indent (ppBody funbody) $+$
    "end"

ppFunction :: Function -> Doc
ppFunction Function {funheader, funbody} =
    ppFunctionHelper funheader funbody
ppFunction MatchingFunction {matchfunheaders, matchfunbodies} =
    foldr ($+$) "" (zipWith ppFunctionHelper matchfunheaders matchfunbodies)

ppTraitExtension :: TraitExtension -> Doc
ppTraitExtension FieldExtension{extname} = ppName extname
ppTraitExtension MethodExtension{extname} = ppName extname <> "()"

ppComposition :: TraitComposition -> Doc
ppComposition Conjunction{tcleft, tcright} =
  ppConjunctionChild tcleft <+> "*" <+> ppConjunctionChild tcright
  where
    ppConjunctionChild disj@Disjunction{} = "(" <> ppComposition disj <> ")"
    ppConjunctionChild c = ppComposition c
ppComposition Disjunction{tcleft, tcright} =
  ppComposition tcleft <+> "+" <+> ppComposition tcright
ppComposition TraitLeaf{tcname, tcext} =
  ppType tcname <> parens (commaSep (map ppTraitExtension tcext))

ppClassDecl :: ClassDecl -> Doc
ppClassDecl Class {cname, cfields, cmethods, ccomposition} =
    "class" <+> ppType cname <+> compositionDoc $+$
        indent (vcat (map ppFieldDecl cfields) $$
                vcat (map ppMethodDecl cmethods)) $+$
    "end"
  where
    compositionDoc =
      case ccomposition of
        Just c -> ":" <+> ppComposition c
        Nothing -> empty

ppFieldDecl :: FieldDecl -> Doc
ppFieldDecl = text . show

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (Param {pmut = Val, pname, ptype}) =
    ppName pname <+> ":" <+> ppType ptype
ppParamDecl (Param {pmut = Var, pname, ptype}) =
    "var" <+> ppName pname <+> ":" <+> ppType ptype

ppMethodDecl :: MethodDecl -> Doc
ppMethodDecl m =
    let header = mheader m
        body = mbody m
        def | isStreamMethod m = "stream"
            | otherwise = "def"
    in
      def <+> ppFunctionHeader header $+$
          indent (ppBody body) $+$
      "end"

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

ppBody (Seq {eseq}) = vcat $ map ppExpr eseq
ppBody e = ppExpr e

ppExpr :: Expr -> Doc
ppExpr Skip {} = "()"
ppExpr MethodCall {target, name, args} =
    maybeParens target <> "." <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr MessageSend {target, name, args} =
    maybeParens target <> "!" <> ppName name <>
      parens (commaSep (map ppExpr args))
ppExpr Liftf {val} = "liftf" <+> parens (ppExpr val)
ppExpr Liftv {val} = "liftv" <+> parens (ppExpr val)
ppExpr PartyJoin {val} = "join" <+> parens (ppExpr val)
ppExpr PartyExtract {val} = "extract" <+> parens(ppExpr val)
ppExpr PartyEach {val} = "each" <+> parens(ppExpr val)
ppExpr PartySeq {par, seqfunc} = ppExpr par <+> ">>" <+> ppExpr seqfunc
ppExpr PartyPar {parl, parr} = ppExpr parl <+> "|||" <+> ppExpr parr
ppExpr PartyReduce {seqfun, pinit, par} = "reduce" <>
    parens (commaSep $ ppExpr <$> [seqfun, pinit, par])
ppExpr FunctionCall {qname, args, typeArguments = []} =
    ppQName qname <> parens (commaSep (map ppExpr args))
ppExpr FunctionCall {qname, args, typeArguments} =
    ppQName qname <> P.brackets (commaSep (map ppType typeArguments)) <>
                     parens (commaSep (map ppExpr args))
ppExpr FunctionAsValue {qname, typeArgs} =
  ppQName qname <> P.brackets (commaSep (map ppType typeArgs))
ppExpr Closure {eparams, mty, body=b@(Seq {})} =
    "fun" <+> parens (commaSep (map ppParamDecl eparams)) <+> returnType mty $+$
       indent (ppExpr b) $+$
    "end"
  where
    returnType Nothing = ""
    returnType (Just t) = ":" <+> ppType t
ppExpr Closure {eparams, body} =
    "fun" <+> parens (commaSep (map ppParamDecl eparams)) <+> "=>" <+> ppExpr body
ppExpr Async {body=b@(Seq {})} =
  "async" $+$
    ppExpr b $+$
  "end"
ppExpr Async {body} = "async" <> parens (ppExpr body)
ppExpr (MaybeValue _ (JustData a)) = "Just" <> parens (ppExpr a)
ppExpr (MaybeValue _ NothingData) = "Nothing"
ppExpr Tuple {args} = parens (commaSep (map ppExpr args))
ppExpr Let {decls, body} =
  "let" $+$
      indent (vcat (map (\(Name x, e) -> text x <+> "=" <+> ppExpr e) decls)) $+$
  "in" $+$
      indent (ppBody body) $+$
  "end"
ppExpr MiniLet {mutability, decl = (x, val)} =
    ppMut mutability <+> ppName x <+> "=" <+> ppExpr val
ppExpr Seq {eseq = [expr]} = ppExpr expr
ppExpr Seq {eseq} =
    "do" $+$
      indent (vcat (map ppExpr eseq)) $+$
    "end"
ppExpr IfThenElse {cond, thn, els} =
    "if" <+> ppExpr cond <+> "then" $+$
         indent (ppBody thn) $+$
    "else" $+$
         indent (ppBody els) $+$
    "end"
ppExpr IfThen {cond, thn} =
    "if" <+> ppExpr cond <+> "then" $+$
         indent (ppBody thn) $+$
    "end"
ppExpr Unless {cond, thn} =
    "unless" <+> ppExpr cond <+> "then" $+$
         indent (ppBody thn) $+$
    "end"
ppExpr While {cond, body} =
    "while" <+> ppExpr cond <+> "do" $+$
         indent (ppBody body) $+$
    "end"
ppExpr Repeat {name, times, body} =
    "repeat" <+> ppName name <+> "<-" <+> ppExpr times <+> "do" $+$
         indent (ppBody body) $+$
    "end"
ppExpr For {name, step = IntLiteral{intLit = 1}, src, body} =
    "for" <+> ppName name <+> "<-" <+> ppExpr src <+> "do" $+$
         indent (ppBody body) $+$
    "end"
ppExpr For {name, step, src, body} =
    "for" <+> ppName name <+> "<-" <+> ppExpr src <+>
    "by" <+> ppExpr step <+> "do" $+$
         indent (ppBody body) $+$
    "end"
ppExpr Match {arg, clauses} =
    "match" <+> ppExpr arg <+> "with" $+$
         ppMatchClauses clauses $+$
    "end"
    where
      ppClause (MatchClause {mcpattern, mchandler, mcguard = BTrue{}}) =
        indent "case" <+> (ppExpr mcpattern <+> "=>" <+> multipleLines mchandler)
      ppClause (MatchClause {mcpattern, mchandler, mcguard}) =
        indent "case" <+> (ppExpr mcpattern <+> "when" <+> ppExpr mcguard <+>
                       "=>" <+> multipleLines mchandler)
      ppMatchClauses = foldr (($+$) . ppClause) ""
      multipleLines s@(Seq {}) = ""
        $+$
          indent (ppBody s) $+$
        "end"
      multipleLines e = ppExpr e
ppExpr FutureChain {future, chain} =
    ppExpr future <+> "~~>" <+> ppExpr chain
ppExpr Get {val} = "get" <> parens (ppExpr val)
ppExpr Yield {val} = "yield" <> parens (ppExpr val)
ppExpr Eos {} = "eos"
ppExpr Await {val} = "await" <+> parens (ppExpr val)
ppExpr IsEos {target} = ppExpr target <> "." <> "eos" <> parens empty
ppExpr StreamNext {target} = ppExpr target <> "." <> "next" <> parens empty
ppExpr Suspend {} = "suspend"
ppExpr FieldAccess {target, name} = maybeParens target <> "." <> ppName name
ppExpr ArrayAccess {target, index} = ppExpr target <> parens (ppExpr index)
ppExpr ArraySize {target} = "|" <> ppExpr target <> "|"
ppExpr ArrayNew {ty, size} = "new" <+> brackets (ppType ty) <> parens (ppExpr size)
ppExpr ArrayLiteral {args} = brackets $ commaSep (map ppExpr args)
ppExpr VarAccess {qname} = ppQName qname
ppExpr Assign {lhs, rhs} = ppExpr lhs <+> "=" <+> ppExpr rhs
ppExpr Null {} = "null"
ppExpr BTrue {} = "true"
ppExpr BFalse {} = "false"
ppExpr NewWithInit {ty, args} =
  "new" <+> ppType ty <> parens (commaSep (map ppExpr args))
ppExpr New {ty} = "new" <+> ppType ty
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
  "embed" <+> ppType ty $+$
          indent (hcat (map (uncurry ppPair) embedded)) $+$
  "end"
  where
    ppPair code Skip{} = text code
    ppPair code expr = text code <> "#{" <> ppExpr expr <> "}"
ppExpr Unary {uop, operand} = ppUnary uop <+> ppExpr operand
ppExpr Binop {binop, loper, roper} =
  ppExpr loper <+> ppBinop binop <+> ppExpr roper
ppExpr TypedExpr {body, ty} = ppExpr body <+> ":" <+> ppType ty

ppUnary :: UnaryOp -> Doc
ppUnary Identifiers.NOT = "not"
ppUnary Identifiers.NEG = "-"

ppBinop :: BinaryOp -> Doc
ppBinop Identifiers.AND   = "&&"
ppBinop Identifiers.OR    = "||"
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
