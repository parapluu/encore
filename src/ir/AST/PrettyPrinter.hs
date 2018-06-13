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
import Data.Text.Prettyprint.Doc hiding(indent)

-- Module dependencies
import Identifiers
import Types
import AST.AST

indent = nest 2
($+$) s e = s <> line <> e

commaSep l = hcat $ punctuate ", " l
--brackets s = hcat ["[", s, "]"]

ppMut :: Mutability -> Doc ann
ppMut Val = "val"
ppMut Var = "var"

ppName :: Name -> Doc ann
ppName = viaShow

ppNamespace :: Namespace -> Doc ann
ppNamespace = viaShow

ppQName :: QualifiedName -> Doc ann
ppQName = viaShow

ppType :: Type -> Doc ann
ppType = viaShow

ppProgram :: Program -> Doc ann
ppProgram Program{moduledecl, etl, imports, typedefs, functions, traits, classes} =
    ppModuleDecl moduledecl $+$
    vcat (map ppEmbedded etl) <+>
    vcat (map ppImportDecl imports) $+$
    vcat (map ppTypedef typedefs) $+$
    -- TODO: reverse these somewhere else...
    vcat (reverse $ map ppFunction functions) $+$
    vcat (reverse $ map ppTraitDecl traits) $+$
    vcat (reverse $ map ppClassDecl classes) $+$
    "" -- new line at end of file

ppEmbedded EmbedTL{etlheader=header, etlbody=code} =
  ppHeader header code

ppHeader header code =
  if null header && null code
  then emptyDoc
  else "EMBED" $+$ pretty header $+$ "BODY" $+$ pretty code $+$ "END\n"

ppModuleDecl :: ModuleDecl -> Doc ann
ppModuleDecl NoModule = emptyDoc
ppModuleDecl Module{modname, modexports} =
  "module" <+> ppName modname <>
               case modexports of
                 Just names -> parens (commaSep $ map ppName names)
                 Nothing -> emptyDoc

ppImportDecl :: ImportDecl -> Doc ann
ppImportDecl Import {itarget
                    ,iqualified
                    ,ihiding
                    ,iselect
                    ,ialias
                    } =
  let import' = if iqualified
                then "import" <+> "qualified"
                else "import"
  in import' <+> ppNamespace itarget <> maybeSelect <> maybeHiding <> maybeAlias
  where
    maybeSelect =
      case iselect of
        Just names -> parens (commaSep $ map ppName names)
        Nothing -> emptyDoc
    maybeHiding =
      case ihiding of
        Just names -> " hiding" <> parens (commaSep $ map ppName names)
        Nothing -> emptyDoc
    maybeAlias =
      case ialias of
        Just alias -> " as" <+> ppNamespace alias
        Nothing -> emptyDoc

ppTypedef :: Typedef -> Doc ann
ppTypedef Typedef { typedefdef=t } =
    "typedef" <+>
    ppType t <+>
    "=" <+>
    ppType (typeSynonymRHS t)

ppFunctionHeader :: FunctionHeader -> Doc ann
ppFunctionHeader header =
    ppName (hname header) <>
    ppTypeParams (htypeparams header) <>
    parens (commaSep $ map ppParamDecl $ hparams header) <+>
    ":" <+> ppType (htype header)

ppTypeParams :: [Type] -> Doc ann
ppTypeParams params =
  if null params
  then emptyDoc
  else brackets (commaSep $ map ppTypeParam params)
  where
  ppTypeParam ty
    | Just bound <- getBound ty = ppType ty <+> ":" <+> ppType bound
    | otherwise = ppType ty

ppFunctionHelper :: FunctionHeader -> Expr -> [Function] -> Doc ann
ppFunctionHelper funheader funbody [] =
    "fun" <+> ppFunctionHeader funheader $+$
        indent (ppBody funbody) $+$
    "end"
ppFunctionHelper funheader funbody funlocals =
    "fun" <+> ppFunctionHeader funheader $+$
        indent (ppBody funbody) $+$
    "where" $+$
        indent (vcat $ map ppFunction funlocals) $+$
    "end"

ppFunction :: Function -> Doc ann
ppFunction Function {funheader, funbody, funlocals} =
  ppFunctionHelper funheader funbody funlocals

ppTraitDecl :: TraitDecl -> Doc ann
ppTraitDecl Trait {tname, treqs, tmethods} =
    trait <+> pretty (showWithoutMode tname) $+$
        indent (vcat (map ppRequirement treqs) $+$
                vcat (map ppMethodDecl tmethods)) $+$
    "end"
  where
    trait | isModeless tname = "trait"
          | otherwise = pretty $ showModeOf tname ++ " trait"
    ppRequirement RequiredField{rfield} =
      "require" <+> ppFieldDecl rfield
    ppRequirement RequiredMethod{rheader} =
      "require" <+> "def" <+> ppFunctionHeader rheader

ppTraitExtension :: TraitExtension -> Doc ann
ppTraitExtension FieldExtension{extname} = ppName extname
ppTraitExtension MethodExtension{extname} = ppName extname <> "()"

ppComposition :: TraitComposition -> Doc ann
ppComposition Conjunction{tcleft, tcright} =
  ppConjunctionChild tcleft <+> "*" <+> ppConjunctionChild tcright
  where
    ppConjunctionChild disj@Disjunction{} = "(" <> ppComposition disj <> ")"
    ppConjunctionChild c = ppComposition c
ppComposition Disjunction{tcleft, tcright} =
  ppComposition tcleft <+> "+" <+> ppComposition tcright
ppComposition TraitLeaf{tcname, tcext} =
  ppType tcname <> if null tcext
                   then emptyDoc
                   else parens (commaSep (map ppTraitExtension tcext))

ppClassDecl :: ClassDecl -> Doc ann
ppClassDecl Class {cname, cfields, cmethods, ccomposition} =
    clss <+> pretty (showWithoutMode cname) <+> compositionDoc $+$
        indent (vcat (map ppFieldDecl cfields) $+$
                vcat (map ppMethodDecl cmethods)) $+$
    "end"
  where
    clss | isModeless cname = "class"
         | otherwise = pretty $ showModeOf cname ++ " class"
    compositionDoc =
      case ccomposition of
        Just c -> ":" <+> ppComposition c
        Nothing -> emptyDoc

ppFieldDecl :: FieldDecl -> Doc ann
ppFieldDecl = viaShow

ppParamDecl :: ParamDecl -> Doc ann
ppParamDecl (Param {pmut = Val, pname, ptype}) =
    ppName pname <+> ":" <+> ppType ptype
ppParamDecl (Param {pmut = Var, pname, ptype}) =
    "var" <+> ppName pname <+> ":" <+> ppType ptype

ppMethodDecl :: MethodDecl -> Doc ann
ppMethodDecl m =
    let header = mheader m
        modifiers = hmodifiers header
        body = mbody m
        def | isStreamMethod m = "stream"
            | otherwise = "def"
        def' = if null modifiers
               then def
               else def <+> ppModifiers modifiers
    in
      def' <+> ppFunctionHeader header $+$
          indent (ppBody body) $+$
      endOrLocals
    where
      ppModifiers [] = emptyDoc
      ppModifiers mods = hcat $ punctuate " " $
                         map (viaShow) mods
      endOrLocals
        | null (mlocals m) = "end"
        | otherwise =
            "where" $+$
              indent (vcat $ map ppFunction (mlocals m)) $+$
            "end"

isSimple :: Expr -> Bool
isSimple VarAccess {} = True
isSimple FieldAccess {target} = isSimple target
isSimple MethodCall {target} = isSimple target
isSimple MessageSend {target} = isSimple target
isSimple FunctionCall {} = True
isSimple _ = False

maybeParens :: Expr -> Doc ann
maybeParens e
    | isSimple e = ppExpr e
    | otherwise  = parens $ ppExpr e

ppSugared :: Expr -> Doc ann
ppSugared e = case getSugared e of
                Just e' -> ppExpr e'
                Nothing -> ppExpr e

ppBody :: Expr -> Doc ann
ppBody (Seq {eseq}) = vcat $ map ppExpr eseq
ppBody e = ppExpr e

withTypeArguments :: [Type] -> Doc ann
withTypeArguments typeArguments =
  if null typeArguments
  then emptyDoc
  else brackets (commaSep (map ppType typeArguments))

ppExpr :: Expr -> Doc ann
ppExpr Skip {} = "()"
ppExpr Break {} = "break"
ppExpr Continue {} = "Continue"

ppExpr Optional {optTag = QuestionBang MessageSend {target, name, args, typeArguments}} =
    maybeParens target <> "?!" <> ppName name <>
      withTypeArguments typeArguments <>
      parens (commaSep (map ppExpr args))
ppExpr Optional {optTag = QuestionDot MethodCall {target, name, args, typeArguments}} =
    maybeParens target <> "?." <> ppName name <>
      withTypeArguments typeArguments <>
      parens (commaSep (map ppExpr args))
ppExpr Optional {optTag = QuestionDot FieldAccess {target, name}} =
  maybeParens target <> "?." <> ppName name
ppExpr Optional {optTag} = error $ "PrettyPrinter.hs: don't know how to " ++
                                   "print expression '" ++ (show $ ppPath optTag) ++ "'"
  where
    ppPath :: OptionalPathComponent -> Doc ann
    ppPath (QuestionBang e) = ppExpr e
    ppPath (QuestionDot e) = ppExpr e

ppExpr MethodCall {target, name, args, typeArguments} =
    maybeParens target <> "." <> ppName name <>
      withTypeArguments typeArguments <>
      parens (commaSep (map ppExpr args))
ppExpr MessageSend {target, name, args, typeArguments} =
    maybeParens target <> "!" <> ppName name <>
      withTypeArguments typeArguments <>
      parens (commaSep (map ppExpr args))
ppExpr PartySeq {par, seqfunc} = ppExpr par <+> ">>" <+> parens (ppExpr seqfunc)
ppExpr PartyPar {parl, parr} = ppExpr parl <+> "|||" <+> ppExpr parr
ppExpr PartyReduce {seqfun, pinit, par} = "reduce" <>
    parens (commaSep $ ppExpr <$> [seqfun, pinit, par])
ppExpr ExtractorPattern {name, arg = arg@Skip{}} =
    ppName name <> ppExpr arg
ppExpr ExtractorPattern {name, arg = arg@Tuple{}} =
    ppName name <> ppExpr arg
ppExpr ExtractorPattern {name, arg} =
    ppName name <> parens (ppExpr arg)
ppExpr FunctionCall {qname, args, typeArguments} =
    ppQName qname <>
      withTypeArguments typeArguments <>
      parens (commaSep (map ppExpr args))
ppExpr FunctionAsValue {qname, typeArgs} =
  ppQName qname <> brackets (commaSep (map ppType typeArgs))
ppExpr Closure {eparams, mty, body=b@(Seq {})} =
    "fun" <+> parens (commaSep (map ppParamDecl eparams)) <+> returnType mty $+$
       indent (ppBody b) $+$
    "end"
  where
    returnType Nothing = ""
    returnType (Just t) = ":" <+> ppType t
ppExpr Closure {eparams, body} =
    "fun" <+> parens (commaSep (map ppParamDecl eparams)) <+> "=>" <+> ppExpr body
ppExpr Async {body=body@(Seq {})} =
  "async" $+$
    indent (ppBody body) $+$
  "end"
ppExpr Async {body} = "async" <> parens (ppExpr body)
ppExpr (MaybeValue _ (JustData a)) = "Just" <> parens (ppExpr a)
ppExpr (MaybeValue _ NothingData) = "Nothing"
ppExpr Tuple {args} = parens (commaSep (map ppExpr args))
ppExpr Let {decls, body} =
  "let" $+$
      indent (vcat (map ppDecl decls)) $+$
  "in" $+$
      indent (ppBody body) $+$
  "end"
ppExpr MiniLet {mutability, decl} =
    ppMut mutability <+> ppDecl decl
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
ppExpr DoWhile {cond, body} =
    "do" $+$
         indent (ppBody body) $+$
    "while" <> parens (ppExpr cond)
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
        "case" <+> ppExpr mcpattern <+> "=>" $+$
          indent (ppBody mchandler) $+$
        "end"
      ppClause (MatchClause {mcpattern, mchandler, mcguard}) =
        "case" <+> ppExpr mcpattern <+> "when" <+> ppExpr mcguard <+> "=>" $+$
          indent (ppBody mchandler) $+$
        "end"
      ppMatchClauses = foldr (($+$) . indent . ppClause) ""
ppExpr Borrow {target, name, body} =
    "borrow" <+> ppExpr target <+> "as" <+> ppName name <+> "in" $+$
      indent (ppBody body) $+$
    "end"
ppExpr FutureChain {future, chain} =
    ppExpr future <+> "~~>" <+> ppExpr chain
ppExpr Get {val} = "get" <> parens (ppExpr val)
ppExpr Yield {val} = "yield" <> parens (ppExpr val)
ppExpr Forward {forwardExpr} = "forward" <> parens (ppExpr forwardExpr)
ppExpr Eos {} = "eos"
ppExpr Await {val} = "await" <> parens (ppExpr val)
ppExpr IsEos {target} = "eos" <> parens (ppExpr target)
ppExpr StreamNext {target} = "getNext" <> parens (ppExpr target)
ppExpr Return {val} = "return" <> parens (ppExpr val)
ppExpr Suspend {} = "suspend"
ppExpr FieldAccess {target, name} =
  maybeParens target <> "." <> ppName name
ppExpr ArrayAccess {target = target@FieldAccess{}, index} =
  parens (ppExpr target) <> parens (ppExpr index)
ppExpr ArrayAccess {target, index} = ppExpr target <> parens (ppExpr index)
ppExpr ArraySize {target} = "|" <> ppExpr target <> "|"
ppExpr ArrayNew {ty, size} = "new" <+> brackets (ppType ty) <> parens (ppExpr size)
ppExpr ArrayLiteral {args} = brackets $ commaSep (map ppExpr args)
ppExpr VarAccess {qname} = ppQName qname
ppExpr TupleAccess {target, compartment} = ppExpr target <> "." <> pretty compartment
ppExpr Consume {target} = "consume" <+> ppExpr target
ppExpr Assign {lhs, rhs} = ppExpr lhs <+> "=" <+> ppExpr rhs
ppExpr Null {} = "null"
ppExpr BTrue {} = "true"
ppExpr BFalse {} = "false"
ppExpr NewWithInit {ty, args} =
  "new" <+> ppType ty <> parens (commaSep (map ppExpr args))
ppExpr New {ty} = "new" <+> ppType ty
ppExpr Print {args} = "print" <> parens (commaSep (map ppExpr args))
ppExpr Exit {args} = "exit" <> parens (commaSep (map ppExpr args))
ppExpr Abort {args} = "abort" <> parens (commaSep (map ppExpr args))
ppExpr StringLiteral {stringLit} = viaShow stringLit
ppExpr CharLiteral {charLit} = viaShow charLit
ppExpr UIntLiteral {intLit} = pretty intLit <> "u"
ppExpr IntLiteral {intLit} = pretty intLit
ppExpr RealLiteral {realLit} = pretty realLit
ppExpr RangeLiteral {start, stop, step} =
  "[" <> ppExpr start <> ".." <> ppExpr stop <> ppStep step <> "]"
  where
    ppStep IntLiteral {intLit = 1} = ""
    ppStep intstep = " by" <+> ppExpr intstep
ppExpr Embed {ty, embedded} =
  "EMBED" <+> parens (ppType ty) $+$
          indent (hcat (map (uncurry ppPair) embedded)) $+$
  "END"
  where
    ppPair code Skip{} = pretty code
    ppPair code expr = pretty code <> "#{" <> ppExpr expr <> "}"
ppExpr Unary {uop, operand} = ppUnary uop <> parens (ppExpr operand)
ppExpr Binop {binop, loper, roper} =
  ppExpr loper <+> ppBinop binop <+> ppExpr roper
ppExpr TypedExpr {body, ty} = ppExpr body <+> ":" <+> ppType ty

ppDecl :: ([VarDecl], Expr) -> Doc ann
ppDecl (vars, val) =
  commaSep (map ppVar vars) <+> "=" <+> ppExpr val
ppVar :: VarDecl -> Doc ann
ppVar (VarType x ty) = ppName x <+> ":" <+> ppType ty
ppVar (VarNoType x) = ppName x


ppUnary :: UnaryOp -> Doc ann
ppUnary Identifiers.NOT = "not"
ppUnary Identifiers.NEG = "-"

ppBinop :: BinaryOp -> Doc ann
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
ppBinop Identifiers.PLUS_EQUALS  = "+="
ppBinop Identifiers.MINUS_EQUALS = "-="
ppBinop Identifiers.TIMES_EQUALS = "*="
ppBinop Identifiers.DIV_EQUALS   = "/="
