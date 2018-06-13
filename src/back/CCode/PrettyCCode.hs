{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|
Converting CCode (see "CCode.Main") to C source.
-}

module CCode.PrettyCCode (pp) where

import CCode.Main
import Data.Text.Prettyprint.Doc hiding(indent)
import Data.List

indent = nest 2
($+$) s e = s <> line <> e

-- | Converts a CCode value to its source representation
pp :: CCode a -> String
pp = show . pp'

addSemi :: Doc ann -> Doc ann
addSemi d |  null (show d)
          || isSuffixOf ";" (show d) = d
          | otherwise = d <> ";"

commaSep :: [Doc ann] -> Doc ann
commaSep = hcat . intersperse ", "

switchBody :: [(CCode Name, CCode Stat)] -> CCode Stat -> Doc ann
switchBody ccodes defCase =
  bracedBlock $ vcat (map switchClause ccodes) $+$
                "default:" $+$ (bracedBlock . vcat . map pp') [defCase]
  where
    switchClause :: (CCode Name, CCode Stat) -> Doc ann
    switchClause (lhs,rhs) =
      "case" <+> pp' lhs <> ":"
             $+$ (bracedBlock . vcat . map pp') (rhs:[Embed "break;"])

pp' :: CCode a -> Doc ann
pp' (Program cs) = pp' cs
pp' Skip = emptyDoc
pp' Null = "NULL"
pp' (Includes ls) = vcat $ map (pretty . ("#include <"++) . (++">")) ls
pp' (LocalInclude s) = "#include" <+> dquotes (pretty s)
pp' (IfDefine str ccode) =
  "#ifdef" <+> pretty str $+$ pp' ccode $+$
  "#endif /* ifdef" <+> pretty str <+> "*/"
pp' (IfNDefine str ccode) =
  "#ifndef" <+> pretty str $+$ pp' ccode $+$
  "#endif /* ifndef" <+> pretty str <+> "*/"
pp' (HashDefine str) = "#define" <+> pretty str
pp' (Statement other) = addSemi $ pp' other
pp' (Switch tst ccodes def) =
  "switch" <+> parens (viaShow tst)  $+$
               switchBody ccodes def
pp' (StructDecl name vardecls) =
  "struct " <> viaShow name $+$ (addSemi . bracedBlock . vcat) (map pp' fields)
  where
    fields =
      map (\(ty, id) -> Embed $ show ty ++ " " ++ show id ++ ";") vardecls
pp' (Struct name) = "struct " <> viaShow name
pp' (Record ccodes) = braces $ commaList ccodes
pp' (Assign lhs rhs) = addSemi $ pp' lhs <+> "=" <+> pp' rhs
pp' (AssignTL lhs rhs) = addSemi $ pp' lhs <+> "=" <+> pp' rhs
pp' (Decl (ty, id)) = viaShow ty <+> viaShow id
pp' (DeclTL (ty, id)) = addSemi $ viaShow ty <+> viaShow id
pp' (FunTypeDef id ty argTys) =
  addSemi $ "typedef" <+> viaShow ty <+> parens ("*" <> viaShow id) <>
            parens (commaList argTys)
pp' (Concat ccodes) = vcat $ intersperse "\n" $ map pp' ccodes
pp' (Seq ccodes) = vcat $ map (addSemi . pp') ccodes
pp' (Enum ids) =
  "enum" $+$ bracedBlock (vcat $ map (\id -> viaShow id <> ",") ids) <> ";"
pp' (Braced ccode) = (bracedBlock . pp') ccode
pp' (Parens ccode) = parens $ pp' ccode
pp' (CUnary o e) = parens $  pp' o <+> pp' e
pp' (BinOp o e1 e2) = parens $  pp' e1 <+> pp' o <+> pp' e2
pp' (Dot ccode id) = pp' ccode <> "." <> viaShow id
pp' (Arrow ccode id) = pp' ccode <> "->" <> viaShow id
pp' (Deref ccode) = parens $ "*" <> pp' ccode
pp' (Cast ty e) = parens $ parens (pp' ty) <+> pp' e
pp' (ArrAcc i l) = parens $  pp' l <> brackets (viaShow i)
pp' (Amp ccode) = parens $ "&" <> parens (pp' ccode)
pp' (Ptr ty) = pp' ty <> "*"
pp' (FunctionDecl retTy name args) =
  viaShow retTy <+> viaShow name <>
  parens (commaList args) <> ";"
pp' (Function retTy name args body) =
  viaShow retTy <+> viaShow name <>
  parens (ppArgs args)  $+$
  (bracedBlock . pp') body
pp' (AsExpr c) = pp' c
pp' (AsLval c) = pp' c
pp' (AsType c) = pp' c
pp' (Nam st) = pretty st
pp' (Var st) = pretty st
pp' (Typ st) = pretty st
pp' (Static ty) = "static" <+> pp' ty
pp' (Extern ty) = "extern" <+> pp' ty
pp' (Embed string) = pretty string
pp' (EmbedC ccode) = pp' ccode
pp' (Call name args) = viaShow name <> parens (commaList args)
pp' (Typedef ty name) = "typedef" <+> pp' ty <+> viaShow name <> ";"
pp' (Sizeof ty) = "sizeof" <> parens (pp' ty)
pp' (While cond body) =
  "while" <+> parens (pp' cond) $+$
              bracedBlock (pp' body)
pp' (DoWhile cond body) = "do" <+> bracedBlock (pp' body) $+$ "while" <+> parens (pp' cond) 

pp' (StatAsExpr n s) = "({" <> pp' s <+> pp' n <> ";})"
pp' (If c t e) =
  "if" <+> parens  (pp' c) $+$
    bracedBlock (pp' t) $+$
  "else" $+$
    bracedBlock (pp' e)
pp' (Ternary c t e) = pp' c <> "?" <+> pp' t <> ":" <+> pp' e
pp' (Return e) = "return" <+> pp' e <> ";"
pp' (Break) = "break;"
pp' (Continue) = "continue;"
pp' (UnionInst name e) = "{." <> viaShow name <+> "=" <+> pp' e <> "}"
pp' (Int n) = viaShow n
pp' (String s) = viaShow s
pp' (Char c) = viaShow c
pp' (Double d) = viaShow d
pp' (Comm s) = pretty $ "/* " ++ s ++ " */"
pp' (Annotated s ccode) = pp' ccode <+> pp' (Comm s)
pp' (FunPtrDecl t name argTypes) =
  let
    args = parens (commaList argTypes)
    id = "(*" <> pp' name <> ")"
  in
    pp' t <+> id <+> args
pp' (CompoundLiteral t pairs) =
  let
    struct = "(" <> pp' t <> ")"
    pairs' = ["." <> pp' l <> "=" <> pp' r | (l,r) <- pairs]
    body = commaSep pairs'
    braced = "{" <> body <> "}"
  in
    "&" <> struct <> braced
pp' (DesignatedInitializer pairs) =
  let
    pairs' = ["." <> pp' l <> "=" <> pp' r | (l,r) <- pairs]
    body = commaSep pairs'
  in
    "{" <> body <> "}"

commaList :: [CCode a] -> Doc ann
commaList l = commaSep $ map pp' l

ppArgs :: [CVarSpec] -> Doc ann
ppArgs [] = emptyDoc
ppArgs as = commaSep $ map ppArg as
ppArg (ty, id) = viaShow ty <+> viaShow id

bracedBlock :: Doc ann -> Doc ann
bracedBlock doc = lbrace $+$
                      indent doc $+$
                      rbrace

instance Show (CCode a) where
  show = pp
