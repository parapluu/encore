{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|
Converting CCode (see "CCode.Main") to C source.
-}

module CCode.PrettyCCode (pp) where

import CCode.Main
import Text.PrettyPrint
import Data.List

indent = nest 2

-- | Converts a CCode value to its source representation
pp :: CCode a -> String
pp = show . pp'

tshow :: Show t => t -> Doc
tshow = text . show

addSemi :: Doc -> Doc
addSemi d |  null (show d)
          || isSuffixOf ";" (show d) = d
          | otherwise = d <> ";"

commaSep :: [Doc] -> Doc
commaSep = hcat . intersperse ", "

switchBody :: [(CCode Name, CCode Stat)] -> CCode Stat -> Doc
switchBody ccodes defCase =
  bracedBlock $ vcat (map switchClause ccodes) $+$
                "default:" $+$ (bracedBlock . vcat . map pp') [defCase]
  where
    switchClause :: (CCode Name, CCode Stat) -> Doc
    switchClause (lhs,rhs) =
      "case" <+> pp' lhs <> ":"
             $+$ (bracedBlock . vcat . map pp') (rhs:[Embed "break;"])

pp' :: CCode a -> Doc
pp' (Program cs) = pp' cs
pp' Skip = empty
pp' Null = "NULL"
pp' (Includes ls) = vcat $ map (text . ("#include <"++) . (++">")) ls
pp' (LocalInclude s) = "#include" <+> doubleQuotes (text s)
pp' (IfDefine str ccode) =
  "#ifdef" <+> text str $+$ pp' ccode $+$
  "#endif /* ifdef" <+> text str <+> "*/"
pp' (IfNDefine str ccode) =
  "#ifndef" <+> text str $+$ pp' ccode $+$
  "#endif /* ifndef" <+> text str <+> text "*/"
pp' (HashDefine str) = "#define" <+> text str
pp' (Statement other) = addSemi $ pp' other
pp' (Switch tst ccodes def) =
  "switch" <+> parens (tshow tst)  $+$
               switchBody ccodes def
pp' (StructDecl name vardecls) =
  "struct " <> tshow name $+$ (addSemi . bracedBlock . vcat) (map pp' fields)
  where
    fields =
      map (\(ty, id) -> Embed $ show ty ++ " " ++ show id ++ ";") vardecls
pp' (Struct name) = "struct " <> tshow name
pp' (Record ccodes) = braces $ commaList ccodes
pp' (Assign lhs rhs) = addSemi $ pp' lhs <+> "=" <+> pp' rhs
pp' (AssignTL lhs rhs) = addSemi $ pp' lhs <+> "=" <+> pp' rhs
pp' (Decl (ty, id)) = tshow ty <+> tshow id
pp' (DeclTL (ty, id)) = addSemi $ tshow ty <+> tshow id
pp' (FunTypeDef id ty argTys) =
  addSemi $ "typedef" <+> tshow ty <+> parens ("*" <> tshow id) <>
            parens (commaList argTys)
pp' (Concat ccodes) = vcat $ intersperse "\n" $ map pp' ccodes
pp' (Seq ccodes) = vcat $ map (addSemi . pp') ccodes
pp' (Enum ids) =
  "enum" $+$ bracedBlock (vcat $ map (\id -> tshow id <> ",") ids) <> ";"
pp' (Braced ccode) = (bracedBlock . pp') ccode
pp' (Parens ccode) = parens $ pp' ccode
pp' (CUnary o e) = parens $  pp' o <+> pp' e
pp' (BinOp o e1 e2) = parens $  pp' e1 <+> pp' o <+> pp' e2
pp' (Dot ccode id) = pp' ccode <> "." <> tshow id
pp' (Arrow ccode id) = pp' ccode <> "->" <> tshow id
pp' (Deref ccode) = parens $ "*" <> pp' ccode
pp' (Cast ty e) = parens $ parens (pp' ty) <+> pp' e
pp' (ArrAcc i l) = parens $  pp' l <> brackets (tshow i)
pp' (Amp ccode) = parens $ "&" <> parens (pp' ccode)
pp' (Ptr ty) = pp' ty <> "*"
pp' (FunctionDecl retTy name args) =
  tshow retTy <+> tshow name <>
  parens (commaList args) <> ";"
pp' (Function retTy name args body) =
  tshow retTy <+> tshow name <>
  parens (ppArgs args)  $+$
  (bracedBlock . pp') body
pp' (AsExpr c) = pp' c
pp' (AsLval c) = pp' c
pp' (AsType c) = pp' c
pp' (Nam st) = text st
pp' (Var st) = text st
pp' (Typ st) = text st
pp' (Static ty) = "static" <+> pp' ty
pp' (Extern ty) = "extern" <+> pp' ty
pp' (Embed string) = text string
pp' (EmbedC ccode) = pp' ccode
pp' (Call name args) = tshow name <> parens (commaList args)
pp' (Typedef ty name) = "typedef" <+> pp' ty <+> tshow name <> ";"
pp' (Sizeof ty) = "sizeof" <> parens (pp' ty)
pp' (While cond body) =
  "while" <+> parens (pp' cond) $+$
              bracedBlock (pp' body)
pp' (StatAsExpr n s) = "({" <> pp' s <+> pp' n <> ";})"
pp' (If c t e) =
  "if" <+> parens  (pp' c) $+$
    bracedBlock (pp' t) $+$
  "else" $+$
    bracedBlock (pp' e)
pp' (Ternary c t e) = pp' c <> "?" <+> pp' t <> ":" <+> pp' e
pp' (Return e) = "return" <+> pp' e <> ";"
pp' (UnionInst name e) = "{." <> tshow name <+> "=" <+> pp' e <> "}"
pp' (Int n) = tshow n
pp' (String s) = tshow s
pp' (Char c) = tshow c
pp' (Double d) = tshow d
pp' (Comm s) = text $ "/* " ++ s ++ " */"
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

commaList :: [CCode a] -> Doc
commaList l = commaSep $ map pp' l

ppArgs :: [CVarSpec] -> Doc
ppArgs [] = empty
ppArgs as = commaSep $ map ppArg as
ppArg (ty, id) = tshow ty <+> tshow id

bracedBlock :: Doc -> Doc
bracedBlock doc = lbrace $+$
                      indent doc $+$
                      rbrace

instance Show (CCode a) where
  show = pp
