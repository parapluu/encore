{-# LANGUAGE GADTs,FlexibleContexts #-}
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
addSemi d = if show d == "" then d
             else if isSuffixOf ";" $ show d then d else d <> text ";"

star :: Doc
star = text "*"

switchBody :: [(CCode Name, CCode Stat)] -> CCode Stat -> Doc
switchBody ccodes defCase =
  lbrace $+$ (nest 2 $ vcat (map switchClause ccodes) $+$
      text "default:" $+$
      (bracedBlock . vcat . map pp') [defCase]) $+$
  rbrace
  where
    switchClause :: (CCode Name, CCode Stat) -> Doc
    switchClause (lhs,rhs) =
      text "case" <+> pp' lhs <> text ":"
               $+$ (bracedBlock . vcat . map pp') (rhs:[Embed "break;"])

pp' :: CCode a -> Doc
pp' (Program cs) = pp' cs
pp' Skip = empty
pp' Null = text "NULL"
pp' (Includes ls) = vcat $ map (text . ("#include <"++) . (++">")) ls
pp' (LocalInclude s) = text "#include" <+> doubleQuotes (text s)
pp' (IfDefine str ccode) = text "#ifdef" <+> text str $+$ pp' ccode $+$ text "#endif /* ifdef" <+> text str <+> text "*/"
pp' (IfNDefine str ccode) = text "#ifndef" <+> text str $+$ pp' ccode $+$ text "#endif /* ifndef" <+> text str <+> text "*/"
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Statement other) =  addSemi $ pp' other
pp' (Switch tst ccodes def) = text "switch" <+> parens (tshow tst)  $+$
                              switchBody ccodes def
pp' (StructDecl name vardecls) = text "struct " <> tshow name $+$
                                 (addSemi . bracedBlock . vcat) (map pp' fields)
    where fields = map (\ (ty, id) -> Embed $ show ty ++ " " ++ show id ++ ";") vardecls
pp' (Struct name) = text "struct " <> tshow name
pp' (Record ccodes) = braces $ commaList ccodes
pp' (Assign lhs rhs) = addSemi $ pp' lhs <+> text "=" <+> pp' rhs
pp' (AssignTL lhs rhs) = addSemi $ pp' lhs <+> text "=" <+> pp' rhs
pp' (Decl (ty, id)) = tshow ty <+> tshow id
pp' (DeclTL (ty, id)) = addSemi $ tshow ty <+> tshow id
pp' (FunTypeDef id ty argTys) = addSemi $ text "typedef" <+> tshow ty <+> parens (star <> tshow id) <>
                                parens (commaList argTys)
pp' (Concat ccodes) = vcat $ intersperse (text "\n") $ map pp' ccodes
pp' (Seq ccodes) = vcat $ map (addSemi . pp') ccodes
--    where
--      pp'' :: UsableAs Stat s => CCode s -> Doc
--      pp'' (Seq ccodes) = vcat $ map pp'' ccodes
--      pp'' other = pp' other
pp' (Enum ids) = text "enum" $+$ bracedBlock (vcat $ map (\id -> tshow id <> text ",") ids) <> text ";"
pp' (Braced ccode) = (bracedBlock . pp') ccode
pp' (Parens ccode) = parens $ pp' ccode
pp' (CUnary o e) = parens $  pp' o <+> pp' e
pp' (BinOp o e1 e2) = parens $  pp' e1 <+> pp' o <+> pp' e2
pp' (Dot ccode id) = pp' ccode <> text "." <> tshow id
pp' (Arrow ccode id) = pp' ccode <> text "->" <> tshow id
pp' (Deref ccode) = parens $ star <> pp' ccode
pp' (Cast ty e) = parens $ (parens $ pp' ty) <+> pp' e
pp' (ArrAcc i l) = parens $  pp' l <> brackets (tshow i)
pp' (Amp ccode) = parens $ text "&" <> (parens $ pp' ccode)
pp' (Ptr ty) = pp' ty <> star
pp' (FunctionDecl retTy name args) =
  tshow retTy <+> tshow name <>
  parens (commaList args) <> text ";"
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
pp' (Static ty) = text "static" <+> pp' ty
pp' (Extern ty) = text "extern" <+> pp' ty
pp' (Embed string) = text string
pp' (EmbedC ccode) = pp' ccode
pp' (Call name args) = tshow name <> parens (commaList args)
pp' (Typedef ty name) = text "typedef" <+> pp' ty <+> tshow name <> text ";"
pp' (Sizeof ty) = text "sizeof" <> parens (pp' ty)
pp' (While cond body) = text "while" <+> parens (pp' cond) $+$
                        bracedBlock (pp' body)
pp' (StatAsExpr n s) = text "({" <> pp' s <+> pp' n <> text ";})"
pp' (If c t e) = text "if" <+> parens  (pp' c) $+$
                   bracedBlock (pp' t) $+$
                 text "else" $+$
                   bracedBlock (pp' e)
pp' (Return e) = text "return" <+> pp' e <> text ";"
pp' (UnionInst name e) = text "{." <> tshow name <+> text "=" <+> pp' e <> text "}"
pp' (Int n) = tshow n
pp' (String s) = tshow s
pp' (Char c) = tshow c
pp' (Double d) = tshow d
pp' (Comm s) = text ("/* "++s++" */")
pp' (Annotated s ccode) = pp' ccode <+> pp' (Comm s)
pp' (FunPtrDecl t name argTypes) =
  let
    args = parens (commaList argTypes)
    id = text "(*" <> pp' name <> text ")"
  in
    pp' t <+> id <+> args
pp' (CompoundLiteral t pairs) =
  let
    struct = text "(" <> pp' t <> text ")"
    pairs' = [text "." <> pp' l <> text "=" <> pp' r | (l,r) <- pairs]
    body = hcat $ intersperse (text ", ") pairs'
    braced = text "{" <> body <> text "}"
  in
    text "&" <> struct <> braced
pp' (DesignatedInitializer pairs) =
  let
    pairs' = [text "." <> pp' l <> text "=" <> pp' r | (l,r) <- pairs]
    body = hcat $ intersperse (text ", ") pairs'
  in
    text "{" <> body <> text "}"

commaList :: [CCode a] -> Doc
commaList l = hcat $ intersperse (text ", ") $ map pp' l

ppArgs :: [CVarSpec] -> Doc
ppArgs [] = empty
ppArgs as = hcat $ intersperse (text ", ") $ map ppArg as
ppArg = \(ty, id) -> tshow ty <+> tshow id

block :: [CCode a] -> Doc
block = vcat . map pp'

bracedBlock :: Doc -> Doc
bracedBlock doc = lbrace $+$
                      indent doc $+$
                      rbrace

instance Show (CCode a) where
  show = pp
