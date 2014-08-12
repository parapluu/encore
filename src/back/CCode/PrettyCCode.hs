{-# LANGUAGE GADTs #-}

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

star :: Doc
star = text "*"

switch_body :: [(CCode Name, CCode Stat)] -> CCode Stat -> Doc
switch_body ccodes def_case = lbrace $+$ (nest 2 $ vcat (map switch_clause ccodes) $+$
                                          text "default:" $+$
                                                   (braced_block . vcat . map pp') [def_case]) $+$
                              rbrace
  where
    switch_clause :: (CCode Name, CCode Stat) -> Doc
    switch_clause (lhs,rhs) =
      text "case" <+> pp' lhs <> text ":"
               $+$ (braced_block . vcat . map pp') (rhs:[Embed "break;"])

pp' :: CCode a -> Doc
pp' (Program cs) = pp' cs
pp' Skip = empty
pp' Null = text "NULL"
pp' (Includes ls) = vcat $ map (text . ("#include <"++) . (++">")) ls
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Statement other) =  pp' other <> text ";"
pp' (Switch tst ccodes def) = text "switch" <+> parens (tshow tst)  $+$
                              switch_body ccodes def
pp' (StructDecl name vardecls) = text "struct ___" <> tshow name $+$
                                 (braced_block . vcat) (map pp' fields) <> text ";"
    where fields = map (\ (ty, id) -> Embed $ show ty ++ " " ++ show id ++ ";") vardecls
pp' (Struct name) = text "struct ___" <> tshow name
pp' (Record ccodes) = braces $ commaList ccodes
pp' (Assign lhs rhs) = pp' lhs <+> text "=" <+> pp' rhs <> text ";"
pp' (AssignTL lhs rhs) = pp' lhs <+> text "=" <+> pp' rhs <> text ";"
pp' (Decl (ty, id)) = tshow ty <+> tshow id
pp' (DeclTL (ty, id)) = tshow ty <+> tshow id <> text ";"
pp' (FunTypeDef id ty argTys) = text "typedef" <+> tshow ty <+> parens (star <> tshow id) <> 
                                parens (commaList argTys) <+> text ";"
pp' (Concat ccodes) = vcat $ intersperse (text "\n") $ map pp' ccodes
pp' (Seq ccodes) = vcat $ map ((<> text ";") . pp') ccodes
pp' (Enum ids) = text "enum" $+$ braced_block (vcat $ map (\id -> tshow id <> text ",") ids) <> text ";"
pp' (Braced ccode) = (braced_block . pp') ccode
pp' (Parens ccode) = parens $ pp' ccode
pp' (CUnary o e) = parens $  pp' o <+> pp' e
pp' (BinOp o e1 e2) = parens $  pp' e1 <+> pp' o <+> pp' e2
pp' (Dot ccode id) = pp' ccode <> text "." <> tshow id
pp' (Deref ccode) = parens $ star <> pp' ccode 
pp' (Cast ty e) = parens $ (parens $ pp' ty) <+> pp' e
pp' (ArrAcc i l) = parens $  pp' l <> brackets (tshow i)
pp' (Amp ccode) = parens $ text "&" <> (parens $ pp' ccode)
pp' (Ptr ty) = pp' ty <> star
pp' (FunctionDecl ret_ty name args) = tshow ret_ty <+> tshow name <>
                                      parens (commaList args) $+$ text ";"
pp' (Function ret_ty name args body) = tshow ret_ty <+> tshow name <>
                                       parens (pp_args args)  $+$
                                       (braced_block . pp') body
pp' (AsExpr c) = pp' c
pp' (AsLval c) = pp' c
pp' (Nam st) = text st
pp' (Var st) = text st
pp' (Typ st) = text st
pp' (Static ty) = text "static" <+> pp' ty
pp' (Embed string) = text string
pp' (EmbedC ccode) = pp' ccode
pp' (Call name args) = tshow name <> parens (commaList args)
pp' (Typedef ty name) = text "typedef" <+> pp' ty <+> tshow name <> text ";"
pp' (Sizeof ty) = text "sizeof" <> parens (pp' ty)
pp' (While cond body) = text "while" <+> parens (pp' cond) $+$
                        braced_block (pp' body)
pp' (StatAsExpr n s) = text "({" <> pp' s <+> pp' n <> text ";})"
pp' (If c t e) = text "if" <+> parens  (pp' c) $+$
                   braced_block (pp' t) $+$
                 text "else" $+$
                   braced_block (pp' e)
pp' (Return e) = text "return" <+> pp' e <> text ";"
pp' (UnionInst name e) = text "{." <> tshow name <+> text "=" <+> pp' e <> text "}"
pp' (Int n) = tshow n
pp' (String s) = tshow s
pp' (Double d) = tshow d

commaList :: [CCode a] -> Doc
commaList l = hcat $ intersperse (text ", ") $ map pp' l

pp_args :: [CVarSpec] -> Doc
pp_args [] = empty
pp_args as = hcat $ intersperse (text ", ") $ map pp_arg as
pp_arg = \(ty, id) -> tshow ty <+> tshow id

block :: [CCode a] -> Doc
block = vcat . map pp'

braced_block :: Doc -> Doc
braced_block doc = lbrace $+$
                      indent doc $+$
                      rbrace

instance Show (CCode a) where
  show = pp

