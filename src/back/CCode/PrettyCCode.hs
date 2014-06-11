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
pp' Skip = text ""
pp' (Includes ls) = vcat $ map (text . ("#include <"++) . (++">")) ls
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Statement seq@(StoopidSeq _)) = pp' seq -- avoid double semicolons!
pp' (Statement other) =  pp' other <> text ";"
pp' (Switch tst ccodes def) = text "switch (" <+> (tshow tst) <+> rparen  $+$
                                  switch_body ccodes def
pp' (StructDecl name vardecls) = text "struct ___" <> tshow name $+$
                      (braced_block . vcat . map pp') fields <> text ";"
    where fields = map (\ (ty, id) -> Embed $ show ty ++ " " ++ show id ++ ";") vardecls
pp' (Record ccodes) = text "{" <+> (hcat $ intersperse (text ", ") $ map pp' ccodes) <+> text "}"
pp' (Assign lhs rhs) = pp' lhs <+> text "=" <+> pp' rhs <> text ";"
pp' (Decl (ty, id)) = tshow ty <+> tshow id
pp' (FunTypeDef id ty argTys) = text "typedef" <+> tshow ty <+> parens (text "*" <> tshow id) <> parens (hcat $ intersperse (text ", ") $ map pp' argTys) <+> text ";"
pp' (Concat ccodes) = block ccodes
pp' (ConcatTL ccodes) = vcat $ intersperse (text "\n") $ map pp' ccodes
pp' (StoopidSeq ccodes) = vcat $ map semicolonify ccodes
    where
      semicolonify :: CCode Expr -> Doc
      semicolonify (StoopidSeq ccodes) = vcat $ map semicolonify ccodes -- avoid double semicolons!
      semicolonify other = pp' other <> text ";"
pp' (Seq ccodes) = vcat $ map ((<> text ";") . pp') ccodes
pp' (Enum ids) = text "enum" $+$ braced_block (vcat $ map (\id -> tshow id <> text ",") ids) <> text ";"
pp' (Braced ccode) = (braced_block . pp') ccode
pp' (Parens ccode) = lparen <> pp' ccode <> rparen
pp' (BinOp o e1 e2) = lparen <> pp' e1 <+> pp' o <+> pp' e2 <> rparen
pp' (Dot ccode id) = pp' ccode <> text "." <> tshow id
pp' (Deref ccode) = lparen <> text "*" <> pp' ccode <> rparen
pp' (ArrAcc i l) = lparen <> pp' l <> text "[" <> tshow i <> text "]" <> rparen
pp' (Amp ccode) = lparen <> text "&" <> pp' ccode <> rparen
pp' (Ptr ty) = pp' ty <> text "*"
pp' (Function ret_ty name args body) =  tshow ret_ty <+> tshow name <>
                    lparen <> pp_args args <> rparen $+$
                    (braced_block . pp') body
pp' (AsExpr c) = pp' c
pp' (AsLval c) = pp' c
pp' (Nam st) = text st
pp' (Var st) = text st
pp' (Typ st) = text st
pp' (Static ty) = text "static" <+> pp' ty
pp' (Embed string) = text string
pp' (EmbedC ccode) = pp' ccode
pp' (Call name args) = tshow name <> lparen <>
                       (hcat $ intersperse (text ", ") $ map pp' args) <>
                       rparen
pp' (TypeDef name ccode) = text ("typedef") <+> pp' ccode <+> tshow name <> text ";"
pp' (While cond body) = text "while" <+> lparen <> pp' cond <> rparen $+$
                        braced_block (pp' body)
pp' (StatAsExpr n s) = text "({" <> pp' s <+> pp' n <> text ";})"
pp' (If c t e) = text "if" <+> lparen <> pp' c <> rparen $+$
                 braced_block (pp' t) $+$
                 text "else" $+$
                 braced_block (pp' e)
pp' (Return e) = text "return" <+> pp' e
--pp' (FwdDecl (Function ret_ty name args _)) = tshow ret_ty <+> tshow name <> lparen <> pp_args args <> rparen <> text ";"
--pp' (New ty) = error "not implemented: New"


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

