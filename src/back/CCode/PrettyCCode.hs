{-# LANGUAGE GADTs #-}

module CCode.PrettyCCode (pp) where

import CCode.Main
import Text.PrettyPrint
import Data.List

indent = nest 2

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
pp' (Program c) = pp' c
pp' (Includes ls) = vcat $ map (text . ("#include <"++) . (++">")) ls
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Statement c) = pp' c <> text ";"
pp' (Switch tst ccodes def) = text "switch (" <+> (tshow tst) <+> rparen  $+$
                                  switch_body ccodes def
pp' (StructDecl name vardecls) = text "struct ___" <> tshow name $+$
                      (braced_block . vcat . map pp') fields
    where fields = map (\ (ty, id) -> Embed $ show ty ++ " " ++ show id ++ ";") vardecls
pp' (Record ccodes) = text "{" <+> (foldr1 (<>) $ intersperse (text ", ") $ map pp' ccodes) <+> text "}"
pp' (Assign lhs rhs) = pp' lhs <+> text "=" <+> pp' rhs
pp' (Decl (ty, id)) = tshow ty <+> tshow id
pp' (Concat ccodes) = block ccodes
pp' (ConcatTL ccodes) = vcat $ intersperse (text "\n") $ map pp' ccodes
pp' (StoopidSeq ccodes) = vcat $ map ((<> text ";") . pp') ccodes
pp' (Enum ids) = text "enum" $+$ braced_block (vcat $ map (\id -> tshow id <> text ",") ids) <> text ";"
pp' (Braced ccode) = (braced_block . pp') ccode
pp' (BinOp o e1 e2) = lparen <> pp' e1 <+> pp' o <+> pp' e2 <> rparen
pp' (Dot ccode id) = pp' ccode <> text "." <> tshow id
pp' (Deref ccode) = lparen <> text "*" <> pp' ccode <> rparen
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

