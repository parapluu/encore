module PrettyCCode (pp) where

import CCode
import Text.PrettyPrint
import Data.List

indent = nest 2

pp :: CCode -> String
pp = show . pp'

tshow :: Show t => t -> Doc
tshow = text . show

switch_body :: [(CCode,CCode)] -> CCode -> Doc
switch_body ccodes def = lbrace $+$ (nest 2 $ vcat (map switch_clause ccodes) $+$
                         text "default:" $+$ (braced_block . vcat . map pp') [def]) $+$ rbrace
  where
    switch_clause :: (CCode,CCode) -> Doc
    switch_clause (lhs,rhs) =
      text "case" <+>
      pp' lhs <> text ":" $+$ (braced_block . vcat . map pp') (rhs:[Embed "break;"])

pp' :: CCode -> Doc
pp' (Includes l) = vcat $ map (text . ("#include <"++) . (++">")) l
pp' (Decl (CVarSpec (ty, id))) = text (show ty) <+> text id
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Switch tst ccodes def) = text "switch (" <+> (text tst) <+> rparen  $+$
                                  switch_body ccodes def
pp' (StructDecl name vardecls) = text "struct" <+> text name $+$
                      (braced_block . vcat . map pp') fields
    where fields = map (\ (CVarSpec (ty, id)) -> Embed $ show ty ++ " " ++ id ++ ";") vardecls
pp' (Record ccodes) = text "{" <+> (foldr1 (<>) $ intersperse (text ", ") $ map pp' ccodes) <+> text "}"
pp' (Assign lhs rhs) = pp' lhs <+> text "=" <+> pp' rhs
pp' (C ccodes) = block ccodes
pp' (Call name args) = text name <> lparen <>
                       (hcat $ intersperse (text ", ") $ map pp' args) <>
                       rparen
pp' (TypeDef name ccode) = text ("typedef") <+> pp' ccode <+> text name
pp' (Deref ccode) = lparen <> text "*" <> pp' ccode <> rparen
pp' (Dot ccode id) = pp' ccode <> text "." <> text id
pp' (Var st) = text st
pp' (Statement c) = pp' c <> text ";"
pp' (Embed string) = text string
pp' (Function ret_ty name args body) =  tshow ret_ty <+> text name <>
                    lparen <> pp_args args <> rparen $+$
                    (braced_block . vcat . map pp') body
pp' (FwdDecl (Function ret_ty name args _)) = tshow ret_ty <+> text name <> lparen <> pp_args args <> rparen <> text ";"
pp' (BracedBlock ccode) = (braced_block . pp') ccode
pp' (Enum ids) = text "enum" $+$ braced_block (vcat $ map (\id -> text id <> text ",") ids)
--pp' (New ty) = error "not implemented: New"


pp_args [] = empty
pp_args as = hcat $ intersperse (text ", ") $ map pp_arg as
pp_arg = \(CVarSpec (ty, id)) -> tshow ty <+> text id

block :: [CCode] -> Doc
block = vcat . map pp'

braced_block :: Doc -> Doc
braced_block doc = lbrace $+$
                      indent doc $+$
                      rbrace

testfun = Function (embedCType "int") "main"
                    [CVarSpec (embedCType "int","argc"), CVarSpec (embedCType "char**", "argv")]
                    [Embed "printf(\"asdf\");"]

instance Show CCode where
  show = pp

main = do print testfun
