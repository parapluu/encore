module PrettyCCode (pp) where

import CCode
import Text.PrettyPrint
import Data.List

indent = nest 2

pp :: CCode -> String
pp = show . pp'

tshow :: Show t => t -> Doc
tshow = text . show

pp' :: CCode -> Doc
pp' (Includes l) = vcat $ map (text . ("#include <"++) . (++">")) l
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Switch tst ccodes) = text "switch (" <+> (text tst) <+> text ")" $+$
                         braced_block ccodes
pp' (StructDecl name vardecls) = text "struct" <+> text name $+$
                      braced_block fields <> text ";"
    where fields = map (\ (CVarDecl (ty, id)) -> Embed $ show ty ++ " " ++ id ++ ";") vardecls
pp' (Record ccodes) = text "{" <+> (foldr1 (<>) $ intersperse (text ", ") $ map pp' ccodes) <+> text "}"
pp' (Static ccode) = text "static" <+> pp' ccode
pp' (Assign lhs rhs) = pp' lhs <+> text "=" <+> pp' rhs
pp' (C ccodes) = block ccodes
pp' (TypeDef name ccode) = text ("typedef " ++ name) <+> pp' ccode
pp' (SEMI c) = pp' c <> text ";"
pp' (Embed string) = text string
pp' (Function ret_ty name args body) =  tshow ret_ty <+> text name <>
                    text "(" <> pp_args args <> text ")" $+$
                    braced_block body
  where pp_args [] = empty
        pp_args as = foldr1 (<>) $
                     intersperse (text ", ") $
                     map pp_arg as
        pp_arg = \ (CVarDecl (ty, id)) -> tshow ty <+> text id

block :: [CCode] -> Doc
block = vcat . map pp'

braced_block :: [CCode] -> Doc
braced_block ccodes = lbrace $+$
                      indent (block ccodes) $+$
                      rbrace

testfun = Function (embedCType "int") "main"
                    [CVarDecl (embedCType "int","argc"), CVarDecl (embedCType "char**", "argv")]
                    [Embed "printf(\"asdf\");"]

instance Show CCode where
  show = pp

main = do print testfun
