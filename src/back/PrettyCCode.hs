module PrettyCCode (pp) where

import CCode
import Text.PrettyPrint
import Data.List

indent = nest 2

pp :: CCode -> String
pp = show . pp'

pp' :: CCode -> Doc
pp' (Includes l) = foldr1 ($+$) $ map (text . ("#include "++)) l
pp' (HashDefine str) = text $ "#define " ++ str
pp' (Switch tst ccodes) = text "switch (" <+> (text tst) <+> text ")" $+$
                         braced_block ccodes
pp' (Record ccodes) = undefined
pp' (C ccodes) = block ccodes
pp' (TypeDef name ccode) = text ("typedef " ++ name) <+> pp' ccode
pp' (SEMI) = text ";"
pp' (Embed string) = text string
pp' (Function fr) =  text ret_ty <+> text name <>
                    text "(" <> pp_args args <> text ")" $+$
                    braced_block body
  where pp_args [] = empty
        pp_args as = foldr1 (<>) $
                     intersperse (text ", ") $
                     map pp_arg as
        pp_arg = \(id, ty) -> text id <+> text ty
        ret_ty = fun_ret fr
        name = fun_name fr
        args = fun_args fr
        body = fun_body fr

block :: [CCode] -> Doc
block = foldr1 ($+$) . map pp'

braced_block :: [CCode] -> Doc
braced_block ccodes = lbrace $+$
                      indent (block ccodes) $+$
                      rbrace

testfun = Function (Funrec "int" "main"
                    [("int","argc"), ("char**", "argv")]
                    [Embed "printf(\"asdf\");"])

main = do print $ pp' testfun
