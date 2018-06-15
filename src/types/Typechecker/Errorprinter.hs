
module Typechecker.Errorprinter (printError) where


import Typechecker.Environment
import Typechecker.TypeError
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import AST.Meta(Position, getPositionFile, getPositions)
import Data.Ix(range)

($+$) s e = s <> line <> e


-- Possible Ansi render settings
--
-- Color commands:  color, colorDull
-- Colors:          Black, Red, Green, Yellow, Blue, Magenta, Cyan, White
-- Font Styles:     bold, italicized, underlined
toErrorStyle :: TCStyle -> AnsiStyle
toErrorStyle Classification = color Red <> bold
toErrorStyle Desc = bold
toErrorStyle Logistic = color Blue
toErrorStyle Highlight = colorDull Red

toWarningStyle :: TCStyle -> AnsiStyle
toWarningStyle Classification = color Yellow <> bold
toWarningStyle Desc = bold
toWarningStyle Logistic = color Blue
toWarningStyle Highlight = colorDull Yellow


printError :: TCError -> IO ()
printError err@(TCError _ Env{bt = []}) =
    putDoc $ reAnnotate toErrorStyle $ prettyError err [] <> line
printError err@(TCError _ Env{bt = ((pos, _):_)}) = do
    code <- getCodeLines pos
    putDoc $ reAnnotate toErrorStyle $ prettyError err code <> line


-- As long as there is no way to either:
--      - Get the source code from all compiled files previous into Env
--      - Make prettyprinter.hs have the ability to include whitespace and parentheses
-- prettyError will need all lines of code it will print beforehand in its second argument

prettyError ::  TCError -> [String] -> Doc TCStyle
prettyError (TCError err@(TypeWithCapabilityMismatchError _ _ _) Env{bt = bt@((pos, _):_)}) code =
    declareError err <+> description err $+$ codeViewer pos code err
prettyError (TCError err Env{bt = []}) _ =
    declareError err <+> description err
prettyError (TCError err Env{bt = bt@((pos, _):_)}) code =
    declareError err <+> description err $+$ codeViewer pos code err
-- Possible extensions:
--  Duplicate Class -> print positions (File + line) of the two classes
--  Type error in func call -> print a version of codeViewer that also shows the function head


declareError :: Error -> Doc TCStyle
declareError _ = classify $ pretty "Error:"

description :: Error -> Doc TCStyle
description err = desc $ viaShow err

codeLine :: String -> String -> Int -> Doc TCStyle
codeLine insertStr codeLine lineNo =
    logistic ((pretty lineNo) <+> pipe) <>
    highlight (pretty insertStr) <>
    pretty codeLine

showPosition :: Position -> Doc TCStyle
showPosition pos = logistic (pretty "-->") <+> viaShow pos

lineHighlighter :: Int -> Int -> Char -> Doc ann
lineHighlighter s e c = indent (s-1) $ pretty $ replicate (e-s) c

multilineHighlighter :: Int -> Bool -> Char -> Doc ann
multilineHighlighter col True c  = indent 2 (pretty (replicate (col-1) '_') <> pretty c)
multilineHighlighter col False c = indent 1 pipe <> (pretty (replicate (col-2) '_') <> pretty c)

codeViewer :: Position -> [String] -> Error -> Doc TCStyle
codeViewer _ [] _ = error "TypeError.hs: No code to view"
codeViewer pos (cHead:cTail) err =
    let
        ((sL, sC), (eL, eC)) = getPositions pos
        digitLen = length $ show sL
        tailCode = zipWith (codeLine " |") cTail (range (sL+1, eL))
    in
        if sL == eL
            then
                nest (digitLen+1) $ indent digitLen (showPosition pos) $+$
                logistic pipe <>
                nest (-(digitLen+1)) (line <> codeLine "" cHead sL) $+$
                logistic pipe <>
                highlight (lineHighlighter sC eC '^') <+>
                smallSuggest err $+$
                longSuggest err
            else
                nest (digitLen+1) $ indent digitLen (showPosition pos) $+$
                logistic pipe <>
                nest (-(digitLen+1)) (line <> codeLine "  " cHead sL) $+$
                logistic pipe <>
                highlight (multilineHighlighter sC True '^') <>
                nest (-(digitLen+1)) (line <> vsep tailCode) $+$
                logistic pipe <>
                highlight (multilineHighlighter eC False '^') <+>
                smallSuggest err $+$
                longSuggest err
                

getCodeLines :: Position -> IO [String]
getCodeLines pos = do
    let ((sL, _), (eL, _)) = getPositions pos
    let start = sL-1
    let end = eL-start
    contents <- readFile $ getPositionFile pos
    case take end $ drop start $ lines contents of
        []  -> error "\nFile has been edited between parsing and type checking"
        l   -> return l
