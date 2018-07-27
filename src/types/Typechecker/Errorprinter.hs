
module Typechecker.Errorprinter (printError) where


-- Library dependencies
import Text.PrettyPrint.Annotated
import Text.PrettyPrint.Annotated.HughesPJ (renderDecoratedM)
import System.Console.ANSI
import Text.Printf (printf)
import Data.Ix(range)
import Data.Map.Strict (keys)

-- Module dependencies
import AST.Meta(Position, getPositionFile, getPositions)
import Identifiers
import Types
import Typechecker.Environment
import Typechecker.TypeError
import Typechecker.Util
import Typechecker.Suggestable


currentPos (TCError _ Env{bt = ((pos, _):_)}) = pos

printError :: TCError -> IO ()
printError err@(TCError _ Env{bt = []}) =
    renderError $ prettyError err [] $+$ text ""
printError error = do
    code <- getCodeLines $ currentPos error
    renderError $ prettyError error code $+$ text ""


renderError :: Doc TCStyle -> IO ()
renderError doc =
    renderDecoratedM toErrorStyle endAnn textprinter endDoc doc
    where
        endAnn :: TCStyle -> IO ()
        endAnn _ = setSGR [Reset]

        textprinter :: String -> IO ()
        textprinter = printf

        endDoc :: IO ()
        endDoc = setSGR [Reset]

toErrorStyle :: TCStyle -> IO ()
toErrorStyle Classification = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
toErrorStyle Desc = setSGR [SetConsoleIntensity BoldIntensity]
toErrorStyle Logistic = setSGR [SetColor Foreground Vivid Blue]
toErrorStyle Highlight = setSGR [SetColor Foreground Dull Red]
toErrorStyle Code = return ()

toWarningStyle :: TCStyle -> IO ()
toWarningStyle Classification = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
toWarningStyle Desc = setSGR [SetConsoleIntensity BoldIntensity]
toWarningStyle Logistic = setSGR [SetColor Foreground Vivid Blue]
toWarningStyle Highlight = setSGR [SetColor Foreground Dull Yellow]
toWarningStyle Code = return ()



-- As long as there is no way to either:
--      - Get the source code from all compiled files previous into Env
--      - Make prettyprinter.hs have the ability to include whitespace and parentheses
-- prettyError will need all lines of code it will print beforehand in its second argument

prettyError ::  TCError -> [String] -> Doc TCStyle
-- Do not show entire class if an unknown trait is declared
prettyError tcErr@(TCError err@(UnknownRefTypeError ty) _) _
    | isTraitType ty = declareError err <+> description err $+$ nest 2 (showPosition $ currentPos tcErr)

-- Default errors
prettyError (TCError err Env{bt = []}) _ =
    declareError err <+> description err
prettyError tcErr@(TCError err _) code =
    declareError err <+> description err $+$ codeViewer tcErr code
-- Possible extensions:
--  Duplicate Class -> print positions (File + line) of the two classes
--  Type error in func call -> print a version of codeViewer that also shows the function head

pipe = char '|'

declareError :: Error -> Doc TCStyle
declareError _ = styleClassify $ text "Error:"

description :: Error -> Doc TCStyle
description err = styleDesc $ text $ show err


showPosition :: Position -> Doc TCStyle
showPosition pos = styleLogistic (text "-->") <+> (text $ show $ pos)


codeViewer :: TCError -> [String] -> Doc TCStyle
codeViewer _ [] = error "TypeError.hs: No code to view"
codeViewer err (cHead:cTail) =
    let
        pos = currentPos err
        ((sL, sC), (eL, eC)) = getPositions pos
        digitLen = 1 + (length $ show eL) --One additional for the space between line-number and pipe
        tailCode = zipWith (codeLine " |") cTail (range (sL+1, eL))
    in
        if sL == eL
            then
                nest (digitLen) $ showPosition pos $+$
                styleLogistic pipe $+$
                codeLine "" cHead sL $+$
                styleLogistic pipe <>
                styleHighlight (lineHighlighter sC eC '^') <+>
                styleHighlight (smallSuggest err) $+$
                longSuggest err
            else
                nest (digitLen) $ showPosition pos $+$
                styleLogistic pipe $+$
                codeLine "  " cHead sL $+$
                styleLogistic pipe <>
                styleHighlight (multilineHighlighter sC FirstLine '^') $+$
                vcat tailCode $+$
                styleLogistic pipe <>
                styleHighlight (multilineHighlighter eC LastLine '^') <+>
                styleHighlight (smallSuggest err) $+$
                longSuggest err


lineHighlighter :: Int -> Int -> Char -> Doc ann
lineHighlighter s e c = text $ replicate (s-1) ' ' ++ replicate (e-s) c

data MultiLineType = FirstLine | LastLine
multilineHighlighter :: Int -> MultiLineType -> Char -> Doc ann
multilineHighlighter col FirstLine c  = space <> space <> text (replicate (col-1) '_') <> char c
multilineHighlighter col LastLine  c  = space <> pipe  <> text (replicate (col-2) '_') <> char c

codeLine ::String -> String -> Int -> Doc TCStyle
codeLine insertStr code lineNo =
    let
        pad = (length $ show lineNo) + 1 --One additional for the space between line-number and pipe
    in
        nest (-pad) $
        styleLogistic ((int lineNo) <+> pipe) <>
        styleHighlight (text insertStr) <>
        styleCode (text code)

getCodeLines :: Position -> IO [String]
getCodeLines pos = do
    let ((sL, _), (eL, _)) = getPositions pos
    let start = sL-1
    let end = eL-start
    contents <- readFile $ getPositionFile pos
    case take end $ drop start $ lines contents of
        []  -> error "\nFile has been edited between parsing and type checking"
        l   -> return l
