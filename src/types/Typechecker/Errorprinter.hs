
module Typechecker.Errorprinter (printError) where


import Typechecker.Environment
import Typechecker.TypeError
import AST.Meta(Position, getPositionFile, getPositions)
import Data.Ix(range)
import Text.PrettyPrint.Annotated.HughesPJ
import Text.Printf
import System.Console.ANSI



printError :: TCError -> IO ()
printError err@(TCError _ Env{bt = []}) =
    renderError $ prettyError err [] $+$ text ""
    -- putDoc $ reAnnotate toErrorStyle $ prettyError err [] $+$ text ""
printError err@(TCError _ Env{bt = ((pos, _):_)}) = do
    code <- getCodeLines pos
    renderError $ prettyError err code $+$ text ""
    -- putDoc $ reAnnotate toErrorStyle $ prettyError err code $+$ text ""

-- renderDecoratedM :: Monad m => (ann -> m r) -> (ann -> m r) -> (String -> m r) -> m r -> Doc ann -> m r
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

toWarningStyle :: TCStyle -> IO ()
toWarningStyle Classification = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
toWarningStyle Desc = setSGR [SetConsoleIntensity BoldIntensity]
toWarningStyle Logistic = setSGR [SetColor Foreground Vivid Blue]
toWarningStyle Highlight = setSGR [SetColor Foreground Dull Yellow]



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

pipe = char '|'

declareError :: Error -> Doc TCStyle
declareError _ = classify $ text "Error:"

description :: Error -> Doc TCStyle
description err = desc $ text $ show err

codeLine :: String -> String -> Int -> Doc TCStyle
codeLine insertStr codeLine lineNo =
    logistic ((int lineNo) <+> pipe) <>
    highlight (text insertStr) <>
    text codeLine

showPosition :: Position -> Doc TCStyle
showPosition pos = logistic (text "-->") <+> (text $ show $ pos)

lineHighlighter :: Int -> Int -> Char -> Doc ann
lineHighlighter s e c = text $ replicate (s-1) ' ' ++ replicate (e-s) c

multilineHighlighter :: Int -> Bool -> Char -> Doc ann
multilineHighlighter col True c  = space <> space <> text (replicate (col-1) '_') <> char c
multilineHighlighter col False c = space <> pipe  <> text (replicate (col-2) '_') <> char c

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
                nest (digitLen+1) $ showPosition pos $+$
                logistic pipe $+$
                nest (-(digitLen+1)) (codeLine "" cHead sL) $+$
                logistic pipe <>
                highlight (lineHighlighter sC eC '^') <+>
                smallSuggest err $+$
                longSuggest err
            else
                nest (digitLen+1) $ showPosition pos $+$
                logistic pipe $+$
                nest (-(digitLen+1)) (codeLine "  " cHead sL) $+$
                logistic pipe <>
                highlight (multilineHighlighter sC True '^') $+$
                nest (-(digitLen+1)) (vcat tailCode) $+$
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
