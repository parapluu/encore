
module Typechecker.Errorprinter (printError, printWarning, noExplanation) where


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
import Typechecker.ExplainTable
import System.IO



printError :: TCError -> IO ()
printError err@(TCError _ Env{bt = []}) =
    renderTCType toErrorStyle $ prettyError err [] $+$ text "\n"
printError err@(TCError _ env) = do
    code <- getCodeLines $ currentBTPos err
    renderTCType toErrorStyle $ prettyError err code $+$ text "\n"


printWarning :: TCWarning -> IO ()
printWarning w@(TCWarning _ Env{bt = []}) =
    renderTCType toWarningStyle $ prettyWarning w [] $+$ text "\n"
printWarning w@(TCWarning _ env) = do
    code <- getCodeLines $ currentBTPos w
    renderTCType toWarningStyle $ prettyWarning w code $+$ text "\n"



renderTCType :: (TCStyle -> IO ()) -> Doc TCStyle -> IO ()
renderTCType colorStyle doc = do
    istty <- hSupportsANSI stdout
    if istty
        then renderDecoratedM colorStyle endAnn textprinter endDoc doc
        else printf $ render doc

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
    | isTraitType ty = declareError err <+> description err $+$ nest 2 (showPosition $ currentBTPos tcErr)

-- Default errors
prettyError (TCError err Env{bt = []}) _ =
    declareError err <+> description err
prettyError tcErr@(TCError err _) code =
    declareError err <+> description err $+$ codeViewer_ver1 tcErr code
-- Possible extensions:
--  Duplicate Class -> print positions (File + line) of the two classes
--  Type error in func call -> print a version of codeViewer that also shows the function head

prettyWarning :: TCWarning -> [String] -> Doc TCStyle
-- Default warnings
prettyWarning (TCWarning w Env{bt = []}) _ =
    declareWarning w <+> description w
prettyWarning tcWarn@(TCWarning w _) code =
        declareWarning w <+> description w $+$ codeViewer_ver1 tcWarn code

pipe = char '|'

declareError :: Error -> Doc TCStyle
declareError err =
    let
        hash = case lookupHash err of
            Nothing -> empty
            Just num -> text $ printf "[E%04d]" num
    in
        styleClassify $ text "Error" <> hash <> char ':'


declareWarning :: Warning -> Doc TCStyle
declareWarning w =
    let
        hash = case lookupHashW w of
            Nothing -> empty
            Just num -> text $ printf "[W%04d]" num
    in
        styleClassify $ text "Warning" <> hash <> char ':'

description :: Show a => a -> Doc TCStyle
description ty = styleDesc $ text $ show ty


showPosition :: Position -> Doc TCStyle
showPosition pos = styleLogistic (text "-->") <+> (text $ show $ pos)

codeViewer_ver1 :: (TCType a, Suggestable a) => a -> [String] -> Doc TCStyle
codeViewer_ver1 _ [] = error "TypeError.hs: No code to view"
codeViewer_ver1 err (cHead:cTail) =
    nest (digitLen) $ showPosition pos $+$
    styleLogistic pipe $+$
    showCodeHead
    showTailCode <+>
    styleHighlight (smallSuggest err) $+$
    longSuggest err

    where
        pos = currentBTPos err
        ((sL, sC), (eL, eC)) = getPositions pos
        digitLen = 1 + (length $ show eL) --One additional for the space between line-number and pipe
        tailCode = zipWith (codeLine " |") cTail [(sL+1)..eL]

        showCodeHead :: Doc TCStyle -> Doc TCStyle
        showCodeHead tail
            | sL == eL =
                codeLine "  " cHead sL $+$
                styleLogistic pipe <>
                styleHighlight (singleLineHighlighter sC eC '^') <+> tail
            | errorIsWholeLine cHead sC = codeLine " /" cHead sL $+$ tail
            | otherwise =
                codeLine "  " cHead sL $+$
                styleLogistic pipe <>
                styleHighlight (multilineHighlighter sC FirstLine '^') $+$ tail

        showTailCode :: Doc TCStyle
        showTailCode
            | null tailCode = empty
            | otherwise =
                vcat tailCode $+$
                styleLogistic pipe <>
                styleHighlight (multilineHighlighter eC LastLine '^')

        errorIsWholeLine _ 0 = True
        errorIsWholeLine _ 1 = True
        errorIsWholeLine (x:xs) n
            | x == ' '  = errorIsWholeLine xs (n-1)
            | otherwise = False



codeViewer_ver2 :: (TCType a, Suggestable a) => a -> [String] -> Doc TCStyle
codeViewer_ver2 _ [] = error "TypeError.hs: No code to view"
codeViewer_ver2 err (cHead:cTail) =
    let
        pos = currentBTPos err
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


-- Remove if version 2 is not to be used
lineHighlighter :: Int -> Int -> Char -> Doc ann
lineHighlighter s e c = text $ replicate (s-1) ' ' ++ replicate (e-s) c

-- Remove if version 1 is not to be used
singleLineHighlighter :: Int -> Int -> Char -> Doc ann
singleLineHighlighter s e c = space <+> text (replicate (s-1) ' ' ++ replicate (e-s) c)

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

noExplanation :: String -> IO ()
noExplanation errCode =
    let
        err = styleClassify $ text "error"
        info = styleDesc $ text $ printf ": no extended information for %s\n" errCode
    in
        renderTCType toErrorStyle $ err <> info