{-# LANGUAGE OverloadedStrings #-}

module Typechecker.Suggestable (
                                smallSuggest
                                ,longSuggest
                                )where

-- Library dependencies
import Text.PrettyPrint.Annotated
import Text.Printf (printf)
import Data.Maybe

-- Module dependencies
import AST.AST
import AST.PrettyPrinter hiding (indent)
import Typechecker.TypeError
import Typechecker.Environment
import Typechecker.Util
import Identifiers
import Types


pipe = char '|'

highlightPretty :: String -> Doc TCStyle
highlightPretty s = highlight $ text s

codeViewerNote :: Doc TCStyle
codeViewerNote = logistic (pipe $+$ equals) <+> desc (text "note:")

-- How to determine if to use a smallSuggest or longSuggest:
-- If a problem justifies it, you could use both, 
-- they are made so that from none to both are able to be used at the same time.
--
-- a smallSuggest are inlined with the highlighting of an error,
-- therefore it is good practice for the text to be fairly short,
-- about 32 characters seem to be a good maximum to strive for.
-- If more are needed, use longSuggest instead.
class Suggestable a where
    smallSuggest :: a -> Doc TCStyle
    longSuggest :: a -> Doc TCStyle

instance Suggestable TCError where
    smallSuggest (TCError (NonAssignableLHSError) _) = highlightPretty "Can only be used on var or fields"
    smallSuggest (TCError (MethodNotFoundError name ty) env)
        | isMethodNameAFunction name ty env = highlightPretty $ printf "Did you mean function `%s`?" (show name)
    smallSuggest (TCError (TypeMismatchError actual expected) _) =
        highlightPretty $ printf "expected %s" (show expected)
    smallSuggest _ = empty

    longSuggest (TCError (TypeMismatchError actual expected) _) =
        let
            expect = text "expected type" <+> desc (text $ show expected)
            found  = text "   found type" <+> desc (text $ show actual)
        in
            codeViewerNote <+> vcat [expect, found]
    longSuggest (TCError (TypeWithCapabilityMismatchError actual cap expected) _) =
        let
            expect = text "expected type" <+> desc (text $ show expected)
            found  = text "   found type" <+> desc (text $ show actual)
        in
            codeViewerNote <+> vcat [expect, found]
    longSuggest (TCError (WrongNumberOfMethodArgumentsError name targetType _ _) env) = 
        let 
            header = snd . fromJust $ findMethodWithEnvironment name targetType env
            types = hparams header
        in
            codeViewerNote <+> hang ("Method" <+> quotes (text $ show name) <+> "is declared:") 0
                (desc (ppFunctionHeader header))
    
    longSuggest _ = empty


instance Suggestable Warning where
    smallSuggest _ = empty
    longSuggest _ = empty



isMethodNameAFunction name ty env =
    let (_, functions) = getFunctionNames ty env
    in elem name functions


