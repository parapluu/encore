{-# LANGUAGE OverloadedStrings #-}

module Typechecker.Suggestable (
                                Suggestable
                                ,smallSuggest
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


makeNotation :: Doc TCStyle
makeNotation = styleLogistic (pipe $+$ equals) <+> styleDesc (text "note:")

-- How to determine if to use a smallSuggest or longSuggest:
-- If a problem justifies it, you could use both, 
-- they are made so that from none to both are able to be used at the same time.
--
-- a smallSuggest are inlined with the highlighting of an error,
-- therefore it is good practice for the text to be fairly short,
-- about 32 characters seem to be a good maximum to strive for.
-- If more are needed, use longSuggest instead.
class Suggestable a where
    smallSuggest :: a -> Doc ann
    longSuggest :: a -> Doc TCStyle

instance Suggestable TCError where
    smallSuggest (TCError (NonAssignableLHSError) _) = "Can only be used on var or fields"
    smallSuggest (TCError (MethodNotFoundError name ty) env)
        | isMethodNameAFunction name ty env = text $ printf "Did you mean function `%s`?" (show name)
    smallSuggest _ = empty



    longSuggest (TCError (TypeMismatchError actual expected) _) =
            makeNotation <+> vcat [expect expected, found actual]
        where
            expect e = text "expected type" <+> styleDesc (text $ show e)
            found a  = text "   found type" <+> styleDesc (text $ show a)

    longSuggest (TCError (WrongNumberOfMethodArgumentsError name targetType _ _) env) = 
        let 
            header = snd . fromJust $ findMethodWithEnvironment name targetType env
            types = hparams header
        in
            makeNotation <+> hang ("Method" <+> quotes (text $ show name) <+> "is declared:") 0
                (styleDesc (ppFunctionHeader header))

    longSuggest (TCError (BinaryOperandMismatchError _ _ lType rType) _) =
        let
            left  = text " Left type: " <+> styleDesc (text $ show lType)
            right = text "Right type: " <+> styleDesc (text $ show rType)
        in
            makeNotation <+> vcat [left, right]
    
    longSuggest _ = empty


instance Suggestable TCWarning where
    smallSuggest _ = empty
    longSuggest _ = empty



isMethodNameAFunction name ty env =
    let (_, functions) = getFunctionNames ty env
    in elem name functions


