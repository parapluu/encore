module EAST.PrettyPrinter where

import Text.PrettyPrint

import qualified AST.PrettyPrinter as PP
import qualified EAST.EAST as EAST

ppProgram :: EAST.Program -> Doc
ppProgram = PP.ppProgram . EAST.toAST