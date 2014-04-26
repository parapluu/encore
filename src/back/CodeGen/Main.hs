{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen.Main (code_from_AST) where

import CodeGen.Program
import CCode.Main

import qualified AST as A

code_from_AST :: A.Program -> CCode
code_from_AST = translate

