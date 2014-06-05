{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
import CodeGen.Type
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.List

translateClosure :: A.Expr -> (Reader Ctx.Context (CCode Toplevel)) where
translateClosure closure 
    | isClosure closure = do 
                             let resultType = A.getType closure
                                 params = A.eparams closure
                                 body = A.body closure
    | otherwise = error "Tried to translate a closure from something that was not a closure"
