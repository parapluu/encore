{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure where

import CodeGen.Typeclasses
import CodeGen.Expr
import CodeGen.CCodeNames
import qualified CodeGen.Context as Ctx
import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util

import Types as Ty

import Control.Monad.Reader
import Control.Monad.State

translateClosure :: A.Expr -> (Reader Ctx.Context (CCode Toplevel))
translateClosure closure 
    | A.isClosure closure = do let resultType = Ty.getResultType $ A.getType closure
                                   argTypes = Ty.getArgTypes $ A.getType closure
                                   params = A.eparams closure
                                   body = A.body closure
                                   id = A.getMetaId closure
                               ctx <- ask
                               let ((bodyName, bodyStat), _) = runState (translate body) ctx
                               return $ ConcatTL $ FunTypeDef (closure_type_name id) (translate resultType) (map translate argTypes) :
                                                   (Function (translate resultType) (closure_fun_name id) 
                                                       (map translateParam params)
                                                       (Seq $ bodyStat : [Embed $ "return " ++ show bodyName])) :
                                                   [StructDecl (Typ $ show $ closure_impl_name id)
                                                       [(Typ $ show (closure_type_name id), Var "call")]]
--    FunTypeDef :: CCode Name -> CCode Ty -> (CCode Ty) -> CCode Toplevel
    | otherwise = error "Tried to translate a closure from something that was not a closure"
    where
      translateParam (A.Param {A.pname = na, A.ptype = ty}) = (translate ty, Var $ show na)
