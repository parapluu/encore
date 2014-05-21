{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @MethodDecl@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.MethodDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
import CodeGen.Type
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST.AST as A
import qualified Identifiers as ID

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.List

instance Translatable A.MethodDecl (Reader Ctx.Context (CCode Toplevel)) where
  translate mdecl = do
    this_ty <- asks (A.cname . fromJust . Ctx.the_class)
    cdecl <- asks (fromJust . Ctx.the_class)
    ctx <- ask
    return $ 
      (Function (translate (A.mtype mdecl)) (method_impl_name (A.cname cdecl) (A.mname mdecl))
       ((data_rec_ptr this_ty, Var "this"):(map mparam_to_cvardecl $ A.mparams mdecl))
       (Statement (fst (runState (translate (A.mbody mdecl)) (Ctx.with_method mdecl ctx))::CCode Expr)))
    where
      mparam_to_cvardecl (A.Param {A.pname = na, A.ptype = ty}) = (translate ty, Var $ show na)

instance FwdDeclaration A.MethodDecl (Reader Ctx.Context (CCode Toplevel)) where
    fwd_decls mdecl = do
      cdecl <- asks (fromJust . Ctx.the_class)
      this_ty <- asks (A.cname . fromJust . Ctx.the_class)
      
      let params = data_rec_ptr this_ty : map (\(A.Param {A.ptype = ty}) -> (translate ty ::CCode Ty)) (A.mparams mdecl)
      return $ Embed $ show (translate . A.mtype $ mdecl) ++ " " ++
             show (method_impl_name (A.cname cdecl) (A.mname mdecl)) ++ "(" ++ 
                  (concat $ intersperse ", " $ map show params) ++
             ");"
