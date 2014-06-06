{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @MethodDecl@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.MethodDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr
import CodeGen.Type
import CodeGen.Closure
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

instance Translatable A.MethodDecl (Reader Ctx.Context (CCode Toplevel)) where
  translate mdecl@(A.Method {A.mtype = mtype, 
                             A.mname = mname, 
                             A.mparams = mparams,
                             A.mbody = mbody}) = do
    cdecl <- asks (fromJust . Ctx.the_class)
    let this_ty = A.cname cdecl
    ctx <- ask
    let ((bodyn,bodys),_) = runState (translate mbody) (Ctx.with_method mdecl ctx)
    closures <- mapM translateClosure (Util.filter A.isClosure mbody)
    return $ ConcatTL $ closures ++ 
       [(Function (translate mtype) (method_impl_name this_ty mname)
           ((data_rec_ptr this_ty, Var "this"):(map mparam_to_cvardecl mparams))
           (if not $ Ty.isVoidType mtype
            then (Seq $ bodys : [Embed ("return " ++ show bodyn)])
            else bodys))]
    where
      mparam_to_cvardecl (A.Param {A.pname = na, A.ptype = ty}) = (translate ty, Var $ show na)

instance FwdDeclaration A.MethodDecl (Reader Ctx.Context (CCode Toplevel)) where
    fwd_decls A.Method {A.mtype = mtype, 
                        A.mname = mname, 
                        A.mparams = mparams, 
                        A.mbody = mbody} = do
      cdecl <- asks (fromJust . Ctx.the_class)
      let this_ty = A.cname cdecl
      let params = data_rec_ptr this_ty : map (\(A.Param {A.ptype = ty}) -> (translate ty)) mparams
      return $ Embed $ show (translate mtype) ++ " " ++
             show (method_impl_name this_ty mname) ++ "(" ++ 
                  (concat $ intersperse ", " $ map show params) ++
             ");"
