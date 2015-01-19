{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}

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

import Control.Monad.Reader hiding(void)
import Control.Monad.State hiding(void)
import Data.Maybe
import Data.List

instance Translatable A.MethodDecl (A.ClassDecl -> CCode Toplevel) where
  -- | Translates a method into the corresponding C-function
  translate mdecl@(A.Method {A.mtype, A.mname, A.mparams, A.mbody})
            cdecl@(A.Class {A.cname}) = 
    let ((bodyn,bodys),_) = runState (translate mbody) Ctx.empty
        -- This reverse makes nested closures come before their enclosing closures. Not very nice...
        closures = map translateClosure (reverse (Util.filter A.isClosure mbody)) 
    in
      Concat $ closures ++ 
       [Function (translate mtype) (method_impl_name cname mname)
        -- When we have a top-level main function, this should be cleaned up
           (if (A.isMainClass cdecl) && (A.mname mdecl == ID.Name "main")
            then [(data_rec_ptr cname, Var "this"), (int, Var "argc"), (Ptr $ Ptr char, Var "argv")]
            else (data_rec_ptr cname, Var "this") : (map mparam_to_cvardecl mparams))
           (if not $ Ty.isVoidType mtype
            then (Seq $ bodys : [Return bodyn])
            else (Seq $ bodys : [Return unit]))]
    where
      mparam_to_cvardecl (A.Param {A.pname, A.ptype}) = (translate ptype, Var $ show pname)

  translate mdecl@(A.StreamMethod {A.mtype, A.mname, A.mparams, A.mbody})
            cdecl@(A.Class {A.cname}) = 
    let ((bodyn,bodys),_) = runState (translate mbody) Ctx.empty
        -- This reverse makes nested closures come before their enclosing closures. Not very nice...
        closures = map translateClosure (reverse (Util.filter A.isClosure mbody)) 
    in
      Concat $ closures ++ 
       [Function void (method_impl_name cname mname)
           ((data_rec_ptr cname, Var "this") : (stream, stream_handle) : 
            (map mparam_to_cvardecl mparams))
           (Seq $ bodys : [Statement $ Call (Nam "stream_close") [stream_handle]])]
    where
      mparam_to_cvardecl (A.Param {A.pname, A.ptype}) = (translate ptype, Var $ show pname)