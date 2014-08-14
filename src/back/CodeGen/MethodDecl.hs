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

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.List

instance Translatable A.MethodDecl (Reader Ctx.Context (CCode Toplevel)) where
  -- | Translates a method into the corresponding C-function
  translate mdecl@(A.Method {A.mtype, A.mname, A.mparams, A.mbody}) = do
    cdecl <- asks Ctx.the_class
    let this_ty = A.cname cdecl
    ctx <- ask

    let ((bodyn,bodys),_) = runState (translate mbody) ctx

    -- This reverse makes nested closures come before their enclosing closures. Not very nice...
    closures <- mapM translateClosure (reverse (Util.filter A.isClosure mbody)) 

    return $ Concat $ closures ++ 
       [Function (translate mtype) (method_impl_name this_ty mname)
        -- When we have a top-level main function, this should be cleaned up
           (if (A.isMainClass cdecl) && (A.mname mdecl == ID.Name "main")
            then [(data_rec_ptr this_ty, Var "this"), (int, Var "argc"), (Ptr $ Ptr char, Var "argv")]
            else (data_rec_ptr this_ty, Var "this") : (map mparam_to_cvardecl mparams))
           (if not $ Ty.isVoidType mtype
            then (Seq $ bodys : [Return bodyn])
            else (Seq $ bodys : [Return unit]))]
    where
      mparam_to_cvardecl (A.Param {A.pname, A.ptype}) = (translate ptype, Var $ show pname)