{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @MethodDecl@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.MethodDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr ()
import CodeGen.Closure
import CodeGen.Task
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx

import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding(void)

instance Translatable A.MethodDecl (A.ClassDecl -> ClassTable -> CCode Toplevel) where
  -- | Translates a method into the corresponding C-function
  translate mdecl@(A.Method {A.mtype, A.mname, A.mparams, A.mbody})
            cdecl@(A.Class {A.cname})
            ctable =
    let enc_arg_names = map A.pname mparams
        enc_arg_types = map A.ptype mparams
        arg_names = map arg_name enc_arg_names
        arg_types = map translate enc_arg_types
        ctx = Ctx.new ((ID.Name "this", Var "_this") : (zip enc_arg_names arg_names)) ctable
        ((bodyn,bodys),_) = runState (translate mbody) ctx
        -- This reverse makes nested closures come before their enclosing closures. Not very nice...
        closures = map (\clos -> translateClosure clos ctable) (reverse (Util.filter A.isClosure mbody))
        tasks = map (\tas -> translateTask tas ctable) $ reverse $ Util.filter A.isTask mbody
        ret_stmt = Return $ if Ty.isVoidType mtype then unit else bodyn
    in
      Concat $ closures ++ tasks ++
       [Function (translate mtype) (method_impl_name cname mname)
        -- When we have a top-level main function, this should be cleaned up
           (if (A.isMainClass cdecl) && (mname == ID.Name "main")
            then [(Ptr . AsType $ class_type_name cname, Var "_this"), (int, Var "argc"), (Ptr $ Ptr char, Var "argv")]
            else (Ptr . AsType $ class_type_name cname, Var "_this") : (zip arg_types arg_names))
            (Seq $ [bodys, ret_stmt])
            ]

  translate mdecl@(A.StreamMethod {A.mtype, A.mname, A.mparams, A.mbody})
            cdecl@(A.Class {A.cname})
            ctable =
    let enc_arg_names = map A.pname mparams
        enc_arg_types = map A.ptype mparams
        arg_names = map arg_name enc_arg_names
        arg_types = map translate enc_arg_types
        ctx = Ctx.new ((ID.Name "this", Var "_this") : (zip enc_arg_names arg_names)) ctable
        ((bodyn,bodys),_) = runState (translate mbody) ctx
        -- This reverse makes nested closures come before their enclosing closures. Not very nice...
        closures = map (\clos -> translateClosure clos ctable) (reverse (Util.filter A.isClosure mbody))
        tasks = map (\tas -> translateTask tas ctable) $ reverse $ Util.filter A.isTask mbody
    in
      Concat $ closures ++ tasks ++
       [Function void (method_impl_name cname mname)
           ((Ptr . AsType $ class_type_name cname, Var "_this") : (stream, stream_handle) :
            (zip arg_types arg_names))
           (Seq $ bodys : [Statement $ Call (Nam "stream_close") [stream_handle]])]
