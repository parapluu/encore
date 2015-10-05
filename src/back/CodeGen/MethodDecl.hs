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
    let returnType = translate mtype
        name = (methodImplName cname mname)
        encArgNames = map A.pname mparams
        encArgTypes = map A.ptype mparams
        argNames = map argName encArgNames
        argTypes = map translate encArgTypes
        args = (Ptr . AsType $ classTypeName cname, Var "_this") :
               if A.isMainClass cdecl && mname == ID.Name "main"
               then if null argNames
                    then [(array, Var "_argv")]
                    else zip argTypes argNames
               else zip argTypes argNames
        ctx = Ctx.new ((ID.Name "this", Var "_this") :
                       (zip encArgNames argNames)) ctable
        ((bodyn,bodys),_) = runState (translate mbody) ctx
        -- This reverse makes nested closures come before their
        -- enclosing closures. Not very nice...
        closures = map (\clos -> translateClosure clos ctable)
                       (reverse (Util.filter A.isClosure mbody))
        tasks = map (\tas -> translateTask tas ctable) $
                    reverse $ Util.filter A.isTask mbody
        retStmt = Return $ if Ty.isVoidType mtype then unit else bodyn
    in
      Concat $ closures ++ tasks ++
               [Function returnType name args (Seq $ [bodys, retStmt])]

  translate mdecl@(A.StreamMethod {A.mtype, A.mname, A.mparams, A.mbody})
            cdecl@(A.Class {A.cname})
            ctable =
    let name = (methodImplName cname mname)
        encArgNames = map A.pname mparams
        encArgTypes = map A.ptype mparams
        argNames = map argName encArgNames
        argTypes = map translate encArgTypes
        args = (Ptr . AsType $ classTypeName cname, Var "_this") :
               (stream, streamHandle) :
               zip argTypes argNames
        ctx = Ctx.new ((ID.Name "this", Var "_this") :
                       (zip encArgNames argNames)) ctable
        ((bodyn,bodys),_) = runState (translate mbody) ctx
        -- This reverse makes nested closures come before their
        -- enclosing closures. Not very nice...
        closures = map (\clos -> translateClosure clos ctable)
                       (reverse (Util.filter A.isClosure mbody))
        tasks = map (\tas -> translateTask tas ctable) $
                    reverse $ Util.filter A.isTask mbody
        streamClose = Statement $ Call (Nam "stream_close") [streamHandle]
    in
      Concat $ closures ++ tasks ++
       [Function void name args (Seq $ [bodys, streamClose])]
