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
import Control.Arrow ((&&&))

instance Translatable A.MethodDecl (A.ClassDecl -> ClassTable -> CCode Toplevel) where
  -- | Translates a method into the corresponding C-function
  translate mdecl@(A.Method {A.mbody})
            cdecl@(A.Class {A.cname})
            ctable
      | A.isStreamMethod mdecl =
    let name = methodImplName cname (A.methodName mdecl)
        (encArgNames, encArgTypes) =
            unzip . map (A.pname &&& A.ptype) $ A.methodParams mdecl
        argNames = map (AsLval . argName) encArgNames
        argTypes = map translate encArgTypes
        args = (Ptr encoreCtxT, encoreCtxVar) :
               (Ptr . AsType $ classTypeName cname, Var "_this") :
               (stream, streamHandle) : zip argTypes argNames
        ctx = Ctx.new ((ID.Name "this", Var "_this") :
                       zip encArgNames argNames) ctable
        ((bodyn,bodys),_) = runState (translate mbody) ctx
        -- This reverse makes nested closures come before their
        -- enclosing closures. Not very nice...
        closures = map (\clos -> translateClosure clos ctable)
                       (reverse (Util.filter A.isClosure mbody))
        tasks = map (\tas -> translateTask tas ctable) $
                    reverse $ Util.filter A.isTask mbody
        streamClose = Statement $ Call streamClose [encoreCtxVar, streamHandle]
    in
      Concat $ closures ++ tasks ++
               [Function void name args (Seq [bodys, streamClose])]
      | otherwise =
    let mName = A.methodName mdecl
        mType = A.methodType mdecl
        returnType = translate mType
        name = methodImplName cname mName
        (encArgNames, encArgTypes) =
            unzip . map (A.pname &&& A.ptype) $ A.methodParams mdecl
        argNames = map (AsLval . argName) encArgNames
        argTypes = map translate encArgTypes
        args = (Ptr encoreCtxT, encoreCtxVar) :
               (Ptr . AsType $ classTypeName cname, Var "_this") :
               if A.isMainMethod cname mName && null argNames
               then [(array, Var "_argv")]
               else zip argTypes argNames
        ctx = Ctx.new ((ID.Name "this", Var "_this") :
                       zip encArgNames argNames) ctable
        ((bodyn,bodys),_) = runState (translate mbody) ctx
        -- This reverse makes nested closures come before their
        -- enclosing closures. Not very nice...
        closures = map (\clos -> translateClosure clos ctable)
                       (reverse (Util.filter A.isClosure mbody))
        tasks = map (\tas -> translateTask tas ctable) $
                    reverse $ Util.filter A.isTask mbody
        retStmt = Return $ if Ty.isVoidType mType then unit else bodyn
    in
      Concat $ closures ++ tasks ++
               [Function returnType name args (Seq [bodys, retStmt])]
