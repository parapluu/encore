{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| Makes @MethodDecl@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.MethodDecl where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr ()
import CodeGen.Closure
import CodeGen.Task
import CodeGen.ClassTable
import CodeGen.Function(returnStatement)
import qualified CodeGen.Context as Ctx

import CCode.Main
import Data.List (intersect)

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding(void)
import Control.Arrow ((&&&))

instance Translatable A.MethodDecl (A.ClassDecl -> ProgramTable -> CCode Toplevel) where
  -- | Translates a method into the corresponding C-function
  translate mdecl@(A.Method {A.mbody})
            cdecl@(A.Class {A.cname})
            table
      | A.isStreamMethod mdecl =
    let args = (Ptr (Ptr encoreCtxT), encoreCtxVar) :
               (Ptr . AsType $ classTypeName cname, Var "_this") :
               (stream, streamHandle) : zip argTypes argNames
        streamCloseStmt = Statement $
          Call streamClose [encoreCtxVar, streamHandle]
    in
      Concat $ closures ++ tasks ++
               [Function void name args
                 (Seq [extractTypeVars, bodys, streamCloseStmt])]
      | otherwise =
    let returnType = translate mType
        args = (Ptr (Ptr encoreCtxT), encoreCtxVar) :
               (Ptr . AsType $ classTypeName cname, Var "_this") :
               if A.isMainMethod cname mName && null argNames
               then [(array, Var "_argv")]
               else zip argTypes argNames
    in
      Concat $ closures ++ tasks ++
               [Function returnType name args
                 (Seq [extractTypeVars, bodys, returnStatement mType bodyn])]
    where
      mName = A.methodName mdecl
      mType = A.methodType mdecl
      typeVars = Ty.getTypeParameters cname
      name = methodImplName cname (A.methodName mdecl)
      (encArgNames, encArgTypes) =
          unzip . map (A.pname &&& A.ptype) $ A.methodParams mdecl
      argNames = map (AsLval . argName) encArgNames
      argTypes = map translate encArgTypes
      subst = [(ID.Name "this", Var "_this")] ++
        varSubFromTypeVars typeVars ++
        zip encArgNames argNames
      ctx = Ctx.new subst table
      ((bodyn,bodys),_) = runState (translate mbody) ctx
      extractTypeVars = Seq $ map assignTypeVar typeVars
      assignTypeVar ty =
        let fName = typeVarRefName ty
        in Assign (Decl (Ptr ponyTypeT, AsLval fName)) $ getVar fName
      getVar name =
        (Deref $ Cast (Ptr . AsType $ classTypeName cname) (Var "_this"))
        `Dot`
        name
      closures = map (\clos -> translateClosure clos typeVars table)
                     (reverse (Util.filter A.isClosure mbody))
      tasks = map (\tas -> translateTask tas table) $
                  reverse $ Util.filter A.isTask mbody
