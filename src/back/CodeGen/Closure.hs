{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| Makes @Closure@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.Closure where

import CodeGen.Type
import CodeGen.Typeclasses
import CodeGen.Expr ()
import CodeGen.CCodeNames
import CodeGen.ClassTable
import qualified CodeGen.Context as Ctx
import CCode.Main

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified AST.Meta as Meta

import Types as Ty

import Control.Monad.State hiding (void)

translateClosure :: A.Expr -> ClassTable -> CCode Toplevel
translateClosure closure ctable
    | A.isClosure closure =
           let arrowType   = A.getType closure
               resultType  = Ty.getResultType arrowType
               argTypes    = Ty.getArgTypes arrowType
               params      = A.eparams closure
               body        = A.body closure
               id          = Meta.getMetaId . A.getMeta $ closure
               funName     = closureFunName id
               envName     = closureEnvName id
               traceName   = closureTraceName id
               freeVars    = Util.freeVariables (map A.pname params) body
               encEnvNames = map fst freeVars
               envNames    = map (AsLval . fieldName) encEnvNames
               encArgNames = map A.pname params
               argNames    = map (AsLval . argName) encArgNames
               subst       = zip encEnvNames envNames ++
                             zip encArgNames argNames
               ctx = Ctx.new subst ctable

               ((bodyName, bodyStat), _) = runState (translate body) ctx
           in
             Concat [buildEnvironment envName freeVars,
                     tracefunDecl traceName envName freeVars,
                     Function (Typ "value_t") funName
                              [(Ptr encoreCtxT, encoreCtxVar),
                               (Typ "value_t", Var "_args[]"),
                               (Ptr void, Var "_env")]
                              (Seq $
                                extractArguments params ++
                                extractEnvironment envName freeVars ++
                                [bodyStat, returnStmnt bodyName resultType])]
    | otherwise =
        error
        "Tried to translate a closure from something that was not a closure"
    where
      returnStmnt var ty
          | isVoidType ty = Return $ asEncoreArgT (translate ty) unit
          | otherwise     = Return $ asEncoreArgT (translate ty) var

      extractArguments params = extractArguments' params 0
      extractArguments' [] _ = []
      extractArguments' ((A.Param{A.pname, A.ptype}):args) i =
          Assign (Decl (ty, arg)) (getArgument i) : extractArguments' args (i+1)
          where
            ty = translate ptype
            arg = AsLval $ argName pname
            getArgument i = fromEncoreArgT ty $ AsExpr $ ArrAcc i (Var "_args")

      buildEnvironment name members =
          StructDecl (Typ $ show name) (map translateBinding members)
              where
                translateBinding (name, ty) =
                    (translate ty, AsLval $ fieldName name)

      extractEnvironment _ [] = []
      extractEnvironment envName ((name, ty):vars) =
          Assign (Decl (translate ty, AsLval $ fieldName name)) (getVar name) :
          extractEnvironment envName vars
          where
            getVar name =
                (Deref $ Cast (Ptr $ Struct envName) (Var "_env"))
                `Dot` fieldName name

      tracefunDecl traceName envName members =
          Function void traceName
                   [(Ptr encoreCtxT, encoreCtxVar), (Ptr void, Var "p")]
                   (Seq $ map traceMember members)
          where
            traceMember (name, ty)
                | Ty.isActiveClassType ty =
                    Call ponyTraceActor [AsExpr encoreCtxVar, Cast (Ptr ponyActorT) (getVar name)]
                | Ty.isPassiveClassType ty =
                    Call ponyTraceObject [encoreCtxVar, getVar name, AsLval $ classTraceFnName ty]
                | otherwise = Comm $ "Not tracing member '" ++ show name ++ "'"
            getVar name =
                (Deref $ Cast (Ptr $ Struct envName) (Var "p"))
                `Dot` fieldName name
