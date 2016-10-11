module CodeGen.Header(generateHeader) where

import Control.Arrow ((&&&))
import Data.List (partition)

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Function
import CodeGen.Type ()

import CCode.Main
import CCode.PrettyCCode ()

import qualified AST.AST as A
import qualified Identifiers as ID
import qualified Types as Ty

-- | Generates the C header file for the translated program
-- | This function generates all the common code, generateHeaderRecurser generates class specific code
generateHeader :: A.Program -> CCode FIN

generateHeader p =
    Program $
    IfNDefine "HEADER_H" $
    Concat $
    HashDefine "HEADER_H" :
    HashDefine "_XOPEN_SOURCE 800" :
    (Includes [
      "pthread.h", -- Needed because of the use of locks in future code, remove if we choose to remove lock-based futures
      "pony.h",
      "pool.h",
      "stdlib.h",
      "closure.h",
      "stream.h",
      "array.h",
      "tuple.h",
      "range.h",
      "future.h",
      "task.h",
      "option.h",
      "party.h",
      "string.h",
      "stdio.h",
      "stdarg.h"
     ]) :
    HashDefine "UNIT ((void*) -1)" :

    [commentSection "Shared messages"] ++
    sharedMessages ++

    [commentSection "Embedded code"] ++
    map Embed embedded ++

    [commentSection "Class type decls"] ++
    classTypeDecls ++

    [commentSection "Trait type decls"] ++
    traitTypeDecls ++

    [commentSection "Passive class types"] ++
    passiveTypes ++

    [commentSection "Runtime types"] ++
    runtimeTypeDecls ++

    [commentSection "Message IDs"] ++
    [messageEnums] ++

    [commentSection "Message types"] ++
    ponyMsgTTypedefs ++
    ponyMsgTImpls ++

    [commentSection "Global functions"] ++
    globalFunctions ++

    [commentSection "Class IDs"] ++
    [classEnums] ++

    [commentSection "Trace functions"] ++
    traceFnDecls ++

    [commentSection "Runtime type init functions"] ++
    runtimeTypeFnDecls ++

    [commentSection "Methods"] ++
    concatMap methodFwds classes ++
    concatMap wrapperMethods classes ++

    [commentSection "Constructors"] ++
    concatMap constructors classes ++

    [commentSection "Main actor rtti"] ++
    [externMainRtti] ++

    [commentSection "Trait types"] ++
    [traitMethodEnums] ++
    traitTypes
   where
     externMainRtti = DeclTL (Typ "extern pony_type_t", Var "_enc__active_Main_type")

     sharedMessages =
          [DeclTL (ponyMsgT, Var "m_MSG_alloc"),
           DeclTL (ponyMsgT, Var "m_resume_get"),
           DeclTL (ponyMsgT, Var "m_resume_suspend"),
           DeclTL (ponyMsgT, Var "m_resume_await"),
           DeclTL (ponyMsgT, Var "m_run_closure")
          ]

     traits = A.traits p
     classes = A.classes p
     functions = A.functions p
     embedded = A.allEmbedded p

     ponyMsgTTypedefs :: [CCode Toplevel]
     ponyMsgTTypedefs = map ponyMsgTTypedefClass classes
            where
                ponyMsgTTypedefClass A.Class{A.cname, A.cmethods} =
                    Concat $ concatMap ponyMsgTTypedef cmethods
                    where
                        ponyMsgTTypedef mdecl =
                            [Typedef (Struct $ futMsgTypeName cname (A.methodName mdecl)) (futMsgTypeName cname (A.methodName mdecl)),
                             Typedef (Struct $ oneWayMsgTypeName cname (A.methodName mdecl)) (oneWayMsgTypeName cname (A.methodName mdecl))]

     ponyMsgTImpls :: [CCode Toplevel]
     ponyMsgTImpls = map ponyMsgTImplsClass classes
              where
                ponyMsgTImplsClass A.Class{A.cname, A.cmethods} =
                    Concat $ map ponyMsgTImpl cmethods
                    where
                      ponyMsgTImpl :: A.MethodDecl -> CCode Toplevel
                      ponyMsgTImpl mdecl =
                          let argrttys = map (translate . A.getType) (A.methodParams mdecl)
                              argnamesWComments = zipWith (\n name -> (Annotated (show name) (Var ("f"++show n)))) ([1..]:: [Int]) (map A.pname $ A.methodParams mdecl)
                              argspecs = zip argrttys argnamesWComments :: [CVarSpec]
                              encoreMsgTSpec = (encMsgT, Var "")
                              encoreMsgTSpecOneway = (encOnewayMsgT, Var "msg")
                          in Concat [StructDecl (AsType $ futMsgTypeName cname (A.methodName mdecl)) (encoreMsgTSpec : argspecs)
                                    ,StructDecl (AsType $ oneWayMsgTypeName cname (A.methodName mdecl)) (encoreMsgTSpecOneway : argspecs)]

     globalFunctions =
       [globalFunctionDecl f | f <- functions] ++
       [globalFunctionWrapperDecl f | f <- functions] ++
       [globalFunctionClosureDecl f | f <- functions]

     messageEnums =
                let
                    meta = concatMap (\cdecl -> zip (repeat $ A.cname cdecl) (map A.methodName (A.cmethods cdecl))) classes
                    methodMsgNames = map (show . (uncurry futMsgId)) meta
                    oneWayMsgNames = map (show . (uncurry oneWayMsgId)) meta
                in
                       Enum $ (Nam "_MSG_DUMMY__ = 1024") : map Nam (methodMsgNames ++ oneWayMsgNames)

     classEnums =
       let
        classIds = map (refTypeId . A.getType) classes
        traitIds = map (refTypeId . A.getType) traits
       in
        Enum $ (Nam "__ID_DUMMY__ = 1024") : classIds ++ traitIds

     traceFnDecls = map traceFnDecl classes
         where
           traceFnDecl A.Class{A.cname} =
               FunctionDecl void (classTraceFnName cname) [Ptr encoreCtxT,Ptr void]

     runtimeTypeFnDecls = map runtimeTypeFnDecl classes
         where
           runtimeTypeFnDecl A.Class{A.cname} =
               FunctionDecl void (runtimeTypeInitFnName cname) [Ptr . AsType $ classTypeName cname, Embed "..."]

     classTypeDecls = map classTypeDecl classes
                 where
                   classTypeDecl A.Class{A.cname} =
                       Typedef (Struct $ classTypeName cname) (classTypeName cname)

     passiveTypes = map passiveType $ filter (A.isPassive) classes
                 where
                   passiveType A.Class{A.cname, A.cfields} =
                       let typeParams = Ty.getTypeParameters cname in
                       StructDecl (AsType $ classTypeName cname)
                                  ((Ptr ponyTypeT, AsLval $ selfTypeField) :
                                   map (\ty -> (Ptr ponyTypeT, AsLval $ typeVarRefName ty)) typeParams ++
                                   zip
                                   (map (translate . A.ftype) cfields)
                                   (map (AsLval . fieldName . A.fname) cfields))
     traitMethodEnums =
       let
         dicts = map (A.getType &&& A.traitInterface) traits
         pairs = concatMap (\(t, hs) -> zip (repeat t) (map A.hname hs)) dicts
         syncs = map (show . (uncurry msgId)) pairs
       in Enum $ (Nam "__TRAIT_METHOD_DUMMY__ = 1024") : map Nam syncs

     traitTypeDecls = map traitTypeDecl traits
       where
         traitTypeDecl A.Trait{A.tname} =
           let ty = refTypeName tname in Typedef (Struct $ ty) ty

     traitTypes = map traitType traits
       where
         traitType A.Trait{A.tname} =
           let
             formal = Ty.getTypeParameters tname
             self = (Ptr ponyTypeT, AsLval $ selfTypeField)
           in
             StructDecl (AsType $ refTypeName tname) [self]

     runtimeTypeDecls = map typeDecl classes ++ map typeDecl traits
       where
         typeDecl ref =
           let
             ty = A.getType ref
             runtimeTy = runtimeTypeName ty
           in
             DeclTL (Extern ponyTypeT, AsLval runtimeTy)

     methodFwds cdecl@(A.Class{A.cname, A.cmethods}) = map methodFwd cmethods
         where
           methodFwd m
               | A.isStreamMethod m =
                   let params = (Ptr (Ptr encoreCtxT)) :
                                (Ptr . AsType $ classTypeName cname) : stream :
                                map (translate . A.ptype) mparams
                   in
                     FunctionDecl void (methodImplName cname mname) params
               | otherwise =
                   let params = if A.isMainClass cdecl && mname == ID.Name "main"
                                then [Ptr . AsType $ classTypeName cname, array]
                                else (Ptr . AsType $ classTypeName cname) :
                                     map (translate . A.ptype) mparams
                   in
                     FunctionDecl (translate mtype) (methodImplName cname mname)
                                  (Ptr (Ptr encoreCtxT):params)
               where
                 mname = A.methodName m
                 mparams = A.methodParams m
                 mtype = A.methodType m

     wrapperMethods A.Class{A.cname, A.cmethods} =
       if Ty.isPassiveClassType $ cname then
         []
       else
         map (genericMethod methodImplFutureName future) nonStreamMethods ++
         map (genericMethod methodImplOneWayName void) nonStreamMethods ++
         map (genericMethod methodImplStreamName stream) streamMethods
       where
         genericMethod genMethodName retType m =
           let
             thisType = Ptr . AsType $ classTypeName cname
             rest = map (translate . A.ptype) (A.methodParams m)
             args = Ptr (Ptr encoreCtxT) : thisType : rest
             f = genMethodName cname (A.methodName m)
           in
             FunctionDecl retType f args

         (streamMethods, nonStreamMethods) =
           partition A.isStreamMethod cmethods

     constructors A.Class{A.cname} = [ctr]
       where
         ctr =
           let
             retType = Ptr. AsType $ classTypeName cname
             f = constructorImplName cname
           in
             FunctionDecl retType f []

commentSection :: String -> CCode Toplevel
commentSection s = Embed $ replicate (5 + length s) '/' ++ "\n// " ++ s
