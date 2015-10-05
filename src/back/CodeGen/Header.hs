module CodeGen.Header(generateHeader) where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
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
    map Embed allembedded ++

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
    globalFunctionDecls ++

    [commentSection "Class IDs"] ++
    [classEnums] ++

    [commentSection "Trace functions"] ++
    traceFnDecls ++

    [commentSection "Runtime type init functions"] ++
    runtimeTypeFnDecls ++

    [commentSection "Methods"] ++
    concatMap methodFwds allclasses ++
    concatMap publicMethods allclasses ++

    [commentSection "Constructors"] ++
    concatMap constructors allclasses ++

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

     allTraits = A.allTraits p
     allclasses = A.allClasses p
     allfunctions = A.allFunctions p
     allembedded = A.allEmbedded p

     ponyMsgTTypedefs :: [CCode Toplevel]
     ponyMsgTTypedefs = map ponyMsgTTypedefClass allclasses
            where
                ponyMsgTTypedefClass cdecl@(A.Class{A.cname, A.cmethods}) =
                    Concat $ concatMap ponyMsgTTypedef cmethods
                    where
                        ponyMsgTTypedef mdecl =
                            [Typedef (Struct $ futMsgTypeName cname (A.mname mdecl)) (futMsgTypeName cname (A.mname mdecl)),
                             Typedef (Struct $ oneWayMsgTypeName cname (A.mname mdecl)) (oneWayMsgTypeName cname (A.mname mdecl))]

     ponyMsgTImpls :: [CCode Toplevel]
     ponyMsgTImpls = map ponyMsgTImplsClass allclasses
              where
                ponyMsgTImplsClass cdecl@(A.Class{A.cname, A.cmethods}) =
                    Concat $ map ponyMsgTImpl cmethods
                    where
                      ponyMsgTImpl :: A.MethodDecl -> CCode Toplevel
                      ponyMsgTImpl mdecl =
                          let argrttys = map (translate . A.getType) (A.mparams mdecl)
                              argnamesWComments = zipWith (\n name -> (Annotated (show name) (Var ("f"++show n)))) ([1..]:: [Int]) (map A.pname $ A.mparams mdecl)
                              argspecs = zip argrttys argnamesWComments :: [CVarSpec]
                              encoreMsgTSpec = (encMsgT, Var "")
                              encoreMsgTSpecOneway = (encOnewayMsgT, Var "msg")
                          in Concat [StructDecl (AsType $ futMsgTypeName cname (A.mname mdecl)) (encoreMsgTSpec : argspecs)
                                    ,StructDecl (AsType $ oneWayMsgTypeName cname (A.mname mdecl)) (encoreMsgTSpecOneway : argspecs)]

     globalFunctionDecls = map globalFunctionDecl allfunctions
           where
               globalFunctionDecl A.Function{A.funname} =
                  DeclTL (closure, AsLval $ globalClosureName funname)

     messageEnums =
                let
                    meta = concatMap (\cdecl -> zip (repeat $ A.cname cdecl) (map A.mname (A.cmethods cdecl))) allclasses
                    methodMsgNames = map (show . (uncurry futMsgId)) meta
                    oneWayMsgNames = map (show . (uncurry oneWayMsgId)) meta
                in
                       Enum $ (Nam "_MSG_DUMMY__ = 1024") : map Nam (methodMsgNames ++ oneWayMsgNames)

     classEnums =
       let
        classIds = map (refTypeId . A.getType) allclasses
        traitIds = map (refTypeId . A.getType) allTraits
       in
        Enum $ (Nam "__ID_DUMMY__ = 1024") : classIds ++ traitIds

     traceFnDecls = map traceFnDecl allclasses
         where
           traceFnDecl A.Class{A.cname} =
               FunctionDecl void (classTraceFnName cname) [Ptr void]

     runtimeTypeFnDecls = map runtimeTypeFnDecl allclasses
         where
           runtimeTypeFnDecl A.Class{A.cname} =
               FunctionDecl void (runtimeTypeInitFnName cname) [Ptr . AsType $ classTypeName cname, Embed "..."]

     classTypeDecls = map classTypeDecl allclasses
                 where
                   classTypeDecl A.Class{A.cname} =
                       Typedef (Struct $ classTypeName cname) (classTypeName cname)

     passiveTypes = map passiveType $ filter (A.isPassive) allclasses
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
         dicts = map (\t -> (A.getType t, A.tmethods t)) allTraits
         pairs = concatMap (\(t, ms) -> zip (repeat t) (map A.mname ms)) dicts
         syncs = map (show . (uncurry oneWayMsgId)) pairs
       in Enum $ (Nam "__TRAIT_METHOD_DUMMY__ = 1024") : map Nam syncs

     traitTypeDecls = map traitTypeDecl allTraits
       where
         traitTypeDecl A.Trait{A.tname} =
           let ty = refTypeName tname in Typedef (Struct $ ty) ty

     traitTypes = map traitType allTraits
       where
         traitType A.Trait{A.tname} =
           let
             formal = Ty.getTypeParameters tname
             self = (Ptr ponyTypeT, AsLval $ selfTypeField)
           in
             StructDecl (AsType $ refTypeName tname) [self]

     runtimeTypeDecls = map typeDecl allclasses ++ map typeDecl allTraits
       where
         typeDecl ref =
           let
             ty = A.getType ref
             runtimeTy = runtimeTypeName ty
           in
             DeclTL (Extern ponyTypeT, AsLval runtimeTy)

     methodFwds cdecl@(A.Class{A.cname, A.cmethods}) = map methodFwd cmethods
                 where
                   methodFwd A.Method{A.mtype, A.mname, A.mparams} =
                     let params = if (A.isMainClass cdecl) && (mname == ID.Name "main")
                                  then [Ptr . AsType $ classTypeName cname, array]
                                  else (Ptr . AsType $ classTypeName cname) : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
                     in
                       FunctionDecl (translate mtype) (methodImplName cname mname) params
                   methodFwd A.StreamMethod{A.mtype, A.mname, A.mparams} =
                     let params = (Ptr . AsType $ classTypeName cname) : stream : map (\(A.Param {A.ptype}) -> (translate ptype)) mparams
                     in
                       FunctionDecl void (methodImplName cname mname) params

     publicMethods A.Class{A.cname, A.cmethods} =
       if not . Ty.isSharedClassType $ cname then
         []
       else
       map futureMethod cmethods ++ map oneWayMethod cmethods
       where
         futureMethod A.Method{A.mtype, A.mname, A.mparams} =
           let
             thisType = Ptr . AsType $ classTypeName cname
             rest = map (translate . A.ptype) mparams
             args = thisType : rest
             retType = future
             f = methodImplFutureName cname mname
           in
             FunctionDecl retType f args

         oneWayMethod A.Method{A.mtype, A.mname, A.mparams} =
           let
             thisType = Ptr . AsType $ classTypeName cname
             rest = map (translate . A.ptype) mparams
             args = thisType : rest
             retType = void
             f = methodImplOneWayName cname mname
           in
             FunctionDecl retType f args

     constructors A.Class{A.cname, A.cmethods} = [ctr]
       where
         ctr =
           let
             retType = Ptr. AsType $ classTypeName cname
             f = constructorImplName cname
           in
             FunctionDecl retType f []

commentSection :: String -> CCode Toplevel
commentSection s = Embed $ (replicate (5 + length s) '/') ++ "\n// " ++ s
