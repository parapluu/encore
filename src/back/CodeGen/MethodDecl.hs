{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| Makes @MethodDecl@ (see "AST") an instance of @Translatable@ (see "CodeGen.Typeclasses") -}
module CodeGen.MethodDecl(translate) where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.Expr ()
import CodeGen.Closure
import CodeGen.Task
import CodeGen.ClassTable
import CodeGen.Type(runtimeType)
import CodeGen.Function(returnStatement, translateLocalFunctions)
import qualified CodeGen.Context as Ctx
import qualified CodeGen.GC as Gc
import CodeGen.DTrace

import CCode.Main
import Data.List (intersect)
import Data.Either(isLeft, isRight)

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding(void)
import Control.Arrow ((&&&), (>>>), arr)

instance Translatable A.MethodDecl (A.ClassDecl -> ProgramTable -> [CCode Toplevel]) where
  -- | Translates a method into the corresponding C-function
  translate mdecl cdecl table =
    -- this code uses the chain of responsibility pattern to decouple
    -- methods from processing functions.
    let pipelineFn = arr (translateGeneral mdecl cdecl table) >>>
                         methodImplWithFuture mdecl cdecl     >>>
                         methodImplOneWay mdecl cdecl table   >>>
                         methodImplStream mdecl cdecl table
    in pipelineFn []

encoreRuntimeTypeParam = (Ptr (Ptr ponyTypeT), encoreRuntimeType)

initialiseMethodDecl cname = [(Ptr (Ptr encoreCtxT), encoreCtxVar),
                              (Ptr . AsType $ classTypeName cname, thisVar),
                              encoreRuntimeTypeParam]

translateGeneral mdecl@(A.Method {A.mbody, A.mlocals})
                 cdecl@(A.Class {A.cname}) table code
  | A.isStreamMethodDecl mdecl =
    let args = initialiseMethodDecl cname ++
               (stream, streamHandle) : zip argTypes argNames
        streamCloseStmt = Statement $
          Call streamClose [encoreCtxVar, streamHandle]
    in
      code ++ (return $ Concat $ locals ++ closures ++ tasks ++
               [Function void name args
                 (Seq [parametricMethodTypeVars, extractTypeVars,
                       bodys, streamCloseStmt])])
  | otherwise =
    let returnType = translate mType
        args = initialiseMethodDecl cname ++
               if A.isMainMethod cname mName && null argNames
               then [(array, Var "_argv")]
               else zip argTypes argNames
    in
      code ++ (return $ Concat $ locals ++ closures ++ tasks ++
               [Function returnType name args
                 (Seq [dtraceMethodEntry thisVar mName argNames
                      ,parametricMethodTypeVars
                      ,extractTypeVars
                      ,bodys
                      ,dtraceMethodExit thisVar mName
                      ,returnStatement mType bodyn])])
  where
      mName = A.methodName mdecl
      localNames = map (ID.qLocal . A.functionName) mlocals
      localized  = map (localize cname (A.methodName mdecl)) mlocals
      localHeaders = map A.funheader localized
      localCNames = map localFunctionNameOf localized
      newTable = withLocalFunctions localNames localHeaders localCNames table
      locals = translateLocalFunctions newTable localized
      mType = A.methodType mdecl
      name = methodImplName cname (A.methodName mdecl)
      (encArgNames, encArgTypes) =
          unzip . map (A.pname &&& A.ptype) $ A.methodParams mdecl
      argNames = map (AsLval . argName) encArgNames
      argTypes = map translate encArgTypes
      subst = [(ID.thisName, thisVar)] ++
        varSubFromTypeVars typeVars ++
        zip encArgNames argNames
      ctx = Ctx.new subst newTable
      ((bodyn,bodys),_) = runState (translate mbody) ctx
      mTypeVars = A.methodTypeParams mdecl
      typeVars = Ty.getTypeParameters cname
      extractTypeVars = Seq $ assignTypeVar <$> typeVars <*> [Nothing]
      parametricMethodTypeVars = Seq $ zipWith assignTypeVar mTypeVars (Just <$> [0..])
      assignTypeVar ty e =
        let fName = typeVarRefName ty
        in Assign (Decl (Ptr ponyTypeT, AsLval fName))
           (case e of
              Just i -> (ArrAcc i encoreRuntimeType)
              Nothing -> getVar fName)
      getVar name =
        (Deref $ Cast (Ptr . AsType $ classTypeName cname) thisVar)
        `Dot`
        name
      closures = map (\clos -> translateClosure clos typeVars newTable)
                     (reverse (Util.filter A.isClosure mbody))
      tasks = map (\tas -> translateTask tas newTable) $
                  reverse $ Util.filter A.isTask mbody

      localize cls prefix fun =
        let oldName = A.functionName fun
            newName = ID.Name $ Ty.getId cls ++ "_" ++
                                show prefix ++ "_" ++
                                show oldName
        in A.setFunctionName newName fun

methodImplWithFuture m cdecl@(A.Class {A.cname}) code
  | A.isActive cdecl ||
    A.isShared cdecl =
    let retType = future
        fName = methodImplFutureName cname mName
        args = initialiseMethodDecl cname ++ zip argTypes argNames
        fBody = Seq $
           parametricMethodTypeVars :
           map assignTypeVar (Ty.getTypeParameters cname) ++
           assignFut :
           Gc.ponyGcSendFuture argPairs ++
           msg ++ [retStmt]
    in code ++ [Function retType fName args fBody]
  | otherwise = code
  where
    mName = A.methodName m
    mParams = A.methodParams m
    mTypeParams = A.methodTypeParams m
    mType = A.methodType m
    assignTypeVar ty =
        let fName = typeVarRefName ty
        in Assign (Decl (Ptr ponyTypeT, AsLval fName)) $ getVar fName
    getVar name =
        (Deref $ Cast (Ptr . AsType $ classTypeName cname) thisVar)
        `Dot`
        name
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    futVar = Var "_fut"
    declFut = Decl (future, futVar)
    futureMk mtype = Call futureMkFn [AsExpr encoreCtxVar,
                                      runtimeType mtype]
    assignFut = Assign declFut $ futureMk mType
    argPairs = zip (map A.ptype mParams) argNames
    msg = expandMethodArgs (sendFutMsg cname) m
    retStmt = Return futVar
    mTypeVars = A.methodTypeParams m
    parametricMethodTypeVars = Seq $ zipWith assignTypeVarMethod mTypeVars [0..]
    assignTypeVarMethod ty i =
      let fName = typeVarRefName ty
      in Assign (Decl (Ptr ponyTypeT, AsLval fName))
                (ArrAcc i encoreRuntimeType)

methodImplOneWay m cdecl@(A.Class {A.cname}) _ code
  | A.isActive cdecl ||
    A.isShared cdecl =
      let retType = void
          fName = methodImplOneWayName cname mName
          args = initialiseMethodDecl cname ++ zip argTypes argNames
          extractedTypeVars = map assignTypeVar (Ty.getTypeParameters cname)
          fBody = Seq $ parametricMethodTypeVars : extractedTypeVars ++
                        Gc.ponyGcSendOneway argPairs ++ msg
      in code ++ [Function retType fName args fBody]
  | otherwise = code
  where
    mName = A.methodName m
    mParams = A.methodParams m
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams

    argPairs = zip (map A.ptype mParams) argNames
    msg = expandMethodArgs (sendOneWayMsg cname) m

    -- extract method type vars
    mTypeVars = A.methodTypeParams m
    parametricMethodTypeVars = Seq $ zipWith assignTypeVarMethod mTypeVars [0..]
    assignTypeVarMethod ty i =
      let fName = typeVarRefName ty
      in Assign (Decl (Ptr ponyTypeT, AsLval fName))
                (ArrAcc i encoreRuntimeType)

    -- extract class type vars
    assignTypeVar ty =
        let fName = typeVarRefName ty
        in Assign (Decl (Ptr ponyTypeT, AsLval fName)) $ getVar fName
    getVar name =
        (Deref $ Cast (Ptr . AsType $ classTypeName cname) thisVar)
        `Dot`
        name

methodImplStream m cdecl@(A.Class {A.cname}) _ code
  | A.isStreamMethodDecl m =
    let retType = stream
        fName = methodImplStreamName cname mName
        args = initialiseMethodDecl cname ++ zip argTypes argNames
        fBody = Seq $ [assignFut] ++ Gc.ponyGcSendStream argPairs ++
                      msg ++ [retStmt]
    in code ++ [Function retType fName args fBody]
  | otherwise = code
  where
    mName = A.methodName m
    mParams = A.methodParams m
    mType = A.methodType m
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    retVar = Var "_stream"
    declVar = Decl (stream, retVar)
    streamMk mtype = Call streamMkFn [encoreCtxVar]
    assignFut = Assign declVar $ streamMk mType
    argPairs = zip (map A.ptype mParams) argNames
    msg = expandMethodArgs (sendStreamMsg cname) m
    retStmt = Return retVar

expandMethodArgs :: forall t.
                    (ID.Name -> [CCode Name] -> [CCode Name] -> t) ->
                    A.MethodDecl -> t
expandMethodArgs fn mdecl =
  fn (A.methodName mdecl)
     (map (argName . A.pname) (A.methodParams mdecl))
     (map typeVarRefName $ A.methodTypeParams mdecl)

mkArgPairs :: [CCode Name] -> [CCode Name] -> [(CCode Name, CCode Name)]
mkArgPairs args tparams =
    let fields = [Nam $ "f" ++ show i | i <- [1..length args]]
    in zip fields args ++ zip tparams tparams

sendFutMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Name] -> [CCode Stat]
sendFutMsg cname mname args tparams =
  let
    (msgId, msgTypeName) = (uncurry futMsgId &&& uncurry futMsgTypeName) (cname, mname)
    argPairs = mkArgPairs args tparams ++ [(Nam "_fut", Nam "_fut")]
  in
    sendMsg cname mname msgId msgTypeName argPairs

sendOneWayMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Name] -> [CCode Stat]
sendOneWayMsg cname mname args tparams =
  let
    (msgId, msgTypeName) = (uncurry oneWayMsgId &&& uncurry oneWayMsgTypeName) (cname, mname)
    argPairs = mkArgPairs args tparams
  in
    sendMsg cname mname msgId msgTypeName argPairs

sendStreamMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Name] -> [CCode Stat]
sendStreamMsg cname mname args tparams =
  let
    (msgId, msgTypeName) = (uncurry futMsgId &&& uncurry futMsgTypeName) (cname, mname)
    argPairs = mkArgPairs args tparams ++ [(Nam "_fut", Nam "_stream")]
  in
    sendMsg cname mname msgId msgTypeName argPairs

sendMsg :: Ty.Type -> ID.Name -> CCode Name -> CCode Name
  -> [(CCode Name, CCode Name)]
  -> [CCode Stat]
sendMsg cname mname msgId msgTypeName argPairs = [
  assignMsg
  , initMsg
  , sendMsg
  ]
  where
    msgType = AsType msgTypeName
    msgTypePtr = Ptr msgType
    msgName = "msg"
    declMsg = Decl (msgTypePtr, Var msgName)
    msgIdExpr = AsExpr. AsLval $ msgId
    msgSizeIndex = Call poolIndexName [Sizeof msgType]
    allocMsg = Call ponyAllocMsgName [msgSizeIndex, msgIdExpr]
    castAllocMsg = Cast msgTypePtr allocMsg
    assignMsg = Assign declMsg castAllocMsg

    assignMsgFields =
      [Assign (Var msgName `Arrow` f) rhs | (f,rhs) <- argPairs]
    initMsg = Seq assignMsgFields

    target = Cast (Ptr ponyActorT) $ thisVar
    msgArg = Cast (Ptr ponyMsgT) $ Var msgName
    sendMsg = Statement $
      Call ponySendvName [AsExpr $ Deref encoreCtxVar, target, msgArg]