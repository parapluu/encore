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
import CodeGen.Function(returnStatement)
import qualified CodeGen.Context as Ctx
import qualified CodeGen.GC as Gc

import CCode.Main
import Data.List (intersect)

import qualified AST.AST as A
import qualified AST.Util as Util
import qualified Identifiers as ID
import qualified Types as Ty

import Control.Monad.State hiding(void)
import Control.Arrow ((&&&), (>>>), arr)

instance Translatable A.MethodDecl (A.ClassDecl -> ProgramTable -> [CCode Toplevel]) where
  -- | Translates a method into the corresponding C-function
  translate mdecl@(A.Method {A.mbody}) cdecl@(A.Class {A.cname}) table =
    -- this code uses the chain of responsibility pattern to decouple
    -- methods from processing functions.
    let pipelineFn = (arr $ translateGeneral mdecl cdecl table)    >>>
                            methodImplWithFuture mdecl cdecl       >>>
                            methodImplOneWay mdecl cdecl table     >>>
                            methodImplStream mdecl cdecl table
    in pipelineFn []

translateGeneral mdecl@(A.Method {A.mbody}) cdecl@(A.Class {A.cname}) table code
      | A.isStreamMethod mdecl =
    let args = (Ptr (Ptr encoreCtxT), encoreCtxVar) :
               (Ptr . AsType $ classTypeName cname, Var "_this") :
               (stream, streamHandle) : zip argTypes argNames
        streamCloseStmt = Statement $
          Call streamClose [encoreCtxVar, streamHandle]
    in
      code ++ (return $ Concat $ closures ++ tasks ++
               [Function void name args
                 (Seq [extractTypeVars, bodys, streamCloseStmt])])
      | otherwise =
    let returnType = translate mType
        args = (Ptr (Ptr encoreCtxT), encoreCtxVar) :
               (Ptr . AsType $ classTypeName cname, Var "_this") :
               if A.isMainMethod cname mName && null argNames
               then [(array, Var "_argv")]
               else zip argTypes argNames
    in
      code ++ (return $ Concat $ closures ++ tasks ++
               [Function returnType name args
                 (Seq [extractTypeVars, bodys, returnStatement mType bodyn])])
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


methodImplWithFuture m cdecl@(A.Class {A.cname}) code
  | A.isActive cdecl ||
    A.isShared cdecl =
    let retType = future
        fName = methodImplFutureName cname mName
        args = (Ptr (Ptr encoreCtxT), encoreCtxVar) : this : zip argTypes argNames
        fBody = Seq $
           runtimeTypeAssignment mType :
           assignFut :
           Gc.ponyGcSendFuture argPairs ++
           msg ++ [retStmt]
    in code ++ [Function retType fName args fBody]
  | otherwise = code
  where
    thisName = "_this"
    mName = A.methodName m
    mParams = A.methodParams m
    mType = A.methodType m
    runtimeTypeAssignment mtype
      | Ty.isTypeVar mtype =
          Assign (Decl (Ptr ponyTypeT, AsLval $ typeVarRefName mtype))
                 (Arrow (Nam "_this") (typeVarRefName mType))
      | otherwise = Skip
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    this = (Ptr . AsType $ classTypeName cname, Var thisName)
    futVar = Var "_fut"
    declFut = Decl (future, futVar)
    futureMk mtype = Call futureMkFn [AsExpr encoreCtxVar,
                                      runtimeType mtype]
    assignFut = Assign declFut $ futureMk mType
    argPairs = zip (map A.ptype mParams) argNames
    msg = sendFutMsg cname mName $ map (argName . A.pname) mParams
    retStmt = Return futVar

methodImplOneWay m cdecl@(A.Class {A.cname}) _ code
  | A.isActive cdecl ||
    A.isShared cdecl =
      let retType = void
          fName = methodImplOneWayName cname mName
          args = (Ptr (Ptr encoreCtxT), encoreCtxVar): this :
                   zip argTypes argNames
          fBody = Seq $ Gc.ponyGcSendOneway argPairs ++ msg
      in code ++ [Function retType fName args fBody]
  | otherwise = code
  where
    thisName = "_this"
    mName = A.methodName m
    mParams = A.methodParams m
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    this = (Ptr . AsType $ classTypeName cname, Var thisName)

    argPairs = zip (map A.ptype mParams) argNames
    msg = sendOneWayMsg cname mName $ map (argName . A.pname) mParams

methodImplStream m cdecl@(A.Class {A.cname}) _ code
  | A.isStreamMethod m =
    let retType = stream
        fName = methodImplStreamName cname mName
        args = (Ptr (Ptr encoreCtxT), encoreCtxVar) : this : zip argTypes argNames
        fBody = Seq $ [assignFut] ++ Gc.ponyGcSendStream argPairs ++
                      msg ++ [retStmt]
    in code ++ [Function retType fName args fBody]
  | otherwise = code
  where
    thisName = "_this"
    mName = A.methodName m
    mParams = A.methodParams m
    mType = A.methodType m
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    this = (Ptr . AsType $ classTypeName cname, Var thisName)
    retVar = Var "_stream"
    declVar = Decl (stream, retVar)
    streamMk mtype = Call streamMkFn [encoreCtxVar]
    assignFut = Assign declVar $ streamMk mType
    argPairs = zip (map A.ptype mParams) argNames
    msg = sendStreamMsg cname mName $ map (argName . A.pname) mParams
    retStmt = Return retVar

sendFutMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Stat]
sendFutMsg cname mname args =
  let
    msgId = futMsgId cname mname
    msgTypeName = futMsgTypeName cname mname
    fields = [Nam $ "f" ++ show i | i <- [1..length args]]
    argPairs = zip fields args ++ [(Nam "_fut", Nam "_fut")]
  in
    sendMsg cname mname msgId msgTypeName argPairs

sendOneWayMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Stat]
sendOneWayMsg cname mname args =
  let
    msgId = oneWayMsgId cname mname
    msgTypeName = oneWayMsgTypeName cname mname
    fields = [Nam $ "f" ++ show i | i <- [1..length args]]
    argPairs = zip fields args
  in
    sendMsg cname mname msgId msgTypeName argPairs

sendStreamMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Stat]
sendStreamMsg cname mname args =
  let
    msgId = futMsgId cname mname
    msgTypeName = futMsgTypeName cname mname
    fields = [Nam $ "f" ++ show i | i <- [1..length args]]
    argPairs = zip fields args ++ [(Nam "_fut", Nam "_stream")]
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
    thisName = "_this"
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

    target = Cast (Ptr ponyActorT) $ Var thisName
    msgArg = Cast (Ptr ponyMsgT) $ Var msgName
    sendMsg = Statement $
      Call ponySendvName [AsExpr $ Deref encoreCtxVar, target, msgArg]
