{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-|

Translate a @ClassDecl@ (see "AST") to its @CCode@ (see "CCode.Main")
equivalent.

-}

module CodeGen.ClassDecl () where

import CodeGen.Typeclasses
import CodeGen.CCodeNames
import CodeGen.MethodDecl ()
import CodeGen.ClassTable
import CodeGen.Type
import CodeGen.Trace

import CCode.Main
import CCode.PrettyCCode ()

import Data.List

import qualified AST.AST as A
import qualified Identifiers as ID
import qualified Types as Ty

instance Translatable A.ClassDecl (ClassTable -> CCode FIN) where
  translate cdecl ctable
      | A.isActive cdecl = translateActiveClass cdecl ctable
      | A.isShared cdecl = translateSharedClass cdecl ctable
      | otherwise        = translatePassiveClass cdecl ctable

-- | Translates an active class into its C representation. Note
-- that there are additional declarations in the file generated by
-- "CodeGen.Header"
translateActiveClass cdecl@(A.Class{A.cname, A.cfields, A.cmethods}) ctable =
    Program $ Concat $
      (LocalInclude "header.h") :
      [traitMethodSelector ctable cdecl] ++
      [typeStructDecl cdecl] ++
      [runtimeTypeInitFunDecl cdecl] ++
      [tracefunDecl cdecl] ++
      methodImpls ++
      [dispatchFunDecl cdecl] ++
      [runtimeTypeDecl cname]
    where
      methodImpls = map methodImpl cmethods
          where
            methodImpl mdecl = translate mdecl cdecl ctable

typeStructDecl :: A.ClassDecl -> CCode Toplevel
typeStructDecl cdecl@(A.Class{A.cname, A.cfields, A.cmethods}) =
    let typeParams = Ty.getTypeParameters cname in
    StructDecl (AsType $ classTypeName cname) $
               ((encoreActorT, Var "_enc__actor") :
                (map (\ty -> (Ptr ponyTypeT, AsLval $ typeVarRefName ty)) typeParams ++
                   zip
                   (map (translate  . A.ftype) cfields)
                   (map (AsLval . fieldName . A.fname) cfields)))

dispatchFunDecl :: A.ClassDecl -> CCode Toplevel
dispatchFunDecl cdecl@(A.Class{A.cname, A.cfields, A.cmethods}) =
    (Function (Static void) (classDispatchName cname)
     ([(Ptr . Typ $ "pony_actor_t", Var "_a"),
       (Ptr . Typ $ "pony_msg_t", Var "_m")])
     (Seq [Assign (Decl (Ptr . AsType $ classTypeName cname, Var "this"))
                  (Cast (Ptr . AsType $ classTypeName cname) (Var "_a")),
           (Switch (Var "_m" `Arrow` Nam "id")
            (
             taskDispatchClause :
             (if (A.isMainClass cdecl)
              then ponyMainClause :
                   (methodClauses $ filter ((/= ID.Name "main") . A.methodName) cmethods)
              else methodClauses $ cmethods
             ))
            (Statement $ Call (Nam "printf") [String "error, got invalid id: %zd", AsExpr $ (Var "_m") `Arrow` (Nam "id")]))]))
     where
       ponyMainClause =
           (Nam "_ENC__MSG_MAIN",
            Seq $ [Assign (Decl (Ptr $ Typ "pony_main_msg_t", Var "msg")) (Cast (Ptr $ Typ "pony_main_msg_t") (Var "_m")),
                   Statement $ Call ((methodImplName (Ty.refType "Main") (ID.Name "main")))
                                    [(Cast (Ptr $ Typ "_enc__active_Main_t") (Var "_a")),
                                     Call (Nam "_init_argv")
                                          [AsExpr $ (Var "msg") `Arrow` (Nam "argc"),
                                           AsExpr $ (Var "msg") `Arrow` (Nam "argv")]]])

       methodClauses = concatMap methodClause

       methodClause m = (mthdDispatchClause m) :
                         if not (A.isStreamMethod m)
                         then [oneWaySendDispatchClause m]
                         else []

       -- explode _enc__Foo_bar_msg_t struct into variable names
       methodUnpackArguments :: A.MethodDecl -> CCode Ty -> [CCode Stat]
       methodUnpackArguments mdecl msgTypeName =
         zipWith unpack (A.methodParams mdecl) [1..]
           where
             unpack :: A.ParamDecl -> Int -> CCode Stat
             unpack A.Param{A.pname, A.ptype} n = (Assign (Decl (translate ptype, (AsLval . argName $ pname))) ((Cast (msgTypeName) (Var "_m")) `Arrow` (Nam $ "f"++show n)))

       -- TODO: include GC
       -- TODO: pack in encore_arg_t the task, infering its type
       taskDispatchClause :: (CCode Name, CCode Stat)
       taskDispatchClause =
         let tmp = Var "task_tmp"
             taskRunner = Statement $ Call (Nam "task_runner") [Var "_task"]
             decl = Assign (Decl (encoreArgT, tmp)) taskRunner
             futureFulfil = Statement $ Call (Nam "future_fulfil") [AsExpr $ Var "_fut", AsExpr tmp]
             taskFree = Statement $ Call (Nam "task_free") [AsExpr $ Var "_task"]
             traceFuture = Statement $ Call (Nam "pony_traceobject") [Var "_fut", futureTypeRecName `Dot` Nam "trace"]
             traceTask = Statement $ Call (Nam "pony_traceobject") [Var "_task", AsLval $ Nam "NULL"]
         in
         (taskMsgId, Seq $ [unpackFuture, unpackTask, decl] ++
                             [Embed $ "",
                              Embed $ "// --- GC on receiving ----------------------------------------",
                              Statement $ Call (Nam "pony_gc_recv") ([] :: [CCode Expr]),
                              traceFuture,
                              traceTask,
                              Embed $ "//---You need to trace the task env and task dependencies---",
                              Statement $ Call (Nam "pony_recv_done") ([] :: [CCode Expr]),
                              Embed $ "// --- GC on sending ----------------------------------------",
                              Embed $ ""]++
                       [futureFulfil, taskFree])

       mthdDispatchClause mdecl
           | A.isStreamMethod mdecl =
               (futMsgId cname mName,
                Seq $ unpackFuture : args ++
                      gcRecieve ++ [streamMethodCall])
           | otherwise =
               (futMsgId cname mName,
                Seq $ unpackFuture : args ++
                      gcRecieve ++ [methodCall])
           where
             args =
                 methodUnpackArguments mdecl
                 (Ptr . AsType $ futMsgTypeName cname mName)
             gcRecieve  =
                 gcRecv mParams
                 (Statement $ Call (Nam "pony_traceobject")
                                   [Var "_fut",
                                    futureTypeRecName `Dot` Nam "trace"])
             streamMethodCall =
                 Statement $ Call (methodImplName cname mName)
                                  (Var "this" :
                                   Var "_fut" :
                                   map (AsLval . argName . A.pname) mParams)
             methodCall =
                 Statement $
                   Call (Nam "future_fulfil")
                        [AsExpr $ Var "_fut",
                         asEncoreArgT (translate mType)
                         (Call (methodImplName cname mName)
                               (Var "this" :
                                map (AsLval . argName . A.pname) mParams))]
             mName   = A.methodName mdecl
             mParams = A.methodParams mdecl
             mType   = A.methodType mdecl

       oneWaySendDispatchClause mdecl =
           (oneWayMsgId cname mName,
            Seq $ args ++ gcRecieve ++ [methodCall])
           where
             args =
                 methodUnpackArguments mdecl
                 (Ptr . AsType $ oneWayMsgTypeName cname mName)
             gcRecieve =
                 gcRecv mParams
                 (Comm "Not tracing the future in a oneWay send")
             methodCall =
                 Statement $
                   Call (methodImplName cname mName)
                        (Var "this" : map (AsLval . argName . A.pname) mParams)
             mName   = A.methodName mdecl
             mParams = A.methodParams mdecl

       unpackFuture =
         let
           lval = Decl (Ptr (Typ "future_t"), Var "_fut")
           rval = (Cast (Ptr $ encMsgT) (Var "_m")) `Arrow` (Nam "_fut")
         in
           Assign lval rval

       unpackTask =
         let
           lval = Decl (task, Var "_task")
           rval = (Cast (Ptr taskMsgT) (Var "_m")) `Arrow` (Nam "_task")
         in
           Assign lval rval

       gcRecv ps futTrace = [Embed $ "",
                               Embed $ "// --- GC on receive ----------------------------------------",
                               Statement $ Call (Nam "pony_gc_recv") ([] :: [CCode Expr])] ++
                              (map traceEachParam ps) ++
                              [futTrace,
                               Statement $ Call (Nam "pony_recv_done") ([] :: [CCode Expr]),
                               Embed $ "// --- GC on receive ----------------------------------------",
                               Embed $ ""]

       traceEachParam A.Param{A.pname, A.ptype} =
         Statement $ traceVariable ptype $ AsLval $ argName pname

sendFutMsg :: Ty.Type -> ID.Name -> [CCode Name] -> [CCode Stat]
sendFutMsg cname mname args =
  let
    msgId = futMsgId cname mname
    msgTypeName = futMsgTypeName cname mname
    fields = [Nam $ "f" ++ show i | i <- [1..length args]]
    argPairs = zip fields args ++ [(Nam "_fut", Nam "fut")]
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
    sendMsg = Statement $ Call ponySendvName [target, msgArg]

methodImplWithFuture :: Ty.Type -> A.MethodDecl -> CCode Toplevel
methodImplWithFuture cname m =
  let
    retType = future
    fName = methodImplFutureName cname mName
    args = this : zip argTypes argNames
    fBody = Seq $ [assignFut] ++
      gcSend ++
      msg ++
      [retStmt]
  in
    Function retType fName args fBody
  where
    thisName = "_this"
    mName = A.methodName m
    mParams = A.methodParams m
    mType = A.methodType m
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    this = (Ptr . AsType $ classTypeName cname, Var thisName)

    declFut = Decl (future, Var "fut")
    futureMk mtype = Call futureMkFn [runtimeType mtype]
    assignFut = Assign declFut $ futureMk mType

    argPairs = zip (map A.ptype mParams) argNames
    gcSend =
      [Embed $ "",
       Embed $ "// --- GC on sending ---------------------------------",
       ponyGcSend,
       Statement . traceFuture $ Var "fut"] ++
       (map (Statement . uncurry traceVariable) argPairs) ++
      [ponySendDone,
       Embed $ "// --- GC on sending ---------------------------------",
       Embed $ ""]

    msg = sendFutMsg cname mName $ map (argName . A.pname) mParams

    retStmt = Return $ Var "fut"


methodImplOneWay :: Ty.Type -> A.MethodDecl -> CCode Toplevel
methodImplOneWay cname m =
  let
    retType = void
    fName = methodImplOneWayName cname mName
    args = this : zip argTypes argNames
    fBody = Seq $
      gcSend ++
      msg
  in
    Function retType fName args fBody
  where
    thisName = "_this"
    mName = A.methodName m
    mParams = A.methodParams m
    mType = A.methodType m
    argNames = map (AsLval . argName . A.pname) mParams
    argTypes = map (translate . A.ptype) mParams
    this = (Ptr . AsType $ classTypeName cname, Var thisName)

    argPairs = zip (map A.ptype mParams) argNames
    gcSend =
      [Embed $ "",
       Embed $ "// --- GC on sending ---------------------------------",
       ponyGcSend] ++
       (map (Statement . uncurry traceVariable) argPairs) ++
      [ponySendDone,
       Embed $ "// --- GC on sending ---------------------------------",
       Embed $ ""]

    msg = sendOneWayMsg cname mName $ map (argName . A.pname) mParams

constructorImpl :: Ty.Type -> CCode Toplevel
constructorImpl cname =
  let
    retType = translate cname
    fName = constructorImplName cname
    args = []
    fBody = Seq [
      assignThis
      , ret this
      ]
  in
    Function retType fName args fBody
  where
    thisType = Ptr . AsType $ classTypeName cname
    cast = Cast thisType
    this = Var "this"
    declThis = Decl (thisType, this)
    runtimeType = Amp $ runtimeTypeName cname
    create = Call encoreCreateName [runtimeType]
    assignThis = Assign declThis $ cast create
    ret = Return

translateSharedClass cdecl@(A.Class{A.cname, A.cfields, A.cmethods}) ctable =
  Program $ Concat $
    (LocalInclude "header.h") :
    [traitMethodSelector ctable cdecl] ++
    [typeStructDecl cdecl] ++
    [runtimeTypeInitFunDecl cdecl] ++
    [tracefunDecl cdecl] ++
    [constructorImpl cname] ++
    methodImpls ++
    methodWithFutureImpls ++
    methodOneWayImpls ++
    [dispatchFunDecl cdecl] ++
    [runtimeTypeDecl cname]
    where
      methodImpls = map methodDecl cmethods
        where
          methodDecl mdecl = translate mdecl cdecl ctable

      methodWithFutureImpls = map (methodImplWithFuture cname) cmethods

      methodOneWayImpls = map (methodImplOneWay cname) cmethods

-- | Translates a passive class into its C representation. Note
-- that there are additional declarations (including the data
-- struct for instance variables) in the file generated by
-- "CodeGen.Header"
translatePassiveClass cdecl@(A.Class{A.cname, A.cfields, A.cmethods}) ctable =
    Program $ Concat $
      (LocalInclude "header.h") :
      [traitMethodSelector ctable cdecl] ++
      [runtimeTypeInitFunDecl cdecl] ++
      [tracefunDecl cdecl] ++
      methodImpls ++
      [dispatchfunDecl] ++
      [runtimeTypeDecl cname]
    where
      methodImpls = map methodDecl cmethods
          where
            methodDecl mdecl = translate mdecl cdecl ctable
      dispatchfunDecl =
          Function (Static void) (classDispatchName cname)
                   [(Ptr ponyActorT, Var "_a"),
                    (Ptr ponyMsgT, Var "_m")]
                   (Comm "Stub! Might be used when we have dynamic dispatch on passive classes")

traitMethodSelector :: ClassTable -> A.ClassDecl -> CCode Toplevel
traitMethodSelector ctable A.Class{A.cname, A.ccapability} =
  let
    retType = Static (Ptr void)
    fname = traitMethodSelectorName
    args = [(Typ "int" , Var "id")]
    cond = Var "id"
    traitTypes = Ty.typesFromCapability ccapability
    traitMethods = map (`lookupMethods` ctable) traitTypes
    cases = concat $ zipWith (traitCase cname) traitTypes traitMethods
    err = String "error, got invalid id: %d"
    defaultCase = Statement $ Call (Nam "printf") [err, AsExpr $ Var "id"]
    switch = Switch cond cases defaultCase
    body = Seq [ switch, Return Null ]
  in
    Function retType fname args body
  where
    traitCase :: Ty.Type -> Ty.Type -> [A.FunctionHeader] ->
                 [(CCode Name, CCode Stat)]
    traitCase cname tname tmethods =
        let
            methodNames = map A.hname tmethods
            caseNames   = map (msgId tname) methodNames
            caseStmts   = map (Return . methodImplName cname) methodNames
        in
          zip caseNames caseStmts

runtimeTypeInitFunDecl :: A.ClassDecl -> CCode Toplevel
runtimeTypeInitFunDecl A.Class{A.cname, A.cfields, A.cmethods} =
    Function void (runtimeTypeInitFnName cname)
                 [(Ptr . AsType $ classTypeName cname, Var "this"), (Embed "...", Embed "")]
                   (Seq $
                    (Statement $ Decl (Typ "va_list", Var "params")) :
                    (Statement $ Call (Nam "va_start") [Var "params", Var "this"]) :
                    map initRuntimeType typeParams ++
                    [Statement $ Call (Nam "va_end") [Var "params"]])
        where
          typeParams = Ty.getTypeParameters cname
          initRuntimeType ty =
              Assign (Var "this" `Arrow` typeVarRefName ty)
                     (Call (Nam "va_arg") [Var "params", Var "pony_type_t *"])

tracefunDecl :: A.ClassDecl -> CCode Toplevel
tracefunDecl A.Class{A.cname, A.cfields, A.cmethods} =
    case find ((== Ty.getId cname ++ "_trace") . show . A.methodName) cmethods of
      Just mdecl@(A.Method{A.mbody}) ->
          Function void (classTraceFnName cname)
                   [(Ptr void, Var "p")]
                   (Statement $ Call (methodImplName cname (A.methodName mdecl))
                                [Var "p"])
      Nothing ->
          Function void (classTraceFnName cname)
                   [(Ptr void, Var "p")]
                   (Seq $
                    (Assign (Decl (Ptr . AsType $ classTypeName cname, Var "this"))
                            (Var "p")) :
                     map traceField cfields)
    where
      traceField A.Field {A.ftype, A.fname} =
        let var = Var . show $ fieldName fname
            field = Var "this" `Arrow` fieldName fname
            fieldAssign = Assign (Decl (translate ftype, var)) field
        in Seq [fieldAssign, traceVariable ftype var]

runtimeTypeDecl cname =
    (AssignTL
     (Decl (Typ "pony_type_t", AsLval $ runtimeTypeName cname))
           (Record [AsExpr . AsLval $ classId cname,
                    Call (Nam "sizeof") [AsLval $ classTypeName cname],
                    Int 0,
                    Int 0,
                    AsExpr . AsLval $ (classTraceFnName cname),
                    Null,
                    Null,
                    AsExpr . AsLval $ classDispatchName cname,
                    Null,
                    Int 0,
                    Null,
                    Null,
                    Record [AsExpr . AsLval $ traitMethodSelectorName]
                    ]))
