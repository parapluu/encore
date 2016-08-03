module CodeGen.Shared(generateShared) where

import CCode.Main
import CodeGen.CCodeNames
import CodeGen.Typeclasses
import CodeGen.Function
import CodeGen.ClassTable
import qualified AST.AST as A

-- | Generates a file containing the shared (but not included) C
-- code of the translated program
generateShared :: A.Program -> ClassTable -> FunctionTable -> CCode FIN
generateShared prog@(A.Program{A.functions, A.imports}) ctable ftable =
    Program $
    Concat $
      (LocalInclude "header.h") :

      embeddedCode ++

      -- [commentSection "Shared messages"] ++
      -- sharedMessages ++
      [commentSection "Global functions"] ++
      globalFunctions ++

      [mainFunction]
    where
      allfunctions = A.allFunctions prog

      globalFunctions =
        [translate f ctable ftable | f <- allfunctions] ++
        [globalFunctionWrapper f | f <- allfunctions] ++
        [initGlobalFunctionClosure f | f <- allfunctions]

      embeddedCode = A.traverseProgram embedded prog
        where
          embedded A.Program{A.source, A.etl} =
              [commentSection $ "Embedded Code from " ++ show source] ++
              map (Embed . A.etlbody) etl

      sharedMessages = [msgAllocDecl, msgFutResumeDecl, msgFutSuspendDecl, msgFutAwaitDecl, msgFutRunClosureDecl]
          where
            msgAllocDecl =
               AssignTL (Decl (ponyMsgT, Var "m_MSG_alloc"))
                        (Record [Int 0, Record ([] :: [CCode Expr])])
            msgFutResumeDecl =
               AssignTL (Decl (ponyMsgT, Var "m_resume_get"))
                        (Record [Int 1, Record [encorePrimitive]])
            msgFutSuspendDecl =
               AssignTL (Decl (ponyMsgT, Var "m_resume_suspend"))
                        (Record [Int 1, Record [encorePrimitive]])
            msgFutAwaitDecl =
               AssignTL (Decl (ponyMsgT, Var "m_resume_await"))
                        (Record [Int 2, Record [encorePrimitive, encorePrimitive]])
            msgFutRunClosureDecl =
               AssignTL (Decl (ponyMsgT, Var "m_run_closure"))
                        (Record [Int 3, Record [encorePrimitive, encorePrimitive, encorePrimitive]])

      mainFunction =
          Function (Typ "int") (Nam "main")
                   [(Typ "int", Var "argc"), (Ptr . Ptr $ char, Var "argv")]
                   $ Return encoreStart
          where
            encoreStart =
                Call (Nam "encore_start")
                     [AsExpr $ Var "argc",
                      AsExpr $ Var "argv",
                      Amp (Var "_enc__active_Main_type")]


commentSection :: String -> CCode Toplevel
commentSection s = Embed $ (take (5 + length s) $ repeat '/') ++ "\n// " ++ s
