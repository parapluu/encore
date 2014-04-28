module Examples(examples) where
import AST
import PrettyPrinter


-- Table of exported example programs 
examples :: [(String, Program)]
examples =
    [
     ("hello", Program [Class (Type "Main")
                        [] $
                        mkMethods
                        [(Name "main", Type "Object", [],
                               Print (Type "string") (StringLiteral "Hello Ponyworld!"))]]),
     ("countdown", Program [Class (Type "Main")
                                [Field (Name "count") (Type "int")] $
                                mkMethods
                                [(Name "main", Type "Object", [],
                                       Seq $
                                           Print (Type "string") (StringLiteral "Hello Ponyworld!") :
                                       (take 5 $ repeat $ Assign
                                                 (LField (VarAccess $ Name "this") $ Name "count")
                                                 (Binop MINUS (FieldAccess (VarAccess $ Name "this") $ Name "count") (IntLiteral 1))) ++
                                       [Print (Type "int") (FieldAccess (VarAccess $ Name "this") $ Name "count")])]]),
     ("theOthers",
                 Program
                 [Class (Type "Main")
                  [] -- no fields
                  [Method (Name "main") (Type "Object") []
                   (Seq [
                     Print (Type "string") (StringLiteral "Hello Ponyworld!"),
                     Let (Name "other") (Type "Other") (New $ Type "Other")
                                            (Seq [
                                              Call (VarAccess $ Name "other") (Name "init") [],
                                              Call (VarAccess $ Name "other") (Name "work") []])])],
                  Class (Type "Other")
                      (mkFields [(Name "count", Type "int")])
                      (mkMethods [(Name "init", Type "void", [],
                                   (Assign
                                    (LField (VarAccess $ Name "this") $ Name "count")
                                    (IntLiteral 3))),
                                  (Name "work", Type "void", [],
                                        Seq (take 3 (repeat $ decrementField (Name "this") $ Name "count") ++
                                             [Print (Type "int") (FieldAccess (VarAccess $ Name "this") $ Name "count"),
                                              Print (Type "string") (StringLiteral "Hello Ponyworld!")]))])]),
     ("primitiveSend",
      Program [
       Class (Type "Main")
           (mkFields [])
           (mkMethods [((Name "main"), (Type "Object"), [],
                        (Seq [
                          Let (Name "other") (Type "Other") (New $ Type "Other")
                          (Seq [Call (VarAccess $ Name "other") (Name "init") [IntLiteral 10],
                                Call (VarAccess $ Name "other") (Name "work") []])]))]),

       Class (Type "Other")
           (mkFields [(Name "count", Type "int")])
           (mkMethods [(Name "init", Type "void", [(Type "int", Name "va")],
                        (Assign (LField (VarAccess $ Name "this") $ Name "count") (VarAccess $ Name "va"))),
                       (Name "work", Type "void", [],
                             Seq (take 3 (repeat $ decrementField (Name "this") $ Name "count") ++
                                  [Print (Type "int") (FieldAccess (VarAccess $ Name "this") $ Name "count"),
                                   Print (Type "string") (StringLiteral "Hello Ponyworld!")]))])]),
     ("stringSend",
      Program [
       Class (Type "Main")
           (mkFields [])
           (mkMethods [((Name "main"), (Type "Object"), [],
                        (Seq [
                          Let (Name "other") (Type "Other") (New $ Type "Other")
                          (Seq [Call (VarAccess $ Name "other") (Name "init")
                                         [StringLiteral "Hello Ponyworld-message"],
                                Call (VarAccess $ Name "other") (Name "work") []])]))]),

       Class (Type "Other")
           (mkFields [(Name "message", Type "string")])
           (mkMethods [(Name "init", Type "void", [(Type "char*", Name "va")],
                        (Assign
                         (LField (VarAccess $ Name "this") $ Name "message")
                         (VarAccess $ Name "va"))),
                       (Name "work", Type "void", [],
                             Print (Type "string") (FieldAccess (VarAccess $ Name "this") $ Name "message"))])])]

decrementField ovar fname = Assign (LField (VarAccess ovar) fname)
                            (Binop MINUS (FieldAccess (VarAccess ovar) fname) (IntLiteral 1))


