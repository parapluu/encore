{-|

Some example programs written in "AST.AST" form. Used for testing.

-}

module Examples(examples) where
import Identifiers
import AST.AST
import AST.PrettyPrinter


-- Table of exported example programs 
examples :: [(String, Program)]
examples =
    [
     ("hello", Program [Class (Type "Main")
                        [] $
                        mkMethods
                        [(Name "main", Type "void", [],
                               Print (Type "string") (StringLiteral "Hello Ponyworld!"))]]),
     ("countdown", Program [Class (Type "Main")
                                [Field (Name "count") (Type "int")] $
                                mkMethods
                                [(Name "main", Type "void", [],
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
                  [Method (Name "main") (Type "void") []
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
           (mkMethods [((Name "main"), (Type "void"), [],
                        (Seq [
                          Let (Name "other") (Type "Other") (New $ Type "Other")
                          (Seq [Call (VarAccess $ Name "other") (Name "init") [IntLiteral 10],
                                Call (VarAccess $ Name "other") (Name "work") []])]))]),

       Class (Type "Other")
           (mkFields [(Name "count", Type "int")])
           (mkMethods [(Name "init", Type "void", [(Name "va", Type "int")],
                        (Assign (LField (VarAccess $ Name "this") $ Name "count") (VarAccess $ Name "va"))),
                       (Name "work", Type "void", [],
                             Seq (take 3 (repeat $ decrementField (Name "this") $ Name "count") ++
                                  [Print (Type "int") (FieldAccess (VarAccess $ Name "this") $ Name "count"),
                                   Print (Type "string") (StringLiteral "Hello Ponyworld!")]))])]),
     ("stringSend",
      Program [
       Class (Type "Main")
           (mkFields [])
           (mkMethods [((Name "main"), (Type "void"), [],
                        (Seq [
                          Let (Name "other") (Type "Other") (New $ Type "Other")
                          (Seq [Call (VarAccess $ Name "other") (Name "init")
                                         [StringLiteral "Hello Ponyworld-message"],
                                Call (VarAccess $ Name "other") (Name "work") []])]))]),

       Class (Type "Other")
           (mkFields [(Name "message", Type "string")])
           (mkMethods [(Name "init", Type "void", [(Name "va", Type "string")],
                        (Assign
                         (LField (VarAccess $ Name "this") $ Name "message")
                         (VarAccess $ Name "va"))),
                       (Name "work", Type "void", [],
                             Print (Type "string") (FieldAccess (VarAccess $ Name "this") $ Name "message"))])]),

     ("actorSend",
      Program [
       Class (Type "Main")
           (mkFields [])
           (mkMethods [((Name "main"), (Type "void"), [],
                        (Seq [
                          Let (Name "other") (Type "Other") (New $ Type "Other") $
                          Let (Name "another") (Type "Other") (New $ Type "Other")
                          (Seq [Call (VarAccess $ Name "other") (Name "init")
                                         [VarAccess $ Name "another"],
                                Call (VarAccess $ Name "other") (Name "work") []])]))]),

       Class (Type "Other")
           (mkFields [(Name "other", Type "Other")])
           (mkMethods [(Name "init", Type "void", [(Name "va", Type "Other")],
                        (Assign
                         (LField (VarAccess $ Name "this") $ Name "other")
                         (VarAccess $ Name "va"))),
                       (Name "work", Type "void", [],
                             Seq $
                                 [(Print
                                   (Type "Other")
                                   (FieldAccess (VarAccess $ Name "this") $ Name "other")),
                                 Let (Name "othertmp")
                                     (Type "Other")
                                     (FieldAccess (VarAccess $ Name "this") $ Name "other") $
                                     Seq $
                                         [Call (VarAccess $ Name "othertmp") (Name "print") [],
                                          Print (Type "string") (StringLiteral "sent")]]),
                       (Name "print", Type "void", [],
                             Print (Type "string") (StringLiteral "Hello Actorworld!"))])]),

     ("twoArgs",
      Program [
       Class (Type "Main")
           (mkFields [])
           (mkMethods [((Name "main"), (Type "void"), [],
                        (Seq [
                          Let (Name "other") (Type "Other") (New $ Type "Other") $
                          (Call (VarAccess $ Name "other") (Name "printSum") [
                                     (IntLiteral 10),
                                     (IntLiteral 20)])]))]),

       Class (Type "Other")
           []
           (mkMethods [(Name "printSum", Type "void", [(Name "a", Type "int"),
                                                       (Name "b", Type "int")],
                        Print (Type "int") (Binop PLUS (VarAccess $ Name "a") (VarAccess $ Name "b")))])])
    ]

decrementField ovar fname = Assign (LField (VarAccess ovar) fname)
                            (Binop MINUS (FieldAccess (VarAccess ovar) fname) (IntLiteral 1))


