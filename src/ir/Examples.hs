module Examples(examples) where
import AST
import PrettyPrinter


-- Table of exported example programs 
examples :: [(String, Program)]
examples =
    [
     ("hello", hello),
     ("countdown", countdown),
     ("theOthers", theOthers),
     ("primitiveSend", primitiveSend)
    ]

-- Hello World
hello :: Program
hello = Program [Class (Type "Main")
                     [] $
                     mkMethods
                      [(Name "main", Type "Object", [],
                        Print (StringLiteral "Hello Ponyworld!"))]]

-- Hello World
countdown :: Program
countdown =
  Program [Class (Type "Main")
           [Field (Name "count") (Type "int")] $
           mkMethods
           [(Name "main", Type "Object", [],
             Seq $
             Print (StringLiteral "Hello Ponyworld!") :
             (take 5 $ repeat $ Assign (LField (VarAccess $ Name "this") $ Name "count")
              (Binop MINUS (FieldAccess (VarAccess $ Name "this") $ Name "count") (IntLiteral 1))) ++
             [Print (FieldAccess (VarAccess $ Name "this") $ Name "count")]
            )]]

-- Create an instance of a class and call a method on it
sumTo :: Program
sumTo = Program [Class (Type "Driver")
                   []
                   [Method (Name "sumTo") (Type "int") [Param (Type "int", Name "n")] 
                    $ IfThenElse (Binop AST.EQ (VarAccess $ Name "n") (IntLiteral 0))
                         (IntLiteral 0)
                       $ Let (Name "rest") (Type "int") (Call (VarAccess $ Name "this") (Name "sumTo") [Binop AST.MINUS (VarAccess $ Name "n") (IntLiteral 1)])
                         $ Binop AST.PLUS (VarAccess $ Name "n") (VarAccess $ Name "rest")],
                   Class (Type "Main")
                    (mkFields
                     [(Name "foo", Type "Foo"), (Name "bar", Type "Bar")])
                    [Method (Name "main") (Type "Object") []
                       $ Let (Name "driver") (Type "Driver") (New $ Type "Driver")
                        $ Call (VarAccess $ Name "driver") (Name "sumTo") [IntLiteral 5]]]

theOthers = Program
            [Class (Type "Main")
             [] -- no fields
             [Method (Name "main") (Type "Object") []
              (Seq [
                Print (StringLiteral "Hello Ponyworld!"),
                Let (Name "other") (Type "Other") (New $ Type "Other") (
                     Seq [
                        Call (VarAccess $ Name "other") (Name "init") [],
                        Call (VarAccess $ Name "other") (Name "work") []
                        ]
                     )
                    ])],
             Class (Type "Other")
             (mkFields [(Name "count", Type "int")])
             (mkMethods [(Name "init", Type "void", [],
                          (Assign (LField (VarAccess $ Name "this") $ Name "count") (IntLiteral 3))),
                         (Name "work", Type "void", [], Seq (
                             take 3 (repeat $ decrementField (Name "this") $ Name "count") ++
                             [
                               Print (FieldAccess (VarAccess $ Name "this") $ Name "count"),
                               Print (StringLiteral "Hello Ponyworld!")]))])
            ]

decrementField ovar fname = Assign (LField (VarAccess ovar) fname)
                            (Binop MINUS (FieldAccess (VarAccess ovar) fname) (IntLiteral 1))


-- mkMethods :: [(Name, Type, [(Type, Name)], Expr)] -> [MethodDecl]
primitiveSend = Program [
                 Class (Type "Main")
                 (mkFields [])
                 (mkMethods [((Name "main"), (Type "Object"), [],
                             (Seq [
                               Let (Name "other") (Type "Other") (New $ Type "Other")
                               (Seq [Call (VarAccess $ Name "other") (Name "init") [IntLiteral 10],
                                     Call (VarAccess $ Name "other") (Name "work") []])
                    ]))]),

                 Class (Type "Other")
                     (mkFields [(Name "count", Type "int")])
                     (mkMethods [(Name "init", Type "void", [(Type "int", Name "va")],
                          (Assign (LField (VarAccess $ Name "this") $ Name "count") (VarAccess $ Name "va"))),
                         (Name "work", Type "void", [], Seq (
                             take 3 (repeat $ decrementField (Name "this") $ Name "count") ++
                             [
                               Print (FieldAccess (VarAccess $ Name "this") $ Name "count"),
                               Print (StringLiteral "Hello Ponyworld!")]))])

                ]

-- Set up a ring of actors that send a message around one time
--ring :: Program
--ring = Program [Class (Type "Actor")
--                    (mkFields [(Name "id", Type "int"), (Name "friend", Type "Actor")])
--                    (mkMethods 
--                     [(Name "setId", Type "void", [(Type "int", Name "id")],
--                        Assign (LField (VarAccess $ Name "this") $ Name "id") (VarAccess $ Name "id")),
--                      (Name "setFriend", Type "void", [(Type "Actor", Name "friend")],
--                        Assign (LField (VarAccess $ Name "this") $ Name "friend") (VarAccess $ Name "friend")),
--                      (Name "send", Type "void", [(Type "int", Name "id")],
--                        IfThenElse (Binop AST.EQ (VarAccess $ Name "id") (FieldAccess (VarAccess $ Name "this") $ Name "id"))
--                            (Print (StringLiteral "Done!"))
--                          (Seq [Print (FieldAccess (VarAccess $ Name "this") $ Name "id"),
--                                Print (StringLiteral ": Passing token "),
--                                Print (VarAccess $ Name "id"),
--                                Call (FieldAccess (VarAccess $ Name "this") $ Name "friend") (Name "send") [VarAccess $ Name "id"]])),
--                      (Name "start", Type "void", [],
--                        Call (FieldAccess (VarAccess $ Name "this") $ Name "friend") (Name "send") [FieldAccess (VarAccess $ Name "this") $ Name "id"])]),
--                   Class (Type "Main")
--                   (mkFields [(Name "a1", Type "Actor"),
--                              (Name "a2", Type "Actor"),
--                              (Name "a3", Type "Actor"),
--                              (Name "a4", Type "Actor"),
--                              (Name "a5", Type "Actor")])
--                     [Method (Name "main") (Type "Object") []
--                        $ Seq
--                         [Assign (LField (VarAccess (Name "this")) (Name "a1")) (New (Type "Actor")),
--                          Assign (LField (VarAccess (Name "this")) (Name "a2")) (New (Type "Actor")),
--                          Assign (LField (VarAccess (Name "this")) (Name "a3")) (New (Type "Actor")),
--                          Assign (LField (VarAccess (Name "this")) (Name "a4")) (New (Type "Actor")),
--                          Assign (LField (VarAccess (Name "this")) (Name "a5")) (New (Type "Actor")),
--                          Call (FieldAccess (VarAccess (Name "this")) (Name "a1")) (Name "setFriend") [FieldAccess (VarAccess (Name "this")) (Name "a2")],
--                          Call (FieldAccess (VarAccess (Name "this")) (Name "a2")) (Name "setFriend") [FieldAccess (VarAccess (Name "this")) (Name "a3")],
--                          Call (FieldAccess (VarAccess (Name "this")) (Name "a3")) (Name "setFriend") [FieldAccess (VarAccess (Name "this")) (Name "a4")],
--                          Call (FieldAccess (VarAccess (Name "this")) (Name "a4")) (Name "setFriend") [FieldAccess (VarAccess (Name "this")) (Name "a5")],
--                          Call (FieldAccess (VarAccess (Name "this")) (Name "a5")) (Name "setFriend") [FieldAccess (VarAccess (Name "this")) (Name "a1")],
--                          Call (FieldAccess (VarAccess (Name "this")) (Name "a1")) (Name "start") []]]]
