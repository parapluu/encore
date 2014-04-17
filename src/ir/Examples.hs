module Examples(examples) where
import AST
import PrettyPrinter


-- Table of exported example programs 
examples :: [(String, Program)]
examples =
    [
     ("hello", hello),
     ("sumTo", sumTo), 
     ("pingPong", pingPong),
     ("ring", ring)
    ]


-- Hello World
hello :: Program
hello = Program [Class "Main"
                     [] $
                     mkMethods
                      [("main", "Object", [],
                         Print (StringLiteral "Hello, World!"))]]

-- Create an instance of a class and call a method on it
sumTo :: Program
sumTo = Program [Class "Driver"
                   []
                   [Method "sumTo" "Int" [Param ("Int", "n")] 
                    $ IfThenElse (Binop AST.EQ (VarAccess "n") (IntLiteral 0))
                         (IntLiteral 0)
                       $ Let "rest" (Call (VarAccess "this") "sumTo" [Binop AST.MINUS (VarAccess "n") (IntLiteral 1)])
                         $ Binop AST.PLUS (VarAccess "n") (VarAccess "rest")],
                   Class "Main" 
                    (mkFields
                     [("foo", "Foo"), ("bar", "Bar")])
                    [Method "main" "Object" []
                       $ Let "driver" (New "Driver")
                        $ Call (VarAccess "driver") "sumTo" [IntLiteral 5]]]

-- Create two actors that communicate with eachother
pingPong :: Program
pingPong = Program [Class "PingPong"
                        (mkFields [("friend", "PingPong")])
                        (mkMethods
                         [("ping", "void", [("int", "n")],
                            IfThenElse (Binop AST.GT (VarAccess "n") (IntLiteral 0))
                                (Seq
                                 [Print (StringLiteral "Ping!"),
                                  Call (VarAccess "friend") "pong" [VarAccess "n"]])
                              (Print (StringLiteral "Done!"))),
                          ("pong", "void", [("int", "n")],
                             Seq
                              [Print (StringLiteral "Pong!"),
                               Call (VarAccess "friend") "ping" [Binop AST.MINUS (VarAccess "n") (IntLiteral 1)]]),
                          ("setFriend", "void", [("PingPong", "friend")],
                            (Assign (LField (VarAccess "this") "friend")
                                    (VarAccess "friend")))]),
                   Class "Main"
                     []
                     [Method "main" "Object" []
                      $ Let "pinger" (New "PingPong")
                       $ Let "ponger" (New "PingPong")
                        $ Seq
                         [Call (VarAccess "pinger") "setFriend" [VarAccess "ponger"],
                          Call (VarAccess "ponger") "setFriend" [VarAccess "pinger"],
                          Call (VarAccess "pinger") "ping" [IntLiteral 5]]]]

-- Set up a ring of actors that send a message around one time
ring :: Program
ring = Program [Class "Actor"
                    (mkFields [("id", "int"), ("friend", "Actor")])
                    (mkMethods 
                     [("setId", "void", [("int", "id")],
                        Assign (LField (VarAccess "this") "id") (VarAccess "id")),
                      ("setFriend", "void", [("Actor", "friend")],
                        Assign (LField (VarAccess "this") "friend") (VarAccess "friend")),
                      ("send", "void", [("int", "id")],
                        IfThenElse (Binop AST.EQ (VarAccess "id") (FieldAccess (VarAccess "this") "id"))
                            (Print (StringLiteral "Done!"))
                          (Seq [Print (FieldAccess (VarAccess "this") "id"),
                                Print (StringLiteral ": Passing token "),
                                Print (VarAccess "id"),
                                Call (FieldAccess (VarAccess "this") "friend") "send" [VarAccess "id"]])),
                      ("start", "void", [],
                        Call (FieldAccess (VarAccess "this") "friend") "send" [FieldAccess (VarAccess "this") "id"])]),
                   Class "Main"
                     (mkFields [("a1", "Actor"),("a2", "Actor"),("a3", "Actor"),("a4", "Actor"),("a5", "Actor")])
                     [Method "main" "Object" []
                        $ Seq
                         [Assign (LField (VarAccess "this") "a1") (New "Actor"),
                          Assign (LField (VarAccess "this") "a2") (New "Actor"),
                          Assign (LField (VarAccess "this") "a3") (New "Actor"),
                          Assign (LField (VarAccess "this") "a4") (New "Actor"),
                          Assign (LField (VarAccess "this") "a5") (New "Actor"),
                          Call (FieldAccess (VarAccess "this") "a1") "setFriend" [FieldAccess (VarAccess "this") "a2"],
                          Call (FieldAccess (VarAccess "this") "a2") "setFriend" [FieldAccess (VarAccess "this") "a3"],
                          Call (FieldAccess (VarAccess "this") "a3") "setFriend" [FieldAccess (VarAccess "this") "a4"],
                          Call (FieldAccess (VarAccess "this") "a4") "setFriend" [FieldAccess (VarAccess "this") "a5"],
                          Call (FieldAccess (VarAccess "this") "a5") "setFriend" [FieldAccess (VarAccess "this") "a1"],
                          Call (FieldAccess (VarAccess "this") "a1") "start" []]]]
