module Examples where
import AST
import PrettyPrinter

hello :: Program
hello = Program [Class "Main"
                     []
                     (mkMethods
                      [("main", "Object", [],
                         Print (StringLiteral "Hello, World!"))])]

sumTo :: Program
sumTo = Program [Class "Driver"
                   []
                   [Method "sumTo" "Int" [Param ("Int", "n")] 
                    (IfThenElse (Binop AST.EQ (VarAccess "n") (IntLiteral 0))
                     (IntLiteral 0)
                     (Let "rest" (Call (VarAccess "this") "sumTo" [Binop AST.MINUS (VarAccess "n") (IntLiteral 1)])
                      (Binop AST.PLUS (VarAccess "n") (VarAccess "rest"))))],
                   (Class "Main" 
                    (mkFields
                     [("foo", "Foo"), ("bar", "Bar")])
                    (mkMethods
                     [("main", "Object", [],
                       Let "driver" (New "Driver")
                        (Call (VarAccess "driver") "sumTo" [IntLiteral 5]))]))]

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
                            (Seq
                              [Print (StringLiteral "Pong!"),
                               Call (VarAccess "friend") "ping" [Binop AST.MINUS (VarAccess "n") (IntLiteral 1)]])),
                          ("setFriend", "void", [("PingPong", "friend")],
                            (Assign (LField (VarAccess "this") "friend")
                                    (VarAccess "friend")))]),
                   Class "Main"
                     []
                     (mkMethods
                      [("main", "Object", [],
                           (Let "pinger" (New "PingPong")
                             (Let "ponger" (New "PingPong")
                               (Seq
                                 [Call (VarAccess "pinger") "setFriend" [VarAccess "ponger"],
                                  Call (VarAccess "ponger") "setFriend" [VarAccess "pinger"],
                                  Call (VarAccess "pinger") "ping" [IntLiteral 5]]))))])]