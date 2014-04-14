
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Control.Monad.Reader

type Name = String

data Class = Class {cname :: Name, 
                    fields::[Field], 
                    constructors :: [Constructor], 
                    methods :: [Method]}

data Field = Field {fname :: Name, ftype::Type}

data Constructor = Constructor Method

data Method = Method {mname :: Name, rtype::Type, mbody :: Expr}

type Parameters = [(Type, Name)]

data Type = Type

data Expr = New String Arguments
          | Call {target :: Expr, tmname :: Name, args :: Arguments}
          | Seq [Expr]
          | Assign Lvar Expr

type Arguments = [Expr]

{- following needs a bit of work -}
data Lvar = LVar Name | LField Name | LThisField Name


{- to be moved into a separate file later -}

data CCode = 
     Includes [String]
   | HashDefine String
   | Switch String [CCode]
   | Record [CCode]
   | C [CCode]
   | TypeDef String CCode
   | SEMI          -- need to get rid of this
   | Embed String  -- for C code that doesn't match other patterns


render (Includes l) = mapM (\i -> line $ "#include " ++ i) l
render (HashDefine s) = wrap1 $ "#define " ++ s
render (C c) = fmap concat $ mapM render c
render (Switch s cases) =
         do
           head <- wrap1 $ "switch (" ++ s ++ ")"
           body <- block (fmap concat $ mapM render cases)
           return $ head ++ body
render (Record c) = block $ fmap concat $ fmap commas $ mapM render c
render (TypeDef s c) = wrap1 "typedef " <~> render c <~> wrap1 (" " ++ s  ++ ";")
render SEMI =  wrap1 ";"
render (Embed s) = wrap1 s

comma [] = []
comma [a] = [a ++ ","]
comma (a:as) = a : comma as

commas [] = []
commas [a,b] = [comma a, b]
commas (a:as) = (comma a : commas as)

spaces n = take n $ repeat ' '
line s = do
     n <- ask
     return $ spaces n ++ s
indent = local (\i -> i + 2) 

block :: MonadReader Int m => m [String] -> m [String]
block c = 
        do
           s <- wrap1 "{" 
           b <- indent c
           f <- wrap1 "}"
           return $ s ++ b ++ f


wrap1 :: MonadReader Int m => String -> m [String]
wrap1 s = 
  do
    l <- line s
    return [l]

a <~ b = do { a' <- a; return $ a' ++ [b] }

a <~> b = do 
              a' <- a
              b' <- b
              return $ a' ++ b'

{-

My approximation

class PingPong 
  PingPong other;

  PingPong() {
    other = new PingPong(this);
  }

  PingPong(PingPoing other) {
    this.other = other;
  }

  void ping() {
    print("Ping");
    other.pong();    
  }

  void pong() {
    print("Pong");
    other.ping();    
  }

Another attempt

}

-}

--- general includes
includes = 
  C [HashDefine "__STDC_FORMAT_MACROS",
     Includes ["<pony/pony.h>", "<stdlib.h>", "<stdio.h>", "<string.h>", 
               "<inttypes.h>", "<assert.h>"] ]

{-
  a data type, using Haskell notation
  data PingPong = Ping | Pong
-}
pingpong_datatype =
  TypeDef "actortype" 
   (Embed "enum { PING, PONG }")

{- 
   record underlying of pingpong data type.
-}
pingpong_rep = 
  TypeDef "pingpong_t" 
    (Embed "struct pingpong_t \
      \{\n\
      \  pony_actor_t* next;\n\
      \  actortype type;\n\
      \}")


-- Generic forward declarations
forward_decls = C 
  [Embed "static void trace(void* p);",
   Embed "static pony_msg_t* message_type(uint64_t id);",
   Embed "static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);"]

actor_type = C 
  [Embed "static pony_actor_type_t type = ",
   Record [
          Embed "1",
          Record [Embed "trace", 
                  Embed "sizeof(pingpong_t)", 
                  Embed "PONY_ACTOR"],
          Embed "message_type",
          Embed "dispatch"], SEMI]

{- Message definitions and handling
   consists of several sections: 
   (1) an enumerated type consisting of an entry for each new message
   (2) a declaration of a message type descriptor for each message, presumably
       giving the number of parameters and their types
   (3) a mapping from the enumerated types to the corresponding message types
   (4) a method body for each of these, which will at present be embedded within
       the main dispatch routing, until something more sophisticated is developed.
-}
messages_enum = Embed "\
\enum\n\
\{\n\
\  MSG_INIT,\n\
\  MSG_PASS,\n\
\};\n"

message_decl = C 
 [Embed "static pony_msg_t m_init = {2, {{NULL, 0, PONY_ACTOR}}};",
  Embed "static pony_msg_t m_pass = {1, {{NULL, 8, PONY_PRIMITIVE}}};"]

message_type = Embed "\n\
\static pony_msg_t* message_type(uint64_t id)\n\
\{\n\
\  switch(id)\n\
\  {\n\
\    case MSG_INIT: return &m_init;\n\
\    case MSG_PASS: return &m_pass;\n\
\  }\n\
\\n\
\  return NULL;\n\
\}\n"

{- This is the initialiser for the instance of the Ping class.
   It is run after the constructor, so that the other Ping instance
   can be created. The second argument determines whether the
   current instance is a pinger or ponger, according to the
   algebraic datatype PingPong. This feature is not used.

  void init(Ping other, PingPong type) {
     this.next = other;
     this.type = type;
  }
    
  note that the initialisation part of this code probably belongs
  elsewhere ... it seems that we've split the initialisaton
  into two, part of it in PONY_MAIN, and part here.
-}
method_msg_init = Embed "\
\    case MSG_INIT:\n\
\    {\n\
\      d = pony_alloc(sizeof(pingpong_t));\n\
\      d->next = argv[0].p;\n\
\      d->type = argv[1].i;\n\
\      pony_set(d);\n\
\      break;\n\
\    }\n\
\ \n"

{- This is the core method from the Ping class.
void pass(int n) {
  if (n > 0) {
    println("Received " + n);
    next.pass(n - 1);   // next is a field
  }
}
-}
method_msg_pass = Embed "\
\    case MSG_PASS:\n\
\    {\n\
\      if(argv[0].i > 0)\n\
\      {\n\
\        printf(\"Received %ld\\n\", argv[0].i);\n\
\        pony_sendi(d->next, MSG_PASS, argv[0].i - 1);\n\
\      }\n\
\      break;\n\
\    }\n"

-- This seems to be the body of pony's main function, but the code is
-- unforunately placed in the middle of our actor. 
-- This particular body creates several actors, sends each of
-- them an initialisation method, and then sends one of them
-- a specific method
-- Why is this in the same dispatch function as the others?????
method_pony_main = Embed "\
\    case PONY_MAIN:\n\
\    {\n\
\      pony_actor_t* ping = pony_create(&type);\n\
\      pony_actor_t* pong = pony_create(&type);\n\ 
\ \n\
\      pony_arg_t ping_args[2] = { {.p = pong} , {.i = PING} };\n\
\      pony_arg_t pong_args[2] = { {.p = ping} , {.i = PONG} };\n\
\ \n\
\      pony_sendv(ping, MSG_INIT, 2, ping_args);\n\
\      pony_sendv(pong, MSG_INIT, 2, pong_args);\n\
\ \n\
\      pony_sendi(ping, MSG_PASS, 500);\n\
\      break;\n\
\    }\n"


dispatch = C [Embed $ "\
\static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)\n\
\{\n\
\  pingpong_t* d = p;",

   Switch "id" [
      method_pony_main,
      method_msg_init, 
      method_msg_pass ],
      Embed "}"]



-- Main function
mainf = Embed "\n\
\int main(int argc, char** argv)\n\
\{\n\
\  return pony_start(argc, argv, pony_create(&type));\n\
\}\n"


-- Util functions
trace = Embed "\
\static void trace(void* p)\n\
\{\n\
\  pingpong_t* d = p;\n\
\  pony_traceactor(&d->next);\n\
\}\n"


usage = Embed "\
\static void usage()\n\
\{\n\
\  printf(\n\
\    \"pingpong\"\n\
\    );\n\
\}\n"



codegen = 
    render includes <~>
    render pingpong_datatype <~>
    render pingpong_rep <~>
    render messages_enum <~>
    render forward_decls <~>
    render actor_type <~>
    render message_decl <~>
    render trace <~>
    render message_type <~>
    render usage <~>
    render dispatch <~>
    render mainf

main = mapM_ putStrLn . runReader codegen $ 0