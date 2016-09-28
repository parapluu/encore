#lang scribble/manual

@title{Basics}

Encore uses active classes which are classes with asynchronous method
call semantics in order to achieve parallelism-by-default. This section gives
a concise overview of encore's classes. The @secref{syntax} and
@secref{semantics} sections will describe the language in much greater
detail.
 
@section{Hello, World!}

The obligatory hello-world program, @tt{hello.enc}, looks like this:

@codeblock|{
    class Main
      def main() : void {
        print("Hello, World!")
      }
}|

To compile and run the program just run @tt{encorec -run hello.enc}.

@codeblock|{
    $ encorec -run hello.enc
    Hello, World
}|

It is important to know that every legal @tt{encore} program needs to
have an active class called @code{Main}, with a method called
@code{main}. The runtime will allocate one object of class @code{Main}
and begin execution in its @code{main} method.

@section{Compiler options}

Running @code{encorec foo.enc} will typecheck the source and produce the executable 
@code{foo}. The following options are supported:

@itemlist[
@item{@code{-c} -- Keep intermediate C-files}
@item{@code{-tc} -- Typecheck only (don't produce an executable)}
@item{@code{-o [file]} -- Specify output file}
@item{@code{-run} -- Run the program and remove the executable}
@item{@code{-clang} -- Use clang to build the executable (default)}
@item{@code{-AST} -- Output the parsed AST as text to foo.AST}
@item{@code{-TypedAST} -- Output the typechecked AST as text to foo.TAST}
@item{@code{-nogc} -- Disable the garbage collection of passive objects.}
@item{@code{-help} -- Prints a help message and then exits.}
@item{@code{-I path1:path2:...} -- Directories in which to look for modules. (Not needed for modules which are in the current folder.)}
@item{@code{-F [flags]} -- Provides additional flags to the C compiler. Nessesary when using some C libraries. Use quatitionmarks to add more than one.}
]

For instance, we might want to keep the intermediate C-files:

@codeblock|{
  encorec -c foo.enc
}|

@section[#:tag "active-passive-classes"]{Active/Passive Classes}

@tt{Encore} implements active and passive classes. Allocating an object from those classes gives active or passive objects, 
respectively. To make a class active, no special keyword is necessary, as classes are active by default: @code{class A}. 
To make a class passive, use @code{passive class P}. 

Calling a method on an active object will have that method execute concurrently (also in parallel, if enough cores are available). 
The return type of an active method is a future (see @secref{futures}) and not the return type in the method declaration. 
Active classes are the default in @tt{encore}.

In contrast to active classes passive classes executes methods synchronously, in the calling thread. Passive objects are similar to 
what you'd expect in a Java-like language.

Note that all fields in an active class are private but all fields are public in a passive class.

A class may have a method called @code{init} which is used as the constructor. It is called when appending arguments to object 
construction (@code{new Foo(42)}) and can not be called on its own. Note that there (currently) is a difference between @code{new Foo} 
and @code{new Foo()}. The first one only creates the object, while the latter one creates the object and immediately calls the @code{init} 
method on it.

@subsection{Example: Active Class}

We write an active class @code{Foo}. A foo has an ID string, calling the @code{printID} method will print that string 5 times to @code{stdout}. 
The main method  creates two @code{Foo} instances, and have them print their IDs

@codeblock|{
    class Main
      def main() : void
        let obj1 = new Foo
            obj2 = new Foo
        in {
          obj1.setID("obj1");
          obj2.setID("obj2");
          obj1.printID();
          obj2.printID();
          ()
        }
    
    class Foo
      id : string
    
      def setID(new_id : string) : void {
        this.id = new_id;
        ()
      }
    
      def printID() : void {
        let i = 0 in {
          while i < 5 {
            i = i + 1;
            print(this.id)
          }
        }
      }
}|

@margin-note{The behavior of this program can be made deterministic using @tt{get}. See @secref{futures} for more information.}
Executing this program gives nondeterministic output:
@verbatim{
    $ encorec -run ex_active.enc
    obj2
    obj1
    obj2
    obj1
    obj2
    obj1
    obj2
    obj1
    obj2
    obj1
}


@subsection{Example: Passive Class}

A passive class is declared using the @code{passive} keyword. Note the use of a constructor (the @code{init} method):

@codeblock|{
    #! /usr/bin/env encorec -run
    --ex_passive.enc
    passive class Location
      x : int
      y : int
      label : string
    
      def init(new_x:int, new_y:int, new_label:string) : void {
        this.x = new_x;
        this.y = new_y;
        this.label = new_label
      }
    
    class Main
      loc : Location
    
      def main() : void {
        this.loc = new Location(1, 2, "a place");
        print this.loc.x;
        print this.loc.y;
        print this.loc.label
      }
}|
Running:
@verbatim{
    $ ./ex_passive.enc
    1
    2
    a place
}
