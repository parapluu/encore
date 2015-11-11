#lang scribble/manual

@title[#:tag "semantics"]{Semantics}

This section describes the semantics of the language.




@section{Classes}

Classes in @tt{encore} have fields and methods. There is no
inheritance. A class is either active or passive, all objects of an
active class are active objects with asynchronous method calls and all
objects of a passive class are passive objects with synchronous method
calls.

Every encore program needs a class @tt{Main} with an argument-less,
@tt{void} method @tt{main}, which is the starting point of a program.



@subsection{Active Objects}

Accessing fields of active objects is only possible via the
@code{this} reference, which means that all fields are
private. Passive objects have public fields.

@codeblock|{
#!/usr/bin/env encorec -run
class ActiveFoo
  sum : int

  def get_sum1() : int
    this.sum -- legal

  def get_sum2() : int
    let x = this in
      x.sum -- not legal

...
}|

@subsection{Passive Objects}
Passive objects have public fields:

@codeblock|{
#!/usr/bin/env encorec -run

passive class Foo
  sum : int

  def init(sum_ : int) : void
    this.sum = sum_

class Main
  def main() : void
    let f = new Foo(12) in
      print("f.sum={}\n", f.sum) -- prints "f.sum=12"
}|

@subsection{Parametric Class Polymorphism}

Classes can take type parameters. This allows to implement, for
example, 2-tuples:

@codeblock|{
passive class Pair<a, b>
passive class Pair<a,b>
  fst : a
  snd : b
  def init(fst_ : a, snd_ : b) : void{
    this.fst = fst_;
    this.snd = snd_
  }
}|

We can now use the class like this:

@codeblock|{
class Main
  def main() : void
    let pair = new (Pair int string)(65, "a") in
      print("({},{})\n", pair.fst, pair.snd)
}|
@subsection{Methods}

As methods on active objects run asynchronously, they will not return
the declared return type @code{a} but @code{Fut a} when called on any
variable other than @code{this}. When calling a method on @code{this},
it will run synchronously and therefore not return a future but the
result directly.

@subsubsection{Expression Sequences}

Syntactically, method bodies are a single expression:

@codeblock|{
  def single() : void
    print "a single expression needs no curly braces"

  def curly() : void {
    print ".. but it CAN use them!"
  }
}|

If you desire to run several expressions in sequence, you must use a
@italic{<Sequence>} expression:

@codeblock|{
  def multiple() : int {
      print "multiple";
      print "expressions";
      print "are wrapped by { ... }";
      print "and separated by ';'";
      2
  }

}|

A sequence expression is a number of expressions, separated by
semicolons and wrapped in curly braces. It evaluates to whatever the
last subexpression evaluates.


@section[#:tag "futures"]{Futures}

As mentioned before (@secref{active-passive-classes}),
a method call on an active class is executed asynchronously.
Futures are produced when you call a method on an active class.

You can think about futures as a small data structure that will contain,
eventually, a value when the asynchronous calculation finishes.

@code{Futures} are considered first class citizens. This allows us to pass @code{futures}
to functions, return them from functions and use them as any other data type.

A @code{future} is considered to be @bold{fulfilled} when the asynchronous operation
has been finished and the @code{future} contains the returned value.

In the following subsections you will find a set of operations that can be
performed on futures.

@bold{Limitations}

The current implementation of futures does not allow more than 16 active
objects blocking on a @code{future}.

@subsection[#:tag "get-future"]{@code{get} a value}

As mentioned in the introduction to @secref{futures}, a future data type is returned
whenever you call a method on an active class. Given the following code:

@codeblock[#:line-numbers 1]|{
class Item
  price: int

  ...

  def get_price(): int
    this.price


class Main
  item: Item

  def init(i: Item): Main{
    this.item = i;
    this;
  }

  def execute(): void {
    let fut = item.get_price() in {
      print(get fut);
      print("End payment!");
    }
  }
}|

line @code{19} returns a @code{future} data type.

In order to get the value of the @code{future}, you need to do a call to the @code{get} method (line @code{20}).
@code{get} checks if the value of the future
has been fulfilled. If it is, then returns a value of the expected type,
in our case, an @code{int} (@code{get_price} returns an @code{int}, line @code{6}).
If the @code{future} is not fulfilled, then it blocks the active object and the @code{Main} actor
will not do any progress until the future is fulfilled.

In this case, the statement @tt{"End payment!"} in line @code{21} will always
be printed after the print out of the price.

@subsection{@code{await} execution}

One of the problems of calling @code{get} on a future is the blocking behaviour
of the actor. As we explained in @seclink{get-future}, this will block
the actor until the future is fulfilled. If the method on which
we get the future does a time-consuming calculation, the actor will be
waiting until it finishes and the future is fulfilled.

A better alternative would be to let the actor continue running some
other messages from its message queue and resume the actor when the future
is fulfilled. This is exactly what the @code{await} on a future does.

Let's take a look at the example below:

@codeblock[#:line-numbers 1]|{
  class Main
    def main() : void
      let t = new Test() in
      {
        t ! run1();
        t ! run2();
      }

  class Producer
    def foo() : int
      17

  class Test
    p:Producer
    def init() : void
      this.p = new Producer

    def run2() : void {
      print "While awaiting";
    }

    def run1() : void
      let f = this.p.foo() in
      {
	print "Before await";
        await f;
	print "After await";
	print get f
      }
}|

In this example, the output is:

@codeblock|{
Before await
While awaiting
After await
17
}|

As you can see in the program, @code{Main} executes @code{run1} before
@code{run2}. In the middle of the execution (line @tt{26}) it decides to
@code{await} until the future is fulfilled and therefore, it starts
executing @code{run2}. After finishing @code{run2}, the future has been
fulfilled and @code{run1} continues from where it left off.

@bold{NOTE}

In the current implementation, @code{await} would cause the actor to save the
context and process other messages in the mailbox if the future is not
fulfilled. Otherwise, @code{await} would behave like a no-op. When the future
is fulfilled, the producer (the actor who fulfills the future) would send a
message to awaited actor, who would resume the save context on processing this
message.

@subsection{@code{chain} on a @code{future}}

The semantics of @code{chain} allows you to run a callback as soon as the future that you
chain on is fulfilled. The result of a `chain` operation is another `Future` that will
contain the result of the chained `lambda`. For instance:

@codeblock[#:line-numbers 1]|{
  #!/usr/bin/env encorec -run

  class Producer
    def produce() : int
      42 + 1

  class Main
    def main(): void {
      let p = new Producer in
          l = \ (x:int) -> { print x; x + 1 } in {
        print get p.produce() ~~> l;
      }
    }
}|

@margin-note{In the current implementation, we do not handle cyclic dependencies and they
causes deadlock! We are working on it!}

In the example above, the lambda @code{lambda_fun} will be executed
as soon as the future from @code{p.produce()} (line @code{10}) is fulfilled.


@section{Tasks}

@subsection{async}
Tasks allows the developer to execute a function asynchronously, not
bound to the actor that calls it. For instance, in the following code,
an actor calls a global function `long_computation`, which is executed
by the actor synchronously. This means that the actor will not be able
to continue until the computation has finished.

@codeblock[#:line-numbers 1]|{
def repeat(max_iterations: int, fn: int -> int): void {
  repeat i <- max_iterations
    print fn(i)
}

def inc(x: int): int
  x+1

class Main
  def main(): void
    repeat(300, inc);
}|


Sometimes you don't care who executes the function, you just want to
schedule the function and let someone run it. This is when tasks come
in handy, for situations such as @code{pmap}, @code{foreach}, etc.
The next example re-writes the previous code and makes it run by different
actors, which doesn't hog the actor that calls the global function.


@codeblock[#:line-numbers 1]|{
def p_repeat(max_iterations: int, fn: int -> int): void
  repeat i <- max_iterations
    async(print fn(i))

def inc(x: int): int
  x+1

class Main
  def main(): void
    p_repeat(300, inc)
}|

Instead of calling a `async function`, you can write
statements inside the `async` construct that will be performed as a
block of code. The following example, gets numbers from 0 to 4 and
runs in parallel and asyncronously the quadruple of the given number.
In this case, we do not want to block on the future, but execute the
side effects (printing).

@codeblock[#:line-numbers 1]|{
def square(x: int): int
  x * x

class Main
  def main(): void {
    repeat i <- 5
      async {
        let s = square(i) in
          print square(s)
      }
  }
}|

@subsection{Finish}
There's another feature that can be used with tasks and allows you to use your
typical fork-join parallel construct. By using the keyword @code{finish}
and tasks in its body you are guaranteed that the tasks will be finished
before you leave the body of it.

 E.g.

@codeblock[#:line-numbers 1]|{
class Main
  def main():void {
    finish {
      async {
        -- perform asynchronous computation
      };
    };
    -- at this point, the task is fulfilled
    print "Finish"
  }
}|

In this case, the asynchronous computation is performed before the program
 prints the word @code{Finish}.

In this other example (@code{async_finish.enc} in tests):

@codeblock[#:line-numbers 1]|{
class Main
  def main(): void {
    let f = async{print "Task declared outside finish"} in {
      finish {
        async(print "Running inside finish");
	print 23;
        f
      };
      print "OUT";
    }
  }
}|

the program returns an output similar to this one (due to non-determinism
 the order can change):

@codeblock[#:line-numbers 1]|{
23
Running inside finish
OUT
Task declared outside finish
}|

the important point to notice here is that @code{Task declared outside finish}
is declared outside of the @code{finish} building block and therefore,
it's not guaranteed to be finished before printing @code{OUT}.

@subsection{foreach}
@code{foreach} allows the developer to iterate over an array and execute
the body of the foreach in parallel. It's used for performing parallel
computations with side-effects.

E.g.

@codeblock[#:line-numbers 1]|{
class Main
  def main(): void
    let master = new Master()
        a = [1, 2, 3, 4, 5]
    in
      foreach item in a {
        master.add(item)
      };
}|

In each iteration (line 7) @code{item} is replaced by the value
@code{a[iteration]}. Line 7 is executed in parallel with other iterations
and therefore, uses @code{tasks} behind the scenes. Please remember
that this does not implies that by the end of the @code{foreach} construct
all the tasks have finished!


@section{Fire and forget@code{!}}

In some occasions, when working on an
@seclink["active-passive-classes"]{active class}, you might want to
perform the side-effects of a method call and forget about the
returned value. You could just throw away the future, but more
performant way is not to generate the future in the first place. The
bang@code{!} operator sends a message without creating a future. The
following snippet shows how to perform this action:

@codeblock[#:line-numbers 1]|{
  class ShoppingCart
    item: Item

    def add_item(item: Item): Item {
      this.item = item;
      item;
    }


  class Main
    def main(): void {
      ...
      let cart = ShoppingCart in {
        cart!add_item(item);
        ...
      }
     ...
    }
}|

In line @code{14}, we use the symbol @code{!} to asynchronously
execute the method call to the object represented by @code{item}
without caring about the returned value.

@section{@code{suspend} execution}

The @code{suspend} operator is a cooperative multitasking abstraction
that suspends the current running method on an agent and schedules the
message to be resumed after the current messages in the inbox have
been processed.

@codeblock[#:line-numbers 1]|{
  class Pi
    def calculate_decimals(decimals: int): double {
      -- perform initial calculations
      ...
      suspend;
      -- continue performing more calculations
      ...
    }


  class Main
    def main(): void {
      let pi = Pi in {
        pi.calculate_decimals(100000000000);
      }
    }
}|

Let's assume from the example above that we want to calculate a large
number of decimals of π. The @code{Pi} active object does a method
call to @code{calculate_decimals} (line @code{15}). This method starts
performing some initial calculations and calls on @code{suspend}.
As a result of this call, @code{Pi} will suspend the execution of the current running
method call, place a new message in its message queue (to resume the execution of the running method)
and continue processing other messages. Upon reaching the message
that resumes the execution of the method @code{calculate_decimals}, it
will continue from where he left off.

@section{Parallel Combinators}
Parallel combinators provide high- and low-level coordination of parallel
computations. There are different operators that lift values and futures
into a parallel collection. Other combinators are in charge of performing
low-level coordination of this parallel collection.

@subsection{liftv}
The @code{liftv} combinator lifts a value to a parallel collection. Its
signature is: @code{liftv :: t -> Par t}.

@codeblock[#:line-numbers 1]|{
  let p = liftv 42 in
    p
}|

@subsection{liftf}
The @code{liftf} combinator lifts a value to a parallel collection. Its
signature is: @code{liftf :: Fut t -> Par t}.

@codeblock[#:line-numbers 1]|{
class Card
  def valid(): bool
    return true

class Main
  def main(): void
    let card = new Card in
      liftf(card.valid())
}|

@subsection{||}
The @code{||} combinator (named @code{par}) merges two parallel computations into a single
parallel collection. Its type signature is: @code{|| :: Par t -> Par t -> Par t}.

@codeblock[#:line-numbers 1]|{
class Card
  card: int

  def init(card: int): void
    this.card = card

  def valid(): bool
    return true

class Main
  def main(): void
    let card1 = new Card(1234)
        card2 = new Card(4567)
    in
      liftf(card1.valid()) || liftf(card2.valid())
}|

@subsection{>>}
The @code{>>} combinator (named @code{sequence}) performs a @code{pmap} operation on the
parallel collection. Its type signature is: @code{>> :: Par a -> (a -> b) -> Par b}.

@codeblock[#:line-numbers 1]|{
-- prints if the card is valid
def show(valid: bool): void
  print valid

class Card
  card: int

  def init(card: int): void
    this.card = card

  def valid(): bool
    return true

class Main
  def main(): void
    let card1 = new Card(1234)
        card2 = new Card(4567)
        p = liftf(card1.valid()) || liftf(card2.valid())
    in
      p >> show
}|

@subsection{join}
The @code{join} combinator merges two parallel computations into a single one.
Its type signature is: @code{Par Par t -> Par t}.

@codeblock[#:line-numbers 1]|{
class Main
  def main(): void
    let par = liftv 42  -- :: Par int
        par_par = liftv par  -- :: Par Par in
    in
      (join par_par)  -- :: Par Par int -> Par int
}|

@section{Embedding of C code}

For implementing low level functionality, @tt{encore} allows to
embed trusted C code. There are two kinds of embedded C code:
expressions, and toplevel @code{embed} blocks.

We do @bold{not} advocate to rely on the @code{embed} functionality
too much. Code that uses @code{embed} is highly likely to break with
future updates to the language (even more likely than code that
doesn't use @code{embed}).

@subsection{Toplevel Embed Blocks}

There can be at most one toplevel embed block. It has to be before the
first class definition of a file. It consists of a header section and
an implementation section.

@codeblock|{
  embed
    // the header section
    int sq(int);
  body
    // the implementation section
    int sq(int n) {
      return n*n;
    }
  end
}|

It is useful to define functions or global variables, include
@tt{C}-header files or to define constants.

The header section will end up in a header file that all the class
implementations will include. The implementation section will end up
in a separate C-file. Consequently, if the @code{sq} function would
not be included in the header section, it could not be used later.


@subsection{Embedded expressions}

When embedding an expression, the programmer needs to assign a type to
the expression; @tt{encorec} will assume that this type is
correct. The value an embedded expression evaluates to is the return
value of the last C-statement in the embedded code.

If you want to access local @tt{encore} variables in an
@code{embed} expression, you'll need to wrap them: @literal{#{x}} for
accessing the local variable @code{x}. If you want to access fields,
you don't wrap them, but use C's arrow operator: @code{this->foo} for
@code{this.foo}.

@verbatim{
#! /usr/bin/env encorec -run
embed
  #include<math.h> // for sqrtl
  int64_t sq(int);
body
  int64_t sq(int n) {
    return n*n;
end
  }

class Main
  def main() : void {
    let x = 2 in {
      -- Contrary to the toplevel embed block, an embed expression
      -- also needs to specify a certain type.
      -- In this example, the expression promises to return an int:
      print(embed int sq(#{x}); end);
      -- ..and here it returns a real:
      print(embed real sqrtl(#{x}); end)
  }
}
}

The following table documents how @tt{encore}'s types map to C types:

@tabular[#:sep @hspace[5]
	  (list
	    (list @bold{encore}              @bold{C})
	    (list @code{string}              @tt{char*})
	    (list @code{real}                @tt{double})
 	    (list @code{int}                 @tt{int64_t})
            (list @code{uint}                @tt{uint64_t})
            (list @code{bool}                @tt{int64_t})
	    (list "<any active class type>"  @code{pony_actor_t*})
	    (list "<any passive class type>" @code{CLASSNAME_data*})
	    (list "<a type parameter>"       @code{void*})
	  )
        ]

@section{Formatted printing}

The @code{print} statement allows formatted output:

@codeblock|{
#! /usr/bin/env encorec -run
class Main
  def main() : void {
    let i = 0 in {
      while i < 5 {
        i = i+1;
        print("{} * {} = {}\n", i, i, i*i);
      }
    }
  }
}|

Running:

@verbatim{
    $ ./ex_printing.enc
    1 * 1 = 1
    2 * 2 = 4
    3 * 3 = 9
    4 * 4 = 16
    5 * 5 = 25
}


@section{Anonymous Functions}

Encore has anonymous functions:

@codeblock|{
    let f = \ (i : int) -> 10*i in
      print(f(10)) -- prints 100
}|


The backslash @literal{\} syntax is borrowed from Haskell and is supposed to resemble a lambda. It is followed by a comma separated list of parameter declarations, an arrow @code{->} and an expression that is the function body.

The type of a function is declared similarly. The function @code{f} above has type @code{int -> int}. Multi-argument functions have types such as @code{(int, string) -> bool}.

@codeblock|{
  #! /usr/bin/env encorec -run
  -- ex_lambdas.enc

  passive class Point
    x : int
    y : int

    def scale(f : int -> int) : void {
      this.x = f(this.x);
      this.y = f(this.y);
      ()
    }

  class Main
    def main() : void {
      let p = new Point in {
        p.x = 1;
        p.y = 2;
        -- scale the point up by 10, using an anonymous function:
        let f = \ (i : int) -> 10*i in {
          p.scale(f);
        };
        print(p.x);
        print(p.y)
      }
    }
}|

@section{Polymorphism}

There is limited support for polymorphic methods and
functions. Type variables are written with lower case letters and
do not need to be declared. If there is a type variable in the
return type it must appear somewhere in the types of the
arguments. Here is an example program that uses a polymorphic
function:

@codeblock|{
    class Main
      def main() : void
        let
          apply = \ (f : a -> b, x : a) -> f(x)
        in
          let bump = \ (x : int) -> x + 1 in
            print(apply(bump, 3))
}|
