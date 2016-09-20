#lang scribble/manual

@(require scribble/core)

@title[#:tag "syntax"]{Syntax}
This section contains information about the grammar, keywords and literals of the
Encore language.


@include-section{grammar.scrbl}

@section{Module system}
As of now encore supports a rudimentary module system. The keyword used to import
modules is @code{import}.

Here follows a trivial example of usage of the module system.

File  @code{Lib.enc}:
@codeblock|{
  bundle Lib where

  class Foo:
    def boo():void {
		print "^-^"
    }

}|

Line @code{bundle Lib where} declares the module name. This line is optional, though desirable
for library code.

File  @code{Bar.enc}:
@codeblock|{
  import Lib

  class Main:
    def main():void {
	  let
		f = new Foo
	  in{
		f.boo();
	  }
    }

}|

Here the file @code{Bar.enc} imports @code{Lib.enc} and can thus access the class @code{Foo}.

To import files from different directories one needs to use the @code{-I path} argument for the compiler.

Modules are hierarchical. Module @code{A.B.C} (in some directory @code{A/B/C.enc}
in the include path) is declared using @code{bundle A.B.C where}  and  imported using @code{import A.B.C}.

As of now the module system has no notion of name spaces so all imported objects needs to have unique names.
There is also no support for cyclic imports and no "include guards" so it's up to the programmer
to ensure that each file is only imported once.

@subsection{Standard Library}

Encore supports a standard library, which is currently stored in
the @code{bundles} directory of the @code{git} hierarchy, but in
the future will be available in the directory
@code{.encore/bundles} in a user's home directory.

The @code{bundles} directory contains three subdirectories.
Directory @code{standard} includes stable library functionality.
Directory @code{prototype} contains experimental and unstable
libraries. These libraries should all be sub-bundles of the
@code{Proto} bundle to remind the programmer of their status.
Finally, directory @code{joy} contains bundles obtained through
the @code{enjoy} package manager (functionality to be
implemented).

@section{Operators}
Operators are special tokens that apply operations on expressions.

The following tokens are operators:

@tabular[#:sep @hspace[5]]{
  @list[ (list "not" "and" "or" "<" ">" "<=" ">=")
         (list "==" "!=" "+" "-" "*" "/" "%")]
}


@section{Type Synonyms}

Type synonyms allow abbreviations to be used for types.
These take one of the two forms, where a single name can be
used to abbreviate another type, or where a name has parameters.

@codeblock|{
  typedef Fint = Foo<int,int>
}|
or
@codeblock|{
  typedef F<x,y> = Foo<y,x>
}|

The types @code{Fint} and @code{F<type1,type2>} can be used in code as synonyms
for @code{Foo<int,int>} and @code{Foo<type2,type1>}, respectively.

Type synonyms cannot be recursive.


@section{Comments}
@;{TODO: Figure out how to write '{' and '}' inside @code}
In Encore you have one-liner comments and block comments.
One-liner comments are written by using @code{--} while block comments
are written starting with {- followed by the text of the comment
and closing the comment with -}. For instance:


@codeblock|{
-- this expression prints numbers from 0 to 5
let i = 0 in
  {-
     this loop iterates 5 times
   -}
  while i < 5 {
    print("i={}\n",i)
    i = i+1
  }
}|


@section{Primitive Types}
The available primitive types and example literals for them are:

@tabular[#:sep @hspace[5]
(list
  (list @code{char}   @tt{'a'})
  (list @code{real}   @code{1.234, -3.141592})
  (list @code{int}    @code{1, -12})
  (list @code{uint}   @code{42})
  (list @code{bool}   @code{true, false})
  (list @code{void}   @code{()})
)]

While not technically a primitive type, there is special support
for the @code{String} type. Strings can be created using the
normal syntax @code{"Hello world!"} (which is just short-hand for
@code{new String("Hello world!")}). The @code{String} class in
@code{bundles/standard} provides basic operations on strings.

@section{Loops}
There are @code{while}, @code{repeat} and @code{for} loops.

A while loop takes a boolean loop condition, and evaluates its body
expression repeatedly -- as long as the loop condition evaluates to
true:

@codeblock|{
let i = 0 in
  while i < 5 {
    print("i={}\n",i)
    i = i+1
  }
}|

This prints:

@verbatim{
i=0
i=1
i=2
i=3
i=4
}

The @code{repeat} look is syntax sugar that makes iterating over
integers simpler. The following example is equivalent to the @code{while} loop above:

@codeblock|{
repeat i <- 5
  print("i={}\n",i)
}|

Encore for loops can iterate over ranges of integers with uniform
strides and arrays. They generalise @code{repeat} loops. To use
@code{for} loops to iterate over the values 1 through 10
(inclusive), we can write:

@codeblock|{
for i in [1..10]
  print i
}|

This prints:

@verbatim{
1
2
3
...
10
}

For loops can be given an optional stride length using the @code{by}
keyword. To modify the example above to iterate in strides of 3,
we can write:

@codeblock|{
for i in [1..10] by 3
  print i
}|

This prints:

@verbatim{
1
4
7
10
}

Arrays can be sources of values for @code{for} loops. In the code
below, let @code{arr} be an array @code{[1,2,3,4,5,6,7,8,9,10]}.
The following two code snippets are equivalent to the two code
snippets above. So, this prints 1 through 10 on the terminal:

@codeblock|{
for i in arr
  print i
}|

And this prints 1, 3, 7, and 10 on the terminal:

@codeblock|{
for i in arr by 3
  print i
}|

It is possible to loop over non-literal ranges as well. Notably,
if @code{rng} is the range @code{[1..10 by 3]} then

@codeblock|{
for i in rng
  print i
}|

prints

@verbatim|{
1
3
7
10
}|

and

@codeblock|{
for i in rng by 2
  print i
}|

prints

@verbatim|{
1
7
}|



@section{Arrays}
For each type @code{T} there is a corresponding array type
@code{[T]}. For example, @code{[int]} is the type of arrays of
integers. You create a new array by writing @code{new [T](n)},
where @code{n} is the length of the array. An array has a fixed
size and can not be dynamically extended or shrunk.

You access an array by using standard bracket notation
@code{a[i]}. This is also how assignment into arrays is written.
You get the size of an array by putting the array within bars
@code{|a|}. You can also create array literals by writing a comma
separated list of expressions within brackets @code{[1, 2, 1+2]}.
This short example uses all features of arrays:

@codeblock|{
class Main
  def bump(arr : [int]) : void
      repeat i <- |arr|
        arr[i] = arr[i] + 1

  def main() : void{
    let a = [1,2,3] in {
      this.bump(a);
      repeat i <- |a|
        print a[i];
      let b = new [int](3) in {
        b[0] = 0;
        b[1] = a[0];
        b[2] = 42 - 19;
      }
    }
  }
}|

The expected output is

@verbatim|{
2
3
4
}|


@section{Ranges}

Ranges are Encore objects which are currently only useful for
iterating over using @code{for} loops. There are currently no
surface-level operations on ranges. Ranges may be passed around
like normal objects. Pretending that ranges can be turned into
their array equivalents, we can explain ranges by examples thus
(ranges left, arrays right):

@codeblock|{
[1..5] == [1,2,3,4,5]
[0..4] == [0,1,2,3,4]
[2..6] == [2,3,4,5,6]
[0..100 by 30] == [0,30,60,90]
[0..4, by 2] == [0,2,4]
}|

When ranges are used in @code{for} loops for iteration, they are
optimised out, meaning there is no memory allocation due to a
temporary range object.
