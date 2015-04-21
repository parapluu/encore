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
  class Foo:
    def boo():void {
		print "^-^"
    }
  
}|

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
in the include path) can be imported using @code{import A.B.C}.

As of now the module system has no notion of name spaces so all imported objects needs to have unique names.
There is also no support for cyclic imports and no "include guards" so it's up to the programmer 
to ensure that each file is only imported once.

@subsection{Standard Library}

Encore supports a standard library, which is currently stored in the @code{bundles} directory
of the @code{git} hierarchy, but in the future will be available in the directory @code{.encore/bundles}
in a user's home directory.

The @code{bundles} directory contains three subdirectories. Directory @code{standard} includes
stable library functionality. Directory @code{prototype} contains experimental and unstable
libraries. These libraries should all be sub-bundles of the @code{Proto} bundle to remind
the programmer of their status. Finally, directory @code{joy} contains bundles obtained
through the @code{enjoy} package manager (functionality to be implemented).

@;@section{Keywords}
@;Keywords are special words used by the Encore language and they have special meaning.
@;In Encore, we have the following keywords:
@;
@;@tabular[#:sep @hspace[4]]{
@;  @list[
@;    (list "passive" "class" "def" "let" "in" "if" "unless" "then" "else")
@;    (list "and" "or" "not" "while" "get" "null" "true" "false" "new")
@;    (list "embed" "body" "end" "Fut" "Par" "import" "qualified" "module" "")]}
@;
@;
@;@section{Literals}
@;Literals allow the programmer to represent data with some semantics.
@;In Encore, you have the following literals:
@;
@;@itemlist[
@;  @item{boolean values @verbatim|{true false null}|}
@;  @item{numeric values @verbatim{int real}}
@;  @item{strings}
@;]

@section{Operators}
Operators are special tokens that apply operations on expressions.

The following tokens are operators:

@tabular[#:sep @hspace[5]]{
  @list[ (list "not" "and" "or" "<" ">" "<=" ">=")
         (list "==" "!=" "+" "-" "*" "/" "%")]
}

@;{
@section{Types}

void
string
int
real
bool
null
Fut
Par

@codeblock|{
  class Person:
    name: string
    age: int
    def init(name: string, age: int){
      self.name = name;
      self.age = age;
  }
}|

}

@section{Primitive Types}
The available primitive types and example literals for them are:

@tabular[#:sep @hspace[5]
(list
  (list @code{string} @code{"hello"})
  (list @code{real}   @code{1.234, -3.141592})
  (list @code{int}    @code{1, -12})
  (list @code{uint}   @code{42})
  (list @code{bool}   @code{true, false})
  (list @code{void}   @code{()})
)]

@section{Loops}
There are @code{while} and @code{repeat} loops.

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

@codeblock|{
2
3
4
}|