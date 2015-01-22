#lang scribble/manual

@(require scribble/core)

@title[#:tag "syntax"]{Syntax}
This section contains information about the grammar, keywords and literals of the
Encore language.

@include-section{grammar.scrbl}

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
  @list[ (list "!" "&&" "||" "<" ">" "<=" ">=")
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