# Schml A GTLC Compiler
Welcome to the home of a compiler for the Gradually Typed Lambda 
Calculus (GTLC). The compiler is intended to explore how different
gradual typing semantics may be represented at runtime. It is currently
a work in progress, but I am at the level where I can start to
play with the runtime representation of objects freely.

I will post some of the relavant paper here or in the wiki soon.

## Installation

The package is known to install correctly with:
+ Racket 6.1
+ Racket 6.1.1

```bash
git clone git@github.com:akuhlens/schml
cd schml
raco pkg install
```
This package utilizes Typed Racket which will give better performancs for the compiler itself at the cost of
a much longer compile time for the compiler during install. Perhaps it may be better to install in a unused
tab.

## Use as a Library
```racket
(require schml)
(compile string/or/path/to/file)
```

## Use as a Program

Comming soon

## The Language supported
```bnf
Program        ::= Expression
Lambda         ::= (lambda (Formal ...) Maybe-Type Expression)
Expression     ::= Lambda
               |   (let ([ID Maybe-Type Expression] ...) Expression)
               |   (letrec ([ID Maybe-Type Lambda] ...) Expression)
               |   (Expresion ...)
               |   (: Expression Type Maybe-String)
               |   (if Expression Expression Expression)
               |   (Primitive Expression Expression)
               |   (unbox expression)
               |   (box expression)
               |   (set expression expression)
Primitive      ::= + | -  | * | binary-and | binary-or
               |   < | <= | = | >= | >
Type           ::= Dyn | Int | Bool | (-> Type ...)
Maybe-type     ::=    | : type
Maybe-String   ::=    | "[.^"]*"
Formal         ::= ID | [ID : Type]
```

### For more information:
- compiler/README.md : information about how the compiler is constructed
