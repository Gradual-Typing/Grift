# Grift: A GTLC Compiler

Welcome to the home of a compiler for the Gradually Typed Lambda 
Calculus (GTLC). The compiler is intended to explore how different
gradual typing semantics may be represented at runtime. It is currently
a work in progress, but I am at the level where I can start to
play with the runtime representation of objects freely.

I will post some of the relavant paper here or in the wiki soon.

## Installation

```bash
git clone git@github.com:akuhlens/grift
cd grift
raco pkg install
```

## Use as a Library
```racket
(require grift)
(compile string/or/path/to/file)
```

## Use as a Program

Comming soon

## The Gradually Typed Lambda Calculus
```bnf
Lambda         ::= (lambda (Formal ...) [: Type] Expression)
Application    ::= (Expression ...)
Ascription     ::= (: Expression Type [String])
Conditional    ::= (if Expression Expression Expression)
Primitives     ::= (Primitive Expression ...)
References     ::= (unbox expression)
               |   (box expression)
               |   (box-set! expression expression)
Binding        ::= (let ((ID [: Type] Expression) ...) Expression)
               |   (letrec ((ID [: Type] Lambda) ...) Expression)
Iteration      ::= (repeat (ID Expression Expression) Expression)
Primitive      ::= + | -  | * | binary-and | binary-or
               |   < | <= | = | >= | >
               | timer-start | timer-stop | timer-report
Type           ::= Dyn | Int | Bool | Unit | (-> Type ...) | (Ref Type)
Maybe-[A]      ::=    | A
Formal         ::= ID | [ID : Type]
```
In order to support more accurate benchmarking we have added the repeat
special form which is guaranteed to be the equivalent of a for loop in
C. The loop ```(repeat (id start stop) exp)``` is the equivalent of
the for loop ```for(id = start; id < stop; id += 1) exp;```. It always
results in a unit value.

Further support for benchmarking has been added with the timer primitives.
All of which take no arguments and return unit. This is a global
one shot timer. Calling (timer-report) will print out the time in seconds
between the calls (timer-start) and (timer-stop). If no such calls have occured
there will be a runtime error.

### Tinkering with the compiler
- Customizing the editor to indent well
   - Emacs users
      - Try racket-mode by Greg Hindershot
      - Get better highlighting and indentation using our file
- Getting the most out of typed racket
   - If typecheck time blows up consider profiling with `export PLTSTDERR="error debug@tr-timing"`
- View the map of the mountain
   - src/README.md : information about the overall construction of the compiler
   - test/model/gtlc-core.rkt : the semantics of the core gradually typed lambda calculus
   
