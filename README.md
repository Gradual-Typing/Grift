# Grift: A GTLC Compiler

Welcome to the home of a compiler for the Gradually Typed Lambda 
Calculus (GTLC). The compiler is intended to explore how different
gradual typing semantics may be represented at runtime. It is currently
a work in progress, but I am at the level where I can start to
play with the runtime representation of objects freely.

## Getting Started

A simple setup for 

```bash
git checkout git@github.com:Gradual-Typing/Grift.git
cd Grift
raco pkg install
```

## Use as a Program

The installation should install a "launcher" for `grift`. If not investigate
where racket puts such things for your platform and add that to your path.

```bash
grift -o f5 test/suite/program/fact-church-5.grift
./f5
```

## Use as a library

```racket
#lang racket
(require grift)
(compile "tests/suite/program/n-body.grift"
         #:output "n-body")
(system "n-body")
```

## The Gradually Typed Lambda Calculus

This is an incomplete grammar of the syntax of the GTLC. In the future
we will try to document the language more carefully and link to that
documentation here.

```bnf
Expression, E  ::= 
Ascription     |   (ann E T [Blame Label])
Binding        |   (let ((X [: T] E) ...) [: T] E)
               |   (letrec ((ID [: T] E) ...) E)
Functions      |   (lambda (X ...) [: T] E)
Application    |   (E_rator E_rand ...)
Conditionals   |   (if E_cond E_conseq E_alt)
Iteration      |   (repeat (x_i E_start E_end) [(x_acc E_init)] E)
Sequencing     |   (begin E_eff ... E_value)
Unit           |   ()
Integer        |   n_i61 | (op_int E ...)
Floats         |   n_f64 | (op_float E ...)
Tuples         |   (tuple E ...) | (tuple-proj E n)
References     |   (box E) | (unbox E) | (box-set! E_box E_box_update)
               |   (make-vector E E) | (vector-ref E E) | (vector-set! E E E)
ops            ::=  + | -  | * | binary-and | binary-or | ...
               |    fl+ | fl- | fl* | ...
               | < | <= | = | >= | > | ...
               | fl< | fl<= | fl= | fl>= | fl> | ...
Type, T        ::= Dyn | Int | Bool | Unit | (-> Type ...) | (Ref Type)
```

In order to support more accurate benchmarking we have added the
repeat special form which is guaranteed to be the equivalent of a for
loop in C. The loop ```(repeat (id start stop) exp)``` is the
equivalent of the for loop ```for(id = start; id < stop; id += 1)
exp;```. It always results in a unit value.

Further support for benchmarking has been added with the timer
primitives.  All of which take no arguments and return unit. This is a
global one shot timer. Calling (timer-report) will print out the time
in seconds between the calls (timer-start) and (timer-stop). If no
such calls have occurred there will be a runtime error.

### Tinkering with the compiler
- [/src/README.md](/src/README.md) contains an overview of the construction
  of the compiler.
- Customizing the editor to indent well
   - Emacs users
      - Try racket-mode by Greg Hindershot
      - Get better highlighting and indentation using our file
- Getting the most out of typed racket
   - If typecheck time blows up consider profiling with 
     `export PLTSTDERR="error debug@tr-timing"`

   
