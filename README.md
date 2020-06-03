# Grift

Grift is a gradually-typed language that was designed and implemented from
scratch. The runtime system is completely designed with the goal to minimize the
overhead of runtime checking for the gradually-typed language it implements.
The compiler implements a variant of space-efficient coercions and
space-efficient monotonic heap. Space-efficiency requires more work to be done
when casting an already casted value at runtime which is typically some sort of
merging between the old and the new casts. On the other hand, it is much cheaper
to access an already casted value because it is guaranteed to go through a
single layer of checks instead of a chain of casts that is linear in the number
of times this value has been casted before. Grift's runtime also represents
values and types in a way that makes many operations on them that are related to
runtime checking very efficient. Our self-comparison performance evaluation
indicates that this approach increased performance substantially in comparison
to our own implementation of the space-inefficient runtime. It also indicates
that the overhead of doing merging is not expensive. Furthermore, fully typed
and fully untyped code is in the ball-park of OCaml, and Typed Racket, and of
Chez Scheme, Gambit Scheme, and Racket respectively.

## Getting Started

```bash
raco pkg install grift
```

## Use as a Program

The installation should install a "launcher" for `grift`. If not investigate
where racket puts such things for your platform and add that to your path.

```bash
grift -o f5 tests/suite/program/fact-church-5.grift
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
Variables, X   ::= X
Formals, F     ::= X | [X : Type]
TopLevel, TE   ::= (define X E) | (define (X F ...) [: T] E ...) | E | TE ...
Expression, E  ::= X
Ascription     |   (ann E T [Blame Label])
Binding        |   (let ((X [: T] E) ...) [: T] E ...)
               |   (letrec ((X [: T] E) ...) E ...)
Functions      |   (lambda (F ...) [: T] E ...)
Application    |   (E_rator E_rand ...)
Conditionals   |   (if E_cond E_conseq E_alt)
Iteration      |   (repeat (x_i E_start E_end) [(x_acc [: T] E_init)] E)
Sequencing     |   (begin E_eff ... E_value)
Unit           |   ()
Integer        |   n_i61 | (op_int E ...)
Floats         |   n_f64 | (op_float E ...)
Tuples         |   (tuple E ...) | (tuple-proj E n)
References     |   (box E) | (unbox E) | (box-set! E_box E_box_update)
               |   (make-vector E E) | (vector-ref E E) | (vector-set! E E E)
ops            ::= + | -  | * | binary-and | binary-or | ...
               |   fl+ | fl- | fl* | ...
               |   < | <= | = | >= | > | ...
               |   fl< | fl<= | fl= | fl>= | fl> | ...
Type, T        ::= Dyn | Unit | Bool | Int | Float 
               |   (-> T ...) | (Ref T) | (Vect T) | (Tuple T ...)
               |   X | (Rec X T)
```

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
