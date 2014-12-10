# Schml A GTLC Compiler
Welcome to the home of a compiler for the Gradually Typed Lambda 
Calculus (GTLC). The compiler is intended to explore how different
gradual typing semantics may be represented at runtime. It is currently
a work in progress, but I am nearing a the level where I can start to
play with the runtime representation of objects freely.

## Installation

The package is known to install correctly with:
-6.1
-6.1.1

'''bash
raco pkg install 
'''

## Use as a library
'''racket
(require schml)
(compile string/or/path/to/file)
'''

## The Language supported
prog ::= expr
lamb ::= (lambda (fml) expr)
expr ::= lamb
     |   (let ([id maybe-type expr] ...) expr)
     |   (letrec ([id maybe-type lamb] ...) expr_
     |   (expr ...)
     |   (: expr type maybe-string)
     |   (if expr expr expr)
     |   (prim expr expr)
prim ::= + | -  | * | boolean-and | boolean-or
     |   < | <= | = | >= | >
type ::= Dyn | Int | Bool | (-> Type ...)
fml  ::= id | [id : type]

## Known Limitations
To many to list currently.

### For more information:
- compiler/README.md : information about how the compiler is constructed

### Known errors

