# Structure of the Compiler Passes

The Compiler is composed of passes which themselves may be composed
of multiple passes.

## Passes - Description
- read - read the file in as an s-expression
- parse - checks that syntax is correct and implements some syntax sugar
- type-check - performs type-checking and explicitly annotates the ast with type information
- insert-implicit-casts - inserts any casts that are necissary by the type semantics so that all casts are explicit
- make-closures-explicit
  - label-lambdas
  - uncover-free
  - convert-closures
  - instroduce-procedure-primitives
  - lift-functions



