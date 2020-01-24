# Structure of the Compiler Passes

[grift/reduce-to-cast-calculus.rkt]: grift/reduce-to-cast-calculus.rkt
[read.rkt]: grift/read.rkt
[syntax-to-grift0.rkt]: grift/syntax-to-grift0.rkt
[type-check.rkt]: grift/type-check.rkt
[insert-casts.rkt]: grift/insert-casts.rkt
[casts/impose-cast-semantics.rkt]: casts/impose-cast-semantics.rkt
[interpret-casts.rkt]: casts/interpret-casts.rkt
[interpret-casts-common.rkt]: casts/interpret-casts-common.rkt
[interpret-casts-with-coercions.rkt]: casts/interpret-casts-with-coercions.rtk
[interpret-casts-with-type-based.rkt]: casts/interpret-casts-with-type-based.rkt
[interpret-casts-with-hyper-coercions.rkt]: casts/interpret-casts-with-hyper-coercions.rkt
[hoist-types-and-coercions.rkt]: casts/hoist-types-and-coercions.rkt
[specify-representation.rkt]: casts/specify-representation.rkt
[data/convert-representation.rkt]: data/conver-representation.rkt
[backend/code-generator.rkt]: backend/code-generator.rkt
[runtime/]: backend/runtime/
[c/generate-c.rkt]: backend/c/generate-c.rkt
[language/]: language/
[language/forms.rkt]: language/forms.rkt
[compile.rkt]: compile.rkt
[configuration.rkt]: configuration.rkt


## The compiler

The compiler driver is located in [compile.rkt] and can be
configured by passing arguments or parameterizing the variables found
in [configuration.rkt].

## Passes - Description

The Compiler is composed of passes which themselves may be composed
of multiple passes.

- [grift/reduce-to-cast-calculus.rkt]:
  - [read.rkt]: reads from file
  - [syntax-to-grift0.rkt]: turns racket syntax objects into grift AST
    representation.
  - [type-check.rkt]: rejects ill-typed programs and annotates the AST
    with type information.
  - [insert-casts.rkt]: inserts explicit cast nodes where types are consistent
    but not equal.
- [casts/impose-cast-semantics.rkt]: 
  - [interpret-casts.rkt]: Creates a casting runtime.
    - [interpret-casts-common.rkt]: generic facilities for building a
      cast runtime.
    - [interpret-casts-with-coercions.rkt]: instantiates the runtime
      with a coercion representation.
    - [interpret-casts-with-type-based.rkt]: instantiates the runtime
      with a type based interpretation of casts.
    - [interpret-casts-with-hyper-coercions.rkt]: instantiates the runtime
      with a hyper-coercion (more cache friendly representation of coercions)
      representation. 
  - [hoist-types-and-coercions.rkt]: compile time hash-consing of
    runtime type/cast representations.
  - [specify-representation.rkt]: turns abstract operations into
    concrete memory manipulation.
- [data/convert-representation.rkt]: Takes an expression oriented
  language to a statement oriented language.
- [backend/code-generator.rkt]: 
  - [c/generate-c.rkt]: Write a c-src file.
  - [runtime/]: GC and other runtime facilities

## Language Definitions and Types

The types that each of these passes handle are defined in the [/language/]
subdirectory.





