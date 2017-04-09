# For Maintainability 
- [ ] Add an ast pretty printer (ast -> sexp)
- [?] normalize context to before specify representation
- [X] Simplify parser (make it untyped) and use macros (this could be done)
- [X] switch twosomes to type-based every where

# For Research
- [X] Add Hoist coercion constants
- [X] Add simplify known casts (v <id;T!> -> <T!> | tag v t)
- [X] Add Dynamic-Operation optimization (App-Dyn, Dyn-Gbox-ref, ...)
- [ ] Fix letrec (ie add letrec* and optimizations)
- [ ] Add closure optimizations
- [ ] add inlining, constant propagation, common subexpression elimination
      dead code elimination pass. (remove specialization of casts)
- [?] Start using better timers that measure process time instead of wall-clock time
- [ ] Fix letrec pass (I think it is currently broken) and make sure that it 
      doesn't add overhead when casting an immediate lambda.
- [ ] Add Lazy Shadow Stack GC + Cheney Copy Collector + Bump Pointer Allocation + GC Statistics
- [X] Implement a switch construct for multiway branching endemic to gradual typing
- [?] Add no-optimize annotations for testing purposes
- [ ] Make sure that data structure function rep works space inefficent coercions
- [ ] look at the code function casts created by extracting a dynamic
      value and make sure that (Id -> Id) is getting collapsed to Id
      this may be implicitly occuring due to pointer equality of types.
- [ ] Manually specialize the code for the fast cases of make coercion
      in compose.
- [ ] Add hoisting of constants in general
- [ ] Tail coercions optimization.
- [ ] Custom backend that allows us to play with the return register

# For Language 
- [x] add define
- [x] include, define-type,
- [ ] recursive types
- [X] product types (tuples and structs)
- [ ] parametric polymorphism
- [ ] unions (tagged?, untagged)
- [ ] Structural Objects 
- [ ] Move to a LLVM Backend 
