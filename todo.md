# For Maintainability 
- [ ] Add an ast pretty printer (ast -> sexp)
- [ ] normalize context to before specify representation
- [ ] Simplify parser (make it untyped) and use macros (this could be done)

# For Research
- [ ] Add Hoist coercion constants
- [ ] Add simplify known casts (v <id;T!> -> <T!> | tag v t)
- [ ] Start using better timers that measure process time instead of wall-clock time
- [ ] Add Dynamic-Operation optimization (App-Dyn, Dyn-Gbox-ref, ...)
- [ ] Fix letrec
- [ ] Lower the letrec purification pass or raise unguarded boxes
- [ ] Add Lazy Shadow Stack GC + Cheney Copy Collector + Bump Pointer Allocation + GC Statistics
- [ ] Implement a switch construct for multiway branching endemic to gradual typing
- [ ] Add no-optimize annotations for testing purposes
- [ ] Make sure that data structure function rep works space inefficent coercions
- [ ] look at the code function casts created by extracting a dynamic
      value and make sure that (Id -> Id) is getting collapsed to Id
      this may be implicitly occuring due to pointer equality of types.
- [ ] Manually specialize the code for the fast cases of make coercion
      in compose.
- [ ] Add hoisting of constants in general
- [ ] add inlining, constant propagation, common subexpression elimination
      dead code elimination pass.
- [ ] Tail coercions optimization.
- [ ] Custom backend that allows us to play with the return register

# For Language 
- [ ] add define, include, define-type,
- [ ] recursive types
- [ ] product types (tuples and structs)
- [ ] parametric polymorphism
- [ ] unions (tagged?, untagged)
- [ ] Structural Objects 
- [ ] Move to a LLVM Backend 
