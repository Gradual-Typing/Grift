# Add space efficient coercions

- Add pass after insert casts to translate casts to coercions if Using coercions
- Function lowering and reference Lowing will end up being about the same
  - Function coercions still being arity specific could call directly to the correct compositionspecific function coercion
     - one way of representing this is to have coercions have the arrity specific version also
     - functions should no longer require composition operators because the use case was for composing unknown
  - References should no longer require a recursive procedure to read and write
     - (if (coerced? b)
            (rt-coerce (ref-coercion-on-read b) (ubox-ref (gproxy-value b)))
            (ubox-ref b))
        - Though it would be nice to be able to specialize all the basic casts
        - coerced? Int-Coercion? Id-Coercion? Failed-Coecion? 
     - though recursive behavior may occur we composing the coercion on the box with the value from the box
- Interpret casts could introduce the coercion compose and apply runtime implementations

# Along the way
- Allow first-order functions higher in the compiler
  - will also require lifting call-label form
  - use these for all runtime procedures to limit the number of closures being created and called executing runtime code
- Extend interpreter to be pass language agnostic
  - pin down the semantics of each form
