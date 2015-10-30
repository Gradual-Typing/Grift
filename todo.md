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
- We will need to add to the runtime the ability to create coercions from types at runtime. 


# Initialization 



# Along the way
- Allow first-order functions higher in the compiler
  - will also require lifting call-label form
  - use these for all runtime procedures to limit the number of closures being created and called executing runtime code
- Extend interpreter to be pass language agnostic
  - pin down the semantics of each form


# Try to specialize syntax to semantics
-- Document when nodes are legal
-- Document semantics if reusing syntax



# Representation Ideas

(define-forms
  ;; Identity Cast
  ;; "Calculated No Op Cast"
  
  (Identity type)
  ;; Projection Coercion
  ;; "Project from dynamic at type blaming label if it fails"
  ;; G?ˡ in most papers
  ;; T?ˡ for this compiler
  (Project type label)
  ;; Injection Coercion
  ;; "Inject into the dynamic type at type
  (Inject type)
  ;; Sequence Coercion
  ;; "Perform the first Coercion and then Another"
  (Sequence fst snd)
  ;; Fail Coercion
  ;; "Fail with the label"
  (Failed label type1 type2)
  ;; Function Coercion
  ;; "Proxy a function call with args coercions and return coercion"
  (Proxy-Fn arity args return)
  ;; Guarded Reference Coercion
  ;; "Proxy a Guarded Reference's Reads and writes"
  (Proxy-GRef read write))

Goal Find a Representation that can represent both values and coercions efficiently

User Reachable Values (Runtime Representation of first class values)

Should we tag all values?

Static Atomically Typed Values
Int  = 0 to n : long 
Bool = 0 or 1 : long
Unit = 2

Statically Dynamically Typed Values
Dyn       = (tag 000 {long value, long type})
DynInt    = (tag 001 (shiftl n 3))
DynAtomic = (tag 111 (shiftl b 3))

Static Fun Constructed Type Value
Fun      = (tag 000 (closure code-label fv1 fv2 fv3))
ProxyFun = (tag 001 (Proxy-Fun f))

Static GRef Constructed Type Value
GRef      = (tag 000 (gbox n))
ProxyGref = (tag 001 (Proxy-GRef (gbox n)))

Types are all allocated arrays where the first file is the "constructor"
Types are only used in the projection case. So we can relax the representation
to be less efficient and more consistent
All types are taggable pointers to the RTTI

IntID BoolID DynID FnID GRef = Number
Dyn  = [DynID]    or enum1:000
Int  = [IntID]    or enum2:000
Bool = [BoolID]   or enum3:000
T1   = (Int -> Int) = [FnID, 1 , Int, Int]
T2   = ((Int -> Int) Bool -> Int) = [FnID, 2, Int, T1, Bool]
T3   = (GRef (Int -> Int)) = [GRef, T1]

;; Integers and Function are at Int and Fun Efficiently

;; Runtime Representation of Coercions
;; Injections are represented as a taged pointer to



(Identity type)              = (tag 000 type)
(Inject   type)              = (tag 001 type)
(Project  type lbl)          = (tag 010 [type, label])
(Sequence first next)        = (tag 011 [first, next])
(ProxyFn  arity args return) = (tag 100 [arity, return, argument])
(ProxyGRef read write)       = (tag 101 [read, write])
;; You don't need additional coercions for Vectors because the projection
;; will fail if the value is wrong.



(define COERCION-TAG-IDENTITY   #b000)
(define COERCION-TAG-INJECT     #b001)
(define COERCION-TAG-PROJECT    #b010)

;; Coercion Instructions
(Identity 
;; Coercion Values


