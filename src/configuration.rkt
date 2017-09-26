#lang typed/racket/base

(provide (all-defined-out))

;; How blame is tracked
(define-type Blame-Semantics (U 'Lazy-D))
(define blame-semantics : (Parameterof Blame-Semantics)
  (make-parameter 'Lazy-D))

;; How casts are represented
(define-type Cast-Representation
  (U '|Type-Based Casts| 'Coercions 'Hyper-Coercions 'Static))
(define cast-representation : (Parameterof Cast-Representation)
  (make-parameter 'Coercions))

(define program-must-be-statically-typed? : (Parameterof Boolean)
  (make-parameter #f))

;; Optimizations options
(: dynamic-operations? (Parameterof (U Boolean 'inline)))
(define dynamic-operations? (make-parameter #t))
(: direct-fn-cast-optimization? (Parameterof Boolean))
(define direct-fn-cast-optimization? (make-parameter #t))
(: check-asserts? (Parameterof Boolean))
(define check-asserts? (make-parameter #f))

;; Vector Behavior
(define bounds-checks? : (Parameterof Boolean)
  (make-parameter #t))
(define emit-vars-with-original-source-location? : (Parameterof Boolean)
  (make-parameter #f))
(define-type Ref-Semantics (U 'Monotonic 'Proxied))
(define reference-semantics : (Parameterof (U 'Monotonic 'Proxied))
  (make-parameter 'Proxied))
(: inline-guarded-branch? (Parameterof Boolean))
(define inline-guarded-branch? (make-parameter #t))

;; Cast behavior
;; TODO this is largely setup based on how the main
;; function is designed we should edit this and the
;; main function to give the best performance by default
;; and options to enable non-optimized configurations
(: specialize-cast-code-generation? (Parameterof Boolean))
;; TODO if this is faster we should default to this
(define specialize-cast-code-generation?
  (make-parameter #f))
(: optimize-first-order-coercions? (Parameterof Boolean))
;; TODO if this doesn't matter anymore we should nix it
(define optimize-first-order-coercions? (make-parameter #t))
(: coercions-are-space-efficient? (Parameterof Boolean))
(define coercions-are-space-efficient? (make-parameter #t))
;; TODO if this is faster we should default to this
(define hybrid-cast/coercion-runtime? : (Parameterof Boolean)
  (make-parameter #f))



;; Default places for everything, but there is no default source
(define c-path : (Parameterof (Option Path))
  (make-parameter #f))
(define s-path : (Parameterof (Option Path))
  (make-parameter #f))
(define output-path : (Parameterof (Option Path))
  (make-parameter #f))
(define init-heap-kilobytes : (Parameterof Natural)
  (make-parameter 1024))
(define init-types-hash-table-slots : (Parameterof Natural)
  (make-parameter 50))
(define types-hash-table-load-factor : (Parameterof Flonum)
  (make-parameter 0.75))
(define-type GC (U 'Boehm 'None))
(define garbage-collector : (Parameterof GC)
  (make-parameter 'Boehm))



;; Interaction with the c compiler
(define c-flags : (Parameterof (Listof String))
  (make-parameter '("-Wno-int-conversion" "-Wno-format" "-Wno-unused-value")))
;; where is the runtime to be used located
(define runtime-path : (Parameterof (Option Path)) (make-parameter #f))
(define hashcons-path : (Parameterof (Option Path)) (make-parameter #f))
