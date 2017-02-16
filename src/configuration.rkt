#lang typed/racket/base

(provide (all-defined-out))

;; How blame is tracked
(define-type Blame-Semantics (U 'Lazy-D))
(define blame-semantics : (Parameterof Blame-Semantics)
  (make-parameter 'Lazy-D))

;; How casts are represented
(define-type Cast-Representation (U 'Type-Based 'Coercions))
(define cast-representation : (Parameterof Cast-Representation)
  (make-parameter 'Coercions))

(define program-must-be-statically-typed? : (Parameterof Boolean)
  (make-parameter #f))

;; Optimizations options
(: dynamic-operations? (Parameterof (U Boolean 'inline)))
(define dynamic-operations? (make-parameter #t))

;; Vector Behavior
(define bounds-checks? : (Parameterof Boolean)
  (make-parameter #t))
(define-type Ref-Semantics (U 'Monotonic 'Guarded))
(define reference-semantics : (Parameterof (U 'Monotonic 'Guarded))
  (make-parameter 'Guarded))
(: inline-guarded-branch? (Parameterof Boolean))
(define inline-guarded-branch? (make-parameter #t))

;; Cast behavior 
(: specialize-cast-code-generation? (Parameterof Boolean))
(define specialize-cast-code-generation? (make-parameter #f))

;; Default places for everything, but there is no default source
(define c-path : (Parameterof (Option Path))
  (make-parameter #f))
(define s-path : (Parameterof (Option Path))
  (make-parameter #f))
(define output-path : (Parameterof (Option Path))
  (make-parameter #f))
(define init-heap-kilobytes : (Parameterof Natural)
  (make-parameter 1024))
(define-type GC (U 'Boehm 'None))
(define garbage-collector : (Parameterof GC)
  (make-parameter 'Boehm))



;; Interaction with the c compiler
(define c-flags : (Parameterof (Listof String))
  (make-parameter '("-Wno-int-conversion" "-Wno-format" "-Wno-unused-value")))
;; where is the runtime to be used located
(define runtime-path : (Parameterof (Option Path)) (make-parameter #f))



