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

;; Optimizations options
(: dynamic-operations? (Parameterof (U Boolean 'inline)))
(define dynamic-operations? (make-parameter #t))

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
  (make-parameter (expt 1024 2)))

;; Interaction with the c compiler
(define c-flags : (Parameterof (Listof String))
  (make-parameter '("-Wno-int-conversion" "-Wno-format" "-Wno-unused-value")))
;; where is the runtime to be used located
(define runtime-path : (Parameterof (Option Path)) (make-parameter #f))



