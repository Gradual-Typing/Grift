#lang typed/racket/base

(provide (all-defined-out))

(define-type Semantics (U 'Lazy-D))
(define-type Cast-Representation (U 'Twosomes 'Coercions))

(: dynamic-operations? (Parameterof (U Boolean 'inline)))
(define dynamic-operations? (make-parameter #t))

(struct Config
  ([source-path : Path]
   [semantics : Semantics]
   [exec-path : Path]
   [c-path : Path]
   [keep-c : Boolean]
   [c-flags : (Listof String)]
   [asm-path : (Option Path)]
   [cast-rep : Cast-Representation]
   [mem-limit : Natural]
   [runtime-path : (Option Path)])
  #:transparent)


