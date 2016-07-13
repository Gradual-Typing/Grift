#lang typed/racket
(require 
         "./normalize-context.rkt"
         "./remove-let.rkt"
         "./remove-complex-opera.rkt"
         "./flatten-values.rkt"
         "./simplify-predicates.rkt"
         "../configuration.rkt"
         "../language/data0.rkt"
         "../language/data5.rkt")

(provide convert-representation
         (all-from-out
          "../language/data0.rkt"
          "../language/data5.rkt"))

(: convert-representation (Data0-Lang . -> . Data5-Lang))
(define (convert-representation d0)
  (let* ([d1 (normalize-context d0)]
         [d2 (remove-let d1)]
         [d3 (remove-complex-opera d2)]
         [d4 (flatten-values d3)]
         [d5 (simplify-predicates d4)])
    d5))



