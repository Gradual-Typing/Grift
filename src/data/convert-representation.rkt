#lang typed/racket/no-check

(require 
 "./normalize-context.rkt"
 "./uncover-locals.rkt"
 "./remove-complex-opera.rkt"
 "./flatten-values.rkt"
 "./simplify-predicates.rkt")

(provide convert-representation)

(: convert-representation (Data0-Lang -> Data5-Lang))
(define (convert-representation d0)
  (let* ([d1 (normalize-context d0)]
         [d2 (uncover-locals d1)]
         [d3 (remove-complex-opera d2)]
         [d4 (flatten-values d3)]
         [d5 (simplify-predicates d4)])
    d5))



