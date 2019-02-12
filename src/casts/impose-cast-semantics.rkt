#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(require (submod "../logging.rkt" typed) 
         "./purify-letrec.rkt"
         "./hoist-types-and-coercions.rkt"
         "define-to-let.rkt" 
         "interpret-casts.rkt"
         "label-lambdas.rkt"
         "convert-closures.rkt"
         "specify-representation.rkt"
         "../language/cast0.rkt"
         "../language/data0.rkt")

(provide (all-defined-out)
         (all-from-out
          "../language/cast0.rkt"
          "../language/data0.rkt"))

(: impose-cast-semantics : Cast0-Lang -> Data0-Lang)
(define (impose-cast-semantics c0)
  (define c0.5 (define->let c0))
  (define c1   (interpret-casts c0.5))
  (define c5   (hoist-types-and-coercions c1))
  (define c5.5 (purify-letrec c5))
  (define c6   (label-lambdas c5.5))
  (define c7   (convert-closures c6))
  (specify-representation c7))


