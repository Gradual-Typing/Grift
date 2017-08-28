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
         "uncover-free.rkt"
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
  (debug c0.5)
  (define c1 : Cast-or-Coerce3-Lang (interpret-casts c0.5))
  (debug c1)
  (define c5 (hoist-types-and-coercions c1))
  (define c5.5 (purify-letrec c5))
  (define c6 (label-lambdas c5.5))
  (define c7 (uncover-free c6))
  (define c8 (convert-closures c7))
  (specify-representation c8))


