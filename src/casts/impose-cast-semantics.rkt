#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(require "../helpers.rkt"
         "../configuration.rkt"
         "./purify-letrec.rkt"
         "./hoist-types-and-coercions.rkt"
         "define-to-let.rkt"
         "lower-function-casts.rkt"
         #;"lower-reference-casts.rkt"
         "interpret-casts-with-twosomes.rkt"
         "casts-to-coercions.rkt"
         "interpret-casts-with-coercions.rkt"
         #;"casts-to-super-coercions.rkt"
         #;"interpret-casts-with-super-coercions.rkt"
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
  (define c1
    (case (cast-representation)
      [(Type-Based)
       (interpret-casts/twosomes (lower-function-casts c0.5))]
      [(Coercions)
       (interpret-casts/coercions (lower-function-casts (casts->coercions c0.5)))]
      #;
      [(Super-Coercions)
       (define c2 (casts->super-coercions c0))
       (interpret-casts/super-coercions (error 'impose-cast-semantics/todo))]))
  (define c5 (hoist-types-and-coercions c1))
  (define c5.5 (purify-letrec c5))
  (define c6 (label-lambdas c5.5))
  (define c7 (uncover-free c6))
  (define c8 (convert-closures c7))
  (specify-representation c8))


