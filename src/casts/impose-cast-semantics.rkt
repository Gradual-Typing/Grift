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
         "./interpret-casts-with-hyper-coercions.rkt"
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

(: statically-typed-program? : Cast0.5-Lang -> Boolean : Cast-or-Coerce3-Lang)
(define (statically-typed-program? x)
  (error 'todo))

(: impose-cast-semantics : Cast0-Lang -> Data0-Lang)
(define (impose-cast-semantics c0)
  (define c0.5 (define->let c0))
  (define c1 : Cast-or-Coerce3-Lang
    (case (cast-representation) 
      [(|Type-Based Casts|)
       (interpret-casts/twosomes c0.5)]
      [(Coercions)
       (interpret-casts/coercions c0.5)]
      [(Hyper-Coercions)
       (interpret-casts/hyper-coercions c0.5)]
      [(Static)
       (cond
         ;; Effectively Cast Programs 
         [(statically-typed-program? c0.5) c0.5]
         [else (error 'schml "tried to compile non-static program with static types")])]))
  (define c5 (hoist-types-and-coercions c1))
  (define c5.5 (purify-letrec c5))
  (define c6 (label-lambdas c5.5))
  (define c7 (uncover-free c6))
  (define c8 (convert-closures c7))
  (specify-representation c8))


