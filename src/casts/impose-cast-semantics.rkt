#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(require "../helpers.rkt"
         "../configuration.rkt"
         "./purify-letrec.rkt"
         "./hoist-types-and-coercions.rkt"
         "lower-function-casts.rkt"
         "lower-reference-casts.rkt"
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

(: impose-cast-semantics : Cast0-Lang Config -> Data0-Lang)
(define (impose-cast-semantics c0 cfg)
  (define c4
    (case (Config-cast-rep cfg)
      [(Twosomes)
       (define c2 (lower-function-casts c0 cfg))
       (interpret-casts/twosomes c2)]
      [(Coercions)
       (define c2 (casts->coercions c0 cfg))
       (define c3 (lower-function-casts c2 cfg))
       (interpret-casts/coercions c3)]
      #;
      [(Super-Coercions)
       (define c2 (casts->super-coercions c0))
       (interpret-casts/super-coercions (error 'impose-cast-semantics/todo))]))
  (define c5 (hoist-types-and-coercions c4 cfg))
  (define c5.5 (purify-letrec c5 cfg))
  (define c6 (label-lambdas c5.5 cfg))
  (define c7 (uncover-free c6 cfg))
  (define c8 (convert-closures c7 cfg))
  (specify-representation c8 cfg))

#;
(define-compiler impose-cast-semantics : (Cast0-Lang Config -> Data0-Lang)
  purify-letrec
  (when coercion-representation?
    casts->coercions)
  lower-function-casts
  (when (not ()))
  (if coercion-representation?
      interpret-casts/coercions
      interpret-casts/twosomes)

  ;; todo put purify-letrec here to take advantage
  label-lambdas
  uncover-free
  convert-closures
  specify-representation)
