#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(provide (all-defined-out))
(require "../helpers.rkt"
         "../language.rkt"
         "./casts-to-coercions.rkt"
         "./lower-reference-casts.rkt"
         "./introduce-castable-functions.rkt"
         "./interpret-casts.rkt"
         "./label-lambdas.rkt"
         "./uncover-free.rkt"
         "./convert-closures.rkt"
         "./specify-representation.rkt")

(: using-coercions? (Config -> Bool))
(define (using-coercions? c)
  (or (eq? (Config-function-representation c)
           'Space-Efficient-Hybrid-Functions)))


(: impose-cast-semantics (Cast0-Lang Config . -> . Data0-Lang))
(trace-define (impose-cast-semantics prgm config)
 (let* ([prgm  (if (using-coercions? config) (casts->coercions prgm config) prgm)]
        [c1 (introduce-castable-functions prgm config)]
        [c2 (lower-reference-casts c1 config)]
        [c3 (interpret-casts c2 config)]
        [l1 (label-lambdas c3 config)]
        [l2 (uncover-free l1 config)]
        [l3 (convert-closures l2 config)])
      (specify-representation l3 config)))
