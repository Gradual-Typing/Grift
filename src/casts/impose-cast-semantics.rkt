#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(provide (all-defined-out))
(require "../helpers.rkt"
         "../language.rkt"
          "./lower-reference-casts.rkt"
          "./introduce-castable-functions.rkt"
          "./interpret-casts.rkt"
          "./hoist-types.rkt"
          "./label-lambdas.rkt"
          "./uncover-free.rkt"
          "./convert-closures.rkt"
          "./specify-representation.rkt")

(: impose-cast-semantics (Cast0-Lang Config . -> . Data0-Lang))
(trace-define (impose-cast-semantics prgm config)
  (let* ([c1 (introduce-castable-functions prgm config)]
         [c2 (lower-reference-casts c1 config)]
         [c3 (interpret-casts c2 config)]
         [t  (hoist-types c3 config)]
         [l1 (label-lambdas t config)]
         [l2 (uncover-free l1 config)]
         [l3 (convert-closures l2 config)])
      (specify-representation l3 config)))
