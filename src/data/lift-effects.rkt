#lang typed/racket

(require "../language.rkt"
         "../helpers.rkt")

(provide lift-effects)

(: lift-effects (Data4-Lang Config -> Data5-Lang))
(define (lift-effects prog config)
  (TODO finish lift-effects))
