#lang typed/racket/base

(provide (except-out (struct-out Unique-Counter)
                     set-Unique-Counter-next!
                     Unique-Counter-next)
         make-unique-counter
         unique-counter-next!)


(struct Unique-Counter ([next : Natural])
  #:mutable)

(: make-unique-counter (Natural -> Unique-Counter))
(define (make-unique-counter x)
  (Unique-Counter x))

(: unique-counter-next! (Unique-Counter -> Natural))
(define (unique-counter-next! u)
  (define tmp (Unique-Counter-next u))
  (set-Unique-Counter-next! u (add1 tmp))
  tmp)
