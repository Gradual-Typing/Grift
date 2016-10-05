#lang typed/racket/base

(provide (except-out (struct-out Unique-Counter)
                     set-Unique-Counter-next!
                     Unique-Counter-next)
         make-unique-counter
         unique-counter-next!
         current-unique-counter)


(struct Unique-Counter ([next : Natural])
  #:mutable)

(: make-unique-counter (Natural -> Unique-Counter))
(define (make-unique-counter x)
  (Unique-Counter x))

(define current-unique-counter : (Parameterof (Option Unique-Counter))
  (make-parameter #f))

(: unique-counter-next! (->* () (Unique-Counter) Natural))
(define (unique-counter-next! [u : (Option Unique-Counter) (current-unique-counter)])
  (cond
    [u
     (define tmp (Unique-Counter-next u))
     (set-Unique-Counter-next! u (add1 tmp))
     tmp]
    [else
     (error 'unique-counter-next!
            "current-unique-counter unbound in dynamic extent")]))


