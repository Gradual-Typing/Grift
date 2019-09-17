#lang racket/base

(require
 (for-syntax
  racket/base
  syntax/parse)
 racket/contract/base
 racket/contract/option
 "../configuration.rkt")

(provide
 (all-from-out racket/contract/option)
 (all-defined-out))

(define (optionally-contract x)
  (if (with-contracts)
      (exercise-option x) 
      x))

;; Helper for defining a bunch of mutually recursive contracts
(define-syntax (define-contracts stx)
  (syntax-parse stx
    [(_  (name*:id other*) ...)
     #'(define-values (name* ...)
         (flat-murec-contract
          ([name* other*] ...)
          (values name* ...)))]))

;; Optional value contract
(define (?/c x/c) (or/c false/c x/c))

;; Natural numbers
(define nat/c exact-nonnegative-integer?)

;; Contract for Switch-Case structure
(define (Switch-Case*/c lhs)
  (listof (cons/c (listof exact-integer?) lhs)))
