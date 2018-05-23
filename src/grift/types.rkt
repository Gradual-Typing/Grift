#lang typed/racket/base

(provide (all-defined-out))
(struct (A D) Cons ([a : A] [d : D])
  #:transparent)

(define-type (T3 A B C) (Cons A (Cons B (Cons C Null))))
