#lang typed/racket

(require "../language/data0.rkt"
         "../language/syntax.rkt")

(provide (all-defined-out))

(: sr-tagged-array-ref (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-ref e t i) (sr-array-ref (sr-untag e t) i))

(: sr-tagged-array-set! (D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-set! e t i v) (sr-array-set! (sr-untag e t) i v))

(: sr-array-ref (D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-ref e i) (op$ Array-ref e i))

(: sr-array-set! (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-set! e i v) (op$ Array-set! e i v))

(: sr-untag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-untag e t) (op$ binary-xor e t))

(: unmask-rest : D0-Expr D0-Expr -> D0-Expr)
(define (unmask-rest e m) (op$ binary-and e (op$ binary-not m)))

(: tagged-array-ref : D0-Expr D0-Expr D0-Expr -> D0-Expr)
(define (tagged-array-ref e tm i)
  (sr-array-ref (unmask-rest e tm) i))

(: check-tag? : D0-Expr D0-Expr -> D0-Expr)
(define (check-tag? e m) (op$ not (op$ = (Quote 0) (sr-get-tag e m))))

(: sr-tag-value (D0-Expr D0-Expr -> D0-Expr))
(define (sr-tag-value e t)
  (match t
    [(Quote 0) e]
    [tag (Op 'binary-or `(,e ,tag))]))

(: sr-check-tag=? (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-check-tag=? e mask tag) (op$ = (sr-get-tag e mask) tag))

(: sr-plus (D0-Expr D0-Expr -> D0-Expr))
(define (sr-plus f s) (op$ + f s))

(: sr-get-tag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-get-tag e mask) (op$ binary-and e mask))
