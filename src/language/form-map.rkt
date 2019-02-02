#lang racket/base

(require "forms.rkt"
         racket/struct
         racket/match)

(provide form-map)

(define (form-map-field x f)
  (cond
    [(form? x) (f x)]
    [(pair? x)
     (define a (form-map-field (car x) f))
     (define d (form-map-field (cdr x) f))
     (if (and (eq? d (cdr x))
              (eq? a (car x)))
         x
         (cons a d))]
    [else x]))

(define (form-map x f)
  (cond
    [(form:leaf? x) x]
    [(form:simple-branch? x)
     (define fields (struct->list x))
     (define f-fields (form-map-field fields f))
     (cond
       [(eq? fields f-fields) x]
       [else
        (define-values (st skipped?) (struct-info x))
        (define ctr (struct-type-make-constructor st))
        (apply ctr f-fields)])]
    [else (error 'form-map "unmatched ~a" x)]))


(module+ test
  (require rackunit)
  (define f1 (Quote 1))
  (define (id x) x)
  (define f2 (App f1 f1))
  (check-eq? (form-map f1 error) f1)
  (check-eq? (form-map f2 id) f2)
  (define f3 (Lambda '(1 2 3) (Castable #f f2)))
  (define (fadd1 x)
    (match x
      [(Quote (? integer? n)) (Quote (add1 n))]
      [other (form-map other fadd1)]))
  (define f5 (Lambda '(1 2 3) (Castable #f (App (Quote 2) (Quote 2)))))
  (check-equal? (fadd1 f3) f5))
