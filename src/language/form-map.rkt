#lang racket/base

(require "forms.rkt"
         racket/struct
         racket/match)

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
    [else
     (match x)]))
