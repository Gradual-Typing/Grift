#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

(define timing-loop-test
  (make-timing-loop
   #:timed-action  (lambda (i acc) `(+ ,i ,acc))
   #:acc-type 'Int
   #:acc-init 0
   #:use-acc-action (lambda (acc) acc)))

(define (make-app-timing-loop casts t1 t2 init use)
  (define id-type `(,t1 -> ,t1))
  (define id-type-inv `(,t2 -> ,t2))
  (define-values (id-casted-type acc-type cl-invocation use-undyned)
    (if (even? casts)
        (values id-type t1  
                `(cast-loop ,casts id)
                (lambda (use) use))
        (values id-type-inv t2
                `(: (cast-loop ,(sub1 casts) id) (,t2 -> ,t2))
                (lambda (use) (lambda (v) `(: ,(use v) Int))))))
  (make-timing-loop
   #:timed-action (lambda (i acc) `(id-casted ,acc))
   #:letrec-bnds
   `([cast-loop : (Int ,id-type -> ,id-type)
                (lambda ([n : Int] [g : ,id-type])
                  (if (= n 0)
                      g 
                      (cast-loop (- n 2) (: g ,id-type-inv))))]
     [id : ,id-type (lambda ([x : ,t1]) x)])
   #:let-bnds
   `([id-casted : ,id-casted-type ,cl-invocation])
   #:acc-type acc-type
   #:acc-init init
   #:use-acc-action (use-undyned use)))

(define (symbolic-repeat n ctr base)
  (if (<= n 0)
      `(,ctr ,base)
      `(,ctr ,(symbolic-repeat (sub1 n) ctr base))))

(define (make-reference-cast-timing-loop t1 t2 depth)
  (make-timing-loop
   #:timed-action (lambda (i acc) `(: (: acc ,t2) ,t1))
   #:acc-type t1
   #:acc-init (symbolic-repeat depth 'gbox 42)
   #:use-acc-action (lambda (v) (symbolic-repeat depth 'gunbox v))))

(define (make-reference-wr-timing-loop t1 t2 casts depth)
  (define ref (symbolic-repeat depth 'gbox -22))
  (define use (lambda (v)
                `(+ ,(cadr (symbolic-repeat depth 'gunbox v))
                    ,(symbolic-repeat depth 'gunbox 'guarded-ref))))
  (define-values (acc-type cl-invocation acc-init)
    (if (even? casts)
        (values (cadr t1)
                `(cast-loop ,casts ,ref)
                (cadr (symbolic-repeat depth 'gbox 21)))
        (values (cadr t2)
                `(: (cast-loop ,(sub1 casts) ,ref) ,t2)
                (cadr (symbolic-repeat depth 'gbox '(: 21 Dyn))))))
  (make-timing-loop
   #:timed-action
   (lambda (i acc)
     `(begin
        (gbox-set! guarded-ref acc)
        (gunbox guarded-ref)))
   #:letrec-bnds
   `([cast-loop : (Int ,t1 -> ,t1)
                (lambda ([n : Int] [g : ,t1])
                  (if (= n 0)
                      g 
                      (cast-loop (- n 2) (: g ,t2))))])
   #:let-bnds
   `([guarded-ref : (GRef ,acc-type) ,cl-invocation])
   #:acc-type acc-type
   #:acc-init acc-init
   #:use-acc-action use))
