#lang racket

(define ground-types '(Fix Flo Bool))

(define dynamic '?)
(define relops '(< <= = >= >))
(define predefined-funcs '(* + - *. +. -.))



(struct Lambda (fml* type body src))
(struct App (rator rand* src))
(struct Op (prim rand* src))
(struct Quote (const src))
(struct Var (ident type src))
(struct Cast (expr type src))
(struct If (test conseq alt src))

(struct Letrec (dec* body src))
(struct Let (dec* body src))


;(struct Begin (effect* expr src))
;(struct Quote (datum src))
;(struct Define (var expr))
;(struct Type-Dec (type1 type2))

(define (Constant? x)
  (or (boolean? x)
      (fixnum? x)
      (flonum? x)))

(define (Datum? x)
  (define (vector-andmap p? v)
    (let loop ((i (sub1 (vector-length v))))
      (or (< i 0) (and (p? (vector-ref v i)) (loop (sub1 n))))))
  (or (Constant? x)
      ;; (and (pair? x)
      ;;      (datum? (car x))
      ;;      (datum? (cdr x)))
      ;; (and (vector? x)
      ;;      (vector-andmap datum? x)))
      ))



