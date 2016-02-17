#lang typed/racket/base

(require "./forms.rkt")
(provide (all-defined-out))


(: schml-primitive->type
   (-> Schml-Primitive (Fn Index (Listof (U Int Bool)) (U Int Bool Unit))))
(define (schml-primitive->type p)
  (cond
   [(IntxInt->Bool-primitive? p) INTxINT->BOOL-TYPE]
   [(IntxInt->Int-primitive? p)  INTxINT->INT-TYPE]
   [(->Int-primitive? p) ->INT-TYPE]
   [(timer-primitive? p)         ->UNIT-TYPE]))




#|-----------------------------------------------------------------------------
We are going to UIL
-----------------------------------------------------------------------------|#

(define-type UIL-Prim  (U Schml-Prim Array-Prim))
(define-type UIL-Prim! (U Schml-Prim! Array-Prim! Print-Prim! Bottom-Prim))
(define-predicate uil-prim-effect? UIL-Prim!)
(define-predicate uil-prim-value? UIL-Prim)

(define-type UIL-Expr-Prim (U Array-Prim IxI->I-Prim ->I-Prim))

(define-type Array-Prim (U 'Alloc 'Array-ref))
(define-type Array-Prim! 'Array-set!)
(define-type Print-Prim! (U 'Printf 'Print))
(define-type Bottom-Prim (U 'Exit))

(define-type (UIL-Op E) (Op UIL-Prim (Listof E)))
(define-type (UIL-Op! E) (Op UIL-Prim! (Listof E)))

