#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt"
         "data1.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data2-Language created by remove-let                                          |
+-----------------------------------------------------------------------------|#

(define-type Data2-Lang
  (Prog (List String Natural Schml-Type)
        (GlobDecs Uid*
                    (Labels D2-Bnd-Code*
                            D2-Body))))

(define-type D2-Body (Locals Uid* D1-Tail))
(define-type D2-Bnd-Code* (Listof D2-Bnd-Code))
(define-type D2-Bnd-Code (Pairof Uid D2-Code))
(define-type D2-Code (Code Uid* D2-Body))

(define-type D2-Tail D1-Tail #;
  (Rec T
   (U (If D2-Pred T T)
      (Begin D2-Effect* T)
      D2-Value
      Success)))

(define-type D2-Value D1-Value #;
 (Rec V
  (U (If D2-Pred V V)
     (Begin D2-Effect* V)
     (App-Code V (Listof V))
     (Op UIL-Expr-Prim (Listof V))
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D2-Literal))))

(define-type D2-Pred D1-Pred #;
 (Rec P
  (U (If D2-Pred P P)
     (Begin D2-Effect* P)
     (Relop IxI->B-Prim D2-Value D2-Value))))

(define-type D2-Effect D1-Effect #;
 (Rec E
  (U (If D2-Pred E E)
     (Begin D2-Effect* No-Op)
     (Repeat Uid D2-Value D2-Value #f #f E)
     (App-Code D2-Value D2-Value*)
     (UIL-Op! D2-Value)
     (Assign Uid D2-Value)
     No-Op)))


(define-type D2-Value* (Listof D2-Value))

(define-type D2-Effect* (Listof D2-Effect))

(define-type D2-Literal Data-Literal)

