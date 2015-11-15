#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data1-Language created by normalize-context                                          |
+-----------------------------------------------------------------------------|#

(define-type Data1-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D1-Bnd-Code*
		D1-Tail)))

(define-type D1-Bnd-Code* (Listof D1-Bnd-Code))
(define-type D1-Bnd-Code (Pairof Uid D1-Code))
(define-type D1-Code (Code Uid* D1-Tail))

(define-type D1-Tail
  (Rec T
   (U (Let D1-Bnd* T)
      (If D1-Pred T T)
      (Begin D1-Effect* T)
      (App-Code D1-Value D1-Value*)
      (Op (U IxI->I-Prim Array-Prim) D1-Value*)
      (Var Uid)
      Halt Success
      (Var Uid)
      (Code-Label Uid)
      (Quote D1-Literal))))

(define-type D1-Value
 (Rec V
  (U (Let D1-Bnd* V)
     (If D1-Pred V V)
     (Begin D1-Effect* V)
     (App-Code V (Listof V))
     (Op (U IxI->I-Prim Array-Prim) (Listof V))
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D1-Literal))))

(define-type D1-Pred
 (Rec P
  (U (Let D1-Bnd* P)
     (If D1-Pred P P)
     (Begin D1-Effect* P)
     (Relop IxI->B-Prim D1-Value D1-Value))))

(define-type D1-Effect
 (Rec E
  (U (Let D1-Bnd* E)
     (If D1-Pred E E)
     (Begin D1-Effect* No-Op)
     (Repeat Uid D1-Value D1-Value E)
     (App-Code D1-Value D1-Value*)
     (UIL-Op! D1-Value)
     No-Op)))


(define-type D1-Value* (Listof D1-Value))
(define-type D1-Effect* (Listof D1-Effect))

(define-type D1-Bnd  (Pairof Uid D1-Value))

(define-type D1-Bnd* (Listof D1-Bnd))

(define-type D1-Literal Data-Literal)
