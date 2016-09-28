#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data4-Language created by flatten-assignments                                |
+-----------------------------------------------------------------------------|#

(define-type Data4-Lang
  (Prog (List String Natural Schml-Type)
	(GlobDecs Uid*
                    (Labels D4-Bnd-Code*
                            D4-Body))))

(define-type D4-Body (Locals Uid* D4-Tail))
(define-type D4-Bnd-Code* (Listof D4-Bnd-Code))
(define-type D4-Bnd-Code (Pairof Uid D4-Code))
(define-type D4-Code (Code Uid* D4-Body))

(define-type D4-Tail
  (Rec T
   (U (If D4-Pred T T)
      (Switch D4-Trivial (Switch-Case* T) T)
      (Begin D4-Effect* T)
      (Return D4-Value)
      (Return Success))))

(define-type D4-Pred
 (Rec P
      (U (If D4-Pred P P)
         (Switch D4-Trivial (Switch-Case* P) P)
         (Begin D4-Effect* P)
         (Relop IxI->B-Prim D4-Trivial D4-Trivial))))

(define-type D4-Effect
 (Rec E
      (U (If D4-Pred E E)
         (Switch D4-Trivial (Switch-Case* E) E)
         (Begin D4-Effect* No-Op)
         (Repeat Uid D4-Trivial D4-Trivial #f #f E)
         (UIL-Op! D4-Trivial)
         (Assign Uid D4-Value)
         No-Op)))

(define-type D4-Value
  (U D4-Trivial
     Halt
     (UIL-Op D4-Trivial)
     (App-Code D4-Trivial D4-Trivial*)))

(define-type D4-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Quote D4-Literal)))


(define-type D4-Trivial* (Listof D4-Trivial))
(define-type D4-Effect* (Listof D4-Effect))

(define-type D4-Literal Data-Literal)
