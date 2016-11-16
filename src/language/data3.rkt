#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data3-Language created by remove-complex-opera                               |
+-----------------------------------------------------------------------------|#

(define-type Data3-Lang
  (Prog (List String Natural Schml-Type)
	(GlobDecs Uid*
                    (Labels D3-Bnd-Code*
                            D3-Body))))

(define-type D3-Body (Locals Uid* D3-Tail))
(define-type D3-Bnd-Code* (Listof D3-Bnd-Code))
(define-type D3-Bnd-Code (Pairof Uid D3-Code))
(define-type D3-Code (Code Uid* D3-Body))

(define-type D3-Tail
  (Rec T
   (U (If D3-Pred T T)
      (Switch D3-Trivial (Switch-Case* T) T)
      (Begin D3-Effect* T)
      (Return D3-Value)
      (Return Success))))

(define-type D3-Value
 (Rec V
  (U D3-Trivial
     Halt
     (If D3-Pred V V)
     (Switch D3-Trivial (Switch-Case* V) V)
     (Begin D3-Effect* V)
     (Op UIL-Expr-Prim (Listof D3-Trivial))
     (App-Code D3-Trivial D3-Trivial*))))

(define-type D3-Pred
 (Rec P
      (U (If D3-Pred P P)
         (Switch D3-Trivial (Switch-Case* P) P)
         (Begin D3-Effect* P)
         (Relop UIL-Pred-Prim D3-Trivial D3-Trivial))))

(define-type D3-Effect
 (Rec E
      (U (If D3-Pred E E)
         (Switch D3-Trivial (Switch-Case* E) E)
         (Begin D3-Effect* No-Op)
         (Repeat Uid D3-Trivial D3-Trivial #f #f E)
         Break-Repeat
         (App-Code D3-Trivial D3-Trivial*)
         (UIL-Op! D3-Trivial)
         (Assign Uid D3-Value)
         No-Op)))

;; (TODO halt is not trivial though because we are targeting c it may be treated so)
;; remove Halt earlier
(define-type D3-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Quote D3-Literal)))

(define-type D3-Value* (Listof D3-Value))
(define-type D3-Trivial* (Listof D3-Trivial))
(define-type D3-Effect* (Listof D3-Effect))

(define-type D3-Literal Data-Literal)

