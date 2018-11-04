#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data5-Language created by simplify-predicate                                 |
+-----------------------------------------------------------------------------|#

(define-type Data5-Lang
  (Prog (List String Natural Grift-Type)
	(GlobDecs Uid*
                    (Labels D5-Bnd-Code*
                            D5-Body))))

(define-type D5-Body (Locals Uid* (Bnd* Nat) D5-Tail))
(define-type D5-Bnd-Code* (Listof D5-Bnd-Code))
(define-type D5-Bnd-Code (Pairof Uid D5-Code))
(define-type D5-Code (Code Uid* D5-Body))

(define-type D5-Tail
  (Rec T
   (U (If D5-Pred T T)
      (Switch D5-Trivial (Switch-Case* T) T)
      (Begin D5-Effect* T)
      (Return D5-Value)
      (Return Success))))

(define-type D5-Pred (Relop UIL-Pred-Prim D5-Trivial D5-Trivial))

(define-type D5-Effect
  (U (Repeat Uid D5-Trivial D5-Trivial #f #f (Begin D5-Effect* No-Op))
     (If D5-Pred (Begin D5-Effect* No-Op) (Begin D5-Effect* No-Op))
     Break-Repeat
     (Switch D5-Trivial
             (Switch-Case* (Begin D5-Effect* No-Op))
             (Begin D5-Effect* No-Op))
     (UIL-Op! D5-Trivial)
     (Assign Id D5-Value)
     No-Op))

(define-type D5-Value
  (U D5-Trivial
     Halt
     (UIL-Op D5-Trivial)
     (App-Code D5-Trivial D5-Trivial*)
     (If D5-Pred D5-Trivial D5-Trivial)))

(define-type D5-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Global String)
     (Quote D5-Literal)))

(define-type D5-Trivial* (Listof D5-Trivial))
(define-type D5-Effect* (Listof D5-Effect))
(define-type D5-Value* (Listof D5-Value))

(define-type D5-Literal Data-Literal)
