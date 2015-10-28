#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#


(define-type Cast3-Lang
  (Prog (List String Natural Schml-Type) C3-Expr))

(define-type C3-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C3-Bnd* E)
	  (Let C3-Bnd* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C3-Expr* E)
          (Repeat Uid E E E)
          ;; closure operations
          (Fn-Caster E)
          ;; Type operations
          (Type-tag E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          (Type-GVect-to E)
          ;; Dyn operations
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-type E)
          (Dyn-value E)
          (Dyn-make E E)
          ;; Observational Operations
          (Blame E)
          (Observe E Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep E)
	  (Quote Cast-Literal))))

(define-type C3-Expr* (Listof C3-Expr))
(define-type C3-Bnd   (Pair Uid C3-Expr))
(define-type C3-Bnd*  (Listof C3-Bnd))
