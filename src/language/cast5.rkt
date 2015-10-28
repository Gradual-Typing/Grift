#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast5 created by label-lambdas                    |
+-----------------------------------------------------------------------------|#

(define-type Cast5-Lang
  (Prog (List String Natural Schml-Type) C5-Expr))

(define-type C5-Expr
  (Rec E (U ;; Non-Terminals
          (Letrec C5-Bnd-Lambda* E)
	  (Let C5-Bnd-Data* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C5-Expr* E)
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
          (Dyn-make E E) ;; This is bad and I do not like it
          ;; Observational Operations
          (Blame E)
          (Observe E Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep E)
	  (Quote Cast-Literal))))

(define-type C5-Expr* (Listof C5-Expr))
(define-type C5-Bnd-Lambda* (Listof C5-Bnd-Lambda))
(define-type C5-Bnd-Lambda  (Pairof Uid C5-Lambda))
(define-type C5-Bnd-Data* (Listof C5-Bnd-Data))
(define-type C5-Bnd-Data  (Pairof Uid C5-Expr))
(define-type C5-Lambda (Lambda Uid* (Free (Option Uid) Uid* C5-Expr)))

