#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast created by label-lambdas                    |
+-----------------------------------------------------------------------------|#

(define-type Cast4-Lang
  (Prog (List String Natural Schml-Type) C4-Expr))

(define-type C4-Expr
  (Rec E (U ;; Non-Terminals
          (Letrec C4-Bnd-Lambda* E)
	  (Let C4-Bnd-Data* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C4-Expr* E)
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

(define-type C4-Expr* (Listof C4-Expr))
(define-type C4-Bnd-Lambda* (Listof C4-Bnd-Lambda))
(define-type C4-Bnd-Lambda  (Pairof Uid C4-Lambda))
(define-type C4-Bnd-Data* (Listof C4-Bnd-Data))
(define-type C4-Bnd-Data  (Pairof Uid C4-Expr))
(define-type C4-Lambda (Lambda Uid* (Castable (Option Uid) C4-Expr)))

