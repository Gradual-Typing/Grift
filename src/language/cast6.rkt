#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast6 created by convert-closures                                   |
+-----------------------------------------------------------------------------|#

(define-type Cast6-Lang
  (Prog (List String Natural Schml-Type) C6-Expr))

(define-type C6-Expr
  (Rec E (U ;; Non-Terminals
          (LetP C6-Bnd-Procedure* (LetC C6-Bnd-Closure* E))
	  (Let C6-Bnd-Data* E)
	  (App (Pair E E) (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C6-Expr* E)
          (Repeat Uid E E E)
          ;; closure operations
          (Closure-code E)
          (Closure-caster E)
          (Closure-ref Uid Uid)
          ;; FN-Type operations
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
	  (Quote Cast-Literal)
          (Code-Label Uid))))

(define-type C6-Expr* (Listof C6-Expr))
(define-type C6-Procedure
  (Procedure Uid Uid* Uid (Option Uid) Uid* C6-Expr))
(define-type C6-Closure (Closure-Data C6-Expr C6-Expr C6-Expr*))
(define-type C6-Bnd-Procedure (Pairof Uid C6-Procedure))
(define-type C6-Bnd-Procedure* (Listof C6-Bnd-Procedure))
(define-type C6-Bnd-Closure (Pairof Uid C6-Closure))
(define-type C6-Bnd-Closure* (Listof C6-Bnd-Closure))
(define-type C6-Bnd-Data (Pairof Uid C6-Expr))
(define-type C6-Bnd-Data* (Listof C6-Bnd-Data))
