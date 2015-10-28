#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#| ----------------------------------------------------------------------------+
|Cast1                                                                         |
------------------------------------------------------------------------------|#

(define-type Cast1-Lang
  (Prog (List String Natural Schml-Type) C1-Expr))

(define-type C1-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C1-Bnd* E)
	  (Let C1-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin C1-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
	  (Fn-Cast E Schml-Type Schml-Type Blame-Label)
          ;; FN-Type operations
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          ;; Observations
          (Blame E)
          ;; Monotonic
          (Mbox (Ann E (Pair Blame-Label Schml-Type)))
          (Munbox E)
          (Munbox (Ann E (Pair Blame-Label Schml-Type)))
          (Mbox-set! (Ann E (Pair Blame-Label Schml-Type)) E)
          (Mbox-set! E E)
          (Mvector E E)
          (Mvector-set! E E E)
          (Mvector-ref E E)
          ;; Guarded Intermediate Representation
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E))))

(define-type C1-Expr* (Listof C1-Expr))
(define-type C1-Bnd   (Pairof Uid C1-Expr))
(define-type C1-Bnd*  (Listof C1-Bnd))
