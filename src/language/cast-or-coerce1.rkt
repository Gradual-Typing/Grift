#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#| ----------------------------------------------------------------------------+
|Cast1                                                                         |
------------------------------------------------------------------------------|#

(define-type Cast-or-Coerce1-Lang
  (Prog (List String Natural Schml-Type)
        (Labels CoC1-Bnd-Code* E)))

(define-type CoC1-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec CoC1-Bnd* E)
	  (Let CoC1-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin CoC1-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
          (Coerce (Coercion Schml-Type Blame-Label) E)
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

(define-type CoC1-Expr* (Listof CoC1-Expr))
(define-type CoC1-Bnd   (Pairof Uid CoC1-Expr))
(define-type CoC1-Bnd*  (Listof CoC1-Bnd))
(define-type CoC1-Bnd-Code* (Listof CoC1-Bnd-Code))
(define-type CoC1-Bnd-Code  (Pairof Uid CoC1-Code))
(define-type CoC1-Code (Code (Listof Uid) CoC1-Expr))
