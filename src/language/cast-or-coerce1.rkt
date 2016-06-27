#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#| ----------------------------------------------------------------------------+
|Cast1                                                                         |
------------------------------------------------------------------------------|#

(define-type Cast-or-Coerce1-Lang
  (Prog (List String Natural Schml-Type) CoC1-Expr))

(define-type CoC1-Expr
  (Rec E (U ;; Non-Terminals
          (Labels CoC1-Bnd-Code* E)
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec CoC1-Bnd* E)
	  (Let CoC1-Bnd* E)
          (App-Code E (Listof E))
          (App-Fn E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin CoC1-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
          (Quote-Coercion Schml-Coercion)
          (Type Schml-Type)
	  (Quote Cast-Literal)
          (Code-Label Uid)
          ;; Coecions Currently Exposed as Expressions
          (Id-Coercion-Huh E)
          (Fn-Coercion (Listof E) E)
          (Fn-Coercion-Arg E E)
          (Fn-Coercion-Return E)
          ;; Casts with different ways of getting the same semantics

          (Interpreted-Cast E (Coercion E))
          (Interpreted-Cast E (Twosome E E E))
	  (Cast E (Twosome Schml-Type Schml-Type Blame-Label))
          (Cast E (Coercion Schml-Coercion))
	  (Fn-Caster E)
          (Compose-Coercions E E)
          ;;
          (App/Fn-Proxy-Huh E (Listof E))
          (Fn-Proxy Index E E)
          (Fn-Proxy-Huh E)
          (Fn-Proxy-Closure E)
          (Fn-Proxy-Coercion E)
          ;; FN-Type operations
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          ;; Observations
          (Blame E)
          ;; Guarded Intermediate Representation
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E)
          ;; Dynamic Operations
          (Dyn-GVector-Set! E E E Schml-Type Blame-Label)
          (Dyn-GVector-Ref E E Blame-Label)
          (Dyn-GRef-Set! E E Schml-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-Fn-App E CoC1-Expr* Schml-Type* Blame-Label))))

(define-type CoC1-Expr* (Listof CoC1-Expr))
(define-type CoC1-Bnd   (Pairof Uid CoC1-Expr))
(define-type CoC1-Bnd*  (Listof CoC1-Bnd))
(define-type CoC1-Bnd-Code* (Listof CoC1-Bnd-Code))
(define-type CoC1-Bnd-Code  (Pairof Uid CoC1-Code))
(define-type CoC1-Code (Code (Listof Uid) CoC1-Expr))
