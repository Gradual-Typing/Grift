#lang typed/racket/base
(require "forms.rkt" "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#| ----------------------------------------------------------------------------+
|Cast1                                                                         |
------------------------------------------------------------------------------|#

(define-type Cast-or-Coerce1-Lang
  (Prog (List String Natural Grift-Type) CoC1-Expr))

(define-type CoC1-Expr
  (Rec E (U ;; Non-Terminals
          (Labels CoC1-Bnd-Code* E)
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec CoC1-Bnd* E)
	  (Let CoC1-Bnd* E)
          (App-Code E (Listof E))
          (App-Fn E (Listof E))
	  (Op Grift-Primitive (Listof E))
	  (If E E E)
          (Switch E (Switch-Case* E) E)
          ;; Terminals
          (Observe E Grift-Type)
          No-Op
          (Begin CoC1-Expr* E)
          (Repeat Uid E E Uid E E)
	  (Var Uid)
          (Quote-Coercion Grift-Coercion)
          (Type Grift-Type)
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
	  (Cast E (Twosome Grift-Type Grift-Type Blame-Label))
          (Cast E (Coercion Grift-Coercion))
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
          (Gvector-length E)
          ;; Monotonic
          (Mbox E Grift-Type)
          (Munbox E) ;; fast read
          (Mbox-set! E E) ;; fast write
          (MBoxCastedRef E Grift-Type)
          (MBoxCastedSet! E E Grift-Type)
          (Mvector E E Grift-Type)
          (Mvector-ref E E) ;; fast read
          (Mvector-set! E E E) ;; fast write
          (MVectCastedRef E E Grift-Type)
          (MVectCastedSet! E E E Grift-Type)
          (Mvector-length E)
          ;; Dynamic Operations
          (Dyn-Tuple-Proj E E E)
          (Dyn-GVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-GVector-Ref E E Blame-Label)
          (Dyn-GVector-Len E E)
          (Dyn-GRef-Set! E E Grift-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-MVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-MVector-Ref E E Blame-Label)
          (Dyn-MRef-Set! E E Grift-Type Blame-Label)
          (Dyn-MRef-Ref E Blame-Label)
          (Dyn-Fn-App E CoC1-Expr* Grift-Type* Blame-Label)
          ;;
          (Create-tuple (Listof E))
          (Tuple-proj E Index))))

(define-type CoC1-Expr* (Listof CoC1-Expr))
(define-type CoC1-Bnd   (Pairof Uid CoC1-Expr))
(define-type CoC1-Bnd*  (Listof CoC1-Bnd))
(define-type CoC1-Bnd-Code* (Listof CoC1-Bnd-Code))
(define-type CoC1-Bnd-Code  (Pairof Uid CoC1-Code))
(define-type CoC1-Code (Code (Listof Uid) CoC1-Expr))
