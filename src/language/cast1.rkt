#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#| ----------------------------------------------------------------------------+
|Cast1                                                                         |
------------------------------------------------------------------------------|#

(define-type Cast1-Lang
  (Prog (List String Natural Grift-Type) C1-Expr))

(define-type C1-Expr
  (Rec E (U ;; Non-Terminals
          (Observe E Grift-Type)
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C1-Bnd* E)
	  (Let C1-Bnd* E)
	  (App E (Listof E))
	  (Op Grift-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin C1-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Grift-Type Grift-Type Blame-Label)
	  (Fn-Cast E Grift-Type Grift-Type Blame-Label)
          ;; FN-Type operations
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          ;; Observations
          (Blame E)
          ;; Monotonic
          (Mbox (Ann E (Pair Blame-Label Grift-Type)))
          (Munbox E)
          (Munbox (Ann E (Pair Blame-Label Grift-Type)))
          (Mbox-set! (Ann E (Pair Blame-Label Grift-Type)) E)
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
          (Gvector-ref E E)
          (Dyn-GVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-GVector-Ref! E E Blame-Label)
          (Dyn-GRef-Set! E E Grift-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-MVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-MVector-Ref E E Blame-Label)
          (Dyn-MRef-Set! E E Grift-Type Blame-Label)
          (Dyn-MRef-Ref E Blame-Label)
          (Dyn-Fn-App E C1-Expr* Grift-Type* Blame-Label))))

(define-type C1-Expr* (Listof C1-Expr))
(define-type C1-Bnd   (Pairof Uid C1-Expr))
(define-type C1-Bnd*  (Listof C1-Bnd))
