#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast2 created by introduce-castable-references                      |
+-----------------------------------------------------------------------------|#

(define-type Cast2-Lang
 (Prog (List String Natural Grift-Type) C2-Expr))

(define-type C2-Expr
  (Rec E (U ;; Non-Terminals
          (Observe E Grift-Type)
          (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C2-Bnd* E)
	  (Let C2-Bnd* E)
	  (App E (Listof E))
          (Op Grift-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin C2-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
          (Type Grift-Type)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Grift-Type Grift-Type Blame-Label)
	  (Fn-Cast E Grift-Type Grift-Type Blame-Label)
          ;; FN-Type operations
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          ;; Observations
          (Blame E)
          ;; Guarded Representation
          (GRep E))))

(define-type C2-Expr* (Listof C2-Expr))
(define-type C2-Bnd (Pairof Uid C2-Expr))
(define-type C2-Bnd* (Listof C2-Bnd))


