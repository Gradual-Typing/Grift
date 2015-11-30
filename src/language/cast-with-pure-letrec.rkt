#lang typed/racket/base

(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

(define-type Cast/Pure-Letrec
  (Prog (List String Natural Schml-Type) C/PL-Expr))

(define-type C/PL-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* E)
	  (Letrec C/PL-Bnd-Lam* E)
	  (Let C/PL-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
	  (Cast E (Twosome Schml-Type Schml-Type Blame-Label))
          (Begin C/PL-Expr* E)
          (Repeat Uid E E E)
          ;; Guarded effects
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E)
	  ;; Terminals
	  (Var Uid)
	  (Quote Cast-Literal))))

(define-type C/PL-Expr* (Listof C/PL-Expr))
(define-type C/PL-Bnd   (Pair Uid C/PL-Expr))
(define-type C/PL-Bnd*  (Listof C/PL-Bnd))
(define-type C/PL-Lam (Lambda Uid* C/PL-Expr))
(define-type C/PL-Bnd-Lam (Pairof Uid C/PL-Lam))
(define-type C/PL-Bnd-Lam* (Listof C/PL-Bnd-Lam))
