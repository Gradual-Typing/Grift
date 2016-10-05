#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Cast0 created by insert-casts                                       |
+------------------------------------------------------------------------------+
| Description: At the begining of this section of the compiler all cast in the |
| ast are performed on known schml language types. But as the compiler imposes |
| the semantics of cast there become situations where a type is dependant on   |
| econtents of a variable. At this point casts are no longer able to be        |
| completely compiled into primitives. These casts require a sort of cast      |
| interpreter which is built later.                                            |
| In general this compiler tries to move as mainy casts into the primitive     |
| operations. Whatever casts are left at the end are then convert to           |
| applications of the cast interpreter function.                               |
+-----------------------------------------------------------------------------|#

(define-type Cast-or-Coerce0-Lang
  (Prog (List String Natural Schml-Type) CoC0-Expr))

(define-type CoC0-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* E)
	  (Letrec CoC0-Bnd* E)
	  (Let CoC0-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Switch E (Switch-Case* E) E)
          (Cast E (Twosome Schml-Type Schml-Type Blame-Label))
          (Cast E (Coercion Schml-Coercion))
          (Begin CoC0-Expr* E)
          (Repeat Uid E E Uid E E)
          ;; Guarded effects
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E)
          ;; Monotonic
          (Mbox E Schml-Type)
          (Munbox E) ;; fast read
          (Mbox-set! E E) ;; fast write
          (MBoxCastedRef Uid Schml-Type)
          (MBoxCastedSet! Uid E Schml-Type)
          (Mvector E E Schml-Type)
          (Mvector-ref E E) ;; fast read
          (Mvector-set! E E E) ;; fast write
          (MVectCastedRef Uid E Schml-Type)
          (MVectCastedSet! Uid E E Schml-Type)
          ;; Dynamic Operations
          (Dyn-GVector-Set! E E E Schml-Type Blame-Label)
          (Dyn-GVector-Ref E E Blame-Label)
          (Dyn-GRef-Set! E E Schml-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-Fn-App E CoC0-Expr* Schml-Type* Blame-Label)
          ;;
          (Create-tuple (Listof E))
          (Tuple-proj E Index)
	  ;; Terminals
          (Var Uid)
	  (Quote Cast-Literal))))

(define-type CoC0-Expr* (Listof CoC0-Expr))
(define-type CoC0-Bnd   (Pairof Uid CoC0-Expr))
(define-type CoC0-Bnd*  (Listof CoC0-Bnd))
(define-type CoC0-Bnd-Lam (Pairof Uid (Lambda Uid* CoC0-Expr)))
(define-type CoC0-Bnd-Lam* (Listof CoC0-Bnd-Lam))
