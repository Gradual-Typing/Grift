#lang typed/racket/base
(require "forms.rkt" "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Coercion created by cast->coercions
+------------------------------------------------------------------------------+
| Description: This language represents all casts as equivalent coercions.
| Subsequent pass languages may represent cast as coercions or casts but both
| are not permitted. The types of various passes cannot enforce this but latter
| passes should enforce this constraint dynamically.
+-----------------------------------------------------------------------------|#

(define-type Coercion-Lang
  (Prog (List String Natural Grift-Type) Crcn-Expr))

(define-type Crcn-Expr
  (Rec E (U ;; Non-Terminals
          ;; replaced (Cast E Grift-Type Grift-Type Label) with next line
          (Observe E Grift-Type)
          No-Op
          (Cast E (Coercion Grift-Coercion))
          (Lambda Uid* E)
	  (Letrec Crcn-Bnd* E)
	  (Let Crcn-Bnd* E)
	  (App E (Listof E))
	  (Op Grift-Primitive (Listof E))
	  (If E E E)
          (Switch E (Switch-Case* E) E)
          (Begin Crcn-Expr* E)
          (Repeat Uid E E Uid E E)
          ;; Guarded effects
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E)
          (Gvector-length E)
          ;; Monotonic references
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
          ;;
          (Create-tuple (Listof E))
          (Tuple-proj E Index)
	  ;; Terminals
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Dynamic Operations
          (Dyn-Tuple-Proj E E E)
          (Dyn-GVector-Len E E)
          (Dyn-GVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-GVector-Ref E E Blame-Label)
          (Dyn-GRef-Set! E E Grift-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-MVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-MVector-Ref E E Blame-Label)
          (Dyn-MRef-Set! E E Grift-Type Blame-Label)
          (Dyn-MRef-Ref E Blame-Label)
          (Dyn-Fn-App E Crcn-Expr* Grift-Type* Blame-Label)
          )))

(define-type Crcn-Expr* (Listof Crcn-Expr))
(define-type Crcn-Bnd   (Pairof Uid Crcn-Expr))
(define-type Crcn-Bnd*  (Listof Crcn-Bnd))

