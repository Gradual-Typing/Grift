#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Coercion created by cast->coercions
+------------------------------------------------------------------------------+
| Description: This language represents all casts as equivalent coercions.
| Subsequent pass languages may represent cast as coercions or casts but both
| are not permitted. The types of various passes cannot enforce this but latter
| passes should enforce this constraint dynamically.
+-----------------------------------------------------------------------------|#

(define-type Coercion-Lang (Prog (List String Natural Schml-Type) Crcn-Expr))

(define-type Crcn-Expr
  (Rec E (U ;; Non-Terminals
          ;; replaced (Cast E Schml-Type Schml-Type Label) with next line
          (Cast E (Coercion Schml-Coercion))
          (Lambda Uid* E)
	  (Letrec Crcn-Bnd* E)
	  (Let Crcn-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin Crcn-Expr* E)
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

(define-type Crcn-Expr* (Listof Crcn-Expr))
(define-type Crcn-Bnd   (Pair Uid Crcn-Expr))
(define-type Crcn-Bnd*  (Listof Crcn-Bnd))
