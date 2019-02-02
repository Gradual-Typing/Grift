#lang typed/racket/base
(require "forms.rkt" "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Cast0 created by insert-casts                                       |
+-----------------------------------------------------------------------------
| Description: At the begining of this section of the compiler all cast in the |
| ast are performed on known grift language types. But as the compiler imposes |
| the semantics of cast there become situations where a type is dependant on   |
| econtents of a variable. At this point casts are no longer able to be        |
| completely compiled into primitives. These casts require a sort of cast      |
| interpreter which is built later.                                            |
| In general this compiler tries to move as mainy casts into the primitive     |
| operations. Whatever casts are left at the end are then convert to           |
| applications of the cast interpreter function.
+-----------------------------------------------------------------------------|#

(define-type Cast0-Lang
  (Prog (List String Natural Grift-Type) C0-Top*))

(define-type C0-Top* (Listof C0-Top))

(define-type C0-Top
  (U (Define Boolean Uid Grift-Type C0-Expr)
     (Observe C0-Expr Grift-Type)))

(define-type Cast0.5-Lang
  (Prog (List String Natural Grift-Type) C0-Expr))

(define-type C0-Expr
  (Rec E (U ;; Non-Terminals
          (Observe E Grift-Type)
          (Lambda Uid* E)
          (Letrec C0-Bnd* E)
          (Let C0-Bnd* E)
          (App E (Listof E))
          (Op Grift-Primitive (Listof E))
          (If E E E)
          (Switch E (Switch-Case* E) E)
          (Cast E (Twosome Grift-Type Grift-Type Blame-Label))
          (Begin C0-Expr* E)
          (Repeat Uid E E Uid E E)
          ;; Guarded effects
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
          (Dyn-GVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-GVector-Ref E E Blame-Label)
          (Dyn-GVector-Len E E)
          (Dyn-GRef-Set! E E Grift-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-MVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-MVector-Ref E E Blame-Label)
          (Dyn-MRef-Set! E E Grift-Type Blame-Label)
          (Dyn-MRef-Ref E Blame-Label)
          (Dyn-Fn-App E C0-Expr* Grift-Type* Blame-Label)
          (Dyn-Tuple-Proj E E E)
          (Create-tuple (Listof E))
          (Tuple-proj E Index)
          ;; Terminals
          (Var Uid)
          (Quote Cast-Literal)
          No-Op)))

(define-type C0-Expr* (Listof C0-Expr))
(define-type C0-Bnd   (Pair Uid C0-Expr))
(define-type C0-Bnd*  (Listof C0-Bnd))
