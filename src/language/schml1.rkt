#lang typed/racket/base
(require "forms.rkt")
(provide (all-from-out "forms.rkt")
         (all-defined-out))
#|-----------------------------------------------------------------------------+
| Language/Schml1 created by type-check
+-----------------------------------------------------------------------------|#

(define-type Schml1-Lang
  (Prog (List String Natural Schml-Type) S1-Expr))

(define-type S1-Expr
  ;; This slightly complicated formulation of lambda's Structure allows me
  ;; To rely on lambdas to have function types during cast insertion
  (Rec E (U (Ann (Lambda Schml-Fml* E) (Pair Src Schml-Fn-Type))
            (Ann (U
                  (Letrec S1-Bnd* E)
                  (Let S1-Bnd* E)
                  (App E (Listof E))
                  (Op (Ann Schml-Primitive Schml-Type*) (Listof E))
                  (If E E E)
                  (Ascribe E Schml-Type (Option Blame-Label))
                  (Var Uid)
                  (Quote Schml-Literal)
                  (Begin (Listof E) E)
                  (Repeat Uid E E Uid E E)
                  ;; Monotonic effects
                  (Mbox E Schml-Type)
                  (Munbox E)
                  (MunboxT E Schml-Type)
                  (Mbox-set! E E)
                  (Mbox-set!T E E Schml-Type)
                  (Mvector E E Schml-Type)
                  (Mvector-ref E E)
                  (Mvector-refT E E Schml-Type)
                  (Mvector-set! E E E)
                  (Mvector-set!T E E E Schml-Type)
                  ;; Guarded effects
                  (Gbox E)
                  (Gunbox E)
                  (Gbox-set! E E)
                  (Gvector E E)
                  (Gvector-set! E E E)
                  (Gvector-ref E E)
                  ;;
                  (Create-tuple (Listof E))
                  (Tuple-proj E Index))
                 (Pair Src Schml-Type)))))

(define-type S1-Bnd (Bnd Uid Schml-Type S1-Expr))
(define-type S1-Bnd* (Listof S1-Bnd))

