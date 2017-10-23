#lang typed/racket/base
(require "forms.rkt" "primitives.rkt")
(provide (all-from-out "forms.rkt" "primitives.rkt")
         (all-defined-out))
#|-----------------------------------------------------------------------------+
| Language/Grift1 created by type-check
+-----------------------------------------------------------------------------|#

(define-type Grift1-Lang
  (Prog (List String Natural Grift-Type) (Listof S1-Top)))

(define-type S1-Top
  (U (Define Boolean Uid Grift-Type S1-Expr)
     (Observe S1-Expr Grift-Type)))

(define-type S1-Expr
  ;; This slightly complicated formulation of lambda's Structure allows me
  ;; To rely on lambdas to have function types during cast insertion
  (Rec E (U (Ann (Lambda Grift-Fml* E) (Pair Src Grift-Fn-Type))
            (Ann (U
                  (Letrec S1-Bnd* E)
                  (Let S1-Bnd* E)
                  (App E (Listof E))
                  (Op (Ann Grift-Primitive Grift-Type*) (Listof E))
                  (If E E E)
                  (Switch E (Switch-Case* E) E)
                  (Ascribe E Grift-Type (Option Blame-Label))
                  (Var Uid)
                  (Quote Grift-Literal)
                  (Begin (Listof E) E)
                  (Repeat Uid E E Uid E E)
                  ;; Monotonic effects
                  (Mbox E Grift-Type)
                  (Munbox E)
                  (Mbox-set! E E)
                  (Mvector E E Grift-Type)
                  (Mvector-ref E E)
                  (Mvector-set! E E E)
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
                 (Pair Src Grift-Type)))))

(define-type S1-Bnd (Bnd Uid Grift-Type S1-Expr))
(define-type S1-Bnd* (Listof S1-Bnd))

