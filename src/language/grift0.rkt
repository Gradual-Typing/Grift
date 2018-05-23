#lang typed/racket/base

(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"
                       "primitives.rkt"))
(module+ test
  (require typed/rackunit)
  (provide (all-defined-out)))

#|-----------------------------------------------------------------------------+
| Language/Grift-Syntax this is a program returned from grift/read
+-----------------------------------------------------------------------------|#
(define-type Syntax-Lang (Prog String (Listof Any)))



#|-----------------------------------------------------------------------------+
| Language/Grift0 this is the language returned by grift/syntax->grift0
+-----------------------------------------------------------------------------|#
(define-type Grift0-Lang
  (Prog (List String Natural) (Listof G0-Top)))

(define-type G0-Top
  ;; Using Ann2 here instead of Ann keeps a bug in typed racket
  ;; from getting Ann at different places in the type signature
  ;; confused.
  (Ann2 (U (Observe G0-Ann-Expr (Option Grift-Type))
           (Define Boolean Uid (Option Grift-Type) G0-Ann-Expr))
        srcloc))
(define-type G0-Top* (Listof G0-Top))

(define-type G0-Ann-Expr (Ann G0-Expr Src))
(define-type G0-Ann-Expr* (Listof G0-Ann-Expr))

(define-type G0-Expr
  (U (Lambda Grift-Fml* (List G0-Ann-Expr (Option Grift-Type)))
     (Letrec G0-Bnd* G0-Ann-Expr)
     (Let G0-Bnd* G0-Ann-Expr)
     (App G0-Ann-Expr G0-Ann-Expr*)
     (Op Grift-Primitive G0-Ann-Expr*)
     (If G0-Ann-Expr G0-Ann-Expr G0-Ann-Expr)
     (Switch G0-Ann-Expr (Switch-Case* G0-Ann-Expr) G0-Ann-Expr)
     (Ascribe G0-Ann-Expr Grift-Type (Option Blame-Label))
     (Var Uid)
     (Quote Grift-Literal)
     (Begin G0-Ann-Expr* G0-Ann-Expr)
     (Repeat Uid G0-Ann-Expr G0-Ann-Expr (List Uid (Option Grift-Type)) G0-Ann-Expr G0-Ann-Expr)
     ;; Monotonic effects
     (MboxS G0-Ann-Expr)
     (Munbox G0-Ann-Expr)
     (Mbox-set! G0-Ann-Expr G0-Ann-Expr)
     (MvectorS G0-Ann-Expr G0-Ann-Expr)
     (Mvector-set! G0-Ann-Expr G0-Ann-Expr G0-Ann-Expr)
     (Mvector-ref G0-Ann-Expr G0-Ann-Expr)
     (Mvector-length G0-Ann-Expr)
     ;; Guarded effects
     (Gbox G0-Ann-Expr)
     (Gunbox G0-Ann-Expr)
     (Gbox-set! G0-Ann-Expr G0-Ann-Expr)
     (Gvector G0-Ann-Expr G0-Ann-Expr)
     (Gvector-set! G0-Ann-Expr G0-Ann-Expr G0-Ann-Expr)
     (Gvector-ref G0-Ann-Expr G0-Ann-Expr)
     (Gvector-length G0-Ann-Expr)
     ;;
     (Create-tuple G0-Ann-Expr*)
     (Tuple-proj G0-Ann-Expr Index)))
(define-type G0-Expr* (Listof G0-Expr))

(define-type G0-Bnd (Bnd Uid Grift-Type? G0-Ann-Expr))
(define-type G0-Bnd* (Listof G0-Bnd))


(module+ test
  (define P0
    (Prog '("0.grift" 0)
      (list
       (Ann2
        (Observe
         (Ann
          (Ascribe
           (Ann (Quote '()) (srcloc "0.grift" 1 5 6 2))
           (Mu (Scope (TVar 0)))
           #f)
          (srcloc "0.grift" 1 0 1 18))
         #f)
        (srcloc "0.grift" 1 0 1 18)))))
  
  (cast P0 Grift0-Lang)
  
  (define-syntax-rule (test-g0 id)
    (test-not-exn "Grift 0 Language" (lambda () (cast id Grift0-Lang))))

  (test-g0 P0))
