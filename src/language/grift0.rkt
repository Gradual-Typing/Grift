#lang typed/racket/base

(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"
                       "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Grift-Syntax this is a program returned from grift/read
+-----------------------------------------------------------------------------|#
(define-type Syntax-Lang (Prog String (Listof Any)))



#|-----------------------------------------------------------------------------+
| Language/Grift0 this is the language returned by grift/syntax->grift0
+-----------------------------------------------------------------------------|#
(define-type Grift0-Lang
  (Prog (List String Natural) (Listof S0-Top)))

(define-type S0-Top
  (U (Observe S0-Form (Option Grift-Type))
     (Define Boolean Uid (Option Grift-Type) S0-Form)))

(define-type (S0-Form E)
  (U (Lambda Grift-Fml* (Ann E (Option Grift-Type)))
     (Letrec S0-Bnd* E)
     (Let S0-Bnd* E)
     (App E (Listof E))
     (Op Grift-Primitive (Listof E))
     (If E E E)
     (Switch E (Listof (Pair (Listof Integer) E)) E)
     (Ascribe E Grift-Type (Option Blame-Label))
     (Var Uid)
     (Quote Grift-Literal)
     (Begin (Listof E) E)
     (Repeat Uid E E (Ann Uid (Option Grift-Type)) E E)
     ;; Monotonic effects
     (MboxS E)
     (Munbox E)
     (Mbox-set! E E)
     (MvectorS E E)
     (Mvector-set! E E E)
     (Mvector-ref E E)
     (Mvector-length E)
     ;; Guarded effects
     (Gbox E)
     (Gunbox E)
     (Gbox-set! E E)
     (Gvector E E)
     (Gvector-set! E E E)
     (Gvector-ref E E)
     (Gvector-length E)
     ;;
     (Create-tuple (Listof E))
     (Tuple-proj E Index)))

(define-type S0-Expr
  (Rec E (Ann (S0-Form E) Src)))

(define-type S0-Expr* (Listof S0-Expr))
(define-type S0-Bnd (Bnd Uid Grift-Type? S0-Expr))
(define-type S0-Bnd* (Listof S0-Bnd))


