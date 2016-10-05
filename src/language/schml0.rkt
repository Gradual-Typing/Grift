#lang typed/racket/base

(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Schml-Syntax this is a program returned from schml/read
+-----------------------------------------------------------------------------|#
(define-type Syntax-Lang (Prog String (Listof Any)))



#|-----------------------------------------------------------------------------+
| Language/Schml0 this is the language returned by schml/syntax->schml0
+-----------------------------------------------------------------------------|#
(define-type Schml0-Lang (Prog (List String Natural) S0-Expr))

(define-type (S0-Form E)
  (U (Lambda Schml-Fml* (Ann E (Option Schml-Type)))
     (Letrec S0-Bnd* E)
     (Let S0-Bnd* E)
     (App E (Listof E))
     (Op Schml-Primitive (Listof E))
     (If E E E)
     (Switch E (Listof (Pair (Listof Integer) E)) E)
     (Ascribe E Schml-Type (Option Blame-Label))
     (Var Uid)
     (Quote Schml-Literal)
     (Begin (Listof E) E)
     (Repeat Uid E E (Ann Uid (Option Schml-Type)) E E)
     ;; Monotonic effects
     (MboxS E)
     (Munbox E)
     (Mbox-set! E E)
     (MvectorS E E)
     (Mvector-set! E E E)
     (Mvector-ref E E)
     ;; Guarded effects
     (Gbox E)
     (Gunbox E)
     (Gbox-set! E E)
     (Gvector E E)
     (Gvector-set! E E E)
     (Gvector-ref E E)
     ;;
     (Create-tuple (Listof E))
     (Tuple-proj E Index)))

(define-type S0-Expr
  (Rec E (Ann (S0-Form E) Src)))

(define-type S0-Expr* (Listof S0-Expr))
(define-type S0-Bnd (Bnd Uid Schml-Type? S0-Expr))
(define-type S0-Bnd* (Listof S0-Bnd))


