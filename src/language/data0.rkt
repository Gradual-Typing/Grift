#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data0-Language created by impose-cast-semantics / specify-representation     | 
+-----------------------------------------------------------------------------|#
(define-type Data0-Lang
  (Prog (List String Natural Schml-Type) D0-Expr))

(define-type D0-Bnd-Code* (Listof D0-Bnd-Code))
(define-type D0-Bnd-Code (Pairof Uid D0-Code))
(define-type D0-Code (Code Uid* D0-Expr))

(define-type D0-Expr
  (Rec E (U (Labels D0-Bnd-Code* E)
            (Let D0-Bnd* E)
	    (App-Code E (Listof E))
            (UIL-Op! E)
            (UIL-Op E)
	    (If E E E)
	    (Begin D0-Expr* E)
            (Repeat Uid E E E)
	    (Var Uid)
	    (Code-Label Uid)
	    (Quote D0-Literal)
            Halt
            Success)))

(define-type D0-Expr* (Listof D0-Expr))
(define-type D0-Bnd* (Listof D0-Bnd))
(define-type D0-Bnd  (Pairof Uid D0-Expr))
(define-type D0-Literal Data-Literal)
