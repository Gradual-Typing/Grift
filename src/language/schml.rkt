#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base)

(define-syntax (define-contracts stx)
  (syntax-parse stx
    [(_  (name*:id other*) ...)
     #'(define-values (name* ...)
         (flat-murec-contract
          ([name* other*] ...)
          (values name* ...)))]))

(define (?/c x/c) (or/c false/c x/c))
(define nat/c exact-nonnegative-integer?)
(define blame-label? string?)
(define (Switch-Case*/c lhs)
  (listof (cons/c (listof exact-integer?) lhs)))

(require "forms.rkt")
(define-contracts
  ;; TODO now that we are out of typed-racket we can delete Ann2
  [G0-Bnd*/c (listof (Bnd/c Uid? (?/c type?) G0-Ann-Expr/c))]
  [G0-Expr/c
   (or/c
    (Lambda/c (Fml/c Uid? (?/c type?)) (list/c G0-Ann-Expr/c (?/c type?)))
    (Letrec/c G0-Bnd*/c G0-Ann-Expr/c)
    (Let/c G0-Bnd*/c G0-Ann-Expr/c)
    (App/c G0-Ann-Expr/c G0-Ann-Expr*/c)
    (Op/c primitive? G0-Ann-Expr*/c)
    (If/c G0-Ann-Expr/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Switch/c G0-Ann-Expr/c (Switch-Case*/c G0-Ann-Expr/c) G0-Ann-Expr/c)
    (Ascribe/c G0-Ann-Expr/c type? (?/c blame-label?))
    (Var/c Uid?)
    (Quote/c grift-literal?)
    (Begin/c G0-Ann-Expr*/c G0-Ann-Expr/c)
    (Repeat/c Uid? G0-Ann-Expr/c G0-Ann-Expr/c (list/c Uid? (?/c type?)) G0-Ann-Expr/c G0-Ann-Expr/c)

    (MboxS/c G0-Ann-Expr/c)
    (Munbox/c G0-Ann-Expr/c)
    (Mbox-set!/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (MvectorS/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Mvector-set!/c G0-Ann-Expr/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Mvector-ref/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Mvector-length/c G0-Ann-Expr/c)
    (Gbox/c G0-Ann-Expr/c)
    (Gunbox/c G0-Ann-Expr/c)
    (Gbox-set!/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Gvector/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Gvector-set!/c G0-Ann-Expr/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Gvector-ref/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Gvector-length/c G0-Ann-Expr/c)
    (Create-tuple/c G0-Ann-Expr*/c)
    (Tuple-proj/c G0-Ann-Expr/c nat/c)
    )]
  [G0-Expr*/c (listof G0-Expr/c)]
  [G0-Ann-Expr/c (Ann/c G0-Expr/c srcloc?)]
  [G0-Ann-Expr*/c (listof G0-Ann-Expr/c)]
  [G0-Top/c
   (Ann2/c (or/c (Observe/c G0-Ann-Expr/c (?/c type?))
                 (Define/c boolean? Uid? (?/c type?) G0-Ann-Expr/c))
           srcloc?)]
  [G0-Top*/c (listof G0-Top/c)]
  [Grift-Lang/c (Prog/c (list/c string? nat/c) G0-Top*/c)])


#|

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

|#
