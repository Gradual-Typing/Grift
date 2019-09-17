#lang racket/base

(require 
 racket/contract/base
 "./forms.rkt"
 "./primitives.rkt"
 "./contracts.rkt")

(provide (all-defined-out))

(define-contracts
  [G0-Bnd*/c (listof (Bnd/c Uid? (?/c type?) G0-Ann-Expr/c))]
  [G0-Expr/c
   (or/c
    (Lambda/c (listof (Fml/c Uid? (?/c type?)))
              (list/c G0-Ann-Expr/c (?/c type?)))
    (Letrec/c G0-Bnd*/c G0-Ann-Expr/c)
    (Let/c G0-Bnd*/c G0-Ann-Expr/c)
    (App/c G0-Ann-Expr/c G0-Ann-Expr*/c)
    (Op/c grift-primitive? G0-Ann-Expr*/c)
    (If/c G0-Ann-Expr/c G0-Ann-Expr/c G0-Ann-Expr/c)
    (Switch/c G0-Ann-Expr/c (Switch-Case*/c G0-Ann-Expr/c) G0-Ann-Expr/c)
    (Ascribe/c G0-Ann-Expr/c type? (?/c blame-label?))
    (Var/c Uid?)
    (Quote/c grift-literal?)
    (Begin/c G0-Ann-Expr*/c G0-Ann-Expr/c)
    (Repeat/c Uid?
              G0-Ann-Expr/c
              G0-Ann-Expr/c
              (list/c Uid? (?/c type?))
              G0-Ann-Expr/c
              G0-Ann-Expr/c)
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
    (Tuple-proj/c G0-Ann-Expr/c nat/c))]
  [G0-Expr*/c (listof G0-Expr/c)]
  [G0-Ann-Expr/c (Ann/c G0-Expr/c srcloc?)]
  [G0-Ann-Expr*/c (listof G0-Ann-Expr/c)]
  [G0-Top/c
   (Ann2/c (or/c (Observe/c G0-Ann-Expr/c (?/c type?))
                 (Define/c boolean? Uid? (?/c type?) G0-Ann-Expr/c))
           srcloc?)]
  [G0-Top*/c (listof G0-Top/c)]
  [Grift-Lang/c (Prog/c (list/c string? nat/c) G0-Top*/c)])


(module+ test
  (require rackunit)

  (check-true (Grift-Lang/c (Prog (list "" 0) '())))

  (define no-src (srcloc #f #f #f #f #f))
  (define forty-two (Quote 42))
  (check-true (G0-Expr/c forty-two))
  (define ann-42 (Ann (Quote 42) no-src))
  (check-true (G0-Ann-Expr/c ann-42))
  (check-true
   (Grift-Lang/c
    (Prog (list "" 0) (list (Ann2 (Observe ann-42 #f) no-src)))))
  (define read-bool (Op 'read-bool '()))
  (check-true (G0-Expr/c read-bool))
  (check-true (grift-primitive? 'read-bool))

  (define ann-f (Ann (Quote #f) (srcloc "lambda1.grift" 1 0 1 14)))
  (check-true (G0-Ann-Expr/c ann-f))
  
  (define ann-lam1
    (Ann (Lambda '() (list ann-f #f)) no-src))
  (check-true (G0-Ann-Expr/c ann-lam1))
  (check-true
   (Grift-Lang/c
    (Prog
     '("lambda1.grift" 0)
     (list
      (Ann2
       (Observe
        ann-lam1
        #f)
       no-src))))))
