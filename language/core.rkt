#lang racket
(provide (all-defined-out))


(define ground-types '(Fix Bool Dyn))
(define fix-binops '(op:fix:* 
                     op:fix:+ op:fix:-
                     op:fix:and op:fix:or
                     op:fix:>> op:fix:<<))

(struct Prog (name exprs type))
(struct Expr (src type))
(struct Lambda Expr (fmls body))
(struct Var Expr (ident))
(struct App Expr (expr expr*))
(struct Op Expr (prim expr*))
(struct Cast Expr (expr blame))
(struct If Expr (test conseq alt))
(struct Let Expr (dec* body type src))
(struct Const Expr (const))
(struct Bnd (ident type expr))
(struct Fml (ident type))

(define (Constant? x)
  (or (boolean? x) (integer? x)))

(define (implicit-core? x)
  (define (expr? x)
    (and (Expr? x) (srcloc? (Expr-src x))
         (match x
           [(Lambda s t f b) (and (fmls? f) (expr? b))]
           [(Var s t i) (and (not t) (uvar? i))]
           [(App s t e e*) (and (not t) (expr? e) (andmap expr? e*))]
           [(Op s t o e*) (and (not t) (prim? o) (andmap expr? e*))]
           [(If s t c r a) (and (not t) (expr? c) (expr? r) (expr? a))]
           [(Let s t b* b) (and (not t) (bnd? b*) (expr? b))]
           [(Const s t k) (and (not t) (Constant? k))]
           [(Cast s t e b) (and (not t) (expr? e) (string? b))]
           [otherwise #f])))
  (define (fmls? x)
    (and (list? x)
         (match x)))
  (and (Prog? x)
       (string? (Prog-name x))
       (list? (Prog-exprs x))
       (andmap (lambda (x) (and (Expr? x)
                           (or ()))) )))

(define (explicit-core? x) #t)

#|
(define explicit-core
  (grammar->Language?
   (Terminal (Str string?)
             (U Uvar?)
             (K Constant?)
             (S Src?)
             (P Prim?)
             (B Blame?)
             (BS BindSet?)
             (T Type?))
   (Prog ((Prog Str Expr* T)))
   (Expr ((Lambda Uvar* T Expr S)
          (Var Uvar T S)
          (App Expr Expr* T S)
          (Op P Expr* T S)
          (Cast Expr T T B)
          (If Expr Expr Expr T S)
          (Let BS Expr T S)
          (Const K T S)))))
 

(define (explicit-core? x)
  (define (gmr:star x? x)
    (and (list? f2) (andmap grm:TExpr? f2)))
  (define (gmr:Prog? x)
    (match x
      [(Prog f1 f2) (and (grmr:Str? f1) (grmr:star grmr:TExpr? f2))]
      [a #f]))
  (define (gmr:TExpr? x)
    (and (grmr:Expr? x)))
  (define (grmr:Expr x)
    (match x
      [(Lambda f1 f2 f3 f4) (and (grmr:fml*))]
      [(Var f1 f2 f3)]
      [(App f1 f2 f3 f4)]
      [(Op f1 f2 f3 f4)]
      [(Cast f1 f2 f3)]
      [(If f1 f2 f3 f4 f5)]
      [(Let f1 f2 f3 f4)]
      [(Const f1 f2 f3)]))
  (grmr:Prog? x))
|#




