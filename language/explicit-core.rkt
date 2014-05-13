#lang racket
(provide (all-defined-out))
(define ground-types '(Fix Bool Dyn))
(define fix-binops '(op:fix:* 
                     op:fix:+ op:fix:-
                     op:fix:and op:fix:or
                     op:fix:>> op:fix:<<))

(struct Prog (file-name exprs type))
(struct Expr (src type))
(struct Lambda  Expr (fmls body src))
(struct Fml Expr (ident))
(struct Var Expr (ident))
(struct App Expr (expr expr*))
(struct Op Expr (prim expr*))
(struct Cast Expr (expr blame))
(struct If Expr (test conseq alt))
(struct Let Expr (dec* body type src))
(struct Binding Expr (ident expr))
(struct Const Expr (const))

(define (Constant? x)
  (or (boolean? x)
      (fixnum? x)
      (flonum? x)))

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




