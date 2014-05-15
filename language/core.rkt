#lang racket
(require Schml/language/types
         Schml/language/shared)
(provide (all-defined-out))

(struct Prog (name type expr*))
(struct Expr (src type))
(struct Lambda Expr (fmls body))
(struct Var Expr (ident))
(struct App Expr (expr expr*))
(struct Cast Expr (expr blame))
(struct If Expr (test conseq alt))
(struct Let Expr (dec* body))
(struct Const Expr (const))
(struct Bnd (ident type expr))
(struct Fml (ident type))

(struct Prim Expr ())
(struct Prim:Rel Prim ())
(struct Prim:Rel:Int Prim:Rel (fst snd))
(struct Prim:Rel:Int:<  Prim:Rel:Int ())
(struct Prim:Rel:Int:>  Prim:Rel:Int ())
(struct Prim:Rel:Int:=  Prim:Rel:Int ())
(struct Prim:Rel:Int:<= Prim:Rel:Int ())
(struct Prim:Rel:Int:>= Prim:Rel:Int ())
(struct Prim:Bin Prim ())
(struct Prim:Bin:Int Prim:Bin (fst snd))
(struct Prim:Bin:Int:*   Prim:Bin:Int ())
(struct Prim:Bin:Int:+   Prim:Bin:Int ())
(struct Prim:Bin:Int:-   Prim:Bin:Int ())
(struct Prim:Bin:Int:and Prim:Bin:Int ())
(struct Prim:Bin:Int:or  Prim:Bin:Int ())
(struct Prim:Bin:Int:>>  Prim:Bin:Int ())
(struct Prim:Bin:Int:<<  Prim:Bin:Int ())


(define (constant? x)
  (or (boolean? x) (integer? x)))

(define (implicit-core? x)
  (define (expr? x)
    (and (Expr? x) (srcloc? (Expr-src x))
         (match x
           [(Lambda s t f* b) (and (or (not t) (type? t)) (fmls? f*) (expr? b))]
           [(Var s t i) (and (not t) (uvar? i))]
           [(App s t e e*) (and (not t) (expr? e) (andmap expr? e*))]
           [(Prim:Bin:Int s t (? expr?) (? expr?)) #t]
           [(Prim:Rel:Int s t (? expr?) (? expr?)) #t] 
           [(If s t c r a) (and (not t) (expr? c) (expr? r) (expr? a))]
           [(Let s t b* b) (and (not t) (bnds? b*) (expr? b))]
           [(Const s t k) (and (not t) (constant? k))]
           [(Cast s t e b) (and (not t) (expr? e) (string? b))]
           [otherwise #f])))
  (define (fmls? x)
    (define (fml? x) (and (uvar? (Fml-ident x)) (type? (Fml-type x))))
    (and (list? x) (andmap fml? x)))
  (define (bnds? x)
    (define (bnd? x)
      (and (uvar? (Bnd-ident x)) (or (not (Bnd-type x)) (type? (Bnd-type x)))
           (expr? (Bnd-expr x))))
    (and (list? x) (andmap bnd? x)))
  (and (Prog? x) (string? (Prog-name x)) (list? (Prog-expr* x))
       (andmap expr? (Prog-expr* x))))

(define (explicit-core? x)
  (define (expr? x)
    (and (Expr? x) (srcloc? (Expr-src x))
         (match x
           [(Lambda s t f b) (and (type? t) (fmls? f) (expr? b))]
           [(Var s t i) (and (type? t) (uvar? i))]
           [(App s t e e*) (and (type? t) (expr? e) (andmap expr? e*))]
           [(Prim:Bin:Int s t (? expr?) (? expr?)) #t]
           [(Prim:Rel:Int s t (? expr?) (? expr?)) #t] 
           [(If s t c r a) (and (type? t) (expr? c) (expr? r) (expr? a))]
           [(Let s t b* b) (and (type? t) (bnds? b*) (expr? b))]
           [(Const s t k) (and (type? t) (constant? k))]
           [(Cast s t e b) (and (type? t) (expr? e) (string? b))]
           [otherwise #f])))
  (define (fmls? x)
    (define (fml? x) (and (uvar? (Fml-ident x)) (type? (Fml-type x))))
    (and (list? x) (andmap fml? x)))
  (define (bnds? x)
    (define (bnd? x)
      (and (uvar? (Bnd-ident x)) (type? (Bnd-type x)) (expr? (Bnd-expr x))))
    (and (list? x) (andmap bnd? x)))
  (and (Prog? x) (string? (Prog-name x)) (list? (Prog-expr* x))
       (andmap expr? (Prog-expr* x))))

