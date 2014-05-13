#lang racket
(provide (all-defined-out))

(struct Prog (file exprs) #:transparent)
(struct Expr (src)  #:transparent)
(struct Lambda Expr (fmls body) #:transparent)
(struct App Expr (expr expr*) #:transparent)
(struct Op Expr (prim rand*)  #:transparent)
(struct Var Expr (ident)  #:transparent)
(struct Cast Expr (expr type)  #:transparent)
(struct If Expr (test conseq alt)  #:transparent)
(struct Let Expr (dec* body)  #:transparent)
(struct Const Expr (k)  #:transparent)
(struct Typed-Fml Expr (ident type)  #:transparent)
(struct Untyped-Fml Expr (ident)  #:transparent)
(struct Typed-Binding Expr (ident type expr)  #:transparent)
(struct Untyped-Binding Expr (ident expr)  #:transparent)

(define (implicit-core? x) #t)
