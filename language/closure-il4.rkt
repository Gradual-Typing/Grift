#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint
         racket/contract)

(provide (all-from-out Schml/language/shared))

(struct Prog (name unique ty exp)  #:transparent)

(struct Expr (ty) #:transparent)

(struct Var Expr (id) #:transparent)

(struct Label Expr (id) #:transparent)

(struct App Expr (exp exp*) #:transparent)

(struct Cast Expr (exp ty-exp lbl) #:transparent)

(struct If Expr (tst csq alt) #:transparent)

(struct Let Expr (bnds exp) #:transparent)

(struct Const Expr (const) #:transparent)

(struct Prim Expr (pexp) #:transparent)

(struct Begin Expr (effects exp) #:transparent)

(struct Let-Proc Expr (bindings exp) #:transparent)

(struct Lambda (ty fmls exp) #:transparent)

(provide
 (contract-out
  [struct Prog ((name string?)
                (unique (and/c integer? positive?))
                (ty Type?)
                (exp Expr?))]
  [struct Expr ((ty Type?))]
  [struct Label ((ty Type?) (id clabel?))]
  [struct Var ((ty Type?) (id uvar?))] 
  [struct App
    ((ty Type?) (exp Expr?) (exp* (listof Expr?)))]
  [struct Cast
    ((ty Type?) (exp Expr?) (ty-exp Type?) (lbl label?))]
  [struct If
    ((ty Type?) (tst Expr?) (csq Expr?) (alt Expr?))]
  [struct Let
    ((ty Type?) (bnds (listof (Bnd/rhs? Expr?)))
     (exp Expr?))]
  [struct Begin ([ty Type?] [effects (listof Expr?)] [exp Expr?])]
  [struct Let-Proc
    ((ty Type?)
     (bindings (listof (Bnd/rhs? Lambda?))) 
     (exp Expr?))]
  [struct Const ((ty Type?) (const constant?))]
  [struct Prim ((ty Type?) (pexp (Prim/args? Expr?)))]
  [struct Lambda ((ty Type?) (fmls (listof Fml:Ty?)) (exp Expr?))]))
