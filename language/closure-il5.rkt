#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint
         racket/contract)

(provide (all-from-out Schml/language/shared))

(struct Prog (name unique ty functions main) #:transparent)

(struct Lambda (type formals expr) #:transparent)

(struct Expr (type) #:transparent)

(struct Var Expr (uvar) #:transparent)

(struct Label Expr (ulabel) #:transparent)

(struct App Expr (expr expr*) #:transparent)

(struct Cast Expr (expr type-expr blame-label) #:transparent)

(struct If Expr (tst-expr csq-expr alt-expr) #:transparent)

(struct Let Expr (bindings expr) #:transparent)

(struct Const Expr (value) #:transparent)

(struct Prim Expr (primitive) #:transparent)

(struct Begin Expr (expr* expr) #:transparent)

(provide
 (contract-out
  [struct Prog ((name string?)
                (unique (and/c integer? positive?))
                (ty Type?)
                (functions (listof (Bnd/rhs? Lambda?)))
                (main Expr?))]
  [struct Lambda ((type Type?)
                  (formals (listof Fml:Ty?))
                  (expr Expr?))]
  [struct Expr ((type Type?))]
  [struct Label ((type Type?) (ulabel clabel?))]
  [struct Var ((type Type?) (uvar uvar?))] 
  [struct App ((type Type?)
               (expr Expr?)
               (expr* (listof Expr?)))]
  [struct Cast
    ((type Type?) (expr Expr?) (type-expr Type?) (blame-label label?))]
  [struct If
    ((type Type?)
     (tst-expr Expr?)
     (csq-expr Expr?)
     (alt-expr Expr?))]
  [struct Let
    ((type Type?)
     (bindings (listof (Bnd/rhs? Expr?)))
     (expr Expr?))]
  [struct Begin ([type Type?] [expr* (listof Expr?)] [expr Expr?])]
  [struct Const ((type Type?) (value constant?))]
  [struct Prim ((type Type?) (primitive (Prim/args? Expr?)))]))
