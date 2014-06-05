#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint)

(provide (all-from-out Schml/language/shared))

(struct Prog (name exp)
        #:methods gen:custom-write
        [(define write-proc (lambda (o p d)
                              (with-printed-labels
                               (pretty-print
                                (hang prog-indent-size
                                      (doc-list
                                       (v-append
                                        (hs-append (text "Prog:")
                                                   (text (Prog-name o)))
                                        (expr->doc (Prog-exp o)))))
                                p)
                               p)))]
        #:transparent)


(provide
 (contract-out
  [struct Prog ((name string?) (exp Expr?))]))

;; The super type of core forms that are considered expressions

(struct Var (ty id) #:transparent)
(struct App (ty exp exp*) #:transparent)
(struct Cast (ty exp ty-exp lbl) #:transparent)
(struct If (ty tst csq alt) #:transparent)
(struct Let (ty bnds exp) #:transparent)
(struct Let-Proc (ty bnds exp) #:transparent)
(struct Const (ty const) #:transparent)
(struct Prim (ty pexp))
(struct Lambda (ty fmls exp) #:transparent)

(define Expr?
  (flat-named-contract
   'Expr?
   (or/c Var? App? Cast? If? Let? Let-Proc? Const? Prim?)))

(define let-rhs?
  (flat-named-contract
   'Expr?
   (or/c Expr? Lambda?)))

(provide
 (contract-out
  [struct (Var Expr) ((ty Type?) (id uvar?))] 
  [struct (App Expr) ((ty Type?) (exp Expr?) (exp* (listof Expr?)))]
  [struct (Cast Expr) ((ty Type?) (exp Expr?) (ty-exp Type?) (lbl label?))]
  [struct (If Expr) ((ty Type?) (tst Expr?) (csq Expr?) (alt Expr?))]
  [struct (Let Expr) ((ty Type?)
                      (bnds (listof (or/c Bnd? Bnd:Ty?)))
                      (exp Expr?))]
  [struct (Let-Proc Expr) [(ty Type?) (bnds (listof Lambda?)) (exp Expr?)]]
  [struct (Const Expr) ((ty Type?) (const constant?))]
  [struct (Prim Expr) ((ty Type?) (pexp PExpr?))]
  [struct Lambda ((fmls (listof (or/c Fml? Fml:Ty?))) 
                  (ty Type?)
                  (exp Expr?))]))


;; Grammar type checks
(define (expr->doc o)
  (define bnd->doc (mk-bnd->doc expr->doc))
  (define prim->doc (mk-prim->doc expr->doc))
  (match o
    [(App t (app expr->doc exp) (list (app expr->doc exp*) ...))
     (doc-list (align (hs-append exp
                                 (if t
                                     (vs-append (vs-concat exp*) (hs-append  colon (type->doc t)))
                                     (vs-concat exp*)))))]
    [(Lambda t (list (app bnd->doc fmls) ...) (app expr->doc body)) 
     (hang lambda-indent-size
           (doc-list (v-append (vs-append (text "lambda") (doc-list (vs-concat fmls)))
                               (h-append (if t (hs-append colon (type->doc t) line) empty)
                                         body))))]
    [(Let t (list (app bnd->doc bnds) ...) (app expr->doc body))
     (hang let-indent-size
           (doc-list (v-append (hs-append (text "let")
                                          (hang 1 (doc-list (v-concat bnds))))
                               (if t
                                   (hs-append colon (type->doc t) line body)
                                   body))))]
    [(Cast (app type->doc t2) (app expr->doc e) t1 l)
     (doc-list (align (vs-append (hs-append colon (label->doc! l))
                                 (align e)
                                 (if t1 (vs-append (type->doc t1) t2) t2))))]
    [(If t (app expr->doc tst) (app expr->doc csq) (app expr->doc alt))
     (doc-list (hs-append (text "if")
                          (align (if t
                                     (v-append tst csq alt (hs-append colon (type->doc t)))
                                     (v-append tst csq alt)))))]
    [(or (Var _ (app format->doc k))
          (Const _ (app format->doc k))) k]
    [(Prim t pe) (prim->doc pe)]
    [o (text "Non printable expr")]))

