#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint
         racket/contract)

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
(struct Const (ty const) #:transparent)
(struct Prim (ty pexp))
(struct Let-Proc (ty bnds exp) #:transparent)
(struct Lambda (ty fmls exp) #:transparent)

(define Expr?
  (make-flat-contract
   #:name 'Expr?
   #:first-order
   (lambda (o)
     (or (Var? o) (App? o) (Cast? o) (If? o)
         (Let? o) (Const? o) (Prim? o) (Let-Proc? o)))))

(provide
 (contract-out
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
  [struct Let-Proc
    ((ty Type?) (bnds (listof (Bnd/rhs? Lambda?))) (exp Expr?))]
  [struct Const ((ty Type?) (const constant?))]
  [struct Prim ((ty Type?) (pexp (Prim/args? Expr?)))]
  [struct Lambda
    ((ty Type?) (fmls (listof Fml:Ty?)) (exp Expr?))]))

(define (expr->doc o)
  (define bnd->doc (mk-bnd->doc expr->doc))
  (define prim->doc (mk-prim->doc expr->doc))
  (define/match (proc->doc p)
    [((Lambda (app type->doc t) (list (app bnd->doc b*) ...) (app expr->doc e)))
     (hang lambda-indent-size
           (doc-list (v-append (vs-append (text "lambda")
                                          (align (doc-list (vs-concat b*))))
                               (hs-append colon t)
                               e)))])
  (define let-proc-bnd->doc (mk-bnd->doc proc->doc))
  (match o
    [(Let-Proc (app type->doc t)
               (list (app let-proc-bnd->doc bnds) ...)
               (app expr->doc body))
     (hang let-indent-size
           (doc-list (v-append (hs-append (text "letp")
                                          (hang 1 (doc-list (v-concat bnds))))
                               (hs-append colon t line body))))]
    [(App (app type->doc t) (app expr->doc exp) (list (app expr->doc exp*) ...))
     (doc-list (align (vs-append exp (vs-concat exp*) (hs-append  colon t))))]
    [(Lambda (app type->doc t) (list (app bnd->doc fmls) ...) (app expr->doc body)) 
     (hang lambda-indent-size
           (doc-list (v-append (vs-append (text "lambda") (doc-list (vs-concat fmls)))
                               (h-append colon t)
                               body)))]
    [(Let (app type->doc t) (list (app bnd->doc bnds) ...) (app expr->doc body))
     (hang let-indent-size
           (doc-list (v-append
                      (hs-append (text "let") (hang 1 (doc-list (v-concat bnds))))
                      (hs-append colon t)
                      body)))]
    [(Cast (app type->doc t2) (app expr->doc e) (app type->doc t1) l)
     (doc-list
      (align (vs-append (hs-append colon (label->doc! l)) (align e) t1 t2)))]
    [(If (app type->doc t) (app expr->doc tst) (app expr->doc csq) (app expr->doc alt))
     (doc-list
      (hs-append (text "if") (align (v-append tst csq alt (hs-append colon t)))))]
    [(or (Var _ (app format->doc k)) (Const _ (app format->doc k))) k]
    [(Prim t pe) (prim->doc pe)]
    [o (text "Non printable expr")]))
