#lang racket

;; Require and Provide other language details 
(require
 racket/contract
 Schml/language/shared
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
(struct Expr (src) #:transparent)
(struct Lambda Expr (fmls ty exp) #:transparent)
(struct Var Expr (id) #:transparent)
(struct App Expr (exp exp*) #:transparent)
(struct Cast Expr (exp ty lbl) #:transparent)
(struct If Expr (tst csq alt) #:transparent)
(struct Let Expr (bnds exp) #:transparent)
(struct Const Expr (const) #:transparent)
(struct Prim Expr (pexp) #:transparent)

(provide
 (contract-out
  [struct Expr ((src srcloc?))]
  [struct (Lambda Expr) ((src srcloc?)
                         (fmls (listof (or/c Fml? Fml:Ty?)))
                         (ty (or/c false/c Type?))
                         (exp Expr?))]
  [struct (Var Expr) ((src srcloc?) (id uvar?))] 
  [struct (App Expr) ((src srcloc?) (exp Expr?) (exp* (listof Expr?)))]
  [struct (Cast Expr) ((src srcloc?) (exp Expr?) (ty Type?) (lbl label?))]
  [struct (If Expr) ((src srcloc?) (tst Expr?) (csq Expr?) (alt Expr?))]
  [struct (Let Expr) ((src srcloc?)
                      (bnds (listof (or/c Bnd? Bnd:Ty?)))
                      (exp Expr?))]
  [struct (Const Expr) ((src srcloc?) (const constant?))]
  ;; There is curren
  [struct (Prim Expr) ((src srcloc?) (pexp (Prim-Expr? Expr?)))]))

(define (core? x)
  (define (expr? x)
    (match x
      [(or (Lambda (? srcloc?) (list (Fml (? uvar?))  ...)
                   (or #f (? type?)) (? expr?))
           (Var (? srcloc?) (? uvar?))
           (App (? srcloc?) (? expr?) (list (? expr?) ...))
           (Prim (? srcloc?) (? prim?))
           (If (? srcloc?) (? expr?) (? expr?) (? expr?))
           (Let (? srcloc?) (list (Bnd (? uvar?) (? expr?)) ...) (? expr?))
           (Const (? srcloc?) (? constant?))
           (Cast (? srcloc?) (? expr?) (? type?) (? label?)))  #t]
      [otherwise #f]))
  (define prim? (mk-prim? expr?))
  (match x ((Prog (? string?) (? expr?)) #t) (o #f)))

(provide core?)


(define/match (expr->doc o)
  [((App _ (app expr->doc exp) (list (app expr->doc exp*) ...)))
   (doc-list (align (hs-append exp (vs-concat exp*))))]
  [((Lambda s (list (app bnd->doc fmls) ...) t (app expr->doc body))) 
   (hang lambda-indent-size
         (doc-list (v-append (vs-append (text "lambda") (doc-list (vs-concat fmls)))
                             (h-append (if t (hs-append colon (type->doc t) line) empty)
                                       body))))]
  [((Let s (list (app bnd->doc bnds) ...) (app expr->doc body)))
   (hang let-indent-size
         (doc-list (v-append (hs-append (text "let")
                                        (hang 1 (doc-list (v-concat bnds))))
                             body)))]
  [((Cast s (app expr->doc e) (app type->doc ty) l))
   (doc-list (align (vs-append (hs-append colon (label->doc! l))
                               (align e)
                               ty)))]
  [((If s (app expr->doc tst) (app expr->doc csq) (app expr->doc alt)))
   (doc-list (hs-append (text "if") (align (v-append tst csq alt))))]
  [((Var s (app format->doc i))) i]
  [((Const s (app format->doc k))) k]
  [((Prim _ (app prim->doc p))) p]
  [(o) (text "Non printable expr")])

(define prim->doc (mk-prim->doc expr->doc))
(define bnd->doc (mk-bnd->doc expr->doc))
