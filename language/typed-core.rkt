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
(struct Expr (src ty) #:transparent)
(struct Lambda Expr (fmls exp) #:transparent)
(struct Var Expr (id) #:transparent)
(struct App Expr (exp exp*) #:transparent)
(struct Cast Expr (exp ty-exp lbl) #:transparent)
(struct If Expr (tst csq alt) #:transparent)
(struct Let Expr (bnds exp) #:transparent)
(struct Const Expr (const) #:transparent)
(struct Prim Expr (pexp))

(provide
 (contract-out
  [struct Expr ((src srcloc?) (ty Type?))]
  [struct (Lambda Expr) ((src srcloc?) (ty Type?)
                         (fmls (listof (or/c Fml? Fml:Ty?))) 
                         (exp Expr?))]
  [struct (Var Expr) ((src srcloc?) (ty Type?) (id uvar?))] 
  [struct (App Expr) ((src srcloc?) (ty Type?) (exp Expr?) (exp* (listof Expr?)))]
  [struct (Cast Expr) ((src srcloc?) (ty Type?) (exp Expr?) (ty-exp Type?) (lbl label?))]
  [struct (If Expr) ((src srcloc?) (ty Type?) (tst Expr?) (csq Expr?) (alt Expr?))]
  [struct (Let Expr) ((src srcloc?) (ty Type?)
                      (bnds (listof (or/c Bnd? Bnd:Ty?)))
                      (exp Expr?))]
  [struct (Const Expr) ((src srcloc?) (ty Type?) (const constant?))]
  ;; There is curren
  [struct (Prim Expr) ((src srcloc?) (ty Type?) (pexp PExpr?))]))


(provide typed-core?)
(define (typed-core? x)
  (define (expr? x)
    (match x
      [(or (Var (? srcloc?) (? type?) (? uvar?))
           (Let (? srcloc?) (? type?)
                (list (Bnd:Ty (? uvar?) (? expr?) (? type?)) ...) (? expr?))
           (Lambda (? srcloc?) (? type?)
                   (list (Fml:Ty (? uvar?) (? type?))  ...) (? expr?))
           (Prim (? srcloc?) (? type?) (? prim?))
           (App (? srcloc?) (? type?) (? expr?) (list (? expr?) ...))
           (Cast (? srcloc?) (? type?) (? expr?) (? type?) (? label?))
           (Const (? srcloc?) (? type?) (? constant?))
           (If (? srcloc?) (? type?) (? expr?) (? expr?) (? expr?))) #t] 
      [otherwise #f]))
  (define prim? (mk-prim? expr?))
  (match x ((Prog (? string?) (? expr?)) #t) (o #f)))

(define/match (expr->doc o)
  [((App _ (app type->doc ty) (app expr->doc exp) (list (app expr->doc exp*) ...)))
   (doc-list (align (hs-append exp (vs-append (vs-concat exp*)
                                              (hs-append colon ty)))))]
  [((Lambda _ (app type->doc ty)
            (list (app bnd->doc fmls) ...)
            (app expr->doc body))) 
   (hang lambda-indent-size
         (doc-list (v-append (vs-append (text "lambda") (doc-list (vs-concat fmls)))
                             (hs-append colon ty)
                             body)))]
  [((Let _ (app type->doc ty) (list (app bnd->doc bnds) ...) (app expr->doc body)))
   (hang let-indent-size
         (doc-list (v-append (hs-append (text "let")
                                        (hang 1 (doc-list (v-concat bnds))))
                             (hs-append colon ty)
                              body)))]
  [((Cast _ (app type->doc t2) (app expr->doc e) (app type->doc t1) l))
   (doc-list (align (vs-append (hs-append colon (label->doc! l))
                               (align e)
                               t1
                               t2)))]
  [((If _ (app type->doc ty)
        (app expr->doc tst) (app expr->doc csq) (app expr->doc alt)))
   (doc-list (hs-append (text "if")
                        (align (v-append tst csq alt (hs-append colon ty)))))]
  [((or (Var _ _ (app format->doc k))
        (Const _ _ (app format->doc k)))) k] 
  [((Prim _ _ o)) (prim->doc o)]
  [(o) (text "Non printable expr")])

(define prim->doc (mk-prim->doc expr->doc))
(define bnd->doc (mk-bnd->doc expr->doc))
