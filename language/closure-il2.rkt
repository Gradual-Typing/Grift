#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint
         racket/contract)

(provide (all-from-out Schml/language/shared))

(struct Prog (name unique ty exp)
        #:methods gen:custom-write
        [(define write-proc ast-pretty-write-proc)]
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((name  (text (Prog-name o)))
                 (unique (text (format "Unique: ~a" (Prog-unique o))))
                 (ty     (hs-append (text "Return type:") (->d (Prog-ty o))))
                 (exp (->d (Prog-exp o))))
             (hang prog-indent-size
                   (doc-list (v-append (text "Prog:") name ty unique exp)))))]
        #:transparent)

(provide
 (contract-out
  [struct Prog ((name string?)
                (unique (and/c integer? positive?))
                (ty Type?)
                (exp Expr?))]))

;; The super type of core forms that are considered expressions

(struct Expr (ty) #:transparent
        )

(struct Var Expr (id) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (->d (Var-id o)))])

(struct App Expr (exp exp*) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((exp (->d (App-exp o)))
                 (exp* (vs-concat (map ->d (App-exp* o)))))
             (doc-list (align (vs-append exp exp*)))))])

(struct Cast Expr (exp ty-exp lbl) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (Cast->doc o)
           (let ((exp (->d (Cast-exp o)))
                 (t1 (->d (Cast-ty-exp o)))
                 (t2 (->d (Expr-ty o))) 
                 (lbl (label->doc! (Cast-lbl o))))
             (doc-list
              (align
               (vs-append (hs-append colon lbl) (align exp) t1 t2)))))])

(struct If Expr (tst csq alt) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((tst (->d (If-tst o)))
                 (csq (->d (If-csq o)))
                 (alt (->d (If-alt o)))
                 (ty  (->d (Expr-ty o))))
             (doc-list
              (hs-append
               (text "if")
               (align (v-append tst csq alt (hs-append colon ty)))))))])

(struct Let Expr (bnds exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((bnds (doc-list (v-concat (map ->d (Let-bnds o)))))
                 (exp (->d (Let-exp o)))
                 (ty (->d (Expr-ty o))))
             (hang
              let-indent-size
              (doc-list
               (v-append
                (hs-append (text "let") (hang 1 bnds))
                (hs-append colon ty)
                exp)))))])

(struct Const Expr (const) #:transparent
        #:methods gen:pretty
        [(define (->doc o) (format->doc (Const-const o)))])

(struct Prim Expr (pexp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (->d (Prim-pexp o)))])

(struct Let-Proc Expr (bnds exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((bnds (doc-list (v-concat (map ->d (Let-Proc-bnds o)))))
                 (exp (->d (Let-Proc-exp o)))
                 (ty (->d (Expr-ty o))))
             (hang
              let-indent-size
              (doc-list
               (v-append
                (hs-append (text "let") (hang 1 bnds))
                (hs-append colon ty)
                exp)))))])


(struct Lambda (ty fmls free exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((fmls (doc-list
                        (align
                         (vs-concat
                          (map ->d (Lambda-fmls o))))))
                 (free (doc-list
                        (align
                         (vs-concat
                          (map ->d (Lambda-free o))))))
                 (ty (->d (Lambda-ty o))) 
                 (exp (->d (Lambda-exp o))))
             (hang
              lambda-indent-size
              (doc-list
               (v-append (vs-append (text "lambda")
                                    (align (v-append fmls free)))
                         (hs-append colon ty)
                         exp)))))])

(provide
 (contract-out
  [struct Expr ((ty Type?))]
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
    ((ty Type?) (fmls (listof Fml:Ty?)) (free (listof Fml:Ty?)) (exp Expr?))]))

(define (expr->doc o)
  (define bnd->doc (mk-bnd->doc expr->doc))
  (define prim->doc (mk-prim->doc expr->doc))
  (define/match (proc->doc p)
    [((Lambda (app type->doc t)
              (list (app bnd->doc b*) ...)
              (list (app bnd->doc f*) ...)
              (app expr->doc e)))
     (hang lambda-indent-size
           (doc-list (v-append (vs-append (text "lambda")
                                          (align (doc-list (vs-concat b*))))
                               (doc-list  (vs-append (text "free") (align (vs-concat f*))))
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
