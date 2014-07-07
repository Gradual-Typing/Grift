#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint)

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
(struct Expr (src ty) #:transparent
        #:methods gen:pretty
        [(define (->doc o)
           (text (format "#<Expr at ~a>" (srcloc->string (Expr-src o)))))])

(struct Lambda Expr (fmls exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((fmls (doc-list
                        (align
                         (vs-concat
                          (map ->d (Lambda-fmls o))))))
                 (ty (->d (Expr-ty o)))
                 (exp (->d (Lambda-exp o))))
             (hang
              lambda-indent-size
              (doc-list
               (v-append (vs-append (text "lambda") fmls)
                         (hs-append colon ty)
                         exp)))))])

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
(provide
 (contract-out
  [struct Expr ((src srcloc?) (ty Type?))]
  [struct (Lambda Expr) ((src srcloc?) (ty Type?)
                         (fmls (listof (or/c Fml? Fml:Ty?))) 
                         (exp Expr?))]
  [struct (Var Expr) ((src srcloc?) (ty Type?) (id uvar?))] 
  [struct (App Expr) ((src srcloc?) (ty Type?) (exp Expr?) (exp* (listof Expr?)))]
  [struct (Cast Expr) ((src srcloc?)
                       (ty Type?)
                       (exp Expr?)
                       (ty-exp Type?)
                       (lbl (or/c label? false?)))]
  [struct (If Expr) ((src srcloc?) (ty Type?) (tst Expr?) (csq Expr?) (alt Expr?))]
  [struct (Let Expr) ((src srcloc?) (ty Type?)
                      (bnds (listof (or/c Bnd? Bnd:Ty?)))
                      (exp Expr?))]
  [struct (Const Expr) ((src srcloc?) (ty Type?) (const constant?))]
  ;; There is curren
  [struct (Prim Expr) ((src srcloc?) (ty Type?) (pexp (Prim/args? Expr?)))]))

