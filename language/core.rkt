#lang racket

;; Require and Provide other language details 
(require
 racket/contract
 Schml/language/shared
 Schml/framework/pprint)

(provide (all-from-out Schml/language/shared))


;; The Program is the type passed from one pass to another
(struct Prog (name unique exp)
        #:methods gen:custom-write
        [(define write-proc ast-pretty-write-proc)]
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((name  (text (Prog-name o)))
                 (unique (text (format "unique: ~a" (Prog-unique o))))
                 (exp (->d (Prog-exp o))))
             (hang prog-indent-size
                   (doc-list (v-append (text "Prog:") name unique exp)))))]
        #:transparent)

;; The Expression is a core for with a source location
;; Lambda, Application, Variables, Constants, Primitive Operations
;; Lets, and Ifs are all Expressions
(struct Expr (src) #:transparent
        #:methods gen:pretty
        [(define (->doc o)
           (text (format "#<Expr at ~a>" (srcloc->string (Expr-src o)))))])

(struct Lambda Expr (fmls ty exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((fmls (doc-list
                        (align
                         (vs-concat
                          (map ->d (Lambda-fmls o))))))
                 (exp (->d (Lambda-exp o))))
             (let ((exp (let ((ty (Lambda-ty o)))
                          (if ty (hs-append colon (->d ty) line exp) exp))))
               (hang
                lambda-indent-size
                (doc-list
                 (v-append (vs-append (text "lambda") fmls) exp))))))])

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

(struct Cast Expr (exp ty lbl) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (Cast->doc o)
           (let ((exp (->d (Cast-exp o)))
                 (ty (->d (Cast-ty o))) 
                 (lbl (label->doc! (Cast-lbl o))))
             (doc-list
              (align
               (vs-append
                (hs-append colon lbl)
                (align exp)
                ty)))))])

(struct If Expr (tst csq alt) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((tst (->d (If-tst o)))
                 (csq (->d (If-csq o)))
                 (alt (->d (If-alt o))))
             (doc-list
              (hs-append
               (text "if")
               (align (v-append tst csq alt))))))])

(struct Let Expr (bnds exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((bnds (doc-list (v-concat (map ->d (Let-bnds o)))))
                 (exp (->d (Let-exp o))))
             (hang
              let-indent-size
              (doc-list
               (v-append
                (hs-append (text "let") (hang 1 bnds))
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
  [struct Prog ((name string?)
                (unique (and/c positive? integer?))
                (exp Expr?))]
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
  [struct (Prim Expr) ((src srcloc?) (pexp (Prim/args? Expr?)))]))
