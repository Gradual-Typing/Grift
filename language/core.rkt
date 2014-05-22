#lang racket

;; Require and Provide other language details 
(require Schml/language/types
         Schml/language/shared)
(provide (all-from-out Schml/language/types
                       Schml/language/shared))

(provide (all-defined-out))
;; The pretty printing utilities
(require (planet dherman/pprint:4) racket/generic racket/format)

;; Quickly change the indentation levels of some core forms
(define gen-indent-size 4)
(define prog-indent-size 2)
(define lambda-indent-size 3)
(define let-indent-size 2)

;; The Pretty interface
(define (not-ast o) (not (or (Expr? o) (Bnd? o) (Fml? o) (Prog? o))))
(define (format->doc x) (text (~a x)))

(define-generics pretty
  (->doc pretty)
  #:defaults ([not-ast (define ->doc format->doc)]))

;; A utility that creates a scheme style list
;; there seems to be a bug in the pretty printing library
;; with using a (hang at the begining as I would like.
(define (doc-list . a)
  (h-append lparen (vs-concat a) rparen))


;; The Program core form
(define/match (prog->doc p)
  [((Prog (app text name) (list (app ->doc e) ...)))
   (hang prog-indent-size
         (doc-list (text "Prog:") name line (v-concat e)))]
  [(o) (text (~a o))])

(struct Prog (name expr*)
	#:methods gen:custom-write
	[(define write-proc (lambda (o p d)
                          (pretty-print (prog->doc o) p)))]
    #:methods gen:pretty
    [(define ->doc prog->doc)])

;; The super type of core forms that are considered expressions
(struct Expr (src type)
        #:methods gen:pretty
        [(define ->doc format->doc)]
        #:transparent)

;; The lambda core form
(define/match (lambda->doc o)
  [((Lambda s #f (list (app ->doc fmls) ...) (app ->doc body))) 
   (align (nest lambda-indent-size
                (doc-list (text "lambda") (apply doc-list fmls) line body)))]
  [((Lambda s (app type->doc type) (list (app ->doc fmls) ...) (app ->doc body))) 
   (align (nest lambda-indent-size
                (doc-list (text "lambda") (apply doc-list fmls) colon type line body)))]
  [(o) (text (~a o))])

(struct Lambda Expr (fmls body)
	#:methods gen:pretty
	[(define ->doc lambda->doc)]
    #:transparent)

;; The variable core form
(define/match (var->doc o)
  [((Var s t (app format->doc i))) i]
  [((Var s (app type->doc t) (app format->doc i))) (h-append i colon t)]
  [(o) (format->doc o)])

(struct Var Expr (ident)
	#:methods gen:pretty
	[(define ->doc var->doc)]
    #:transparent)

;; the Application core form
(define (app->doc o)
  (doc-list (->doc (App-expr o)) (vs-concat (map ->doc (App-expr* o)))))

(struct App Expr (expr expr*)
	#:methods gen:pretty
	[(define ->doc app->doc)]
    #:transparent)

;; The cast core form
(define/match (cast->doc o)
  [((Cast s (app type->doc t1) (app ->doc e) (app type->doc t2) l))
   (doc-list colon (text l) e t1 t2)]
  [(o) (text (~a o))])

(struct Cast Expr (expr expr-type blame)
	#:methods gen:pretty
	[(define ->doc cast->doc)]
    #:transparent)

;; The if core form
(define/match (if->doc o)
  [((If s ty (app ->doc tst) (app ->doc csq) (app ->doc alt)))
   (doc-list (text "if") (align (vs-append tst csq alt)))]
  [(o) (text (~a o))])
 
(struct If Expr (test conseq alt)
	#:methods gen:pretty
	[(define ->doc if->doc)]
    #:transparent)

;; The let core form
(define/match (let->doc o)
  [((Let s t (list (app ->doc bnds) ...) (app ->doc body)))
   (hang let-indent-size
         (h-append lparen 
                   (v-append
                    (hs-append
                     (text "let")
                     (h-append lparen (align (v-concat bnds)) rparen))
                    body)
                   rparen))]
  [(o) (text (~a o))])

(struct Let Expr (bnds body)
	#:methods gen:pretty
	[(define ->doc let->doc)]
    #:transparent)

;; The literal constant core form 
(struct Const Expr (const)
        	#:methods gen:pretty
            [(define ->doc (lambda (o) (format->doc (Const-const o))))]
            #:transparent)

;; The binding core form
(define/match (bnd->doc o)
  [((Bnd i #f e))
   (h-append lparen (hs-append (text (symbol->string i)) (->doc e)) rparen)]
  [((Bnd i t e))
   (h-append
    lparen
    (hs-append (text (symbol->string i)) colon (type->doc t) (->doc e))
    rparen)]
  [(o) (text (~a o))])

(struct Bnd (ident type expr)
        #:methods gen:pretty
        [(define ->doc bnd->doc)]
        #:transparent)

;; The formal parameter core form
(define/match (fml->doc o)
  [((Fml i #f)) (text (symbol->string i))]
  [((Fml i t))
   (h-append lparen
             (hs-append (text (symbol->string i)) colon (type->doc t))
             rparen)]
  [(o) (text (~a o))])

(struct Fml (ident type)
	#:methods gen:pretty
	[(define ->doc fml->doc)]
    #:transparent)

;; The primitive application core forms
(define (prim-get-op-string p)
  (match p
    [(struct Prim:Bin:Int:* _)   "%*"]
    [(struct Prim:Bin:Int:+ _)   "%+"]
    [(struct Prim:Bin:Int:- _)   "%-"]
    [(struct Prim:Bin:Int:and _) "%and"]
    [(struct Prim:Bin:Int:or _)  "%or"]
    [(struct Prim:Bin:Int:>> _)  "%>>"]
    [(struct Prim:Bin:Int:<< _)  "%<<"]
    [(struct Prim:Rel:Int:< _)  "%<"]
    [(struct Prim:Rel:Int:> _)  "%>"]
    [(struct Prim:Rel:Int:= _)  "%="]
    [(struct Prim:Rel:Int:<= _) "%<="]
    [(struct Prim:Rel:Int:>= _) "%>="]
    [otherwise (error 'prim-get-op-string "Not a Prim: ~a" p)]))

(define/match (prim->doc o)
  [((Prim:Bin:Int s t (app ->doc fst) (app ->doc snd)))
   (h-append lparen (vs-append (text (prim-get-op-string o)) fst snd) rparen)]
  [((Prim:Rel:Int s t (app ->doc fst) (app ->doc snd)))
   (h-append lparen (vs-append (text (prim-get-op-string o)) fst snd) rparen)]
  [(o) (text (~a o))])


(struct Prim Expr ()
        #:methods gen:pretty
        [(define ->doc prim->doc)])
(struct Prim:Rel Prim ())
(struct Prim:Rel:Int Prim:Rel (fst snd))
(struct Prim:Rel:Int:<  Prim:Rel:Int () #:transparent)
(struct Prim:Rel:Int:>  Prim:Rel:Int () #:transparent)
(struct Prim:Rel:Int:=  Prim:Rel:Int () #:transparent)
(struct Prim:Rel:Int:<= Prim:Rel:Int () #:transparent)
(struct Prim:Rel:Int:>= Prim:Rel:Int () #:transparent)
(struct Prim:Bin Prim ())
(struct Prim:Bin:Int Prim:Bin (fst snd))
(struct Prim:Bin:Int:*   Prim:Bin:Int () #:transparent)
(struct Prim:Bin:Int:+   Prim:Bin:Int () #:transparent)
(struct Prim:Bin:Int:-   Prim:Bin:Int () #:transparent)
(struct Prim:Bin:Int:and Prim:Bin:Int () #:transparent)
(struct Prim:Bin:Int:or  Prim:Bin:Int () #:transparent)
(struct Prim:Bin:Int:>>  Prim:Bin:Int () #:transparent)
(struct Prim:Bin:Int:<<  Prim:Bin:Int () #:transparent)

(define/match (type->doc t)
  [((Dyn)) (text "Dyn")]
  [((Int)) (text "Int")]
  [((Bool)) (text "Bool")]
  [((Function (list (app type->doc f) ...) (app type->doc t)))
   (doc-list (hs-concat f) (text "->") t)]
  [(o) (format->doc o)])


;; Grammar type checks
(define (constant? x)
  (or (boolean? x) (integer? x)))

(define (implicit-core? x)
  (define (expr? x)
    (and (Expr? x) (srcloc? (Expr-src x))
         (match x
           [(Lambda s t f* b) (and (or (not t) (type? t)) (fmls? f*) (expr? b))]
           [(Var s t i) (and (not t) (uvar? i))]
           [(App s t e e*) (and (not t) (expr? e) (andmap expr? e*))]
           [(Prim:Bin:Int s t (? expr?) (? expr?)) #t]
           [(Prim:Rel:Int s t (? expr?) (? expr?)) #t] 
           [(If s t c r a) (and (not t) (expr? c) (expr? r) (expr? a))]
           [(Let s t b* b) (and (not t) (bnds? b*) (expr? b))]
           [(Const s t k) (and (not t) (constant? k))]
           [(Cast s t1 e t2 b) (and (type? t1) (not t2) (expr? e) (string? b))]
           [otherwise #f])))
  (define (fmls? x)
    (define (fml? x) (and (uvar? (Fml-ident x)) (type? (Fml-type x))))
    (and (list? x) (andmap fml? x)))
  (define (bnds? x)
    (define (bnd? x)
      (and (uvar? (Bnd-ident x)) (or (not (Bnd-type x)) (type? (Bnd-type x)))
           (expr? (Bnd-expr x))))
    (and (list? x) (andmap bnd? x)))
  (and (Prog? x) (string? (Prog-name x)) (list? (Prog-expr* x))
       (andmap expr? (Prog-expr* x))))

(define (explicit-core? x)
  (define (expr? x)
    (and (Expr? x) (srcloc? (Expr-src x))
         (match x
           [(Lambda s t f b) (and (type? t) (fmls? f) (expr? b))]
           [(Var s t i) (and (type? t) (uvar? i))]
           [(App s t e e*) (and (type? t) (expr? e) (andmap expr? e*))]
           [(Prim:Bin:Int s t (? expr?) (? expr?)) #t]
           [(Prim:Rel:Int s t (? expr?) (? expr?)) #t] 
           [(If s t c r a) (and (type? t) (expr? c) (expr? r) (expr? a))]
           [(Let s t b* b) (and (type? t) (bnds? b*) (expr? b))]
           [(Const s t k) (and (type? t) (constant? k))]
           [(Cast s t1 e t2 b) (and (type? t1) (type? t2) (expr? e) (string? b))]
           [otherwise #f])))
  (define (fmls? x)
    (define (fml? x) (and (uvar? (Fml-ident x)) (type? (Fml-type x))))
    (and (list? x) (andmap fml? x)))
  (define (bnds? x)
    (define (bnd? x)
      (and (uvar? (Bnd-ident x)) (type? (Bnd-type x)) (expr? (Bnd-expr x))))
    (and (list? x) (andmap bnd? x)))
  (and (Prog? x) (string? (Prog-name x)) (list? (Prog-expr* x))
       (andmap expr? (Prog-expr* x))))

