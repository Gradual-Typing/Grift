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


;; A utility that creates a scheme style list
;; there seems to be a bug in the pretty printing library
;; with using a (hang at the begining as I would like.
(define (doc-list a)
  (h-append lparen a rparen))
(define (format->doc x) (text (~a x)))
(define (format:type->doc x t) (h-append (text (~a x)) colon (type->doc t)))

;; The Pretty interface
(define (not-ast o) (not (or (Expr? o) (Bnd? o) (Fml? o) (Prog? o))))

;; The Program core form
(define/match (prog->doc p)
  [((Prog (app text name) (app expr->doc e)))
   (hang prog-indent-size
         (doc-list (v-append (hs-append (text "Prog:") name) e)))]
  [(o) (text "Non-printable program")])

(define/match (fml->doc o)
  [((Fml (app format->doc i) t))
   (if t (doc-list (hs-append i colon (type->doc t))) i)])
(define/match (bnd->doc o)
  [((Bnd (app format->doc i) t (app expr->doc e)))
   (doc-list (align (vs-append i (if t
                                     (align (v-append (hs-append colon (type->doc t)) e))
                                     (align e)))))])

(define/match (expr->doc o)
  [((App s t (app expr->doc exp) (list (app expr->doc exp*) ...)))
   (doc-list (align (hs-append exp
                               (if t
                                   (vs-append (vs-concat exp*) (hs-append  colon (type->doc t)))
                                   (vs-concat exp*)))))]
  [((Lambda s t (list (app fml->doc fmls) ...) (app expr->doc body))) 
   (hang lambda-indent-size
    (doc-list (v-append (vs-append (text "lambda") (doc-list (vs-concat fmls)))
                        (h-append (if t (hs-append colon (type->doc t) line) empty)
                                  body))))]
  [((Let s t (list (app bnd->doc bnds) ...) (app expr->doc body)))
   (hang let-indent-size
         (doc-list (v-append (hs-append (text "let")
                                        (hang 1 (doc-list (v-concat bnds))))
                             (if t
                                 (hs-append colon (type->doc t) line body)
                                 body))))]
  [((Cast s (app type->doc t2) (app expr->doc e) t1 l))
   (doc-list (align (vs-append (hs-append colon (label->doc! l))
                               (align e)
                               (if t1 (vs-append (type->doc t1) t2) t2))))]
  [((If s t (app expr->doc tst) (app expr->doc csq) (app expr->doc alt)))
   (doc-list (hs-append (text "if")
                        (align (if t
                                   (v-append tst csq alt (hs-append colon (type->doc t)))
                                   (v-append tst csq alt)))))]
  [((Var s t i)) (if t (format:type->doc i t) (format->doc i))]
  [((Const s t k)) (if t (format:type->doc k t) (format->doc k))]
  [(Prim) (prim->doc o)]
  [(o) (text "Non printable expr")])

(define/match (type->doc t)
  [((Dyn)) (text "Dyn")]
  [((Int)) (text "Int")]
  [((Bool)) (text "Bool")]
  [((Function (list (app type->doc f) ...) (app type->doc t)))
   (doc-list (hs-append (hs-concat f) (text "->") t))]
  [(o) (text "Non-printable Type")])

(define (prim-get-op-string p)
  (cond
    [(Prim:Bin:Int:*? p)   "%*"]
    [(Prim:Bin:Int:+? p)   "%+"]
    [(Prim:Bin:Int:-? p)   "%-"]
    [(Prim:Bin:Int:and? p) "%and"]
    [(Prim:Bin:Int:or? p)  "%or"]
    [(Prim:Bin:Int:>>? p)  "%>>"]
    [(Prim:Bin:Int:<<? p)  "%<<"]
    [(Prim:Rel:Int:<? p)  "%<"]
    [(Prim:Rel:Int:>? p)  "%>"]
    [(Prim:Rel:Int:=? p)  "%="]
    [(Prim:Rel:Int:<=? p) "%<="]
    [(Prim:Rel:Int:>=? p) "%>="]
    [else "Non-printable op"]))

(define/match (prim->doc o)
  [((Prim:Bin:Int s t (app expr->doc fst) (app expr->doc snd)))
   (doc-list (align (hs-append (text (prim-get-op-string o)) (align fst) (align snd))))]
  [((Prim:Rel:Int s t (app expr->doc fst) (app expr->doc snd)))
   (doc-list (align (hs-append (text (prim-get-op-string o)) (align fst) (align snd))))])

(define label-table (box '(0 . ())))
(define (label->doc! lbl)
  (let* ((tbl (unbox label-table))
         (cnt (car tbl))
         (entries (cdr tbl)))
    (set-box! label-table `(,(add1 cnt) . ((,cnt . ,lbl) . ,entries)))
    (text (format "l:~a" cnt))))
(define (reset-label-table!)
  (set-box! label-table '(0 . ())))
(define (print-label-table p)
  (fprintf p "lables:\n")
  (for ((ent (in-list (cdr (unbox label-table)))))
    (fprintf p "\t l:~a -> ~a\n" (car ent) (cdr ent))))

(struct Prog (name expr*)
        #:methods gen:custom-write
        [(define write-proc (lambda (o p d)
                              (reset-label-table!)
                              (let ((doc (prog->doc o))) 
                                (pretty-print doc p)
                                (newline p)
                                (print-label-table p))))]
        #:transparent)

;; The super type of core forms that are considered expressions
(struct Expr (src type) #:transparent)
(struct Lambda Expr (fmls body) #:transparent)
(struct Var Expr (ident) #:transparent)
(struct App Expr (expr expr*) #:transparent)
(struct Cast Expr (expr expr-type blame) #:transparent)
(struct If Expr (test conseq alt) #:transparent)
(struct Let Expr (bnds body) #:transparent)
(struct Const Expr (const) #:transparent)
(struct Bnd (ident type expr) #:transparent)
(struct Fml (ident type) #:transparent)
(struct Prim Expr ())
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

;; Grammar type checks
(define (constant? x)
  (or (boolean? x) (integer? x)))

(define (implicit-core? x)
  (define (expr? x)
    (and (Expr? x) (srcloc? (Expr-src x))
         (match x
           [(Lambda _ t (list (Fml (? uvar?) (? type?)) ...) b)
            (and (or (not t) (type? t)) (expr? b))]
           [(Var _ t i) (and (not t) (uvar? i))]
           [(App _ t e e*) (and (not t) (expr? e) (andmap expr? e*))]
           [(Prim:Bin:Int _ _ (? expr?) (? expr?)) #t]
           [(Prim:Rel:Int _ _ (? expr?) (? expr?)) #t]
           [(If _ #f (? expr?) (? expr?) (? expr?)) #t]
           [(Let _ #f (list (Bnd (? uvar?) (or (? type?) (? not)) (? expr?)) ...)
                 (? expr?)) #t]
           [(Const _ t k) (and (not t) (constant? k))]
           [(Cast _ t1 e t2 b) (and (type? t1) (not t2) (expr? e) (string? b))]
           [otherwise #f])))
  (match x ((Prog (? string?) (? expr?)) #t) (o #f)))

(define (explicit-core? x)
  (define (expr? x)
    (and (Expr? x) (let ((s (Expr-src x))) (or (srcloc? s) (eq? s 'implicit))) 
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
  (and (Prog? x) (string? (Prog-name x)) (expr? (Prog-expr* x))))

