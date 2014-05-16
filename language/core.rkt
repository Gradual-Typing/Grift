#lang racket
(require Schml/language/types
         Schml/language/shared)

(require racket/generic
	 racket/format)

(define (display/indent o i p)
  (display o p))

(define not-Expr (lambda (o) (not (Expr? o))))

(define-generics pretty-print
  (pprint pretty-print indent port)
  #:defaults ([not-Expr 
	       (define pprint display/indent)])
  #:fallbacks (display/indent))

(provide (all-defined-out))

(define (indent i p)
  (do ((n 0 (add1 n))) ((= n i)) (display #\space)))

(define (write-prog p port mode)
  (write-string "\n(Prog: " port)
  (write-string (Prog-name p) port)
  (write-string " Type: " port)
  (write-string (~a (Prog-type p)) port)
  (for ((e (in-list (Prog-expr* p))))
    (newline port)
    (indent 4 port)
    (pprint e 4 port))
  (write-string ")" port))

(struct Prog (name type expr*)
	#:methods gen:custom-write
	[(define write-proc write-prog)])
    
(struct Expr (src type)
	 #:methods gen:pretty-print
	 [(define (pprint o i p)
	    (indent i p)
	    (write-string "<Expr>" p))])

(define (pprint-lambda o i p)
  (write-string "(lambda (" p)
  (let ((fml* (Lambda-fmls o)))
    (if (null? fml*)
	(write-string "): " p)
	(begin 
	  (pprint (car fml*) (+ 5 i) p)
	  (let loop ((fml*  (cdr fml*)))
	    (if (null? fml*)
		(write-string "): " p)
		(begin
		  (write-string " " p)
		  (pprint (car fml*) (+ 5 i) p)
		  (loop (cdr fml*))))))))
  (pprint (Expr-type o) (+ 5 i) p)
  (newline p)
  (indent (+ 3 i) p)
  (pprint (Lambda-body o) (+ 3 i) p)
  (write-string ")" p))

(struct Lambda Expr (fmls body)
	#:methods gen:pretty-print
	[(define pprint pprint-lambda)])

(struct Var Expr (ident)
	#:methods gen:pretty-print
	[(define pprint 
	   (lambda (o i p) (display (Var-ident o) p)))])

(define (pprint-app o i p)
  (display #\( p)
  (pprint (App-expr o) (add1 i) p)
  (for ((e (in-list (App-expr* o))))
    (display #\space p)
    (pprint e (+ 4 i) p))
  (display #\) p))

(struct App Expr (expr expr*)
	#:methods gen:pretty-print
	[(define pprint pprint-app)])

(define (pprint-cast o i p)
  (display "(: " p)
  (pprint (Cast-expr o) (add1 i) p)
  (fprintf p " ~a ~a)" (Expr-type o) (Cast-blame o)))

(struct Cast Expr (expr blame)
	#:methods gen:pretty-print
	[(define pprint pprint-cast)])

(define (pprint-if o i p)
  (display "(if " p)
  (pprint (If-test o) (+ 4 i) p)
  (newline p)
  (pprint (If-conseq o) (+ 4 i) p)
  (newline p)
  (pprint (If-alt o) (+ 4 i) p)
  (display ")" p))
  
(struct If Expr (test conseq alt)
	#:methods gen:pretty-print
	[(define pprint pprint-if)])

(define (pprint-let o i p)
  (write-string "(let (" p)
  (for ((bnd (in-list (Let-bnds o))))
    (pprint bnd (+ 6 i) p)
    (newline p))
  (write-string ")\n" p)
  (indent (+ 3 i) p)
  (pprint (Let-body o) (+ 3 i) p)
  (display ")" p))

(struct Let Expr (bnds body)
	#:methods gen:pretty-print
	[(define pprint pprint-let)])
(struct Const Expr (const))

(define (pprint-bnd o i p)
  (write-string "(" p)
  (display (Bnd-ident o) p)
  (when (Bnd-type o) 
    (write-string " :" p)
    (display (Bnd-type o) p))
  (write-string " " p)
  (pprint (Bnd-expr o) (+ 4 i) p)
  (write-string "(" p))

(struct Bnd (ident type expr)
	#:methods gen:pretty-print
	[(define pprint pprint-bnd)])

(define (pprint-fml o i p)
  (when (Fml-type o) (write-string "(" p))
  (display (Fml-ident o) p)
  (when (Fml-type o)
    (write-string " : " p)
    (display (Fml-type o) p)
    (write-string ")" p)))

(struct Fml (ident type)
	#:methods gen:pretty-print
	[(define pprint pprint-fml)])

(struct Prim Expr ())
(struct Prim:Rel Prim ())
(struct Prim:Rel:Int Prim:Rel (fst snd))
(struct Prim:Rel:Int:<  Prim:Rel:Int ())
(struct Prim:Rel:Int:>  Prim:Rel:Int ())
(struct Prim:Rel:Int:=  Prim:Rel:Int ())
(struct Prim:Rel:Int:<= Prim:Rel:Int ())
(struct Prim:Rel:Int:>= Prim:Rel:Int ())
(struct Prim:Bin Prim ())
(struct Prim:Bin:Int Prim:Bin (fst snd))
(struct Prim:Bin:Int:*   Prim:Bin:Int ())
(struct Prim:Bin:Int:+   Prim:Bin:Int ())
(struct Prim:Bin:Int:-   Prim:Bin:Int ())
(struct Prim:Bin:Int:and Prim:Bin:Int ())
(struct Prim:Bin:Int:or  Prim:Bin:Int ())
(struct Prim:Bin:Int:>>  Prim:Bin:Int ())
(struct Prim:Bin:Int:<<  Prim:Bin:Int ())


;; (define (ast-pp ast port mode)
  
;;   (let-values (((l c p) (port-next-location port)))
;;     (let ((c (or c (begin (newline p) 0))))
;;       (fprintf port "(Program ~s\n" (Prog-name))
;;       (for ((e (in-list (Prog-expr* ast))))
;; 	(for ((i (in-range
;; 	(fprintf (pp (Prog)



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
           [(Cast s t e b) (and (not t) (expr? e) (string? b))]
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
           [(Cast s t e b) (and (type? t) (expr? e) (string? b))]
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


;; (pretty-print 
;;   (vs-append 
;;    (text "(let") 
;;    (v-append
;;     (h-append lparen 
;;     (align 
;;      (v-append 
;;       (text "(kdfjakljfdsklgj kdjsfkajfkfjakdsj)") 
;;       (h-append (text "(kadsfj dkjfk)") rparen))))
;;     (indent 4 (h-append  (text "bango!") rparen)))))
