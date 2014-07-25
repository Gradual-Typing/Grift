#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/parse                                    |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: The parse pass takes racket's native syntax objects to a set of   |
|core structures that are annotated with the source location.                   |
|                                                                               |
+-------------------------------------------------------------------------------+
|Grammar for Core-Prog found in Schml/languages/core-forms                      |
|Core-Prog = (Core-Prog {File-Name} {Next-Uvar-Suffix} {Core-Expr})             |
|Core-Expr = (Lambda {Formals}* {Type}? {Core-Expr} {Src})                      |
|          | (Var {Uvar} {Src})                                                 |
|          | (App {Expr} {Expr}* {Src})                                         |
|          | (Op {Prim} {Expr}* {Src})                                          |
|          | (Ascrip {Expr} {Type} {Blame?} {Src})                              |
|          | (If {Expr} {Expr} {Expr} {Src})                                    |
|          | (Let {Binding} {Expr} {Src})                                       |
|          | (Quote {Datum} {Src})                                              |
|Binding   = (Bnd {Uvar} {Expr}) | (Bnd-Typed {Uvar} {Type} {Expr})             |
|Formal    = (Fml {Uvar}) | (Fmlt {Uvar} {Type})                                |
|Src       = Racket's native srcloc type                                        |
|Blame?    = String | #f                                                        |
|Type?     = {Type} | #f                                                        |
|Type     = Int | Bool | (Fn {Type}* {Type})                                     |
|Uvar      = (Uvar String Natural)                                              |
|Datum     = Integer |  Boolean                                                 |
|Prim      = * | + | - | % | % | >> | << | < | <= | = | > | >=                  |
|                                                                               |
+------------------------------------------------------------------------------|#

(require Schml/framework/build-compiler
	 Schml/language/types
         Schml/language/syntax
         Schml/language/core-forms)

;; May use the syntax parse form soon to create a more modular parser
;; with better error messages.

(provide parse)

#| The type of the enviroment bindings and symbol-sets|#
(define-type Env (HashTable Symbol Uvar))

(: env-lookup (Env Symbol srcloc . -> . Uvar))
(define (env-lookup e s l)
  (hash-ref e s (lambda () (raise-unbound-variable-exn s l))))

(: env-extend (Env Symbol Uvar . -> . Env))
(define env-extend hash-set)

#| pattern helpers |#
(: seq? (Stx Symbol . -> . Boolean))
(define (seq? s p) (eq? (syntax-e s) p))

(: colon? (Stx . -> . Boolean))
(define (colon? x) (seq? x ':))

(: arrow? (Stx . -> . Boolean))
(define (arrow? x) (seq? x '->))

#| The type of source locations and syntax helpers|#	     

(: srcloc->str (srcloc . -> . String))
(define (srcloc->str s)
  (let ((s (srcloc->string s)))
    (or s "")))
	 
(: syntax->srcloc (Stx . -> . srcloc))
(define (syntax->srcloc x)
  (srcloc (syntax-source x) 
	  (syntax-line x)
          (syntax-column x) 
	  (syntax-position x) 
	  (syntax-span x)))

(: syntax-unroll (Stx . -> . (U Symbol Literal (Listof Stx))))
(define (syntax-unroll stx)
  (let ((a (syntax-e stx)))
    (if ((make-predicate (U Symbol Literal (Listof Stx)))
	 a)
	a
	(raise-unsupported-syntax-exn stx))))


;; stx-list? is equivalent to (and (list? o) (andmap syntax? o))
(define-predicate stx-list? (Listof Stx))	   

(: file->srcloc (String . -> . srcloc))
(define (file->srcloc n)
  (srcloc n #f #f #f #f))

(: reserved-symbol? (Symbol . -> . Boolean))
(define (reserved-symbol? x)
  (case x
    [(lambda letrec let if :) #t]
    [(* + - binary-and binary-or << >> < <= = >= >) #t]
    [else #f]))


   
#| The parser |#
(: parse-expr (Stx Env Natural . -> . (values Core-Form Natural)))
(define (parse-expr stx env next)
  (let ((src (syntax->srcloc stx))
	(exp (syntax-unroll stx)))
    (cond
     [(symbol? exp) (values (parse-variable exp src env) next)]
     [(pair? exp) 
      (let* ((rator (car exp)) 
	     (rand* (cdr exp)) 
	     (exp^ (syntax-unroll rator)))
	(cond
	 [(symbol? exp^)
	  (case exp^
	      [(lambda) (parse-lambda rand* src env next)]
	      [(letrec) (parse-letrec rand* src env next)]
	      [(let)    (parse-let rand* src env next)]
	      [(if)     (parse-if rand* src env next)]
	      [(:)      (parse-ascription rand* src env next)]
	      [else
	       (if (primitive-id? exp^) 
		   (parse-primitive exp^ rand* src env next)
		   (parse-application rator rand* src env next))])]
	 [(pair? exp^) (parse-application rator rand* src env next)]
	 [else (bad-syntax-application stx)]))]
     [(Literal? exp) (values (Quote exp src) next)] 
     [else (raise-unsupported-syntax-exn stx)])))

(: parse-variable (Symbol srcloc Env . -> . Core-Form))
(define (parse-variable v s e)
  (Var (env-lookup e v s) s))

(: parse-blame-label (Stx . -> . String))
(define (parse-blame-label s)
  (let ([e (syntax-unroll s)])
    (if (string? e) e (raise-blame-label-exn s))))


(define-type Acc2 (List (Listof (Fml Core-Type)) 
			(Listof Symbol) 
			Env 
			Natural))
(: parse-lambda ((Listof Stx) srcloc Env Natural . -> . 
		 (values Core-Form Natural)))
(define (parse-lambda stx* src env next)
  (define (help [stx : Stx] 
		[t : (U Stx False)]
		[b : Stx] 
		[s : srcloc] 
		[e : Env] 
		[n : Natural])
    : (values Core-Form Natural)
    (let ([f* (syntax-unroll stx)])
      (if (stx-list? f*)
	  (let ([a : Acc2 (list '() '() e n)])
	    (match-let ([`(,f* ,_ ,e ,n) (foldr parse-fml a f*)])
	      (let-values ([(t) (and t (parse-type t))]
			   [(b n) (parse-expr b e n)])
		(values (Lambda f* t b s) n))))
	  (raise-fml-exn stx))))
  (match stx*
    [(list fmls body)
     (help fmls #f body src env next)]
    [(list fmls (? colon?) type body)
     (help fmls type body src env next)]
    [otherwise (raise-lambda-exn stx* src)]))

(: parse-fml (Stx Acc2 . -> . Acc2))
(define (parse-fml stx acc)
  (define (help [s : Symbol] [t : (U Stx False)] [l : srcloc] [a : Acc2]) 
    : Acc2 
    (match-let ([`(,f* ,s* ,e ,n) a])
      (let* ([t (if t (parse-type t) Dyn-Type)]
	     [u (Uvar (symbol->string s) n)]
	     [f (Fml u t)]
	     [e (env-extend e s u)])
	(cond
	 [(memq s s*) (raise-duplicate-binding s l)]
	 [(reserved-symbol? s) (raise-reservered-sym s l)]
	 [else (list (cons f f*) (cons s s*) e (add1 n))]))))
  (let ((src (syntax->srcloc stx)))
    (match (syntax-unroll stx)
      [(list (app syntax-unroll (? symbol? sym)) (? colon?) type) 
       (help sym type src acc)]
      [(? symbol? sym) (help sym #f src acc)]
      [otherwise (raise-fml-exn stx)])))

(struct bnd ([u : Uvar]
	     [t : (U Stx False)] 
	     [r : Stx]))

(struct Acc0 ([b* : (Listof bnd)]
	      [s* : (Listof Symbol)]
	      [e : Env]
	      [n : Natural]))

(define-type Acc1 (Pair (Listof (Bnd Core-Form Maybe-Type)) 
			Natural))

(define-syntax-rule 
  (parse-let-form form cnstrct init-env body-env bnd-env)
  (lambda ([stx* : (Listof Stx)]
	   [loc  : srcloc]
	   [init-env : Env]
	   [next : Natural])
    (match stx*
      [(list s b)
       (let ([b* (syntax-unroll s)])
	 (if (not (stx-list? b*))
	     (raise-bnd-exn s)
	     (let* ([a (Acc0 '() '() init-env next)]
		    [a : Acc0 (foldr unsplice-bnd a b*)]
		    [b* : (Listof bnd) (Acc0-b* a)]
		    [body-env : Env (Acc0-e a)]
		    [n : Natural (Acc0-n a)]
		    [a : Acc1 (cons '() n)]
		    [a : Acc1 
		       (foldr (parse-bnd bnd-env) a b*)]
		    [b* (car a)]
		    [n  (cdr a)])
	       (let-values ([(b n) (parse-expr b body-env n)])
		 (values (cnstrct b* b loc) n)))))]
      [otherwise (raise-let-exn form stx* loc)])))

(: parse-letrec ((Listof Stx) srcloc Env Natural . -> . 
		 (values Core-Form Natural)))
(define parse-letrec (parse-let-form letrec Letrec e1 e2 e2)) 

(: parse-let ((Listof Stx) srcloc Env Natural . -> . 
	      (values Core-Form Natural)))
(define parse-let (parse-let-form let Let e1 e2 e1))
#|
(define (parse-let stx* loc env next)
  (match stx*
    [(list (app syntax-unroll (list stx* ...)) b)
     (match-let* ([acc `(() () () () ,env ,next)]
		  [`(,_ ,u* ,t* ,r* ,e ,n) (foldr unsplice-bnd acc stx*)]
		  [`(,b* . ,n) (foldr (parse-bnd env) `(() . ,n) u* t* r*)])
      (let-values ([(b n) (parse-expr b e n)])
	(values (Let b* b loc) n)))]
    [otherwise (raise-let-exn stx* loc)]))
|#
(: parse-bnd (Env . -> . (bnd Acc1 . -> . Acc1)))
(define (parse-bnd env)
  (lambda ([b : bnd] [a : Acc1]) 
    : Acc1
    (let ([b* (car a)]
	  [n  (cdr a)]
	  [u  (bnd-u b)]
	  [t  (bnd-t b)]
	  [e  (bnd-r b)])
      (let-values ([(e n) (parse-expr e env n)]
		   [(t) (and t (parse-type t))])
	(let ([b (Bnd u t e)])
	  `((,b . ,b*) . ,n))))))


(: unsplice-bnd (Stx Acc0 -> Acc0))
(define (unsplice-bnd stx acc)
  (define (help [s : Symbol] [t : (U Stx False)] 
		[r : Stx] [l : srcloc] [a : Acc0]) 
    : Acc0
    (let* ([b* (Acc0-b* a)]
	   [s* (Acc0-s* a)]
	   [e  (Acc0-e  a)]
	   [n  (Acc0-n  a)]
	   [u (Uvar (symbol->string s) n)])
      (cond
	 [(memq s s*) (raise-duplicate-binding s l)]
	 [(reserved-symbol? s) (raise-reservered-sym s l)]
	 [else (Acc0 (cons (bnd u t r) b*)
		     (cons s s*)
		     (env-extend e s u)
		     (add1 n))])))
  (let ([loc (syntax->srcloc stx)])
    (match (syntax-unroll stx)
      [(list (app syntax-unroll (? symbol? sym)) (? colon?) type rhs)
       (help sym type rhs loc acc)]
      [(list (app syntax-unroll (? symbol? sym)) rhs)
       (help sym #f rhs loc acc)]
      [otherwise (raise-bnd-exn stx)])))


(: parse-if ((Listof Stx) srcloc Env Natural . -> . 
		 (values Core-Form Natural)))
(define (parse-if stx* src env next)
  (match stx*
    [(list test then else)
     (let*-values ([(tst next) (parse-expr test env next)]
		   [(csq next) (parse-expr then env next)]
		   [(alt next) (parse-expr else env next)])
       (values (If tst csq alt src) next))]
    [othewise (raise-if-exn stx* src)]))

(: parse-ascription ((Listof Stx) srcloc Env Natural . -> . 
		     (values Core-Form Natural)))
(define (parse-ascription stx* src env next)
  (define (help [s : Stx] [t : Stx] [l : (U Stx False)] 
		[src : srcloc] [e : Env] [n : Natural]) 
    : (values Core-Form Natural) 
    (let*-values ([(exp next) (parse-expr s e n)]
		  [(type) (parse-type t)]
		  [(lbl) (and l (parse-blame-label l))])
      (values (Ascribe exp type lbl src) next)))
  (match stx*
    [(list exp type) (help exp type #f src env next)]
    [(list exp type lbl) (help exp type lbl src env next)]
    [othewise (raise-ascribe-exn stx* src)]))

(: parse-primitive (Primitive-id (Listof Stx) srcloc Env Natural . -> . 
			      (values Core-Form Natural)))
(define (parse-primitive sym stx* src env next)
  (let-values ([(args next) (parse-expr* stx* env next)])
    (values (Op sym args src) next)))

(: parse-application (Stx (Listof Stx) srcloc Env Natural . -> .
		      (values Core-Form Natural)))
(define (parse-application stx stx* src env next)
  (let*-values ([(rator next) (parse-expr stx env next)]
		[(rands next) (parse-expr* stx* env next)])
    (values (App rator rands src) next)))

(: parse-expr* ((Listof Stx) Env Natural . -> . (values (Listof Core-Form) Natural)))
(define (parse-expr* s* e n) 
  (if (null? s*)
      (values '() n)
      (let*-values ([(cf new-n) (parse-expr (car s*) e n)]
		    [(cf* newer-n) (parse-expr* (cdr s*) e new-n)])
         (values (cons cf cf*) newer-n))))

(: parse-type (Stx . -> . Core-Type))
(define (parse-type stx)
  (match (syntax-unroll stx)
    ['Int (Int)]
    ['Bool (Bool)]
    ['Dyn  (Dyn)]
    [(list stx* ...) (parse-fn-type stx)]
    [othewise (raise-type-exn stx)]))

(: parse-fn-type (Stx . -> . Core-Type))
(define (parse-fn-type stx)
  (let ([stx* (syntax-unroll stx)])
    (if (not (stx-list? stx*))
	(raise-type-exn stx)
	(letrec ([loop : ((Listof Stx) . -> . 
			  (values (Listof Stx) (Listof Stx)))
		  (lambda ([s* : (Listof Stx)])
		    (if (null? s*) 
			(raise-type-exn stx)
			(let ((fst : Stx (car s*))
			      (rst : (Listof Stx) (cdr s*)))
			  (if (arrow? fst)
			      (values '() rst)
			      (let-values ([(from to) (loop rst)])
				(values (cons fst from) to))))))])
	  (let-values ([(from to) (loop stx*)])
	    (: from (Listof Stx))
	    (: to (Listof Stx))
	    (Fn (map parse-type from) (map parse-type to)))))))
  
(: parse-top-level
   (String (Listof Stx) Env Natural . -> . (values Core-Form Natural)))
(define (parse-top-level name stx* env next) 
  : (values Core-Form Natural)
  (cond
   [(null? stx*) (raise-file-empty-exn name)]
   [(not (null? (cdr stx*))) (raise-<1-exp-exn name)]
   [else (parse-expr (car stx*) env next)]))

(define (parse [ast : Stx-Prog] [c : Config]) : Core-Prog
  (let ((name : String (Stx-Prog-name ast))
	(stx* : (Listof Stx) (Stx-Prog-syntax* ast))
	(next-uvar : Natural 0)
	(env : Env (hasheq)))
    (let-values ([(core-form next-uvar) 
		  (parse-top-level name stx* env next-uvar)])
      (Core-Prog name next-uvar core-form))))
