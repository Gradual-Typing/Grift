#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/schml/parse                                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: The parse pass takes racket's native syntax objects to a set of   |
|core structures that are annotated with the source location.                   |
+-------------------------------------------------------------------------------+
+------------------------------------------------------------------------------|#

(require schml/src/helpers
	 schml/src/errors
	 schml/src/language)

(provide parse)

(: parse (Syntax-Lang Config . -> . Schml0-Lang))
(define (parse prgm config)
  (match-let ([(Prog name stx*) prgm])
    (let-values 
	([((exp : S0-Expr) (next : Natural)) 
          (run-state (parse-top-level name stx* (hasheq)) FIRST-UID-SUFFIX)])
      (Prog (list name next) exp))))

#| The type of the enviroment bindings and symbol-sets|#
(define-type Env (HashTable Symbol Uid))
(: env-lookup (Env Symbol Src . -> . Uid))
(define (env-lookup e s l)
  (hash-ref e s (lambda () (raise-unbound-variable-exn s l))))

(: env-extend (Env Symbol Uid . -> . Env))
(define env-extend hash-set)

#| pattern helpers |#
(: seq? (Stx Symbol . -> . Boolean))
(define (seq? s p) (eq? (syntax-e s) p))

(: colon? (Stx . -> . Boolean))
(define (colon? x) (seq? x ':))

(: arrow? (Stx . -> . Boolean))
(define (arrow? x) (seq? x '->))

#| The type of source locations and syntax helpers|#	     

(: syntax-unroll (Stx . -> . (U Symbol String Schml-Literal (Listof Stx))))
(define (syntax-unroll stx)
  (let ((a (syntax-e stx)))
    (if ((make-predicate (U Symbol Schml-Literal String (Listof Stx)))
	 a)
	a
	(raise-unsupported-syntax-exn stx))))


;; stx-list? is equivalent to (and (list? o) (andmap syntax? o))
(define-predicate stx-list? (Listof Stx))	   

(: reserved-symbol? (Symbol . -> . Boolean))
(define (reserved-symbol? x)
  (case x
    [(lambda letrec let if :) #t]
    [(+ - * binary-and binary-or < <= = >= > %/ %<< %>>) #t]
    [else #f]))

(: parse-top-level (String (Listof Stx) Env . -> . (State Natural S0-Expr)))
(define (parse-top-level name stx* env) : (State Natural S0-Expr)
  (cond
   [(null? stx*) (return-state (raise-file-empty-exn name))]
   [(not (null? (cdr stx*))) (return-state (raise-<1-exp-exn name))]
   [else (parse-expr (car stx*) env)]))
   
#| The parser |#
(: parse-expr (Stx Env . -> . (State Natural S0-Expr)))
(define (parse-expr stx env)
  (lambda ((next : Natural))
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
                [(begin)  (run-state (parse-begin rand* src env) next)]
                [(box)          (TODO parse box-unbox forms)]
                [(box-set!)     (TODO parse box-unbox forms)]
                [(unbox)        (TODO parse box-unbox forms)]
                [(vector)       (TODO parse vector forms)]
                [(vector-set!)  (TODO parse vector forms)]
                [(vector-ref)   (TODO parse vector forms)]
                [(gbox)         (TODO NEXT)]
                [(gunbox)       (TODO parse box-unbox forms)]
                [(mbox)         (TODO parse box-unbox forms)]
                [(mumbox)       (TODO parse box-unbox forms)]
                [(gvector)      (TODO parse vector forms)]
                [(gvector-set!) (TODO parse vector forms)]
                [(gvector-ref)  (TODO parse vector forms)]
                [(mvector)      (TODO parse vector forms)]
                [(mvector-set!) (TODO parse vector forms)]
                [(mvector-ref)  (TODO parse vector forms)]
                [else
                 (if (Schml-Prim? exp^) 
                     (parse-primitive exp^ rand* src env next)
                     (parse-application rator rand* src env next))])]
             [(pair? exp^) (parse-application rator rand* src env next)]
             [else (bad-syntax-application stx)]))]
     [(schml-literal? exp) (values (Ann (Quote exp) src) next)] 
     [else (raise-unsupported-syntax-exn stx)]))))

(: parse-variable (Symbol Src Env . -> . S0-Expr))
(define (parse-variable v s e)
  (Ann (Var (env-lookup e v s)) s))

(: parse-blame-label (Stx . -> . String))
(define (parse-blame-label s)
  (let ([e (syntax-unroll s)])
    (if (string? e) e (raise-blame-label-exn s))))

#| Begin the section about parsing effects |#

(: parse-begin (-> (Listof Stx) Src Env (State Natural S0-Expr)))
(define (parse-begin stx* src env)
  (let* ([s* (reverse stx*)])
    (if (null? s*)
        (TODO the right error message and fixing the double reverse)
        (do bind-state                   
            (e* : S0-Expr* <- (parse-expr* (reverse (cdr s*)) env))
            (e  : S0-Expr  <- (parse-expr  (car s*) env))
            (return-state (Ann (Begin e* e) src))))))



(define-type Acc2 
  (List Schml-Fml* (Listof Symbol) Env Natural))
(: parse-lambda (-> (Listof Stx) Src Env Natural 
		    (values S0-Expr Natural)))
(define (parse-lambda stx* src env next)
  (define (help [stx : Stx] 
		[t : (U Stx False)]
		[b : Stx] 
		[s : Src] 
		[e : Env] 
		[n : Natural])
    : (values S0-Expr Natural)
    (let ([f* (syntax-unroll stx)])
      (if (stx-list? f*)
	  (let ([a : Acc2 (list '() '() e n)])
	    (match-let ([(list f* _ e n) (foldr parse-fml a f*)])
	      (let-values ([(t) (and t (parse-type t))]
			   [(b n) ((parse-expr b e) n)])
		(values (Ann (Lambda f* t b) s)  n))))
	  (raise-fml-exn stx))))
  (match stx*
    [(list fmls body)
     (help fmls #f body src env next)]
    [(list fmls (? colon?) type body)
     (help fmls type body src env next)]
    [otherwise (raise-lambda-exn stx* src)]))

(: parse-fml (Stx Acc2 . -> . Acc2))
(define (parse-fml stx acc)
  (define (help [s : Symbol] [t : (U Stx False)] [l : Src] [a : Acc2]) 
    : Acc2 
    (match-let ([(list f* s* e n) a])
      (let* ([t (if t (parse-type t) DYN-TYPE)]
	     [u (Uid (symbol->string s) n)]
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

(define-type Tmp-Bnd (List Uid (U Stx False) Stx))
(define-type Acc0 (List (Listof Tmp-Bnd) (Listof Symbol) Env Natural))
(define-type Acc1 (Pair S0-Bnd* Natural))

(define-syntax-rule (parse-let-form sym ctor env env-body env-bnd)
  (lambda (stx* src env next)
    (match stx*
      [(list bindings body)
       (let ([b* (syntax-unroll bindings)])
	 (if (not (stx-list? b*))
	     (raise-bnd-exn bindings)
	     (let ([a0 : Acc0 (list '() '() env next)]) ;;no unification in type checker
	       (match-let ([(list b* _ env-body n) (foldr unsplice-bnd a0 b*)])
		 (let ([a1 : Acc1 (cons '() n)]) ;; again
		   (match-let ([(cons b* n) (foldr (parse-bnd env-bnd) a1 b*)])
		     (let-values ([(e n) ((parse-expr body env-body) n)])
		       (values (Ann (ctor b* e) src) n))))))))]
      [otherwise (raise-let-exn form stx* src)])))

(: parse-letrec (-> (Listof Stx) Src Env Natural 
		    (values S0-Expr Natural)))
(define parse-letrec (parse-let-form letrec Letrec e1 e2 e2)) 

(: parse-let (-> (Listof Stx) Src Env Natural 
		 (values S0-Expr Natural)))
(define parse-let (parse-let-form let Let e1 e2 e1))

(: parse-bnd (Env . -> . (Tmp-Bnd Acc1 . -> . Acc1)))
(define (parse-bnd env)
  (lambda ([b : Tmp-Bnd] [a : Acc1]) 
    : Acc1
    (match-let ([(cons bnd* next) a]
		[(list uid type exp) b])
      (let-values ([(exp next) ((parse-expr exp env) next)]
		   [(type) (and type (parse-type type))])
	(cons (cons (Bnd uid type exp) bnd*) next)))))


(: unsplice-bnd (Stx Acc0 -> Acc0))
(define (unsplice-bnd stx acc)
  (: help (-> Symbol (U Stx False) Stx Src Acc0
	      Acc0))
  (define (help s t r l a)
    (match-let ([(list b* s* e n) a])
      (let ([u (Uid (symbol->string s) n)])
	(cond
	 [(memq s s*) (raise-duplicate-binding s l)]
	 [(reserved-symbol? s) (raise-reservered-sym s l)]
	 [else (list (cons (list u t r) b*) (cons s s*) 
		     (env-extend e s u)     (add1 n))]))))
  (let ([loc (syntax->srcloc stx)])
    (match (syntax-unroll stx)
      [(list (app syntax-unroll (? symbol? sym)) (? colon?) type rhs)
       (help sym type rhs loc acc)]
      [(list (app syntax-unroll (? symbol? sym)) rhs)
       (help sym #f rhs loc acc)]
      [otherwise (raise-bnd-exn stx)])))


(: parse-if (-> (Listof Stx) Src Env Natural 
		(values S0-Expr Natural)))
(define (parse-if stx* src env next)
  (match stx*
    [(list test then else)
     (let*-values ([(tst next) ((parse-expr test env) next)]
		   [(csq next) ((parse-expr then env) next)]
		   [(alt next) ((parse-expr else env) next)])
       (values (Ann (If tst csq alt) src) next))]
    [othewise (raise-if-exn stx* src)]))

(: parse-ascription (-> (Listof Stx) Src Env Natural
			(values S0-Expr Natural)))
(define (parse-ascription stx* src env next)
  (define (help [s : Stx] [t : Stx] [l : (U Stx False)] 
		[src : Src] [e : Env] [n : Natural]) 
    : (values S0-Expr Natural) 
    (let*-values ([(exp next) ((parse-expr s e) n)]
		  [(type) (parse-type t)]
		  [(lbl) (and l (parse-blame-label l))])
      (values (Ann (Ascribe exp type lbl) src) next)))
  (match stx*
    [(list exp type) (help exp type #f src env next)]
    [(list exp type lbl) (help exp type lbl src env next)]
    [othewise (raise-ascribe-exn stx* src)]))

(: parse-primitive (-> Schml-Prim (Listof Stx) Src Env Natural 
		       (values S0-Expr Natural)))
(define (parse-primitive sym stx* src env next)
  (let-values ([(args next) ((parse-expr* stx* env) next)])
    (values (Ann (Op sym args) src) next)))

(: parse-application (-> Stx (Listof Stx) Src Env Natural
			 (values S0-Expr Natural)))
(define (parse-application stx stx* src env next)
  (let*-values ([(rator next) ((parse-expr stx env) next)]
		[(rands next) ((parse-expr* stx* env) next)])
    (values (Ann (App rator rands) src) next)))

(: parse-expr* (-> (Listof Stx) Env (State Natural S0-Expr*)))
(define (parse-expr* stx* env)
  (do bind-state
    (let ([a : Stx  (car stx*)]
          [d : Stx* (cdr stx*)]))
    (a : S0-Expr  <- (parse-expr a env))
    (d : S0-Expr* <- (parse-expr* d env))
    (return-state (cons a d))))

#|
Parse Type converts syntax objects to the core forms that
represents types in the schml abstract syntax tree.
|#

(: parse-type (Stx . -> . Schml-Type))
(define (parse-type stx)
  (match (syntax-unroll stx)
    ['Int INT-TYPE]
    ['Bool BOOL-TYPE]
    ['Dyn  DYN-TYPE]
    [(list (app syntax-unroll fst) stx* ...)
     (match* (fst stx*)
       #| References and Vectors are the same in a general sense |#
       ;; Guarded
       [('GRef  (list stx))  (GRef (parse-type stx))]
       [('GVect (list stx))  (GVect (parse-type stx))]
       ;; Monotonic
       [('MRef  (list stx))  (MRef (parse-type stx))]
       [('MVect (list stx))  (MVect (parse-type stx))]
       #| I want to do gradual universal types |#
       ;;[('All (list id stx* ...)) (TODO actually parse universal types)]
       [(otherwise _) (parse-fn-type stx)])]
    [othewise (raise-type-exn stx)]))



(: parse-fn-type (Stx . -> . Schml-Type))
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
	    (Fn (length from) (map parse-type from) (car (map parse-type to))))))))
  


