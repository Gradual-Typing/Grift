#lang racket
(require Schml/framework/debug
         Schml/framework/build-compiler
         Schml/framework/helpers
	 Schml/framework/errors
         Schml/language/shared
         Schml/language/types
         Schml/language/syntax
         Schml/language/core
         syntax/parse)

(provide parse)


(define-pass (parse stx-tree comp-config)
  (lang-syntax? -> implicit-core?)
  
  (define (parse-expr* exp* env) 
    (for/list ([exp (in-list exp*)]) (parse-expr exp env)))

  (define (parse-expr exp env)
    (let ((src (syntax-srcloc exp)) (exp^ (syntax-e exp)))
    (cond
      [(pair? exp^) 
       (let* ((rator (car exp^)) (rands (cdr exp^)) (e (syntax-e rator)))
         (cond
	  [(pair? e) (App src #f (parse-expr rator env) (parse-expr* rands env))]
	  [(symbol? e)
	   (match 
	     (env-lookup env e (th (unbound src e (syntax->datum exp))))
	     [(Bnd:Var b)
	      (let ((vsrc (syntax-srcloc rator)))
		(App src #f (Var vsrc #f b) (parse-expr* rands env)))]
	     [(Bnd:Core t) (t exp env)]
	     [else (bad-syntax src e (strip exp))])]
           [else (bad-syntax src e (strip exp))]))]
      [(symbol? exp^)
       (match (env-lookup env exp^ (th (unbound src exp^ 'expression)))
	 [(Bnd:Var b) (Var src #f b)]
	 [otherwise (bad-syntax src exp^ 'expression)])]
      [(constant? exp^) (Const src #f exp^)]
      [else (cond-pass-error pass 'parse-expr exp)])))


(define (parse-type* ty* env)
  (for/list ((ty (in-list ty*))) (parse-type ty env)))

(define (parse-type ty env)
  (let ((ty^ (syntax-e ty)))
    (cond
     [(pair? ty^)
      (let* ((rator (car ty^))
             (rands (cdr ty^))
             (te (syntax-e rator)))
        (cond
         [(pair? te) (bad-syntax (syntax-srcloc ty) te ty)]
         [(symbol? te)
	  (match (env-lookup env te (th (unbound (syntax-srcloc ty)
						 te (syntax->datum ty))))
            [(Bnd:Type t) t]
            [(Bnd:Type-Core t) (t ty env)]
            [otherwise (bad-syntax (syntax-srcloc ty) te ty)])]
         [else (bad-syntax (syntax-srcloc ty) te ty)]))] 
    [(symbol? ty^)
     (match 
       (env-lookup env ty^ (th (unbound (syntax-srcloc ty) ty^ 'type)))
       [(Bnd:Type t) t]
       [otherwise (bad-syntax (syntax-srcloc ty) ty^ 'type)])]
    [else (cond-pass-error pass 'parse-type (strip ty))])))

(define (lambda-transformer exp env)
  (define (from-to-fml fml env)
    (syntax-case fml ()
      [(i k t) (and (eq? ': (syntax-e #'k))
                    (symbol? (syntax-e #'i)))
       (let* ((i (syntax-e #'i)) (u (uvar i)))
         (values i (Bnd:Var u) (Fml u (parse-type #'t env))))]
      [i (symbol? (syntax-e #'i))
         (let* ((i (syntax-e #'i)) (u (uvar i)))
           (values i (Bnd:Var u) (Fml u Dyn-Type)))]))
  (define (dup-args-error)
    (bad-syntax (syntax-srcloc exp) "identicle bindings" (strip exp)))
  (define (help type fmls body env)
    (let-values 
	([(fmls env) (parsef*/no-dup from-to-fml fmls env dup-args-error)]
	 [(src) (syntax-srcloc exp)])
      (Lambda src (and type (parse-type type env)) fmls (parse-expr body env))))
  (syntax-case exp ()
    [(_ (f ...) k t b) (eq? ': (syntax-e #'k)) (help #'t #'(f ...) #'b env)]
    [(_ (f ...) b) (help #f #'(f ...) #'b env)]))

(define (parsef*/no-dup make-from-to-struct s* env err-th)
  (let-values (((p* ext-env)
		(let loop ((s* (syntax-e s*)))
		  (if (null? s*)
		      (values '() (empty-env))
		      (let*-values
			  (((p* ext-env-set) (loop (cdr s*)))
			   ((from to p) (make-from-to-struct (car s*) env)))
			(when (env-lookup ext-env-set from #f)
			  (err-th))
			(values (cons p p*)
				(env-extend ext-env-set from to)))))))
    (values p* (env-extend/env env ext-env))))

(define (let-transformer exp env)
  (define (from-to-bnd bnd env^)
    (syntax-case bnd ()
      [(i k t e) (and (eq? ': (syntax-e #'k))
		      (symbol? (syntax-e #'i)))
       (let* ((i (syntax-e #'i)) (u (uvar i)))
         (values i (Bnd:Var u) (Bnd u (parse-type #'t env) (parse-expr #'e env))))]
      [(i e) (symbol? (syntax-e #'i))
       (let* ((i (syntax-e #'i)) (u (uvar i)))
         (values i (Bnd:Var u) (Bnd u #f (parse-expr #'e env))))]))
  (define (dup-err)
    (bad-syntax (syntax-srcloc exp) "identicle bindings" (strip exp)))
  (let ((src (syntax-srcloc exp)))
    (syntax-case exp ()
      [(_ (b ...) body) 
       (let-values ([(binds env) (parsef*/no-dup from-to-bnd #'(b ...) env dup-err)])
         (Let src #f binds (parse-expr #'body env)))])))

(define (if-transformer stx env)
  (syntax-case stx ()
    [(_ t c a) (If (syntax-srcloc stx) #f
                   (parse-expr #'t) 
                   (parse-expr #'c) 
                   (parse-expr #'a))]))

(define (cast-transformer stx env)
  (syntax-case stx ()
    [(_ e t l) (string (syntax-e #'l))
     (Cast (syntax-srcloc stx) #f
           (parse-expr #'e) (parse-type #'t) (syntax-e #'l))]
    [(_ e t)
     (let ((src (syntax-srcloc stx)))
       (Cast src #f (parse-expr #'e) (parse-type #'t) (srcloc->string src)))]))

(define (lt-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Rel:Int:< (syntax-srcloc stx) Bool-Type
                             (parse-expr #'n env) (parse-expr #'m env))]))

(define (gt-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Rel:Int:> (syntax-srcloc stx) Bool-Type
                             (parse-expr #'n env) (parse-expr #'m env))]))

(define (eq-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Rel:Int:= (syntax-srcloc stx) Bool-Type
                             (parse-expr #'n env) (parse-expr #'m env))]))

(define (lteq-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Rel:Int:<= (syntax-srcloc stx) Bool-Type
                              (parse-expr #'n env) (parse-expr #'m env))]))
(define (gteq-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Rel:Int:>= (syntax-srcloc stx) Bool-Type
                              (parse-expr #'n env) (parse-expr #'m env))]))

(define (mult-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Bin:Int:* (syntax-srcloc stx) Int-Type
                             (parse-expr #'n env) (parse-expr #'m env))]))
(define (plus-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Bin:Int:+ (syntax-srcloc stx) Int-Type
                             (parse-expr #'n env) (parse-expr #'m env))]))
(define (minus-transformer stx env)
  (syntax-case stx ()
    [(_ n m) (Prim:Bin:Int:- (syntax-srcloc stx) Int-Type
                             (parse-expr #'n env) (parse-expr #'m env))]))

(define (function-type-transformer stx env)
  (syntax-case stx ()
    [(_ f t) (Function #'f #'t)]))

(struct Binding (value))
(struct Bnd:Var  Binding ())
(struct Bnd:Core Binding ())
(struct Bnd:Type Binding ())
(struct Bnd:Type-Core Binding ())




(define core-env
  (hasheq
   'lambda    (Bnd:Core lambda-transformer)
   'let  (Bnd:Core let-transformer)
   'if   (Bnd:Core if-transformer)
   ':    (Bnd:Core cast-transformer)
   'Dyn  (Bnd:Type Dyn-Type)
   'Int  (Bnd:Type Int-Type)
   'Bool (Bnd:Type Bool-Type)
   '->   (Bnd:Type-Core function-type-transformer)
   '%<   (Bnd:Core lt-transformer)
   '%>   (Bnd:Core gt-transformer)
   '%=   (Bnd:Core eq-transformer)
   '%<=  (Bnd:Core lteq-transformer)
   '%>=  (Bnd:Core gteq-transformer)
   '%*   (Bnd:Core mult-transformer)
   '%+   (Bnd:Core plus-transformer)
   '%-   (Bnd:Core minus-transformer)))

(match stx-tree
  [(File name stx*)
   (Prog name #f (for/list ([stx stx*]) (parse-expr stx core-env)))]
  [otherwise (match-pass-error pass 'parse-file stx-tree)]))


;; Unnoteworthy helpers
(define (syntax-srcloc exp)
  (srcloc (syntax-source exp) (syntax-line exp)
          (syntax-column exp) (syntax-position exp) (syntax-span exp)))
(define-syntax strip
  (syntax-rules ()
    [(_ stx) (syntax->datum stx)]))

(define (desugar-arrow-notation stx)
    (match (syntax-e stx)
      [(list (? (lambda (s) (not (eq? '-> (syntax-e s)))) 
                (app desugar-arrow-notation args)) ...
             (? (lambda (s) (eq? '-> (syntax-e s)))) 
             (app desugar-arrow-notation return))
       #`(-> #,args #,return)]
      [(list (? (lambda (s) (not (eq? '-> (syntax-e s)))) 
                     (app desugar-arrow-notation args)) ...
                  (? (lambda (s) (eq? '-> (syntax-e s)))) 
                  rest ...)
       #`(-> #,args #,(desugar-arrow-notation rest))]
      [otherwise stx]))
    
(module+ main
  (local-require rackunit rackunit/text-ui)
  (define tests
    (test-suite "all"
      (test-suite "desugar-arrow-notation"
        (test-equal? "simple" (desugar-arrow-notation #'a) #'a)))))



          
  

