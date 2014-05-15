#lang racket
(require Schml/framework/debug
         Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/language/shared
         Schml/language/types
         Schml/language/syntax
         Schml/language/core
         syntax/parse)
(provide parse)


(define-pass (parse stx-tree comp-config)
  (lang-syntax? -> implicit-core?)
  (match stx-tree
    [(File name stx*)
     (Prog name #f (for/list ([stx stx*]) (parse-expr stx core-env)))]
    [otherwise (error pass "Match ~a" otherwise)]))

(define (parse-expr* exp* env) 
  (for/list ([exp (in-list exp*)]) (parse-expr exp env)))

(define (parse-expr exp env)
  (let ((src (syntax-srcloc exp)) (exp^ (syntax-e exp)))
    (cond
      [(pair? exp^)
       (let* ((rator (car exp^))
              (rands (cdr exp^))
              (e (syntax-e rator)))
         (cond
           [(pair? e) (App src #f
                           (parse-expr rator env)
                           (parse-expr* (syntax-e rands) env))]
           [(symbol? e)
            (match (env-lookup env e (lambda () (error 'unbound)))
                [(Bnd:Var b)
                 (let ((vsrc (srcloc (syntax-source rator)
                                     (syntax-line rator)
                                     (syntax-column rator)
                                     (syntax-position rator)
                                     (syntax-span rator))))
                   (App src #f
                        (Var vsrc #f b)
                        (parse-expr* (syntax-e #'rands) env)))]
                [(Bnd:Core t) (t exp env)]
                [else (error 'incorrect-binding)])]
           [else (error 'invalid-syntax)]))]
      [(symbol? exp^)
       (let ((b (env-lookup env exp^ (lambda () (error 'unbound)))))
         (match b
           [(Bnd:Var b) (Var src #f b)]
           [otherwise (error 'invalid-syntax)]))]
      [(constant? exp^) (Const src #f exp^)]
      [else (error 'invalid-thing-for-unkown-reason)])))


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
         [(pair? te) (error 'parse-type "not an available parse")]
         [(symbol? te)
          (match (env-lookup env te (lambda () (error 'unbound)))
            [(Bnd:Type t) t]
            [(Bnd:Type-Core t) (t ty env)]
            [otherwise (error 'invalid-syntax)])]
         [else (error 'invalid-syntax)]))] 
    [(symbol? ty^)
     (match (env-lookup env ty^ (lambda () (error 'unbound)))
       [(Bnd:Type t) t]
       [otherwise (error 'syntax-error)])]
    [else (error 'invalid-thing-for-unkown-reason)])))

(define (lambda-transformer exp env)
  (define (parse-fml* fml* env)
    (let-values (((fml* ext-to-env)
                  (let loop ((fml* fml*))
                    (if (null? fml*)
                        (values '() (empty-env))
                        (let*-values
                            (((fmls env-set) (loop (cdr fml*)))
                             ((ident fml env)(parse-fml (car fml*) env-set)))
                          (unless (env-lookup env-set ident #f)
                            (error 'syntax-error))
                          (values (cons fml fmls) env))))))
      (values fml* (env-extend/env env ext-to-env))))
  (define (parse-fml fml env)
    (syntax-case fml ()
      [(i k t) (and (eq? ': (syntax-e #'k))
                    (symbol? (syntax-e #'i)))
       (let* ((i (syntax-e #'i)) (u (uvar i)))
         (values i (Fml u (parse-type #'t env)) (env-extend env i (Bnd:Var u))))]
      [i (symbol? (syntax-e #'i))
         (let* ((i (syntax-e #'i)) (u (uvar i)))
           (values i (Fml u Dyn-Type) (env-extend env i (Bnd:Var u))))]))
  (define (help type fmls body env)
    (let-values ([(fmls env) (parse-fml* (syntax-e fmls) env)]
                 [(src) (syntax-srcloc exp)])
      (Lambda src (and type (parse-type type env))
              fmls (parse-expr #'b env))))
  (syntax-case exp ()
    [(_ (f ...) k t b) (eq? ': (syntax-e #'k)) (help #'t #'(f ...) #'b env)]
    [(_ (f ...) b) (help #f #'(f ...) #'b env)]))

(define (let-transformer exp env)
  (define (parse-binding* bnd* env)
    (let-values (((bnd* ext-to-env)
                  (let loop ((bnd* bnd*))
                    (if (null? bnd*)
                        (values '() (empty-env))
                        (let*-values
                            (((bnds env-set) (loop (cdr bnd*)))
                             ((ident bnd env)(parse-bnd (car bnd*) env-set)))
                          (unless (env-lookup env-set ident #f)
                            (error 'syntax-error))
                          (values (cons bnd bnds) env))))))
      (values bnd* (env-extend/env env ext-to-env))))
  (define (parse-bnd bnd env^)
    (syntax-case bnd ()
      [(i k t e) (and (eq? ': (syntax-e #'k))
                    (symbol? (syntax-e #'i)))
       (let* ((i (syntax-e #'i)) (u (uvar i)))
         (values i 
                 (Bnd u (parse-type #'t env) (parse-expr #'e env)) 
                 (env-extend env^ i (Bnd:Var u))))]
      [(i e) (symbol? (syntax-e #'i))
       (let* ((i (syntax-e #'i)) (u (uvar i)))
         (values i 
                 (Bnd u #f (parse-expr #'e env)) 
                 (env-extend env^ i (Bnd:Var u))))]))
  (let ((src (syntax-srcloc exp)))
    (syntax-case exp ()
      [(_ (b ...) body) 
       (let-values ([(binds env) (parse-binding* (syntax-e #'(b ...)) env)])
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

(define (syntax-srcloc exp)
  (srcloc (syntax-source exp) (syntax-line exp)
          (syntax-column exp) (syntax-position exp) (syntax-span exp)))

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



          
  

