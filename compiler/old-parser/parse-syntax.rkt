#lang racket

(require Schml/framework/build-compiler
         Schml/language/shared
         Schml/language/implicit-core
         Schml/language/syntax
         Schml/language/types)
(provide parse-syntax)

(define-pass (parse-syntax stx config)
  (lang-syntax? -> (lambda (s) #t))
  (define (parse-file e)
    (match e
      [(File name stx*)
       (Prog name (for/vector #:length (length stx*)
                              ([stx stx*])
                    (parse-expr stx (hasheq))))]
      [a (error pass "Match")]))
  (define (parse-expr exp env)
    (let ((src (syntax-source exp)))
      (syntax-case exp (lambda let if :)
        [(lambda (fml ...)  e)
         (let-values ([(fmls env) (parse-fmls #'(fml ...) env)])
           (Lambda src fmls (parse-expr #'e env)))]
        [(let bnds e) 
         (let-values ([(binds env) (parse-bindings #'bnds env)])
           (Let src binds (parse-expr #'e env)))]
        [(if t c a) (If src (parse-expr #'t env) 
                        (parse-expr #'c env) 
                        (parse-expr #'e env))]
        [(e : t) (Cast src (parse-expr #'e env) (parse-type #'t) #f)]
        [(e : t l) (string? (syntax-e #'l)) (Cast src (parse-expr #'e env)
                                                  (parse-type #'t) #'l)]
        [(a . d) (let ((p  (symbolic-primitiveq (syntax-e #'a))))
                   (if p
                       (Op src p (parse-expr* #'d env))
                       (App (parse-expr #'a env) (parse-expr* #'d env))))]
        [o (let ((o (syntax->datum #'o)))
             (cond
              [(symbol? o) (Var src (hash-ref env o (lambda () (env-err o))))] 
              [(self-evaluating? o) (Const src o)]
              [else (error 'syntax "~a" o)]))])))
  (define (parse-expr* exp* env)
    (for/vector #:length (length (syntax-e exp*)) ([exp (in-list exp*)])
                (parse-expr exp env)))
  (define (env-err ident)
    (error 'syntax "Unbound Identifier ~a" ident))
  (define (parse-fmls fml* env)
    (let ((fml* (syntax-e fml*)))
      (for/fold ([fmls (make-vector (length fml*))] [env env]) 
          ([fml (in-list fml*)] [i (in-naturals)])
        (let ((src (syntax-source fml)))
          (syntax-case fml (:)
            [(ident : t) 
             (let ((ident (syntax->datum #'ident)))
               (if (symbol? ident)
                   (let ((u (uvar ident))
                         (t (parse-type #'vector)))
                     (vector-set! fmls i (Typed-Fml src u t))
                     (values fmls (hash-set env ident u)))
                   (error 'syntax-5)))]
            [ident
             (let ((ident (syntax->datum #'ident)))
               (if (symbol? ident)
                   (let ((u (uvar ident)))
                     (vector-set! fmls i (Untyped-Fml src u))
                     (values fmls (hash-set env ident u)))
                   (error 'syntax-6)))])))))
  (define (parse-bindings bnd* env)
    (let ((bnd* (syntax-e bnd*)))
      (for/fold ([bnds (make-vector (length bnd*))] [post-env env]) 
          ([bnd (in-list bnd*)] [i (in-naturals)])
        (let ((src (syntax-source bnd)))
          (syntax-case bnd (:)
            [(ident : t e) 
             (let ((ident (syntax->datum #'ident)))
               (if (symbol? ident)
                   (let ((u (uvar ident))
                         (t (parse-type #'t))
                         (e (parse-expr #'e env)))
                     (vector-set! bnds i (Typed-Binding src u t e))
                     (values bnds (hash-set post-env ident u)))
                   (error 'syntax-5)))]
            [(ident e)
             (let ((ident (syntax->datum #'ident)))
               (if (symbol? ident)
                   (let ((u (uvar ident)) (e (parse-expr #'e env)))
                     (vector-set! bnds i (Untyped-Binding src u e))
                     (values bnds (hash-set post-env ident u)))
                   (error 'syntax-6)))])))))
  (define (parse-type* ty* env) 
    (for/vector ([ty (in-list (syntax-e ty*))]) (parse-type ty env)))
  (define (parse-type ty env)
    (syntax-case ty (-> Int Bool Dyn)
      [(ta ... -> tr) (Function (parse-type* #'(ta ...) env)
                                (parse-type #'tr env))]
      [Int Int-Type]
      [Bool Bool-Type]
      [Dyn Dyn-Type]))
  (parse-file stx))
             
