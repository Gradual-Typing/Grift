#lang racket

(require Schml/framework/build-compiler
         Schml/language/shared
         Schml/language/implicit-core
         Schml/language/syntax)



(define-pass (parse-syntax syntax config)
  ((lambda (s) #t) -> (lambda (s) #t))
  (define (parse-prog e)
    (match e
      [(File name stx*)
       (Prog name (for/vector #:length (length stx*)
                              ([stx stx*])
                    (parse-expr stx)))]
      [a (error pass "Match")]))
  (define (parse-expr exp env)
    (syntax-case exp (lambda let if :)
        [(lambda (fml ...)  e) (syntax (fml ...))]
   ))
;; (let-values ([(fmls env) (parse-fmls #'(fml ...) env)])
;;   (Lambda src fmls (parse-expr #'e env)))
;;      [(_ bnds e) 
;;       (let-values ([(binds env) (parse-bindings (syntax bnds) env)])
;;         (Let src binds (parse-expr #'e env)))]
;;      [(if t c a) (If src (parse-expr #'t env) 
;;                      (parse-expr #'c env) 
;;                      (parse-expr #'e env))]
;;      [(e : t) (Cast src (parse-expr #'e env) (parse-type #'t) #f)]
;;      [(e : t l) (string? strip l) (Cast src (parse-expr e env)
;;                                         (parse-type #'t) l)]
;;      [(p . d) (Primitive? p) (Op src (get-prim p) (parse-expr* d env))]:
;;      [(a . d) (App (parse-expr a env) (parse-expr* d env))]
;;      [o (let ((o (syntax->datum o)))
;;           (cond
;;            [(symbol? o) (Var src o (env-ref env o))] 
;;            [(constant? o) (Const src o)]
;;            [else (error 'syntax "~a" o)]))]

  (define (parse-expr* exp* env)
    (for/vector #:length (length (syntax-e fml*)) ([(exp (in-list exp*))])
                (parse-expr exp env)))
  (define (parse-fmls fml* env)
    (let ((fml* (syntax-e fml*)))
      (for/fold ([fmls (make-vector (length fml*))] [env env]) 
          ([fml (in-list fml*)] [i (in-naturals)])
        (syntax-case fml (:)
          [(ident : t) 
           (let ((ident (syntax->datum #'ident)))
             (if (symbol? ident)
                 (let ((u (uvar ident))
                       (t (parse-type #'vector)))
                   (vector-set! bnds i (Typed-Binding src u t e))
                   (values bnds (extend-env post-env ident u)))
                 (error 'syntax-5)))]
          [ident
           (let ((ident (syntax->datum #'ident)))
             (if (symbol? ident)
                 (let ((u (uvar ident)))
                   (vector-set! bnds i (Untyped-Fml src u))
                   (values bnds (extend-env env ident u)))
                 (error 'syntax-6)))]))))
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
                     (values bnds (extend-env post-env ident u)))
                   (error 'syntax-5)))]
            [(ident e)
             (let ((ident (syntax->datum #'ident)))
               (if (symbol? ident)
                   (let ((u (uvar ident)) (e (parse-expr #'e env)))
                     (vector-set! bnds i (Untyped-Binding src u e))
                     (values bnds (extend-env post-env ident u)))
                   (error 'syntax-6)))])))))
  (parse-prog e))
             
