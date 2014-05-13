#lang racket
(require Schml/framework/debug
         Schml/framework/build-compiler
         Schml/language/shared
         Schml/language/types
         Schml/language/syntax
         Schml/language/implicit-core
         syntax/parse)
(provide parse)


(define-pass (parse stx-tree comp-config)
  (lang-syntax? -> implicit-core?)
  (match stx-tree
    [(File name stx*)
     (Prog name (for/vector #:length (length stx*)
                            ([stx stx*])
                            (parse-expr stx (hasheq))))]
    [a (error pass "Match")]))

(define (parse-expr exp env)
  (let ((src (syntax-source exp)))
    (syntax-parse exp
      [((~datum lambda) . rest) (parse-lambda #'rest src env)]
      [(a . d) (parse-app #'a #'d src env)]
      [o (parse-atom #'o src env)])))

(define (parse-expr* exp* env) 
  (for/list ([exp (in-list exp*)]) (parse-expr exp env)))

(define (parse-lambda exp src env)
  (define (parse-fmls fmls env)
    (values '() '()))
  (syntax-case exp ()
    [((x ...) b)
     (let-values ([(fmls env) (parse-fmls #'(x ...) env)])
       (Lambda src fmls (parse-expr #'e env)))]))

(define (parse-app rand rator* src env)
  (let ((p  (symbolic-primitiveq (syntax-e #'a))))
    (if p
        (Op src p (parse-expr* #'d))
        (App src (parse-expr #'a env) (parse-expr* #'d env)))))

(define (parse-atom atm src env)
  (let ((atm (syntax->datum atm)))
    (cond ;;(hash-ref env o (lambda () (env-err o)))
     [(symbol? atm) (Var src atm)] 
     [(self-evaluating? atm) (Const src atm)]
     [else (error 'syntax "~a" atm)])))



