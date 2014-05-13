#lang racket
(require Schml/framework/debug
         Schml/framework/build-compiler
         Schml/framework/helpers
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
  ;; This could be converted to an assosiative environment as
  ;; part of env. This would enable lexical rebinding of core
  ;; forms.
  (let ((src (srcloc (syntax-source exp) (syntax-line exp)
                     (syntax-column exp) (syntax-position exp)
                     (syntax-span exp))))
    (syntax-parse exp
      [((~datum lambda) . rest)   (parse-lambda exp src env)]
      [((~datum let) . rest) (parse-lambda exp src env)]
      [((~datum if) . rest)  (parse-if exp src env)]
      [(e (~datum :) . rest) (parse-cast exp src env)]
      [(a . d)               (parse-app exp src env)]
      [o                     (parse-atom exp src env)])))

(define (parse-expr* exp* env) 
  (for/list ([exp (in-list exp*)]) (parse-expr exp env)))

(define (parse-lambda exp src env)
  (define (make-fmls ident* type* env)
    (if (null? ident*)
        (values '() env)
        (let-values (((fmls env) (make-fmls (cdr ident*) (cdr type*) env)))
          (let* ((i (syntax->datum (car ident*)))
                 (u (uvar i))
                 (t (parse-type (car type*) env)))
            (values (cons (Fml u t) fmls) (env-extend env i u))))))
  (syntax-parse exp
    [(_ f:fmls-pat (~datum :) t:type-pat b)
     (let-values ([(fmls env) (make-fmls (syntax-e #'(f.ident ...)) 
                                         (syntax-e #'(f.type ...)) env)])
       (Lambda src fmls (parse-type #'t) (parse-expr #'b env)))]
    [(_ f:fmls-pat b)
     (let-values ([(fmls env) (make-fmls (syntax-e #'(f.ident ...)) 
                                         (syntax-e #'(f.type ...)) env)])
       (Lambda src fmls #f (parse-expr #'b env)))]))

(define (parse-let exp src env)
  (define (make-bindings bind* env)
    (define (help i t e b* g)
      (let* ((i (syntax->datum i)) 
             (u (uvar i)) 
             (t (and t (parse-type t g))))
        (values (cons (Bnd u t e) b*) (env-extend env i u))))
    (if (null? bind*)
        (values '() env)
        (let-values (((binds env) (make-bindings (cdr bind*) env)))
          (syntax-parse (car bind*)
            [(ident:id (~datum :) type:type-pat expr) 
             (help #'ident #'type #'expr binds env)]
            [(ident:id expr) (help #'ident #f #'expr #'binds env)]))))
  (syntax-parse exp
    [(_ b:binds-pat body)
     (let-values ([(binds env) (make-bindings (syntax-e #'b) env)])
       (Let src binds (parse-expr #'body env)))]))

(define (parse-if rest src env) '())

(define (parse-cast exp rest src env)
  (syntax-case rest ()
    [(t) (Cast src (parse-expr #'exp env) (parse-type #'t) (srcloc->string src))]
    [(t l) (string? (syntax->datum #'l))
     (Cast src (parse-expr #'exp env) (parse-type #'t) #'l)]))

(define (parse-type ty)
  '())

(define (parse-app rand rator* src env)
  (let ((p  (symbolic-primitiveq (syntax-e #'a))))
    (if p
        (Op src p (parse-expr* #'d))
        (App src (parse-expr #'a env) (parse-expr* #'d env)))))

(define (parse-atom atm src env)
  (let ((atm (syntax->datum atm)))
    (cond 
     [(symbol? atm) (Var src (env-lookup env atm 'TODO-ERROR))] 
     [(self-evaluating? atm) (Const src atm)]
     [else (error 'syntax "~a" atm)])))

;;; Syntax Patterns not sure where to put these
(define-syntax-class fmls-pat
  #:description "formal parameter list"
  (pattern (fml:fml-pat ...)
           #:fail-when (check-duplicate-identifier
                        (syntax->list #'(fml.ident ...)))
           "duplicated formal parameter"
           #:with (ident ...) #'(fml.ident ...)
           #:with (type ...) #'(fml.type ...)))

;; the pattern that destructures a fml
(define-syntax-class fml-pat
  #:description "formal parameter"
  (pattern ident:id
           #:with type #'Dyn)
  (pattern (ident:id (~datum :) type:type-pat)))

;; The pattern that quickly checks let binding syntax
;; must manually descontruct because I do not know how
;; to side channel a binding type that doesn't exist
(define-syntax-class binds-pat
  #:description "variable binding list"
  (pattern (bind:bind-pat ...)
           #:fail-when (check-duplicate-identifier
                        (syntax->list #'(bind.ident ...)))
           "duplicated formal parameter"))

;; the pattern checks for correct syntax in let bindings
(define-syntax-class bind-pat
  #:description "variable binding"
  (pattern (ident:id expr) #:with type #f)
  (pattern (ident:id (~datum :) type:type-pat expr)))

;; The pattern that performs a quick check that type is correct
(define-syntax-class type-pat
  #:description "type"
  (pattern ident:id)
  (pattern (in:type-pat ... (~datum ->) out:type-pat)))
  
