#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/introduce-closure-primitives                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)

(require Schml/language/shared
         (prefix-in s: Schml/language/closure-il3)
         (prefix-in t: Schml/language/closure-il4))
;; Only the pass is provided by this module
(provide introduce-closure-primitives)

(define-pass (introduce-closure-primitives prgm comp-config)
  ;; A holder for now of a check that we must perform if we
  ;; are going to optimize direct calls
  (define uvar->field-name uvar->string)
  (define s:Code-Label? (lambda (a) #f))
  (define code-field-name "code")
  (define (icp-binding lbl ty fml* free* exp)
    (let* ((env (mk-env (car free*) (cdr free*)))
           (exp ((icp-expr env) exp))
           (lam (t:Lambda ty fml* exp)))
      (Bnd:Ty lbl lam ty)))
  (define (fml->type.field f)
    `(,(Fml:Ty-type f) . ,(uvar->field-name (Fml-id f))))
  (define (icp-closures var lbl free* ty)
    (let* ((field-decl (cons `(,Code-Label-Type . "code") (map fml->type.field free*)))
           (pexp (t:Prim ty (Closure-field:Build field-decl))))
      (Bnd:Ty var pexp ty)))
  (define (cset-fold env)
    ;; cset* is an accumulator that is used with foldr in icp-let-proc 
    ;; in order to append the sublist together 
    (lambda (clos-var clos-ty clos-code-lbl free-fml* clos-set-exp*) 
      ;; Build the first cset by hand because it it made from a label
      (let* ((lhs-exp (t:Var clos-ty clos-var))
             (rhs-exp (t:Label Code-Label-Type clos-code-lbl))
             (cset (Closure-field:Set! lhs-exp code-field-name rhs-exp))
             (pexp (t:Prim Void-Type cset)))
        (cons pexp
              (for/fold ([cset* clos-set-exp*]) 
                  ([free-fml (in-list free-fml*)])
                (let ((free-var (Fml-id free-fml))
                      (free-var-ty (Fml:Ty-type free-fml)))
                  (cons (mk-cset lhs-exp free-var free-var free-var-ty env) 
                        cset*)))))))
  (define (mk-cset clos field-var free-var free-var-ty env)
    (let ((rhs-exp (lookup free-var free-var-ty env))
          (field-name (uvar->field-name field-var)))
      (t:Prim Void-Type (Closure-field:Set! clos field-name rhs-exp)))) 
  (define (lookup var var-ty env)
    (env-lookup env var (th (t:Var var-ty var))))
  (define (extend clos)
    (let ((clos-var (t:Var (Fml:Ty-type clos) (Fml-id clos))))
      (lambda (free env)
        (let* ((free-uvar (Fml-id free))
               (free-ty   (Fml:Ty-type free))
               (clos-prim (Closure-field:Ref clos-var (uvar->field-name free-uvar)))
               (prim-exp (t:Prim free-ty clos-prim)))
          (env-extend env free-uvar prim-exp)))))
  (define (mk-env clos free*)
    (foldr (extend clos) (empty-env) free*))
  ;; A destructuring of let-proc view
  (define (destruct-let-proc o)
    (values (s:Expr-ty o) (s:Let-Proc-bindings o) 
            (s:Let-Proc-closures o) (s:Let-Proc-exp o)))
  (define (destruct-bnd:ty o) (values (Bnd-id o) (Bnd-exp o) (Bnd:Ty-type o)))
  (define (destruct-lambda o)
    (values (s:Lambda-ty o) (s:Lambda-fmls o) (s:Lambda-free o) (s:Lambda-exp o)))
  (define (destruct-close-over o)
    (values (s:Close-Over-code o) (s:Close-Over-vars o)))
  (define (rebuild-let-proc ty bindings closures closure-inits body)
    (t:Let-Proc ty bindings 
                (t:Let ty closures 
                       (t:Begin ty closure-inits body))))
  (define (icp-let-proc icp-expr/env env exp)
    ;; I could do better than this by restructuring instead the three step process
    ;; The catamorphism of let-proc
    ;; Destructure the let-proc form a little complicated
    (let*-values ([(lp-ty lp-bnd-lam* lp-bnd-clos* exp) 
                   (destruct-let-proc exp)]
                  [(lam-lbl* lam* lam-ty*) 
                   (map/values destruct-bnd:ty ('() '() '()) lp-bnd-lam*)]
                  [(clos-var* clos* clos-ty*) 
                   (map/values destruct-bnd:ty ('() '() '()) lp-bnd-clos*)]
                  [(lam-ty* lam-fml** lam-free** lam-exp*)
                   (map/values destruct-lambda ('() '() '() '()) lam*)]
                  [(clos-lbl* clos-free**) 
                   (map/values destruct-close-over ('() '()) clos*)]
                  ;; rebuild and process the components
                  [(body) (icp-expr/env exp)]
                  [(bindings)
                   (map icp-binding lam-lbl* lam-ty* lam-fml** lam-free** lam-exp*)]
                  [(closures)
                   (map icp-closures clos-var* clos-lbl* clos-free** clos-ty*)]
                  [(closure-inits)
                   (foldr (cset-fold env) '() clos-var* clos-ty* clos-lbl* clos-free**)])
      (rebuild-let-proc lp-ty bindings closures closure-inits body)))
  (define (icp-expr env)
    (define (recur exp)
      (match exp
        [(? s:Let-Proc? lp) (icp-let-proc recur env lp)]
        [(s:Var t i) (lookup i t env)]
        [(s:App ty exp (list (app recur exp*) ...))
         (let* ((exp (recur exp))
                (exp-ty (t:Expr-ty exp))
                (prim (Closure-field:Ref exp code-field-name))
                (pexp (t:Prim exp-ty prim)))
           (t:App ty pexp exp*))
         ;; (if (s:Code-Label? exp)
         ;;    (t:App ty exp exp*))
         ] 
        [(? s:Code-Label? cl) cl]
        [(s:Let ty (list (Bnd:Ty i* (app recur e*) t*) ...) exp)
         (t:Let ty (map Bnd:Ty i* e* t*) (recur exp))]
        [(s:Cast ty-cast (app recur exp) ty-exp label)
         (t:Cast ty-cast exp ty-exp label)]
        [(s:If ty tst csq alt)
         (t:If ty (recur tst) (recur csq) (recur alt))]
        [(s:Prim ty pexp) (icp-prim recur ty pexp)]
        [(s:Const t k) (t:Const t k)]
        [e (match-pass-error pass 'icp-expr e)]))
    recur)
  (define (icp-prim icp-expr ty pexp)
    (match pexp
      [(Op:IntxInt (app icp-expr fst) (app icp-expr snd))
       (t:Prim ty (mk-op:int-int pexp fst snd))]
      [(Rel:IntxInt (app icp-expr fst) (app icp-expr snd))
       (t:Prim ty (mk-rel:int-int pexp fst snd))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  (match prgm
    [(s:Prog n u t e)
     (t:Prog n u t ((icp-expr (empty-env)) e))]
    [otherwise (match-pass-error pass 'body prgm)]))
