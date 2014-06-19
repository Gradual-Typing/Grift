#lang racket
#|------------------------------------------------------------------------------+
|Pass: testing/ast-interps/insert-implicit-casts                                |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
|Input Grammar "Explicitly-Typed-Core-Forms" found in Schml/languages/core.rkt  |
|Prog   = (Prog File-Name {TExpr} Type)                                         |
|TExpr  = {Expr}                                                                |
|Expr   = (Lambda {uvar}* {Type} {Expr} {Src})                                  |
|       | (Var {Uvar} {Type} {Src})                                             |
|       | (App {Expr} {Expr}* {Type} {Src})                                     |
|       | (Op Prim {Expr}* {Type} {Src})                                        |
|       | (Cast {Expr} {Type} {Type} {Blame})                                   |
|       | (If {Expr} {Expr} {Expr} {Type} {Src})                                |
|       | (Let {BndSet} {Expr} {Type} {Src})                                    |
|       | (Const {Imdt} {Type})                                                 |
|BndSet = ({Uvar} {Expr})                                                       |
|Fml    = {Uvar} | ({Uvar} {Type})                                              |
|Src    = (Src File Line Col Span)                                              |
|Blame  = {Src} | {String}                                                      |
|Uvar   = A Symbol with the format 'original$uniqueness                         |
|Imdt   = Fixnums and Booleans                                                  |
|Prim   = op:fix:* | op:fix:+ | op:fix:- | op:fix:and | op:fix:or | op:fix:>>   |
|       | op:fix:<< | relop:fix:< | relop:fix:<= | relop:fix:=  | relop:fix:>=  |
|       | relop:fix:>                                                           |
|Type   = Fix | Bool | Dyn | ({Type}* -> {Type})                                |
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)
;; The constuctors of the core language
(require Schml/language/sourceless-core)
;; Only the pass is provided by this module
(provide interp)



(struct casted-value (label val type1 type2))

(define (mk-cast l e t g)
  (if (equal? t g) e (casted-value l e t g)))

(define (interp prgm comp-config)
  
  (define (interp-expr cast apply)
    (define (recur exp env)
      (define (recur/env e) (recur e env))
      (match exp
        [(Lambda _ (list (Fml:Ty id* _) ...) body)
         (lambda arg* (recur body (env-extend* env id* arg*)))]
        [(Let _ (list (Bnd:Ty id* (app recur/env exp*) _) ...) body)
         (recur body (env-extend* env id* exp*))]
        [(Var _ id) (env-lookup env id (th (unbound-var-error id)))]
        [(Const _ k) k]
        [(If _ (app recur/env tst) csq alt)
         (if tst (recur/env csq) (recur/env alt))]
        [(App _ (app recur/env v) (list (app recur/env v*) ...))
         (apply v v*)]
        [(Prim _ pexp) (iic-delta pexp recur/env)]
        [(Cast t-casted (app recur/env val) t-exp label)
         (cast label val t-exp t-casted)]
        [e (error 'interp "Umatched expression ~a" e)]))
    recur)
  
  ;; The lazy-d parts
  (define (apply-cast-ld l1 v1 t1 t2)
    (if (shallow-consistent? t1 t2)
        (if (Dyn? t1)
            (match v1
              [(casted-value l2 v2 t3 t1)
               (apply-cast-ld l1 v2 t3 t2)]
              [o (error 'interp "Unexpected value in apply-cast-ld match ~a" o)])
            (mk-cast l1 v1 t1 t2))
        (raise l1)))
  
  (define (apply-lazy cast)
    (define (arrity-mismatch l) (th (raise l)))
    (define (recur rator rands)
      (match rator
        [(casted-value l rator^ (Function t1* t2) (Function t3* t4))
         (let* ((rands^ (map/length= (lambda (e t1 t2)
                                       (cast l e t1 t2)) rands t3* t1*
                                       (arrity-mismatch l))) 
                (result (recur rator^ rands^)))
           (cast l result t2 t4))]
        [(? procedure? p) (apply p rands)]
        [o (error 'interp "Unexpected value in apply-lazy match ~a" o)]))
    recur)
  
  (define (observe-lazy interp exp env)
    (with-handlers ([string? (lambda (e) `(blame ,e))])
      (match (interp exp env)
        [(? constant? k) k]
        [(? procedure? p) 'procedure]
        [(casted-value label v t (Function t2 t3)) 'procedure]
        [(casted-value l v t (Dyn)) 'dynamic]
        [o (error 'interp "Unexpected value in observe-lazy match ~a" o)])))
  
  (define (get-interp)
    (interp-expr apply-cast-ld (apply-lazy apply-cast-ld)))
  (define (get-observe)
    observe-lazy)
  
  ;; This is the body of the type-check
  (match prgm
    [(Prog _ _ _ e) ((get-observe) (get-interp) e (empty-env))]
    [otherwise (error 'interp "Not a program ~a" prgm)]))

  (define (unbound-var-error id) (error 'eval "Unbound variable ~a" id))

(module+ main
  (local-require rackunit rackunit/text-ui)

  (define conf (make-parameter (compiler-config #f #f #f #f)))

  (define-syntax test-interp
    (syntax-rules ()
      [(_ n p v) (test-equal? n (interp (Prog n p) (conf)) v)]))
  
  (run-tests
   (test-suite "interp"
     (parameterize ((conf (compiler-config 'lazy 'downcast 'none 'none)))
       (test-suite "lazy downcast"
         ;; tests for the literals
         (test-interp "lit int" (Const '_ 5) 5)
         (test-interp "lit int" (Const '_ #t) #t)
         (test-interp "cast lit" (Cast (Dyn) (Const '_ 10) (Int) "1")
                      'dynamic)
         (test-interp "cast lit" (Cast (Dyn) (Cast (Dyn) (Const '_ 10)
                                                   (Int) "1")
                                       (Dyn) "1")
                      'dynamic)
         (test-interp "cast lit" (Cast (Int) (Cast (Dyn) (Const '_ #f)
                                                   (Bool) "1")
                                       (Dyn) "2")
                      '(blame "2"))

         )))))

