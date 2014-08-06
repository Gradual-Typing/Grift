#lang racket
#|------------------------------------------------------------------------------+
|Pass: testing/ast-interps/lambda-forms-interp                                  |
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

(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/testing/values
	 Schml/compiler/language)

(provide lambda-forms-interp)


(struct dyn (v t))

(define-syntax-rule (env-lookup e u)
  (hash-ref e u (lambda () (error 'cfi "Unbound var ~a" u))))

(define-syntax-rule (env-extend* e u* v*)
  (for/fold ([env e]) ([u u*][v v*]) (hash-set env u v)))

(define-syntax-rule (empty-env) (hash))

(define (lambda-forms-interp prgm comp-config)
  (let ([observe observe-lazy])
    (observe interp-expr (Lambda-Prog-expression prgm) (hash))))

(define (interp-expr exp env)
  (define (recur/env e) (interp-expr e env))
  (match exp
    [(Lambda f* _  e _)
     (let ((id* (for/list ([f f*]) (Fml-identifier f))))
       (lambda (arg*) (interp-expr e (env-extend* env id* arg*))))]
    [(Letrec b* e _)
     (let ([env env])
       (let* ([id*  (for/list ([b b*]) (Bnd-identifier b))]
	      [val* (for/list ([b b*])
		      (match-let ([(Lambda f* _ e _) (Bnd-expression b)])
			(let ((id* (for/list ([f f*]) (Fml-identifier f))))
			  (lambda (arg*) (interp-expr e (env-extend* env id* arg*))))))])
	 (set! env (env-extend* env id* val*))
	 (interp-expr e env)))]
    [(Let b* e _)
     (let ([id* (for/list ([b b*]) (Bnd-identifier b))]
	   [val* (for/list ([b b*]) (recur/env (Bnd-expression b)))])
       (interp-expr e (env-extend* env id* val*)))]
    [(Var id _) (env-lookup env id)]
    [(Quote k _) k]
    [(If tst csq alt _)
     (if (recur/env tst) (recur/env csq) (recur/env alt))]
    [(App e e* _) ((recur/env e) (map recur/env e*))]
    [(Op p e* _) (delta p (map recur/env e*))]
    [(When/blame l t e _)
     (if (recur/env t)
	 (recur/env e)
	 (raise (exn:schml:type:dynamic 
		 (recur/env l)
		 (current-continuation-marks))))]
    [e (error 'interp "Umatched expression ~a" e)]))


(define (delta p v*)
  (case p
    [(+) (apply + v*)] [(-) (apply - v*)] [(*) (apply * v*)]
    [(=) (apply = v*)] [(<) (apply < v*)] [(>) (apply > v*)] 
    [(>=) (apply >= v*)] [(<=) (apply <= v*)]
    [(Dyn:Int) (dyn (car v*) 'Int)]   
    [(Dyn:Int?) (eq? (dyn-t (car v*)) 'Int)] 
    [(Dyn:Bool) (dyn (car v*) 'Bool)] 
    [(Dyn:Bool?)(eq? (dyn-t (car v*)) 'Bool)]
    [(Dyn:Int! Dyn:Bool! Dyn:Fn!) (dyn-v (car v*))]
    [(Dyn:Fn) (dyn (car v*) (cadr v*))]
    [(Dyn:FnT) (dyn-t (car v*))]
    [(Dyn:Fn?) (pair? (dyn-t (car v*)))]
    [(Type:Int?) (eq? (car v*) 'Int)]
    [(Type:Bool?) (eq? (car v*) 'Bool)]
    [(Type:Dyn?) (eq? (car v*) 'Dyn)]
    [(Type:Fn?) (pair? (car v*))]
    [(Type:Fn-arg) (car (car v*))]
    [(Type:Fn-ret) (cdr (car v*))]
    [(Type:Fn) (cons (car v*) (cadr v*))]
    [else (error 'delta "~a" p)]))


(define (observe-lazy interp exp env)
  (with-handlers ([exn:schml:type:dynamic? 
		   (lambda (e) (blame #f (exn-message e)))])
    (let ([v (interp exp env)])
     (cond 
      [(integer? v) (integ v)]
      [(boolean? v) (boole v)]
      [(procedure? v) (function)]
      [(and (dyn? v) (pair? (dyn-t v))) (function)]
      [(dyn? v) (dynamic)]
      [else (error 'observe)]))))
