#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: testing/ast-interps/cast-forms-interp                                           |
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

(provide cast-forms-interp)


(define-type cfi-val (U cfi-bool cfi-int cfi-proc cfi-dyn))
(define-type cfi-bool boole)
(define-type cfi-int  integ)

(struct cfi-proc ([value : (-> (Listof cfi-val) cfi-val)]))
(struct cfi-dyn ([label : String]
		 [value : cfi-val]
		 [type1 : Cast-Type]
		 [type2 : Cast-Type]))

(define-type Env (HashTable Uvar cfi-val))

(define-syntax-rule (env-lookup e u)
  (hash-ref e u (lambda () (error 'cfi "Unbound var ~a" u))))

(define-syntax-rule (env-extend* e u* v*)
  (for/fold ([env : Env e])
      ([u u*][v v*])
    (hash-set env u v)))

(define-syntax-rule (empty-env)
  (hash))

(: mk-cast cast-fn-type)
(define (mk-cast l e t g)
  (if (equal? t g) 
      e 
      (cfi-dyn l e t g)))

(: cast-forms-interp (Cast-Prog Config . -> . Value))
(define (cast-forms-interp prgm comp-config)
  (let ([eval (interp-expr apply-cast-ld (apply-lazy apply-cast-ld))] 
	[observe observe-lazy])
    (observe eval (Cast-Prog-expression prgm) (empty-env))))

(define-type interp-ctr-type 
  (cast-fn-type apply-fn-type . -> . interp-fn-type)) 
(define-type interp-fn-type
  (Cast-Form Env . -> . cfi-val))
(define-type cast-fn-type
  (String cfi-val Cast-Type Cast-Type . -> . cfi-val))
(define-type apply-fn-type
  (cfi-val (Listof cfi-val) . -> . cfi-val))
(define-type apply-ctr-type 
  (cast-fn-type . -> . apply-fn-type))
(define-type observe-fn-type
  (interp-fn-type Cast-Form Env . -> . Value))


(: interp-expr interp-ctr-type)
(define (interp-expr cast apply)
  (: recur interp-fn-type)
  (define (recur exp env)
    (: recur/env (Cast-Form . -> . cfi-val))
    (define (recur/env e) (recur e env))
    (match exp
      [(Lambda fmls _  body _)
       (let ((id* (for/list : (Listof Uvar) ([f fmls]) 
			    (Fml-identifier f))))
	 (cfi-proc (lambda (arg*)
		     (recur body (env-extend* env id* arg*)))))]
       [(Let bnds body _)
       (let ([id* (for/list : (Listof Uvar) ([b bnds]) (Bnd-identifier b))]
	     [rhs* (for/list : (Listof cfi-val) ([b bnds]) (recur/env (Bnd-expression b)))])
	 (recur body (env-extend* env id* rhs*)))]
       [(Var id _) (env-lookup env id)]
       [(Quote k _) (if (boolean? k) (boole k) (integ k))]
       [(If (app recur/env tst) csq alt _)
	(if (equal? tst (boole #t))
	    (recur/env csq)
	    (recur/env alt))]
       [(App e e* _) (apply (recur/env e) (map recur/env e*))]
       [(Op p e* _) (delta p (map recur/env e*))]
       [(Cast (app recur/env val) t-exp t-cast label)
	(cast label val t-exp t-cast)]
       [e (error 'interp "Umatched expression ~a" e)]))
  recur)

(: delta (Cast-Prim (Listof cfi-val) . -> . cfi-val))
(define (delta p v*)
  (define-syntax app
    (syntax-rules (IxI->I IxI->B)
      [(_ IxI->I p) (! p integ? integ-value integ (car v*) (cadr v*))]
      [(_ IxI->B p) (! p integ? integ-value boole (car v*) (cadr v*))]))
  (define-syntax !
    (syntax-rules ()
      [(_ p p? from ret (e ...) (e^ ...))
       (let ((tmp (e ...))
	     (tmp^ (e^ ...)))
	 (if (and (p? tmp) (p? tmp^))
	     (ret (p (from tmp) (from tmp^))) 
	     (error 'cfi-delta "type error ~a" 'p)))]))
  (case p
    [(+) (app IxI->I +)]
    [(-) (app IxI->I -)]
    [(*) (app IxI->I *)]
    [(=) (app IxI->B =)]
    [(<) (app IxI->B <)]
    [(>) (app IxI->B >)]
    [(>=) (app IxI->B >=)]
    [(<=) (app IxI->B <=)]
    [else (error 'delta "~a" p)]))

  ;; The lazy-d parts
(: apply-cast-ld cast-fn-type)
(define (apply-cast-ld l1 v1 t1 t2)
  (if (shallow-consistent? t1 t2)
      (if (Dyn? t1)
	  (match v1
	    [(cfi-dyn l2 v2 t3 t1)
	     (apply-cast-ld l1 v2 t3 t2)]
	    [o (error 'interp "Unexpected value in apply-cast-ld match ~a" o)])
	  (mk-cast l1 v1 t1 t2))
      (raise-dynamic-type-error l1)))

(: apply-lazy apply-ctr-type)
(define (apply-lazy cast)
  (: cast/lbl (-> String 
		  (-> cfi-val Cast-Type Cast-Type 
		      cfi-val)))
  (define (cast/lbl l) (lambda (v t1 t2) (cast l v t1 t2)))
  (: recur apply-fn-type)
  (define (recur rator rands)
    (match rator
     [(cfi-dyn lbl val (Fn/a ar1 t1* t2) (Fn/a ar2 t3* t4))
      (let* ((rands^ (if (= ar1 ar2)
			 (map (cast/lbl lbl) rands t3* t1*)
			 (raise-dynamic-type-error lbl)))
	     (result (recur val rands^)))
	(cast lbl result t2 t4))]
     [(cfi-proc proc) (proc rands)]
     [else (error 'interp "Unexpected value in apply-lazy match ~a" rator)]))
  recur)

(: observe-lazy observe-fn-type)
(define (observe-lazy interp exp env)
  (with-handlers ([exn:schml:type:dynamic? 
		   (lambda (e) (blame #f (exn-message e)))])
    (let ([v (interp exp env)])
     (cond 
      [(or (integ? v) (boole? v)) v]
      [(cfi-proc? v) (function)]
      [else (if (Fn/a? cfi-dyn-type2) (function) (dynamic))]))))


