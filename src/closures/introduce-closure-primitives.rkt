#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/closures/introduce-closure-primitives                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/src/helpers
         Schml/src/errors
	 Schml/src/language)

;; Only the pass is provided by this module
(provide introduce-closure-primitives)

(: introduce-closure-primitives (-> Lambda3-Lang Config Lambda4-Lang))
(define (introduce-closure-primitives prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (Prog (list name count type) ((icp-expr (hash)) exp))))

(: icp-expr (-> Env (-> L3-Expr L4-Expr)))
(define (icp-expr env)
  (define-syntax-rule (clos-code-ref c) 
    (Op 'Clos:ref (list c (Quote 0))))
  (: recur* (-> (Listof L3-Expr) (Listof L4-Expr)))
  (define (recur* exp*) (map recur exp*))
  (: recur (-> L3-Expr L4-Expr))
  (define (recur exp)
    (match exp
      [(LetP p* (LetC c* e)) (icp-let-proc p* c* e env)]
      [(Let bnd* exp) (Let (icp-bnd-data* bnd* env) 
			   ((icp-expr env) exp))]
      [(App (cons (app recur e) (app recur e^)) (app recur* e*))
       (if (Code-Label? e)
	   (App e (cons e^ e*))
	   (App (clos-code-ref e) (cons e^ e*)))]
      [(Op p (app recur* e*)) 
       ;; Fn casts are just accessing the caster which is always the
       ;; second field of closures
       (if (eq? p 'Fn-cast)
	   (match-let ([(list e) e*])
	     (Op 'Clos:ref (list e (Quote 1))))
	   (Op p e*))]
      [(If (app recur t) (app recur c) (app recur a)) (If t c a)]
      [(Var i) (env-lookup env i)]
      [(Quote k) (Quote k)]))
  recur)

(: icp-let-proc (-> L3-Bnd-Procedure* L3-Bnd-Closure* L3-Expr Env L4-Expr))
(define (icp-let-proc bndp* bndc* exp env)
  (let*-values ([(bndp* env) (icp-bndp* bndp* env)]
		[(bndc* setc* env) (icp-bndc* bndc* env)])
    (Labels bndp* (Let bndc* (Begin setc* ((icp-expr env) exp))))))

(: icp-bndp* (-> L3-Bnd-Procedure* Env (values L4-Bnd-Code* Env)))
(define (icp-bndp* bnd* env)
  (: loop (-> L3-Bnd-Procedure* L4-Bnd-Code* Env (values L4-Bnd-Code* Env)))
  (define (loop b3* b4* env)
    (if (null? b3*)
	(values b4* env)
	(let* ([b3 (car b3*)] [b3* (cdr b3*)])
	  (match-let ([(cons u (Procedure cp param* ctr? fvar* exp)) b3])
	    (let ([b4 (cons u (icp-procedure cp param* ctr? fvar* exp))])
	      (loop b3* (cons b4 b4*) (env-extend env u (Code-Label u))))))))
  (loop bnd* '() env))


(: icp-bndc* (-> L3-Bnd-Closure* Env (values L4-Bnd-Data* L4-Effect* Env)))
(define (icp-bndc* b3* env)
  (: collect-clos (-> L3-Bnd-Closure* Env Env))
  ;; since the environment is short circuts there may not be a need to
  ;; do this but it could be used as a means to identifify well known
  ;; functions
  (define (collect-clos b* env)
    (if (null? b*)
	env
	(let ((u (caar b*)))
	  (collect-clos (cdr b*) (env-extend env u (Var u))))))
  (: fold-clos (-> L3-Bnd-Closure* Env (values L4-Bnd-Data* L4-Effect*)))
  (define (fold-clos bnd* env)
    (if (null? bnd*)
	(values '() '())
	(let*-values ([(bnd bnd*) (values (car bnd*) (cdr bnd*))]
		      [(bnd* set*)  (fold-clos bnd* env)])
	  (values (mk-bnd* bnd bnd*) (mk-set* bnd set* env)))))  
  (: mk-bnd* (-> L3-Bnd-Closure L4-Bnd-Data* L4-Bnd-Data*))
  (define (mk-bnd* b3 b4*)
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b3])
      (let* ([size/lbl 1]
	     [size/ctr (if ctr? (add1 size/lbl) size/lbl)]
	     [size (+ size/ctr (length free*))]
	     [bnd (cons uid (Op 'Clos:make (list (Quote size))))])
	(cons bnd b4*))))
  (: mk-set* (-> L3-Bnd-Closure L4-Effect* Env L4-Effect*))
  (define (mk-set* b3 set* env)
    (: next-set (-> L4-Expr Index Uid L4-Effect* Env (Values L4-Effect* Index)))
    (define (next-set v i u set* env)
      (values (cons (Op 'Clos:set! (list v (Quote i) (env-lookup env u))) set*)
	      (let ([i (add1 i)]) 
		(if (index? i) 
		    i 
		    (error 'mk-set!* "closure too large")))))
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b3])
      (: loop (-> (Var Uid) Index Uid* L4-Effect* L4-Effect*))
      (define (loop cvar i free* set*)
	(if (null? free*)
	    set*
	    (let*-values ([(free free*) (values (car free*) (cdr free*))]
			  [(set* i) (next-set cvar i free set* env)])
	      (if (index? i)
		  (loop cvar i free* set*)
		  (error 'indroduce-closure-primitives "Closure is to big")))))
      (let*-values ([(cvar) (Var uid)]
		    [(set* i) (next-set cvar 0 lbl set* env)]
		    [(set* i)  (if ctr? 
				   (next-set cvar i ctr? set* env)
				   (values set* i))])
	(loop cvar i free* set*)))) 
  (let*-values ([(env) (collect-clos b3* env)]
		[(bnd* exp*) (fold-clos b3* env)])
    (values bnd* exp* env)))

(: icp-procedure (-> Uid Uid* (Option Uid) Uid* L3-Expr L4-Code))
(define (icp-procedure cp param* ctr? free* body)
  ;; build-env has two parts first the refs are generated
  (: build-env ((Var Uid) (Option Uid) (Listof Uid) . -> . Env))
  (define (build-env cvar ctr? free*)
    (: loop (-> Uid* Index Env Env))
    (define (loop f* i env)
      (let ([i^ (add1 i)]) 
	(if (index? i^)
	    (if (null? f*)
		env
		(loop 
		 (cdr f*)  
		 i^
		 (env-extend env (car f*) (Op 'Clos:ref (list cvar (Quote i))))))
	    (error 'introduce-closure-primitives "Index rolled over"))))
    (loop free* (if ctr? 2 1) (hash)))
  (Code (cons cp param*) 
	((icp-expr (build-env (Var cp) ctr? free*)) body)))

(: icp-bnd-data* (-> L3-Bnd-Data* Env L4-Bnd-Data*))
(define (icp-bnd-data* b3* env)
  (let ([icp-expr (icp-expr env)]) 
    (: loop (-> L3-Bnd-Data* L4-Bnd-Data* L4-Bnd-Data*))
    (define (loop b3* b4*)
      (if (null? b3*)
	  b4*
	  (let* ([b3 (car b3*)]
		 [b3* (cdr b3*)]
		 [b4 (cons (car b3) (icp-expr (cdr b3)))])
	    (loop b3* (cons b4 b4*)))))
    (loop b3* '())))

(define-type Env (HashTable Uid L4-Expr))
(define-syntax-rule (env-extend e k v) 
  (hash-set e k v))

(: env-lookup (-> Env Uid L4-Expr))
(define (env-lookup e u)
  (hash-ref e u (lambda () (Var u))))
