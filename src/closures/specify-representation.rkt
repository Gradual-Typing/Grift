#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/specify-representation                                 |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)

;; Only the pass is provided by this module
(provide specify-representation)

(: specify-representation (-> Lambda3-Lang Config Lambda4-Lang))
(define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (Prog (list name count type) ((sr-expr (hash)) exp))))

(: sr-expr (-> Env (-> L3-Expr L4-Expr)))
(define (sr-expr env)
  (define-syntax-rule (clos-code-ref c) 
    (Op 'Clos:ref (list c (Quote 0))))
  (: recur (-> L3-Expr L4-Expr))
  (define (recur exp)
    (match exp
      [(LetP p* (LetC c* e)) (sr-let-proc p* c* e env)]
      [(Let bnd* exp) (Let (sr-bnd-data* bnd* env) 
			   ((sr-expr env) exp))]
      [(App (cons (app recur e) (app recur e^)) e*)
       (if (Code-Label? e)
	   (App e (cons e^ (sr-expr* e* env)))
	   (App (Op 'Array-ref (list e (Quote CLOS-CODE-INDEX)))
                (cons e^ (sr-expr* e* env))))]
      [(Op p e*) (Op p (sr-expr* e* env))]
      [(Fn-Caster (app recur e)) 
       (Op 'Array-ref (list e (Quote CLOS-CSTR-INDEX)))]
      [(If (app recur t) (app recur c) (app recur a)) (If t c a)]
      [(Begin stm* (app recur exp)) (Begin (sr-stmt* stm* env) exp)]
;;      [(Halt) (Halt)]
      [(Var i) (env-lookup env i)]
      [(Quote k) (Quote k)]))
  recur)

(: sr-expr* (-> (Listof L3-Expr) Env (Listof L4-Expr)))
(define (sr-expr* exp* env) (map (sr-expr env) exp*))

;; this is rediculous to maintain
;; TODO make the code loop through the closures and build sets and refs all at once


(: sr-let-proc (-> L3-Bnd-Procedure* L3-Bnd-Closure* L3-Expr Env L4-Expr))
(define (sr-let-proc bndp* bndc* exp env)
  (let*-values ([(bndp* env) (sr-bndp* bndp* env)]
		[(bndc* setc* env) (sr-bndc* bndc* env)])
    (Labels bndp* (Let bndc* (Begin setc* ((sr-expr env) exp))))))

(: sr-bndp* (-> L3-Bnd-Procedure* Env (values L4-Bnd-Code* Env)))
(define (sr-bndp* bnd* env)
  (: loop (-> L3-Bnd-Procedure* L4-Bnd-Code* Env (values L4-Bnd-Code* Env)))
  (define (loop b3* b4* env)
    (if (null? b3*)
	(values b4* env)
	(let* ([b3 (car b3*)] [b3* (cdr b3*)])
	  (match-let ([(cons u (Procedure cp param* ctr? fvar* exp)) b3])
	    (let ([b4 (cons u (sr-procedure cp param* ctr? fvar* exp))])
	      (loop b3* (cons b4 b4*) (env-extend env u (Code-Label u))))))))
  (loop bnd* '() env))


(: sr-bndc* (-> L3-Bnd-Closure* Env (values L4-Bnd-Data* L4-Stmt* Env)))
(define (sr-bndc* b3* env)
  (: collect-clos (-> L3-Bnd-Closure* Env Env))
  ;; since the environment is short circuts there may not be a need to
  ;; do this but it could be used as a means to identifify well known
  ;; functions
  (define (collect-clos b* env)
    (if (null? b*)
	env
	(let ((u (caar b*)))
	  (collect-clos (cdr b*) (env-extend env u (Var u))))))
  (: fold-clos (-> L3-Bnd-Closure* Env (values L4-Bnd-Data* L4-Stmt*)))
  (define (fold-clos bnd* env)
    (if (null? bnd*)
	(values '() '())
	(let*-values ([(bnd bnd*) (values (car bnd*) (cdr bnd*))]
		      [(bnd* set*)  (fold-clos bnd* env)])
	  (values (mk-bnd* bnd bnd*) (mk-set* bnd set* env)))))  
  (: mk-bnd* (-> L3-Bnd-Closure L4-Bnd-Data* L4-Bnd-Data*))
  (define (mk-bnd* b3 b4*)
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b3])
      (let* ([size (+ CLOS-FVAR-OFFSET (length free*))]
	     [bnd (cons uid (Op 'Alloc (list (Quote size))))])
	(cons bnd b4*))))
  (: mk-set* (-> L3-Bnd-Closure L4-Stmt* Env L4-Stmt*))
  (define (mk-set* b3 set* env)
    (: next-set (-> L4-Expr Index Uid L4-Stmt* Env (Values L4-Stmt* Index)))
    (define (next-set v i u set* env)
      (values (cons (Op 'Array-set! (list v (Quote i) (env-lookup env u))) set*)
	      (let ([i (add1 i)]) 
		(if (index? i) 
		    i 
		    (error 'sr-mk-set!* "closure too large")))))
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b3])
      (: loop (-> (Var Uid) Index Uid* L4-Stmt* L4-Stmt*))
      (define (loop cvar i free* set*)
	(if (null? free*)
	    set*
	    (let*-values ([(free free*) (values (car free*) (cdr free*))]
			  [(set* i) (next-set cvar i free set* env)])
	      (if (index? i)
		  (loop cvar i free* set*)
		  (error 'indroduce-closure-primitives "Closure is to big")))))
      (let* ([cvar (Var uid)]
             [setcode (Op 'Array-set! (list cvar (Quote CLOS-CODE-INDEX) (env-lookup env lbl)))]
             [cstr (if ctr? (env-lookup env ctr?) (Quote FALSE-IMDT))]
             [setcstr (Op 'Array-set! (list cvar (Quote CLOS-CSTR-INDEX) cstr))]
             [set* (cons setcode (cons setcstr set*))]);; I added this line 
	(loop cvar CLOS-FVAR-OFFSET free* set*)))) 
  (let*-values ([(env) (collect-clos b3* env)]
		[(bnd* exp*) (fold-clos b3* env)])
    (values bnd* exp* env)))

(: sr-procedure (-> Uid Uid* (Option Uid) Uid* L3-Expr L4-Code))
(define (sr-procedure cp param* ctr? free* body)
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
		 (env-extend env (car f*) (Op 'Array-ref (list cvar (Quote i))))))
	    (error 'specify-representation "Index rolled over"))))
    (loop free* CLOS-FVAR-OFFSET (hash)))
  (Code (cons cp param*) 
	((sr-expr (build-env (Var cp) ctr? free*)) body)))

(: sr-bnd-data* (-> L3-Bnd-Data* Env L4-Bnd-Data*))
(define (sr-bnd-data* b3* env)
  (let ([sr-expr (sr-expr env)]) 
    (: loop (-> L3-Bnd-Data* L4-Bnd-Data* L4-Bnd-Data*))
    (define (loop b3* b4*)
      (if (null? b3*)
	  b4*
	  (let* ([b3 (car b3*)]
		 [b3* (cdr b3*)]
		 [b4 (cons (car b3) (sr-expr (cdr b3)))])
	    (loop b3* (cons b4 b4*)))))
    (loop b3* '())))

(define-type Env (HashTable Uid L4-Expr))
(define-syntax-rule (env-extend e k v) 
  (hash-set e k v))

(: env-lookup (-> Env Uid L4-Expr))
(define (env-lookup e u)
  (hash-ref e u (lambda () (Var u))))

(: sr-stmt* (-> L3-Stmt* Env L4-Stmt*))
(define (sr-stmt* s* env)
  (map (lambda ([s : L3-Stmt]) (sr-stmt s env)) s*))

(: sr-stmt (-> L3-Stmt Env L4-Stmt))
(define (sr-stmt s env)
  (match s
    [(Op p e*) (Op p (sr-expr* e* env))]
    [otherwise (error 'sr-stmt "Unmatched statement ~a" s)]))
