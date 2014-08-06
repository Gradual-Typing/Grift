#lang typed/racket
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
         Schml/framework/errors
	 Schml/compiler/language)

;; Only the pass is provided by this module
(provide introduce-closure-primitives)

(define-type Env (HashTable Uvar L4-Expr))
(define-syntax (env-extend e k v) (hash-set e k v))
(define-syntax (env-lookup e k)
  (hash-ref 
   e k 
   (lambda () 
     (raise-pass-exn 
      'introduce-closure-primitives
      "Var ~a not bound in ~a" k e))))

(: introduce-closure-primitives 
   (Lambda3 Config . -> . Lambda4))
(define (introduce-closure-primitives prgm comp-config)
  (match-let ([(Prog (List n c t) e) prgm])
    (Prog (List n c t) ((icp-expr (env)) e))))

(: icp-expr (Env . -> . (L3-Expr . -> . L4-Expr)) 
(define (icp-expr env)
  (: recur* ((Listof L3-Expr) . -> . (Listof L4-Expr)))
  (define (recur* e*) (map recur e*))
  (: recur (L3-Expr . -> . L4-Expr))
  (define (recur exp)
    (match exp
      [(LetProc p* (Letclos c* e) t) (icp-let-proc p* c* e t env)]
      [(Let b* e t) 
       (let*-values ([(b* env) (icp-bnd-data b* env)])
	 (Let b* ((icp-expr env) e) t))]
      [(App (cons (app recur e)  (app recur e^)) (app recur* e*) t)
       (if (Label? e)
	   (App e (cons e^ e*) t)
	   (let ([e (Op Clos:code (List e) t)])
	     (App e (cons e^ e*) t)))]
      [(Op p (App recur* e*) t) (Op p e* t)]
      [(If (app recur t) (app recur c) (app recur a) ty)
       (If t c a ty)]
      [(When/blame l (app recur t) (app recur c) ty)
       (When/blame l t c ty)]
      [(Var i t) (env-lookup env i)]
      [(Quote k t) exp]))
  recur)

(define-type L3BndP (Bnd L3-Procedure L3-Type))
(define-type L3BndC (Bnd L3-Closure L3-Type))
(define-type L3BndD (Bnd L3-Expr L3-Type))

(: icp-let-proc (-> (Listof L3BndP) (Listof L3BndC) L3-Expr L3-Type Env 
		    L4-Expr))
(define (icp-let-proc p* c* e t env)
  (let*-values ([(p* env) (icp-bndp* p* env)]
		[(c* s* env) (icp-bndc* c* env)])
    (Letproc p* (Let c* (Begin s* ((icp-expr env) e) t) t) t)))

(: icp-bndp* ((Listof L3BndP) Env . -> . (values (Listof L4BndP) Env)))
(define (icp-bndp* b* env)
  ((Listof L3BndP) (Listof L4BndP) Env . -> . (values (Listof L4BndP) Env))
  (define (loop b3* b4* env)
    (if (null? b3*)
	(values b4* e)
	(match-let ([(Bnd l t (Procedure cp fml fr e t^)) (car b3*)])
	  (let ([p (icp-procedure cp fml fr e t^)])
	    (icp-bndp* (cdr b3*) 
		       (cons (Bnd l t p) b4*) 
		       (env-extend env l (Label l t)))))))
    (loop b* '() env))

(: icp-bndc* (-> (Listof L3BndC) Env 
		 (values (Listof L4BndC) (Listof L4-Expr) Env)))
(define (icp-bndc* b3* env)
  ;; Step 1 collect the fields into list and extend the environment
  (: collect-clos (-> (Listof L3BndC) (Listof Uvar) (Listof L4-Type)
		      (Listof L3-Closure) Env
		      (values (Listof L4BndC) (Listof L4-Expr) Env)))
  (define (collect-clos b* u* t* cd* env)
    (if (null? b*)
	(fold-clos u* t* cd* env) ;;goto step 2
	(match-let ([(Bnd u t cd) (car b*)])
	  (collect-clos (cdr b*) (cons u u*) (cons t t*) (cons cd cd*)
			(extend/env env u (var u t))))))
  
  ;; step 2 iterate the values creating allocation and setters
  (: fold-clos (-> (Listof Uvar) (Listof L3-Type) (Listof L3-Closure)
		   (Listof L4BndC) (Listof L4-Expr) Env
		   (values (Listof L4BndC) (Listof L4-Expr) Env)))
  (define (fold-clos uvar* type* clos-data* clos-exp* set* env)
    (cond            
     [(null? uvar*) (values closure-exp* set* env)];;done
     [(or (null? t*) (null? cd*));; This shouldn't be possible 
      (raise-pass-exn "icp-bndc*" "dependent types would be nice here")]
     [else
      (match-let ([(Closure-data label free) (car clos-data*)])
	(let-values ([(c* s*) (make-prims 0 (car u*) (car t*) (car cd*) c* s* env)])
	  (fold (cdr u*) (cdr t*) (cdr cd*) c* s*))])))  
  ;; step 2.5 create a single allocation and all the setters for it
  (: make-prims 
     (Natural Uvar L3-Type Uvar (Listof Uvar) (Listof L4BndC) (Listof L4-Expr)
	      . -> . (values (Listof L4BndC) (Listof L4-Expr))))
  (define (make-prims n u t l f* c* s* env)
    (if (null? f*)
	(let ([op   (Op 'Clos (List (Quote (add1 n))) t)]
	      [set  (Op 'Clos:code-set 
			(List (Var u t) (Label l t)) VOID)])
	  (values (cons (Bnd u t op) c) (cons set s*)))
	(let* ([var (Var u t)]
	       [index (Quote n INT-TYPE)]
	       [val (env-lookup env (car f*))])
	  (make-prims (add1 n) u t l (cdr f*) c*
		      (cons (Op 'Clos:set (List var index val) VOID) s*)))))
  (collect-clos bnd* '() '() '() env))

(: icp-procedure (-> Uvar (List (Fml L3-Type)) (List Uvar) L3-Expr L3-Type
		     L4-Code))
(define (icp-procedure cp fml* fr* body ty)
  ;; build-env has two parts first the refs are generated
  (: build-new-env (Uvar (List (Fml L3-Type)) (List Uvar) . -> . Env))
  (define (build-env env cp-var n fml* fr*)
    (if (null? u*)
	(build-step2 env fml*)
	(let* ([uvar (car fr*)]
	       [index (Quote n INT-TYPE)]
	       [op (Op 'Clos:ref (list cp-var index))])
	(build-env (env-extend env uvar op) (add1 n) fml* (cdr fr*)))))
   ;; as the contiuation build-step2 then generates the regular vars
  (: build-step2 (Env (Listof (Fml L3-Type)) . -> . Env))
  (define (build-step2 env fml*)
     (if (null? fml*)
	 env
	 (match-let ([(Fml i t) (car fml*)])
	   (build-step2 (extend-env env i (Var i t)) (cdr f*)))))
   ;; Procedures are now flat ish code that may be lifted
   (let* ([params (cons (Fml cp t^) fml*)]
	  [env    (build-env (Var cp ty) fml* fr*)]
	  [body   ((icp-expr env) e)])
     (code params body ty)))

(define (icp-bnd-data b* env)
  (let ([icp-expr (icp-expr env)]) 
    (define (loop b3* b4* env)
      (if (null? b3*)
	  (values b4* env)
	  (match-let ([(Bnd u t e) (car b3*)])
	    (let ([e (icp-expr e)])
	      (loop (cdr b3*) 
		    (cons (Bnd u t e) b4*)
		    (env-extend env u (Var u t)))))))
    (loop b* '() env)))

  

