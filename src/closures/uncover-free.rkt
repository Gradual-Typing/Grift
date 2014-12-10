#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/uncover-free                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass labels all lambda with there free variables by adding
| the captures language form. 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/language
         schml/src/helpers
         schml/src/errors)

;; Only the pass is provided by this module
(provide uncover-free)

(: uncover-free (Lambda1-Lang Config . -> . Lambda2-Lang))
(define (uncover-free prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp free*) (uf-expr exp)])
      (if (set-empty? free*)
	  (Prog (list name count type) exp)
	  (raise-pass-exn 'uncover-free "Free variables detect ~a" free*)))))

(: uf-expr (-> L1-Expr (Values L2-Expr (Setof Uid))))
(define (uf-expr exp)
  (match exp
    [(Letrec b* e) 
     (let-values ([(b-vars b* b-fvars) (uf-bnd* uf-lambda b*)]
		  [(e  e-fvars) (uf-expr e)])
       (values (Letrec b* e) 
	       (set-subtract (set-union e-fvars b-fvars) b-vars)))]
    [(Let b* e)
     (let-values ([(b-vars b* b-fvars) (uf-bnd* uf-expr b*)]
		  [(e e-fvars) (uf-expr e)])
       (values (Let b* e) 
	       (set-subtract (set-union e-fvars b-fvars) b-vars)))]
    [(If (app uf-expr t t-fvars)
	 (app uf-expr c c-fvars)
	 (app uf-expr a a-fvars))
     (values (If t c a) (set-union t-fvars c-fvars a-fvars))]
    [(Begin (app uf-stmt* s* s*-fvars) 
            (app uf-expr e e-fvars))
     (values (Begin s* e) (set-union s*-fvars e-fvars))]
    [(App (app uf-expr e e-fvars) (app uf-expr* e* e*-fvars))
     (values (App e e*) (set-union e-fvars e*-fvars))]
    [(Op p (app uf-expr* e* e*-fvars)) (values (Op p e*) e*-fvars)]
    [(Fn-Caster (app uf-expr e e-fvars))
     (values (Fn-Caster e) e-fvars)]
    ;;[(Halt) (values (Halt) (set))]
    [(Var u) (values (Var u) (set u))]
    [(Quote k) (values (Quote k) (set))]))

(: uf-expr* (-> (Listof L1-Expr)
		(values (Listof L2-Expr) (Setof Uid))))
(define (uf-expr* e*)
  (if (null? e*)
      (values '() (set))
      (let ([a (car e*)]
	    [d (cdr e*)])
	(let-values ([(e* e*-fvars) (uf-expr* d)]
		     [(e e-fvars) (uf-expr a)])
	  (values (cons e e*) (set-union e*-fvars e-fvars))))))

(: uf-lambda (L1-Lambda . -> . (values L2-Lambda (Setof Uid)))) 
(define (uf-lambda lam)
  (: id-subtract (-> Uid (Setof Uid) (Setof Uid)))
  ;; id-subtract is set remove with the arguments in reverse
  (define (id-subtract f s) (set-remove s f))
  (match-let ([(Lambda f* r (Castable ctr? (app uf-expr e fvars))) lam])
    (let* ([fvars (foldl id-subtract fvars f*)]
	   [fvars (if ctr? (set-add fvars ctr?) fvars)])
      (values (Lambda f* r (Free ctr? (set->list fvars) e)) fvars))))

(: uf-bnd* (All (T U) 
	     (-> (-> T (values U (Setof Uid))) 
		 (Listof (Pairof Uid T))
		 (values (Setof Uid) ;; Newly Uids of this binding 
			 (Listof (Pairof Uid U)) ;; new binds
			 (Setof Uid))))) ;; all free vars
(define (uf-bnd* p b*)
  (if (null? b*)
      (values (set) '() (set))
      (let ([a (car b*)] [d (cdr b*)]) ;; free the list
	(let-values ([(u* b* f*) (uf-bnd* p d)])
	  (match-let ([(cons u (app p rhs rhs-f*)) a])
	    (values (set-add u* u) 
		    (cons (cons u rhs) b*) 
		    (set-union f* rhs-f*)))))))

(: uf-stmt* (-> L1-Stmt* (values L2-Stmt* (Setof Uid))))
(define (uf-stmt* s*)
  (if (null? s*)
      (values '() (set))
      (let-values ([(s s-fvars) (uf-stmt (car s*))]
                   [(s* s*-fvars) (uf-stmt* (cdr s*))])
        (values (cons s s*) (set-union s*-fvars s-fvars)))))

(: uf-stmt (-> L1-Stmt (values L2-Stmt (Setof Uid))))
(define (uf-stmt stm)
  (match stm
    [(Op p (app uf-expr* e* e*-fvars)) 
     (values (Op p e*) e*-fvars)]
    [otherwise (error 'uf-stmt "Unmatched statement ~a" stm)]))
