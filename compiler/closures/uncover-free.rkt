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
(require Schml/framework/build-compiler
	 Schml/compiler/language
         Schml/framework/helpers
         Schml/framework/errors)

;; Only the pass is provided by this module
(provide uncover-free)

(: uncover-free (L1-Prog Config . -> . L2-Prog))
(define (uncover-free prgm comp-config)
  (match-let ([(L1-Prog n u e t) prgm])
    (let-values ([(e f*) (uf-expr e)])
      (if (set-empty? f*)
	  (L2-Prog n u e t)
	  (raise-pass-exn "closure/uncover-free"
			  "Free variables detect ~a" f*)))))

(: uf-expr (L1-Form . -> . (Values L2-Form (Setof Uvar))))
(define (uf-expr exp)
  (match exp
    [(Letrec b* e t) 
     (let-values ([(b-vars b* b-fvars) (uf-bnd* uf-lambda b*)]
		  [(e  e-fvars) (uf-expr e)])
       (values (Letrec b* e t) 
	       (set-subtract (set-union e-fvars b-fvars) b-vars)))]
    [(Let b* e t)
     (let-values ([(b-vars b* b-fvars) (uf-bnd* uf-expr b*)]
		  [(e  e-fvars) (uf-expr e)])
       (values (Let b* e t) 
	       (set-subtract (set-union e-fvars b-fvars) b-vars)))]
    [(If (app uf-expr t t-fvars)
	 (app uf-expr c c-fvars)
	 (app uf-expr a a-fvars) ty)
     (values (If t c a ty) (set-union t-fvars c-fvars a-fvars))]
    [(When/blame l (app uf-expr t t-fvars) (app uf-expr c c-fvars) ty)
     (values (When/blame l t c ty) (set-union t-fvars c-fvars))]
    [(App (app uf-expr e e-fvars) (app uf-expr* e* e*-fvars) ty)
     (values (App e e* ty) (set-union e-fvars e*-fvars))]
    [(Op p (app uf-expr* e* e*-fvars) ty) (values (Op p e* ty) e*-fvars)]
    [(Var u t) (values (Var u t) (set u))]
    [(Quote k t) (values (Quote k t) (set))]))

(: uf-expr* (-> (Listof L1-Form)
		(values (Listof L2-Form) (Setof Uvar))))
(define (uf-expr* e*)
  (if (null? e*)
      (values '() (set))
      (let-values ([(e e-fvars) (uf-expr (car e*))]
		   [(e* e*-fvars) (uf-expr* (cdr e*))])
	(values (cons e e*) (set-union e*-fvars e-fvars)))))

(: uf-lambda (L1-Lambda . -> . (values L2-Lambda (Setof Uvar)))) 
(define (uf-lambda lam)
  (: id-subtract (-> (Fml L1-Type) (Setof Uvar) (Setof Uvar)))
  (define (id-subtract f s)
    (set-remove s (Fml-identifier f)))
  (match-let ([(Lambda f* r (app uf-expr e fvars) t) lam])
    (let ([fvars (foldl id-subtract fvars f*)])
      (values (Lambda f* r (Free (set->list fvars) e) t) fvars))))

(: uf-bnd* (All (T U) 
	     (-> (-> T (values U (Setof Uvar))) 
		 (Listof (Bnd T L1-Type))
		 (values (Setof Uvar) 
			 (Listof (Bnd U L2-Type)) 
			 (Setof Uvar)))))
(define (uf-bnd* p b*)
  (if (null? b*)
      (values (set) '() (set))
      (match-let ([(Bnd u t (app p rhs rhs-f*)) (car b*)])
	(let-values ([(u* b* f*) (uf-bnd* p (cdr b*))])
	  (values (set-add u* u) 
		  (cons (Bnd u t rhs) b*) 
		  (set-union f* rhs-f*))))))

  
