#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/convert-closures                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass sets up the structure of closures 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
         Schml/compiler/language)

(provide convert-closures)

(: convert-closures (L2-Prog Config . -> . Lambda3))
(define (convert-closures prgm conf)
  (match-let ([(L2-Prog n c e t) prgm])
    (let-values ([(e _ c) (cc-expr e c)])
      (Prog (list n c t) e))))

(: cc-expr (-> L2-Form Natural 
	       (values L3-Expr L3-Type Natural)))
(define (cc-expr exp next)
  (match exp
    [(Letrec b* e t)
     (let*-values ([(bp* bd* n) (cc-bnd-lambda* b* next)]
		   [(e _ n) (cc-expr e n)])
       (values (Letproc bp* (Letclos bd* e) t) t n))] 
    [(Let b* e t)
     (let*-values ([(b* n) (cc-bnd-data* b* next)]
		   [(e _ n) (cc-expr e n)])
       (values (Let b* e t) t n))]
    [(If t c a ty)
     (let*-values ([(t _ n) (cc-expr t next)]
		   [(c _ n) (cc-expr c n)]
		   [(a _ n) (cc-expr a n)])
       (values (If t c a ty) ty n))]
    [(When/blame l t c ty)
     (let*-values ([(t _ n) (cc-expr t next)]
		   [(c _ n) (cc-expr c n)])
       (values (When/blame l t c ty) ty n))]
    [(App e e* t)
     (let*-values ([(e e-ty n) (cc-expr e next)]
		   [(e* n) (cc-expr* e* n)])
       (if (Var? e)
           (values (App (cons e e) e* t) t n)
           (let* ([tmp-u (Uvar "tmp_clos" n)]
		  [tmp-v (Var tmp-u e-ty)])
             (values (Let (list (Bnd tmp-u e-ty e))
			  (App (cons tmp-v tmp-v) e* t)
			  t)
		     t 
		     (add1 n)))))]
      [(Op p e* t) 
       (let-values ([(e* n) (cc-expr* e* next)])
	 (values (Op p e* t) t n))]
      [(Var u t) (values (Var u t) t next)]
      [(Quote k t) (values (Quote k t) t next)]))

(: cc-expr* (-> (Listof L2-Form) Natural
		(values (Listof L3-Expr) Natural)))
(define (cc-expr* exp* n)
  (if (null? exp*)
      (values '() n)
      (let*-values ([(e* n) (cc-expr* (cdr exp*) n)]
		    [(e _ n) (cc-expr (car exp*) n)])
	(values (cons e e*) n))))

(define-type L2BndP (Bnd L2-Lambda L2-Type))
(define-type L3BndP (Bnd L3-Procedure L3-Type))
(define-type L3BndC (Bnd L3-Closure L3-Type))
(define-type L2BndD (Bnd L2-Form L2-Type))
(define-type L3BndD (Bnd L3-Expr L3-Type))


(: cc-bnd-lambda* (-> (Listof L2BndP) Natural
		      (values (Listof L3BndP) 
			      (Listof L3BndC) 
			      Natural)))
(define (cc-bnd-lambda* b* n)
  (if (null? b*)
      (values '() '() n)
      (let-values ([(p* c* n) (cc-bnd-lambda* (cdr b*) n)])
	(match-let ([(Bnd ext-cp-var t 
			  (Lambda fml* r (Free fv* e) t^)) (car b*)])
	  (let*-values ([(e _ n) (cc-expr e n)]
			[(int-cp-var n) (mk-clos-ptr-uvar ext-cp-var n)]
			[(code-var n) (mk-code-ptr-uvar ext-cp-var n)])
	    (let* ([proc (Procedure int-cp-var fml* fv* e t^)]
		   [bndp (Bnd code-var t proc)]
		   [clos (Closure-data code-var fv*)]
		   [bndc (Bnd ext-cp-var t clos)])
	      (values (cons bndp p*) (cons bndc c*) n)))))))

(: cc-bnd-data* (-> (Listof L2BndD) Natural
		    (values (Listof L3BndD) Natural)))

(define (cc-bnd-data* bnd* n)
  (if (null? bnd*)
      (values '() n)
      (let*-values ([(b* n) (cc-bnd-data* (cdr bnd*) n)])
	(match-let ([(Bnd u t e) (car bnd*)])
	  (let*-values ([(e _ n) (cc-expr e n)])
	    (values (cons (Bnd u t e) b*) n))))))

(: mk-uvar (Uvar String Natural . -> . (values Uvar Natural)))
(define (mk-uvar u s n)
  (values (Uvar (string-append (Uvar-prefix u) s) n)
	  (add1 n)))

(: mk-code-ptr-uvar (Uvar Natural . -> . (values Uvar Natural)))
(define (mk-code-ptr-uvar u n) (mk-uvar u "_code" n))
(: mk-clos-ptr-uvar (Uvar Natural . -> . (values Uvar Natural)))
(define (mk-clos-ptr-uvar u n) (mk-uvar u "_clos" n))


