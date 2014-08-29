#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/remove-anonymous-lambdas                                    |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: Moves every lambda into a letrec thus giving it a
| binding.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/compiler/language)
;; Only the pass is provided by this module
(provide label-lambdas)

(: label-lambdas (Lambda0-Lang Config . -> . Lambda1-Lang))
(define (label-lambdas prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (ll-expr exp count)])
      (Prog (list name count type) exp))))

(: ll-expr (-> L0-Expr Natural
	       (values L1-Expr Natural)))
(define (ll-expr exp next)
  (: recur* (-> (Listof L0-Expr) Natural (values (Listof L1-Expr) Natural)))
  (define (recur* e* n)
    (if (null? e*)
	(values '() n)
	(let ([a (car e*)]
	      [d (cdr e*)])
	  (let*-values ([(e* n) (recur* d n)]
			[(e n)  (ll-expr a n)])
	    (values (cons e e*) n)))))
  (match exp
    ;; This line should only be reached if the lambda
    ;; is not being bound by a let or a letrec
    [(Lambda f* ret-t (Castable ctr exp))
     (let*-values ([(name next) (next-uid "annon" next)]
		   [(exp next) (ll-expr exp next)])
       (values
	(Letrec (list (cons name (Lambda f* ret-t (Castable ctr exp))))
		(Var name))
	next))]
    ;; This is okay because of the absence of side effects are pure?
    [(Letrec b* e) (ll-let b* e next)]
    [(Let b* e) (ll-let b* e next)]
    [(If tst csq alt)
     (let*-values ([(tst next) (ll-expr tst next)]
		   [(csq next) (ll-expr csq next)]
		   [(alt next) (ll-expr alt next)])
       (values (If tst csq alt) next))]
    [(App exp exp*)
     (let*-values ([(exp next) (ll-expr exp next)]
		   [(exp* next) (recur* exp* next)])
       (values (App exp exp*) next))]
    [(Op p exp*)
     (let-values ([(exp* next) (recur* exp* next)])
       (values (Op p exp*) next))]
    [(Var i) (values (Var i) next)]
    [(Quote k) (values (Quote k) next)]))
  
;; ll-let takes the fields of from core and pulls all
;; bound procedures out into the let-proc form. Placing
;; the rest of the let as the body of the let-proc
(: ll-let (-> L0-Bnd* L0-Expr Natural (values L1-Expr Natural)))
(define (ll-let b* e n)
  ;; split-bound-procedures actually performs the filtering
  (: split-bnds (-> L0-Bnd* Natural 
		    (values L1-Bnd-Lambda* L1-Bnd-Data* Natural)))
  (define (split-bnds b* n)
    (for/fold ([bp* :  L1-Bnd-Lambda* '()] 
	       [bd* : L1-Bnd-Data* '()] 
	       [n   : Natural n])
	([b : L0-Bnd b*])
      (match b
	[(cons i (Lambda f* r (Castable b e)))
	 (let-values ([(e n) (ll-expr e n)])
	   (let ([bnd (inst cons Uid L1-Lambda)])
	     (values (cons (bnd i (Lambda f* r (Castable b e))) bp*) bd* n)))]
	[(cons i e)
	 (let-values ([(e n) (ll-expr e n)])
	   (let ([bnd (inst cons Uid L1-Expr)])
	     (values bp* (cons (bnd i e) bd*) n)))])))
  (let*-values ([(bp* bd* n) (split-bnds b* n)]
		[(e n) (ll-expr e n)])
    (: bp* L1-Bnd-Lambda*)
    (: bd* L1-Bnd-Data*)
    (: n Natural)
    ;; if both are null then we may remove an empty let
    ;; This transformation is only valid because bindings
    ;; being unique otherwise we might be changing
    ;; the scope on the rhs
    (if (null? bp*)
	 (if (null? bd*) 
	     (values e n)
	     (values (Let bd* e) n))
	 (let ([lr : L1-Expr (Letrec bp* e)])
	   (if (null? bd*)
	       (values lr n)
	       (values (Let bd* lr) n))))))
