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
(require schml/src/helpers
         schml/src/errors
         schml/src/language)


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
  (match exp
    ;; This line should only be reached if the lambda
    ;; is not being bound by a let or a letrec
    [(Lambda f* (Castable ctr exp))
     (let*-values ([(name next) (next-uid "annon" next)]
		   [(exp next) (ll-expr exp next)])
       (values
	(Letrec (list (cons name (Lambda f* (Castable ctr exp))))
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
    [(Begin s* e)
     (let*-values ([(s* next) (ll-stmt* s* next)]
                   [(e next) (ll-expr e next)])
       (values (Begin s* e) next))]
    [(App exp exp*)
     (let*-values ([(exp next) (ll-expr exp next)]
		   [(exp* next) (ll-expr* exp* next)])
       (values (App exp exp*) next))]
    [(Op p exp*)
     (let-values ([(exp* next) (ll-expr* exp* next)])
       (values (Op p exp*) next))]
    [(Fn-Caster e)
     (let-values ([(e next) (ll-expr e next)])
       (values (Fn-Caster e) next))]
    ;;It may be possible to remove this: ak
    ;;[(Halt) (values (Halt) next)]
    [(Var i) (values (Var i) next)]
    [(Quote k) (values (Quote k) next)]))

(: ll-expr* (-> (Listof L0-Expr) Natural (values (Listof L1-Expr) Natural)))
  (define (ll-expr* e* n)
    (if (null? e*)
	(values '() n)
	(let ([a (car e*)]
	      [d (cdr e*)])
	  (let*-values ([(e* n) (ll-expr* d n)]
			[(e n)  (ll-expr a n)])
	    (values (cons e e*) n)))))

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
	[(cons i (Lambda f* (Castable b e)))
         (let-values ([(e n) (ll-expr e n)])
           (let (#;[bnd (inst cons Uid L1-Lambda)])
	     (values (cons (cons i (Lambda f* (Castable b e))) bp*) bd* n)))] 
	[(cons i e)
	 (let-values ([(e n) (ll-expr e n)])
           (let (#;[bnd (inst cons Uid L1-Expr)]) ;; nonsensical type error
             (values bp* (cons (cons i e) bd*) n)))])))
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

(: ll-stmt* (-> L0-Stmt* Natural (values L1-Stmt* Natural)))
(define (ll-stmt* st* next)
  (if (null? st*)
      (values '() next)
      (let*-values ([(st next)  (ll-stmt (car st*) next)]
                    [(st* next) (ll-stmt* (cdr st*) next)])
        (values (cons st st*) next))))

(: ll-stmt (-> L0-Stmt Natural (values L1-Stmt Natural)))
(define (ll-stmt st next)
  (match st
    [(Op p e*) 
     (let-values ([(e* next) (ll-expr* e* next)])
       (values (Op p e*) next))]
    [otherwise (error 'll-stmt "unmatched statement ~a" st)]))
