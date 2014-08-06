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
(define-type LF Lambda-Form)
(define-type LT Lambda-Type)
(define-type LBnd (Bnd LF Lambda-Type))
(define-type L1F L1-Form)
(define-type Bnd-P (Bnd L1-Lambda L1-Type))
(define-type Bnd-D (Bnd L1-Form L1-Type))
(define-type Nat Natural)

(: label-lambdas (Lambda-Prog Config . -> . L1-Prog))
(define (label-lambdas prgm comp-config)
  (match-let ([(Lambda-Prog n c e t) prgm])
    (let-values ([(e c) (ll-expr e c)])
      (L1-Prog n c e t))))

(: ll-expr (LF Nat . -> . (values L1F Nat)))
(define (ll-expr exp next)
  (: recur* ((Listof LF) Nat . -> . (values (Listof L1F) Nat)))
  (define (recur* e* n)
    (if (null? e*)
	(values '() n)
	(let*-values ([(e* n) (recur* (cdr e*) n)]
		      [(e n) (ll-expr (car e*) n)])
	  (values (cons e e*) n))))
  (match exp
    ;; This line should only be reached if the lambda
    ;; is not being bound by a let or a letrec
    [(Lambda f* ret-t e t)
     (let*-values ([(name) (Uvar "annon" next)]
		   [(e n) (ll-expr e (add1 next))])
       (values
	(Letrec (list (Bnd name t (Lambda f* ret-t e t)))
		(Var name t) t)
	n))]
    ;; This is okay because of the absence of side effects are pure?
    [(Letrec b* e t) (ll-let b* e t next)]
    [(Let b* e t) (ll-let b* e t next)]
    [(If t c a ty)
     (let*-values ([(t n) (ll-expr t next)]
		   [(c n) (ll-expr c n)]
		   [(a n) (ll-expr a n)])
       (values (If t c a ty) n))]
    [(When/blame l t c ty)
     (let*-values ([(t n) (ll-expr t next)]
		   [(c n) (ll-expr c n)])
       (values (When/blame l t c ty) n))]
    [(App e e* t)
     (let*-values ([(e n) (ll-expr e next)]
		   [(e* n) (recur* e* n)])
       (values (App e e* t) n))]
    [(Op p e* t)
     (let-values ([(e* n) (recur* e* next)])
       (values (Op p e* t) n))]
    [(Var i t) (values (Var i t) next)]
    [(Quote k t) (values (Quote k t) next)]))
  
;; ll-let takes the fields of from core and pulls all
;; bound procedures out into the let-proc form. Placing
;; the rest of the let as the body of the let-proc
(: ll-let ((Listof LBnd) LF LT Nat . -> . (values L1F Nat)))
(define (ll-let b* e t n)
  ;; split-bound-procedures actually performs the filtering
  (: split-bnds ((Listof LBnd) Nat . -> .
		 (values (Listof Bnd-P) (Listof Bnd-D) Nat)))
  (define (split-bnds b* n)
    (for/fold ([bp* : (Listof Bnd-P) '()] 
	       [bd* : (Listof Bnd-D)'()] 
	       [n   : Nat n])
	([b : LBnd b*])
      (match b
	[(Bnd i t (Lambda f* r e a))
	 (let-values ([(e n) (ll-expr e n)])
	   (values (cons (Bnd i t (Lambda f* r e a)) bp*) bd* n))]
	[(Bnd i t e)
	 (let-values ([(e n) (ll-expr e n)])
	   (values bp* (cons (Bnd i t e) bd*) n))])))
  (let*-values ([(bp* bd* n) (split-bnds b* n)]
		[(e n) (ll-expr e n)])
    ;; if both are null then we may remove an empty let
    ;; This transformation is only valid because bindings
    ;; being unique otherwise we might be changing
    ;; the scope on the rhs
    (cond 
     [(null? bd*) (values (if (null? bp*) e (Letrec bp* e t)) n)]
     [(null? bp*) (values (Let bd* e t) n)] 
     [else (values (Let bd* (Letrec bp* e t) t) n)])))
