#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/insert-implicit-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: This pass converts differences in type equality via a inserted
|cast. After this pass all types should be explicit and the program should be 
|roughly equivalent to the cast-calculus mentioned in papers on the gradually
|typed lambda calculus.
+-------------------------------------------------------------------------------+
|Input Grammar 
+------------------------------------------------------------------------------|#
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)

;; Only the pass is provided by this module
(provide insert-implicit-casts)

(: insert-implicit-casts (Schml1-Lang Config . -> . Cast0-Lang))
(define (insert-implicit-casts prgm comp-config)
  (match-let ([(Prog (list name next-uid type) exp) prgm])
    (Prog (list name next-uid type) (iic-expr exp))))

(: mk-cast ((-> String) C0-Expr Schml-Type Schml-Type . -> . C0-Expr))
(define (mk-cast l-th e t1 t2)
  (if (equal? t1 t2) e (Cast e t1 t2 (l-th));(Ann  t2)
      ))

(: iic-expr (S1-Expr . -> . C0-Expr))
(define (iic-expr exp^)
  (match-let ([(Ann exp (cons src type)) exp^])
    (match exp
       [(Lambda fml* ret-type (and (Ann _ (cons body-src body-type)) 
				 (app iic-expr body)))
	(let* ([lbl-th (mk-label "lambda" body-src)]
	       [body (mk-cast lbl-th body body-type ret-type)])
	  ;(Ann  type)
	  (Lambda fml* ret-type body))]
       [(Let bnd* (and (Ann _ (cons src type^)) body))
	(Let (map iic-bnd bnd*) (mk-cast (mk-label "let" src) (iic-expr body) type^ type))]
       [(Letrec bnd* (and (Ann _ (cons src type^)) body))
	(Letrec (map iic-bnd bnd*) (mk-cast (mk-label "letrec" src) (iic-expr body) type^ type))]
       [(App rator rand*) (iic-application rator rand* src type)]
       [(Op (Ann prim rand-ty*) rand*)
	;;(Ann  type)
	(Op prim (map (iic-operand/cast src) rand* rand-ty*))]
       [(Ascribe exp t1 lbl?)
	(let ([lbl (if lbl? (th lbl?) (mk-label "ascription" src))])
	  (mk-cast lbl (iic-expr exp) t1 type))]
       [(If (and (Ann _ (cons tst-src tst-ty)) (app iic-expr tst))
	    (and (Ann _ (cons csq-src csq-ty)) (app iic-expr csq))
	    (and (Ann _ (cons alt-src alt-ty)) (app iic-expr alt))) 
	(If (mk-cast (mk-label "if" tst-src) tst tst-ty BOOL-TYPE) 
	    (mk-cast (mk-label "if" csq-src) csq csq-ty type)
	    (mk-cast (mk-label "if" alt-src) alt alt-ty type))]
       [(Var id) (Var id)]
       [(Quote lit) (Quote lit)]
       [(Begin e* e)      (TODO define insert implicit casts)]
       [(Gbox e1)         (TODO define box insert implicit casts)]
       [(Gunbox e1)       (TODO define box insert implicit casts)]
       [(Gbox-set! e1 e2) (TODO define box insert implicit casts)]
       [(Mbox e1)         (TODO define box insert implicit casts)]
       [(Munbox e1)       (TODO define box insert implicit casts)]
       [(Mbox-set! e1 e2)     (TODO define box insert implicit casts)]
       [(Gvector e1 e2)         (TODO define vector insert implicit casts)]
       [(Gvector-ref e1 e2)     (TODO define vector insert implicit casts)]
       [(Gvector-set! e1 e2 e3) (TODO define vector insert implicit casts)]
       [(Mvector e1 e2)         (TODO define vector insert implicit casts)]
       [(Mvector-ref e1 e2)     (TODO define vector insert implicit casts)]
       [(Mvector-set! e1 e2 e3) (TODO define vector insert implicit casts)])))

(: iic-bnd (S1-Bnd . -> . C0-Bnd))
  (define (iic-bnd b)
    (match-let ([(Bnd i t (and rhs (Ann _ (cons rhs-src rhs-type)))) b])
      (Bnd i t (mk-cast (mk-label "binding" rhs-src) (iic-expr rhs) rhs-type t))))

(: iic-application (S1-Expr (Listof S1-Expr) Src Schml-Type . -> . C0-Expr))
(define (iic-application rator rand* src type)
  (match-let ([(Ann _ (cons rator-src rator-type)) rator])
    (cond
     [(Dyn? rator-type)
      (let*-values ([(rand* rand-type*) (iic-operands rand*)]
		    [(lbl-th) (mk-label "Application" src rator-src)]
		    [(needed-type) (Fn (length rand-type*) rand-type* type)])
	;(Ann type)
	(App (mk-cast lbl-th (iic-expr rator) rator-type needed-type) rand*))]
     [(Fn? rator-type)
      ;(Ann type)
      (App (iic-expr rator) 
		(map (iic-operand/cast src) rand* (Fn-fmls rator-type)))]
     [else (raise-pass-exn "insert-implicit-casts" 
			   "error in iic-application's implimentation")])))

(: iic-operands 
   (-> (Listof S1-Expr) 
       (values (Listof C0-Expr) (Listof Schml-Type))))
(define (iic-operands rand*) 
  (for/lists ([cf* : (Listof C0-Expr)]
	      [ty* : Schml-Type*])
      ([rand : S1-Expr rand*])
    (match-let ([(Ann _ (cons _ type)) rand])
      (values (iic-expr rand) type))))

(: iic-operand/cast (-> Src (-> S1-Expr Schml-Type C0-Expr)))
(define (iic-operand/cast app-src)
  (lambda (rand arg-type)
    (match-let ([(Ann _ (cons rand-src rand-type)) rand])
      (mk-cast (mk-label "application" app-src rand-src)
	       (iic-expr rand)
	       rand-type arg-type))))

(define-syntax-rule (th o ...)
  (lambda () o ...))

(define-syntax mk-label
  (syntax-rules ()
    [(_ pos src)
     (lambda ()
       (format "Implicit cast in ~a on expression at ~a"
	       pos (srcloc->string src)))]
    [(_ pos sup-src sub-src)
     (mk-label (format "~a at ~a" pos (srcloc->string sup-src))
	       sub-src)]))
