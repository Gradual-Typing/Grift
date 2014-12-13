#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: testing/program-generator                                                |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: This program uses the first few passes of the compiler to generate
|a the entire latice for dynamically typed programs that are structurally equal.
+------------------------------------------------------------------------------|#
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)

;; Only the pass is provided by this module
(provide generate-programs)

(: insert-implicit-casts (Schml1-Lang Config . -> . Cast0-Lang))
(define (insert-implicit-casts prgm comp-config)
  (match-let ([(Prog (list name next-uid type) exp) prgm])
    (let ([vars ()]))
    (Prog (list name next-uid type) (iic-expr exp))))


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
       [(Quote lit) (Quote lit)])))

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
