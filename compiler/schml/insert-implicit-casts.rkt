#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/insert-implicit-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:
+-------------------------------------------------------------------------------+
|Input Grammar 
+------------------------------------------------------------------------------|#
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/compiler/language)

;; Only the pass is provided by this module
(provide insert-implicit-casts)

;; Some Type shorthands that I will use for just this pass
(define-type TT Typed-Type)
(define-type TT* (Listof TT))
(define-type TL Typed-Literal)
(define-type TP Typed-Prim)
(define-type TF Typed-Form)
(define-type ST Src.Type)
(define-type TFml (Fml TT))
(define-type TBnd (Bnd TF TT))
(define-type TP.TT (Pair TP TT*))
(define-type CF Cast-Form)
(define-type CT Cast-Type)
(define-type CL Cast-Literal)
(define-type CBnd (Bnd CF CT))
(define-type CFml (Fml CT))
(define-type CP Cast-Prim)

(: insert-implicit-casts 
   (Typed-Prog Config . -> . Cast-Prog))
(define (insert-implicit-casts prgm comp-config)
  (match-let ([(Typed-Prog n c e t) prgm])
    (Cast-Prog n c (iic-expr e) t)))

(: mk-cast ((-> Label) CF TT TT . -> . CF))
(define (mk-cast l-th e t1 t2)
  (if (equal? t1 t2) 
      e 
      (Cast e t1 t2 (l-th))))

(: iic-expr (TF . -> . CF))
(define (iic-expr [exp : TF])
  (match exp
    [(Lambda fmls ret-ty body ann)
     (iic-lambda fmls ret-ty body (cdr ann))]
    [(Let bnds body ann) 
     (Let (iic-bnds bnds) (iic-expr body) (cdr ann))]
    [(Letrec bnds body ann) 
     (Letrec (iic-bnds bnds) (iic-expr body) (cdr ann))]
    [(App rator rand* ann)
     (iic-application rator rand* (car ann) (cdr ann))]
    [(Op prim.ty rand* ann)
     (let* ([prim (car prim.ty)]
	    [rand-ty* (cdr prim.ty)]
	    [ret-ty (cdr ann)]
	    [src (car ann)])
       (Op prim (iic-operands/cast rand* rand-ty* src) ret-ty))]
    [(Ascribe exp ty lbl? ann)
     (let ([lbl (if lbl? 
		    (lambda () lbl?)
		    (mk-label "ascription" (car ann)))])
       (mk-cast lbl (iic-expr exp) ty (cdr ann)))]
    [(If tst csq alt ann) (iic-if tst csq alt (cdr ann))]
    [(Var id ann) (Var id (cdr ann))]
    [(Quote lit ann) (Quote lit (cdr ann))]
    ))

(: iic-lambda ((Listof TFml) TT TF TT . -> . CF))
(define (iic-lambda fmls ret-ty body lam-ty)
  (let* ([body-ann (get-annotation body)]
	 [body-src (car body-ann)]
	 [body-ty (cdr body-ann)] 
	 [body (iic-expr body)])
    (Lambda fmls ret-ty 
	    (mk-cast (mk-label "lambda" body-src) body body-ty ret-ty) 
	    lam-ty)))

(: iic-bnds ((Listof TBnd) . -> . (Listof CBnd)))
(define (iic-bnds bnd*)
  (for/list 
      ;; : (Listof CF-Bnd) 
      ([b : TBnd bnd*])
    (let* ([id (Bnd-identifier b)]
	   [bnd-ty (Bnd-type b)]
	   [rhs (Bnd-expression b)]
	   [rhs-ann (get-annotation rhs)]
	   [rhs-ty (cdr rhs-ann)]
	   [rhs-src (car rhs-ann)]
	   [rhs (iic-expr rhs)]
	   [lbl-th (mk-label "Binding" rhs-src)])
      (Bnd id bnd-ty (mk-cast lbl-th rhs rhs-ty bnd-ty)))))

(: iic-application (TF (Listof TF) Src TT . -> . CF))
(define (iic-application rator rand* src ty)
  (let* ([rator-ann (get-annotation rator)]
	 [rator-ty (cdr rator-ann)]
	 [rator-src (car rator-ann)])
    (cond
     [(Dyn? rator-ty)
      (let-values ([(rand* ty*) (iic-operands rand*)])
	(App (mk-cast (mk-label "Application" src rator-src)
		      (iic-expr rator)
		      rator-ty 
		      (Fn/a (length ty*) ty* ty))
	     rand* ty))]
     [(Fn/a? rator-ty)
      (App (iic-expr rator) 
	   (iic-operands/cast rand* (Fn/a-fmls rator-ty) src)
	   ty)]
     [else (raise-pass-exn "insert-implicit-casts" 
			   "error in iic-application's implimentation")])))

(: iic-operands 
   (-> (Listof TF) 
       (values (Listof CF) (Listof TT))))
(define (iic-operands rand*) 
  (for/lists ([cf* : (Listof CF)]
	      [ty* : (Listof TT)])
      ([rand : TF rand*])
    (let ([ann (get-annotation rand)])
      (values (iic-expr rand) (cdr ann)))))

(: iic-operands/cast 
   (-> (Listof TF) (Listof TT) Src
       (Listof CF)))
(define (iic-operands/cast rand* ty* src1)
  (for/list ([rand rand*] [ty2 ty*])
    (let* ([ann (get-annotation rand)]
	   [ty1 (cdr ann)]
	   [src2 (car ann)])
      (mk-cast (mk-label "application" src1 src2)
	       (iic-expr rand)
	       ty1 ty2))))



(: iic-if (TF TF TF TT . -> . CF))
(define (iic-if tst csq alt ty)
  (let* ([tst-ann (get-annotation tst)]
	 [csq-ann (get-annotation csq)]
	 [alt-ann (get-annotation alt)]
	 [tst-ty  (cdr tst-ann)]
	 [csq-ty  (cdr csq-ann)]
	 [alt-ty  (cdr alt-ann)]
	 [tst-src (car tst-ann)]
	 [csq-src (car csq-ann)]
	 [alt-src (car alt-ann)])
    (If (mk-cast (mk-label "if" tst-src) (iic-expr tst) tst-ty BOOL-TYPE) 
	(mk-cast (mk-label "if" csq-src) (iic-expr csq) csq-ty ty)
	(mk-cast (mk-label "if" alt-src) (iic-expr alt) alt-ty ty)
	ty)))


(define-syntax mk-label
  (syntax-rules ()
    [(_ pos src)
     (lambda ()
       (format "Implicit cast in ~a on expression at ~a"
	       pos (srcloc->string src)))]
    [(_ pos sup-src sub-src)
     (mk-label (format "~a at ~a" pos (srcloc->string sup-src))
	       sub-src)]))

(: get-annotation (TF . -> . ST))
(define (get-annotation x)
  (cond
   [(Lambda? x) (Lambda-annotation x)]
   [(Let? x) (Let-annotation x)]
   [(Letrec? x) (Letrec-annotation x)]
   [(App? x) (App-annotation x)]
   [(Op? x) (Op-annotation x)]
   [(Ascribe? x) (Ascribe-annotation x)]
   [(If? x) (If-annotation x)]
   [(Var? x) (Var-annotation x)]
   [else (Quote-annotation x)]))
