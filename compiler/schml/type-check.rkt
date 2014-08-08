#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/schml/typecheck                                                  |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: This is a first attempt at typechecking for the GTLC core         |
|language.  It is currently limited to just the grammar noted below but with the|
|with the addition of a unification algorithm is could be generalize to mutually|
|recursive functions. There is a major way in which this algorithm is different |
|from that of the STLC. The difference is that instead of checking for type     |
|equality the algorithm is checking for type consitency.                        |
|                                                                               |
|Essence of the consistency relationship:                                       |
|Two type are consistent any of the following relationships are true.           |
|   1: One of them is dyn.                                                      |
|   2: They are structurally equal.                                             |
|   3: They both share the same super-structure                                 |
|      and their sub-structures are consistent.                                 |
|                                                                               |
+-------------------------------------------------------------------------------+
|Grammar for Core-Prog found in Schml/languages/core-forms                      |
|Core-Prog = (Core-Prog {File-Name} {Next-Uvar-Suffix} {Core-Expr})             |
|Core-Expr = (Lambda {Formals}* {Type}? {Core-Expr} {Src})                      |
|          | (Var {Uvar} {Src})                                                 |
|          | (App {Expr} {Expr}* {Src})                                         |
|          | (Op {Prim} {Expr}* {Src})                                          |
|          | (Ascribe {Expr} {Type} {Blame?} {Src})                              |
|          | (If {Expr} {Expr} {Expr} {Src})                                    |
|          | (Let {Binding} {Expr} {Src})                                       |
|          | (Quote {Datum} {Src})                                              |
|Binding   = (Bnd {Uvar} {Type?} {Expr})                         |
|Formal    = (Fmlt {Uvar} {Type})                                           |
|Src       = Racket's native srcloc type                                        |
|Blame?    = String | #f                                                        |
|Type?     = {Type} | #f                                                        |
|Type      = Int | Bool | (Fn {Type}* {Type}*)                                    |
|Uvar      = (Uvar String Natural)                                              |
|Datum     = Integer |  Boolean                                                 |
|Prim      = * | + | - | % | % | >> | << | < | <= | = | > | >=                  |
|                                                                               |
+-------------------------------------------------------------------------------+
|Grammar for Typed-Prog found in Schml/languages/typed-forms                    |
|Typed-Prog = (Typed-Prog {File-Name} {Next-Uvar-Suffix} {Core-Expr} {Core-Type})
|Typed-Expr = (Lambda {Formals}* {Type} {Typed-Expr} {Src} {Core-Type})         |
|           | (Var {Uvar} {Src} {Core-Type})                                    |
|           | (App {Expr} {Expr}* {Src} {Core-Type})                            |
|           | (Op {Prim} {Expr}* {Src})                                         |
|           | (Ascribe {Expr} {Type} {Blame?} {Src} {Core-Type})                |
|           | (If {Expr} {Expr} {Expr} {Src} {Core-Type})                       |
|           | (Let {Binding} {Expr} {Src} {Core-Type})                          |
|           | (Quote {Literal} {Src} {Core-Type})                               |
|Binding   = (Bnd {Uvar} {Core-Type} {Expr})                                    |
|Formal    = (Fml {Uvar} {Type})                                                |
|Src       = Racket's native srcloc type                                        |
|Label?    = String | #f                                                        |
|Type      = Int | Bool | (Fn {Type}* {Type})                                   |
|Uvar      = (Uvar String Natural)                                              |
|Literal   = Integer |  Boolean                                                 |
|Prim      = * | + | - | % | % | >> | << | < | <= | = | > | >=                  |
+------------------------------------------------------------------------------|#
(require Schml/framework/build-compiler
         Schml/framework/errors
	 Schml/framework/helpers
	 Schml/compiler/language)

;; Only the pass is provided by this module
(provide type-check)
#| Some type shorthand to keep types reasonable |#
(define-type TT Typed-Type)
(define-type TT* (Listof TT))
(define-type TL Typed-Literal)
(define-type TP Typed-Prim)
(define-type TF Typed-Form)
(define-type ST Src.Type)
(define-type TFml (Fml TT))
(define-type TBnd (Bnd TF TT))
(define-type TP.TT (Pair TP TT*))
(define-type CF Core-Form)
(define-type CT Core-Type)
(define-type CL Core-Literal)
(define-type CBnd (Bnd CF (Maybe CT)))
(define-type CFml (Fml Core-Type))
(define-type CP Core-Prim)

(define-type Env (HashTable Uvar TT))
(define-syntax-rule (env-extend* e x* v*)
  (for/fold ([t : Env e]) ([x x*] [v v*])
    (hash-set t x v)))



(: type-check (Core-Prog Config . -> . Typed-Prog)) 
(define (type-check prgm comp-config)
  (match-let ([(Core-Prog n c e) prgm])
    (let-values (((e t) (tyck-expr e (hasheq))))
      (Typed-Prog n c e t))))

(: map-Fml ((Listof Uvar) (Listof TT) . -> . (Listof TFml)))
(define (map-Fml id* ty*)
  (for/list : (Listof TFml) 
	    ([id id*] [ty ty*])
	    (Fml id ty)))

(: map-Bnd ((Listof Uvar) (Listof TT) (Listof TF)
	    . -> . 
	    (Listof (Bnd TF TT))))
(define (map-Bnd id* ty* rhs*)
  (for/list : (Listof (Bnd TF TT))
	    ([id id*] [ty ty*] [rhs rhs*])
	    (Bnd id ty rhs)))

;;; Procedures that are used to throw errors
;; The error that occurs when a variable is not found. It is an internal
;; error because it is a syntax error to have an unbound variable.
(define-syntax-rule (lookup-failed src id)
  (lambda () (raise-variable-not-found src id)))

;;; The type rules for core forms that have interesting type rules
;; The type of a lambda that is annotated is the type of the annotation
;; as long as the annotation is consistent with the type of the
;; body
(: lambda-type-rule (Src (Listof TT) TT (Maybe TT) . -> . (Fn/a (Listof TT) TT)))
(define (lambda-type-rule src ty* t-body t-ann)
  (cond
   [(not t-ann) (Fn/a (length ty*) ty* t-body)]
   [(consistent? t-body t-ann) (Fn/a (length ty*) ty* t-ann)]
   [else  (raise-lambda-inconsistent src t-body t-ann)]))

;; The type of a annotated let binding is the type of the annotation
;; as long as it is consistent with the type of the expression.
(: let-binding-type-rule ((Maybe TT) TT Uvar Src . -> . TT))
(define (let-binding-type-rule t-bnd t-exp id src)
  (cond
   [(not t-bnd) t-exp]
   [(consistent? t-bnd t-exp) t-bnd]
   [else (raise-binding-inconsistent src id t-bnd t-exp)]))

;; The type of a cast is the cast-type if the expression type and
;; the cast type are consistent.
(: ascription-type-rule (TT TT Src (Maybe Label) . -> . TT))
(define (ascription-type-rule ty-exp ty-cast src label)
  (if (not (consistent? ty-exp ty-cast))
      (if label
	  (raise-blame-label label)
	  (raise-ascription-inconsistent src ty-exp ty-cast))
      ty-cast))

;; The type of an if is the join of the consequence and alternative
;; types if the type of the branches are consistent and the test is
;; consistent with Bool.
(: if-type-rule (TT TT TT Src . -> . TT))
(define (if-type-rule t-tst t-csq t-alt src)
  (cond
   [(not (consistent? t-tst BOOL-TYPE))
    (raise-if-inconsistent-test src t-tst)]
   [(not (consistent? t-csq t-alt))
    (raise-if-inconsistent-branches src t-csq t-alt)]
   [else (join t-csq t-alt)]))

;; The type of literal constants are staticly known
(: const-type-rule (Core-Literal . -> . (U Bool Int)))
(define (const-type-rule c)
  (if (boolean? c)
      BOOL-TYPE
      INT-TYPE))

;; The type of an application is the return type of the applied
;; procedure given that the arguments are consistent with the
;; arguments types of the proceedure.
(: application-type-rule (TT (Listof TT) Src . -> . TT))
(define (application-type-rule t-rator t-rand* src)
  (match t-rator
    [(Fn/a arr t-fml* t-ret)
     (if (and (= arr (length t-rand*))
	      (andmap consistent? t-fml* t-rand*))
	 t-ret
	 (raise-app-inconsistent src t-rator t-rand*))]
    [(Dyn) DYN-TYPE]
    [otherwise (raise-app-not-function src t-rator)]))

;;; Procedures that destructure and restructure the ast
;; tyck-expr : (Struct Expr) * Env -> (values (Struct Expr) Type)
(: tyck-expr (Core-Form Env . -> . (values TF TT)))
(define (tyck-expr exp env)
  (: recur (Core-Form . -> . (values TF TT)))
  (define (recur e) : (values TF TT)
    (define-syntax-rule (map-recur exp*)
      (for/lists ([e* : (Listof TF)]
		  [t* : (Listof TT)])
	  ([e exp*])
	(recur e)))
    (match e
      [(Lambda fmls ty-ret body src) 
       (tyck-lambda fmls ty-ret body src env)] 
      ;; let and letrec are very similar the only difference are restrictions
      ;; of not using type inference and the environment used to check the bindings
      [(Letrec bnd body src) (tyck-letrec bnd body src env)]
      [(Let bnd body src) (tyck-let bnd body src env recur)]
      [(Var id src)
       (let* ([ty (hash-ref env id (lookup-failed src id))]
	      [ann (cons src ty)])
	 (values (Var id ann) ty))]
      [(Ascribe (app recur exp ty-exp) ty-ann label src)
       (let* ([ty-ann ((check-type src) ty-ann)]
	      [ty (ascription-type-rule ty-exp ty-ann src label)]
	      [ann (cons src ty)])
	 (values (Ascribe exp ty-exp label ann) ty))]
      [(If tst csq alt src)
       (let*-values ([(tst ty-tst) (recur tst)]
		     [(csq ty-csq) (recur csq)]
		     [(alt ty-alt) (recur alt)]
		     [(ty) (if-type-rule ty-tst ty-csq ty-alt src)]
		     [(ann) (cons src ty)])
	 (values (If tst csq alt ann) ty))]
      [(Quote lit src)
       (let* ([ty (const-type-rule lit)]
	      [ann (cons src ty)])
	 (values (Quote lit ann) ty))]
      [(App rator rand* src)
       (let*-values ([(rator ty-rator) (recur rator)]
		     [(rand* ty-rand*) (map-recur rand*)]
		     [(ty) (application-type-rule ty-rator ty-rand* src)]
		     [(ann) (cons src ty)])
	 (values (App rator rand* ann) ty))]
      [(Op prim rand* src) 
       (let*-values ([(ty-prim) (typed-prim->typed-type prim)]
		     [(rand* ty-rand*) (map-recur rand*)]
		     [(ty) (application-type-rule ty-prim ty-rand* src)]
		     [(ann) (cons src ty)])
	 ;; The instance is ugly but needed in order to 
	 ;; get Op to typecheck.
	 (values (Op ((inst cons TP (Listof TT))
		      prim (Fn/a-fmls ty-prim)) 
		     rand* ann) ty))]))
  (recur exp))


#|
;; Variables are typed at the type associated with it in environment
;; If it isn't found then there must be a mistake in the
;; compiler because all unbound variable should be caught during parsing.


))
|#
(: tyck-lambda 
   (-> (Listof CFml) (Maybe CT) CF Src Env (values TF TT)))

(: unzip-formals 
   (-> (Listof CFml) 
       (values (Listof Uvar) (Listof CT))))
(define (unzip-formals f*)
  (for/lists ([i* : (Listof Uvar)] [t* : (Listof Core-Type)])
      ([f f*])
    (values (Fml-identifier f) (Fml-type f))))

(define (tyck-lambda fmls ty-ret body src env)
  (let*-values ([(id* ty*) (unzip-formals fmls)]
		[(ty*) (map (check-type src) ty*)]
		[(ty-ret) ((check-maybe-type src) ty-ret)]
		[(body ty-body) (tyck-expr body (env-extend* env id* ty*))]
		[(ty-lambda) (lambda-type-rule src ty* ty-body ty-ret)]
		[(ty-ret) (Fn/a-ret ty-lambda)]
		[(fml*) (map-Fml id* ty*)]
		[(ann) (cons src ty-lambda)]) 
    (values (Lambda fml* ty-ret body ann) ty-lambda)))

(: tyck-letrec 
   (-> (Listof CBnd) CF Src Env (values TF TT)))
(define (tyck-letrec bnd body src env)
  (let*-values 
      ([(id* ty* rhs*) (check-letrec-bindings src bnd)]
       [(env) (env-extend* env id* ty*)]
       [(recur/env) (lambda ([e : Core-Form]) 
		      (tyck-expr e env))]
       [(id* ty* rhs*) (tyck-bindings src recur/env id* ty* rhs*)] 
       [(body ty-body) (tyck-expr body env)]
       [(bnd*) (map-Bnd id* ty* rhs*)]
       [(ann) (cons src ty-body)])
    (values (Letrec bnd* body ann) ty-body)))

(: check-letrec-bindings 
   (-> Src (Listof CBnd)
       (values (Listof Uvar) (Listof TT) (Listof CF))))
(define (check-letrec-bindings src bnd*)
  (define check (check-type src))
  (for/lists ([u* : (Listof Uvar)]
	      [t* : (Listof TT)]
	      [r* : (Listof CF)])
      ([b bnd*])
    (let ([u (Bnd-identifier b)]
	  [t (Bnd-type b)]
	  [r (Bnd-expression b)])
      (if (and t (Lambda? r)) 
	  (values u (check t) r)
	  (if (Lambda? r)
	      (let ([ret (Lambda-return-type r)]
		    [args : (Listof CT) 
			  (map (inst Fml-type CT) 
			       (Lambda-formals r))])
		(if ret
		    (values u 
			    (Fn/a (length args) 
				  (map check args) 
				  (check ret)) 
			    r)
		    (raise-letrec-restrict src)))
	      (raise-letrec-restrict src))))))

(: tyck-let 
   (-> (Listof CBnd) CF Src Env (-> CF (values TF TT))
       (values TF TT)))
(define (tyck-let bnd* body src env recur)
  (let*-values 
      ([(id* ty* rhs*) (check-let-bindings src bnd*)]
       [(id* ty* rhs*) (tyck-bindings src recur id* ty* rhs*)] 
       [(body ty-body) (tyck-expr body (env-extend* env id* ty*))]
       [(bnd*) (map-Bnd id* ty* rhs*)]
       [(ann) (cons src ty-body)])
    (values (Let bnd* body ann) ty-body)))

(: check-let-bindings 
   (-> Src (Listof CBnd)
       (values (Listof Uvar) (Listof (Maybe TT)) (Listof CF))))
(define (check-let-bindings src bnd*)
  (let ([check (check-maybe-type src)])
    (for/lists ([u* : (Listof Uvar)]
		[t* : (Listof (Maybe TT))]
		[r* : (Listof CF)])
	([b bnd*])
      (let ([u (Bnd-identifier b)]
	    [t (Bnd-type b)]
	    [r (Bnd-expression b)])
	(values u (check t) r)))))


(: check-type (-> Src (-> CT TT)))

(define (check-type src)
  (letrec ([recur :  (CT . -> . TT)
		  (lambda (t) 
		    (if (or (Int? t) (Bool? t) (Dyn? t))
			t
			(let* ((ret (Fn-ret t))
			       (arg (Fn-fmls t))
			       (ary (length arg)))
			  (cond
			   [(not (= ary 1)) (raise-only-single-arity-fns src)]
			   [(not (= 1 (length ret))) (raise-only-single-arity-ret src)]
			   [else (Fn/a ary (map recur arg) (recur (car ret)))]))))])
    recur))

(: check-maybe-type 
   (Src . -> . ((Maybe Core-Type) . -> . (Maybe TT))))

(define (check-maybe-type src)
  (let ((check (check-type src)))
    (lambda ([t : (Maybe Core-Type)]) (and t (check t)))))

;; Type information about check-letrec-bindings


;; Type checks the rhs to be consistent with type annotation if
;; provided the resulting type is the type of the annotation.
(: tyck-bindings (-> Src (Core-Form . -> . (Values TF TT))
		     (Listof Uvar) (Listof (Maybe TT)) (Listof Core-Form)
		     (values (Listof Uvar) 
			     (Listof TT) 
			     (Listof TF))))
(define (tyck-bindings src tyck-exp uvar* type* rhs*)
  (for/lists ([u* : (Listof Uvar)]
	      [t* : (Listof TT)]
	      [r* : (Listof TF)])
      ([u uvar*][t type*][r rhs*])
    (let*-values ([(r t^) (tyck-exp r)]
		  [(t) (let-binding-type-rule t t^ u src)])
      (values u t r))))

