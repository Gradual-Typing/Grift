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
|Grammar for Core-Prog found in schml/languages/core-forms                      |
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
|Grammar for Typed-Prog found in schml/languages/typed-forms                    |
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
(require schml/compiler/errors
	 schml/compiler/helpers
	 schml/compiler/language)

;; Only the pass is provided by this module
(provide type-check)

(: type-check (Schml0-Lang Config . -> . Schml1-Lang)) 
(define (type-check prgm config)
  (match-let ([(Prog (list name next-uid) exp) prgm])
    (let-values ([(exp type) (tc-expr exp (hash))])
      (Prog (list name next-uid type) exp))))

(define-type Env (HashTable Uid Schml-Type))
(define-syntax-rule (env-extend* e x* v*)
  (for/fold ([t : Env e]) ([x x*] [v v*])
    (hash-set t x v)))

;;; Procedures that are used to throw errors
;; The error that occurs when a variable is not found. It is an internal
;; error because it is a syntax error to have an unbound variable.
(define-syntax-rule (lookup-failed src id)
  (lambda () (raise-variable-not-found src id)))

;;; The type rules for core forms that have interesting type rules
;; The type of a lambda that is annotated is the type of the annotation
;; as long as the annotation is consistent with the type of the
;; body
(: lambda-type-rule (-> Src Schml-Type* Schml-Type Schml-Type? 
			(Fn Index Schml-Type* Schml-Type)))
(define (lambda-type-rule src ty* t-body t-ann)
  (cond
   [(not t-ann) (Fn (length ty*) ty* t-body)]
   [(consistent? t-body t-ann) (Fn (length ty*) ty* t-ann)]
   [else  (raise-lambda-inconsistent src t-body t-ann)]))

;; The type of a annotated let binding is the type of the annotation
;; as long as it is consistent with the type of the expression.
(: let-binding-type-rule (-> Schml-Type? Schml-Type Uid Src Schml-Type))
(define (let-binding-type-rule t-bnd t-exp id src)
  (cond
   [(not t-bnd) t-exp]
   [(consistent? t-bnd t-exp) t-bnd]
   [else (raise-binding-inconsistent src id t-bnd t-exp)]))

;; The type of a cast is the cast-type if the expression type and
;; the cast type are consistent.
(: ascription-type-rule (-> Schml-Type Schml-Type Src (Option String) 
			    Schml-Type))
(define (ascription-type-rule ty-exp ty-cast src label)
  (if (not (consistent? ty-exp ty-cast))
      (if label
	  (raise-blame-label label)
	  (raise-ascription-inconsistent src ty-exp ty-cast))
      ty-cast))

;; The type of an if is the join of the consequence and alternative
;; types if the type of the branches are consistent and the test is
;; consistent with Bool.
(: if-type-rule (-> Schml-Type Schml-Type Schml-Type Src
		    Schml-Type))
(define (if-type-rule t-tst t-csq t-alt src)
  (cond
   [(not (consistent? t-tst BOOL-TYPE))
    (raise-if-inconsistent-test src t-tst)]
   [(not (consistent? t-csq t-alt))
    (raise-if-inconsistent-branches src t-csq t-alt)]
   [else (join t-csq t-alt)]))

;; The type of literal constants are staticly known
(: const-type-rule (Schml-Literal . -> . (U Bool Int)))
(define (const-type-rule c)
  (if (boolean? c)
      BOOL-TYPE
      INT-TYPE))

;; The type of an application is the return type of the applied
;; procedure given that the arguments are consistent with the
;; arguments types of the proceedure.
(: application-type-rule (-> Schml-Type Schml-Type* Src
			     Schml-Type))
(define (application-type-rule t-rator t-rand* src)
  (match t-rator
    [(Fn arr t-fml* t-ret)
     (if (and (= arr (length t-rand*))
	      (andmap consistent? t-fml* t-rand*))
	 t-ret
	 (raise-app-inconsistent src t-rator t-rand*))]
    [(Dyn) DYN-TYPE]
    [otherwise (raise-app-not-function src t-rator)]))

;;; Procedures that destructure and restructure the ast
;; tc-expr : (Struct Expr) * Env -> (values (Struct Expr) Type)
(: tc-expr (-> S0-Expr Env (values S1-Expr Schml-Type)))
(define (tc-expr exp env)
  (define-syntax-rule (map-recur exp*)
    (for/lists ([e* : (Listof S1-Expr)] [t* : (Listof Schml-Type)])
	([e exp*]) (recur e)))
  (: recur (-> S0-Expr (values S1-Expr Schml-Type)))
  (define (recur e)
    (let ((src (Ann-data e)))
      (match (Ann-value e)
	[(Lambda fmls ty-ret body) 
	 (tc-lambda fmls ty-ret body src env)] 
	[(Letrec bnd body) (tc-letrec bnd body src env)]
	[(Let bnd body) (tc-let bnd body src env recur)]
	[(App rator rand*)
	 (let*-values ([(rator ty-rator) (recur rator)]
		       [(rand* ty-rand*) (map-recur rand*)]
		       [(ty) (application-type-rule ty-rator ty-rand* src)])
	   (values (Ann (App rator rand*) (cons src ty)) ty))]
	[(Op prim rand*)
	 (let*-values ([(ty-prim) (schml-prim->type prim)]
		       [(rand* ty-rand*) (map-recur rand*)]
		       [(ty) (application-type-rule ty-prim ty-rand* src)])
	   (values (Ann (Op (Ann prim (Fn-fmls ty-prim)) rand*)
			(cons src ty))
		   ty))]
	[(Ascribe (app recur exp ty-exp) ty-ann label)
	 (let* ([ty (ascription-type-rule ty-exp ty-ann src label)]
		[ann (cons src ty)])
	   (values (Ann (Ascribe exp ty-exp label) (cons src ty)) ty))]
	[(If tst csq alt)
	 (let*-values ([(tst ty-tst) (recur tst)]
		       [(csq ty-csq) (recur csq)]
		       [(alt ty-alt) (recur alt)]
		       [(ty) (if-type-rule ty-tst ty-csq ty-alt src)])
	   (values (Ann (If tst csq alt) (cons src ty)) ty))]
	[(Var id) (let ([ty (hash-ref env id (lookup-failed src id))])
		    (values (Ann (Var id) (cons src ty)) ty))]
	[(Quote lit) (let* ([ty (const-type-rule lit)])
		       (values (Ann (Quote lit) (cons src ty)) ty))])))
  (recur exp))


(: tc-lambda (-> Schml-Fml* Schml-Type? S0-Expr Src Env
		 (values S1-Expr Schml-Type)))
(define (tc-lambda fml* ty-ret body src env)
  (let*-values ([(id* ty*) (unzip-formals fml*)]
		[(body ty-body) (tc-expr body (env-extend* env id* ty*))]
		[(ty-lambda) (lambda-type-rule src ty* ty-body ty-ret)]) 
    (values (Ann (Lambda fml* (Fn-ret ty-lambda) body)
		 (cons src ty-lambda))
	    ty-lambda)))

(: unzip-formals (-> Schml-Fml* (values (Listof Uid) Schml-Type*)))
(define (unzip-formals f*)
  (for/lists ([i* : (Listof Uid)] [t* : Schml-Type*])
      ([f f*])
    (values (Fml-identifier f) (Fml-type f))))


(: tc-letrec (-> S0-Bnd* S0-Expr Src Env 
		 (values S1-Expr Schml-Type)))
(define (tc-letrec bnd* body src env)
    (: fold-env-extend/bnd (S0-Bnd* Env . -> . (values S0-Bnd* Env)))
  (define (fold-env-extend/bnd b* env)
    (for/fold : (values S0-Bnd* Env) ([bnd* : S0-Bnd* '()] [env : Env env]) ([bnd : S0-Bnd b*])
              (match bnd
                [(Bnd id type (Ann (Lambda f* t^ b) src))
                 (if type
                  (values (cons bnd bnd*) (hash-set env id type))
                  (let* ([arg-t* (map (inst Fml-type Uid Schml-Type) f*)]
                         [arity  (length arg-t*)])
                    (if t^
                        (values (cons bnd bnd*)
                                (hash-set env id (Fn arity arg-t* t^)))
                        (let* ([type : Schml-Type (Fn arity arg-t* DYN-TYPE)]
                               [rhs : S0-Expr (Ann (Lambda f* DYN-TYPE b) src)])
                          (values (cons (Bnd id type rhs) bnd*)
                                  (hash-set env id type))))))]
                  ;; [t^ (values (cons bnd bnd*) (hash-set env i ))]
                  ;; []
                  ;; [else ]) (if t
                  ;;       (hash-set env i t)
                  ;;       (let ([args (map (inst Fml-type Uid Schml-Type) f*)])
                  ;;         (if t^
                  ;;             (hash-set env i (Fn (length args) args t^))
                  ;;             (hash-set env i (Fn (length args) args DYN-TYPE)))))]
                [else (raise-letrec-restrict src)])))
  (let*-values 
      ([(bnd* env) (fold-env-extend/bnd bnd* env)] 
       [(recur/env) (lambda ([e : S0-Expr]) (tc-expr e env))]
       [(bnd*) (map (mk-tc-binding src recur/env) bnd*)] 
       [(body ty-body) (tc-expr body env)])
    (values (Ann (Letrec bnd* body) (cons src ty-body)) ty-body)))

(: tc-let (-> S0-Bnd* S0-Expr Src Env (-> S0-Expr (values S1-Expr Schml-Type)) 
	      (values S1-Expr Schml-Type)))
(define (tc-let bnd* body src env recur)
  (: env-extend/bnd (S1-Bnd Env . -> . Env))
  (define (env-extend/bnd b env)
    (match-let ([(Bnd i t _) b]) (hash-set env i t)))
  (let*-values
      ([(bnd*) (map (mk-tc-binding src recur) bnd*)]
       [(body ty-body) (tc-expr body (foldl env-extend/bnd env bnd*))])
    (values (Ann (Let bnd* body) (cons src ty-body)) ty-body)))


;; Type checks the rhs to be consistent with type annotation if
;; provided the resulting type is the type of the annotation.
(: mk-tc-binding (-> Src (-> S0-Expr (values S1-Expr Schml-Type)) 
		     (-> S0-Bnd S1-Bnd)))
(define (mk-tc-binding src tc-expr)
  (lambda ([bnd : S0-Bnd]) : S1-Bnd
    (match-let ([(Bnd id type rhs) bnd])
      (let-values ([(rhs type-rhs) (tc-expr rhs)])
	(Bnd id 
	     (let-binding-type-rule type type-rhs id src)
	     rhs)))))

