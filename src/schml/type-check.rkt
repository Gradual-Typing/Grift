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
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

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

;; I am really just defining this in order to maintain
;; the abstraction but the type of a begin is the type
;; of it's final argument
(: begin-type-rule (-> Schml-Type* Schml-Type Schml-Type))
(define (begin-type-rule t* ty) ty)

;; The type of wrapping a value in a gaurded box is a
;; Gref of the value's type
(: gbox-type-rule (-> Schml-Type Schml-Type))
(define (gbox-type-rule ty) (GRef ty))

;; The type of unwrapping a Dyn value is also Dyn
;; The type of unwrapping a Ref is the type that the reference is
;; parameterized by.
(: gunbox-type-rule (-> Schml-Type Schml-Type))
(define (gunbox-type-rule ty)
  (match ty
    [(Dyn) DYN-TYPE]
    [(GRef g) g]
    [otherwise (TODO raise an appropriate error here)]))

;; The type of setting a reference is always unit
(: gbox-set!-type-rule (-> Schml-Type Schml-Type Schml-Type))
(define (gbox-set!-type-rule box-ty val-ty)
  (match box-ty
    [(Dyn) UNIT-TYPE]
    [(GRef g) (if (consistent? g val-ty)
                  UNIT-TYPE
                  (TODO raise an error about type inconsistency between g and val-ty))]
    [otherwise (TODO raise an error about box-type being type-incorrect)]))

;; The type of wrapping a value in a monotonic box is a
;; MRef of the value's type
(: mbox-type-rule (-> Schml-Type Schml-Type))
(define (mbox-type-rule ty) (MRef ty))

;; The type of unboxing a Dyn value is also Dyn
;; The type of unboxing a MRef is the type that the reference is
;; parameterized by.
(: munbox-type-rule (-> Schml-Type Schml-Type))
(define (munbox-type-rule ty)
  (match ty
    [(Dyn) DYN-TYPE]
    [(MRef m) m]
    [otherwise (TODO raise an error about there a type error here)]))

;; The type of setting a dyn value is dyn
;; The type of setting a MRef value is the type of the armurment
(: mbox-set!-type-rule (-> Schml-Type Schml-Type Schml-Type))
(define (mbox-set!-type-rule box-ty val-ty)
  (match box-ty
    [(Dyn) DYN-TYPE]
    [(MRef m) (if (consistent? m val-ty)
                  val-ty
                  (TODO raise an error about type inconsistency between m and val-ty))]
    [otherwise (TODO raise an error about box-type beinm type-incorrect)]))

;; The type of creating an array is Vect of the type of the initializing argument
;; The size argument must be consistent with Int
(: gvector-type-rule (-> Schml-Type Schml-Type Schml-Type))
(define (gvector-type-rule size-ty init-ty)
  (if (consistent? size-ty INT-TYPE)
      (GVect init-ty)
      (TODO come up with an error for being inconsistent with int here)))

;; The type of reffing into an Dyn is Dyn
;; The type of reffing into a Vect T is T
;; The type of the index must be consistent with Int
(: gvector-ref-type-rule (-> Schml-Type Schml-Type Schml-Type))
(define (gvector-ref-type-rule vect-ty index-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) DYN-TYPE]
        [(GVect g) g]
        [otherwise (TODO raise inconsistent with GVECT 'a here)])
      (TODO raise inconsistent with INT here)))

;; The type of setting a guarded vector of type T is the type of
;; The new value as long as the new value is consistent with the old value
;; The type of setting a Dyn is Dyn
;; The indice must be consistent with int
(: gvector-set!-type-rule (-> Schml-Type Schml-Type Schml-Type Schml-Type))
(define (gvector-set!-type-rule vect-ty index-ty val-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) DYN-TYPE]
        [(GVect g) (if (consistent? g val-ty)
                       val-ty
                       (TODO raise a error about consistency between g and val-ty))]
        [otherwise (TODO raise an error about consistency between (Gvect val-ty))])
      (TODO raise an error about consistency between index-ty and int)))


;; The type of creating an array is Vect of the type of the initializing argument
;; The size argument must be consistent with Int
(: mvector-type-rule (-> Schml-Type Schml-Type Schml-Type))
(define (mvector-type-rule size-ty init-ty)
  (if (consistent? size-ty INT-TYPE)
      (MVect init-ty)
      (TODO come up with an error for being inconsistent with int here)))

;; The type of reffing into an Dyn is Dyn
;; The type of reffing into a Vect T is T
;; The type of the index must be consistent with Int
(: mvector-ref-type-rule (-> Schml-Type Schml-Type Schml-Type))
(define (mvector-ref-type-rule vect-ty index-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) DYN-TYPE]
        [(MVect g) g]
        [otherwise (TODO raise inconsistent with MVECT 'a here)])
      (TODO raise inconsistent with INT here)))

;; The type of setting a monotonic vector of type T is the type of
;; The new value as long as the new value is consistent with the old value
;; The type of setting a Dyn is Dyn
;; The indice must be consistent with int
(: mvector-set!-type-rule (-> Schml-Type Schml-Type Schml-Type Schml-Type))
(define (mvector-set!-type-rule vect-ty index-ty val-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) DYN-TYPE]
        [(MVect g) (if (consistent? g val-ty)
                       val-ty
                       (TODO raise a error about consistency between g and val-ty))]
        [otherwise (TODO raise an error about consistency between (Mvect val-ty))])
      (TODO raise an error about consistency between index-ty and int)))


;;; Procedures that destructure and restructure the ast
(: tc-expr (-> S0-Expr Env (values S1-Expr Schml-Type)))
(define (tc-expr exp env)
  (define-syntax-rule (map-recur exp*)
    (for/lists ([e* : (Listof S1-Expr)] [t* : (Listof Schml-Type)])
      ([e exp*]) (recur e)))
  (: recur (-> S0-Expr (values S1-Expr Schml-Type)))
  (define (recur e)
    (let ((src (Ann-data e)))
      (match (Ann-value e)
	[(Lambda fmls (Ann body ty-ret)) 
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
		       (values (Ann (Quote lit) (cons src ty)) ty))]
        [(Begin e* e) 
         (let*-values ([(e  t1) (recur e)]
                       [(e* t*) (map-recur e*)]
                       [(ty)    (begin-type-rule t* t1)])
           (values (Ann (Begin e* e) (cons src ty)) ty))]
        [(Gbox e) 
         (let*-values ([(e ty) (recur e)]
                       [(ty)   (gbox-type-rule ty)])
           (values (Ann (Gbox e) (cons src ty)) ty))]
        [(Gunbox e)          
         (let*-values ([(e ty) (recur e)]
                       [(ty)   (gunbox-type-rule ty)])
           (values (Ann (Gunbox e) (cons src ty)) ty))]
        [(Gbox-set! e1 e2)
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(ty)    (gbox-set!-type-rule t1 t2)])
           (values (Ann (Gbox-set! e1 e2) (cons src ty)) ty))]
        [(Mbox e) 
         (let*-values ([(e ty) (recur e)]
                       [(ty)   (mbox-type-rule ty)])
           (values (Ann (Mbox e) (cons src ty)) ty))]
        [(Munbox e)          
         (let*-values ([(e ty) (recur e)]
                       [(ty)   (munbox-type-rule ty)])
           (values (Ann (Munbox e) (cons src ty)) ty))]
        [(Mbox-set! e1 e2)
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(ty)    (mbox-set!-type-rule t1 t2)])
           (values (Ann (Mbox-set! e1 e2) (cons src ty)) ty))]
        [(Gvector e1 e2)  
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(ty)    (gvector-type-rule t1 t2)])
           (values (Ann (Gvector e1 e2) (cons src ty)) ty))]
        [(Gvector-ref e1 e2)
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(ty)    (gvector-ref-type-rule t1 t2)])
           (values (Ann (Gvector-ref e1 e2) (cons src ty)) ty))]
        [(Gvector-set! e1 e2 e3) 
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(e3 t3) (recur e3)]
                       [(ty)    (gvector-set!-type-rule t1 t2 t3)])
           (values (Ann (Gvector-set! e1 e2 e3) (cons src ty)) ty))]
        [(Mvector e1 e2)  
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(ty)    (mvector-type-rule t1 t2)])
           (values (Ann (Mvector e1 e2) (cons src ty)) ty))]
        [(Mvector-ref e1 e2)
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(ty)    (mvector-ref-type-rule t1 t2)])
           (values (Ann (Mvector-ref e1 e2) (cons src ty)) ty))]
        [(Mvector-set! e1 e2 e3) 
         (let*-values ([(e1 t1) (recur e1)]
                       [(e2 t2) (recur e2)]
                       [(e3 t3) (recur e3)]
                       [(ty)    (mvector-set!-type-rule t1 t2 t3)])
           (values (Ann (Mvector-set! e1 e2 e3) (cons src ty)) ty))])))
  (recur exp))

(: tc-lambda (-> Schml-Fml* Schml-Type? S0-Expr Src Env
		 (values S1-Expr Schml-Type)))
(define (tc-lambda fml* ty-ret body src env)
  (let*-values ([(id* ty*) (unzip-formals fml*)]
		[(body ty-body) (tc-expr body (env-extend* env id* ty*))]
		[(ty-lambda) (lambda-type-rule src ty* ty-body ty-ret)]) 
    (values (Ann (Lambda fml* body) (cons src ty-lambda))
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
                [(Bnd id type (Ann (Lambda f* (Ann b t^)) src))
                 (if type
                  (values (cons bnd bnd*) (hash-set env id type))
                  (let* ([arg-t* (map (inst Fml-type Uid Schml-Type) f*)]
                         [arity  (length arg-t*)])
                    (if t^
                        (values (cons bnd bnd*)
                                (hash-set env id (Fn arity arg-t* t^)))
                        (let* ([type : Schml-Type (Fn arity arg-t* DYN-TYPE)]
                               [rhs : S0-Expr (Ann (Lambda f* (Ann b DYN-TYPE)) src)])
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

