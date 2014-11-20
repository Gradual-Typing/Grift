#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/introduce-castable-functions                              |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
 Discription: 
 This pass adds the castable form which extends all lambdas that
 are castable with a free variable which represents the function that is       
 necissary in order to cast the closure created by this lambda. It can now be  
 extracted from a closure value using the fn-cast form. Note that lambdas      
 created in the process of creating these casting functions are not castable.  
 Though their return values are.

 Casts of the form (runtime-cast value-exp type1-exp type2-exp label-exp) are
 used in each of the higher order casting functions because the values needed
 to perform the cast are not available until runtime.

 This pass also substitutes all casts of the for (cast v Fn1 Fn2 l) with the
 equivalent form (fn-cast v Fn1 Fn2 l), in order to show that the cast specific
 to the closure value is being used. 

 Furthermore all types not found in casts are removed.
+-------------------------------------------------------------------------------+
|Input Grammar Cast0-Language                                                   |
|Output Grammar Cast1-Language                                                  |
+------------------------------------------------------------------------------|#
(require schml/framework/build-compiler
         schml/framework/helpers
         schml/framework/errors
	 schml/compiler/language)

;; Only the pass is provided by this module
(provide introduce-castable-functions)

;; Using state passing style with a imutable hashtable that maps the arity of 
;; a function to the untyped binding of a function that can cast that arity.
(define-type Caster-State (Pair Natural (HashTable Index (Pairof Uid C1-Expr))))

(: get-caster (Index Caster-State . -> . (Values Uid Caster-State)))
(define (get-caster arity state)
  (match-let ([(cons next casters) state])
    (let ([bnd? (hash-ref casters arity #f)])
	  (if bnd?
	      (values (car bnd?) state)
	      (let-values ([(bnd next) (mk-caster-bnd arity next)])
		(values (car bnd) 
                        (cons next 
                              (hash-set casters arity bnd))))))))

(: extract-state (-> Caster-State 
                     (Values Natural (Listof (Pairof Uid C1-Expr)))))
(define (extract-state s) 
  (values (car s) (hash-values (cdr s))))

(: new-state (-> Natural Caster-State))
(define (new-state n) 
 (cons n ((inst hasheq Index (Pairof Uid C1-Expr)))))

;; The entry point for this pass it is called by impose-casting semantics
(: introduce-castable-functions (Cast0-Lang Config . -> . Cast1-Lang))
(define (introduce-castable-functions prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let*-values ([(exp state) (icf-expr exp (new-state next))]
		  [(next caster-bnd*) (extract-state state)])
      (Prog (list name next type) 
       (Letrec caster-bnd* exp)))))

(: icf-expr (-> C0-Expr Caster-State (values C1-Expr Caster-State)))
(define (icf-expr exp state)
  (match exp
      [(Lambda f* returns exp)
       (let*-values ([(f*) (map (inst Fml-identifier Uid Schml-Type) f*)]
                     [(uid state) (get-caster (length f*) state)]
		     [(exp state) (icf-expr exp state)])
         (values (Lambda f* #f (Castable uid exp)) state))]
      [(Cast exp t1 t2 lbl)
       (let*-values ([(exp state) (icf-expr exp state)])
	 (if (and (Fn? t1) (Fn? t2))
             (values (Fn-Cast exp t1 t2 lbl) state)
             (values (Cast exp t1 t2 lbl) state)))]
      [(Letrec bnd* exp)
       (let*-values ([(bnd* state) (icf-bnd* bnd* state)]
		     [(exp state) (icf-expr exp state)])
	 (values (Letrec bnd* exp) state))]
      [(Let bnd* exp)
       (let*-values ([(bnd* state) (icf-bnd* bnd* state)]
		     [(exp state) (icf-expr exp state)])
	 (values (Let bnd* exp) state))]
      [(App exp exp*)
       (let*-values ([(exp state) (icf-expr exp state)]
		     [(exp* state) (icf-expr* exp* state)])
	 (values (App exp exp*) state))]
      [(Op p exp*)
       (let*-values ([(exp* state) (icf-expr* exp* state)])
	 (values (Op p exp*) state))]
      [(If tst csq alt)
       (let*-values ([(tst state) (icf-expr tst state)]
		     [(csq state) (icf-expr csq state)]
		     [(alt state) (icf-expr alt state)])
	 (values (If tst csq alt) state))]
      [(Var id) (values (Var id) state)]
      [(Quote lit) (values (Quote lit) state)]))

;; State passing style over expressions
(: icf-expr* (-> C0-Expr* Caster-State (Values C1-Expr* Caster-State)))
(define (icf-expr* e* s)
  (if (null? e*)
      (values e* s)
      (let*-values ([(e s) (icf-expr (car e*) s)]
		    [(e* s) (icf-expr* (cdr e*) s)])
	(values (cons e e*) s))))

;; state passing style over bindings with a change frome typed to untyped
(: icf-bnd* (-> C0-Bnd* Caster-State (values C1-Bnd* Caster-State)))
(define (icf-bnd* b* s)
  (if (null? b*)
      (values '() s)
      (match-let ([(Bnd u t r) (car b*)])
	(let*-values ([(b* s) (icf-bnd* (cdr b*) s)]
		      [(r s) (icf-expr r s)]
                      [(b) (cons u r)])
	  (values (cons b b*) s)))))

;; create a untyped binding for a casting function
(: mk-caster-bnd (-> Index Natural (values C1-Bnd Natural)))
(define (mk-caster-bnd ary next)
  (let*-values ([(uid next) (next-uid (format "cast_fn~a" ary) next)]
		[(rhs next) (mk-caster-fn ary next uid)])
    (values (cons uid rhs) next)))

;; make a function that builds a function proxy for the cast of a specific
;; arity. This function is always of the following form
#|(lambda (v t1 t2 l)
    (if (= (fn-type-arity t1) (fn-type-arity t2))
      (lambda (a ...)
        (cast (v (cast a (fn-type-arg t2) (fn-type-arg t1) l) ...)
              (fn-type-ret t1)
              (fn-type-ret t2)
              l)))|#
(: mk-caster-fn (-> Index Natural Uid (values C1-Expr Natural)))
(define (mk-caster-fn ary next name)
  (: mk-caster-ids (-> Index Natural (Listof Uid) 
		       (values (Listof Uid) Natural)))
  ;; create uids for all arguments to the proxy
  (define (mk-caster-ids ary next vars)
    (if (zero? ary)
	(values vars next)
	(mk-caster-ids (sub1 ary) (add1 next) (cons (Uid "v" next) vars))))
  (: mk-casted-args (-> C1-Expr C1-Expr C1-Expr Uid* C1-Expr*))
  ;; create expression for the runtime cast of arguments to the proxy
  (define (mk-casted-args t1 t2 lbl uid*)
    (: loop (-> Uid* Index C1-Expr*))
    (define (loop uid* index)
      (if (null? uid*)
	  '()
	  (let ([val : C1-Expr (Var (car uid*))]
		[t1 : C1-Expr (Type-Fn-ref t1 index)]
		[t2 : C1-Expr (Type-Fn-ref t2 index)])
	    (cons (Runtime-Cast val t1 t2 lbl)
		  (loop (cdr uid*) (cast (add1 index) Index))))))
    (loop uid* 0))
  ;; create the uids for the casting function
  (let*-values ([(fn next) (next-uid "f" next)]
		[(t1 next) (next-uid "t1" next)]
		[(t2 next) (next-uid "t2" next)]
		[(lbl next) (next-uid "lbl" next)]
		[(uid* next) (mk-caster-ids ary next '())])
    ;; create the vars for these uids
    (let* ([fn-var : C1-Expr (Var fn)]
	   [t1-var : C1-Expr (Var t1)]
	   [t2-var : C1-Expr (Var t2)]
	   [lbl-var : C1-Expr (Var lbl)]
           ;; arguments to the unproxied function call
	   [casted-args : C1-Expr* 
            (mk-casted-args t1-var t2-var lbl-var uid*)]
           ;; formals for the casting function
	   [caster-fml : Uid* (list fn t1 t2 lbl)]
	   ;; cast the return of the unproxied application
           [cast-call-cast : C1-Expr 
            (Runtime-Cast (App fn-var casted-args) 
                          (Type-Fn-ref t1-var 'return) 
                          (Type-Fn-ref t2-var 'return) 
                          lbl-var)]
           ;; application isn't performed if the arity is a mismatch
	   [arity-t1 : C1-Expr (Type-Fn-ref t1-var 'arity)]
           [arity-t2 : C1-Expr (Type-Fn-ref t2-var 'arity)]
           [arity-test : C1-Expr (Op '= (list arity-t1 arity-t2))]
	   ;; the closure to return if arity is correct
           [then-cast : C1-Expr 
            (Lambda uid* #f (Castable name cast-call-cast))]
           ;; the closure to return if arity is incorrect
	   [else-blame : C1-Expr 
            (Lambda '() #f (Castable name (Blame lbl-var)))]
	   [check : C1-Expr 
            (If arity-test then-cast else-blame)]
	   ;; The entire casting function is now all constructed
           [caster  : C1-Expr 
            (Lambda caster-fml #f (Castable #f check))])
      (values caster next))))

