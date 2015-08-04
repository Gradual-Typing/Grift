#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/introduce-castable-functions                              |
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
|Input Grammar Cast1-Language                                                   |
|Output Grammar Cast2-Language                                                  |
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

;; Only the pass is provided by this module
(provide introduce-castable-functions)

;; The entry point for this pass it is called by impose-casting semantics
(: introduce-castable-functions (Cast0-Lang Config . -> . Cast1-Lang))
(define (introduce-castable-functions prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let-values ([(exp state) (run-state (icf-expr exp)
                                         (cons next no-casters))])
      (Prog (list name (car state) type)
       (Letrec (hash-values (cdr state)) exp)))))

(: icf-expr (-> C0-Expr (State Casters C1-Expr)))
(define (icf-expr exp)
  (match exp
    [(Lambda f* exp)
     (do (bind-state : (State Casters C1-Expr))
         (uid : Uid     <- (get-caster (length f*)))
         (exp : C1-Expr <- (icf-expr exp))
         ;; TODO we
         (return-state (Lambda f* (Castable uid exp))))]
    [(Cast exp t1 t2 lbl)
     (do (bind-state : (State Casters C1-Expr))
         (exp : C1-Expr <- (icf-expr exp))
         (if (and (Fn? t1) (Fn? t2))
             ;; TODO maybe this should be (App (Fn-Caster exp) exp t1 t2 lbl)
             (return-state (Fn-Cast exp t1 t2 lbl))
             (return-state (Cast exp t1 t2 lbl))))]
    [(Letrec bnd* exp)
     (do (bind-state : (State Casters C1-Expr))
         (bnd* : C1-Bnd* <- (icf-bnd* bnd*))
         (exp : C1-Expr  <- (icf-expr exp))
         (return-state (Letrec bnd* exp)))]
    [(Let bnd* exp)
     (do (bind-state : (State Casters C1-Expr))
         (bnd* : C1-Bnd* <- (icf-bnd* bnd*))
         (exp : C1-Expr  <- (icf-expr exp))
         (return-state (Let bnd* exp)))]
    [(App exp exp*)
     (do (bind-state : (State Casters C1-Expr))
         (exp  : C1-Expr  <- (icf-expr  exp))
         (exp* : C1-Expr* <- (icf-expr* exp*))
         (return-state (App exp exp*)))]
    [(Op p exp*)
     (do (bind-state : (State Casters C1-Expr))
         (exp* : C1-Expr* <- (icf-expr* exp*))
         (return-state (Op p exp*)))]
    [(If tst csq alt)
     (do (bind-state : (State Casters C1-Expr))
         (tst : C1-Expr <- (icf-expr tst))
         (csq : C1-Expr <- (icf-expr csq))
         (alt : C1-Expr <- (icf-expr alt))
         (return-state (If tst csq alt)))]
    [(Begin e* e)
     (do (bind-state : (State Casters C1-Expr))
         (e* : C1-Expr* <- (icf-expr* e*))
         (e  : C1-Expr  <- (icf-expr  e))
         (return-state (Begin e* e)))]
    [(Repeat i e1 e2 e3)
     (do (bind-state : (State Casters C1-Expr))
         (e1 : C1-Expr <- (icf-expr e1))
         (e2 : C1-Expr <- (icf-expr e2))
         (e3 : C1-Expr <- (icf-expr e3))
         (return-state (Repeat i e1 e2 e3)))]
    [(Gbox e) (lift-state (inst Gbox C1-Expr) (icf-expr e))]
    [(Gunbox e) (lift-state (inst Gunbox C1-Expr) (icf-expr e))]
    [(Gbox-set! e1 e2) (lift-state (inst Gbox-set! C1-Expr C1-Expr)
                                   (icf-expr e1) (icf-expr e2))]
    [(Var id)    (return-state (Var id))]
    [(Quote lit) (return-state (Quote lit))]))

(: icf-expr* (-> C0-Expr* (State Casters C1-Expr*)))
(define (icf-expr* e*) (map-state icf-expr e*))

;; Recur through binding with the casting state
(: icf-bnd* (-> C0-Bnd* (State Casters C1-Bnd*)))
(define (icf-bnd* b*)
  (: icf-bnd (-> C0-Bnd (State Casters C1-Bnd)))
  (define (icf-bnd b)
    (match-let ([(cons i e) b])
      (do (bind-state : (State Casters C1-Bnd))
          (e : C1-Expr <- (icf-expr e))
          (return-state (cons i e)))))
  (map-state icf-bnd b*))

;; Using state passing style with a imutable hashtable that maps the arity of
;; a function to the untyped binding of a function that can cast that arity.
(define-type Casters (Pair Natural (HashTable Index (Pairof Uid C1-Expr))))

(: get-caster (Index . -> . (State Casters Uid)))
(define (get-caster arity)
  (do (bind-state : (State Casters Uid))
      (state : Casters <- get-state)
      (match-let ([(cons next table) state])
        (let ([bnd? (hash-ref table arity #f)])
	  (if bnd?
	      (return-state (car bnd?))
	      (let-values ([(bnd next) (run-state (mk-caster-bnd arity) next)])
		(do (bind-state : (State Casters Uid))
                    (_ : Null <- (put-state (cons next (hash-set table arity bnd))))
                    (return-state (car bnd)))))))))

(: no-casters (HashTable Index C1-Bnd))
(define no-casters (hasheq))



;; create a untyped binding for a casting function
(: mk-caster-bnd (-> Index (State Natural C1-Bnd)))
(define (mk-caster-bnd ary)
  (do (bind-state : (State Natural C1-Bnd))
      (uid : Uid     <- (uid-state (format "cast_fn~a" ary)))
      (rhs : C1-Expr <- (mk-caster-fn ary uid))
      (return-state (cons uid rhs))))

;; make a function that builds a function proxy for the cast of a specific
;; arity. This function is always of the following form
#|
(lambda (v t1 t2 l)
  (if (= (fn-type-arity t1) (fn-type-arity t2))
      (lambda (a ...)
        (cast (v (cast a (fn-type-arg t2) (fn-type-arg t1) l) ...)
              (fn-type-ret t1) (fn-type-ret t2) l)))
|#
(: mk-caster-fn (-> Index Uid (State Natural C1-Expr)))
(define (mk-caster-fn ary name)
  ;; create uids for all arguments to the proxy
  (: mk-caster-ids (-> Index Uid* (State Natural Uid*)))
  (define (mk-caster-ids ary vars)
    (if (zero? ary)
	(return-state vars)
        (do (bind-state : (State Natural Uid*))
            (uid : Uid <- (uid-state "v"))
            (mk-caster-ids (- ary 1) (cons uid vars)))))
  ;; create expression for the runtime cast of arguments to the proxy
  (: mk-casted-args (-> C1-Expr C1-Expr C1-Expr Uid* C1-Expr*))
  (define (mk-casted-args t1 t2 lbl uid*)
    (: loop (-> Uid* Index C1-Expr*))
    (define (loop uid* index)
      (let ([i (Quote index)])
        (if (null? uid*)
            '()
            (let ([val : C1-Expr (Var (car uid*))]
                  [t1 : C1-Expr (Type-Fn-arg t1 i)]
                  [t2 : C1-Expr (Type-Fn-arg t2 i)])
              (cons (Runtime-Cast val t2 t1 lbl)        ;; contravarient argument cast
                    (loop (cdr uid*) (cast (add1 index) Index)))))))
    (loop uid* 0))
  (do (bind-state : (State Natural C1-Expr))
      ;; create the uids for the casting function
      (fn : Uid <- (uid-state "f"))
      (t1 : Uid <- (uid-state "t1"))
      (t2 : Uid <- (uid-state "t2"))
      (lbl : Uid <- (uid-state "lbl"))
      (uid* : Uid* <- (mk-caster-ids ary '()))
      ;; create the vars for these uids
      (let* ([fn-var  : C1-Expr (Var fn)]
             [t1-var  : C1-Expr (Var t1)]
             [t2-var  : C1-Expr (Var t2)]
             [lbl-var : C1-Expr (Var lbl)]
             ;; arguments to the unproxied function call
             [args : C1-Expr* (mk-casted-args t1-var t2-var lbl-var uid*)]
             ;; formals for the casting function
             [caster-fml : Uid* (list fn t1 t2 lbl)]
             ;; cast the return of the unproxied application
             [t1-ret    : C1-Expr (Type-Fn-return t1-var)]
             [t2-ret    : C1-Expr (Type-Fn-return t2-var)]
             [call      : C1-Expr (App fn-var args)]
             [cast-call : C1-Expr (Runtime-Cast call t1-ret t2-ret lbl-var)]
             ;; application isn't performed if the arity is a mismatch
             [arity-t1 : C1-Expr (Type-Fn-arity t1-var)]
             [arity-t2 : C1-Expr (Type-Fn-arity t2-var)]
             [arity-test : C1-Expr (Op '= (list arity-t1 arity-t2))]
             ;; the closure to return if arity is correct
             [then-cast : C1-Expr (Lambda uid* (Castable name cast-call))]
             ;; the closure to return if arity is incorrect
             [else-blame : C1-Expr (Lambda '() (Castable name (Blame lbl-var)))]
             [check : C1-Expr (If arity-test then-cast else-blame)])
        ;; The entire casting function is now all constructed
        (return-state (Lambda caster-fml (Castable #f check))))))
