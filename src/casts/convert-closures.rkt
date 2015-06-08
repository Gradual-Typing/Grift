#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/convert-closures                                               |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass sets up the structure of closures
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

(provide convert-closures)


(define optimize-self-reference?
  (make-parameter #f))

(define optimize-known-call?
  (make-parameter #f))

(define use-code-casters?
  (make-parameter #f))

(: convert-closures (Cast5-Lang Config . -> . Cast6-Lang))
(define (convert-closures prgm conf)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let ([base-cc-expr (cc-expr empty-env empty-env no-selfp)])
      (let-values ([(exp count) (run-state (base-cc-expr exp) count)])
        (Prog (list name count type) exp)))))

(define empty-env : Env (hash))
(define no-selfp : SelfP
  (lambda a #f))

(define-type BP  C6-Bnd-Procedure)
(define-type BC  C6-Bnd-Closure)
(define-type BP* C6-Bnd-Procedure*)
(define-type BC* C6-Bnd-Closure*)
(define-type BD* C6-Bnd-Data*)
(define-type Env (HashTable Uid C6-Expr))
(define-type SelfP (-> Uid (Option C6-Expr)))

(: cc-expr : Env Env SelfP -> (C5-Expr -> (State Nat C6-Expr)))
(define (cc-expr code-env data-env selfp)
  (: cc-expr/env : C5-Expr -> (State Nat C6-Expr))
  (define (cc-expr/env e)
    (match e
      ;; The interesting cases:
      ;; letrec are actually code bindings with shared state closure
      ;; creating bodies
      [(Letrec b* e)
       (do (bind-state : (State Nat C6-Expr))
           (b*-ext : (List BP* BC* Env Env) <- (cc-bnd-lambda* code-env data-env selfp b*))
           (match-let ([(list bp bc c-env d-env) b*-ext])
             (e : C6-Expr <- ((cc-expr c-env d-env selfp) e))
             (return-state (LetP bp (LetC bc e)))))]
      ;; Function cast extraction removes the cast closure (could optimize to cast pointer)
      [(Fn-Caster e)
       (do (bind-state : (State Nat C6-Expr))
           (e  : C6-Expr <- (cc-expr/env e))
           (return-state (Closure-caster e)))]
      ;; Applications get the code pointer and pass the closure as the first argument
      [(App e e*)
       ;; When optimize known call is in effect any known closure
       ;; that is applied has the code label inlined
       ;; otherwise the closure pointer is extracted
       (do (bind-state : (State Nat C6-Expr))
           (e* : C6-Expr* <- (map-state cc-expr/env e*))
           (e : C6-Expr  <- (cc-expr/env e))
           (if (Var? e)
               (let ([code? : (Option C6-Expr)
                            (or (optimize-known-call?)
                                (hash-ref code-env (Var-id e) #f))])
                 (if code?
                     (return-state (App (cons code? e) e*))
                     (return-state (App (cons (Closure-code e) e) e*))))
               (do (bind-state : (State Nat C6-Expr))
                   (id : Uid <- (uid-state "tmp_clos"))
                   (let ([var (Var id)])
                    (return-state
                     (Let (list (cons id e))
                      (App (cons (Closure-code (Var id)) (Var id)) e*)))))))]
      ;; varibles that are free are extracted from the closure
      ;; while variable that are not bound by the closure are rebuilt
      [(Var u) (return-state (lookup data-env selfp u))]
      ;; The rest of the cases are just recursing into sub expressions
      [(Let b* e)
       (do (bind-state : (State Nat C6-Expr))
           (b* : BD*     <- (cc-bnd-data* cc-expr/env b*))
           (e  : C6-Expr <- (cc-expr/env e))
           (return-state (Let b* e)))]
      [(If t c a)
       (do (bind-state : (State Nat C6-Expr))
           (t : C6-Expr <- (cc-expr/env t))
           (c : C6-Expr <- (cc-expr/env c))
           (a : C6-Expr <- (cc-expr/env a))
           (return-state (If t c a)))]
      [(Op p e*)
       (do (bind-state : (State Nat C6-Expr))
           (e* : C6-Expr* <- (map-state cc-expr/env e*))
           (return-state (Op p e*)))]
      [(Quote k) (return-state (Quote k))]
      [(Tag t)   (return-state (Tag t))]
      ;; Observables Representation
      [(Blame e)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (return-state (Blame e)))]
      [(Observe e t)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (return-state (Observe e t)))]
      ;; Type Representation
      [(Type t) (return-state (Type t))]
      [(Type-tag e) (lift-state (inst Type-tag C6-Expr) (cc-expr/env e))]
      [(Type-GRef-to e)
       (do (bind-state : (State Nat C6-Expr))
           (e  : C6-Expr <- (cc-expr/env  e))
           (return-state (Type-GRef-to e)))]
      [(Type-Fn-arg e i)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (i : C6-Expr <- (cc-expr/env i))
           (return-state (Type-Fn-arg e i)))]
      [(Type-Fn-return e)
       (do (bind-state : (State Nat C6-Expr))
           (e  : C6-Expr <- (cc-expr/env  e))
           (return-state (Type-Fn-return e)))]
      [(Type-Fn-arity e)
       (do (bind-state : (State Nat C6-Expr))
           (e  : C6-Expr <- (cc-expr/env  e))
           (return-state (Type-Fn-arity e)))]
      ;; Dynamic Representation
      [(Dyn-tag e)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (return-state (Dyn-tag e)))]
      [(Dyn-immediate e)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (return-state (Dyn-immediate e)))]
      [(Dyn-type e)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (return-state (Dyn-type e)))]
      [(Dyn-value e)
       (do (bind-state : (State Nat C6-Expr))
           (e : C6-Expr <- (cc-expr/env e))
           (return-state (Dyn-value e)))]
      [(Dyn-make e1 e2)
       (do (bind-state : (State Nat C6-Expr))
           (e1 : C6-Expr <- (cc-expr/env e1))
           (e2 : C6-Expr <- (cc-expr/env e2))
           (return-state (Dyn-make e1 e2)))]
      ;; control flow for effects
      [(Begin e* e)
       (do (bind-state : (State Nat C6-Expr))
           (e* : C6-Expr* <- (map-state cc-expr/env e*))
           (e  : C6-Expr  <- (cc-expr/env  e))
           (return-state (Begin e* e)))]
      ;; Gaurded Representation
      [(GRep-proxied? e)
       (lift-state (inst GRep-proxied? C6-Expr) (cc-expr/env e))]
      [(UGbox e)
       (lift-state (inst UGbox C6-Expr) (cc-expr/env e))]
      [(UGbox-ref e)
       (lift-state (inst UGbox-ref C6-Expr) (cc-expr/env e))]
      [(UGbox-set! e1 e2)
       (do (bind-state : (State Nat C6-Expr))
           (e1 : C6-Expr <- (cc-expr/env e1))
           (e2 : C6-Expr <- (cc-expr/env e2))
           (return-state (UGbox-set! e1 e2)))]
      [(Gproxy e1 e2 e3 e4)
       (do (bind-state : (State Nat C6-Expr))
           (e1 : C6-Expr <- (cc-expr/env e1))
           (e2 : C6-Expr <- (cc-expr/env e2))
           (e3 : C6-Expr <- (cc-expr/env e3))
           (e4 : C6-Expr <- (cc-expr/env e4))
           (return-state (Gproxy e1 e2 e3 e4)))]
      [(Gproxy-for e)
       (lift-state (inst Gproxy-for C6-Expr) (cc-expr/env e))]
      [(Gproxy-from e)
       (lift-state (inst Gproxy-from C6-Expr) (cc-expr/env e))]
      [(Gproxy-to e)
       (lift-state (inst Gproxy-to C6-Expr) (cc-expr/env e))]
      [(Gproxy-blames e)
       (lift-state (inst Gproxy-blames C6-Expr) (cc-expr/env e))]))
  cc-expr/env)

;;TODO remove this continuation base return
(define-type LCont (BP* BC* Env Env -> (State Nat C6-Expr)))

(: cc-bnd-lambda* (Env Env SelfP C5-Bnd-Lambda*  ->
                       (State Nat (List BP* BC* Env Env))))
(define (cc-bnd-lambda* code-env data-env selfp b*)
  ;; the code labels
  (define cp* : Uid* (map (inst car Uid Any) b*))
  (define cl* : C6-Expr* (map (inst Code-Label Uid) cp*))
  (do (bind-state : (State Nat (List BP* BC* Env Env)))
      ;; the inner closure names
      (ic* : Uid* <- (map-state clos-uid cp*))
      ;; the outer closure names
      (oc* : Uid* <- (map-state clos-uid cp*))
      ;; extend environments with extra closure and code info
      (let* ([oc-var* (map (inst Var Uid) oc*)]
             [ic-env (extend* code-env ic* cl*)]
             [oc-env (extend* code-env oc* cl*)]
             [od-env (extend* data-env cp* oc-var*)]
             [ic.b*  (map (inst cons Uid C5-Bnd-Lambda) ic* b*)]
             [oc.b*  (map (inst cons Uid C5-Bnd-Lambda) oc* b*)])
        (bp* : BP* <- (map-state (cc-make-proc ic-env) ic.b*))
        (bd* : BC* <- (map-state (cc-make-clos oc-env od-env selfp) oc.b*))
        ;; call the continuation that will make the body of the let forms
        (return-state (list bp* bd* oc-env od-env)))))

(: cc-make-proc (Env -> ((Pairof Uid C5-Bnd-Lambda) -> (State Nat BP))))
(define (cc-make-proc code-env)
  (lambda ([b : (Pairof Uid C5-Bnd-Lambda)]) : (State Nat BP)
   (match-let ([(cons clos (cons code (Lambda fml* (Free ctr? uid* e)))) b])
      ;; even though the closure contains other data only free vars will be
      ;; found in the body.
      (let* ([clos-ref* (map (mk-clos-ref clos) uid*)]
             [data-env  (extend* (hash) uid* clos-ref*)]
             [self   (Var clos)]
             [selfp  (lambda ([u : Uid]) : (Option C6-Expr)
                       (and (eq? u code) self))])
        (do (bind-state : (State Nat BP))
            (e : C6-Expr <- ((cc-expr code-env data-env selfp) e))
            (return-state
             (cons code
                   (Procedure clos fml* code ctr? uid* e))))))))

(: mk-clos-ref : Uid -> (Uid -> C6-Expr))
(define (mk-clos-ref clos)
  (lambda ([fvar : Uid]) : C6-Expr
   (Closure-ref clos fvar)))


(: cc-make-clos : Env Env SelfP -> ((Pairof Uid C5-Bnd-Lambda) -> (State Nat BC)))
(define (cc-make-clos code-env data-env selfp)
  (lambda ([b : (Pairof Uid C5-Bnd-Lambda)]) : (State Nat BC)
    (match-let ([(cons clos (cons code (Lambda fml* (Free ctr? uid* _)))) b])
      (let* ([bound* (map (lookup-in data-env selfp) uid*)]
             [ctr    (get-caster-binding code-env data-env ctr?)]
             [data   (Closure-Data (Code-Label code) ctr bound*)])
        (return-state (cons clos data))))))

(: get-caster-binding : Env Env (Option Uid) -> C6-Expr)
(define (get-caster-binding c-env d-env ctr?)
  (cond
    [(not ctr?) (Quote #f)]
    [(use-code-casters?)
     (or (hash-ref c-env ctr? #f)
         (error 'convert-closures/get-caste-binding
                "no code found for ~a" ctr?))]
    [else (hash-ref d-env ctr? (thunk (Var ctr?)))]))

(: cc-bnd-data* ((C5-Expr -> (State Nat C6-Expr)) C5-Bnd-Data*
                 ->  (State Nat C6-Bnd-Data*)))
(define (cc-bnd-data* cc-expr bnd*)
  (map-state (cc-bnd-data cc-expr) bnd*))

(: cc-bnd-data ((C5-Expr -> (State Nat C6-Expr)) ->
                (C5-Bnd-Data -> (State Nat C6-Bnd-Data))))
(define ((cc-bnd-data cc-expr) bnd)
  (do (bind-state : (State Nat C6-Bnd-Data))
      (e : C6-Expr <- (cc-expr (cdr bnd)))
      (return-state (cons (car bnd) e))))

;; create a clos annotated unique name
(: clos-uid (-> Uid (State Nat Uid)))
(define (clos-uid u)
  (uid-state (string-append (Uid-prefix u) "_clos")))

(: lookup : Env SelfP Uid -> C6-Expr)
(define (lookup env selfp uid)
  ;; If optimize self reference is enabled then check
  ;; to see if this is a self reference.
  (or (and (optimize-self-reference?) (selfp uid))
      (hash-ref env uid (thunk (Var uid)))))

(: lookup-in : Env SelfP -> (Uid -> C6-Expr))
(define ((lookup-in env selfp) uid)
  (lookup env selfp uid))

(: extend* :  Env Uid* C6-Expr* -> Env)
(define (extend* env k* v*)
  (for/fold ([env : Env env])
            ([k : Uid k*] [v : C6-Expr v*])
    (hash-set env k v)))
