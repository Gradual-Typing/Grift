#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/normalize-contexts
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass create effect, and value contexts
+-------------------------------------------------------------------------------+
| Source Grammar : Cast3
| Target Grammar : Lambda0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")
;; Only the pass is provided by this module
(provide normalize-context)

(: normalize-context (Cast6-Lang Config . -> . Cast7-Lang))
(define (normalize-context prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let* ([next : (Box Nat) (box count)]
           [exp (expr->value next exp)]
           [next (unbox next)])
      (Prog (list name next type) exp))))

(: expr->value ((Boxof Natural) C6-Expr -> C7-Valur))
(define (expr->value next exp)
  (define (recur exp)
    (match exp
    [(If t c a)
     (lift-state (inst If C7-Value C7-Value C7-Value)
                 (nc-expr->value t) (nc-expr->value c) (nc-expr->value a))]
    [(LetP p* (LetC c* e))
     (do (bind-state : (State Nat C7-Value))
         (p* : C7-Bnd-Procedure* <- (nc-bnd-procedure* p*))
         (v  : C7-Value          <- (nc-expr->value e))
         (return-state (LetP p* (LetC c* v))))]
    [(Let b* e)
     (lift-state (inst Let C7-Bnd-Data* C7-Value) (nc-bnd-data* b*) (nc-expr->value e))]
    [(App (cons e e^) e*)
     (do (bind-state : (State Nat C7-Value))
         (v  : C7-Value  <- (nc-expr->value  e))
         (v^ : C7-Value  <- (nc-expr->value  e^))
         (v* : C7-Value* <- (nc-expr->value* e*))
         (return-state (App (cons v v^) v*)))]
    [(Op p e*)
     (do (bind-state : (State Natural C7-Value))
         (e* : C7-Value* <- (nc-expr->value* e*))
         (return-state (Op p e*)))]
    [(Var i) (return-state (Var i))]
    [(Quote k) (return-state (Quote k))]
    [(Tag t)  (return-state (Tag t))]
    ;; Function Representation
    [(Fn-Caster e) (lift-state (inst Fn-Caster C7-Value) (nc-expr->value e))]
    ;; Type Representation
    [(Type t) (return-state (Type t))]
    [(Type-tag e) (lift-state (inst Type-tag C7-Value) (nc-expr->value e))]
    [(Type-Fn-arity e) (lift-state (inst Type-Fn-arity C7-Value) (nc-expr->value e))]
    [(Type-Fn-arg e1 e2) (lift-state (inst Type-Fn-arg C7-Value C7-Value)
                                     (nc-expr->value e1) (nc-expr->value e2))]
    [(Type-Fn-return e) (lift-state (inst Type-Fn-return C7-Value) (nc-expr->value e))]
    [(Dyn-tag e)  (lift-state (inst Dyn-tag C7-Value) (nc-expr->value e))]
    [(Dyn-immediate e) (lift-state (inst Dyn-immediate C7-Value) (nc-expr->value e))]
    [(Dyn-type e) (lift-state (inst Dyn-type C7-Value) (nc-expr->value e))]
    [(Dyn-value e) (lift-state (inst Dyn-value C7-Value) (nc-expr->value e))]
    [(Dyn-make e1 e2)
     (lift-state (inst Dyn-make C7-Value C7-Value) (nc-expr->value e1) (nc-expr->value e2))]
    [(Blame e) (lift-state (inst Blame C7-Value) (nc-expr->value e))]
    [(Observe e t)
     (do (bind-state : (State Natural C7-Value))
         (e : C7-Value <- (nc-expr->value e))
         (return-state (Observe e t)))]
    [(Begin e* e)
     (lift-state make-begin (nc-expr->effect* e*) (nc-expr->value e))]
    [(UGbox e)  (lift-state (inst UGbox C7-Value) (nc-expr->value e))]
    [(UGbox-ref e) (lift-state (inst UGbox-ref C7-Value) (nc-expr->value e))]
    [(UGbox-set! e1 e2) (TODO handle this case with more grace)]
    [(GRep-proxied? e)
     (lift-state (inst GRep-proxied? C7-Value) (nc-expr->value e))]
    [(Gproxy for from to blames)
     (do (bind-state : (State Natural C7-Value))
         (for    : C7-Value <- (nc-expr->value for))
         (from   : C7-Value <- (nc-expr->value from))
         (to     : C7-Value <- (nc-expr->value to))
         (blames : C7-Value <- (nc-expr->value blames))
         (return-state (Gproxy for from to blames)))]
    [(Gproxy-for e)
     (lift-state (inst Gproxy-for C7-Value) (nc-expr->value e))]
    [(Gproxy-from e)
     (lift-state (inst Gproxy-from C7-Value) (nc-expr->value e))]
    [(Gproxy-to e)
     (lift-state (inst Gproxy-to C7-Value) (nc-expr->value e))]
    [(Gproxy-blames e)
     (lift-state (inst Gproxy-blames C7-Value) (nc-expr->value e))]))
  (recur exp))

(: nc-expr->value* (-> C6-Expr* (State Natural C7-Value*)))
(define (nc-expr->value* e*) (map-state nc-expr->value e*))

(: nc-bnd-data* (-> C6-Bnd-Data* (State Natural C7-Bnd-Data*)))
(define (nc-bnd-data* b*) (map-state nc-bnd-data b*))

(: nc-bnd-data (-> C6-Bnd-Data (State Natural C7-Bnd-Data)))
(define (nc-bnd-data b)
  (do (bind-state : (State Natural C7-Bnd-Data))
      (d : C7-Value <- (nc-expr->value (cdr b)))
      (return-state (cons (car b) d))))

(: nc-bnd-procedure* (-> C6-Bnd-Procedure* (State Nat C7-Bnd-Procedure*)))
(define (nc-bnd-procedure* p*) (map-state nc-bnd-procedure p*))

(: nc-bnd-procedure (-> C6-Bnd-Procedure (State Nat C7-Bnd-Procedure)))
(define (nc-bnd-procedure p)
  (match-let ([(cons id (Procedure s f* c? b* e)) p])
    (do (bind-state : (State Nat C7-Bnd-Procedure))
        (e : C7-Value <- (nc-expr->value e))
        (return-state (cons id (Procedure s f* c? b* e))))))



(: nc-expr->effect* (-> C6-Expr* (State Natural C7-Effect*)))
(define (nc-expr->effect* eff*) (map-state nc-expr->effect eff*))

(: nc-expr->effect (-> C6-Expr (State Natural C7-Effect)))
(define (nc-expr->effect eff)
  (match eff
    [(UGbox-set! e1 e2)
     (lift-state (inst UGbox-set! C7-Value C7-Value)
                 (nc-expr->value e1) (nc-expr->value e2))]
    [(Blame e)  (TODO thread a tail-state through so that this cannot happen)]
    [(Observe e t) (TODO get rid of this form)]
    ;; Necisarry Forms that are not easily proved non-effectfull
    [(If t c a)
     (lift-state (inst If C7-Value C7-Effect C7-Effect)
                 (nc-expr->value t) (nc-expr->effect c) (nc-expr->effect a))]
    [(LetP p* (LetC c* e))
     (do (bind-state : (State Nat C7-Effect))
         (p* : C7-Bnd-Procedure* <- (nc-bnd-procedure* p*))
         (e  : C7-Effect         <- (nc-expr->effect e))
         (return-state (LetP p* (LetC c* e))))]
    [(Let b* e)
     (lift-state (inst Let C7-Bnd-Data* C7-Effect) (nc-bnd-data* b*) (nc-expr->effect e))]
    [(App (cons e e^) e*)
     (do (bind-state : (State Nat C7-Effect))
         (v  : C7-Value  <- (nc-expr->value  e))
         (v^ : C7-Value  <- (nc-expr->value  e^))
         (v* : C7-Value* <- (nc-expr->value* e*))
         (return-state (App (cons v v^) v*)))]
    ;; Can be eliminated if subforms are eliminated as not having effects
    ;; Otherwise force them into value context and leave behind a NOP
    [(Begin e* e)
     (do (bind-state : (State Natural C7-Effect))
         (e* : C7-Effect* <- (nc-expr->effect* e*))
         (e  : C7-Effect  <- (nc-expr->effect e))
         (return-state (make-begin (append e* (list e)) NO-OP)))]
    [(Op p e*) (do (bind-state : (State Natural C7-Effect))
                   (e* : C7-Effect* <- (nc-expr->effect* e*))
                   (return-state (make-begin e* NO-OP)))]
    [(Dyn-make e1 e2)
     (do (bind-state : (State Natural C7-Effect))
         (e1 : C7-Effect <- (nc-expr->effect e1))
         (e2 : C7-Effect <- (nc-expr->effect e2))
         (return-state (make-begin (list e1 e2) NO-OP)))]
    [(Gproxy e1 e2 e3 e4)
     (do (bind-state : (State Natural C7-Effect))
         (e1 : C7-Effect <- (nc-expr->effect e1))
         (e2 : C7-Effect <- (nc-expr->effect e2))
         (e3 : C7-Effect <- (nc-expr->effect e3))
         (e4 : C7-Effect <- (nc-expr->effect e4))
         (return-state (make-begin (list e1 e2 e3 e4) NO-OP)))]
    [(Fn-Caster e) (nc-expr->effect e)]
    [(Type-Fn-ref e _) (nc-expr->effect e)]
    [(Type-Fn-ref e _) (nc-expr->effect e)]
    [(Type-tag e) (nc-expr->effect e)]
    [(Dyn-tag e) (nc-expr->effect e)]
    [(Dyn-immediate e) (nc-expr->effect e)]
    [(Dyn-value e) (nc-expr->effect e)]
    [(Dyn-type e) (nc-expr->effect e)]
    [(UGbox e) (nc-expr->effect e)]
    [(UGbox-ref e) (nc-expr->effect e)]
    [(GRep-proxied? e) (nc-expr->effect e)]
    [(Gproxy-for e) (nc-expr->effect e)]
    [(Gproxy-from e) (nc-expr->effect e)]
    [(Gproxy-to e) (nc-expr->effect e)]
    [(Gproxy-blames e) (nc-expr->effect e)]
    ;; Can always be eliminated because they only result in a value
    [(or (Lambda _ _) (Var _) (Quote _) (Type _) (Tag _))
     (return-state NO-OP)]))

(: nc-procedure (-> C6-Procedure (State Nat C7-Procedure)))
(define (nc-procedure p)
  (match-let ([(Procedure self bv* c? fv e) p])
    (do (bind-state : (State Nat C7-Procedure))
        (e : C7-Value <- (nc-expr->value e))
        (return-state (Procedure self bv* c? fv e)))))
