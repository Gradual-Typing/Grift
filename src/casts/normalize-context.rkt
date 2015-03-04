#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/normalize-contexts
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description:
+-------------------------------------------------------------------------------+
| Source Grammar : Cast3
| Target Grammar : Lambda0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/helpers schml/src/errors schml/src/language)
;; Only the pass is provided by this module
(provide normalize-context)

(: normalize-context (Cast3-Lang Config . -> . Cast4-Lang))
(define (normalize-context prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (run-state (nc-value exp) count)])
      (Prog (list name count type) exp))))

(: nc-value (-> C3-Expr (State Natural C4-Value)))
(define (nc-value exp)
  (match exp
    [(Lambda f* (Castable ctr exp))
     (do (bind-state : (State Natural C4-Value))
         (exp : C4-Value <- (nc-value exp))
         (return-state (Lambda f* (Castable ctr exp))))]
    [(If t c a)
     (lift-state (inst If C4-Value C4-Value C4-Value)
                 (nc-value t) (nc-value c) (nc-value a))]
    [(Letrec b* e)
     (lift-state (inst Letrec C4-Bnd* C4-Value) (nc-bnd* b*) (nc-value e))]
    [(Let b* e)
     (lift-state (inst Let C4-Bnd* C4-Value) (nc-bnd* b*) (nc-value e))]
    [(App e e*)
     (lift-state (inst App C4-Value C4-Value*) (nc-value e) (nc-value* e*))]
    [(Op p e*)
     (do (bind-state : (State Natural C4-Value))
         (e* : C4-Value* <- (nc-value* e*))
         (return-state (Op p e*)))]
    [(Fn-Caster e) (lift-state (inst Fn-Caster C4-Value) (nc-value e))]
    [(Var i) (return-state (Var i))]
    [(Quote k) (return-state (Quote k))]
    [(Type-Fn-arity e) (lift-state (inst Type-Fn-arity C4-Value) (nc-value e))]
    [(Type-Fn-arg e1 e2) (lift-state (inst Type-Fn-arg C4-Value C4-Value)
                                     (nc-value e1) (nc-value e2))]
    [(Type-Fn-return e) (lift-state (inst Type-Fn-return C4-Value) (nc-value e))]
    [(Type-tag e) (lift-state (inst Type-tag C4-Value) (nc-value e))]
    [(Dyn-tag e)  (lift-state (inst Dyn-tag C4-Value) (nc-value e))]
    [(Dyn-immediate e) (lift-state (inst Dyn-immediate C4-Value) (nc-value e))]
    [(Dyn-type e) (lift-state (inst Dyn-type C4-Value) (nc-value e))]
    [(Dyn-value e) (lift-state (inst Dyn-value C4-Value) (nc-value e))]
    [(Dyn-make e1 e2)
     (lift-state (inst Dyn-make C4-Value C4-Value) (nc-value e1) (nc-value e2))]
    [(Blame e) (lift-state (inst Blame C4-Value) (nc-value e))]
    [(Observe e t)
     (do (bind-state : (State Natural C4-Value))
         (e : C4-Value <- (nc-value e))
         (return-state (Observe e t)))]
    [(Type t) (return-state (Type t))]
    [(Tag t)  (return-state (Tag t))]
    [(Begin e* e)
     (lift-state make-begin (nc-effect* e*) (nc-value e))]
    [(UGbox e)  (lift-state (inst UGbox C4-Value) (nc-value e))]
    [(UGbox-ref e) (lift-state (inst UGbox-ref C4-Value) (nc-value e))]
    [(UGbox-set! e1 e2) (TODO handle this case with more grace)]
    [(GRep-proxied? e)
     (lift-state (inst GRep-proxied? C4-Value) (nc-value e))]
    [(Gproxy for from to blames)
     (do (bind-state : (State Natural C4-Value))
         (for    : C4-Value <- (nc-value for))
         (from   : C4-Value <- (nc-value from))
         (to     : C4-Value <- (nc-value to))
         (blames : C4-Value <- (nc-value blames))
         (return-state (Gproxy for from to blames)))]
    [(Gproxy-for e)
     (lift-state (inst Gproxy-for C4-Value) (nc-value e))]
    [(Gproxy-from e)
     (lift-state (inst Gproxy-from C4-Value) (nc-value e))]
    [(Gproxy-to e)
     (lift-state (inst Gproxy-to C4-Value) (nc-value e))]
    [(Gproxy-blames e)
     (lift-state (inst Gproxy-blames C4-Value) (nc-value e))]))

(: nc-value* (-> C3-Expr* (State Natural C4-Value*)))
(define (nc-value* e*) (map-state nc-value e*))

(: nc-bnd* (-> C3-Bnd* (State Natural C4-Bnd*)))
(define (nc-bnd* b*) (map-state nc-bnd b*))

(: nc-bnd (-> C3-Bnd (State Natural C4-Bnd)))
(define (nc-bnd b)
  (do (bind-state : (State Natural C4-Bnd))
      (d : C4-Value <- (nc-value (cdr b)))
      (return-state (cons (car b) d))))

;; Their are a few assumption that are being made here that
;; ends up assuming that make begin is called at every sight
;; where a begin in constructed in this recursion tree.

(: make-begin
   (case->
    (-> C4-Effect* C4-Value  C4-Value)
    (-> C4-Effect* No-Op C4-Effect)))
(define (make-begin eff* res)
  (: splice-eff (-> C4-Effect C4-Effect* C4-Effect*))
  (define (splice-eff eff rest)
    (cond
      [(No-Op? eff) rest]
      [(Begin? eff) (append (ann (Begin-effects eff) C4-Effect*) rest)]
      [else (cons eff rest)]))
  (let ([eff* (foldl splice-eff '() eff*)])
    (cond
      [(null? eff*) res]
      [(Begin? res) (Begin (append eff* (Begin-effects res))
                           (Begin-value res))]
      [(and (No-Op? res) (null? (cdr eff*))) (car eff*)]
      [else (Begin eff* res)])))


(: nc-effect* (-> C3-Expr* (State Natural C4-Effect*)))
(define (nc-effect* eff*) (map-state nc-effect eff*))

(: nc-effect (-> C3-Expr (State Natural C4-Effect)))
(define (nc-effect eff)
  (match eff
    [(UGbox-set! e1 e2)
     (lift-state (inst UGbox-set! C4-Value C4-Value)
                 (nc-value e1) (nc-value e2))]
    [(Blame e)  (TODO thread a tail-state through so that this cannot happen)]
    [(Observe e t) (TODO get rid of this form)]
    ;; Necisarry Forms that are not easily proved non-effectfull
    [(If t c a)
     (lift-state (inst If C4-Value C4-Effect C4-Effect)
                 (nc-value t) (nc-effect c) (nc-effect a))]
    [(Letrec b* e)
     (lift-state (inst Letrec C4-Bnd* C4-Effect) (nc-bnd* b*) (nc-effect e))]
    [(Let b* e)
     (lift-state (inst Let C4-Bnd* C4-Effect) (nc-bnd* b*) (nc-effect e))]
    [(App e e*)
     (lift-state (inst App C4-Value C4-Value*) (nc-value e) (nc-value* e*))]
    ;; Can be eliminated if subforms are eliminated as not having effects
    ;; Otherwise force them into value context and leave behind a NOP

    [(Begin e* e)
     (do (bind-state : (State Natural C4-Effect))
         (e* : C4-Effect* <- (nc-effect* e*))
         (e  : C4-Effect  <- (nc-effect e))
         (return-state (make-begin (append e* (list e)) NO-OP)))]
    [(Op p e*) (do (bind-state : (State Natural C4-Effect))
                   (e* : C4-Effect* <- (nc-effect* e*))
                 (return-state (make-begin e* NO-OP)))]
    [(Dyn-make e1 e2)
     (do (bind-state : (State Natural C4-Effect))
         (e1 : C4-Effect <- (nc-effect e1))
       (e2 : C4-Effect <- (nc-effect e2))
       (return-state (make-begin (list e1 e2) NO-OP)))]
    [(Gproxy e1 e2 e3 e4)
     (do (bind-state : (State Natural C4-Effect))
         (e1 : C4-Effect <- (nc-effect e1))
         (e2 : C4-Effect <- (nc-effect e2))
         (e3 : C4-Effect <- (nc-effect e3))
         (e4 : C4-Effect <- (nc-effect e4))
         (return-state (make-begin (list e1 e2 e3 e4) NO-OP)))]
    [(Fn-Caster e) (nc-effect e)]
    [(Type-Fn-ref e _) (nc-effect e)]
    [(Type-Fn-ref e _) (nc-effect e)]
    [(Type-tag e) (nc-effect e)]
    [(Dyn-tag e) (nc-effect e)]
    [(Dyn-immediate e) (nc-effect e)]
    [(Dyn-value e) (nc-effect e)]
    [(Dyn-type e) (nc-effect e)]
    [(UGbox e) (nc-effect e)]
    [(UGbox-ref e) (nc-effect e)]
    [(GRep-proxied? e) (nc-effect e)]
    [(Gproxy-for e) (nc-effect e)]
    [(Gproxy-from e) (nc-effect e)]
    [(Gproxy-to e) (nc-effect e)]
    [(Gproxy-blames e) (nc-effect e)]
    ;; Can always be eliminated because they only result in a value
    [(or (Lambda _ _) (Var _) (Quote _) (Type _) (Tag _))
     (return-state NO-OP)]))
