#lang typed/racket/base
#|------------------------------------------------------------------------------+
|Pass: src/data/remove-let                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass records local variables and should probably be placed
| after the flattening passes.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require racket/set
         racket/match
         "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/data1.rkt"
         "../language/data2.rkt"
         "../language/make-begin.rkt")

;; Only the pass is provided by this module
(provide uncover-locals
         (all-from-out
          "../language/data1.rkt"
          "../language/data2.rkt"))

#;
(TODO this pass has a very wierd interface for effects.
      I think that at the time I was trying to be clever.
      This should be fixed to be more naive.
      A perhaps everything in the body should be in the state
      monad.)

(: uncover-locals (-> Data1-Lang Data2-Lang))
(define (uncover-locals prgm)
  (match-let ([(Prog (list name count type) (GlobDecs d* (Labels bnd-code* tail))) prgm])
    (let* ([gd-set(list->set d*)]
           [bnd-code* : D2-Bnd-Code* (ul-bnd-code* gd-set bnd-code*)]
           [body : D2-Body (ul-body gd-set tail)])
      (Prog (list name count type) (GlobDecs d* (Labels bnd-code* body))))))


(: ul-bnd-code* (-> (Setof Uid) D1-Bnd-Code* D2-Bnd-Code*))
(define (ul-bnd-code* dec* bnd*) (map (ul-bnd-code dec*) bnd*))

(: ul-bnd-code ((Setof Uid) -> (D1-Bnd-Code -> D2-Bnd-Code)))
(define ((ul-bnd-code dec*) bnd)
  (match-let ([(cons uid (Code uid* exp)) bnd])
    (cons uid (Code uid* (ul-body dec* exp)))))

(: ul-body ((Setof Uid) D1-Tail -> D2-Body))
(define (ul-body gdecs tail)
  (let-values ([(tail^ vars) (ul-tail tail (set))])
    (Locals (set->list (set-subtract vars gdecs)) tail)))

(: ul-tail (-> D1-Tail (Setof Uid) (values D2-Tail (Setof Uid))))
(define (ul-tail tail lv*)
  (logging ul-tail (Vomit) "~v" tail)
  (match tail
    [(If t c a)
     (let*-values ([(t lv*) (ul-pred t lv*)]
                   [(c lv*) (ul-tail c lv*)]
                   [(a lv*) (ul-tail a lv*)])
       (values  (If t c a) lv*))]
    [(Begin stm* tail)
     (let*-values ([(stm* lv*)  (ul-effect* stm* lv*)]
                   [(tail lv*)  (ul-tail tail lv*)])
       (values (Begin stm* tail) lv*))]
    [(App-Code exp exp*)
     (let*-values ([(exp  lv*) (ul-value exp lv*)]
                   [(exp* lv*) (ul-value* exp* lv*)])
       (values (App-Code exp exp*) lv*))]
    [(Op p exp*)
     (let*-values ([(exp* lv*) (ul-value* exp* lv*)])
       (values (Op p exp*) lv*))]
    [(Var i) (values (Var i) lv*)]
    [(Code-Label i) (values (Code-Label i) lv*)]
    [(Quote k) (values (Quote k) lv*)]
    [(Halt) (values (Halt) lv*)]
    [(Success) (values (Success) lv*)]
    [other (error 'uncover-locals/ul-tail "~a" other)]))

(: ul-value (-> D1-Value (Setof Uid) (values D2-Value (Setof Uid))))
(define (ul-value exp lv*)
  (logging ul-value (Vomit) "~v" exp)
  (match exp
    [(If t c a)
     (let*-values ([(t lv*) (ul-pred t lv*)]
                   [(c lv*) (ul-value c lv*)]
                   [(a lv*) (ul-value a lv*)])
         (values (If t c a) lv*))]
    [(Begin stm* exp)
     (let*-values ([(exp lv*) (ul-value exp lv*)]
                   [(stm* lv*) (ul-effect* stm* lv*)])
       (values (Begin stm* exp) lv*))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*) (ul-value exp lv*)]
                   [(exp* lv*) (ul-value* exp* lv*)])
       (values (App-Code exp exp*) lv*))]
    [(Op p exp*)
     (let*-values ([(exp* lv*) (ul-value* exp* lv*)])
       (values (Op p exp*) lv*))]
    [(Halt) (values (Halt) lv*)]
    [(Var i) (values (Var i) lv*)]
    [(Code-Label i) (values (Code-Label i) lv*)]
    [(Quote k) (values (Quote k) lv*)]
    [other (error 'uncover-locals/ul-value "~a" other)]))

(: ul-pred (-> D1-Pred (Setof Uid) (values D2-Pred (Setof Uid))))
(define (ul-pred exp lv*)
  (logging ul-pred (Vomit) "~v" exp)
  (match exp
    [(If t c a)
     (let*-values ([(t lv*) (ul-pred t lv*)]
                   [(c lv*) (ul-pred c lv*)]
                   [(a lv*) (ul-pred a lv*)])
         (values (If t c a) lv*))]
    [(Begin stm* pred)
     (let*-values ([(pred lv*) (ul-pred pred lv*)]
                   [(stm* lv*) (ul-effect* stm* lv*)])
       (values (Begin stm* pred) lv*))]
    [(Relop p e1 e2)
     (let*-values ([(e1 lv*) (ul-value e1 lv*)]
                   [(e2 lv*) (ul-value e2 lv*)])
       (values (Relop p e1 e2) lv*))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*) (ul-value exp lv*)]
                   [(exp* lv*) (ul-value* exp* lv*)])
       (values (App-Code exp exp*) lv*))]
    [other (error 'uncover-locals/ul-pred "~a" other)]))

(: ul-value* (-> (Listof D1-Value) (Setof Uid) (values (Listof D2-Value) (Setof Uid))))
(define (ul-value* exp* lv*)
  (if (null? exp*)
      (values '() lv*)
      (let*-values ([(a d) (values (car exp*) (cdr exp*))]
                    [(e  lv*) (ul-value a lv*)]
                    [(e* lv*) (ul-value* d lv*)])
        (values (cons e e*) lv*))))

#|
(: ul-bnd (->  D1-Bnd Uid* (values D2-Effect (Setof Uid))))
(define (ul-bnd bnd lv*)
  (match-let ([(cons uid rhs) bnd])
    (let-values ([(rhs lv*^) (ul-value rhs)])
      (values (Assign uid rhs) (cons uid (append lv*^ lv*))))))

(: ul-bnd* (->  D1-Bnd* Uid* (values D2-Effect* (Setof Uid))))
(define (ul-bnd* bnd* lv*)
  (if (null? bnd*)
      (values bnd* lv*)
      (let*-values ([(a) (car bnd*)]
                    [(d) (cdr bnd*)]
                    [(a lv*) (ul-bnd a lv*)]
                    [(a* lv*) (ul-bnd* d lv*)])
        (values (cons a a*) lv*))))
|#

(: ul-effect (D1-Effect (Setof Uid) -> (values D2-Effect (Setof Uid))))
(define (ul-effect eff lv*)
  (match eff
    [(Assign u e1)
     (let-values ([(e2 lv*) (ul-value e1 lv*)])
       (values (Assign u e2) (set-add lv* u)))]
    [(Halt) (values (Halt) lv*)]
    [(If t c a)
     (let*-values ([(t lv*) (ul-pred t lv*)]
                   [(c lv*) (ul-effect c lv*)]
                   [(a lv*) (ul-effect a lv*)])
         (values (If t c a) lv*))]
    [(Begin stm* _)
     (let*-values ([(stm2* lv*) (ul-effect* stm* lv*)])
       (values (Begin stm2* NO-OP) lv*))]
    [(Repeat i e1 e2 #f #f e3)
     (let*-values ([(e1 lv*) (ul-value e1 lv*)]
                   [(e2 lv*) (ul-value e2 lv*)]
                   [(lv*)    (set-add lv* i)]
                   [(e3 lv*) (ul-effect e3 lv*)])
       (values (Repeat i e1 e2 #f #f e3) lv*))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*)   (ul-value exp lv*)]
                   [(exp* lv*) (ul-value* exp* lv*)])
       (values (App-Code exp exp*) lv*))]
    [(Op p exp*)
     (let*-values ([(exp* lv*) (ul-value* exp* lv*)])
      (values (Op p exp*) lv*))]
    [(No-Op) (values NO-OP lv*)]
    [other (error 'uncover-locals/ul-value "~a" other)]))

(: ul-effect* (-> D1-Effect* (Setof Uid) (values D2-Effect* (Setof Uid))))
(define (ul-effect* stm* lv*)
  (if (null? stm*)
      (values stm* lv*)
      (let*-values ([(a) (car stm*)]
                    [(d) (cdr stm*)]
                    [(a lv*) (ul-effect a lv*)]
                    [(a* lv*) (ul-effect* d lv*)])
        (values (cons a a*) lv*))))
