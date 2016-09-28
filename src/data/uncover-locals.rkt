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
  (Locals (set->list (set-subtract (ul-tail tail (set)) gdecs)) tail))

(: fold-switch-case (All (A B) (A B -> B) -> ((Switch-Case A) B -> B)))
(define ((fold-switch-case f) c a)
  (match-let ([(cons l r) c])
    (f r a)))
(: foldr-switch-case* (All (A B) (A B -> B) B (Switch-Case* A) -> B))
(define (foldr-switch-case* f b c*)
  (foldr (fold-switch-case f) b c*))

(: ul-tail (D1-Tail (Setof Uid) -> (Setof Uid)))
(define (ul-tail tail lv*)
  (logging ul-tail (Vomit) "~v" tail)
  (match tail
    [(If t c a)
     (let*-values ([(lv*) (ul-pred t lv*)]
                   [(lv*) (ul-tail c lv*)]
                   [(lv*) (ul-tail a lv*)])
       lv*)]
    [(Switch e c* d)
     (ul-value e (ul-tail d (foldr-switch-case* ul-tail lv* c*)))]
    [(Begin stm* tail)
     (let*-values ([(lv*) (ul-effect* stm* lv*)]
                   [(lv*)  (ul-tail tail lv*)])
       lv*)]
    [(App-Code exp exp*)
     (let*-values ([(lv*) (ul-value exp lv*)]
                   [(lv*) (ul-value* exp* lv*)])
       lv*)]
    [(Op p exp*) (ul-value* exp* lv*)]
    [other lv*]))

(: ul-value (D1-Value (Setof Uid) -> (Setof Uid)))
(define (ul-value exp lv*)
  (logging ul-value (Vomit) "~v" exp)
  (match exp
    [(If t c a)
     (let*-values ([(lv*) (ul-pred t lv*)]
                   [(lv*) (ul-value c lv*)]
                   [(lv*) (ul-value a lv*)])
       lv*)]
    [(Switch e c* d)
     (ul-value e (ul-value d (foldr-switch-case* ul-value lv* c*)))]
    [(Begin stm* exp)
     (let*-values ([(lv*) (ul-value exp lv*)]
                   [(lv*) (ul-effect* stm* lv*)])
       lv*)]
    [(App-Code exp exp*)
     (let*-values ([(lv*) (ul-value exp lv*)]
                   [(lv*) (ul-value* exp* lv*)])
       lv*)]
    [(Op p exp*)
     (let*-values ([(lv*) (ul-value* exp* lv*)])
       lv*)]
    [other lv*]))

(: ul-pred (D1-Pred (Setof Uid) -> (Setof Uid)))
(define (ul-pred exp lv*)
  (logging ul-pred (Vomit) "~v" exp)
  (match exp
    [(If t c a)
     (let*-values ([(lv*) (ul-pred t lv*)]
                   [(lv*) (ul-pred c lv*)]
                   [(lv*) (ul-pred a lv*)])
       lv*)]
    [(Switch e c* d)
     (ul-value e (ul-pred d (foldr-switch-case* ul-pred lv* c*)))]
    [(Begin stm* pred)
     (let*-values ([(lv*) (ul-pred pred lv*)]
                   [(lv*) (ul-effect* stm* lv*)])
       lv*)]
    [(Relop p e1 e2)
     (let*-values ([(lv*) (ul-value e1 lv*)]
                   [(lv*) (ul-value e2 lv*)])
       lv*)]
    [(App-Code exp exp*)
     (let*-values ([(lv*) (ul-value exp lv*)]
                   [(lv*) (ul-value* exp* lv*)])
       lv*)]
    [other (error 'uncover-locals/ul-pred "~a" other)]))

(: ul-value* (-> (Listof D1-Value) (Setof Uid) (Setof Uid)))
(define (ul-value* exp* lv*) (foldl ul-value lv* exp*))

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

(: ul-effect (D1-Effect (Setof Uid) ->  (Setof Uid)))
(define (ul-effect eff lv*)
  (match eff
    [(Assign u e1) (set-add (ul-value e1 lv*) u)]
    [(If t c a)
     (let*-values ([(lv*) (ul-pred t lv*)]
                   [(lv*) (ul-effect c lv*)]
                   [(lv*) (ul-effect a lv*)])
       lv*)]
    [(Switch e c* d)
     (ul-value e (ul-effect d (foldr-switch-case* ul-effect lv* c*)))]
    [(Begin stm* _) (ul-effect* stm* lv*)]
    [(Repeat i e1 e2 #f #f e3)
     (let*-values ([(lv*) (ul-value e1 lv*)]
                   [(lv*) (ul-value e2 lv*)]
                   [(lv*)    (set-add lv* i)]
                   [(lv*) (ul-effect e3 lv*)])
       lv*)]
    [(App-Code exp exp*)
     (let*-values ([(lv*)   (ul-value exp lv*)]
                   [(lv*) (ul-value* exp* lv*)])
       lv*)]
    [(Op p exp*) (ul-value* exp* lv*)]
    [other lv*]))

(: ul-effect* (D1-Effect* (Setof Uid) -> (Setof Uid)))
(define (ul-effect* stm* lv*) (foldl ul-effect lv* stm*))
