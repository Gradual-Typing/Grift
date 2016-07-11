#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/data/remove-let                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass records local variables and treats let bindings as
| their initialization
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/data1.rkt"
         "../language/data2.rkt"
         "../language/make-begin.rkt")

;; Only the pass is provided by this module
(provide remove-let
         (all-from-out
          "../language/data1.rkt"
          "../language/data2.rkt"))

#;
(TODO this pass has a very wierd interface for effects.
      I think that at the time I was trying to be clever.
      This should be fixed to be more naive.
      A perhaps everything in the body should be in the state
      monad.)

(: remove-let (-> Data1-Lang Data2-Lang))
(define (remove-let prgm)
  (match-let ([(Prog (list name count type) (GlobDecs d* (Labels bnd-code* tail))) prgm])
    (let ([bnd-code* : D2-Bnd-Code* (rl-bnd-code* bnd-code*)]
          [body : D2-Body (rl-body tail)])
      (Prog (list name count type) (GlobDecs d* (Labels bnd-code* body))))))


(: rl-bnd-code* (-> D1-Bnd-Code* D2-Bnd-Code*))
(define (rl-bnd-code* bnd*) (map rl-bnd-code bnd*))

(: rl-bnd-code (-> D1-Bnd-Code D2-Bnd-Code))
(define (rl-bnd-code bnd)
  (match-let ([(cons uid (Code uid* exp)) bnd])
    (cons uid (Code uid* (rl-body exp)))))

(: rl-body (-> D1-Tail D2-Body))
(define (rl-body tail)
  (let-values ([(tail local-vars) (rl-tail tail)])
    (Locals local-vars tail)))

(: rl-tail (-> D1-Tail (values D2-Tail Uid*)))
(define (rl-tail tail)
  (logging rl-tail (Vomit) "~v" tail)
  (match tail
    [(Let bnd* tail)
     (let*-values ([(tail lv*) (rl-tail tail)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (Begin stm* tail) lv*))]
    [(If t c a)
     (let*-values ([(t t-lv*) (rl-pred t)]
                   [(c c-lv*) (rl-tail c)]
                   [(a a-lv*) (rl-tail a)])
       (values  (If t c a) (append t-lv* c-lv* a-lv*)))]
    [(Begin stm* tail)
     (let*-values ([(tail lv*) (rl-tail tail)]
                   [(stm* lv*)  (rl-effect* stm* lv*)])
       (values (Begin stm* tail) lv*))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(exp* lv*^) (rl-value* exp*)])
       (values (App-Code exp exp*) (append lv* lv*^)))]
    [(Op p exp*)
     (let*-values ([(exp* lv*) (rl-value* exp*)])
       (values (Op p exp*) lv*))]
    [(Var i) (values (Var i) '())]
    [(Code-Label i) (values (Code-Label i) '())]
    [(Quote k) (values (Quote k) '())]
    [(Halt) (values (Halt) '())]
    [(Success) (values (Success) '())]
    [other (error 'remove-let/rl-tail "~a" other)]))

(: rl-value (-> D1-Value (values D2-Value Uid*)))
(define (rl-value exp)
  (logging rl-value (Vomit) "~v" exp)
  (match exp
    [(Let bnd* exp)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (Begin stm* exp) lv*))]
    [(If t c a)
     (let*-values ([(t t-lv*) (rl-pred t)]
                   [(c c-lv*) (rl-value c)]
                   [(a a-lv*) (rl-value a)])
         (values (If t c a) (append t-lv* c-lv* a-lv*)))]
    [(Begin stm* exp)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(stm* lv*) (rl-effect* stm* lv*)])
       (values (Begin stm* exp) lv*))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(exp* lv*^) (rl-value* exp*)])
       (values (App-Code exp exp*) (append lv* lv*^)))]
    [(Op p exp*)
     (let*-values ([(exp* lv*) (rl-value* exp*)])
       (values (Op p exp*) lv*))]
    [(Halt) (values (Halt) '())]
    [(Var i) (values (Var i) '())]
    [(Code-Label i) (values (Code-Label i) '())]
    [(Quote k) (values (Quote k) '())]
    [other (error 'remove-let/rl-value "~a" other)]))

(: rl-pred (-> D1-Pred (values D2-Pred Uid*)))
(define (rl-pred exp)
  (logging rl-pred (Vomit) "~v" exp)
  (match exp
    [(Let bnd* exp)
     (let*-values ([(pred lv*) (rl-pred exp)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (Begin stm* pred) lv*))]
    [(If t c a)
     (let*-values ([(t t-lv*) (rl-pred t)]
                   [(c c-lv*) (rl-pred c)]
                   [(a a-lv*) (rl-pred a)])
         (values (If t c a) (append t-lv* c-lv* a-lv*)))]
    [(Begin stm* pred)
     (let*-values ([(pred lv*) (rl-pred pred)]
                   [(stm* lv*) (rl-effect* stm* lv*)])
       (values (make-begin stm* pred) lv*))]
    [(Relop p e1 e2)
     (let-values ([(e1 e1-lv*) (rl-value e1)]
                  [(e2 e2-lv*) (rl-value e2)])
       (values (Relop p e1 e2) (append e1-lv* e2-lv*)))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(exp* lv*^) (rl-value* exp*)])
       (values (App-Code exp exp*) (append lv* lv*^)))]
    [other (error 'remove-let/rl-pred "~a" other)]))

(: rl-value* (-> (Listof D1-Value) (values (Listof D2-Value) Uid*)))
(define (rl-value* exp*)
  (if (null? exp*)
      (values '() '())
      (let*-values ([(a d) (values (car exp*) (cdr exp*))]
                    [(e e-lv*) (rl-value a)]
                    [(e* e*-lv*) (rl-value* d)])
        (values (cons e e*) (append e-lv* e*-lv*)))))

(: rl-bnd (->  D1-Bnd Uid* (values D2-Effect Uid*)))
(define (rl-bnd bnd lv*)
  (match-let ([(cons uid rhs) bnd])
    (let-values ([(rhs lv*^) (rl-value rhs)])
      (values (Assign uid rhs) (cons uid (append lv*^ lv*))))))

(: rl-bnd* (->  D1-Bnd* Uid* (values D2-Effect* Uid*)))
(define (rl-bnd* bnd* lv*)
  (if (null? bnd*)
      (values bnd* lv*)
      (let*-values ([(a) (car bnd*)]
                    [(d) (cdr bnd*)]
                    [(a lv*) (rl-bnd a lv*)]
                    [(a* lv*) (rl-bnd* d lv*)])
        (values (cons a a*) lv*))))

(: rl-effect (D1-Effect Uid* -> (values D2-Effect Uid*)))
(define (rl-effect eff lv*)
  (match eff
    [(Let bnd* e)
     (let*-values ([(e lv*)  (rl-effect e lv*)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (make-begin (append stm* (list e)) NO-OP) lv*))]
    [(Assign u e1)
     (let-values ([(e2 lv2*) (rl-value e1)])
       (values (Assign u e2) (append lv* lv2*)))]
    [(Halt) (values (Halt) lv*)]
    [(If t c a)
     (let*-values ([(t p-lv*) (rl-pred t)]
                   [(c lv*)   (rl-effect c lv*)]
                   [(a lv*)   (rl-effect a lv*)])
         (values (If t c a) (append p-lv* lv*)))]
    [(Begin stm* _)
     (let*-values ([(stm2* lv*) (rl-effect* stm* lv*)])
       (values (make-begin stm2* NO-OP) lv*))]
    [(Repeat i e1 e2 e3)
     (let*-values ([(e1 l1) (rl-value e1)]
                   [(e2 l2) (rl-value e2)]
                   [(e3 lv*) (rl-effect e3 lv*)])
       (values (Repeat i e1 e2 e3) (cons i (append l1 l2 lv*))))]
    [(App-Code exp exp*)
     (let*-values ([(exp lv*^)   (rl-value exp)]
                   [(exp* lv*^^) (rl-value* exp*)])
       (values (App-Code exp exp*) (append lv*^ lv*^^ lv*)))]
    [(Op p exp*)
     (let*-values ([(exp* lv*^) (rl-value* exp*)])
      (values (Op p exp*) (append lv*^ lv*)))]
    [(No-Op) (values NO-OP lv*)]
    [other (error 'remove-let/rl-value "~a" other)]))

(: rl-effect* (-> D1-Effect* Uid* (values D2-Effect* Uid*)))
(define (rl-effect* stm* lv*)
  (if (null? stm*)
      (values stm* lv*)
      (let*-values ([(a) (car stm*)]
                    [(d) (cdr stm*)]
                    [(a lv*) (rl-effect a lv*)]
                    [(a* lv*) (rl-effect* d lv*)])
        (values (cons a a*) lv*))))
