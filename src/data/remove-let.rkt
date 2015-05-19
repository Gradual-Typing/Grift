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
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)

;; Only the pass is provided by this module
(provide remove-let)

#;
(TODO this pass has a very wierd interface for effects.
      I think that at the time I was trying to be clever.
      This should be fixed to be more naive.
      A perhaps everything in the body should be in the state
      monad.)

(: remove-let (-> Data1-Lang Config Data2-Lang))
(trace-define (remove-let prgm comp-config)
  (match-let ([(Prog (list name count type) (Labels bnd-code* tail)) prgm])
    (let ([bnd-code* : D2-Bnd-Code* (rl-bnd-code* bnd-code*)]
          [body : D2-Body (rl-body tail)])
      (Prog (list name count type) (Labels bnd-code* body)))))


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
  (logf "rl-tail ~v\n\n" tail)
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
    [(Return exp)
     (let-values ([(exp lv*) (rl-value exp)])
       (values (Return exp) lv*))]
    [other (error 'remove-let/rl-tail "~a" other)]))

(: rl-value (-> D1-Value (values D2-Value Uid*)))
(define (rl-value exp)
  (logf "rl-value ~v\n\n" exp)
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
    [(App exp exp*)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(exp* lv*^) (rl-value* exp*)])
       (values (App exp exp*) (append lv* lv*^)))]
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
  (logf "rl-pred ~v\n\n" exp)
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
    [(App exp exp*)
     (let*-values ([(exp lv*) (rl-value exp)]
                   [(exp* lv*^) (rl-value* exp*)])
       (values (App exp exp*) (append lv* lv*^)))]
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

(: rl-effect (-> D1-Effect Uid* (values D2-Effect Uid*)))
(define (rl-effect eff lv*)
  (logf "rl-effect ~v\n\n" eff)
  (match eff
    [(Let bnd* exp)
     (let*-values ([(exp lv*)  (rl-effect exp lv*)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (logf "rl-effect/let\n stm* ~a\n exp ~a\n\n" stm* exp)
       (values (make-begin (append stm* (list exp)) NO-OP) lv*))]
    [(If t c a)
     (let*-values ([(t p-lv*) (rl-pred t)]
                   [(c lv*)   (rl-effect c lv*)]
                   [(a lv*) (rl-effect a lv*)])
         (values (If t c a) (append p-lv* lv*)))]
    [(Begin stm* _)
     (let*-values ([(stm* lv*) (rl-effect* stm* lv*)])
       (values (make-begin stm* NO-OP) lv*))]
    [(App exp exp*)
     (let*-values ([(exp lv*^)   (rl-value exp)]
                   [(exp* lv*^^) (rl-value* exp*)])
       (values (App exp exp*) (append lv*^ lv*^^ lv*)))]
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
