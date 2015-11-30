#lang typed/racket

(require "../language/data4.rkt"
         "../language/data5.rkt"
         "../language/make-begin.rkt"
         "../language/data-representation.rkt"
         "../configuration.rkt"
         "../helpers.rkt")

(provide simplify-predicates)
(: simplify-predicates (Data4-Lang Config -> Data5-Lang))
(define (simplify-predicates prog config)
  (match-let ([(Prog (list name count ty) (GlobDecs d* (Labels bnd* body))) prog])
    (let*-values ([(body count) (run-state (sp-body body) count)]
                  [(bnd* count) (run-state (map-state sp-bnd-code bnd*) count)])
      (Prog (list name count ty) (GlobDecs d* (Labels bnd* body))))))

;; Simple recursion into the body of code bindings
(: sp-bnd-code (D4-Bnd-Code -> (State Nat D5-Bnd-Code)))
(define (sp-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (do (bind-state : (State Nat D5-Bnd-Code))
        (b : D5-Body <- (sp-body b))
        (return-state (cons i (Code i* b))))))

(define-type SpSt (Pair Uid* Nat))

;; loc-state adds a new local to the state and returns it
(: new-uid-state (String -> (State SpSt Uid)))
(define (new-uid-state suffix)
  (do (bind-state : (State SpSt Uid))
      ((cons u* n) : SpSt <- get-state)
      (let* ([u (Uid suffix n)]
             [u* (cons u u*)]
             [n (add1 n)])
        (_ : Null <- (put-state (cons u* n)))
        (return-state u))))

;; Add new local variables to the declaration to each body
(: sp-body (D4-Body -> (State Nat D5-Body)))
(define (sp-body body)
  (match-let ([(Locals i* t) body])
    (do (bind-state : (State Nat D5-Body))
        (n : Nat <- get-state)
        (let-values ([(t l) (run-state (sp-tail t) `(() . ,n))])
          (match-let ([(cons i*^ n) l])
            (_ : Null <- (put-state n))
            (return-state (Locals (append i*^ i*) t)))))))

(: sp-tail (D4-Tail -> (State SpSt D5-Tail)))
(define (sp-tail tail)
  (match tail
    [(If t c a)
     (do (bind-state : (State SpSt D5-Tail))
         ((cons e* p) : (D5E D5-Pred) <- (sp-pred t))
         (c : D5-Tail <- (sp-tail c))
         (a : D5-Tail <- (sp-tail a))
         (return-state (make-begin e* (If p c a))))]
      [(Begin e* v)
       (do (bind-state : (State SpSt D5-Tail))
           (e* : D5-Effect* <- (sp-effect* e*))
           (t  : D5-Tail    <- (sp-tail v))
           (return-state (make-begin e* t)))]
    [(Return v)
     (if (Success? v)
         (return-state (Return (Success)))
         (do (bind-state : (State SpSt D5-Tail))
           (v : D5-Value <- (sp-value v))
           (return-state (Return v))))]))

;; (TODO remove relops and just use value in predicate context)
;; I think that having relops is not actually necissary

(: TRUE D5-Trivial)
(define TRUE (Quote TRUE-IMDT))
(: FALSE D5-Trivial)
(define FALSE (Quote FALSE-IMDT))

(define-type (D5E d) (Pair D5-Effect* d))

(: sp-pred (D4-Pred -> (State SpSt (D5E D5-Pred))))
(define (sp-pred pred)
  (match pred
    [(If t c a)
     (do (bind-state : (State SpSt (D5E D5-Pred)))
         (u1 : Uid <- (new-uid-state "pred_res"))
         ((cons te* t) : (D5E D5-Pred) <- (sp-pred t))
         ((cons ce* c) : (D5E D5-Pred) <- (sp-pred c))
         ((cons ae* a) : (D5E D5-Pred) <- (sp-pred a))
         (let* ([ce : D5-Effect (Assign u1 (If c TRUE FALSE))]
                [ae : D5-Effect (Assign u1 (If a TRUE FALSE))]
                [cb             (Begin (snoc ce* ce) NO-OP)]
                [ab             (Begin (snoc ae* ae) NO-OP)]
                [eif : D5-Effect  (If t cb ab)]
                [e*  : D5-Effect* (snoc te* eif)]
                [p   : D5-Pred (Relop '= (Var u1) TRUE)])
         (return-state (cons e* p))))]
      [(Begin e* p)
       (do (bind-state : (State SpSt (D5E D5-Pred)))
           (e* : D5-Effect* <- (sp-effect* e*))
           ((cons e*^ p)  : (D5E D5-Pred) <- (sp-pred p))
           (return-state (cons (append e* e*^) p)))]
      [(Relop p t1 t2)
       (do (bind-state : (State SpSt (D5E D5-Pred)))
           (t1 : D5-Trivial <- (sp-trivial t1))
           (t2 : D5-Trivial <- (sp-trivial t2))
           (return-state (cons '() (Relop p t1 t2))))]))

(: sp-value (D4-Value -> (State SpSt D5-Value)))
(define (sp-value v)
  (match v
    [(App-Code t t*)
     (do (bind-state : (State SpSt D5-Value))
         (t  : D5-Trivial  <- (sp-trivial  t))
         (t* : D5-Trivial* <- (sp-trivial* t*))
         (return-state (App-Code t t*)))]
    [(Op p t*)
     (do (bind-state : (State SpSt D5-Value))
         (t* : D5-Trivial* <- (sp-trivial* t*))
         (return-state (Op p t*)))]
    [(Halt) (return-state (Halt))]
    [(Var i) (return-state (Var i))]
    [(Code-Label i) (return-state (Code-Label i))]
    [(Quote k) (return-state (Quote k))]))

(: sp-effect (D4-Effect -> (State SpSt D5-Effect*)))
(define (sp-effect effect)
  (match effect
    [(Assign i v)
     (do (bind-state : (State SpSt D5-Effect*))
         (v : D5-Value <- (sp-value v))
       (return-state (list (Assign i v))))]
    [(Halt) (return-state (list (Halt)))]
    [(If t c a)
     (do (bind-state : (State SpSt D5-Effect*))
         ((cons e* t) : (D5E D5-Pred)   <- (sp-pred t))
         (c* : D5-Effect* <- (sp-effect c))
         (a* : D5-Effect* <- (sp-effect a))
         (return-state
          (snoc
           e*
           (If t (Begin c* NO-OP) (Begin a* NO-OP)))))]
    [(Begin e* _) (sp-effect* e*)]
    [(Repeat i t1 t2 e)
     (do (bind-state : (State SpSt D5-Effect*))
         (t1 : D5-Trivial <- (sp-trivial t1))
         (t2 : D5-Trivial <- (sp-trivial t2))
         (e* : D5-Effect* <- (sp-effect  e))
         (return-state (list (Repeat i t1 t2 (Begin e* NO-OP)))))]
    [(Op p t*)
     (do (bind-state : (State SpSt D5-Effect*))
         (t* : D5-Trivial* <- (sp-trivial* t*))
         (return-state (list (ann (Op p t*) D5-Effect))))]
    [(No-Op) (return-state (list NO-OP))]
    [other (error 'simplify-predicates/effect "~a" other)]))

(: sp-effect* (D4-Effect* -> (State SpSt D5-Effect*)))
(define (sp-effect* effect*)
  (do (bind-state : (State SpSt D5-Effect*))
      (e** : (Listof D5-Effect*) <- (map-state sp-effect effect*))
      (return-state (foldr (inst append D5-Effect) '() e**))))

(: sp-trivial (D4-Trivial -> (State SpSt D5-Trivial)))
(define (sp-trivial triv)
  (match triv
    [(Var u) (return-state (Var u))]
    [(Code-Label u) (return-state (Code-Label u))]
    [(Quote k) (return-state (Quote k))]))

(: sp-trivial* (D4-Trivial* -> (State SpSt D5-Trivial*)))
(define (sp-trivial* t*)
  (map-state sp-trivial t*))
