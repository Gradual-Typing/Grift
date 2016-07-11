#lang typed/racket
#|
 A little bit of clarification needs to be made in this pass
 This pass is really about flattening the value context. So
 that all begins and if are not allowed to occur there.

 Remove complex opera is about creating a trivial context
 where you known that there is an immediate aviailable without
 further evaluation.

 Simplify predicates is about getting control flow out of predicate
 contexts.

 In total these three passes force all computation to occur in
 the tail context. This context might be innapropriately named
 becuse it is actually more of a stmt contexts itself.

|#


(require "../language/data3.rkt"
         "../language/data4.rkt"
         "../language/make-begin.rkt"
         "../configuration.rkt"
         "../helpers.rkt")

(provide flatten-values)
(: flatten-values (Data3-Lang -> Data4-Lang))
(define (flatten-values prog)
  (match-let ([(Prog (list name count ty) (GlobDecs d* (Labels bnd* body))) prog])
    (let*-values ([(body count) (run-state (fv-body body) count)]
                  [(bnd* count) (run-state (map-state fv-bnd-code bnd*) count)])
      (Prog (list name count ty) (GlobDecs d* (Labels bnd* body))))))

;; Simple recursion into the body of code bindings
(: fv-bnd-code (D3-Bnd-Code -> (State Nat D4-Bnd-Code)))
(define (fv-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (do (bind-state : (State Nat D4-Bnd-Code))
        (b : D4-Body <- (fv-body b))
        (return-state (cons i (Code i* b))))))

;; The state macro monad in the body holds a list of
;; allocated names
(define-type Locs (Pair Uid* Nat))

;; loc-state adds a new local to the state and returns it
(: loc-state (String -> (State Locs Uid)))
(define ((loc-state suffix) state)
  (match-let ([(cons u* n) state])
    (let ([u (Uid suffix n)])
      (values u (cons (cons u u*) (add1 n))))))

;; Add new local variables to the declaration to each body
(: fv-body (D3-Body -> (State Nat D4-Body)))
(define (fv-body body)
  (match-let ([(Locals i* t) body])
    (do (bind-state : (State Nat D4-Body))
        (n : Nat <- get-state)
        (let-values ([(t l) (run-state (fv-tail t) `(() . ,n))])
          (match-let ([(cons i*^ n) l])
            (_ : Null <- (put-state n))
            (return-state (Locals (append i*^ i*) t)))))))

(define-type (E* t) (Pair D4-Effect* t))

(: fv-tail (D3-Tail -> (State Locs D4-Tail)))
(define (fv-tail tail)
  (logging fv-tail (Vomit) "~v" tail)
  (match tail
    [(If t c a)
     (do (bind-state : (State Locs D4-Tail))
         (t : D4-Pred <- (fv-pred t))
         (c : D4-Tail <- (fv-tail c))
         (a : D4-Tail <- (fv-tail a))
         (return-state (If t c a)))]
    [(Begin e* v)
     (do (bind-state : (State Locs D4-Tail))
         (e* : D4-Effect* <- (fv-effect* e*))
       (t  : D4-Tail <- (fv-tail v))
       (return-state (make-begin e* t)))]
    [(Return v)
     (if (Success? v)
         (return-state (Return (Success)))
         (do (bind-state  : (State Locs D4-Tail))
             ((cons e* v) : (E* D4-Value) <- (fv-value v))
             (return-state (make-begin e* (Return v)))))]))

(: fv-value (D3-Value -> (State Locs (E* D4-Value))))
(define (fv-value v)
  (logging fv-value (Vomit) "~v" v)
  (match v
    [(If t c a)
     (do (bind-state : (State Locs (E* D4-Value)))
         (t  : D4-Pred  <- (fv-pred t))
         ((cons c* c) : (E* D4-Value) <- (fv-value c))
         ((cons a* a) : (E* D4-Value) <- (fv-value a))
         (id : Uid <- (loc-state "ifValue"))
         (let* ([ca : D4-Effect (Assign id c)]
                [aa : D4-Effect (Assign id a)]
                [cb : D4-Effect (make-begin (snoc c* ca) NO-OP)]
                [ab : D4-Effect (make-begin (snoc a* aa) NO-OP)]
                [e  : D4-Effect (If t cb cb)]
                [v  : D4-Value  (Var id)])
           (return-state (cons (list e) v))))]
    [(Begin e* v)
     (do (bind-state : (State Locs (E* D4-Value)))
         (e* : D4-Effect* <- (fv-effect* e*))
         ((cons e*^ v) : (E* D4-Value) <- (fv-value v))
         (return-state (cons (append e* e*^) v)))]
    [(App-Code t t*)
     (do (bind-state : (State Locs (E* D4-Value)))
         (t  : D4-Trivial  <- (fv-trivial  t))
         (t* : D4-Trivial* <- (fv-trivial* t*))
         (let ([v : D4-Value (App-Code t t*)])
           (return-state (cons '() v))))]
    [(Op p t*)
     (do (bind-state : (State Locs (E* D4-Value)))
         (t* : D4-Trivial* <- (fv-trivial* t*))
         (let ([v : D4-Value (Op p t*)])
           (return-state (cons '() v))))]
    [(Halt) (return-state (cons '() (Halt)))]
    [(Var i) (return-state (cons '() (Var i)))]
    [(Quote k) (return-state (cons '() (Quote k)))]))


(: fv-pred (D3-Pred -> (State Locs D4-Pred)))
(define (fv-pred pred)
  (logging fv-pred (Vomit) "~v" pred)
  (match pred
    [(If t c a)
     (do (bind-state : (State Locs D4-Pred))
         (t : D4-Pred <- (fv-pred t))
         (c : D4-Pred <- (fv-pred c))
         (a : D4-Pred <- (fv-pred a))
         (return-state (If t c a)))]
      [(Begin e* p)
       (do (bind-state : (State Locs D4-Pred))
           (e* : D4-Effect* <- (fv-effect* e*))
           (p  : D4-Pred    <- (fv-pred p))
           (return-state (Begin e* p)))]
      [(Relop p t1 t2)
       (do (bind-state : (State Locs D4-Pred))
           (t1 : D4-Trivial <- (fv-trivial t1))
           (t2 : D4-Trivial <- (fv-trivial t2))
           (return-state (Relop p t1 t2)))]))

(: fv-effect (D3-Effect -> (State Locs D4-Effect)))
(define (fv-effect effect)
  (logging fv-effect (Vomit) "~v" effect)
  (match effect
    [(Assign i v) (simplify-assignment i v)]
    [(Halt) (return-state (Halt))]
    [(If t c a)
     (do (bind-state : (State Locs D4-Effect))
         (t : D4-Pred   <- (fv-pred t))
         (c : D4-Effect <- (fv-effect c))
         (a : D4-Effect <- (fv-effect a))
         (return-state (If t c a)))]
    [(Begin e* _)
     (do (bind-state : (State Locs D4-Effect))
         (e* : D4-Effect* <- (fv-effect* e*))
         (return-state (Begin e* NO-OP)))]
    [(Repeat i t1 t2 e)
     (do (bind-state : (State Locs D4-Effect))
         (t1 : D4-Trivial <- (fv-trivial t1))
         (t2 : D4-Trivial <- (fv-trivial t2))
         (e  : D4-Effect  <- (fv-effect  e))
         (return-state (Repeat i t1 t2 e)))]
    [(App-Code t t*)
     (do (bind-state : (State Locs D4-Effect))
         (t  : D4-Trivial  <- (fv-trivial  t))
         (t* : D4-Trivial* <- (fv-trivial* t*))
         (u  : Uid         <- (loc-state "unused_return"))
         (return-state (Assign u (App-Code t t*))))]
    [(Op p t*)
     (do (bind-state : (State Locs D4-Effect))
         (t* : D4-Trivial* <- (fv-trivial* t*))
         (return-state (Op p t*)))]
    [(No-Op) (return-state NO-OP)]
    [other (error 'remove-complex-opera/effect "~a" other)]))

(: fv-effect* (D3-Effect* -> (State Locs D4-Effect*)))
(define (fv-effect* effect*)
  (map-state fv-effect effect*))

(: fv-trivial (D3-Trivial -> (State Locs D4-Trivial)))
(define (fv-trivial triv)
  (match triv
    [(Var u) (return-state (Var u))]
    [(Code-Label u) (return-state (Code-Label u))]
    [(Quote k) (return-state (Quote k))]))

(: fv-trivial* (D3-Trivial* -> (State Locs D4-Trivial*)))
(define (fv-trivial* t*)
  (map-state fv-trivial t*))

(: simplify-assignment (Uid D3-Value -> (State Locs D4-Effect)))
(define (simplify-assignment var val)
  (logging simplify-assignment (Vomit) "\n~v\n~v" var val)
  (define (sa [val : D3-Value]) (simplify-assignment var val))
  (match val
    [(Var u) (return-state (Assign var (Var u)))]
    [(Code-Label u) (return-state (Assign var (Code-Label u)))]
    [(Quote k) (return-state (Assign var (Quote k)))]
    [(Op p t*)
     (do (bind-state : (State Locs D4-Effect))
         (t* : D4-Trivial* <- (map-state fv-trivial t*))
         (return-state (Assign var (Op p t*))))]
    [(App-Code t t*)
     (do (bind-state : (State Locs D4-Effect))
         (t  : D4-Trivial  <- (fv-trivial  t))
         (t* : D4-Trivial* <- (fv-trivial* t*))
         (return-state (Assign var (App-Code t t*))))]
    [(If t c a)
     (do (bind-state : (State Locs D4-Effect))
         (t : D4-Pred <- (fv-pred t))
         (c : D4-Effect <- (simplify-assignment var c))
         (a : D4-Effect <- (simplify-assignment var a))
         (begin
           (logging fv-sa-if (Vomit) "t ~v\nc ~v\na ~v" t c a)
           (return-state (If t c a))))]
    [(Begin e* v)
     (do (bind-state : (State Locs D4-Effect))
         (e* : D4-Effect* <- (fv-effect* e*))
         (e  : D4-Effect <- (simplify-assignment var v))
         (return-state (make-begin (snoc e* e) NO-OP)))]
    [err (error 'simplify-assignment "unmatched datum ~a" err)]))
