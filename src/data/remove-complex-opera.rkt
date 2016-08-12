#lang typed/racket

(require "../language/data2.rkt"
         "../language/data3.rkt"
         "../language/make-begin.rkt"
         "../configuration.rkt"
         "../helpers.rkt")

(provide remove-complex-opera)

(: remove-complex-opera (Data2-Lang -> Data3-Lang))
(define (remove-complex-opera prog)
  (match-let ([(Prog (list name count ty) (GlobDecs d* (Labels bnd* body))) prog])
    (let*-values ([(body count) (run-state (rco-body body) count)]
                  [(bnd* count) (run-state (map-state rco-bnd-code bnd*) count)])
      (Prog (list name count ty) (GlobDecs d* (Labels bnd* body))))))

;; Simple recursion into the body of code bindings
(: rco-bnd-code (D2-Bnd-Code -> (State Nat D3-Bnd-Code)))
(define (rco-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (do (bind-state : (State Nat D3-Bnd-Code))
        (b : D3-Body <- (rco-body b))
        (return-state (cons i (Code i* b))))))

;; Monad keeps track of uniqueness and allocated ids
(define-type RcoSt (Pair Uid* Nat))
(: loc-state (String -> (State RcoSt Uid)))
(define ((loc-state suffix) state)
  (match-let ([(cons u* n) state])
    (let ([u (Uid suffix n)])
      (values u (cons (cons u u*) (add1 n))))))

;; Add new local variables to the declaration to each body
(: rco-body (D2-Body -> (State Nat D3-Body)))
(define (rco-body body)
  (match-let ([(Locals i* t) body])
    (do (bind-state : (State Nat D3-Body))
        (n : Nat <- get-state)
        (match-let-values ([(t (cons u* n)) (run-state (rco-tail t) `((). ,n))])
         (_ : Null <- (put-state n))
         (return-state (Locals (append u* i*) t))))))

(: rco-tail (D2-Tail -> (State RcoSt D3-Tail)))
(define (rco-tail tail)
  (match tail
    [(If t c a)
     (do (bind-state : (State RcoSt D3-Tail))
         (t : D3-Pred <- (rco-pred t))
         (c : D3-Tail <- (rco-tail c))
         (a : D3-Tail <- (rco-tail a))
         (return-state (If t c a)))]
    [(Begin e* v)
     (do (bind-state : (State RcoSt D3-Tail))
         (e* : D3-Effect* <- (rco-effect* e*))
         (t  : D3-Tail    <- (rco-tail v))
         (return-state (Begin e* t)))]
    [(App-Code v v*)
     (do (bind-state  : (State RcoSt D3-Tail))
         ((cons s*1 t)  : Triv-Value  <- (trivialize-value v))
         ((cons s*2 t*) : Triv-Value* <- (trivialize-value* v*))
         (return-state (Begin (append s*1 s*2) (Return (App-Code t t*)))))]
    [(Op p v*)
     (do (bind-state : (State RcoSt D3-Tail))
         ((cons s* t*) : Triv-Value* <- (trivialize-value* v*))
         (return-state (make-begin s* (Return (Op p t*)))))]
    ;; I do not think that this should ever happen
    ;; perhaps I need a type rule to guarentee it.
    [(Code-Label i) (return-state (Return (Code-Label i)))]
    [(Var i) (return-state (Return (Var i)))]
    [(Quote k) (return-state (Return (Quote k)))]
    [(Halt) (return-state (Return (Halt)))]
    [(Success) (return-state (Return (Success)))]
    [other (error 'remove-complex-opera "unmatched ~a" other)]))

(: rco-pred (D2-Pred -> (State RcoSt D3-Pred)))
(define (rco-pred pred)
  (match pred
    [(If t c a)
     (do (bind-state : (State RcoSt D3-Pred))
         (t : D3-Pred <- (rco-pred t))
         (c : D3-Pred <- (rco-pred c))
         (a : D3-Pred <- (rco-pred a))
         (return-state (If t c a)))]
      [(Begin e* p)
       (do (bind-state : (State RcoSt D3-Pred))
           (e* : D3-Effect* <- (rco-effect* e*))
           (p  : D3-Pred    <- (rco-pred p))
           (return-state (make-begin e* p)))]
      [(Relop p v1 v2)
       (do (bind-state : (State RcoSt D3-Pred))
           ((cons s*1 t1) : Triv-Value <- (trivialize-value v1))
           ((cons s*2 t2) : Triv-Value <- (trivialize-value v2))
           (let* ([e* : D3-Effect* (append s*1 s*2)]
                  [p  : D3-Pred    (Relop p t1 t2)])
             (return-state (make-begin e* p))))]))

(: rco-effect (D2-Effect -> (State RcoSt D3-Effect)))
(define (rco-effect effect)
  (match effect
    [(Assign i v)
     (do (bind-state : (State RcoSt D3-Effect))
         (v : D3-Value <- (rco-value v))
       (return-state (Assign i v)))]
    [(Halt) (return-state (Halt))]
    [(If t c a)
     (do (bind-state : (State RcoSt D3-Effect))
         (t : D3-Pred   <- (rco-pred t))
         (c : D3-Effect <- (rco-effect c))
         (a : D3-Effect <- (rco-effect a))
         (return-state (If t c a)))]
    [(Begin e* _)
     (do (bind-state : (State RcoSt D3-Effect))
         (e* : D3-Effect* <- (rco-effect* e*))
         (return-state (make-begin e* NO-OP)))]
    [(Repeat i v1 v2 #f #f e)
     (do (bind-state : (State RcoSt D3-Effect))
         ((cons s*1 t1) : Triv-Value <- (trivialize-value v1))
         ((cons s*2 t2) : Triv-Value <- (trivialize-value v2))
         (e             : D3-Effect  <- (rco-effect e))
         (i1 : Uid <- (loc-state "start"))
         (i2 : Uid <- (loc-state "stop"))
         (return-state
          (Begin
            (append
             s*1 s*2
             (list (Assign i1 t1)
                   (Assign i2 t2)
                   (Repeat i (Var i1) (Var i2) #f #f e))) NO-OP)))]
    [(App-Code v v*)
     (do (bind-state : (State RcoSt D3-Effect))
         ((cons s*  t)  : Triv-Value  <- (trivialize-value v))
         ((cons s*^ t*) : Triv-Value* <- (trivialize-value* v*))
         (return-state (make-begin (append s* s*^ (list (App-Code t t*))) NO-OP)))]
    [(Op p v*)
     (do (bind-state : (State RcoSt D3-Effect))
         ((cons s* t*) : Triv-Value* <- (trivialize-value* v*))
         (return-state (make-begin (append s* (list (Op p t*))) NO-OP)))]
    [(No-Op) (return-state NO-OP)]
    [other (error 'remove-complex-opera/effect "~a" other)]))

(: rco-effect* (D2-Effect* -> (State RcoSt D3-Effect*)))
(define (rco-effect* effect*)
  (map-state rco-effect effect*))

(: rco-value (D2-Value -> (State RcoSt D3-Value)))
(define (rco-value val)
  (match val
    [(If t c a)
     (do (bind-state : (State RcoSt D3-Value))
         (t : D3-Pred  <- (rco-pred t))
         (c : D3-Value <- (rco-value c))
         (a : D3-Value <- (rco-value a))
         (return-state (If t c a)))]
    [(Begin e* v)
     (do (bind-state : (State RcoSt D3-Value))
         (e* : D3-Effect* <- (rco-effect* e*))
         (v  : D3-Value   <- (rco-value v))
         (return-state (make-begin e* v)))]
    [(App-Code v v*)
     (do (bind-state : (State RcoSt D3-Value))
         ((cons s*  t)  : Triv-Value  <- (trivialize-value v))
         ((cons s*^ t*) : Triv-Value* <- (trivialize-value* v*))
         (return-state (make-begin (append s* s*^) (App-Code t t*))))]
    [(Op p v*)
     (do (bind-state : (State RcoSt D3-Value))
         ((cons s* t*) : Triv-Value* <- (trivialize-value* v*))
         (return-state (make-begin s* (Op p t*))))]
    [(Halt) (return-state (Halt))]
    [(Code-Label i) (return-state (Code-Label i))]
    [(Var i) (return-state (Var i))]
    [(Quote k) (return-state (Quote k))]))

(define-type Triv-Value (Pair D3-Effect* D3-Trivial))
(define-type Triv-Value* (Pair D3-Effect* D3-Trivial*))

(: trivialize-value (D2-Value -> (State RcoSt Triv-Value)))
(define (trivialize-value value)
  (match value
    [(If t c a)
     (do (bind-state : (State RcoSt Triv-Value))
         (t : D3-Pred    <- (rco-pred t))
         ((cons c* c) : Triv-Value <- (trivialize-value c))
         ((cons a* a) : Triv-Value <- (trivialize-value a))
         (i : Uid <- (loc-state "tmp"))
         (let* ([c (make-begin c* c)]
                [a (make-begin a* a)]
                [s (Assign i (If t c a))])
           (return-state (cons (list s) (Var i)))))]
    [(Begin e* v)
     (do (bind-state : (State RcoSt Triv-Value))
         (e* : D3-Effect* <- (rco-effect* e*))
         ((cons t* t)  : Triv-Value <- (trivialize-value v))
         (return-state (cons (append e* t*) t)))]
    [(App-Code v v*)
     (do (bind-state : (State RcoSt Triv-Value))
         ((cons s*  t)  : Triv-Value  <- (trivialize-value v))
         ((cons s*^ t*) : Triv-Value* <- (trivialize-value* v*))
         (i : Uid <- (loc-state "tmp_value"))
         (let ([s (Assign i (App-Code t t*))])
           (return-state (cons (append s* s*^ (list s)) (Var i)))))]
    [(Op p v*)
     (do (bind-state : (State RcoSt Triv-Value))
         ((cons s* t*) : Triv-Value* <- (trivialize-value* v*))
         (i : Uid <- (loc-state "tmp_value"))
         (return-state (cons (append s* (list (Assign i (Op p t*)))) (Var i))))]
    [(Halt)
     (do (bind-state : (State RcoSt Triv-Value))
         (i : Uid <- (loc-state "tmp"))
         (return-state (cons (list (Assign i (Halt))) (Var i))))]
    [(Code-Label i) (return-state (cons '() (Code-Label i)))]
    [(Var i) (return-state (cons '() (Var i)))]
    [(Quote k) (return-state (cons '() (Quote k)))]))

(: trivialize-value* (D2-Value* -> (State RcoSt Triv-Value*)))
(define (trivialize-value* value*)
  (define pair : (All (a b) a b -> (Pair a b)) cons)
  (if (null? value*)
      (return-state '(() . ()))
      (do (bind-state : (State RcoSt Triv-Value*))
          ((cons s*  t)  : Triv-Value  <- (trivialize-value  (car value*)))
          ((cons s*^ t*) : Triv-Value* <- (trivialize-value* (cdr value*)))
          (return-state (pair (append s* s*^) (cons t t*))))))
