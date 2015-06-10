#lang typed/racket

(require "../language.rkt"
         "../helpers.rkt")

(provide remove-complex-opera)

(: remove-complex-opera (Data2-Lang Config -> Data3-Lang))
(define (remove-complex-opera prog config)
  (match-let ([(Prog (list name count ty) (Labels bnd* body)) prog])
    (let*-values ([(body count) (run-state (rco-body body) count)]
                  [(bnd* count) (run-state (map-state rco-bnd-code bnd*) count)])
      (Prog (list name count ty) (Labels bnd* body)))))

;; Simple recursion into the body of code bindings
(: rco-bnd-code (D2-Bnd-Code -> (State Nat D3-Bnd-Code)))
(define (rco-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (do (bind-state : (State Nat D3-Bnd-Code))
        (b : D3-Body <- (rco-body b))
        (return-state (cons i (Code i* b))))))

;; Add new local variables to the declaration to each body
(: rco-body (D2-Body -> (State Nat D3-Body)))
(define (rco-body body)
  (match-let ([(Locals i* t) body])
    (do (bind-state : (State Nat D3-Body))
        (t.l* : (Pair D3-Tail Uid*) <- (rco-tail t))
        (match-let ([(cons t l*) t.l*])
          (return-state (Locals (append i* l*) t))))))

(: rco-tail (D2-Tail -> (State Nat (Pair D3-Tail Uid*))))
(define (rco-tail tail)
  (match tail
    [(If t c a)
     (do (bind-state : (State Nat (Pair D3-Tail Uid*)))
         ((cons t ti*) : (Pair D3-Pred Uid*) <- (rco-pred t))
         ((cons c ci*) : (Pair D3-Tail Uid*) <- (rco-tail c))
         ((cons a ai*) : (Pair D3-Tail Uid*) <- (rco-tail a))
         (let* ([t : D3-Tail (If t c a)]
                  [i : Uid* (append ti* ci* ai*)]
                  [r : (Pair D3-Tail Uid*) (cons t i)])
             (return-state r)))]
      [(Begin e* v)
       (do (bind-state : (State Nat (Pair D3-Tail Uid*)))
           (e* : (Pair D3-Effect* Uid*) <- (rco-effect* e*))
           (t  : (Pair D3-Tail Uid*) <- (rco-tail v))
           (match-let ([(cons e* ei*) e*]
                       [(cons t ti*) t])
             (let* ([v : D3-Tail (Begin e* t)]
                    [i : Uid*   (append ei* ti*)]
                    [r : (Pair D3-Tail Uid*) (cons v i)])
               (return-state r))))]
      [(App v v*)
       (do (bind-state : (State Nat (Pair D3-Tail Uid*)))
           (t  : Triv-Value  <- (trivialize-value v))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t i*) t]
                       [(list s*^ t* i*^) t*])
             (let* ([v : D3-Tail (Return (Begin (append s* s*^) (App t t*)))]
                    [i : Uid* (append i* i*^)]
                    [r : (Pair D3-Tail Uid*) (cons v i)])
               (return-state r))))]
      [(Op p v*)
       (do (bind-state : (State Nat (Pair D3-Tail Uid*)))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t* i*) t*])
             (let* ([v : D3-Tail (Begin s* (Return (Op p t*)))]
                    [r : (Pair D3-Tail Uid*) (cons v i*)])
               (return-state r))))]
      [(Halt) (return-state (ann (cons (Return (Halt)) '()) (Pair D3-Tail Uid*)))]
      [(Var i) (return-state (ann (cons (Return (Var i)) '()) (Pair D3-Tail Uid*)))]
      [(Quote k) (return-state (ann (cons (Return (Quote k)) '()) (Pair D3-Tail Uid*)))]))

(: rco-pred (D2-Pred -> (State Nat (Pair D3-Pred Uid*))))
(define (rco-pred pred)
  (match pred
    [(If t c a)
     (do (bind-state : (State Nat (Pair D3-Pred Uid*)))
         (t : (Pair D3-Pred Uid*) <- (rco-pred t))
         (c : (Pair D3-Pred Uid*) <- (rco-pred c))
         (a : (Pair D3-Pred Uid*) <- (rco-pred a))
         (match-let ([(cons t ti*) t]
                     [(cons c ci*) c]
                     [(cons a ai*) a])
           (let* ([p : D3-Pred (If t c a)]
                  [i : Uid* (append ti* ci* ai*)]
                  [r : (Pair D3-Pred Uid*) (cons p i)])
             (return-state r))))]
      [(Begin e* v)
       (do (bind-state : (State Nat (Pair D3-Pred Uid*)))
           (e* : (Pair D3-Effect* Uid*) <- (rco-effect* e*))
           (t  : (Pair D3-Pred Uid*) <- (rco-pred v))
           (match-let ([(cons e* ei*) e*]
                       [(cons t ti*) t])
             (let* ([v : D3-Pred (Begin e* t)]
                    [i : Uid*   (append ei* ti*)]
                    [r : (Pair D3-Pred Uid*) (cons v i)])
               (return-state r))))]
      [(Relop p v1 v2)
       (do (bind-state : (State Nat (Pair D3-Pred Uid*)))
           (t1 : Triv-Value <- (trivialize-value v1))
           (t2 : Triv-Value <- (trivialize-value v2))
           (match-let ([(list s1* t1 i1*) t1]
                       [(list s2* t2 i2*) t2])
             (let* ([p : D3-Pred (Begin (append s1* s2*) (Relop p t1 t2))]
                    [i : Uid* (append i1* i2*)]
                    [r : (Pair D3-Pred Uid*) (cons p i)])
               (return-state r))))]))

(: rco-effect (D2-Effect -> (State Nat (Pair D3-Effect Uid*))))
(define (rco-effect effect)
  (match effect
    [(Assign i v)
     (do (bind-state : (State Nat (Pair D3-Effect Uid*)))
         (v : (Pair D3-Value Uid*) <- (rco-value v))
         (match-let ([(cons v i*) v])
           (let ([r : (Pair D3-Effect Uid*) (cons (Assign i v) i*)])
             (return-state r))))]
    [(If t c a)
     (do (bind-state : (State Nat (Pair D3-Effect Uid*)))
         (t : (Pair D3-Pred Uid*) <- (rco-pred t))
         (c : (Pair D3-Effect Uid*) <- (rco-effect c))
         (a : (Pair D3-Effect Uid*) <- (rco-effect a))
         (match-let ([(cons t ti*) t]
                     [(cons c ci*) c]
                     [(cons a ai*) a])
           (let* ([e : D3-Effect (If t c a)]
                  [i : Uid* (append ti* ci* ai*)]
                  [r : (Pair D3-Effect Uid*) (cons e i)])
             (return-state r))))]
    [(Begin e* _)
     (do (bind-state : (State Nat (Pair D3-Effect Uid*)))
         (e* : (Pair D3-Effect* Uid*) <- (rco-effect* e*))
       (match-let ([(cons e* i*) e*])
         (let* ([v : D3-Effect (Begin e* NO-OP)]
                [r : (Pair D3-Effect Uid*) (cons v i*)])
           (return-state r))))]
    [(Repeat i v1 v2 e)
     (do (bind-state : (State Nat (Pair D3-Effect Uid*)))
         ((list s1* t1 i1*) : Triv-Value <- (trivialize-value v1))
         ((list s2* t2 i2*) : Triv-Value <- (trivialize-value v2))
         ((cons e i3*) : (Pair D3-Effect Uid*) <- (rco-effect e))
         (i1 : Uid <- (uid-state "start"))
         (i2 : Uid <- (uid-state "stop"))
         (let* ([s1 : D3-Effect (Assign i1 t1)]
                [s2 : D3-Effect (Assign i2 t2)]
                [r : D3-Effect* (list s1 s2 (Repeat i (Var i1) (Var i2) e))]
                [e : D3-Effect (Begin (append s1* s2* r) NO-OP)]
                [i* : Uid* (cons i1 (cons i2 (append i1* i2* i3*)))]
                [r : (Pair D3-Effect Uid*) (cons e i*)])
           (return-state r)))]
      [(App v v*)
       (do (bind-state : (State Nat (Pair D3-Effect Uid*)))
           ((list s* t i*)    : Triv-Value  <- (trivialize-value v))
           ((list s*^ t* i*^) : Triv-Value* <- (trivialize-value* v*))
           (let* ([e* : D3-Effect* (list (App t t*))]
                  [e* : D3-Effect* (append s* s*^ e*)]
                  [e  : D3-Effect  (make-begin e* NO-OP)]
                  [i* : Uid*       (append i* i*^)])
             (return-state (ann (cons e i*) (Pair D3-Effect Uid*)))))]
      [(Op p v*)
       (do (bind-state : (State Nat (Pair D3-Effect Uid*)))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t* i*) t*])
             (let* ([e* : D3-Effect* (append s* (list (Op p t*)))]
                    [v : D3-Effect (Begin e* NO-OP)]
                    [r : (Pair D3-Effect Uid*) (cons v i*)])
               (return-state r))))]
      [(No-Op) (return-state (ann (cons NO-OP '()) (Pair D3-Effect Uid*)))]
    [other (error 'remove-complex-opera/effect "~a" other)]))

(: rco-effect* (D2-Effect* -> (State Nat (Pair D3-Effect* Uid*))))
(define (rco-effect* effect*)
  (if (null? effect*)
      (return-state '(()))
      (do (bind-state : (State Nat (Pair D3-Effect* Uid*)))
          ((cons e  i*)  : (Pair D3-Effect Uid*) <- (rco-effect (car effect*)))
          ((cons e* i*^) : (Pair D3-Effect* Uid*) <- (rco-effect* (cdr effect*)))
          (let ([e* : D3-Effect* (cons e e*)]
                [i* : Uid* (append i* i*^)]
                [r  : (Pair D3-Effect* Uid*) (cons e* i*)])
            (return-state r)))))


(: rco-value (D2-Value -> (State Nat (Pair D3-Value Uid*))))
(define (rco-value val)
  (match val
      [(If t c a)
       (do (bind-state : (State Nat (Pair D3-Value Uid*)))
           (t : (Pair D3-Pred Uid*) <- (rco-pred t))
           (c : (Pair D3-Value Uid*) <- (rco-value c))
           (a : (Pair D3-Value Uid*) <- (rco-value a))
           (match-let ([(cons t ti*) t]
                       [(cons c ci*) c]
                       [(cons a ai*) a])
             (return-state
              (ann  (cons (If t c a) (append ti* ci* ai*))
                    (Pair D3-Value Uid*)))))]
      [(Begin e* v)
       (do (bind-state : (State Nat (Pair D3-Value Uid*)))
           (e* : (Pair D3-Effect* Uid*) <- (rco-effect* e*))
           (t  : (Pair D3-Value Uid*) <- (rco-value v))
           (match-let ([(cons e* ei*) e*]
                       [(cons t ti*) t])
             (let* ([v : D3-Value (Begin e* t)]
                   [i* : Uid* (append ei* ti*)]
                   [r : (Pair D3-Value Uid*) (cons v i*)])
               (return-state r))))]
      [(App v v*)
       (do (bind-state : (State Nat (Pair D3-Value Uid*)))
           (t  : Triv-Value  <- (trivialize-value v))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t i*) t]
                       [(list s*^ t* i*^) t*])
             (let* ([v : D3-Value (Begin (append s* s*^) (App t t*))]
                    [i* : Uid* (append i* i*^)]
                    [r : (Pair D3-Value Uid*) (cons v i*)])
               (return-state r))))]
      [(Op p v*)
       (do (bind-state : (State Nat (Pair D3-Value Uid*)))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t* i*) t*])
             (let* ([v : D3-Value (Begin s* (Op p t*))]
                    [r : (Pair D3-Value Uid*) (cons v i*)])
               (return-state r))))]
      [(Halt) (return-state (ann (cons (Halt) '()) (Pair D3-Value Uid*)))]
      [(Var i) (return-state (ann (cons (Var i) '()) (Pair D3-Value Uid*)))]
      [(Quote k) (return-state (ann (cons (Quote k) '()) (Pair D3-Value Uid*)))]))


(define-type Triv-Value (List D3-Effect* D3-Trivial Uid*))
(define-type Triv-Value* (List D3-Effect* D3-Trivial* Uid*))

(: trivialize-value (D2-Value -> (State Nat Triv-Value)))
(define (trivialize-value val)
  (match val
    [(If t c a)
     (do (bind-state : (State Nat Triv-Value))
         (t : (Pair D3-Pred Uid*) <- (rco-pred t))
         (c : Triv-Value <- (trivialize-value c))
         (a : Triv-Value <- (trivialize-value a))
         (match-let ([(cons t i*) t]
                     [(list cs* c ci*) c]
                     [(list as* a ai*) a])
           (i : Uid <- (uid-state "tmp"))
           (let* ([c (make-begin cs* c)]
                  [a (make-begin as* a)]
                  [s (Assign i (If t c a))])
             (return-state (list (list s) (Var i) (append i* ci* ai*))))))]
    [(Begin e* v)
     (do (bind-state : (State Nat Triv-Value))
         (e* : (Pair D3-Effect* Uid*) <- (rco-effect* e*))
         (t  : Triv-Value <- (trivialize-value v))
         (match-let ([(cons e* ei*) e*]
                     [(list s* t ti*) t])
           (return-state (list (append e* s*) t (append ei* ti*)))))]
    [(App v v*)
     (do (bind-state : (State Nat Triv-Value))
         (t  : Triv-Value  <- (trivialize-value v))
         (t* : Triv-Value* <- (trivialize-value* v*))
         (match-let ([(list s* t i*) t]
                     [(list s*^ t* i*^) t*])
           (i : Uid <- (uid-state "tmp"))
           (let ([s (Assign i (App t t*))])
             (return-state
              (list (append s* s*^ (list s))
                    (Var i)
                    (cons i (append i* i*^)))))))]
    [(Op p v*)
     (do (bind-state : (State Nat Triv-Value))
         (t* : Triv-Value* <- (trivialize-value* v*))
         (match-let ([(list s* t* i*) t*])
           (i : Uid <- (uid-state "tmp"))
           (let ([s (Assign i (Op p t*))])
             (return-state
              (list (append s* (list s)) (Var i) (cons i i*))))))]
    [(Halt)
     (do (bind-state : (State Nat Triv-Value))
         (i : Uid <- (uid-state "tmp"))
         (return-state
          (list (list (Assign i (Halt))) (Var i) (list i))))]
    [(Var i) (return-state (list '() (Var i) '()))]
    [(Quote k) (return-state (list '() (Quote k) '()))]))

(: trivialize-value* (D2-Value* -> (State Nat Triv-Value*)))
(define (trivialize-value* value*)
  (if (pair? value*)
      (do (bind-state : (State Nat Triv-Value*))
          (t  : Triv-Value  <- (trivialize-value  (car value*)))
        (t* : Triv-Value* <- (trivialize-value* (cdr value*)))
        (match-let ([(list s t i) t]
                    [(list s* t* i*) t*])
          (return-state
           (list (append s s*) (cons t t*) (append i i*)))))
      (return-state (list '() '() '()))))
