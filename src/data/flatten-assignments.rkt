#lang typed/racket

(require "../language.rkt"
         "../helpers.rkt")

(provide flatten-assignments)
(: flatten-assignments (Data3-Lang Config -> Data4-Lang))
(define (flatten-assignments prog config)
  (match-let ([(Prog (list name count ty) (Labels bnd* body)) prog])
    (let*-values ([(body count) (run-state (fa-body body) count)]
                  [(bnd* count) (run-state (map-state fa-bnd-code bnd*) count)])
      (Prog (list name count ty) (Labels bnd* body)))))

;; Simple recursion into the body of code bindings
(: fa-bnd-code (D3-Bnd-Code -> (State Nat D4-Bnd-Code)))
(define (fa-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (do (bind-state : (State Nat D3-Bnd-Code))
        (b : D4-Body <- (fa-body b))
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
(: fa-body (D3-Body -> (State Nat D4-Body)))
(define (fa-body body)
  (match-let ([(Locals i* t) body])
    (do (bind-state : (State Nat D3-Body))
        (n : Nat <- get-state)
        (let-values ([(t l) (run-state (fa-tail t) `(() . ,n))])
          (match-let ([(cons i*^ n) l])
            (_ : Null <- (put-state n))
            (return-state (Locals (append i*^ i*) t)))))))

(: fa-tail (D3-Tail -> (State Locs D4-Tail)))
(define (fa-tail tail)
  (match tail
    [(If t c a)
     (do (bind-state : (State Locs D4-Tail))
         (t : D4-Pred <- (fa-pred t))
         (c : D4-Tail <- (fa-tail c))
         (a : D4-Tail <- (fa-tail a))
         (return-state (If t c a)))]
      [(Begin e* v)
       (do (bind-state : (State Locs D4-Tail))
           (e* : D4-Effect* <- (fa-effect e*))
           (t  : D4-Tail <- (fa-tail v))
           (return-state (make-begin e* t)))]
      [(App v v*) (TODO)#;
       (do (bind-state : (State Nat (Pair D4-Tail Uid*)))
           (t  : Triv-Value  <- (trivialize-value v))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t i*) t]
                       [(list s*^ t* i*^) t*])
             (let* ([v : D4-Tail (Begin (append s* s*^) (App t t*))]
                    [i : Uid* (append i* i*^)]
                    [r : (Pair D4-Tail Uid*) (cons v i)])
               (return-state r))))]
      [(Op p v*) (TODO)#;
       (do (bind-state : (State Nat (Pair D4-Tail Uid*)))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t* i*) t*])
             (let* ([v : D4-Tail (Begin s* (Op p t*))]
                    [r : (Pair D4-Tail Uid*) (cons v i*)])
               (return-state r))))]
      [(Halt) (return-state (Halt))]
      [(Var i) (return-state (Var i))]
      [(Quote k) (return-state (Quote k))]))


(: fa-pred (D3-Pred -> (State Nat (Pair D4-Pred Uid*))))
(define (fa-pred pred)
  (match pred
    [(If t c a) (TODO)#;
     (do (bind-state : (State Nat (Pair D4-Pred Uid*)))
         (t : (Pair D4-Pred Uid*) <- (fa-pred t))
         (c : (Pair D4-Pred Uid*) <- (fa-pred c))
         (a : (Pair D4-Pred Uid*) <- (fa-pred a))
         (match-let ([(cons t ti*) t]
                     [(cons c ci*) c]
                     [(cons a ai*) a])
           (let* ([p : D4-Pred (If t c a)]
                  [i : Uid* (append ti* ci* ai*)]
                  [r : (Pair D4-Pred Uid*) (cons p i)])
             (return-state r))))]
      [(Begin e* v) (TODO)#;
       (do (bind-state : (State Nat (Pair D4-Pred Uid*)))
           (e* : (Pair D4-Effect* Uid*) <- (fa-effect* e*))
           (t  : (Pair D4-Pred Uid*) <- (fa-pred v))
           (match-let ([(cons e* ei*) e*]
                       [(cons t ti*) t])
             (let* ([v : D4-Pred (Begin e* t)]
                    [i : Uid*   (append ei* ti*)]
                    [r : (Pair D4-Pred Uid*) (cons v i)])
               (return-state r))))]
      [(Relop p v1 v2) (TODO)#;
       (do (bind-state : (State Nat (Pair D4-Pred Uid*)))
           (t1 : Triv-Value <- (trivialize-value v1))
           (t2 : Triv-Value <- (trivialize-value v2))
           (match-let ([(list s1* t1 i1*) t1]
                       [(list s2* t2 i2*) t2])
             (let* ([p : D4-Pred (Begin (append s1* s2*) (Relop p t1 t2))]
                    [i : Uid* (append i1* i2*)]
                    [r : (Pair D4-Pred Uid*) (cons p i)])
               (return-state r))))]))

(: fa-effect (D3-Effect -> (State Nat (Pair D4-Effect Uid*))))
(define (fa-effect effect)
  (match effect
    [(Assign i v) (TODO)#;
     (do (bind-state : (State Nat (Pair D4-Effect Uid*)))
         (v : (Pair D4-Value Uid*) <- (fa-value v))
         (match-let ([(cons v i*) v])
           (let ([r : (Pair D4-Effect Uid*) (cons (Assign i v) i*)])
             (return-state r))))]
    [(If t c a) (TODO)#;
     (do (bind-state : (State Nat (Pair D4-Effect Uid*)))
         (t : (Pair D4-Pred Uid*) <- (fa-pred t))
         (c : (Pair D4-Effect Uid*) <- (fa-effect c))
         (a : (Pair D4-Effect Uid*) <- (fa-effect a))
         (match-let ([(cons t ti*) t]
                     [(cons c ci*) c]
                     [(cons a ai*) a])
           (let* ([e : D4-Effect (If t c a)]
                  [i : Uid* (append ti* ci* ai*)]
                  [r : (Pair D4-Effect Uid*) (cons e i)])
             (return-state r))))]
    [(Begin e* _) (TODO)#;
     (do (bind-state : (State Nat (Pair D4-Effect Uid*)))
         (e* : (Pair D4-Effect* Uid*) <- (fa-effect* e*))
       (match-let ([(cons e* i*) e*])
         (let* ([v : D4-Effect (Begin e* NO-OP)]
                [r : (Pair D4-Effect Uid*) (cons v i*)])
           (return-state r))))]
    [(Repeat i v1 v2 e) (TODO)#;
     (do (bind-state : (State Nat (Pair D4-Effect Uid*)))
         (t1 : Triv-Value <- (trivialize-value v1))
         (t2 : Triv-Value <- (trivialize-value v2))
         (e  : (Pair D4-Effect Uid*) <- (fa-effect e))
         (match-let ([(list s1* t1 i1*) t1]
                     [(list s2* t2 i2*) t2]
                     [(cons e      i3*) e])
           (i1 : Uid <- (uid-state "start"))
           (i2 : Uid <- (uid-state "stop"))
           (let* ([s1 : D4-Effect (Assign i1 t1)]
                  [s2 : D4-Effect (Assign i2 t2)]
                  [r : D4-Effect* (list s1 s2 (Repeat i i1 i2 e))]
                  [e : D4-Effect (Begin (append s1* s2* r) NO-OP)]
                  [i* : Uid* (cons i1 (cons i2 (append i1* i2* i3*)))]
                  [r : (Pair D4-Effect Uid*) (cons e i*)])
             (return-state r))))]
      [(App v v*) (TODO)#;
       (do (bind-state : (State Nat (Pair D4-Effect Uid*)))
           (t  : Triv-Value  <- (trivialize-value v))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t i*) t]
                       [(list s*^ t* i*^) t*])
             (i : Uid <- (uid-state "effectCall"))
             (let* ([a : D4-Effect* (list (Assign i (App t t*)))]
                    [v : D4-Effect  (make-begin (append s* s*^ a) NO-OP)]
                    [i* : Uid* (cons i (append i* i*^))]
                    [r : (Pair D4-Effect Uid*) (cons v i*)])
               (return-state r))))]
      [(Op p v*) (TODO)#;
       (do (bind-state : (State Nat (Pair D4-Effect Uid*)))
           (t* : Triv-Value* <- (trivialize-value* v*))
           (match-let ([(list s* t* i*) t*])
             (let* ([e* : D4-Effect* (append s* (list (Op p t*)))]
                    [v : D4-Effect (Begin e* NO-OP)]
                    [r : (Pair D4-Effect Uid*) (cons v i*)])
               (return-state r))))]
      [(No-Op) (return-state NO-OP)]
      [other (error 'remove-complex-opera/effect "~a" other)]))

(: fa-effect* (D3-Effect* -> (State Locs D4-Effect*)))
(define (fa-effect* effect*) (TODO)#;
  (do (bind-state : (State Nat (Pair D4-Effect* Uid*)))
      (e : (Pair D4-Effect Uid*) <- (fa-effect (car effect*)))
      (e* : (Pair D4-Effect* Uid*) <- (fa-effect* (cdr effect*)))
      (match-let ([(cons e i*) e]
                  [(cons e* i*^) e*])
        (let ([e* : D4-Effect* (cons e e*)]
              [i* : Uid* (append i* i*^)]
              [r  : (Pair D4-Effect* Uid*) (cons e* i*)])
          (return-state r)))))

(: simplify-assignment (Uid D3-Value -> D4-Effect))
(define (simplify-assignment var val)
  (define (sa val) (simplify-assignment var val))
  (match val
    [#t (TODO)]
    #;
    [() (guard (or (uvar? trv) (int64? trv) (label? trv)))
    `(set! ,var ,trv)]
    #;
    [(alloc ,x) `(set! ,var (alloc ,x))]
    #;
    [(mref ,x ,y) `(set! ,var (mref ,x ,y))]
    #;
    [(if ,[Pred -> test] ,[Set-Bang-dr -> conseq] ,[Set-Bang-dr -> altern])
     `(if ,test ,conseq ,altern)]
    #;
    [(begin ,[Effect -> ef*] ... ,[Set-Bang-dr -> val])
     `(begin ,ef* ... ,val)]
    #;
    [(,binop ,x ,y)    `(set! ,var (,binop ,x ,y))]
    #;
    [(,trv1 ,trv* ...) `(set! ,var (,trv1 ,trv* ...))]
    #;
    [,err (error who "unmatched datum in Set-Bang: ~a" err)]))
