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
  (match-let ([(Prog (list name next type) (GlobDecs d* (Labels bnd* body))) prog])
    (let* ([next : (Boxof Nat) (box next)]
           [body (fv-body next body)]
           [bnd* (for/list : D4-Bnd-Code* #;(Listof ) ([b bnd*]) (fv-bnd-code next b)
                           )]
           [next : Nat (unbox next)])
      (Prog (list name next type) (GlobDecs d* (Labels bnd* body))))))

;; Simple recursion into the body of code bindings
(: fv-bnd-code ((Boxof Nat) D3-Bnd-Code -> D4-Bnd-Code))
(define (fv-bnd-code next bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (let ([b : D4-Body (fv-body next b)])
      (cons i (Code i* b)))))

;; Add new local variables to the declaration to each body
(: fv-body ((Boxof Nat) D3-Body -> D4-Body))
(define (fv-body next body)
  (define loc* : (Boxof Uid*) (box '()))
  
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let* ([n (unbox next)]
           [u* (unbox loc*)]
           [u (Uid s n)])
      (set-box! next (+ n 1))
      (set-box! loc* (cons u u*))
      u))
  
  (: fv-tail (D3-Tail -> D4-Tail))
  (define (fv-tail tail)
    (match tail
      [(If t c a)
       (let ([t (fv-pred t)]
             [c (fv-tail c)]
             [a (fv-tail a)])
         (If t c a))]
      [(Begin e* v) (make-begin (fv-effect* e*) (fv-tail v))]
      [(Return v)
       (if (Success? v)
           (Return (Success))
           (match-let ([(cons e* v) (fv-value v)])
             (make-begin e* (Return v))))]))

  (: fv-value (D3-Value -> (Pair D4-Effect* D4-Value)))
  (define (fv-value v)
    (match v
      [(If t c a)
       (let ([t (fv-pred t)])
         (match-let ([(cons c* c) (fv-value c)]
                     [(cons a* a) (fv-value a)]
                     [id (next-uid! "ifValue")])
           (let* ([ca : D4-Effect (Assign id c)]
                  [aa : D4-Effect (Assign id a)]
                  [cb : D4-Effect (make-begin (snoc c* ca) NO-OP)]
                  [ab : D4-Effect (make-begin (snoc a* aa) NO-OP)]
                  [e  : D4-Effect (If t cb ab)]
                  [v  : D4-Value  (Var id)])
             (cons (list e) v))))]
      [(Begin e* v)
       (match-let ([(cons e*^ v) (fv-value v)])
         (cons (append (fv-effect* e*) e*^) v))]
      [(App-Code t t*) (cons '() (App-Code (fv-trivial t) (fv-trivial* t*)))]
      [(Op p t*) (cons '() (Op p (fv-trivial* t*)))]
      [(Halt) (cons '() (Halt))]
      [(Var i) (cons '() (Var i))]
      [(Quote k) (cons '() (Quote k))]))

  (: fv-pred (D3-Pred -> D4-Pred))
  (define (fv-pred pred)
    (match pred
      [(If t c a)
       (let ([t (fv-pred t)]
             [c (fv-pred c)]
             [a (fv-pred a)])
         (If t c a))]
      [(Begin e* p) (Begin (fv-effect* e*) (fv-pred p))]
      [(Relop p t1 t2)
       (let ([t1 (fv-trivial t1)]
             [t2 (fv-trivial t2)])
         (Relop p t1 t2))]))

  (: fv-effect (D3-Effect -> D4-Effect))
  (define (fv-effect effect)
    (match effect
      [(Assign i v) (simplify-assignment i v)]
      [(Halt) (Halt)]
      [(If t c a)
       (let ([t (fv-pred t)]
             [c (fv-effect c)]
             [a (fv-effect a)])
         (If t c a))]
      [(Begin e* _) (Begin (fv-effect* e*) NO-OP)]
      [(Repeat i t1 t2 #f #f e)
       (let ([t1 (fv-trivial t1)]
             [t2 (fv-trivial t2)]
             [e  (fv-effect  e)])
         (Repeat i t1 t2 #f #f e))]
      [(App-Code t t*)
       (let ([t (fv-trivial t)]
             [t* (fv-trivial* t*)]
             [u (next-uid! "unused_return")])
         (Assign u (App-Code t t*)))]
      [(Op p t*) (Op p (fv-trivial* t*))]
      [(No-Op) NO-OP]
      [other (error 'remove-complex-opera/effect "~a" other)]))

  (: fv-effect* (D3-Effect* -> D4-Effect*))
  (define (fv-effect* effect*) (map fv-effect effect*))

  (: simplify-assignment (Uid D3-Value -> D4-Effect))
  (define (simplify-assignment var val)
    (define (sa [val : D3-Value]) (simplify-assignment var val))
    (match val
      [(Var u) (Assign var (Var u))]
      [(Code-Label u) (Assign var (Code-Label u))]
      [(Quote k) (Assign var (Quote k))]
      [(Op p t*) (Assign var (Op p (map fv-trivial t*)))]
      [(App-Code t t*)
       (Assign var (App-Code (fv-trivial t) (fv-trivial* t*)))]
      [(If t c a)
       (let ([t (fv-pred t)]
             [c (simplify-assignment var c)]
             [a (simplify-assignment var a)])
         (If t c a))]
      [(Begin e* v)
       (let ([e* (fv-effect* e*)]
             [e (simplify-assignment var v)])
         (make-begin (snoc e* e) NO-OP))]
      [err (error 'simplify-assignment "unmatched datum ~a" err)]))

  (match-let ([(Locals i* t) body])
    (let ([t (fv-tail t)]
          [x (append (unbox loc*) i*)])
      (Locals x t))))

(: fv-trivial (D3-Trivial -> D4-Trivial))
(define (fv-trivial triv)
  (match triv
    [(Var u) (Var u)]
    [(Code-Label u) (Code-Label u)]
    [(Quote k) (Quote k)]))

(: fv-trivial* (D3-Trivial* -> D4-Trivial*))
(define (fv-trivial* t*)
  (map fv-trivial t*))
