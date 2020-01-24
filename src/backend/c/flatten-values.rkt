#lang typed/racket/no-check
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


(require
 "../../language/forms.rkt"
 "../../language/make-begin.rkt"
 "../../configuration.rkt"
 "../../helpers.rkt")

(provide flatten-values)
(: flatten-values (Data3-Lang -> Data4-Lang))
(define (flatten-values prog)
  (match-define (Prog (list n c t) (GlobDecs d* (Labels b* b))) prog)
  (define uc (make-unique-counter c))
  (define-values (bnd* body)
    (parameterize ([current-unique-counter uc])
      (let* ([body (fv-body b)]
             [bnd* (map fv-bnd-code b*)])
        (values bnd* body))))
  (Prog (list n (unique-counter-next! uc) t) (GlobDecs d* (Labels bnd* body))))

;; Simple recursion into the body of code bindings
(: fv-bnd-code (D3-Bnd-Code -> D4-Bnd-Code))
(define (fv-bnd-code bnd)
  (match-define (cons i (Code i* b)) bnd)
  (cons i (Code i* (fv-body b))))

;; Add new local variables to the declaration to each body
(: fv-body (D3-Body -> D4-Body))
(define (fv-body body)
  (match-define (Locals l* s* t) body)
  (define new-locals : Uid* l*)
  (: local-next-uid! (String -> Uid))
  (define (local-next-uid! s)
    (define u (next-uid! s))
    (set! new-locals (cons u new-locals))
    u)
  (: fv-tail (D3-Tail -> D4-Tail))
  (define (fv-tail tail)
    (match tail
      [(If t c a)
       (let ([t (fv-pred t)]
             [c (fv-tail c)]
             [a (fv-tail a)])
         (If t c a))]
      [(Switch t c* d)
       ;; I Jumped the gun and flattened the expression during remove complex
       ;; opera. perhaps we should combine the passes?
       (Switch (fv-trivial t) (map-switch-case* fv-tail c*) (fv-tail d))]
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
                     [id (local-next-uid! "ifValue")])
           (let* ([ca : D4-Effect (Assign id c)]
                  [aa : D4-Effect (Assign id a)]
                  [cb : D4-Effect (make-begin (snoc c* ca) NO-OP)]
                  [ab : D4-Effect (make-begin (snoc a* aa) NO-OP)]
                  [e  : D4-Effect (If t cb ab)]
                  [v  : D4-Value  (Var id)])
             (cons (list e) v))))]
      [(Switch t c* d)
       (define u (local-next-uid! "tmp_fv"))
       (define c*^
         (for/list : (Switch-Case* D4-Effect) ([c c*])
           (match-define (cons l (app fv-value (cons s* r))) c)
           (cons l (make-begin (append s* (list (Assign u r))) NO-OP))))
       (match-define (cons s* v) (fv-value d))
       (define s (Switch t c*^ (make-begin (append s* (list (Assign u v))) NO-OP)))
       (cons (list s) (Var u))]
      [(Begin e* v)
       (match-let ([(cons e*^ v) (fv-value v)])
         (cons (append (fv-effect* e*) e*^) v))]
      [(App-Code t t*) (cons '() (App-Code (fv-trivial t) (fv-trivial* t*)))]
      [(Op p t*) (cons '() (Op p (fv-trivial* t*)))]
      [(Halt) (cons '() (Halt))]
      [(Var i) (cons '() (Var i))]
      [(Global s) (cons '() (Global s))]
      [(Quote k) (cons '() (Quote k))]))

  (: fv-pred (D3-Pred -> D4-Pred))
  (define (fv-pred pred)
    (match pred
      [(Quote c) pred]
      [(If t c a)
       (let ([t (fv-pred t)]
             [c (fv-pred c)]
             [a (fv-pred a)])
         (If t c a))]
      [(Switch t c* d)
       (Switch t (map-switch-case* fv-pred c*) (fv-pred d))]
      [(Begin e* p) (Begin (fv-effect* e*) (fv-pred p))]
      [(Relop p e*) (Relop p (map fv-trivial e*))]))

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
      [(Switch t c* a)
       (Switch t (map-switch-case* fv-effect c*) (fv-effect a))]
      [(Begin e* _) (Begin (fv-effect* e*) NO-OP)]
      [(Repeat i t1 t2 #f #f e)
       (let ([t1 (fv-trivial t1)]
             [t2 (fv-trivial t2)]
             [e  (fv-effect  e)])
         (Repeat i t1 t2 #f #f e))]
      [(While e1 e2) (While (fv-pred e1) (fv-effect e2))]
      [(App-Code t t*)
       (let ([t (fv-trivial t)]
             [t* (fv-trivial* t*)]
             [u (local-next-uid! "unused_return")])
         (Assign u (App-Code t t*)))]
      [(Op p t*) (Op p (fv-trivial* t*))]
      [(No-Op) NO-OP]
      [(Break-Repeat) (Break-Repeat)]
      [other (error 'remove-complex-opera/effect "~a" other)]))

  (: fv-effect* (D3-Effect* -> D4-Effect*))
  (define (fv-effect* effect*) (map fv-effect effect*))

  (: simplify-assignment (Id D3-Value -> D4-Effect))
  (define (simplify-assignment var val)
    (define (sa [val : D3-Value]) (simplify-assignment var val))
    (match val
      [(Var u) (Assign var (Var u))]
      [(Global s) (Assign var (Global s))]
      [(Code-Label u) (Assign var (Code-Label u))]
      [(Quote k) (Assign var (Quote k))]
      [(Op p t*) (Assign var (Op p (map fv-trivial t*)))]
      [(App-Code t t*)
       (Assign var (App-Code (fv-trivial t) (fv-trivial* t*)))]
      [(If t c a) (If (fv-pred t) (sa c) (sa a))]
      [(Switch t c* d) (Switch (fv-trivial t) (map-switch-case* sa c*) (sa d))]
      [(Begin e* v) (make-begin (snoc (fv-effect* e*) (sa v)) NO-OP)]
      [err (error 'simplify-assignment "unmatched datum ~a" err)]))
  (define tail (fv-tail t))
  (Locals new-locals s* tail))

(: fv-trivial (D3-Trivial -> D4-Trivial))
(define (fv-trivial triv)
  (match triv
    [(Var u) (Var u)]
    [(Global s) (Global s)]
    [(Code-Label u) (Code-Label u)]
    [(Quote k) (Quote k)]))

(: fv-trivial* (D3-Trivial* -> D4-Trivial*))
(define (fv-trivial* t*)
  (map fv-trivial t*))
