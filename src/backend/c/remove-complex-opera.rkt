#lang typed/racket/base/no-check

(require
 racket/match
 "../../language/make-begin.rkt"
 "../../language/forms.rkt"
 "../../configuration.rkt"
 "../../helpers.rkt")

(provide remove-complex-opera)

(: remove-complex-opera (Data2-Lang -> Data3-Lang))
(define (remove-complex-opera prog)
  (match-define (Prog (list n c t) (GlobDecs d* (Labels b* b))) prog)
  (define uc (make-unique-counter c))
  (define-values (body bnd*)
    (parameterize ([current-unique-counter uc])
      (with-handlers ([(λ a #t) (λ (e) (print b) (raise e))])
        (let* ([body (rco-body b)]
             [bnd* (map rco-bnd-code b*)])
          (values body bnd*)))))
  (Prog (list n (unique-counter-next! uc) t)
    (GlobDecs d* (Labels bnd* body))))

;; Simple recursion into the body of code bindings
(: rco-bnd-code (D2-Bnd-Code -> D3-Bnd-Code))
(define (rco-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    (cons i (Code i* (rco-body b)))))

;; Add new local variables to the declaration to each body
(: rco-body (D2-Body -> D3-Body))
(define (rco-body body)
  (define new-locals : Uid* '())
  (: local-next-uid! : String -> Uid)
  (define (local-next-uid! s)
    (define u (next-uid! s))
    (set! new-locals (cons u new-locals))
    u)
  (: rco-tail (D2-Tail -> D3-Tail))
  (define (rco-tail tail)
    (with-handlers ([(λ a #t)
                     (λ (e) (printf "rco-tail: ~a\n" tail) (raise e))])
      (match tail
        [(If t c a)
         (If (rco-pred t) (rco-tail c) (rco-tail a))]
        [(Switch e c* d)
         (define-values (s* t) (trivialize-value e))
         (make-begin s* (Switch t
                                (map-switch-case* rco-tail c*)
                                (rco-tail d)))]
        [(Begin e* v) (make-begin (rco-effect* e*) (rco-tail v))]
        [(App-Code v v*)
         (define-values (s*1 t)  (trivialize-value v))
         (define-values (s*2 t*) (trivialize-value* v*))
         (make-begin s*1 (make-begin s*2 (Return (App-Code t t*))))]
        [(Op p v*)
         (unless (symbol? p)
           (error 'remove-complex-opera.rkt "Found non-symbol primitive: ~a" p))
         (define-values (s* t*) (trivialize-value* v*))
         (make-begin s* (Return (Op p t*)))]
        [(and cl (Code-Label i)) (Return cl)]
        [(and v  (Var i))        (Return v)]
        [(and g  (Global _))     (Return g)]
        [(and d  (Quote k))      (Return d)]
        [(and h  (Halt))         (Return h)]
        [(and h  (Break-Repeat)) (Return h)]
        [(and s  (Success))      (Return s)]
        [other (error 'remove-complex-opera "unmatched ~a" other)]))
    )
  (: rco-pred (D2-Pred -> D3-Pred))
  (define (rco-pred pred)
    (match pred
      [(Quote c) (Quote c)]
      [(If t c a) (If (rco-pred t) (rco-pred c) (rco-pred a))]
      [(Switch e c* d)
       (define-values (s* t) (trivialize-value e))
       (make-begin s* (Switch t (map-switch-case* rco-pred c*) (rco-pred d)))]
      [(Begin e* v) (make-begin (rco-effect* e*) (rco-pred v))]
      [(Relop p (list (app trivialize-value s** t*) ...))
       (foldr make-begin (Relop p t*) s**)]))
  (: rco-effect (D2-Effect -> D3-Effect))
  (define (rco-effect effect)
    (match effect
      ;; Is this line correct?
      [(Assign i v) (Assign i (rco-value v))]
      [(and h (Halt)) h]
      [(and h (Break-Repeat)) h]
      [(If t c a) (If (rco-pred t) (rco-effect c) (rco-effect a))]
      [(Switch e c* d)
       (define-values (s* t) (trivialize-value e))
       (define s (list (Switch t (map-switch-case* rco-effect c*) (rco-effect d))))
       (make-begin (append s* s) NO-OP)]
      [(Begin e* _) (make-begin (rco-effect* e*) NO-OP)]
      [(Repeat i v1 v2 #f #f e)
       (define-values (s*1 t1) (trivialize-value v1))
       (define-values (s*2 t2) (trivialize-value v2))
       (define u1 (local-next-uid! "tmp_rco"))
       (define u2 (local-next-uid! "tmp_rco"))
       (make-begin
        (append s*1
                s*2
                (list (Assign u1 t1)
                      (Assign u2 t2)
                      (Repeat i (Var u1) (Var u2) #f #f (rco-effect e))))
        NO-OP)]
      [(While e1 e2) (make-begin (list (While (rco-pred e1) (rco-effect e2))) NO-OP)]
      [(App-Code v v*)
       (define-values (s*1 t)  (trivialize-value v))
       (define-values (s*2 t*) (trivialize-value* v*))
       (make-begin (append s*1 s*2 (list (App-Code t t*))) NO-OP)]
      [(Op p v*)
       (unless (symbol? p)
         (error 'remove-complex-opera.rkt "Found non-symbol primitive: ~a" p))
       (define-values (s* t*) (trivialize-value* v*))
       (make-begin (append s* (list (Op p t*))) NO-OP)]
      [(No-Op) NO-OP]
      [other (error 'remove-complex-opera/effect "~a" other)]))
  (: rco-effect* (D2-Effect* -> D3-Effect*))
  (define (rco-effect* effect*) (map rco-effect effect*))
  (: rco-value (D2-Value -> D3-Value))
  (define (rco-value val)
    (match val
      [(If t c a) (If (rco-pred t) (rco-value c) (rco-value a))]
      [(Switch e c* d)
       (define-values (s* t) (trivialize-value e))
       (make-begin s* (Switch t (map-switch-case* rco-value c*) (rco-value d)))]
      [(Begin e* v) (make-begin (rco-effect* e*) (rco-value v))]
      [(App-Code v v*)
       (define-values (s*1 t)  (trivialize-value v))
       (define-values (s*2 t*) (trivialize-value* v*))
       (make-begin s*1 (make-begin s*2 (App-Code t t*)))]
      [(Op p v*)
       (unless (symbol? p)
         (error 'remove-complex-opera.rkt "Found non-symbol primitive: ~a" p))
       (define-values (s* t*) (trivialize-value* v*))
       (make-begin s* (Op p t*))]
      [(and cl (Code-Label i)) cl]
      [(and v  (Var i))        v]
      [(and g  (Global _))     g]
      [(and d  (Quote k))      d]
      [(and h  (Halt))         h]
      [(and h  (Break-Repeat)) h]))
  (: trivialize-value (D2-Value -> (Values D3-Effect* D3-Trivial)))
  (define (trivialize-value value)
    (match value
      [(If t (app trivialize-value c* c) (app trivialize-value a* a))
       (define u (local-next-uid! "tmp_rco"))
       (values
        (list (Assign u (If (rco-pred t)
                            (make-begin c* c)
                            (make-begin a* a))))
        (Var u))]
      [(Switch e c* d)
       (define-values (s* t) (trivialize-value e))
       (define u (local-next-uid! "tmp_rco"))
       (define a
         (Assign u (Switch t (map-switch-case* rco-value c*) (rco-value d))))
       (values (append s* (list a)) (Var u))]
      [(Begin (app rco-effect* e*) v)
       (define-values (t* t) (trivialize-value v))
       (values (append e* t*) t)]
      [(App-Code v v*)
       (define-values (s*1 t) (trivialize-value v))
       (define-values (s*2 t*) (trivialize-value* v*))
       (define u (local-next-uid! "tmp_rco"))
       (values (append s*1 s*2 (list (Assign u (App-Code t t*))))
               (Var u))]
      [(Op p v*)
       (define-values (s* t*) (trivialize-value* v*))
       (define u (local-next-uid! "tmp_rco"))
       (values (append s* (list (Assign u (Op p t*)))) (Var u))]
      ;; This next line seems suspect...
      [(Halt)
       (define u (local-next-uid! "tmp_rco"))
       (values (list (Assign u (Halt))) (Var u))]
      [(and c (Code-Label _)) (values '() c)]
      [(and v (Var _)) (values '() v)]
      [(and g (Global _)) (values '() g)]
      [(and d (Quote _)) (values '() d)]
      [(and d (Break-Repeat)) (values '() d)]))
  (: trivialize-value* (D2-Value* -> (Values D3-Effect* D3-Trivial*)))
  (define (trivialize-value* value*)
    (: trv : D2-Value (Pair D3-Effect* D3-Trivial*)
       -> (Pair D3-Effect* D3-Trivial*))
    (define (trv v a)
      (define-values (e* t) (trivialize-value v))
      (cons (append e* (car a)) (cons t (cdr a))))
    (match-define (cons s* t*)
      (foldr trv '(() . ()) value*))
    (values s* t*))
  
  ;; body of rco-body
  (match-define (Locals i* s* t) body)
  (define tail (rco-tail t))
  (Locals (append new-locals i*) s* tail))


