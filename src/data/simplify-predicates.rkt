#lang typed/racket

(require "../language/data4.rkt"
         "../language/data5.rkt"
         (submod "../language/make-begin.rkt" typed)
         "../language/data-representation.rkt"
         "../configuration.rkt"
         "../helpers.rkt")

(provide simplify-predicates)
(: simplify-predicates (Data4-Lang -> Data5-Lang))
(define (simplify-predicates prog)
  (match-define (Prog (list n c t) (GlobDecs d* (Labels b* b))) prog)
  (define uc (make-unique-counter c))
  (define-values (bnd* body)
    (parameterize ([current-unique-counter uc])
      (let* ([body (sp-body b)]
             [bnd* (map sp-bnd-code b*)])
        (values bnd* body))))
  (Prog (list n (unique-counter-next! uc) t) (GlobDecs d* (Labels bnd* body))))

;; Simple recursion into the body of code bindings
(: sp-bnd-code (D4-Bnd-Code -> D5-Bnd-Code))
(define (sp-bnd-code bnd)
  (match-define (cons i (Code i* b)) bnd)
  (cons i (Code i* (sp-body b))))

;; Add new local variables to the declaration to each body
(: sp-body (D4-Body -> D5-Body))
(define (sp-body body)
  (match-define (Locals i* t) body)
  (define new-locals : Uid* i*)
  (: local-next-uid! : String -> Uid)
  (define (local-next-uid! s)
    (define u (next-uid! s))
    (set! new-locals (cons u new-locals))
    u)
  (: sp-tail (D4-Tail -> D5-Tail))
  (define (sp-tail tail)
    (match tail
      [(If t c a)
       (define-values (e* p) (sp-pred t))
       (make-begin e* (If p (sp-tail c) (sp-tail a)))]
      [(Switch t c* d)
       (Switch t (map-switch-case* sp-tail c*) (sp-tail d))]
      [(Begin e* t)
       (make-begin (sp-effect* e*) (sp-tail t))]
      [(and r (Return v))
       (cond
         [(Success? v) r]
         [else (Return (sp-value v))])]))

  ;; (TODO remove relops and just use value in predicate context)
  ;; I think that having relops is not actually necissary  
  (: sp-pred (D4-Pred -> (Values D5-Effect* D5-Pred)))
  (define (sp-pred pred)
    (match pred
      [(If (app sp-pred te* t)
           (app sp-pred ce* c)
           (app sp-pred ae* a))
       (define u (local-next-uid! "tmp_sp"))
       ;; This seems dumb both the if and the snoc we should make
       ;; effectfull begins able to handle having an effect in the
       ;; value position.
       (define ce (Begin (snoc ce* (Assign u (If c TRUE-IMDT FALSE-IMDT))) NO-OP))
       (define ae (Begin (snoc ae* (Assign u (If a TRUE-IMDT FALSE-IMDT))) NO-OP))
       (values (snoc te* (If t ce ae))
               (Relop '= (Var u) TRUE-IMDT))]
      [(Begin (app sp-effect* e*) (app sp-pred pe* p))
       (values (append e* pe*) p)]
      [(Relop p (app sp-trivial t1) (app sp-trivial t2))
       (values '() (Relop p t1 t2))]
      [(Switch (app sp-trivial e) c* (app sp-pred d* d))
       (define u (local-next-uid! "tmp_sp"))
       (define (recur-sc* [rhs : D4-Pred])
         (define-values (e* e) (sp-pred rhs))
         (Begin (snoc e* (Assign u (If e TRUE-IMDT FALSE-IMDT))) NO-OP))
       (define c*^ (map-switch-case* recur-sc* c*))
       (define d^  (Begin (snoc d* (Assign u (If d TRUE-IMDT FALSE-IMDT))) NO-OP))
       (values (list (Switch e c*^ d^)) (Relop '= (Var u) TRUE-IMDT))]
      [o (error 'simplify-predicates/sp-pred "invalid: ~a" o)]))
  (: sp-value (D4-Value -> D5-Value))
  (define (sp-value v)
    (match v
      [(App-Code t t*) (App-Code (sp-trivial t) (sp-trivial* t*))]
      [(Op p t*) (Op p (sp-trivial* t*))]
      ;; typed racket made me do it
      [(and v (Halt)) v]
      [(and v (Var _)) v]
      [(and v (Quote _)) v]
      [(and v (Code-Label _)) v]))
  (: sp-effect (D4-Effect -> D5-Effect*))
  (define (sp-effect effect)
    (match effect
      [(Assign i v) (list (Assign i (sp-value v)))]
      [(If (app sp-pred e* t) (app sp-effect c*) (app sp-effect a*))
       (snoc e* (If t (Begin c* NO-OP) (Begin a* NO-OP)))]
      [(Switch (app sp-trivial t) c* d)
       (define c**
         (for/list : (Switch-Case* (Begin D5-Effect* No-Op)) ([c c*])
           (cons (car c) (Begin (sp-effect (cdr c)) NO-OP))))
       (list (Switch t c** (Begin (sp-effect d) NO-OP)))]
      [(Begin e* _) (sp-effect* e*)]
      [(Repeat i (app sp-trivial t1) (app sp-trivial t2) #f #f e)
       (list (Repeat i t1 t2 #f #f (Begin (sp-effect e) NO-OP)))]
      [(Op p t*) (list (Op p (sp-trivial* t*)))]
      [(and e (or (Halt) (No-Op))) (list e)]
      [(and v (Break-Repeat)) (list v)]
      [other (error 'simplify-predicates/effect "~a" other)]))
  (: sp-effect* (D4-Effect* -> D5-Effect*))
  (define (sp-effect* effect*) (append* (map sp-effect effect*)))
  (: sp-trivial (D4-Trivial -> D5-Trivial))
  (define (sp-trivial t) t)
  (: sp-trivial* (D4-Trivial* -> D5-Trivial*))
  (define (sp-trivial* t*) t*)
  
  (define new-body (sp-tail t))
  (Locals new-locals new-body))


