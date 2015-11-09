#lang typed/racket
(require
 racket/match
 racket/format
 "../../helpers.rkt"
 "../../errors.rkt"
 "../../configuration.rkt"
 "../../language/cast-or-coerce2.rkt"
 "../../language/cast-or-coerce3.rkt")

(provide (all-defined-out))

(define-type Function-Proxy-Rep (U 'Data 'Hybrid))

(define-type Cast-Rule
  (CoC3-Expr (U (Twosome CoC3-Expr CoC3-Expr CoC3-Expr)
                (Coercion CoC3-Expr))
             -> (State Nat CoC3-Expr)))

(define-type ComposeT (CoC3-Expr CoC3-Expr -> (State Nat CoC3-Expr)))

(define-type ApplyT   (CoC3-Expr CoC3-Expr* -> (State Nat CoC3-Expr)))

(define-type Get-Helpers-Type
  (Function-Proxy-Rep
   -> (State Nat (List Cast-Rule ComposeT ApplyT CoC3-Bnd-Code*))))


;; Expr$ and cond$ will prune branches if the condition is (Quote #t) or (Quote #f)
;; and generates If statements (to construct the cast tree)
(: if$ (CoC3-Expr (State Nat CoC3-Expr) (State Nat CoC3-Expr) -> (State Nat CoC3-Expr)))
(define (if$ t c a)
  (if (Quote? t)
      (if (Quote-literal t)
          c
          a)
      (do (bind-state : (State Nat CoC3-Expr))
          (tmp-c : CoC3-Expr <- c)
        (tmp-a : CoC3-Expr <- a)
        (return-state (If t tmp-c tmp-a)))))

;; Pure version of the above
(: if$_ (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (if$_ t c a)
  (if (Quote? t)
      (if (Quote-literal t) c a)
      (If t c a)))

(: and$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (and$ fst snd)
  (if$_ fst snd (Quote #f)))

(: or$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (or$ fst snd)
  (if$_ fst (Quote #t) snd))

(define-syntax cond$
  (syntax-rules (else)
    [(_ (else c)) c]
    [(_ (t c) (t* c*) ...)
     (if$ t c (cond$ (t* c*) ...))]))

;; make sure that t is a trivial expression
(define-type CoC3-Trivial (U (Quote-Coercion Schml-Coercion)
                             (Quote Cast-Literal)
                             (Tag Tag-Symbol)
                             (Type Schml-Type)
                             (Var Uid)))

;(: trivial? (CoC3-Expr -> Boolean : Trivial))
(define (trivial? x)
  (or (Quote? x)
      (Tag? x)
      (Type? x)
      (Var? x)))

(: trivialize (String CoC3-Expr CoC3-Bnd* ->
                      (State Nat (Pair CoC3-Bnd* CoC3-Trivial))))
(define (trivialize s e b*)
  (if (or (Quote? e) (Tag? e) (Type? e) (Var? e))
      (return-state (cons b* e))
      (bind-state
       (uid-state s)
       (lambda ([u : Uid])
         : (State Nat (Pair CoC3-Bnd* (Var Uid)))
         (return-state `(((,u . ,e) . ,b*) . ,(Var u)))))))

;; This is meant to help let$* which accumulates the list of
;; bindings backwards. This gives proper let* behavior
(: let$*-help (CoC3-Bnd* CoC3-Expr -> CoC3-Expr))
(define (let$*-help b* b)
  (if (null? b*)
      b
      (let$*-help (cdr b*) (Let (list (car b*)) b))))

;; creates CoC3-Expr let bindings for non-trivial CoC3-Expr expressions,
;; since non-trivial expressions must be evaluated only once.
;; This has to be a macro because it plays with what value is bound to
;; the t* variable in the code in order to reduce the number of cases
;; that must be handle
(define-syntax let$*
  (syntax-rules ()
    [(_ () b) b]
    [(_ ([t* v*] ...) b)
     ;; I have had some major typechecking problems with sites that
     ;; have multiple cases of this code. On way around this would be
     ;; to stop this nonsense and to make sure that every expression
     ;; is fully evaluated before casting. The thought would then be
     ;; that some uneeded evaluations wouldn't be remove by eliminating
     ;; uneeded results later.
     ;; Another way would be to always allocate the uvar and just throw
     ;; it away if it turns out that the thing is already a trivial value.
     (let ([b* : CoC3-Bnd* '()])
       (do (bind-state : (State Nat CoC3-Expr))
           ((cons b* t*) : (Pair CoC3-Bnd* CoC3-Trivial)
            <- (trivialize (~a 't*) v* b*)) ...
           (b^ : CoC3-Expr <- b)
           (if (null? b*)
               (return-state b^)
               (return-state (let$*-help b* b^)))))]))



;; performs compile time folding of prim = on literals
;; todo convert this to using eq-huh
;; TODO this should be op=?$
(: op=? (-> CoC3-Expr CoC3-Expr CoC3-Expr))
(define (op=? o x)
  (cond
    [(and (Quote? o) (Quote? x) (Quote (eq? (Quote-literal o)
                                            (Quote-literal x))))]
    [(and (Tag? o) (Tag? x)) (Quote (eq? (Tag-bits o) (Tag-bits x)))]
    [(and (Type? o) (Type? x)) (Quote (equal? (Type-type o) (Type-type x)))]
    [else (Op '= (list o x))]))

  ;;These functions type to fold predicates in order to allow ifs to
  ;;generate only checks that are actually needed
(: type-tag (-> CoC3-Expr CoC3-Expr))
(define (type-tag o)
  (if (Type? o)
      (let ([v (Type-type o)])
        (cond
          [(or (Dyn? v) (Int? v) (Bool? v))
           (Tag 'Atomic)]
          [(GRef? v) (Tag 'GRef)]
          [(GVect? v) (Tag 'GVect)]
          [(Fn? v) (Tag 'Fn)]
          [else (error 'interpret-casts/type-tag
                       "Unexpected ~a" v)]))
        (Type-Tag o)))

  (: type-fn-arity (CoC3-Expr -> CoC3-Expr))
  (define (type-fn-arity o)
    (cond
      [(not (Type? o)) (Type-Fn-arity o)]
      [(Fn? (Type-type o)) (Quote (Fn-arity (Type-type o)))]
      [else (error 'ic/type-fn-arity "bad value?: ~a" o)]))

(define-syntax-rule
  (define-smart-coercion? (name compile-time? run-time?) ...)
  (begin
    (: name (CoC3-Expr -> CoC3-Expr)) ...
    (define (name x)
    (if (not (Quote-Coercion? x))
        (run-time? x)
        (let ([x (Quote-Coercion-const x)])
          (if (compile-time? x)
              (Quote #t)
              (Quote #f))))) ...))

(define-smart-coercion?
  (id?$   Identity? Id-Coercion-Huh)
  (seq?$  Sequence? Sequence-Coercion-Huh)
  (prj?$  Project?  Project-Coercion-Huh)
  (inj?$  Inject?   Inject-Coercion-Huh)
  (fail?$ Failed?   Failed-Coercion-Huh)
  (fnC?$  Fn?       Fn-Coercion-Huh)
  (ref?$  Ref?      Ref-Coercion-Huh))

(define-syntax-rule
  (define-smart-access (name check kuote compile-time run-time) ...)
  (begin
    (: name (CoC3-Expr -> CoC3-Expr)) ...
    (define (name x)
      (if (not (Quote-Coercion? x))
          (run-time x)
          (let ([x (Quote-Coercion-const x)])
            (if (check x)
                (kuote (compile-time x))
                (error 'interpret-casts "~a applied to ~a" 'name x)))))
    ...))

(define-smart-access
  (seq-fst$    Sequence?  Quote-Coercion Sequence-fst  Sequence-Coercion-Fst)
  (seq-snd$    Sequence?  Quote-Coercion Sequence-snd  Sequence-Coercion-Snd)
  (prj-type$   Project?   Type           Project-type  Project-Coercion-Type)
  (prj-label$  Project?   Quote          Project-label Project-Coercion-Label)
  (inj-type$   Inject?    Type           Inject-type   Inject-Coercion-Type)
  (ref-read$   Ref?       Quote-Coercion Ref-read      Ref-Coercion-Read)
  (ref-write$  Ref?       Quote-Coercion Ref-write     Ref-Coercion-Write)  
  (fail-label$ Failed?    Quote          Failed-label  Failed-Coercion-Label))

(define-syntax-rule
  (define-smart-coercion (name compile-time run-time field ...) ...)
  (begin
    (define (name (field : CoC3-Expr) ...) : CoC3-Expr
      (if (and (Quote-Coercion? field) ...)
          (Quote-Coercion (compile-time (Quote-Coercion-const field) ...))
          (run-time field ...)))
    ...))

(define-smart-coercion
  (seq$  Sequence Sequence-Coercion fst snd)
  (ref$  Ref      Ref-Coercion      read write))

(: inj$ (CoC3-Expr -> CoC3-Expr))
(define (inj$ type)
  (if (Type? type)
      (let ([t (Type-type type)])
        (Quote-Coercion (Inject t)))
      (Inject-Coercion type)))

(: prj$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (prj$ type label)
  (if (and (Type? type) (Quote? label))
      (let ([t (Type-type type)]
            [s (Quote-literal label)])
        (if (string? s)
            (Quote-Coercion (Project t s))
            (error 'proj$ "given ~a ~a" t s)))
      (Project-Coercion type label)))

(: fail$ (CoC3-Expr -> CoC3-Expr))
(define (fail$ label)
  (cond
    [(not (Quote? label)) (Failed-Coercion label)]
    [else
     (let ([s (Quote-literal label)])
       (if (string? s)
           (Quote-Coercion (Failed s))
           (error 'fail$ "given ~a" s)))]))

(define-syntax-rule
  (define-smart-type? (name compile-time? run-time?) ...)
  (begin
    (: name (CoC3-Expr -> CoC3-Expr)) ...
    (define (name x)
    (if (not (Type? x))
        (run-time? x)
        (let ([x (Type-type x)])
          (if (compile-time? x)
              (Quote #t)
              (Quote #f))))) ...))

(define-smart-type?
  (dyn?$   Dyn?   Type-Dyn-Huh)
  (fnT?$   Fn?    Type-Fn-Huh)
  (gvect?$ GVect? Type-GVect-Huh)
  (gref?$  GRef?  Type-GRef-Huh))

(: fnT-arity$ (CoC3-Expr -> CoC3-Expr))
(define (fnT-arity$ x)
  (if (not (Type? x))
      (Type-Fn-arity x)
      (let ([x (Type-type x)])
        (if (Fn? x)
            (Quote (Fn-arity x))
            (error 'fnT-arity$ "given ~a" x)))))

(: gvect-of$ (CoC3-Expr -> CoC3-Expr))
(define (gvect-of$ x)
  (if (not (Type? x))
      (Type-GVect-Of x)
      (let ([x (Type-type x)])
        (if (GVect? x)
            (Type (GVect-arg x))
            (error 'gvect-of$ "given ~a" x)))))

(: gref-of$ (CoC3-Expr -> CoC3-Expr))
(define (gref-of$ x)
  (if (not (Type? x))
      (Type-GRef-Of x)
      (let ([x (Type-type x)])
        (if (GRef? x)
            (Type (GRef-arg x))
            (error 'gvect-of$ "given ~a" x)))))
