#lang typed/racket/base/no-check
#|-----------------------------------------------------------------------------+
|Pass: src/casts/interpret-casts                                          |
+------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                             |
+------------------------------------------------------------------------------+
|Description:
- This pass creates a cast-interpreter that can cast arbitrary values at
runtime.
- Converts runtime casts to applications of the cast-interpreter.
- Specializes casts with known types, at this point represented with the cast
form, to the shortest branch of the cast tree that is relevant.
- Replaces types that are structure with a psuedo constructor call
- Replaces tags atomic types with the Type form.
- Introduces the short lived dynamic type manipulation forms
+-----------------------------------------------------------------------------|#

(require
 racket/match
 racket/function
 racket/set
 "../logging.rkt"
 "../configuration.rkt"
 "../language/forms.rkt"
 "../language/syntax.rkt"
 "../language/syntax-with-constants.rkt"
 "../lib/option-set.rkt"
 "./cast-profiler.rkt"
 "./interpret-casts-common.rkt")

(provide interpret-casts/coercions
         cast-runtime-constant-bindings)

(define-type Compose-Coercions/ID/FVS
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Compose-Coercions/FVS
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Make-Coercion/ID/FVS
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

;; Given two type literals and Blame-Label generate the equivalent
;; coercion literal. This function maintains and invarient that a
;; coercion is equivalent to ID then the coercion is represented as
;; ID.  If the keyword argument #:top-level? #t is present and the
;; optimize-first-order-coercions? parameter is true then top-level
;; coercions might not be in normal form.
(: make-coercion
   (->* (Grift-Type Grift-Type Blame-Label)
        (#:top-level? Boolean)
        Grift-Coercion))
(define (make-coercion t1 t2 lbl
                       #:top-level? [top-level? #f])
  ;; mc - make-coercion recusive call
  ;; returns
  ;; 1) A coercions that is equivalent to cast from t1 to t2 with lbl
  ;; 2) os : (Option (Setof Uid))
  ;;    #f if there is already evidence that the coercion has
  ;;    computational content.
  ;;    Otherwise it is the set of free variables of the cast. These
  ;;    free variables must be bound before you can determine if the
  ;;    cast has content.
  (: mc :
     Grift-Type
     Grift-Type
     (HashTable (Pairof Grift-Type Grift-Type) Pre-Mu)
     -> (Values Grift-Coercion (Option (Setof Uid))))
  (define (mc t1 t2 m)
    (: r : Grift-Type Grift-Type -> (Values Grift-Coercion (Option (Setof Uid))))
    (define (r t1 t2) (mc t1 t2 m))
    (match* (t1 t2)
      [(t t) (values IDENTITY (seteq))]
      [((Dyn) _) (values (Sequence (Project t2 lbl) IDENTITY) #f)]
      [(_ (Dyn))  (values (Sequence IDENTITY (Inject t1))      #f)]
      [((Fn n t1* t1) (Fn n t2* t2))
       (define-values (c* id?*)
         ;; Contravarient arguments
         (for/lists ([c* : (Listof Grift-Coercion)]
                     [id?* : (Listof (Option (Setof Uid)))])
                    ([t1 t1*] [t2 t2*])
           (r t2 t1)))
       (define-values (c id?)  (r t1 t2))
       (values (Fn n c* c) (option-set-union id? (option-set-union* id?*)))]
      [((STuple n t1*) (STuple n t2*))
       (define-values (c* id?*)
         (for/lists ([c* : (Listof Grift-Coercion)]
                     [id?* : (Listof (Option (Setof Uid)))])
                    ([t1 t1*] [t2 t2*])
           (r t1 t2)))
       (values (CTuple n c*) (option-set-union* id?*))]
      [((GRef t1)(GRef t2))
       (define-values (t3 id3?) (r t1 t2))
       (define-values (t4 id4?) (r t2 t1))
       (values (Ref t3 t4 'GRef) (option-set-union id3? id4?))]
      [((GVect t1) (GVect t2))
       (define-values (t3 id3?) (r t1 t2))
       (define-values (t4 id4?) (r t2 t1))
       (values (Ref t3 t4 'GVect) (option-set-union id3? id4?))]
      ;; Should we always compute the join in Monotonic.
      ;; Doing so might retain better type information and be more
      ;; likely to be equal to the runtime type info.
      [((MRef t1) (MRef t2))
       (cond
         [(le-precise? t2 t1) (values IDENTITY (seteq))]
         [else (values (MonoRef t2) #f)])]
      [((MVect t1) (MVect t2))
       (cond
         [(le-precise? t2 t1) (values IDENTITY (seteq))]
         [else (values (MonoVect t2) #f)])]
      [(t1 t2) #:when (or (Mu? t1) (Mu? t2))
       (define t1.t2 (cons t1 t2))
       (cond
         [(hash-ref m t1.t2 #f) =>
          (λ ([pm : Pre-Mu])
            (define uid (pre-mu-uid! pm))
            (values (CVar uid) (seteq uid)))]
         [else
          (define pm : Pre-Mu (make-pre-mu))
          (define m^ (hash-set m (cons t1 t2) pm))
          ;; fmvs? = optional-free-mu-vars
          (define-values (c fmvs?)
            (match* (t1 t2)
              [((Mu s) _)
               (define t1^ (grift-type-instantiate s t1))
               (mc t1^ t2 m^)]
              [(_ (Mu s))
               (define t2^ (grift-type-instantiate s t2))
               (mc t1 t2^ m^)])) 
          (define fmvs/v? (option-set-remove fmvs? (Pre-Mu-uid pm)))
          (values 
           (cond
             ;; If there are no free mu vars and there is no evidence
             ;; that the coercion has content then we can conclude it
             ;; is the id coercion.
             [(option-set-empty? fmvs/v?) IDENTITY]
             ;; If the mu binds a variable then we need to build this
             ;; one.
             [(Pre-Mu-uid pm) => (λ ([u : Uid]) (CRec u c))]
             ;; This Mu binding wasn't used therefore we don't need to build it. 
             [else c])
           fmvs/v?)])]
      [(_ _) (values (Failed lbl) #f)]))
  (match* (t1 t2)
    [((Dyn) _) 
     (if (and top-level? (optimize-first-order-coercions?))
         (Project t2 lbl)
         (Sequence (Project t2 lbl) IDENTITY))]
    [(_ (Dyn))
     (if (and top-level? (optimize-first-order-coercions?))
         (Inject t1)
         (Sequence IDENTITY (Inject t1)))]
    [(t1 t2)
     (define-values (c _) (mc t1 t2 (hash)))
     c]))

(module+ test
  (require rackunit)
  (define m1 (Mu (Scope (STuple 2 (list (Int) (Fn 0 '() (TVar 0)))))))
  (define m2 (Mu (Scope (Fn 0 '() (STuple 2 (list (Dyn) (TVar 0)))))))
  (define t1 (Fn 0 '() m1))
  (define t2 (Fn 0 '() (STuple 2 (list (Dyn) m2))))
  (define r  (Uid "mu" 0))
  (define c0 (CRec r (CTuple 2 (list (Sequence IDENTITY (Inject (Int)))
                                     (Fn 0 '() (CVar r))))))
  (define c1 (Fn 0 '() c0))

  (test-equal?
   "mu id collapse bug fix"
   (parameterize ([current-unique-counter (make-unique-counter 0)])
     (make-coercion t1 t2 "1"))
   c1))


(define monolithic-make-coercion? : (Parameterof Boolean)
  (make-parameter #t))
(define make-coercion-inline/both-vars? : (Parameterof Boolean)
  (make-parameter #f))

;; TODO this could be factored out of both coercion implementations
(define (make-compile-make-coercion/make-med-coercion)
  : (Values Compile-Make-Coercion-Type Make-Med-Coercion-Type)
  (define make-coercion-uid (next-uid! "make-coercion"))
  (define make-med-coercion-uid (next-uid! "make-med-coercion"))
  (define mc-assoc-stack-uid (next-uid! "make-coercion-assoc-stack"))
  (define mc-assoc-stack (Var mc-assoc-stack-uid))
  (add-cast-runtime-constant! mc-assoc-stack-uid (op$ make-assoc-stack))
  (: interp-make-coercion Make-Coercion/ID/FVS)
  (define (interp-make-coercion t1 t2 lbl ret-id? ret-fvs)
    (apply-code make-coercion-uid t1 t2 lbl ret-id? ret-fvs))
  (: interp-make-med-coercion Make-Coercion/ID/FVS)
  (define (interp-make-med-coercion t1 t2 lbl ret-id? ret-fvs)
    (apply-code make-med-coercion-uid t1 t2 lbl ret-id? ret-fvs))
  (: interp-make-coercion/id/fvs Make-Coercion-Type)
  (define (interp-make-coercion/id/fvs t1 t2 lbl)
    (let$ ([ret-id? (Unguarded-Box-On-Stack (Quote #t))]
           [ret-fvs (Unguarded-Box-On-Stack (Quote 0))])
      (apply-code make-coercion-uid t1 t2 lbl ret-id? ret-fvs)))
  (: interp-make-med-coercion/id/fvs Make-Coercion-Type)
  (define (interp-make-med-coercion/id/fvs t1 t2 lbl)
    (let$ ([ret-id? (Unguarded-Box-On-Stack (Quote #t))]
           [ret-fvs (Unguarded-Box-On-Stack (Quote 0))])
      (apply-code make-med-coercion-uid t1 t2 lbl ret-id? ret-fvs)))

  (define make-fn-coercion : Make-Coercion/ID/FVS
    (make-make-fn-coercion #:id-coercion? Id-Coercion-Huh
                           #:make-coercion interp-make-coercion))
  (: make-fn-coercion/id/fvs Make-Coercion-Type)
  (define (make-fn-coercion/id/fvs t1 t2 lbl)
    (let$ ([ret-id? (Unguarded-Box-On-Stack (Quote #t))]
           [ret-fvs (Unguarded-Box-On-Stack (Quote 0))])
      (make-fn-coercion t1 t2 lbl ret-id? ret-fvs)))
  
  (define make-tuple-coercion : Make-Coercion/ID/FVS
    (make-make-tuple-coercion #:id-coercion? Id-Coercion-Huh
                              #:make-coercion interp-make-coercion))
  
  (: make-tuple-coercion/id/fvs Make-Coercion-Type)
  (define (make-tuple-coercion/id/fvs t1 t2 lbl)
    (let$ ([ret-id? (Unguarded-Box-On-Stack (Quote #t))]
           [ret-fvs (Unguarded-Box-On-Stack (Quote 0))])
      (make-tuple-coercion t1 t2 lbl ret-id? ret-fvs)))
  
  ;; Invariant: `t1` is not pointer equal to `t2` the
  ;; `compile-med-coercion` function takes care of this case But `t1`
  ;; can be equivalent to `t2` via equirecursive equality.

  
  ;; This follows the logic in the compile time version of this function
  ;; except that a set of free variables isn't maintained for efficiency
  ;; reasons. Instead we use a counter that is only incremented/decremented
  ;; the first time a mu is allocated/closed. 
  (: code-gen-make-med-coercion Make-Coercion/ID/FVS)
  (define (code-gen-make-med-coercion t1 t2 lbl ret-id? ret-fvs)
    (cond$
     ;; Checking for Mu now allows all other cases to ignore
     ;; that they exist. 
     [(or$ (Type-Mu-Huh t1) (Type-Mu-Huh t2))
      (let$ ([i (op$ assoc-stack-find mc-assoc-stack t1 t2)])
        (cond$
         ;; If we have already cast between these t1 and t2 then we 
         ;; can create a back-edge in the coercion to the Mu node
         ;; that performed those casts.  Because we are checking
         ;; equirecursive equations at the same time that we are
         ;; making coercions we may decide at the top of the Mu that
         ;; this entire Mu isn't needed if it is equivalent to id?
         [(op$ >= i ZERO-EXPR)
          (ann
           (let$ ([pm (op$ assoc-stack-ref mc-assoc-stack i)])
            (cond$
             ;; We have already allocated the mu
             [(op$ not (op$ = ZERO-EXPR pm)) pm]
             ;; We haven't allocated the Mu so we need to do that
             [else
              (let$ ([pm (Make-Mu-Coercion)])
                (op$ assoc-stack-set! mc-assoc-stack i pm)
                (Unguarded-Box-Set!
                 ret-fvs
                 (op$ + (Unguarded-Box-Ref ret-fvs) (Quote 1)))
                pm)]))
           CoC3-Expr)] 
         [else
          (op$ assoc-stack-push! mc-assoc-stack t1 t2 ZERO-EXPR)
          ;; new-id? is a out parameter to interp-make-coercion
          ;; We make a new-id? because the input id?-box might
          ;; already be false, and we want a chance to collapse
          ;; this mu.
          (ann
           (let*$ ([new-id? (Unguarded-Box-On-Stack (Quote #t))]
                   ;; Could we make this `interp-make-med-coercion`
                   [c  (cond$
                        [(Type-Mu-Huh t1)
                         (interp-make-coercion
                          (Type-Mu-Body t1) t2 lbl new-id? ret-fvs)]
                        [else
                         (interp-make-coercion
                          t1 (Type-Mu-Body t2) lbl new-id? ret-fvs)])]
                   [mu (op$ assoc-stack-pop! mc-assoc-stack)]
                   [fv-count (Unguarded-Box-Ref ret-fvs)]
                   [id?      (Unguarded-Box-Ref new-id?)])
             (unless$ id? (Unguarded-Box-Set! ret-id? (Quote #f))) 
             (ann
              (cond$
               ;; This mu binding was never used in the body
               [(op$ = ZERO-EXPR mu) c]
               [else
                (Unguarded-Box-Set! ret-fvs (op$ - fv-count (Quote 1)))
                (cond$
                 ;; if ret-fvs is 0 and id? is still true then we can
                 ;; conclude this mu is equivalent to identity
                 [(and$ id? (op=? fv-count (Quote 1))) ID-EXPR]
                 [else
                  (Mu-Coercion-Body-Set! mu c)
                  mu])])
              CoC3-Expr))
           CoC3-Expr)]))]
     [(and$ (type-fn?$ t1) (type-fn?$ t2)
            (op=? (type-fn-arity$ t1) (type-fn-arity$ t2)))
      (make-fn-coercion t1 t2 lbl ret-id? ret-fvs)]
     [(and$ (type-tup?$ t1) (type-tup?$ t2)
            (op<=? (type-tup-arity$ t2) (type-tup-arity$ t1)))
      (make-tuple-coercion t1 t2 lbl ret-id? ret-fvs)]
     [(and$ (type-pvec?$ t1) (type-pvec?$ t2))
      (let*$ ([pvof1 (type-pvec-of$ t1)]
              [pvof2 (type-pvec-of$ t2)]
              [read_crcn  (interp-make-coercion pvof1 pvof2 lbl ret-id? ret-fvs)]
              [write_crcn (interp-make-coercion pvof2 pvof1 lbl ret-id? ret-fvs)])
        (cond$
         [(and$ (op=? read_crcn  ID-EXPR)
                (op=? write_crcn ID-EXPR))
          ID-EXPR]
         [else (vec-coercion$ read_crcn write_crcn)]))]
     [(and$ (type-pbox?$ t1) (type-pbox?$ t2))
      (ann (let*$ ([pbof1 (type-pbox-of$ t1)]
                   [pbof2 (type-pbox-of$ t2)]
                   [read_crcn  (interp-make-coercion
                                pbof1 pbof2 lbl ret-id? ret-fvs)]
                   [write_crcn (interp-make-coercion
                                pbof2 pbof1 lbl ret-id? ret-fvs)])
             (cond$
              [(and$ (op=? read_crcn  ID-EXPR)
                     (op=? write_crcn ID-EXPR))
               ID-EXPR]
              [else (ref-coercion$ read_crcn write_crcn)]))
           CoC3-Expr)]
     [(and$ (type-mvec?$ t1) (type-mvec?$ t2))
      (ann (let$ ([t (type-mvec-of$ t2)])
             (cond$
              [(type-dyn?$ t2) ID-EXPR]
              [else
               (Unguarded-Box-Set! ret-id? (Quote #f))
               (MVect-Coercion t)]))
           CoC3-Expr)]
     [(and$ (type-mbox?$ t1) (type-mbox?$ t2))
      (ann (let$ ([t (type-mbox-of$ t2)])
             (cond$
              [(type-dyn?$ t2) ID-EXPR]
              [else
               (Unguarded-Box-Set! ret-id? (Quote #f))
               (MRef-Coercion t)]))
           CoC3-Expr)]
     [else (Failed-Coercion lbl)]))

  (: compile-make-med-coercion Make-Med-Coercion-Type)
  (define (compile-make-med-coercion t1 t2 lbl #:know-not-eq? [know-not-eq? #f])
    (: r : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    ;; The real inspection but it expects all expressions to be literals
    ;; or variables 
    (define (r t1 t2 lbl)
      (match* (t1 t2)
        [((Type t1-t) (Type t2-t))
         ;; This code may run if the blame label isn't known for some
         ;; reason. I don't think it is necesary, but I am not certain
         ;; that it isn't needed.
         ;; -- We also know that these types will not result in an id
         ;;    coercion because of the check that t1 <:_eq t2.
         (match* (t1-t t2-t)
           [(t t) ID-EXPR]
           [((Fn n _ _) (Fn n _ _))
            (make-fn-coercion/id/fvs t1 t2 lbl)]
           [((STuple n _) (STuple m _)) #:when (<= n m)
            (make-tuple-coercion/id/fvs t1 t2 lbl)]
           [((GRef t1-t) (GRef t2-t))
            (define t1 (Type t1-t))
            (define t2 (Type t2-t))
            (ref-coercion$
             (interp-make-coercion/id/fvs t1 t2 lbl)
             (interp-make-coercion/id/fvs t2 t1 lbl))]
           [((GVect t1-t) (GVect t2-t))
            (define t1 (Type t1-t))
            (define t2 (Type t2-t))
            (vec-coercion$
             (interp-make-coercion/id/fvs t1 t2 lbl)
             (interp-make-coercion/id/fvs t2 t1 lbl))]
           [((MRef _) (MRef t2))
            (Quote-Coercion (MonoRef t2))]
           [((MVect _) (MVect t2))
            (Quote-Coercion (MonoVect t2))]
           ;; TODO consider doing better
           [(_ _) (interp-make-med-coercion/id/fvs t1 t2 lbl)])]
        [((Type t1-t) t2)
         (match t1-t
           ;; in each of these cases t2 could be a mu
           ;; todo consider doing better
           [(Fn n _ _)
            (If (and$ (type-fn?$ t2) (op=? (type-fn-arity$ t2) (Quote n)))
                (make-fn-coercion/id/fvs t1 t2 lbl)
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(STuple n _)
            (If (and$ (type-tup?$ t2) (op=? (type-tup-arity$ t2) (Quote n)))
                (make-tuple-coercion/id/fvs t1 t2 lbl)
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(GRef t1-t)
            (define t1 (Type t1-t))
            (If (type-pbox?$ t2)
                (let$ ([t2 (type-pbox-of$ t2)])
                  (ref-coercion$
                   (interp-make-coercion/id/fvs t1 t2 lbl)
                   (interp-make-coercion/id/fvs t2 t1 lbl)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(GVect t1-t)
            (define t1 (Type t1-t))
            (If (type-pvec?$ t2)
                (let$ ([t2 (type-pvec-of$ t2)])
                  (vec-coercion$ (interp-make-coercion/id/fvs t1 t2 lbl)
                                 (interp-make-coercion/id/fvs t2 t1 lbl)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(MRef _)
            (If (type-mbox?$ t2)
                (MRef-Coercion (type-mbox-of$ t2))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(MVect _)
            (If (type-mvec?$ t2)
                (MVect-Coercion (type-mvec-of$ t2))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [_ (interp-make-med-coercion/id/fvs t1 t2 lbl)])]
        [(t1 (Type t2-t))
         (match t2-t
           [(Fn n _ _)
            (If (and$ (type-fn?$ t1) (op=? (type-fn-arity$ t1) (Quote n)))
                (make-fn-coercion/id/fvs t1 t2 lbl)
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(STuple n _)
            (If (and$ (type-tup?$ t1) (op=? (type-tup-arity$ t1) (Quote n)))
                (make-tuple-coercion/id/fvs t1 t2 lbl)
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(GRef t2-t)
            (define t2 (Type t2-t))
            (If (type-pbox?$ t1)
                (let$ ([t1 (type-pbox-of$ t1)])
                  (ref-coercion$
                   (interp-make-coercion/id/fvs t1 t2 lbl)
                   (interp-make-coercion/id/fvs t2 t1 lbl)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(GVect t2-t)
            (define t2 (Type t2-t))
            (If (type-pvec?$ t1)
                (let$ ([t1 (type-pvec-of$ t1)])
                  (vec-coercion$
                   (interp-make-coercion/id/fvs t1 t2 lbl)
                   (interp-make-coercion/id/fvs t2 t1 lbl)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(MRef t2-t)
            (If (type-mbox?$ t1)
                (MRef-Coercion (Type t2-t))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [(MVect t2-t)
            (If (type-mvec?$ t1)
                (MVect-Coercion (Type t2-t))
                (interp-make-med-coercion/id/fvs t1 t2 lbl))]
           [_ (interp-make-med-coercion/id/fvs t1 t2 lbl)])]
        [(t1 t2) (interp-make-med-coercion/id/fvs t1 t2 lbl)]))
    ;; Bind non-values and decide to be eq? check 
    (bind-value$
     ([t1 t1] [t2 t2] [lbl lbl])
     (match* (t1 t2)
       [(t t) ID-EXPR]
       [((Type t1) (Type t2)) #:when (Quote? lbl)
        (match-define (Quote lit) lbl)
        (unless (string? lit)
          (error 'compile-make-med-coercion "expected a string got: ~v" lit))
        (Quote-Coercion (make-coercion t1 t2 lit #:top-level? #f))]
       [((Type _) (Type _)) (r t1 t2 lbl)]
       [(_ _) #:when know-not-eq? (r t1 t2 lbl)]
       [(_ _) (If (op=? t1 t2) ID-EXPR (r t1 t2 lbl))])
     ))
  
  (add-cast-runtime-binding!
   make-med-coercion-uid
   (code$ (t1 t2 lbl ret-id? ret-fvs)
     (code-gen-make-med-coercion t1 t2 lbl ret-id? ret-fvs)))
  
  (add-cast-runtime-binding!
   make-coercion-uid
   (code$ (t1 t2 lbl ret-id? ret-fvs)
     (cond$
      [(op=? t1 t2) (Quote-Coercion (Identity))]
      ;; This is absolutly necisarry
      ;; While Injections and Projections are never made by
      ;; source code coercions composition can create new
      ;; projections and injections
      [(type-dyn?$ t1)
       (Unguarded-Box-Set! ret-id? (Quote #f))
       (Sequence-Coercion (Project-Coercion t2 lbl)
                          (Quote-Coercion (Identity)))]
      [(type-dyn?$ t2)
       (Unguarded-Box-Set! ret-id? (Quote #f))
       (Sequence-Coercion (Quote-Coercion (Identity))
                          (Inject-Coercion t1))]
      [else
       (cond
         [(monolithic-make-coercion?)
          (code-gen-make-med-coercion t1 t2 lbl ret-id? ret-fvs)]
         [else
          (interp-make-med-coercion t1 t2 lbl ret-id? ret-fvs)])])))
  
  ;; This is only applied to source level make-coercions at the top level
  ;; ie there are know recursive calls to compile-make-coercion therefore
  ;; Projects and Injects returned from compile make-coercion do not
  ;; have to be in space-efficient normal-form.
  (: compile-make-coercion Compile-Make-Coercion-Type)
  (define (compile-make-coercion t1 t2 lbl
                                 #:top-level? [top-level? #t]
                                 #:know-not-eq? [know-not-eq? #f])
    (: r : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    ;; r expects t1 t2 lbl to be either literals or vars
    (define (r t1 t2 lbl)
      (match* (t1 t2)
        [(t t) (Quote-Coercion (Identity))]
        [((Type (Dyn)) _)
         (let ([prj (Project-Coercion t2 lbl)])
           (if (and top-level? (optimize-first-order-coercions?))
               prj
               (Sequence-Coercion prj ID-EXPR)))]
        [(_ (Type (Dyn)))
         (let ([inj (Inject-Coercion t1)])
           (if (and top-level? (optimize-first-order-coercions?))
               inj
               (Sequence-Coercion ID-EXPR inj)))]
        ;; From here on any use of the type pattern is used to rule out
        ;; that variable from being dynamic
        [((Type _) (Type _))
         (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)]
        [((Var _) (Type _))
         (cond$
          [(type-dyn?$ t1) (r DYN-EXPR t2 lbl)]
          [else (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)])]
        [((Type _) (Var _))
         (cond$
          [(type-dyn?$ t2) (r t1 DYN-EXPR lbl)]
          [else (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)])]
        [((Var _) (Var _))
         (cond
           [(make-coercion-inline/both-vars?) 
            (cond$
             [(type-dyn?$ t1) (r DYN-EXPR t2 lbl)]
             [(type-dyn?$ t2) (r t1 DYN-EXPR lbl)]
             ;; There is no more information to specialize on
             [else (interp-make-med-coercion/id/fvs t1 t2 lbl)])]
           [else (interp-make-coercion/id/fvs t1 t2 lbl)])]))
    ;; bind all non-literals and decide to check for eq?
    (define ret
      (bind-value$
       ([t1 t1] [t2 t2] [lbl lbl])
       (match* (t1 t2)
         [(t t) ID-EXPR]
         [((Type t1) (Type t2)) #:when (Quote? lbl)
          (match-define (Quote lit) lbl)
          (unless (string? lit)
            (error 'compile-make-med-coercion "expected a string got: ~v" lit))
          (Quote-Coercion (make-coercion t1 t2 lit #:top-level? top-level?))]
         [((Type _) (Type _)) ;; lbl is an expression
          ;; Two types that are not equal do not need an eq check
          (r t1 t2 lbl)]
         [((Var _) (Var _)) (interp-make-coercion/id/fvs t1 t2 lbl)]
         [(_ _) #:when know-not-eq? (r t1 t2 lbl)]
         [(_ _) (If (op=? t1 t2) ID-EXPR (r t1 t2 lbl))])))
    ret)
  (values compile-make-coercion compile-make-med-coercion))

(: make-apply-coercion-runtime!
   (->* (#:apply-coercion-uid Uid
         #:project Project-Type
         #:inject  Inject-Type
         #:apply-fn-coercion  Apply-Coercion-Type
         #:apply-tup-coercion Apply-Coercion-Type
         #:apply-pref-coercion Apply-Coercion-Type
         #:apply-pvec-coercion Apply-Coercion-Type
         #:mbox-cast Monotonic-Cast-Type
         #:mvec-cast Monotonic-Cast-Type)
        Void))
(define (make-apply-coercion-runtime!
         #:apply-coercion-uid apply-coercion-uid
         #:project project
         #:inject  inject
         #:apply-fn-coercion apply-fn-coercion
         #:apply-tup-coercion apply-tup-coercion
         #:apply-pref-coercion apply-pref-coercion
         #:apply-pvec-coercion apply-pvec-coercion
         #:mbox-cast mbox-cast
         #:mvec-cast mvec-cast)

  (add-cast-runtime-binding!
   apply-coercion-uid
   (code$ (v c suspend-monotonic-heap-casts?)
     (cond$
      [(Id-Coercion-Huh c) v]
      [(Sequence-Coercion-Huh c)
       (let*$ ([seq-fst (Sequence-Coercion-Fst c)]
               [seq-snd (Sequence-Coercion-Snd c)])
         ;; This is a racket conditional
         (cond
           ;; TODO make sure the original code works then move to this
           #;
           [(coercions-are-space-efficient?)
            (precondition (or$ (Project-Coercion-Huh seq-fst)
                               (Inject-Coercion-Huh seq-snd)))
            (cond$
             [(Project-Coercion-Huh seq-fst)
              (let$ ([v (project v
                                 (Project-Coercion-Type seq-fst)
                                 (Project-Coercion-Label seq-fst) mt)])
                (cond$
                 [(Id-Coercion-Huh seq-snd) v]
                 [else (apply-code apply-coercion-uid v seq-snd mt)]))]
             [else ;; Must be inject coercion
              (let$ ([v (cond$
                         [(Id-Coercion-Huh seq-fst) v]
                         [else (apply-code apply-coercion-uid v seq-fst mt)])])
                (inject v (Inject-Coercion-Type c)))])]
           [else
            (let$ ([v (apply-code apply-coercion-uid v seq-fst suspend-monotonic-heap-casts?)])
              (apply-code apply-coercion-uid v seq-snd suspend-monotonic-heap-casts?))]))]
      [(Project-Coercion-Huh c)
       (project v (Project-Coercion-Type c) (Project-Coercion-Label c) suspend-monotonic-heap-casts?)]
      [(Inject-Coercion-Huh c)
       (inject v (Inject-Coercion-Type c))]
      [(Mediating-Coercion-Huh c)
       (cond$
        [(Fn-Coercion-Huh c) (apply-fn-coercion v c)]
        [(Tuple-Coercion-Huh c)
         (apply-tup-coercion v c suspend-monotonic-heap-casts?)]
        [(Mu-Coercion-Huh c)
         (apply-code apply-coercion-uid v (Mu-Coercion-Body c) suspend-monotonic-heap-casts?)]
        [(Ref-Coercion-Huh c)
         (if (cast-profiler?)
             (cond$
              [(Ref-Coercion-Ref-Huh c) (apply-pref-coercion v c)]
              [else (apply-pvec-coercion v c)])
             (apply-pref-coercion v c))] 
        [(MRef-Coercion-Huh c) (mbox-cast v (MRef-Coercion-Type c) suspend-monotonic-heap-casts?)]
        [(MVect-Coercion-Huh c) (mvec-cast v (MRef-Coercion-Type c) suspend-monotonic-heap-casts?)]
        [else (Blame (Quote "bad implemention of mediating coercions"))])]
      ;; the coercion must be failure
      [else
       (precondition$ (Failed-Coercion-Huh c)
         (Blame (Failed-Coercion-Label c)))]))))

(: make-compose-coercions
   (->* (#:make-coercion Make-Coercion-Type
         #:greatest-lower-bound (Code-Label Uid))
        (Values Uid Compose-Coercions-Type)))
;; TODO Make sure compose-coercions-uid isn't needed anymore and get rid of it
(define (make-compose-coercions #:make-coercion make-coercion
                                #:greatest-lower-bound greatest-lower-bound)
  (define compose-coercions-uid (next-uid! "compose-coercions"))
  (define cc-assoc-stack-uid (next-uid! "compose-coercions-assoc-stack"))
  (define cc-assoc-stack (Var cc-assoc-stack-uid))
  (add-cast-runtime-constant! cc-assoc-stack-uid (op$ make-assoc-stack))
  (: compose-coercions Compose-Coercions/ID/FVS)
  (define (compose-coercions c1 c2 ret-id? ret-fvs)
    (apply-code compose-coercions-uid c1 c2 ret-id? ret-fvs))
  (: compose-coercions/id Compose-Coercions/FVS)
  (define (compose-coercions/id c1 c2 ret-fvs)
    (let$ ([unused-id?-ret-box (Unguarded-Box-On-Stack (Quote #t))])
      (apply-code compose-coercions-uid c1 c2 unused-id?-ret-box ret-fvs)))
  (: compose-coercions/id/fvs Compose-Coercions-Type)
  (define (compose-coercions/id/fvs c1 c2)
    (let$ ([unused-id?-ret-box (Unguarded-Box-On-Stack (Quote #t))]
           [fv-count-ret-box   (Unguarded-Box-On-Stack (Quote 0))])
      (apply-code compose-coercions-uid c1 c2
                  unused-id?-ret-box fv-count-ret-box)))
  (: compose-fn-coercions Compose-Coercions/ID/FVS)
  (define compose-fn-coercions
    (make-compose-fn-coercions #:id-coercion? Id-Coercion-Huh
                               #:compose-coercions compose-coercions))
  (: compose-tup-coercions Compose-Coercions/ID/FVS)
  (define compose-tup-coercions
    (make-compose-tup-coercions #:id-coercion? Id-Coercion-Huh
                                #:compose-coercions compose-coercions))
  (add-cast-runtime-binding!
   compose-coercions-uid
   ;; The precondition of this function is that all coercions
   ;; equivalent to the identity coercion have already been colapsed
   ;; to the identity coercion.
   ;; `id?-ret-box` is an out parameter that starts out as true and
   ;; needs to be set to false if the returned coercion isn't
   ;; equivalent to the identity coercion. It is set during or after
   ;; any recursive calls to compose-because, because doing so allows
   ;; composition of mediating coercions to use the flag to check
   ;; if they created identity casts.
   ;; the return is a coercion equivalent to applying `c1` then `c2`.

   ;; TODO break this function up so there is a compose-med-coercions
   ;; Rational: the only meaningfull coercions under a Mu are also
   ;; med-coercions being able to dispatch there quicker makes sense
   (code$ (c1 c2 ret-id? ret-fvs)
     (cond$
      ;; Eliminating the Identities cuts down on the number of branches
      ;; and should be fast
      [(id-coercion?$ c1)
       (if (id-coercion?$ c2)
           (Quote '())
           (if (enable-tail-coercion-composition?)
               (when$ (not$ (op$ = (Quote 0) ret-id?)) (Unguarded-Box-Set! ret-id? (Quote #f)))
               (Unguarded-Box-Set! ret-id? (Quote #f))))
       c2]
      [(id-coercion?$ c2)
       ;; We know that c1 isn't id because of the test above
       c1]
      ;; We could elminate failure on the left next, but we choose to make
      ;; success as fast as possible even if it introduces more branches overall.
      [(seq-coercion?$ c1)
       (let*$ ([seq_fst (seq-coercion-fst$ c1)]
               [seq_snd (seq-coercion-snd$ c1)])
         (cond$
          [(prj-coercion?$ seq_fst)
           (let*$ ([comp_seq_snd (compose-coercions seq_snd c2 ret-id? ret-fvs)])
             (if (enable-tail-coercion-composition?)
                 (when$ (not$ (op$ = (Quote 0) ret-id?)) (Unguarded-Box-Set! ret-id? (Quote #f)))
                 (Unguarded-Box-Set! ret-id? (Quote #f)))
             (seq-coercion$ seq_fst comp_seq_snd))]
          ;; We have to prioritize failure on the right over injection
          [(failed-coercion?$ c2)
           (if (enable-tail-coercion-composition?)
               (when$ (not$ (op$ = (Quote 0) ret-id?)) (Unguarded-Box-Set! ret-id? (Quote #f)))
               (Unguarded-Box-Set! ret-id? (Quote #f)))
           c2]
          ;; Because of the typeing rule for coercions we know that
          ;; c2 must be a (I?;i) aka projection sequence because
          ;; it must have the type dyn => I.
          ;; Therefore we are doing (m;I1!) ;; (I2?;i)
          [else
           (let*$ ([proj  (seq-coercion-fst$ c2)]
                   [final (seq-coercion-snd$ c2)]
                   [t1    (inj-coercion-type$ seq_snd)]
                   [t2    (prj-coercion-type$ proj)]
                   [lbl   (prj-coercion-label$ proj)]
                   [comp_prj_inj (make-coercion t1 t2 lbl)]
                   [comp_fst_pi
                    (compose-coercions/id seq_fst comp_prj_inj ret-fvs)])
             ;; Only the final compose call should factor into the
             ;; ret-id? .  Coercions created in the call to
             ;; make-coercion and first compose-coercions calls can be
             ;; eliminated in subsequent composes.
             (compose-coercions comp_fst_pi final ret-id? ret-fvs))]))]
      [(seq-coercion?$ c2)
       (cond$
        [(failed-coercion?$ c1)
         (if (enable-tail-coercion-composition?)
             (when$ (not$ (op$ = (Quote 0) ret-id?)) (Unguarded-Box-Set! ret-id? (Quote #f)))
             (Unguarded-Box-Set! ret-id? (Quote #f)))
         c1]
        [else
         ;; must be c1 & (g;I?)
         (let*$ ([seq_fst (seq-coercion-fst$ c2)]
                 [seq_snd (seq-coercion-snd$ c2)]
                 [comp_c1_final (compose-coercions c1 seq_fst ret-id? ret-fvs)])
           (if (enable-tail-coercion-composition?)
               (when$ (not$ (op$ = (Quote 0) ret-id?)) (Unguarded-Box-Set! ret-id? (Quote #f)))
               (Unguarded-Box-Set! ret-id? (Quote #f)))
           (seq-coercion$ comp_c1_final seq_snd))])]
      ;; All Branches from here out will have to check if c2 is failure
      ;; so we do it once to eliminate the possibility
      [(failed-coercion?$ c2)
       (if (enable-tail-coercion-composition?)
           (when$ (not$ (op$ = (Quote 0) ret-id?)) (Unguarded-Box-Set! ret-id? (Quote #f)))
           (Unguarded-Box-Set! ret-id? (Quote #f)))
       (If (failed-coercion?$ c1)
           c1
           c2)]
      [(med-coercion?$ c1)
       (cond$
        [(or$ (Mu-Coercion-Huh c1) (Mu-Coercion-Huh c2))
         (let$ ([i (op$ assoc-stack-find cc-assoc-stack c1 c2)])
           (cond$
            ;; We haven't previously composed this pair of coercions
            [(op$ < i ZERO-EXPR)
             (op$ assoc-stack-push! cc-assoc-stack c1 c2 ZERO-EXPR)
             (let*$ ([new-id? (Unguarded-Box-On-Stack (Quote #t))]
                     [c (cond$
                         [(Mu-Coercion-Huh c1)
                          (compose-coercions
                           (Mu-Coercion-Body c1) c2 new-id? ret-fvs)]
                         [else
                          (compose-coercions
                           c1 (Mu-Coercion-Body c2) new-id? ret-fvs)])]
                     [mu (op$ assoc-stack-pop! cc-assoc-stack)]
                     [id? (Unguarded-Box-Ref new-id?)]
                     [fv-count (Unguarded-Box-Ref ret-fvs)])
               (unless$ id? (Unguarded-Box-Set! ret-id? (Quote #f))) 
               (cond$
                 ;; This mu binding was never used in the body
                 [(op$ = ZERO-EXPR mu) c]
                 [else
                  (Unguarded-Box-Set! ret-fvs (op$ - fv-count (Quote 1)))
                  (cond$
                   ;; if ret-fvs is 0 and id? is still true then we can
                   ;; conclude this mu is equivalent to identity
                   [(and$ id? (op=? fv-count (Quote 1))) ID-EXPR]
                   [else
                    (Mu-Coercion-Body-Set! mu c)
                    mu])]))]
            ;; We have previously composed this pair of coercions
            ;; and `i` may point to a recursive coercion that will
            ;; represent the result of composing.
            [else 
             (let$ ([mu (op$ assoc-stack-ref cc-assoc-stack i)])
               (cond$
                ;; No one else has used this mu yet so we have to
                ;; allocate it to comunicate back up the stack that
                ;; we need it to point to the coercion we want to
                ;; exist here too.
                [(op$ = ZERO-EXPR mu)
                 (Unguarded-Box-Set!
                  ret-fvs
                  (op$ + (Unguarded-Box-Ref ret-fvs) (Quote 1))) 
                 (let$ ([mu (Make-Mu-Coercion)])
                   (op$ assoc-stack-set! cc-assoc-stack i mu)
                   mu)]
                ;; Otherwise we just use the mu that someone else
                ;; made, it will point to the coercion we need here.
                [else mu]))]))]               
        [(fn-coercion?$ c1) ;; c2 must be a Function Coercion
         (compose-fn-coercions c1 c2 ret-id? ret-fvs)]
        [(ref-coercion?$ c1) ;; c2 must also be a reference coercion
         (let*$ ([ref1_read  (ref-coercion-read$  c1)]
                 [ref1_write (ref-coercion-write$ c1)]
                 [ref2_read  (ref-coercion-read$  c2)]
                 [ref2_write (ref-coercion-write$ c2)]
                 [read  (compose-coercions ref1_read  ref2_read
                                           ret-id? ret-fvs)]
                 [write (compose-coercions ref2_write ref1_write
                                           ret-id? ret-fvs)])
           (If (and$ (id-coercion?$ read)
                     (id-coercion?$ write))
               ID-EXPR
               (ref-coercion$ read write)))]
        [(tup-coercion?$ c1)
         (compose-tup-coercions c1 c2 ret-id? ret-fvs)]
        [(mbox-coercion?$ c1)
         (let*$ ([ref1_type  (mbox-coercion-type$ c1)]
                 [ref2_type  (mbox-coercion-type$ c2)]
                 [type3  (app-code$ greatest-lower-bound ref1_type ref2_type)])
           (mbox-coercion$ type3))]
        [(mvec-coercion?$ c1)
         (let*$ ([ref1_type  (mvec-coercion-type$ c1)]
                 [ref2_type  (mvec-coercion-type$ c2)]
                 [type3  (app-code$ greatest-lower-bound ref1_type ref2_type)])
           (mvec-coercion$ type3))]
        [else (Blame (Quote "bad implemention of mediating coercions"))])]
      ;; C1 must be a failed coercion
      [else (Blame (failed-coercion-label$ c1))])))
  ;; For now we are not even trying to be good at compiling coercion composition
  (values compose-coercions-uid compose-coercions/id/fvs))

(: interpret-casts/coercions : -> (C0-Expr -> (Values CoC3-Expr Uid Uid)))
(define (interpret-casts/coercions)
  (define greatest-lower-bound (make-compile-types-greatest-lower-bound))
  (define-values (compile-make-coercion compile-make-med-coercion)
    (make-compile-make-coercion/make-med-coercion))
  (define-values (compose-coercions-uid compose-coercions)
    (make-compose-coercions #:make-coercion compile-make-coercion
                            #:greatest-lower-bound greatest-lower-bound))
  (define apply-coercion-uid (next-uid! "apply-coercion"))
  (define (apply-coercion [v : CoC3-Expr] [c : CoC3-Expr] [suspend-monotonic-heap-casts? : CoC3-Expr do-not-suspend-monotonic-heap-casts])
    : CoC3-Expr
    (apply-code apply-coercion-uid v c suspend-monotonic-heap-casts?))

  (define get-fn-cast!
    (make-fn-cast-helpers
     (make-build-caster/coercions
      #:apply-coercion-uid apply-coercion-uid
      #:compose-coercions-uid compose-coercions-uid
      #:compose-coercions compose-coercions
      #:id-coercion-huh   Id-Coercion-Huh)))
  
  (: compile-lambda Lambda-Type)
  (define (compile-lambda fml* e)
    (define arity (length fml*))
    (define ctr (get-fn-cast! (if (enable-tail-coercion-composition?) (+ 1 arity) arity)))
    (Lambda fml* (Castable ctr (cast-profile/max-function-chain$ e))))
  
  (: compile-app App-Type)
  (define (compile-app e e*)
    (App-Fn-or-Proxy apply-coercion-uid compose-coercions-uid e e*))
  
  (define compile-fn-cast/coercions
    (make-compile-fn-cast/coercions
     #:get-fn-cast! get-fn-cast!
     #:make-fn-coercion compile-make-coercion))

  (define compile-types-greatest-lower-bound
    (make-compile-types-greatest-lower-bound))
  
  (define compile-apply-fn-coercion
    (make-compile-apply-fn-coercion #:get-fn-cast! get-fn-cast!))
  
  (define compile-apply-tup-coercion
    (make-compile-apply-tuple-coercion
     #:apply-coercion-uid apply-coercion-uid))
  
  (define compile-apply-pref-coercion
    (make-compile-apply-pref-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? Id-Coercion-Huh))

  (define compile-apply-pvec-coercion
    (make-compile-apply-pvec-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? Id-Coercion-Huh))
  
  (define compile-inject (make-compile-inject))

  ;; Interp-Cast Builds a call to a runtime function
  ;; that casts based on types.
  ;; Compile cast specializes based on types
  (define-values (interp-cast compile-cast mref-state-reduction mvect-state-reduction)
    (cond
      [(hybrid-cast/coercion-runtime?)
       ;; interp-cast-refers to running code which uses type-based
       ;; reasoning instead of coercions to cast values. Higher-Order
       ;; casts are residuallized as coercions lazily
       (define interp-cast-uid (next-uid! "interp-cast"))

       (: interp-cast Cast-Type)
       (define (interp-cast v t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
         (apply-code interp-cast-uid v t1 t2 l suspend-monotonic-heap-casts?))

       (define mref-state-reduction (make-compile-mref-state-reduction
                                     #:interp-cast interp-cast
                                     #:greatest-lower-bound greatest-lower-bound))

       (define mvect-state-reduction (make-compile-mvect-state-reduction
                                     #:interp-cast interp-cast
                                     #:greatest-lower-bound greatest-lower-bound))

       ;; This first section builds the cast interpreter that falls
       ;; back to make-coercion when a higher-order cast is applied
       (define compile-tuple-cast/type-based
         (make-compile-cast-tuple #:interp-cast-uid interp-cast-uid))
       
       (define compile-pref-cast/coercions
         (make-compile-cast-pref/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-pvec-cast/coercions
         (make-compile-cast-pvec/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-mbox-cast/type-based
         (make-compile-mbox-cast
          #:interp-cast interp-cast
          #:greatest-lower-bound compile-types-greatest-lower-bound
          #:mref-state-reduction mref-state-reduction))
       
       (define compile-mvec-cast/type-based
         (make-compile-mvec-cast
          #:interp-cast interp-cast
          #:greatest-lower-bound compile-types-greatest-lower-bound
          #:mvect-state-reduction mvect-state-reduction))

       (define interp-med-cast
         (make-interp-med-cast-runtime!
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/type-based
          #:pref-cast  compile-pref-cast/coercions
          #:pvec-cast  compile-pvec-cast/coercions
          #:mbox-cast  compile-mbox-cast/type-based
          #:mvec-cast  compile-mvec-cast/type-based))
       
       (define compile-med-cast
         (make-compile-med-cast
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/type-based
          #:pref-cast  compile-pref-cast/coercions
          #:pvec-cast  compile-pvec-cast/coercions
          #:mbox-cast  compile-mbox-cast/type-based
          #:mvec-cast  compile-mvec-cast/type-based
          #:interp-med-cast interp-med-cast))

       (define compile-project/type-based
         (make-compile-project #:compile-med-cast compile-med-cast))

       (make-interp-cast-runtime!
        #:interp-cast-uid interp-cast-uid 
        #:project compile-project/type-based
        #:inject  compile-inject
        #:compile-med-cast compile-med-cast)

       (make-apply-coercion-runtime!
        #:apply-coercion-uid apply-coercion-uid
        #:project compile-project/type-based
        #:inject  compile-inject
        #:apply-fn-coercion compile-apply-fn-coercion
        #:apply-tup-coercion compile-apply-tup-coercion
        #:apply-pref-coercion compile-apply-pref-coercion
        #:apply-pvec-coercion compile-apply-pvec-coercion
        #:mbox-cast compile-mbox-cast/type-based
        #:mvec-cast compile-mvec-cast/type-based)
       
       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast
          #:project compile-project/type-based
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast))
       
       (values interp-cast compile-cast mref-state-reduction mvect-state-reduction)]
      [else
       (define interp-cast-uid (next-uid! "interp-cast"))
       
       (: interp-cast/coercions Cast-Type)
       (define (interp-cast/coercions v t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
         (apply-coercion v (compile-make-coercion t1 t2 l #:top-level? #t) suspend-monotonic-heap-casts?))

       (: interp-med-cast/coercions Cast-Type)
       (define (interp-med-cast/coercions v t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
         (apply-coercion v (compile-make-med-coercion t1 t2 l) suspend-monotonic-heap-casts?))

       (define mref-state-reduction (make-compile-mref-state-reduction
                                     #:interp-cast interp-cast/coercions
                                     #:greatest-lower-bound greatest-lower-bound))

       (define mvect-state-reduction (make-compile-mvect-state-reduction
                                     #:interp-cast interp-cast/coercions
                                     #:greatest-lower-bound greatest-lower-bound))

       ;; This first section builds the cast interpreter that falls
       ;; back to make-coercion when a higher-order cast is applied
       (define compile-tuple-cast/coercions
         (make-compile-cast-tuple/coercions
          #:apply-coercion-uid apply-coercion-uid
          #:make-med-coercion compile-make-med-coercion))
       
       (define compile-pref-cast/coercions
         (make-compile-cast-pref/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-pvec-cast/coercions
         (make-compile-cast-pvec/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-mbox-cast/coercions
         (make-compile-mbox-cast
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound compile-types-greatest-lower-bound
          #:mref-state-reduction mref-state-reduction))
       
       (define compile-mvec-cast/coercions
         (make-compile-mvec-cast
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound compile-types-greatest-lower-bound
          #:mvect-state-reduction mvect-state-reduction))
       
       (define compile-med-cast/coercions
         (make-compile-med-cast
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/coercions
          #:pref-cast  compile-pref-cast/coercions
          #:pvec-cast  compile-pvec-cast/coercions
          #:mbox-cast  compile-mbox-cast/coercions
          #:mvec-cast  compile-mvec-cast/coercions
          #:interp-med-cast interp-med-cast/coercions))

       (define compile-project/coercions
         (make-compile-project #:compile-med-cast compile-med-cast/coercions))

       (make-apply-coercion-runtime!
        #:apply-coercion-uid apply-coercion-uid
        #:project compile-project/coercions
        #:inject  compile-inject
        #:apply-fn-coercion compile-apply-fn-coercion
        #:apply-tup-coercion compile-apply-tup-coercion
        #:apply-pref-coercion compile-apply-pref-coercion
        #:apply-pvec-coercion compile-apply-pvec-coercion
        #:mbox-cast compile-mbox-cast/coercions
        #:mvec-cast compile-mvec-cast/coercions)

       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast/coercions
          #:project compile-project/coercions
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast/coercions))
       
       (values interp-cast/coercions compile-cast mref-state-reduction mvect-state-reduction)]))

  (define-values (pbox-ref pbox-set! pvec-ref pvec-set! pvec-len)
    (make-proxied-reference/coercions-code-gen-helpers
     #:apply-coercion apply-coercion))
  
  (define-values (mbox-ref mbox-set! mvec-ref mvec-set!)
    (make-monotonic-helpers
     #:interp-cast interp-cast
     #:mref-state-reduction mref-state-reduction
     #:mvect-state-reduction mvect-state-reduction
     #:compile-cast compile-cast))

  (define-values (dyn-pbox-ref
                  dyn-pbox-set!
                  dyn-pvec-ref
                  dyn-pvec-set!
                  dyn-pvec-len
                  dyn-mbox-ref
                  dyn-mbox-set!
                  dyn-mvec-ref
                  dyn-mvec-set!
                  dyn-fn-app
                  dyn-tup-prj)
    (make-dynamic-operations-helpers
     #:compile-cast compile-cast
     #:compile-app compile-app
     #:pbox-ref pbox-ref #:pbox-set pbox-set!
     #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
     #:mbox-ref mbox-ref #:mbox-set mbox-set!
     #:mvec-ref mvec-ref #:mvec-set mvec-set!))
  
  (values
   (make-map-expr
    #:compile-cast    compile-cast
    #:compile-lambda  compile-lambda
    #:compile-app     compile-app
    #:pbox-ref        pbox-ref
    #:pbox-set        pbox-set!
    #:pvec-ref        pvec-ref
    #:pvec-set        pvec-set!
    #:pvec-len        pvec-len
    #:mbox-ref        mbox-ref
    #:mbox-set        mbox-set!
    #:mvec-ref        mvec-ref
    #:mvec-set        mvec-set!
    #:dyn-pbox-ref    dyn-pbox-ref
    #:dyn-pbox-set    dyn-pbox-set!
    #:dyn-pvec-ref    dyn-pvec-ref
    #:dyn-pvec-set    dyn-pvec-set!
    #:dyn-pvec-len    dyn-pvec-len
    #:dyn-mbox-ref    dyn-mbox-ref
    #:dyn-mbox-set    dyn-mbox-set!
    #:dyn-mvec-ref    dyn-mvec-ref
    #:dyn-mvec-set    dyn-mvec-set!
    #:dyn-fn-app      dyn-fn-app
    #:dyn-tup-prj     dyn-tup-prj)
   apply-coercion-uid
   compose-coercions-uid))



;; Struct that represents a mu that might be built
(struct Pre-Mu ([uid : (Option Uid)]) #:mutable)

(: make-pre-mu : -> Pre-Mu)
(define (make-pre-mu) (Pre-Mu #f))

(: pre-mu-used? : Pre-Mu -> Boolean)
(define (pre-mu-used? pm)
  (and (Pre-Mu-uid pm) #t))

(: pre-mu-uid! : Pre-Mu -> Uid)
(define (pre-mu-uid! pm)
  (cond
    [(Pre-Mu-uid pm) => values]
    [else
     (define u (next-uid! "mu"))
     (set-Pre-Mu-uid! pm u)
     u]))

;; This code (`make-compose-fn-coercions` and
;; `make-compose-tup-coercions`) is temporarily really wonky and
;; redundant because I am duplicating code so that I don't have to
;; worry about hyper-coerions and coercions at the same time.  This
;; code should be moved to ic-common.rkt when hyper-coercions is
;; extend to work with recursive coercions.
(define (make-compose-fn-coercions
         #:id-coercion? [id-coercion? : Id-Coercion-Huh-Type]
         #:compose-coercions [compose-coercions : Compose-Coercions/ID/FVS])
  : Compose-Coercions/ID/FVS
  (define compose-fn-coercions-uid (next-uid! "compose-fn-coercions"))
  (add-cast-runtime-binding!
   compose-fn-coercions-uid
   (code$ (c1 c2 ret-id? ret-fvs)
     (let$ ([c3-box  (Unguarded-Box-On-Stack ID-EXPR)]
            [j       (Unguarded-Box-On-Stack ZERO-EXPR)]
            [arity   (fn-coercion-arity$ c1)])
       ;; Compose the return
       (let$ ([c3-ret (compose-coercions (Fn-Coercion-Return c1)
                                         (Fn-Coercion-Return c2)
                                         ret-id? ret-fvs)])
         (cond$
          [(id-coercion? c3-ret) (Quote '())]
          [else
           (let$ ([fnc (Id-Fn-Coercion arity)])
             (Fn-Coercion-Return-Set! fnc c3-ret)
             (Unguarded-Box-Set! c3-box fnc))]))
       ;; Compose the first arguments that result in id
       (repeat$ (i ZERO-EXPR arity) ()
         (Unguarded-Box-Set! j (op$ + (Quote 1) i))
         (let$ ([c3-arg (compose-coercions (Fn-Coercion-Arg c2 i)
                                           (Fn-Coercion-Arg c1 i)
                                           ret-id? ret-fvs)])
           (cond$
            [(id-coercion? c3-arg) (Quote '())]
            [else
             (when$ (id-coercion? (Unguarded-Box-Ref c3-box))
              (Unguarded-Box-Set! c3-box (Id-Fn-Coercion arity))) 
             (Fn-Coercion-Arg-Set! (Unguarded-Box-Ref c3-box) i c3-arg)
             (Break-Repeat)])))
       ;; Compose the rest of the arguments
       (repeat$ (i (Unguarded-Box-Ref j) arity) ()
         (let$ ([c3-arg (compose-coercions (Fn-Coercion-Arg c2 i)
                                           (Fn-Coercion-Arg c1 i)
                                           ret-id? ret-fvs)])
           ;; The function coercion has to exist because we are
           ;; in this loop
           (Fn-Coercion-Arg-Set! (Unguarded-Box-Ref c3-box) i c3-arg)))
       ;; No need to set the id?-ret-box because the sub calls would
       ;; have set it if they resulted in anything besides id. 
       (Unguarded-Box-Ref c3-box))))
  (λ ([c1 : CoC3-Expr]
      [c2 : CoC3-Expr]
      [ret-id? : CoC3-Expr]
      [ret-fvs : CoC3-Expr])
    (apply-code compose-fn-coercions-uid c1 c2 ret-id? ret-fvs)))

(define (make-compose-tup-coercions
         #:id-coercion? [id-coercion? : Id-Coercion-Huh-Type]
         #:compose-coercions [compose-coercions : Compose-Coercions/ID/FVS])
  : Compose-Coercions/ID/FVS
  (define compose-tup-coercions-uid (next-uid! "compose-tuple-coercions"))
  (add-cast-runtime-binding!
   compose-tup-coercions-uid
   (code$ (c1 c2 ret-id? ret-fvs)
     (let$ ([c3-box  (Unguarded-Box-On-Stack ID-EXPR)]
            [j       (Unguarded-Box-On-Stack ZERO-EXPR)]
            [arity   (Tuple-Coercion-Num c1)])
       ;; Compose up to the first non-id coercion
       (repeat$ (i ZERO-EXPR arity) ()
         (Unguarded-Box-Set! j (op$ + (Quote 1) i))
         (let$ ([c3-arg (compose-coercions (Tuple-Coercion-Item c1 i)
                                           (Tuple-Coercion-Item c2 i)
                                           ret-id? ret-fvs)])
           (cond$
            [(id-coercion? c3-arg) (Quote '())]
            [else
             (let$ ([tupc (Id-Tuple-Coercion arity)])
               (Tuple-Coercion-Item-Set! tupc i c3-arg)
               (Unguarded-Box-Set! c3-box tupc)
               (Break-Repeat))])))
       ;; Compose the rest
       (repeat$ (i (Unguarded-Box-Ref j) arity) ()
         (Tuple-Coercion-Item-Set!
          (Unguarded-Box-Ref c3-box)
          i
          (compose-coercions (Tuple-Coercion-Item c1 i)
                             (Tuple-Coercion-Item c2 i)
                             ret-id? ret-fvs)))
       ;; We don't need to set id?-ret-box because any recursive
       ;; calls resulting in non-id coercions would have set it.
       (Unguarded-Box-Ref c3-box))))
  (λ ([c1 : CoC3-Expr]
      [c2 : CoC3-Expr]
      [ret-id? : CoC3-Expr]
      [ret-fvs : CoC3-Expr])
    (apply-code compose-tup-coercions-uid c1 c2 ret-id? ret-fvs)))

(define (make-make-fn-coercion
         #:id-coercion? [id-coercion? : Id-Coercion-Huh-Type]
         #:make-coercion [make-coercion : Make-Coercion/ID/FVS])
  : Make-Coercion/ID/FVS
  (define make-fn-coercion-uid (next-uid! "make-fn-coercions"))
  (add-cast-runtime-binding!
   make-fn-coercion-uid
   (code$ (t1 t2 lbl ret-id? ret-fvs)
     ;; This code is slightly more complicated than it should
     ;; be in order to avoid allocating a fn-coercion before
     ;; we know that we need to. If we had a faster GC for
     ;; then I would be less worried about this.
     (let$ ([c-box (Unguarded-Box-On-Stack ID-EXPR)]
            [j     (Unguarded-Box-On-Stack ZERO-EXPR)]
            [arity (Type-Fn-arity t1)])
       ;; make the return
       (let$ ([c-ret (make-coercion (Type-Fn-return t1)
                                    (Type-Fn-return t2)
                                    lbl ret-id? ret-fvs)])
         (cond$
          [(id-coercion? c-ret) (Quote '())]
          [else
           (let$ ([fnc (Id-Fn-Coercion arity)])
             (Fn-Coercion-Return-Set! fnc c-ret)
             (Unguarded-Box-Set! c-box fnc))]))
       ;; make the first arguments that result in id
       (repeat$ (i ZERO-EXPR arity) ()
         (Unguarded-Box-Set! j (op$ + (Quote 1) i))
         (let$ ([c-arg (make-coercion (Type-Fn-arg t2 i)
                                      (Type-Fn-arg t1 i)
                                      lbl ret-id? ret-fvs)])
           (cond$
            [(id-coercion? c-arg) (Quote '())]
            [else
             (when$ (id-coercion? (Unguarded-Box-Ref c-box))
              (Unguarded-Box-Set! c-box (Id-Fn-Coercion arity))) 
             (Fn-Coercion-Arg-Set! (Unguarded-Box-Ref c-box) i c-arg)
             (Break-Repeat)])))
       ;; make the rest of the arguments
       (repeat$ (i (Unguarded-Box-Ref j) arity) ()
         (let$ ([c-arg (make-coercion (Type-Fn-arg t2 i)
                                      (Type-Fn-arg t1 i)
                                      lbl ret-id? ret-fvs)])
           ;; The function coercion has to exist because we are
           ;; in this loop
           (Fn-Coercion-Arg-Set! (Unguarded-Box-Ref c-box) i c-arg)))
       ;; No need to set the id?-ret-box because the sub calls would
       ;; have set it if they resulted in anything besides id. 
       (Unguarded-Box-Ref c-box))))
    (λ ([t1 : CoC3-Expr]
        [t2 : CoC3-Expr]
        [lbl : CoC3-Expr]
        [ret-id? : CoC3-Expr]
        [ret-fvs : CoC3-Expr])
      : CoC3-Expr
      (apply-code make-fn-coercion-uid t1 t2 lbl ret-id? ret-fvs)))

(define (make-make-tuple-coercion
         #:id-coercion? [id-coercion? : Id-Coercion-Huh-Type]
         #:make-coercion [make-coercion : Make-Coercion/ID/FVS])
  : Make-Coercion/ID/FVS
  (define make-tuple-coercion-uid (next-uid! "make-tuple-coercion"))
  (add-cast-runtime-binding!
   make-tuple-coercion-uid
   ;; mk-tuple-crcn creates a coercion out of two tuple types, it also checks
   ;; if the two types are identical, so that it can generate a simple
   ;; identity coercion without allocating unnecessary tuple coercion of
   ;; identities. It expects the length of the first tuple to be greater than
   ;; or equal to the length of the second.
   (code$ (t1 t2 lbl ret-id? ret-fvs)
     (let$ ([c-box (Unguarded-Box-On-Stack ID-EXPR)]
            [j     (Unguarded-Box-On-Stack ZERO-EXPR)]
            [arity (Type-Tuple-num t2)])
       ;; Compose up to the first non-id coercion
       (repeat$ (i ZERO-EXPR arity) ()
         (Unguarded-Box-Set! j (op$ + (Quote 1) i))
         (let$ ([c-arg (make-coercion (Type-Tuple-item t1 i)
                                      (Type-Tuple-item t2 i)
                                      lbl ret-id? ret-fvs)])
           (cond$
            [(id-coercion? c-arg) (Quote '())]
            [else
             (let$ ([tupc (Id-Tuple-Coercion arity)])
               (Tuple-Coercion-Item-Set! tupc i c-arg)
               (Unguarded-Box-Set! c-box tupc)
               (Break-Repeat))])))
       ;; Compose the rest
       (repeat$ (i (Unguarded-Box-Ref j) arity) ()
         (Tuple-Coercion-Item-Set!
          (Unguarded-Box-Ref c-box)
          i
          (make-coercion (Type-Tuple-item t1 i)
                         (Type-Tuple-item t2 i)
                         lbl ret-id? ret-fvs)))
       ;; We don't need to set id?-ret-box because any recursive
       ;; calls resulting in non-id coercions would have set it.
       (Unguarded-Box-Ref c-box))))
  (λ ([t1 : CoC3-Expr]
      [t2 : CoC3-Expr]
      [lbl : CoC3-Expr]
      [ret-id? : CoC3-Expr]
      [ret-fvs : CoC3-Expr])
    (apply-code make-tuple-coercion-uid t1 t2 lbl ret-id? ret-fvs)))


(define cast-runtime-constant-bindings
  : (Parameterof (Option (Listof (Pairof Uid CoC3-Expr))))
  (make-parameter #f))

(: add-cast-runtime-constant! : Uid CoC3-Expr -> Void)
(define (add-cast-runtime-constant! uid init)
  (define const* (cast-runtime-constant-bindings))
  (unless const*
    (error 'todo))
  (cast-runtime-constant-bindings (cons (cons uid init) const*)))

(require (for-syntax racket/base))

(define-syntax (when$ stx)
  (syntax-case stx ()
    [(when$ c s* ...)
     #'(cond$
        [c s* ...]
        [else ZERO-EXPR])]))

(define-syntax (unless$ stx)
  (syntax-case stx ()
    [(_ c s* ...)
     (syntax/loc stx
       (cond$
        [c ZERO-EXPR]
        [else s* ...]))]))

(: and* : (Listof Boolean) -> Boolean)
(define (and* ls)
  (or (null? ls)
      (and (car ls)
           (and* (cdr ls)))))
