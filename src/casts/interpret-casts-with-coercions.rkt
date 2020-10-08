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
 "../lib/list.rkt"
 "./cast-profiler.rkt"
 "./interpret-casts-common.rkt")

(provide interpret-casts/coercions
         cast-runtime-constant-bindings
         apply-coercions-at-first-tail-cast?
         constant-fold-coercions?
         cps-fn-application)

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
         [(hash-ref m t1.t2 #f)
          =>
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
      [(_ _) (values (Sequence IDENTITY (Failed lbl)) #f)]))
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

  (define RET-ID-HUH-EXPR (Unguarded-Box-On-Stack (Quote #t)))
  (define RET-FVS-EXPR (Unguarded-Box-On-Stack (Quote 0)))
  
  (define ((call-mk-crcn/id/fvs mk-crcn) t1 t2 lbl #:top-level? [top-level? #f])
    (define-values (ret-id? ret-fvs)
      (cond
        [top-level? (values RET-ID-HUH-EXPR RET-FVS-EXPR)]
        [else (values ZERO-EXPR ZERO-EXPR)]))
    (bind-value$
     ([ret-id? ret-id?]
      [ret-fvs ret-fvs])
     (mk-crcn t1 t2 lbl ret-id? ret-fvs)))
  
  (define interp-make-coercion/id/fvs (call-mk-crcn/id/fvs interp-make-coercion))
  (define interp-make-med-coercion/id/fvs (call-mk-crcn/id/fvs interp-make-med-coercion))
  
  (define make-fn-coercion : Make-Coercion/ID/FVS
    (make-make-fn-coercion #:id-coercion? Id-Coercion-Huh
                           #:make-coercion interp-make-coercion))
  (define make-fn-coercion/id/fvs
    (call-mk-crcn/id/fvs make-fn-coercion))
  
  (define make-tuple-coercion : Make-Coercion/ID/FVS
    (make-make-tuple-coercion #:id-coercion? Id-Coercion-Huh
                              #:make-coercion interp-make-coercion))
  
  (define make-tuple-coercion/id/fvs
    (call-mk-crcn/id/fvs make-tuple-coercion))
  
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
          (let*$
           ([new-id? (Unguarded-Box-On-Stack TRUE-EXPR)]
            [ret-fvs (cond$
                      [(op=? ret-fvs ZERO-EXPR)
                       (Unguarded-Box-On-Stack ZERO-EXPR)]
                      [else ret-fvs])]
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
           (unless$
            (or$ id? (op=? ret-fvs ZERO-EXPR))
            (Unguarded-Box-Set! ret-id? (Quote #f))) 
           (cond$
            ;; This mu binding was never used in the body
            [(op$ = ZERO-EXPR mu) c]
            [else
             ;; This has been guarded for being null by adding a
             ;; shadowing above that allocates when null.
             (Unguarded-Box-Set! ret-fvs (op$ - fv-count (Quote 1)))
             (cond$
              ;; if ret-fvs is 0 and id? is still true then we can
              ;; conclude this mu is equivalent to identity
              [(and$ id? (op=? fv-count (Quote 1))) ID-EXPR]
              [else
               (Mu-Coercion-Body-Set! mu c)
               mu])]))]))]
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
     [else (Sequence-Coercion ID-EXPR (Failed-Coercion lbl))]))

  (: compile-make-med-coercion Make-Med-Coercion-Type)
  (define (compile-make-med-coercion
           t1 t2 lbl
           ;; top-level? indicates that this call is not dominated by a call to make-med-coercion.
           #:top-level? [top-level? #t]
           #:know-not-eq? [know-not-eq? #f])
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
            (make-fn-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)]
           [((STuple n _) (STuple m _)) #:when (<= n m)
            (make-tuple-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)]
           [((GRef t1-t) (GRef t2-t))
            (define t1 (Type t1-t))
            (define t2 (Type t2-t))
            (ref-coercion$
             (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
             (interp-make-coercion/id/fvs t2 t1 lbl #:top-level? top-level?))]
           [((GVect t1-t) (GVect t2-t))
            (define t1 (Type t1-t))
            (define t2 (Type t2-t))
            (vec-coercion$
             (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
             (interp-make-coercion/id/fvs t2 t1 lbl #:top-level? top-level?))]
           ;; Can we avoid doing some work at runtime by computing the
           ;; join now?
           [((MRef _) (MRef t2))
            (Quote-Coercion (MonoRef t2))]
           [((MVect _) (MVect t2))
            (Quote-Coercion (MonoVect t2))]
           ;; TODO consider doing better
           [(_ _) (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)])]
        [((Type t1-t) t2)
         (match t1-t
           ;; in each of these cases t2 could be a mu
           ;; todo consider doing better
           [(Fn n _ _)
            (If (and$ (type-fn?$ t2) (op=? (type-fn-arity$ t2) (Quote n)))
                (make-fn-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(STuple n _)
            (If (and$ (type-tup?$ t2) (op=? (type-tup-arity$ t2) (Quote n)))
                (make-tuple-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(GRef t1-t)
            (define t1 (Type t1-t))
            (If (type-pbox?$ t2)
                (let$ ([t2 (type-pbox-of$ t2)])
                  (ref-coercion$
                   (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                   (interp-make-coercion/id/fvs t2 t1 lbl #:top-level? top-level?)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(GVect t1-t)
            (define t1 (Type t1-t))
            (If (type-pvec?$ t2)
                (let$ ([t2 (type-pvec-of$ t2)])
                  (vec-coercion$ (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                                 (interp-make-coercion/id/fvs t2 t1 lbl #:top-level? top-level?)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(MRef _)
            (If (type-mbox?$ t2)
                (MRef-Coercion (type-mbox-of$ t2))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(MVect _)
            (If (type-mvec?$ t2)
                (MVect-Coercion (type-mvec-of$ t2))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [_ (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)])]
        [(t1 (Type t2-t))
         (match t2-t
           [(Fn n _ _)
            (If (and$ (type-fn?$ t1) (op=? (type-fn-arity$ t1) (Quote n)))
                (make-fn-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(STuple n _)
            (If (and$ (type-tup?$ t1) (op=? (type-tup-arity$ t1) (Quote n)))
                (make-tuple-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(GRef t2-t)
            (define t2 (Type t2-t))
            (If (type-pbox?$ t1)
                (let$ ([t1 (type-pbox-of$ t1)])
                  (ref-coercion$
                   (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                   (interp-make-coercion/id/fvs t2 t1 lbl #:top-level? top-level?)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(GVect t2-t)
            (define t2 (Type t2-t))
            (If (type-pvec?$ t1)
                (let$ ([t1 (type-pvec-of$ t1)])
                  (vec-coercion$
                   (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)
                   (interp-make-coercion/id/fvs t2 t1 lbl #:top-level? top-level?)))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(MRef t2-t)
            (If (type-mbox?$ t1)
                (MRef-Coercion (Type t2-t))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [(MVect t2-t)
            (If (type-mvec?$ t1)
                (MVect-Coercion (Type t2-t))
                (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?))]
           [_ (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)])]
        [(t1 t2) (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)]))
    ;; Bind non-values and decide to be eq? check 
    (bind-value$
     ([t1 t1] [t2 t2] [lbl lbl])
     (match* (t1 t2)
       [(t t) ID-EXPR]
       [((Type t1) (Type t2)) #:when (Quote? lbl)
        (match-define (Quote lit) lbl)
        (unless (string? lit)
          (error 'compile-make-med-coercion "expected a string got: ~v" lit))
        ;; top-level set to false to prevent make coercion from optimizing
        ;; Subcoercions of this mediating coercion. This might not be needed if
        ;; make-coercion is implemented correctly
        ;; TODO fix make-coercion to not optimize under structural coercions
        ;; by default
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
       (unless$
        (op=? ret-id? ZERO-EXPR)
        (Unguarded-Box-Set! ret-id? (Quote #f)))
       (Sequence-Coercion (Project-Coercion t2 lbl) ID-EXPR)]
      [(type-dyn?$ t2)
       (unless$
        (op=? ret-id? ZERO-EXPR)
        (Unguarded-Box-Set! ret-id? (Quote #f)))
       (Sequence-Coercion ID-EXPR (Inject-Coercion t1))]
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
    ;; r expects t1 t2 lbl to be either literals or vars, but at least one is a var
    (define (r t1 t2 lbl)
      (match* (t1 t2)
        [(t t) (Quote-Coercion (Identity))]
        [((Type (Dyn)) _) ;; Dyn on right ruled out by last pattern
         (let ([prj (Project-Coercion t2 lbl)])
           (if (and top-level? (optimize-first-order-coercions?))
               prj
               (Sequence-Coercion prj ID-EXPR)))]
        [(_ (Type (Dyn))) ;; Dyn on left ruled out by last pattern
         (let ([inj (Inject-Coercion t1)])
           (if (and top-level? (optimize-first-order-coercions?))
               inj
               (Sequence-Coercion ID-EXPR inj)))]
        ;; From here on any use of the type pattern is used to rule out
        ;; that variable from being dynamic
        [((Type _) (Type _))
         (compile-make-med-coercion
          t1 t2 lbl #:top-level? top-level? #:know-not-eq? #t)]
        [((Var _) (Type _))
         (cond$
          [(type-dyn?$ t1) (r DYN-EXPR t2 lbl)]
          [else (compile-make-med-coercion
                 t1 t2 lbl #:top-level? top-level? #:know-not-eq? #t)])]
        [((Type _) (Var _))
         (cond$
          [(type-dyn?$ t2) (r t1 DYN-EXPR lbl)]
          [else (compile-make-med-coercion
                 t1 t2 lbl #:top-level? top-level? #:know-not-eq? #t)])]
        [((Var _) (Var _))
         (cond
           [(make-coercion-inline/both-vars?) 
            (cond$
             [(type-dyn?$ t1) (r DYN-EXPR t2 lbl)]
             [(type-dyn?$ t2) (r t1 DYN-EXPR lbl)]
             ;; There is no more information to specialize on
             [else (interp-make-med-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)])]
           [else (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)])]))
    ;; bind all non-literals and decide to check for eq?
    (define who 'make-coercion)
    (debug who t1 t2 lbl)
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
         [((Var _) (Var _))
          (interp-make-coercion/id/fvs t1 t2 lbl #:top-level? top-level?)]
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
        (->* CoC3-Expr CoC3-Expr (CoC3-Expr) CoC3-Expr)))
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

  (define (interp-apply-coercion e c [top-level? (Quote #t)])
    (apply-code apply-coercion-uid e c top-level?))

  (add-cast-runtime-binding!
   apply-coercion-uid
   (code$
    (v c suspend-monotonic-heap-casts?)
    (cond$
     [(Id-Coercion-Huh c) v]
     [(Sequence-Coercion-Huh c)
      (let$
       ([seq-fst (Sequence-Coercion-Fst c)]
        [seq-snd (Sequence-Coercion-Snd c)])
       ;; This is a racket conditional
       (cond
         [(coercions-are-space-efficient?)
          (precondition$
           (or$ (Project-Coercion-Huh seq-fst)
                (Inject-Coercion-Huh seq-snd)
                (Failed-Coercion-Huh seq-snd))
           (cond$
            [(Project-Coercion-Huh seq-fst)
             (let*$
              ([t (Project-Coercion-Type seq-fst)]
               [l (Project-Coercion-Label seq-fst)]
               [v (project v t l suspend-monotonic-heap-casts?)])
              (cond$
               [(Id-Coercion-Huh seq-snd) v]
               [else (interp-apply-coercion v seq-snd suspend-monotonic-heap-casts?)]))]
            [(Inject-Coercion-Huh seq-snd)
             (let$
              ([v (cond$
                   [(Id-Coercion-Huh seq-fst) v]
                   [else (interp-apply-coercion v seq-fst suspend-monotonic-heap-casts?)])])
              (inject v (Inject-Coercion-Type seq-snd)))]
            [else
             (precondition$
              (Failed-Coercion-Huh seq-snd)
              (begin$
                (interp-apply-coercion v seq-fst suspend-monotonic-heap-casts?)
                (Blame (Failed-Coercion-Label seq-snd))))]))]
         [else
          (let$ ([v (interp-apply-coercion v seq-fst suspend-monotonic-heap-casts?)])
                (interp-apply-coercion v seq-snd suspend-monotonic-heap-casts?))]))]
     [else
      (let ()
        (define structural-coercion-cases
          (precondition$
           (Mediating-Coercion-Huh c)
           (cond$
            [(Fn-Coercion-Huh c) (apply-fn-coercion v c)]
            [(Tuple-Coercion-Huh c)
             (apply-tup-coercion v c suspend-monotonic-heap-casts?)]
            [(Mu-Coercion-Huh c)
             (interp-apply-coercion v (Mu-Coercion-Body c) suspend-monotonic-heap-casts?)]
            [(Ref-Coercion-Huh c)
             (if (cast-profiler?)
                 (cond$
                  [(Ref-Coercion-Ref-Huh c) (apply-pref-coercion v c)]
                  [else (apply-pvec-coercion v c)])
                 (apply-pref-coercion v c))] 
            [(MRef-Coercion-Huh c) (mbox-cast v (MRef-Coercion-Type c) suspend-monotonic-heap-casts?)]
            [(MVect-Coercion-Huh c) (mvec-cast v (MRef-Coercion-Type c) suspend-monotonic-heap-casts?)]
            [else (Blame (Quote "bad implemention of mediating coercions"))])))
        (cond
          ;; Todo this flag is basically useless now 
          [(or (optimize-first-order-coercions?) (not (coercions-are-space-efficient?)))
           (cond$
            [(Project-Coercion-Huh c)
             (project v (Project-Coercion-Type c) (Project-Coercion-Label c) suspend-monotonic-heap-casts?)]
            [(Inject-Coercion-Huh c) (inject v (Inject-Coercion-Type c))]
            [(Failed-Coercion-Huh c) (Blame (Failed-Coercion-Label c))]
            [else structural-coercion-cases])]
          [else structural-coercion-cases]))])))
  ;; compile-apply-coercion
  (cond
    [(specialize-cast-code-generation?)
     (lambda (e c [suspend-monotonic-heap-casts? (Quote #t)])
       (match c
         [(Quote-Coercion c)
          (let rec ([e e] [c c])
            (match c
              [(Identity) e]
              [(Sequence (Project t l) f)
               (rec (project e (Type t) (Quote l) suspend-monotonic-heap-casts?) f)]
              [(Sequence r (Failed l))
               (begin$ (rec e r) (Blame (Quote l)))]
              [(Sequence r (Inject t))
               (inject (rec e r) (Type t))]
              [(CRec u body)
               (rec e (subst body u c))]
              [(Fn _ _ _) (apply-fn-coercion e (Quote-Coercion c))]
              [(CTuple _ _) (apply-tup-coercion e (Quote-Coercion c) suspend-monotonic-heap-casts?)]
              [(Ref _ _ f)
               (cond
                 [(and (eq? f 'GVect) (cast-profiler?))
                  (apply-pvec-coercion e (Quote-Coercion c))]
                 [else (apply-pref-coercion e (Quote-Coercion c))])]
              [(MonoRef  t) (mbox-cast e (Type t) suspend-monotonic-heap-casts?)]
              [(MonoVect t) (mvec-cast e (Type t) suspend-monotonic-heap-casts?)]))]
         [_ (interp-apply-coercion e c suspend-monotonic-heap-casts?)]))]
    [else interp-apply-coercion])
  )

(define (subst c x v)
  (let rec ([c c])
    (match c
      [(CVar u) #:when (equal? u x) v]
      [(CVar u) c]
      [(Sequence (and (Project _ _) p) f) (Sequence p (rec f))]
      [(Sequence r (and (or (Inject _) (Failed _)) i)) (Sequence (rec r) i)]
      [(Identity) IDENTITY]
      [(CRec u _) #:when (equal? u x) c]
      [(CRec u c) (CRec u (rec c))]
      [(Fn n c* c) (Fn n (map rec c*) (rec c))]
      [(CTuple n c*) (CTuple n (map rec c*))]
      [(Ref r w t) (Ref (rec r) (rec w) t)]
      [(and (or (MonoRef _) (MonoVect _)) c) c])))

  ;; Compile time compose-coercions for
(define (cc c1 c2)
  ;; this is doing composition and substitution at the same time
  ;; c1-unfold-env, c2-unfold-env are the substitution environment
  ;; mu-env is a map of seen mu equivalences and an optional uid allocated to that
  ;; mu if it has already been used.
  ;; id? - And whether or not the thing returned is equivalent to identity
  ;; fvs - This function is also keeping track of the number of unique free variables
  ;; This algorithm is trying to directly imitate what is happening at runtime.
  ;; it uses multiple value returns instead of out parameters
  (define-values (res _1 _2)
    (let aux ([c1 c1] [c2 c2] [mu-env (hash)])
      (define (rec c1 c2) (aux c1 c2 mu-env))
      (define (rec* c1* c2*)
        (define-values (c3* id?* fv*)
          (for/lists (c3* id?* fv*) ([c1 c1*] [c2 c2*]) (rec c1 c2)))
        (values c3* (andmap identity id?*) (foldl + 0 fv*)))
      (match* (c1 c2)
        [((Sequence s (Inject I1)) (Sequence (Project I2 l) f))
         (define c1 (make-coercion I1 I2 l))
         (define-values (c2 _ fvs1) (rec c1 f))
         (define-values (c3 id? fvs2) (rec s c2))
         (values c3 id? (+ fvs1 fvs2))]
        [((Identity) (Identity)) (values c1 #t 0)]
        [((Identity) d)
         ;; d would be the identity if it was identity equivalent
         (values d #f 0)]
        [(d (Identity))
         ;; d would be the identity if it was identity equivalent
         (values d #f 0)]
        [((Sequence (and (Project _ _) p) f) d)
         (define-values (f^ _ fvs) (rec f d))
         (values (Sequence p f^) #f fvs)]
        [((and f (Sequence _ (Failed _))) d)
         (values f #f 0)]
        [(r1 (Sequence r2 (and (Inject I) i)))
         (define-values (r3 _ fvs) (rec r1 r2))
         (values (Sequence r3 i) #f fvs)]
        [(r1 (Sequence r2 (and (Failed _) f)))
         (define-values (r3 _ fvs) (rec r1 r2))
         (values (Sequence r3 f) #f fvs)]
        [(r1 r2)
         #:when (or (CRec? r1) (CRec? r2))
         (define p (cons c1 c2))
         (cond
           [(hash-ref mu-env p #f)
            =>
            ;; This pair of coercions have been compose higher up
            (λ ([pm : Pre-Mu])
              (cond
                ;; Mu has already been used
                [(Pre-Mu-uid pm)
                 =>
                 (λ ([u : Uid])
                   (values (CVar u) #t 0))]
                [else
                 ;; Mu hasn't been used yet so count this unique free var
                 (define u (pre-mu-uid! pm))
                 (values (CVar u) #t 1)]))]
           [else
            ;; Haven't come across this pair of coercion yet
            (define pm : Pre-Mu (make-pre-mu))
            (define mu-env^ (hash-set mu-env p pm))
            ;; fmvs? = optional-free-mu-vars
            (define-values (c id? fvs)
              (match* (r1 r2)
                [((CRec u s) r2)
                 (aux s (subst s u r1) r2 mu-env^)]
                [(r1 (CRec u s))
                 (aux r1 (subst s u r2) mu-env^)]))
            (cond
              ;; If the mu binds a variable then we need to build then we need to consider it
              [(Pre-Mu-uid pm)
               =>
               (λ ([u : Uid])
                 (values
                  (cond
                    ;; This is a closed expression once this mu is created
                    [(and (= fvs 1) id?) IDENTITY]
                    [else (CRec u c)])
                  id?
                  (- fvs 1)))]
              ;; This Mu binding wasn't used therefore we don't need to build it. 
              [else (values c id? fvs)])])]
        [((Fn n c1* c1) (Fn n c2* c2))
         (define-values (c3* id?1 fvs1) (rec* c2* c1*))
         (define-values (c3  id?2 fvs2) (rec c1 c2))
         (values
          (cond
            [(and (Identity? c3) (andmap Identity? c3*)) IDENTITY]
            [else (Fn n c3* c3)])
          (and id?1 id?2)
          (+ fvs1 fvs2))]
        [((CTuple n c1*) (CTuple n c2*))
         (define-values (c3* id? fvs) (rec* c1* c2*))
         (values
          (cond
            [(andmap Identity? c3*) IDENTITY]
            [else (CTuple n c3*)])
          id?
          fvs)]
        [((Ref r1 w1 t) (Ref r2 w2 t))
         (define-values (r3 id?r fvsr) (rec r1 r2))
         (define-values (w3 id?w fvsw) (rec w2 w1))
         (values
          (cond
            [(and (Identity? r3) (Identity? w3)) IDENTITY]
            [else (Ref r3 w3 t)])
          (and id?r id?w)
          (+ fvsr fvsw))]
        [((MonoRef t1) (MonoRef t2))
         (match (join t1 t2)
           [(Bottom t g)
            (error 'compose-coercions "precision-join: inconsistent types")]
           [t3 (MonoRef t3)])]
        [((MonoVect t1) (MonoVect t2))
         (match (join t1 t2)
           [(Bottom t g)
            (error 'compose-coercions "precision-join: inconsistent types")]
           [t3 (MonoVect t3)])]
        [(c1 c2)
         (error 'compose-coercion "coercion typing invariant broke: (cc ~a ~a)" c1 c2)])))
  res)

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
  (: interp-compose-coercions Compose-Coercions/ID/FVS)
  (define (interp-compose-coercions c1 c2 ret-id? ret-fvs)
    (apply-code compose-coercions-uid c1 c2 ret-id? ret-fvs))
  (: compose-coercions/id/fvs Compose-Coercions-Type)
  (define (compose-coercions/id/fvs c1 c2)
    (interp-compose-coercions c1 c2 ZERO-EXPR ZERO-EXPR))

  (: compose-fn-coercions Compose-Coercions/ID/FVS)
  (define compose-fn-coercions
    (make-compose-fn-coercions #:id-coercion? Id-Coercion-Huh
                               #:compose-coercions interp-compose-coercions))
  (: compose-tup-coercions Compose-Coercions/ID/FVS)
  (define compose-tup-coercions
    (make-compose-tup-coercions #:id-coercion? Id-Coercion-Huh
                                #:compose-coercions interp-compose-coercions))
  
  (add-cast-runtime-binding!
   compose-coercions-uid
   ;; The precondition of this function is that all coercions
   ;; equivalent to the identity coercion have already been colapsed
   ;; to the identity coercion.
   
   ;; `ret-id?` is an out parameter.  It is a null reference if we
   ;; haven't gone under a recursive coercion yet. 
   ;; If we have gone under a recursive coercion it is a reference
   ;; pointing true and needs to be set to false if the returned
   ;; coercion isn't equivalent to the identity coercion.

   ;; `ret-fvs` is an out parameter. It is a null reference if we
   ;; haven't gone under a recursive coercion yet. No need to update.
   ;; If we have gone under a recursive coercion it is a reference
   ;; that must be incremented the first time each recursive coercion
   ;; is referenced. Note, each recursive coercion should only consider
   ;; the free variables of it's expression which will require allocating
   ;; a new out parameter at each recursive coercion. 

   ;; TODO break this function up so there is a compose-med-coercions
   ;; Rational: the only meaningfull coercions under a Mu are also
   ;; med-coercions being able to dispatch there quicker makes sense
   (code$
    (c1 c2 ret-id? ret-fvs)
    (precondition$
     (not$ (or$ (failed-coercion?$ c1)
                (failed-coercion?$ c2)))
     (cond$
      ;; Eliminating the Identities cuts down on the number of branches
      ;; and should be fast
      [(id-coercion?$ c1)
       (unless$
        (or$ (id-coercion?$ c2)
             (op=? ZERO-EXPR ret-id?))
        (Unguarded-Box-Set! ret-id? FALSE-EXPR))
       c2]
      ;; We know that c1 isn't id because of the test above
      [(id-coercion?$ c2) c1]
      [(seq-coercion?$ c1)
       (let*$
        ([seq_fst (seq-coercion-fst$ c1)]
         [seq_snd (seq-coercion-snd$ c1)])
        (cond$
         ;; Prioritize success as long as it is mutually exclusive with failure
         [(prj-coercion?$ seq_fst)
          (let*$
           ([comp_seq_snd (interp-compose-coercions seq_snd c2 ret-id? ret-fvs)])
           (unless$
            (op$ = ZERO-EXPR ret-id?)
            (Unguarded-Box-Set! ret-id? FALSE-EXPR))
           (seq-coercion$ seq_fst comp_seq_snd))]
         [(inj-coercion?$ seq_snd)
          ;; Because of the typeing rule for coercions we know that
          ;; c2 must be a (I?;i) aka projection sequence because
          ;; it must have the type dyn => I.
          ;; Therefore we are doing (m;I1!) ;; (I2?;i)
          (precondition$
           (and$ (seq-coercion?$ c2)
                 (not$ (failed-coercion?$ (seq-coercion-snd$ c2))))
           (let*$
            ([proj  (seq-coercion-fst$ c2)]
             [final (seq-coercion-snd$ c2)]
             [t1    (inj-coercion-type$ seq_snd)]
             [t2    (prj-coercion-type$ proj)]
             [lbl   (prj-coercion-label$ proj)]
             [comp_prj_inj (make-coercion t1 t2 lbl)]
             [comp_fst_pi
              (interp-compose-coercions seq_fst comp_prj_inj ZERO-EXPR ret-fvs)])
            ;; Only the final compose call should factor into the
            ;; ret-id? .  Coercions created in the call to
            ;; make-coercion and first compose-coercions calls can be
            ;; eliminated in subsequent composes.
            (interp-compose-coercions comp_fst_pi final ret-id? ret-fvs)))]
         ;; Otherwise the only posibility is that this is a failure sequence
         [else
          (precondition$
           (failed-coercion?$ seq_snd)
           (begin$
             (unless$
              (op=? ZERO-EXPR ret-id?)
              (Unguarded-Box-Set! ret-id? FALSE-EXPR))
             c1))]))]
      [(seq-coercion?$ c2)
       (let$
        ([seq_fst (seq-coercion-fst$ c2)]
         [seq_snd (seq-coercion-snd$ c2)])
        (cond$
         [(failed-coercion?$ c1)
          ;; TODO remove this case once this is working
          (Quote-Coercion (Failed "this shouldn't happen todo remove this KJfklajsfkljafkldj"))]
         [(inj-coercion?$ seq_snd)
          ;; must be c1 & (g;I?)
          (let$
           ([comp_c1_final (interp-compose-coercions c1 seq_fst ret-id? ret-fvs)])
           (unless$
            (op=? ZERO-EXPR ret-id?)
            (Unguarded-Box-Set! ret-id? FALSE-EXPR))
           (seq-coercion$ comp_c1_final seq_snd))]
         [else
          ;; must be r1 & (r2 ; _|_ l) = (r1;;r2);_|_ l
          (precondition$
           (and$ (failed-coercion?$ seq_snd) (med-coercion?$ c1))
           (begin$
             (unless$
              (op=? ZERO-EXPR ret-id?)
              (Unguarded-Box-Set! ret-id? FALSE-EXPR))
             (seq-coercion$ (interp-compose-coercions c1 seq_fst ret-id? ret-fvs) seq_snd)))]))]
      [(med-coercion?$ c1)
       (cond$
        [(or$ (Mu-Coercion-Huh c1) (Mu-Coercion-Huh c2))
         (let$
          ([i (op$ assoc-stack-find cc-assoc-stack c1 c2)])
          (cond$
           ;; We haven't previously composed this pair of coercions
           [(op$ < i ZERO-EXPR)
            (op$ assoc-stack-push! cc-assoc-stack c1 c2 ZERO-EXPR)
            (let*$
             ([new-id? (Unguarded-Box-On-Stack TRUE-EXPR)]
              [new-fvs (Unguarded-Box-On-Stack ZERO-EXPR)]
              [c (cond$
                  [(Mu-Coercion-Huh c1)
                   (interp-compose-coercions
                    (Mu-Coercion-Body c1) c2 new-id? new-fvs)]
                  [else
                   (interp-compose-coercions
                    c1 (Mu-Coercion-Body c2) new-id? new-fvs)])]
              [mu (op$ assoc-stack-pop! cc-assoc-stack)]
              [id? (Unguarded-Box-Ref new-id?)]
              [fv-in-body (Unguarded-Box-Ref new-fvs)])
             (unless$
              id?
              (unless$
               (op=? ret-id? ZERO-EXPR)
               (Unguarded-Box-Set! ret-id? FALSE-EXPR))) 
             (cond$
              ;; This mu binding was never used in the body
              ;; NOTE if id? then this should be ID-EXPR too
              [(op$ < ZERO-EXPR mu) c]
              [else
               ;; We are using the Mu-Coercion-Body field to count
               ;; the number of times this recursive-coercion was referenced
               ;; in the body.
               ;; The number of free variables is the number of variable
               ;; references minus the number being closed over here.
               (let$
                ([fv-count (op$ - fv-in-body (Mu-Coercion-Body mu))])
                (precondition$
                 (op$ <= ZERO-EXPR fv-count)
                 (cond$
                  ;; if ret-fvs is 0 and id? is still true then we can
                  ;; conclude this mu is equivalent to identity
                  ;; If their were still free variables then some coercion
                  ;; higher up in the tree could cause this mu to not be
                  ;; equivalent to identity.
                  ;; also no need to update the ret-fvs cause there
                  ;; are no free variables is this expression. 
                  [(and$ id? (op=? fv-count ZERO-EXPR)) ID-EXPR]
                  [else
                   (unless$
                    (op=? ZERO-EXPR ret-fvs)
                    (Unguarded-Box-Set!
                     ret-fvs
                     (op$ + (Unguarded-Box-Ref ret-fvs) fv-count)))
                   (Mu-Coercion-Body-Set! mu c)
                   mu])))]))]
           ;; We have previously composed this pair of coercions
           ;; and `i` may point to a recursive coercion that will
           ;; represent the result of composing.
           [else
            ;; No need to guard this because we know we are under
            ;; a recursive coercion.
            (Unguarded-Box-Set!
             ret-fvs
             (op$ + (Unguarded-Box-Ref ret-fvs) (Quote 1)))
            (let$
             ([mu (op$ assoc-stack-ref cc-assoc-stack i)])
             (cond$
              ;; No one else has used this mu yet so we have to
              ;; allocate it to comunicate back up the stack that
              ;; we need it to point to the coercion we want to
              ;; exist here too.
              ;; Also we use the mu-body field to keep track of the
              ;; number of references are used.
              [(op$ = ZERO-EXPR mu) 
               (let$
                ([mu (Make-Mu-Coercion)])
                (Mu-Coercion-Body-Set! mu ONE-EXPR)
                (op$ assoc-stack-set! cc-assoc-stack i mu)
                mu)]
              ;; Otherwise we just use the mu that someone else
              ;; made, it will point to the coercion we need here.
              ;; We need to bump the number of references to this
              ;; mu coercion so that we can tell when we are dealing
              ;; with a closed mu-coercion for the id equivalence check.
              [else
               (Mu-Coercion-Body-Set! mu (op$ + ONE-EXPR (Mu-Coercion-Body mu)))
               mu]))]))]               
        [(fn-coercion?$ c1) ;; c2 must be a Function Coercion
         (compose-fn-coercions c1 c2 ret-id? ret-fvs)]
        [(ref-coercion?$ c1) ;; c2 must also be a reference coercion
         (let*$ ([ref1_read  (ref-coercion-read$  c1)]
                 [ref1_write (ref-coercion-write$ c1)]
                 [ref2_read  (ref-coercion-read$  c2)]
                 [ref2_write (ref-coercion-write$ c2)]
                 [read  (interp-compose-coercions ref1_read  ref2_read
                                                  ret-id? ret-fvs)]
                 [write (interp-compose-coercions ref2_write ref1_write
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
      [else (Blame (failed-coercion-label$ c1))]))))

  (define (compile-compose-coercions c1 c2)
    (match* (c1 c2)
      [((Quote-Coercion c1) (Quote-Coercion c2))
       (Quote-Coercion (cc c1 c2))]
      [((Quote-Coercion (Identity)) c2)
       c2]
      [(c1 (Quote-Coercion (Identity)))
       c1]
      ;; We might be able to get rid of some dispatch time both by
      ;; eliminating dispatch on a statically known coercion and
      ;; by considering what the statically known coercions implies
      ;; about the dynamically known coercion via the typing rule
      ;; for compose coercions.
      [(c1 c2)
       (compose-coercions/id/fvs c1 c2)]))
  
  (values compose-coercions-uid compile-compose-coercions))

(: interpret-casts/coercions : -> (C0-Expr -> (Values CoC3-Expr Uid Uid)))
(define (interpret-casts/coercions)
  (define speciailize-casts? (specialize-cast-code-generation?))
  (define greatest-lower-bound (make-compile-types-greatest-lower-bound))
  (define-values (compile-make-coercion compile-make-med-coercion)
    (make-compile-make-coercion/make-med-coercion))
  (define-values (compose-coercions-uid compile-compose-coercions)
    (make-compose-coercions
     #:make-coercion compile-make-coercion
     #:greatest-lower-bound greatest-lower-bound))
  (define apply-coercion-uid (next-uid! "apply-coercion"))
  (define (interp-apply-coercion
           [v : CoC3-Expr] [c : CoC3-Expr]
           [suspend-monotonic-heap-casts? : CoC3-Expr do-not-suspend-monotonic-heap-casts])
    : CoC3-Expr
    (apply-code apply-coercion-uid v c suspend-monotonic-heap-casts?))

  (define get-fn-cast!
    (make-fn-cast-helpers
     (make-build-caster/coercions
      #:apply-coercion-uid apply-coercion-uid
      #:compose-coercions-uid compose-coercions-uid
      #:compose-coercions compile-compose-coercions
      #:id-coercion-huh   Id-Coercion-Huh)))
  
  (: compile-lambda Lambda-Type)
  (define (compile-lambda fml* e)
    (define arity (length fml*))
    (define ctr
      (get-fn-cast!
       (if (eq? (enable-tail-coercion-composition?) 'andre)
           (- arity 1)
           arity)))
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
     #:compose-coercions compile-compose-coercions
     #:id-coercion? Id-Coercion-Huh))

  (define compile-apply-pvec-coercion
    (make-compile-apply-pvec-coercion
     #:compose-coercions compile-compose-coercions
     #:id-coercion? Id-Coercion-Huh))
  
  (define compile-inject (make-compile-inject))

  ;; Interp-Cast Builds a call to a runtime function
  ;; that casts based on types.
  ;; Compile cast specializes based on types
  (define-values (interp-cast compile-cast compile-apply-coercion
                  mref-state-reduction mvect-state-reduction)
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
          #:compose-coercions compile-compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-pvec-cast/coercions
         (make-compile-cast-pvec/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compile-compose-coercions
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

       (define compile-apply-coercion
         (make-apply-coercion-runtime!
          #:apply-coercion-uid apply-coercion-uid
          #:project compile-project/type-based
          #:inject  compile-inject
          #:apply-fn-coercion compile-apply-fn-coercion
          #:apply-tup-coercion compile-apply-tup-coercion
          #:apply-pref-coercion compile-apply-pref-coercion
          #:apply-pvec-coercion compile-apply-pvec-coercion
          #:mbox-cast compile-mbox-cast/type-based
          #:mvec-cast compile-mvec-cast/type-based))
       
       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast
          #:project compile-project/type-based
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast))

       
       (values interp-cast compile-cast compile-apply-coercion
               mref-state-reduction mvect-state-reduction)]
      [else
       (define interp-cast-uid (next-uid! "interp-cast"))
       
       (: interp-cast/coercions Cast-Type)
       (define (interp-cast/coercions
                v t1 t2 l
                [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
         (interp-apply-coercion v (compile-make-coercion t1 t2 l #:top-level? #t) suspend-monotonic-heap-casts?))

       (: interp-med-cast/coercions Cast-Type)
       (define (interp-med-cast/coercions
                v t1 t2 l
                [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
         (interp-apply-coercion v (compile-make-med-coercion t1 t2 l) suspend-monotonic-heap-casts?))

       (define mref-state-reduction
         (make-compile-mref-state-reduction
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound greatest-lower-bound))

       (define mvect-state-reduction
         (make-compile-mvect-state-reduction
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
          #:compose-coercions compile-compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-pvec-cast/coercions
         (make-compile-cast-pvec/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compile-compose-coercions
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

       (define compile-apply-coercion
         (make-apply-coercion-runtime!
          #:apply-coercion-uid apply-coercion-uid
          #:project compile-project/coercions
          #:inject  compile-inject
          #:apply-fn-coercion compile-apply-fn-coercion
          #:apply-tup-coercion compile-apply-tup-coercion
          #:apply-pref-coercion compile-apply-pref-coercion
          #:apply-pvec-coercion compile-apply-pvec-coercion
          #:mbox-cast compile-mbox-cast/coercions
          #:mvec-cast compile-mvec-cast/coercions))

       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast/coercions
          #:project compile-project/coercions
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast/coercions))
       
       (values interp-cast/coercions compile-cast compile-apply-coercion
               mref-state-reduction mvect-state-reduction)]))

  (define-values (pbox-ref pbox-set! pvec-ref pvec-set! pvec-len)
    (make-proxied-reference/coercions-code-gen-helpers
     #:apply-coercion interp-apply-coercion))
  
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
   (cond
     [(eq? (enable-tail-coercion-composition?) 'andre)
      (make-coercion-passing-style-translation
       #:apply-coercion    compile-apply-coercion
       #:compose-coercions compile-compose-coercions
       #:make-coercion     compile-make-coercion
       #:compile-cast    compile-cast
       #:compile-lambda  compile-lambda
       #:compile-app     compile-app
       #:pbox-ref pbox-ref
       #:pbox-set pbox-set!
       #:pvec-ref pvec-ref
       #:pvec-set pvec-set!
       #:pvec-len pvec-len
       #:mbox-ref mbox-ref
       #:mbox-set mbox-set!
       #:mvec-ref mvec-ref
       #:mvec-set mvec-set!
       #:dyn-pbox-ref dyn-pbox-ref
       #:dyn-pbox-set dyn-pbox-set!
       #:dyn-pvec-ref dyn-pvec-ref
       #:dyn-pvec-set dyn-pvec-set!
       #:dyn-pvec-len dyn-pvec-len
       #:dyn-mbox-ref dyn-mbox-ref
       #:dyn-mbox-set dyn-mbox-set!
       #:dyn-mvec-ref dyn-mvec-ref
       #:dyn-mvec-set dyn-mvec-set!
       #:dyn-tup-prj  dyn-tup-prj)]
     [else
      (make-map-expr
       #:compile-cast    compile-cast
       #:compile-lambda  compile-lambda
       #:compile-app     compile-app
       #:pbox-ref pbox-ref
       #:pbox-set pbox-set!
       #:pvec-ref pvec-ref
       #:pvec-set pvec-set!
       #:pvec-len pvec-len
       #:mbox-ref mbox-ref
       #:mbox-set mbox-set!
       #:mvec-ref mvec-ref
       #:mvec-set mvec-set!
       #:dyn-fn-app dyn-fn-app
       #:dyn-pbox-ref dyn-pbox-ref
       #:dyn-pbox-set dyn-pbox-set!
       #:dyn-pvec-ref dyn-pvec-ref
       #:dyn-pvec-set dyn-pvec-set!
       #:dyn-pvec-len dyn-pvec-len
       #:dyn-mbox-ref dyn-mbox-ref
       #:dyn-mbox-set dyn-mbox-set!
       #:dyn-mvec-ref dyn-mvec-ref
       #:dyn-mvec-set dyn-mvec-set!
       #:dyn-tup-prj  dyn-tup-prj)])
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


;; This is also used in the convert-closures pass
;; Translation for function calls highly abstracted because the actual
;; concrete cases are quite numerous and duplicated across dynamic
;; and static function calls.
(define (cps-fn-application
         app compose-coercions apply-coercion
         e e* [crcn #f] [tail-crcn-ref #f]
         #:tail-coercion-composition?
         [tail-coercion-composition?
          (and (enable-tail-coercion-composition?)
               (not (eq? (enable-tail-coercion-composition?) 'false)))]
         #:casting-contexts?
         [casting-contexts?
          (and tail-coercion-composition?
               (apply-coercions-at-first-tail-cast?))])
  (define who 'cps-fn-application)
  (define (const? x)
    (or (Var? x) (Quote-Coercion? x) (Quote? x) (Type? x)))
  (debug who crcn tail-crcn-ref tail-coercion-composition? casting-contexts?)
  (let app-help ([e e] [e* e*] [crcn crcn] [tail-crcn-ref tail-crcn-ref])
    (cond
      [(not tail-coercion-composition?)
       (app e e*)]
      [(and tail-coercion-composition? (not crcn))
       (error 'app-help "invariant broken")]
      [(not casting-contexts?)
       (app e (snoc e* crcn))]
      [(and (equal? crcn ID-EXPR) tail-crcn-ref)
       (app e (snoc e* tail-crcn-ref))]
      [(equal? crcn ID-EXPR)
       (app e (snoc e* ZERO-EXPR))]
      ;; Continuation coercion for a non-tail call
      [(not tail-crcn-ref)
       (let*$ ([tail-crcn-ref (Unguarded-Box crcn)]
               [ret-val (app e (snoc e* tail-crcn-ref))])
              (apply-coercion ret-val (Unguarded-Box-Ref tail-crcn-ref)))]
      ;; We will now inevitably duplicate e and e*
      [(not (and (const? e) (andmap const? e*)))
       (define-values (bnd* v v*)
         (let loop ([bnd* '()] [rev-v* '()] [e* (cons e e*)])
           (match e*
             [(cons (? const? v) e*)
              (loop bnd* (cons v rev-v*) e*)]
             [(cons e e*)
              (define u (next-uid! "tmp"))
              (loop (cons (cons u e) bnd*) (cons (Var u) rev-v*) e*)]
             [(list)
              (define v* (reverse rev-v*))
              (values bnd* (car v*) (cdr v*))])))
       (Let bnd* (app-help v v* crcn tail-crcn-ref))]
      [(not (or (Quote-Coercion? crcn) (Var? crcn)))
       (let$
        ([cont-crcn crcn])
        (app-help e e* (Var cont-crcn) tail-crcn-ref))]
      ;; Know crcn != ID-EXPR and tail-crcn-ref != #f
      ;; that e and e* are all vars
      ;; that crcn is Var? or Quote-Coercion?
      [else
       (define compose-ref
         (Unguarded-Box-Set!
          tail-crcn-ref
          (compose-coercions
           crcn
           (Unguarded-Box-Ref tail-crcn-ref))))
       (cond$
        [(op=? tail-crcn-ref ZERO-EXPR)
         (app-help e e* crcn #f)]
        [else
         (cond
           [(Quote-Coercion? crcn) compose-ref]
           [else
            (unless$ (op=? crcn ID-EXPR) compose-ref)])
         (app-help e e* ID-EXPR tail-crcn-ref)])])))


;; This procedure maps over expressions translating cast expression to
;; space-efficient coercion passing style which prevents tail-coercions
;; from getting in the way of tail-call optimization.
;;
;; If enable-tail-coercion-composition? is #f this pass should be equivalent
;; to the standard translation to space-efficient coercions from type-based
;; casts.
;; 
;; This is a refactor / comparison of the work done by Tsuda in the file
;; named `coercion-passing.rkt` in this directory.
;;
;; This pass tries to be abstract over the coercion representation so that
;; it could be reused for hyper-coercions.
;; 
;; The improvements made are gated by a few parameters:
;; - `constant-fold-coercions?` enables composing coercions at compile time
;;   instead of runtime. 
;; - `specialize-cast-code-generation?` allows specializing the cast code
;;   to a coercion literal to reduce interpretive overhead of casting.
;; - `apply-coercions-at-first-tail-cast?` moves the application of
;;   coercions from function returns to the first tail-cast that introduced
;;   a tail-cast.
(define (make-coercion-passing-style-translation
         #:apply-coercion    [apply-coercion : Apply-Coercion-Type]
         #:compose-coercions [compose-coercions : Compose-Coercions-Type]
         #:make-coercion     [make-coercion : Make-Coercion-Type]
         #:compile-cast    [compile-cast : Compile-Cast-Type]
         #:compile-lambda  [compile-lambda : Lambda-Type]
         #:compile-app     [compile-app : App-Type]
         #:pbox-ref [pbox-ref  : PBox-Ref-Type]
         #:pbox-set [pbox-set! : PBox-Set-Type]
         #:pvec-ref [pvec-ref  : PVec-Ref-Type]
         #:pvec-set [pvec-set! : PVec-Set-Type]
         #:pvec-len [pvec-len  : PVec-Len-Type]
         #:mbox-ref [mbox-ref  : MBox-Ref-Type]
         #:mbox-set [mbox-set! : MBox-Set-Type]
         #:mvec-ref [mvec-ref  : MVec-Ref-Type]
         #:mvec-set [mvec-set! : MVec-Set-Type]
         #:dyn-pbox-ref [dyn-pbox-ref  : Dyn-PBox-Ref-Type]
         #:dyn-pbox-set [dyn-pbox-set! : Dyn-PBox-Set-Type]
         #:dyn-pvec-ref [dyn-pvec-ref  : Dyn-PVec-Ref-Type]
         #:dyn-pvec-set [dyn-pvec-set! : Dyn-PVec-Set-Type]
         #:dyn-pvec-len [dyn-pvec-len  : Dyn-PVec-Len-Type]
         #:dyn-mbox-ref [dyn-mbox-ref  : Dyn-MBox-Ref-Type]
         #:dyn-mbox-set [dyn-mbox-set! : Dyn-MBox-Set-Type]
         #:dyn-mvec-ref [dyn-mvec-ref  : Dyn-MVec-Ref-Type]
         #:dyn-mvec-set [dyn-mvec-set! : Dyn-MVec-Set-Type]
         #:dyn-tup-prj  [dyn-tup-prj   : Dyn-Tup-Prj-Type]
         ;; These are node that may get compiled differently for static
         #:mbox         [mbox : (CoC3-Expr Grift-Type -> CoC3-Expr) Mbox]
         #:stc-mbox-ref [stc-mbox-ref : (CoC3-Expr -> CoC3-Expr) Mbox-val-ref]
         #:stc-mbox-set [stc-mbox-set! : (CoC3-Expr CoC3-Expr -> CoC3-Expr) Mbox-val-set!]
         #:mvec         [mvec : (CoC3-Expr CoC3-Expr Grift-Type -> CoC3-Expr) Mvector]
         #:stc-mvec-ref [stc-mvec-ref : (CoC3-Expr CoC3-Expr -> CoC3-Expr) mvector-val-ref-with-check-bounds]
         #:stc-mvec-set [stc-mvec-set! : (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) mvector-val-set!-with-check-bounds]
         #:mvec-len     [mvec-len : (CoC3-Expr -> CoC3-Expr) Mvector-length])

  ;; 'false is a flag for testing if this map without tcc is the same
  ;; as the old map-expr
  (define tail-coercion-composition?
    (and (enable-tail-coercion-composition?)
         (not (eq? (enable-tail-coercion-composition?) 'false))))
  (define constant-fold? (and tail-coercion-composition?
                              (constant-fold-coercions?)))
  (define casting-contexts? (and tail-coercion-composition?
                                 (apply-coercions-at-first-tail-cast?)))

  
  ;; This helper function mitigates the differences in the configuration parameters
  ;; and how the algorithm as implemented by Tsuda worked. It may seem superfluous
  ;; but is carefully crafted. -- Andre
  (define (apply-help [exp : CoC3-Expr] [cont : CoC3-Expr]) : CoC3-Expr
    (cond
      ;; The original algorithm always erased ID applications
      [(equal? cont ID-EXPR) exp]
      ;; apply-coercion may specialize the code generated base on
      ;; specialize-cast-code-generation?
      [else  (apply-coercion exp cont)]))

  (define (app-help e e* [crcn #f] [tail-crcn-ref #f])
    (cps-fn-application
     compile-app compose-coercions apply-coercion
     e e* crcn tail-crcn-ref
     #:tail-coercion-composition? tail-coercion-composition?
     #:casting-contexts? casting-contexts?))

  
  ;; Translate and expression with only a statically-known coercion.
  ;; This is important conceptually but it actually turns out that
  ;; all of the "dynamic" code (non-existent) mirrors the static case
  ;; instead we write one function `trans` which handles both static
  ;; and dynamic. To know which case we are in we inspect the dynamically
  ;; known coercion to see if it is a variable (dynamic) or identity-coercion
  ;; (static).
  (define (static-trans e [cont-crcn ID-EXPR])
    (trans e ID-EXPR cont-crcn))
  (define (static-trans* e*) (map static-trans e*))
  (define (static-trans-bnd* b*)
    (define (static-trans-bnd b) (cons (car b) (static-trans (cdr b))))
    (map static-trans-bnd b*))
  
  ;; Dyn-fn-apps would be broken if we didn't use app-help here
  ;; Tail-crcn : coercion-expression
  ;; Tail-crcn-ref : (Optional ref-coercion-expression)
  (define (dyn-fn-app e e* t* le tail-crcn tail-crcn-ref)
    (define bnd (inst cons Uid CoC3-Expr))
    (unless (= (length e*) (length t*))
      (error 'interpret-casts-with-hyper-coercions/make-fn-app
             "expected types to be same length as arguments"))
    (define app-arity (length e*))
    (define fn-of-dyns-type
      (Type (Fn app-arity (make-list app-arity DYN-TYPE) DYN-TYPE)))
    (let$
     ([dyn-fn-blame-info le] [dyn-v e])
     (let-values
         ([(l) dyn-fn-blame-info]
          [(u* v*) (for/lists ([u* : Uid*]
                               [v* : (Listof (Var Uid))])
                              ([_ e*])
                     (define u (next-uid! "dyn_fn_arg"))
                     (values u (Var u)))])
       (Let
        (map bnd u* e*)
        (cond$
         ;; This condition asks if this is a boxed dynamic value
         [(Dyn-Immediate-Tag=Huh dyn-v FN-DYN-DYN-EXPR)
          (let*$
           ([unboxed-value (Dyn-Box-Value dyn-v)]
            [src-type (Dyn-Box-Type  dyn-v)]
            [src-type (unfold-possible-mu$ src-type)])
           (cond$
            [(Type-Fn-Huh src-type)
             (let-values
                 ;; `cu*` Uids for casted expression bindings
                 ;; `cv*` Vars for cu*
                 ;; `ce*` The expressions that cast e*
                 ([(cu* cv* ce*)
                   ;; `v*` Vars of the `e*` bindings
                   ;; `t` The type undyned and unfolded
                   (for/lists ([cu* : Uid*]
                               [cv* : (Listof (Var Uid))]
                               [ce* : (Listof CoC3-Expr)])
                              ([v v*] [t t*] [i (in-naturals)])
                     (define u (next-uid! "dyn-fn-casted-argument"))
                     (values
                      u (Var u)
                      (compile-cast
                       v (Type t) (Type-Fn-arg src-type (Quote i)) l)))])
               (Let
                (map bnd cu* ce*)
                (cond
                  [tail-coercion-composition?
                   (let*$
                    ([inj-crcn (make-coercion (Type-Fn-return src-type) DYN-EXPR l)]
                     [tail-crcn (compose-coercions inj-crcn tail-crcn)])
                    (app-help unboxed-value cv* tail-crcn tail-crcn-ref))]
                  [else
                   (let$
                    ([ret-val (compile-app unboxed-value cv*)]
                     [ret-ty  (Type-Fn-return src-type)])
                    (compile-cast ret-val ret-ty DYN-EXPR l))])))]
            [else (Blame l)]))]
         [else (Blame l)])))))
  


  
  ;; Translate cast expression to coercion-passing style expression
  ;; e : cast-expression to be translated
  ;; tail-coercion-ref : (or/c Var? #f) -- only used when context-cast? == #t
  ;; dynamic-cont-crcn : (or/c Var? ID-EXPR?) -- ID-EXPR? -> coercion-context is statically known
  ;; static-cont-crcn : Quote-Coercion?
  (define (trans e dynamic-cont-crcn static-cont-crcn [tail-crcn-ref #f])
    (define (recur e) (trans e dynamic-cont-crcn static-cont-crcn tail-crcn-ref))
    (match e
      [(Cast e (Twosome (app Type t1) (app Type t2) (app Quote l)))
       ;; This case decides whether or not we are using the algorithm as described
       ;; by Tsuda, or if we are using the version of the algorithm that is aware
       ;; of the difference between statically known coercions, and dynamically
       ;; known coercions.
       (cond
         [(not tail-coercion-composition?)
          (compile-cast (static-trans e) t1 t2 l)]
         [else
          (define c (make-coercion t1 t2 l))
          (cond
            [constant-fold?
             (unless (Quote-Coercion? static-cont-crcn)
               (error
                'trans
                (string-append
                 "Invariant:"
                 "When constant folding, variable `static-cont-crcn` is expected to be"
                 " a statically known coercion. Instead I got: ~a")
                static-cont-crcn))
             (trans e dynamic-cont-crcn (compose-coercions c static-cont-crcn) tail-crcn-ref)]
            [else
             ;; We are about to place `c` after static-cont-crcn if it were not ID
             ;; then this wouldn't be valid.
             (unless (equal? static-cont-crcn ID-EXPR)
               (error
                'trans
                (string-append
                 "Invariant:"
                 "When not constant folding, variable `static-cont-crcn` is expected to be"
                 " the identity-coercion. Instead I got: ~a")
                static-cont-crcn))
             (let$
              ([cont-crcn (compose-coercions c dynamic-cont-crcn)])
              (trans e cont-crcn ID-EXPR tail-crcn-ref))])])]
      [(App e e*)
       ;; Applications get turned into an application that "checks for
       ;; the the presence of proxies" This eventually gets optimized
       ;; away into a functional proxy representation.  The statically
       ;; and dynamically known coercions must be composed at runtime
       ;; unless their is an identity coercion present for the dynamic
       ;; continuation coercion which can happen as a result of implementing
       ;; the static-translation in terms of the dynamic-translation.
       (cast-profile/inc-function-uses$
        (app-help
         (static-trans e) (static-trans* e*)
         (compose-coercions static-cont-crcn dynamic-cont-crcn) tail-crcn-ref))]
      ;; Use make coercion to implement dynamic application without
      ;; allocating a one time use coercion.
      [(Dyn-Fn-App e e* t* l)
       (cast-profile/inc-function-uses$
        (dyn-fn-app
         (static-trans e) (static-trans* e*) t* (Quote l)
         (compose-coercions static-cont-crcn dynamic-cont-crcn) tail-crcn-ref))]
      ;; Need to alter dyn-fn-app, compile-lambda, compile-app
      ;; Forms that have noticable control flow
      [(Letrec bnd* exp)
       (Letrec (static-trans-bnd* bnd*) (recur exp))]
      [(Let bnd* exp)
       (Let (static-trans-bnd* bnd*) (recur exp))]
      [(If tst csq alt)
       (If (static-trans tst) (recur csq) (recur alt))]
      [(Switch e c* d)
       (Switch (static-trans e) (map-switch-case* recur c*) (recur d))]
      [(Begin e* e)
       (Begin (static-trans* e*) (recur e))]
      [triv
       (apply-help (trivial-trans triv) (compose-coercions static-cont-crcn dynamic-cont-crcn))]))

  ;; Translate a trivial value which does not interact with the
  ;; continuation-coercion. All component non-trivial values will
  ;; be composed with statically known coercions.
  (define (trivial-trans val)
    (match val
      [(Var _) val]
      [(Quote _) val]  ;; Literal
      ;; Lambdas add a extra meta information field that ultimately
      ;; turns into a method for casting a particular arity at runtime.
      [(Lambda f* exp)
       (define k (next-uid! "cont-crcn"))
       (cast-profile/inc-uncasted-function-values$
        (compile-lambda
         (cond
           [tail-coercion-composition? (snoc f* k)]
           [else f*])
         (cond
           [casting-contexts? (trans exp ID-EXPR ID-EXPR (Var k))]
           [else (trans exp (Var k) ID-EXPR)])))]
      [(Observe e t) (Observe (static-trans e) t)]
      [(Op p exp*) (Op p (static-trans* exp*))]
      [(and (No-Op) e) e]
      ;; Transformation to lower guarded reference types
      ;; Guarded Operations on Guarded values are turned into
      ;; calls/inlinings of the runtime proceedures that perform
      ;; proxied reads and writes.
      ;;-------------------------------------------------------------------
      ;; Every Guarded Reference Starts As an Unguarded box
      [(Gbox e)
       (cast-profile/inc-uncasted-ref-values$
        (Unguarded-Box (static-trans e)))]
      [(Gvector n init)
       (cast-profile/inc-uncasted-vector-values$
        (Unguarded-Vect (static-trans n) (static-trans init)))]
      ;; Unboxing calls off to the helpers we have defined
      [(Gvector-ref v i)
       (cast-profile/inc-vector-uses$
        (pvec-ref (static-trans v) (static-trans i)))]
      [(Gunbox b)
       (cast-profile/inc-ref-uses$
        (pbox-ref (static-trans b)))]
      [(Dyn-GRef-Ref e l)
       (cast-profile/inc-ref-uses$
        (dyn-pbox-ref (static-trans e) (Quote l)))]
      [(Dyn-GVector-Ref e i l)
       (cast-profile/inc-vector-uses$
        (dyn-pvec-ref (static-trans e) (static-trans i) (Quote l)))]
      ;; Setting a Guarded reference results in iteratively applying
      ;; all the guarding casts to the value to be written.
      ;; Most of the work is already done but the code requires values
      ;; so there is a little repetative to let bind any values that
      ;; haven't been evaluated.
      [(Gbox-set! b w)
       (cast-profile/inc-ref-uses$
        (pbox-set! (static-trans b) (static-trans w)))]
      [(Gvector-set! v i w)
       (cast-profile/inc-vector-uses$
        (pvec-set! (static-trans v) (static-trans i) (static-trans w)))]
      [(Dyn-GRef-Set! e1 e2 t l)
       (cast-profile/inc-ref-uses$
        (dyn-pbox-set! (static-trans e1) (static-trans e2) (Type t) (Quote l)))]
      [(Dyn-GVector-Set! e1 i e2 t l)
       (cast-profile/inc-vector-uses$
        (dyn-pvec-set! (static-trans e1) (static-trans i) (static-trans e2) (Type t) (Quote l)))]
      [(Dyn-GVector-Len e l)
       (cast-profile/inc-vector-uses$
        (dyn-pvec-len (static-trans e) (static-trans l)))]
      [(Gvector-length e)
       (cast-profile/inc-vector-uses$
        (pvec-len (static-trans e)))]
      [(MBoxCastedRef e t)
       (cast-profile/inc-ref-uses$
        (mbox-ref (static-trans e) (Type t)))]
      [(MBoxCastedSet! e1 e2 t)
       (cast-profile/inc-ref-uses$
        (mbox-set! (static-trans e1) (static-trans e2) (Type t)))]
      [(MVectCastedRef e i t)
       (cast-profile/inc-vector-uses$
        (mvec-ref (static-trans e) (static-trans i) (Type t)))]
      [(MVectCastedSet! e1 i e2 t)
       (cast-profile/inc-vector-uses$
        (mvec-set! (static-trans e1) (static-trans i) (static-trans e2) (Type t)))]
      [(Dyn-MRef-Ref e l)
       (cast-profile/inc-ref-uses$
        (dyn-mbox-ref (static-trans e) (Quote l)))]
      [(Dyn-MRef-Set! e1 e2 t l)
       (cast-profile/inc-ref-uses$
        (dyn-mbox-set! (static-trans e1) (static-trans e2) (Type t) (Quote l)))]
      [(Dyn-MVector-Ref e i l)
       (cast-profile/inc-vector-uses$
        (dyn-mvec-ref (static-trans e) (static-trans i) (Quote l)))]
      [(Dyn-MVector-Set! e1 i e2 t l)
       (cast-profile/inc-vector-uses$
        (dyn-mvec-set! (static-trans e1) (static-trans i) (static-trans e2) (Type t) (Quote l)))]
      [(Mvector-ref e1 e2)
       (cast-profile/inc-vector-uses$
        (stc-mvec-ref (static-trans e1) (static-trans e2)))]
      [(Mvector-set! e1 e2 e3)
       (cast-profile/inc-vector-uses$
        (stc-mvec-set! (static-trans e1) (static-trans e2) (static-trans e3)))]
      [(Munbox e)
       (cast-profile/inc-ref-uses$
        (stc-mbox-ref (static-trans e)))]
      [(Mbox-set! e1 e2)
       (cast-profile/inc-ref-uses$
        (stc-mbox-set! (static-trans e1) (static-trans e2)))]
      [(Mvector-length e) 
       (cast-profile/inc-vector-uses$
        (mvec-len (static-trans e)))]
      [(Mbox e t)
       (cast-profile/inc-uncasted-ref-values$
        (mbox (static-trans e) t))]
      [(Mvector e1 e2 t)
       (cast-profile/inc-uncasted-vector-values$
        (mvec (static-trans e1) (static-trans e2) t))]
      ;; While tuples don't get any special attention in this pass
      ;; dynamic tuple projection needs to get dusugared
      [(Create-tuple e*)
       (cast-profile/inc-uncasted-tuple-values$
        (Create-tuple (static-trans* e*)))]
      [(Tuple-proj e i)
       (cast-profile/inc-tuple-uses$
        (Tuple-proj (static-trans e) (Quote i)))]
      [(Dyn-Tuple-Proj e i l)
       (cast-profile/inc-tuple-uses$
        (dyn-tup-prj (static-trans e) (static-trans i) (static-trans l)))]
      [(Repeat i e1 e2 a e3 e4)
       (Repeat i (static-trans e1) (static-trans e2) a (static-trans e3) (static-trans e4))]
      [other
       (error 'trivial-trans "Non-trivial expression: ~a" other)]))
  static-trans)


