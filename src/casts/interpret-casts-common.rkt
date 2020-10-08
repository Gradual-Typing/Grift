#lang typed/racket/base/no-check
#| 
This file shares code among various implementations of runtime casts
for the interpret-casts compiler pass. Logically this is the
file the exports code-generation and configuration options that
are shared between two or more implementations of casts.

This code is fragile PROCEED WITH CAUTION
TODO write unit tests
|#

(require
 racket/match
 racket/list
 "../configuration.rkt"
 "../language/forms.rkt"
 "../language/syntax.rkt"
 "../logging.rkt"
 "./cast-profiler.rkt"
 "./constants-and-codes.rkt")

(provide (all-defined-out))


(define project-inline-without-types? : (Parameterof Boolean)
  (make-parameter #f))

(define inject-inline-without-types? : (Parameterof Boolean)
  (make-parameter #f))

(define med-cast-inline-without-types? : (Parameterof Boolean)
  (make-parameter #f))

(define cast-runtime-code-bindings : (Parameterof (Option CoC3-Bnd-Code*))
  (make-parameter #f))

(define types-greatest-lower-bound-code-label? : (Parameterof (Option (Code-Label Uid)))
  (make-parameter #f))

(define mref-state-reduction-uid? : (Parameterof (Option Uid))
  (make-parameter #f))

(define mvect-state-reduction-uid? : (Parameterof (Option Uid))
  (make-parameter #f))

(: apply-code (->* (Uid) #:rest CoC3-Expr CoC3-Expr))
(define (apply-code u . a*)
  (App-Code (Code-Label u) a*))

(: apply-code-curry (-> Uid (->* () #:rest CoC3-Expr CoC3-Expr)))
(define ((apply-code-curry u) . a*)
  (App-Code (Code-Label u) a*))

;; Adds a piece of runtime code to the program
(: add-cast-runtime-binding! : Uid CoC3-Code -> Void)
(define (add-cast-runtime-binding! uid code)
  (define prev (cast-runtime-code-bindings))
  (unless prev
    (error 'add-cast-runtime-binding! "used outside the dynamic scope of interprete-cast"))
  (cast-runtime-code-bindings (cons (cons uid code) prev))
  (void))

;; Adds a piece of runtime code the first time it's uid is requested
(: make-lazy-add-cast-runtime-binding! : String CoC3-Code -> (-> Uid))
(define (make-lazy-add-cast-runtime-binding! name code)
  (define boxed-uid? : (Boxof (Option Uid)) (box #f))
  (define (get-uid!) : Uid
    (define uid? (unbox boxed-uid?))
    (cond
      [uid? uid?]
      [else
       (define uid (next-uid! name))
       (set-box! boxed-uid? uid)
       (add-cast-runtime-binding! uid code)
       uid]))
  get-uid!)

(define-type Monotonic-Cast-Type
  (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr CoC3-Expr #:t1 CoC3-Expr) CoC3-Expr))
(define-type Fn-Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Tuple-Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Proxied-Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Project-Type (->* (CoC3-Expr CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Inject-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Apply-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Coerce-Proxied-Type
  (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Apply-Med-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr)
       (CoC3-Expr #:know-not-eq? Boolean) CoC3-Expr))

(define-type Id-Coercion-Huh-Type (CoC3-Expr -> CoC3-Expr))

;; suspend-monotonic-heap-casts? optional keyword parameter is a runtime boolean
;; value that indiciates whether the next monotonic heap cast should be added to
;; the queue or executed.
(define-type Compile-Make-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr)
       (#:top-level? Boolean #:know-not-eq? Boolean)
       CoC3-Expr))

(define-type Make-Med-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr)
       (#:know-not-eq? Boolean)
       CoC3-Expr))

(define-type Lambda-Type (Uid* CoC3-Expr -> CoC3-Expr)) 
(define-type App-Type (CoC3-Expr CoC3-Expr* -> CoC3-Expr))
(define-type Dyn-PBox-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PBox-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PVec-Ref-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PVec-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MBox-Ref-Type
  (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MBox-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MVec-Ref-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MVec-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PVec-Len-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-Fn-App-Type
  (CoC3-Expr CoC3-Expr* Grift-Type* CoC3-Expr -> CoC3-Expr))
(define-type Dyn-Tup-Prj-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MBox-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MBox-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MVec-Ref-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MVec-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Compose-Coercions-Type
  (CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Compile-Cast-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
       (CoC3-Expr #:t1-not-dyn Boolean #:t2-not-dyn Boolean)
       CoC3-Expr))
(define-type Cast-Type (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Cast-Tuple-Type (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Copy-Mref-Type (CoC3-Expr -> CoC3-Expr))
(define-type Make-Coercion-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Id-Coercions-Huh-Type (CoC3-Expr -> CoC3-Expr))

;; Functions for use sites of guarded references with coercions
(define-type PBox-Ref-Type (CoC3-Expr -> CoC3-Expr))
(define-type PBox-Set-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type PVec-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type PVec-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type PVec-Len-Type (CoC3-Expr -> CoC3-Expr))
(define-type State-Reduction-Type (-> CoC3-Expr))

;; This next section of code are the procedures that build all
;; of the runtime code for hyper coercions based casting.

(: make-compile-types-greatest-lower-bound : -> (Code-Label Uid))
(define (make-compile-types-greatest-lower-bound)
  (define (make-code!)
    (define-track-next-uid!$ types-greatest-lower-bound)
    (define types-greatest-lower-bound-label
      (Code-Label types-greatest-lower-bound))
    (: interp-tglb (CoC3-Expr CoC3-Expr -> CoC3-Expr))
    (define (interp-tglb t1 t2)
      (apply-code types-greatest-lower-bound t1 t2))
    (define tglb-code : CoC3-Code
    (code$ (t1 t2)
      (cond$
       [(op=? t1 t2) t1]
       [(type-dyn?$ t1) t2]
       [(type-dyn?$  t2) t1]
       [(and$ (type-fn?$ t1) (type-fn?$ t2)
              (op=? (type-fn-arity$ t1) (type-fn-arity$ t2)))
        (Make-GLB-Two-Fn-Types types-greatest-lower-bound t1 t2)]
       [(and$ (type-pbox?$ t1) (type-pbox?$ t2))
        (Type-GRef (interp-tglb (type-pbox-of$ t1) (type-pbox-of$ t2)))]
       [(and$ (type-pvec?$ t1) (type-pvec?$ t2))
        (Type-GVect (interp-tglb (type-pvec-of$ t1) (type-pvec-of$ t2)))]
       [(and$ (type-mbox?$ t1) (type-mbox?$ t2))
        (Type-MRef (interp-tglb (type-mbox-of$ t1) (type-mbox-of$ t2)))]
       [(and$ (type-mvec?$ t1) (type-mvec?$ t2))
        (Type-MVect (interp-tglb (type-mvec-of$ t1) (type-mvec-of$ t2)))]
       [(and$ (type-tup?$ t1) (type-tup?$ t2)
              (op<=? (type-tup-arity$ t2) (type-tup-arity$ t1)))
        (Make-GLB-Two-Tuple-Types types-greatest-lower-bound t1 t2)]
       [else (Error (Quote "inconsistent types"))])))
    (add-cast-runtime-binding! types-greatest-lower-bound tglb-code)
    (types-greatest-lower-bound-code-label? types-greatest-lower-bound-label)
    types-greatest-lower-bound-label)
  (let ([cl? (types-greatest-lower-bound-code-label?)])
    (or cl? (make-code!))))

(: mvector-val-ref-with-check-bounds (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (mvector-val-ref-with-check-bounds mvect i)
  (Mvector-val-ref mvect i 'check-bounds))

(: mvector-val-set!-with-check-bounds (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (mvector-val-set!-with-check-bounds mvect i val)
  (Mvector-val-set! mvect i val 'check-bounds))

(define (make-map-expr
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
         #:dyn-fn-app   [dyn-fn-app    : Dyn-Fn-App-Type]
         #:dyn-tup-prj  [dyn-tup-prj   : Dyn-Tup-Prj-Type]
         ;; These are node that may get compiled differently for static
         #:mbox         [mbox : (CoC3-Expr Grift-Type -> CoC3-Expr) Mbox]
         #:stc-mbox-ref [stc-mbox-ref : (CoC3-Expr -> CoC3-Expr) Mbox-val-ref]
         #:stc-mbox-set [stc-mbox-set! : (CoC3-Expr CoC3-Expr -> CoC3-Expr) Mbox-val-set!]
         #:mvec         [mvec : (CoC3-Expr CoC3-Expr Grift-Type -> CoC3-Expr) Mvector]
         #:stc-mvec-ref [stc-mvec-ref : (CoC3-Expr CoC3-Expr -> CoC3-Expr) mvector-val-ref-with-check-bounds]
         #:stc-mvec-set [stc-mvec-set! : (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) mvector-val-set!-with-check-bounds]
         #:mvec-len     [mvec-len : (CoC3-Expr -> CoC3-Expr) Mvector-length])
  : (C0-Expr -> CoC3-Expr) 
  ;; map the pass through lists of expressions
  (: recur* (C0-Expr* -> CoC3-Expr*))
  (define (recur* e*) (map recur e*))
  ;; map the pass through lists of bindings
  (: recur-bnd* (C0-Bnd* -> CoC3-Bnd*))
  (define (recur-bnd* b*)
    (map (λ ([b : C0-Bnd]) (cons (car b) (recur (cdr b)))) b*))
  (: recur : C0-Expr -> CoC3-Expr)
  (define (recur e)
    (debug 'interpret-cast-common/map-expr e)
    (match e
      ;; Casts get turned into calls to the cast interpreter with
      ;; hyper-coercion. The later pass makes this slightly more
      ;; efficient by statically allocating the coercion object.
      [(Cast (app recur e) (Twosome t1 t2 l))
       (compile-cast e (Type t1) (Type t2) (Quote l))]
      ;; Lambdas add a extra meta information field that ultimately
      ;; turns into a method for casting a particular arrity at runtime.
      [(Lambda f* (app recur exp))
       (cast-profile/inc-uncasted-function-values$ (compile-lambda f* exp))]
      ;; Applications get turned into an application that "checks for the
      ;; the presence of proxies" This eventually gets optimized aways
      ;; into a functional proxy representation. 
      [(App (app recur e) (app recur* e*))
       (cast-profile/inc-function-uses$ (compile-app e e*))]
      ;; Use make coercion to implement dynamic application without
      ;; allocating a one time use coercion.
      [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
       (cast-profile/inc-function-uses$ (dyn-fn-app e e* t* (Quote l)))]
      ;; Transformation to lower guarded reference types
      ;; Guarded Operations on Guarded values are turned into
      ;; calls/inlinings of the runtime proceedures that perform
      ;; proxied reads and writes.
      ;;-------------------------------------------------------------------
      ;; Every Guarded Reference Starts As an Unguarded box
      [(Gbox (app recur e)) (cast-profile/inc-uncasted-ref-values$ (Unguarded-Box e))]
      [(Gvector (app recur n) (app recur init))
       (cast-profile/inc-uncasted-vector-values$ (Unguarded-Vect n init))]
      ;; Unboxing calls off to the helpers we have defined
      [(Gvector-ref (app recur v) (app recur i))
       (cast-profile/inc-vector-uses$ (pvec-ref v i))]
      [(Gunbox (app recur b))
       (cast-profile/inc-ref-uses$ (pbox-ref b))]
      [(Dyn-GRef-Ref (app recur e) l)
       (cast-profile/inc-ref-uses$ (dyn-pbox-ref e (Quote l)))]
      [(Dyn-GVector-Ref (app recur e) (app recur i) l)
       (cast-profile/inc-vector-uses$ (dyn-pvec-ref e i (Quote l)))]
      ;; Setting a Gaurded reference results in iteratively applying
      ;; all the guarding casts to the value to be written.
      ;; Most of the work is already done but the code requires values
      ;; so there is a little repetative to let bind any values that
      ;; haven't been evaluated.
      [(Gbox-set! (app recur b) (app recur w))
       (cast-profile/inc-ref-uses$ (pbox-set! b w))]
      [(Gvector-set! (app recur v) (app recur i) (app recur w))
       (cast-profile/inc-vector-uses$ (pvec-set! v i w))]
      [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
       (cast-profile/inc-ref-uses$ (dyn-pbox-set! e1 e2 (Type t) (Quote l)))]
      [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (cast-profile/inc-vector-uses$ (dyn-pvec-set! e1 i e2 (Type t) (Quote l)))]
      ;; TODO add tests for dyn-gvector-len
      [(Dyn-GVector-Len (app recur e) (app recur l))
       (cast-profile/inc-vector-uses$ (dyn-pvec-len e l))]
      [(Gvector-length (app recur e))
       (cast-profile/inc-vector-uses$ (pvec-len e))]
      [(MBoxCastedRef (app recur e) t)
       (cast-profile/inc-ref-uses$ (mbox-ref e (Type t)))]
      [(MBoxCastedSet! (app recur e1) (app recur e2) t)
       (cast-profile/inc-ref-uses$ (mbox-set! e1 e2 (Type t)))]
      [(MVectCastedRef (app recur e) (app recur i) t)
       (cast-profile/inc-vector-uses$ (mvec-ref e i (Type t)))]
      [(MVectCastedSet! (app recur e1) (app recur i) (app recur e2) t)
       (cast-profile/inc-vector-uses$ (mvec-set! e1 i e2 (Type t)))]
      [(Dyn-MRef-Ref (app recur e) l)
       (cast-profile/inc-ref-uses$ (dyn-mbox-ref e (Quote l)))]
      [(Dyn-MRef-Set! (app recur e1) (app recur e2) t l)
       (cast-profile/inc-ref-uses$ (dyn-mbox-set! e1 e2 (Type t) (Quote l)))]
      [(Dyn-MVector-Ref (app recur e) (app recur i) l)
       (cast-profile/inc-vector-uses$ (dyn-mvec-ref e i (Quote l)))]
      [(Dyn-MVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (cast-profile/inc-vector-uses$ (dyn-mvec-set! e1 i e2 (Type t) (Quote l)))]
      [(Mvector-ref (app recur e1) (app recur e2))
       (cast-profile/inc-vector-uses$ (stc-mvec-ref e1 e2))]
      [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
       (cast-profile/inc-vector-uses$ (stc-mvec-set! e1 e2 e3))]
      [(Munbox (app recur e))
       (cast-profile/inc-ref-uses$ (stc-mbox-ref e))]
      [(Mbox-set! (app recur e1) (app recur e2))
       (cast-profile/inc-ref-uses$ (stc-mbox-set! e1 e2))]
      [(Mvector-length (app recur e)) 
       (cast-profile/inc-vector-uses$ (mvec-len e))]
      [(Mbox (app recur e) t)
       (cast-profile/inc-uncasted-ref-values$ (mbox e t))]
      [(Mvector (app recur e1) (app recur e2) t)
       (cast-profile/inc-uncasted-vector-values$ (mvec e1 e2 t))]
      ;; While tuples don't get any special attention in this pass
      ;; dynamic tuple projection needs to get dusugared
      [(Create-tuple e*)
       (cast-profile/inc-uncasted-tuple-values$ (Create-tuple (recur* e*)))]
      [(Tuple-proj e i) (cast-profile/inc-tuple-uses$ (Tuple-proj (recur e) (Quote i)))]
      [(Dyn-Tuple-Proj (app recur e) (app recur i) (app recur l))
       (cast-profile/inc-tuple-uses$ (dyn-tup-prj e i l))]
      ;; Simple Recursion Patterns
      [(Observe (app recur e) t)
       (Observe e t)]
      [(Letrec bnd* exp)
       (Letrec (recur-bnd* bnd*) (recur exp))]
      [(Let bnd* exp)
       (Let (recur-bnd* bnd*) (recur exp))]
      [(Op p exp*)
       (Op p (recur* exp*))]
      [(If tst csq alt)
       (If (recur tst) (recur csq) (recur alt))]
      [(Switch e c* d)
       (Switch (recur e) (map-switch-case* recur c*) (recur d))]
      [(Begin e* e)
       (Begin (recur* e*) (recur e))]
      [(Repeat i e1 e2 a e3 e4)
       (Repeat i (recur e1) (recur e2) a (recur e3) (recur e4))]
      [(Var id)    (Var id)]
      [(Quote lit) (Quote lit)]
      [(and noop (No-Op)) noop]))
  ;; Body of map-expr
  ;; just return the recursion procedure
  recur)

;; build-caster! generates a piece of code that performs a fn-cast
;; for a specific arity. 

;; This could be done as 3 seperate casters one that handles both
;; casted and uncasted closures and another each seperately.
;; The general caster is used in static code
;; The uncasted is attatched to the original closure
;; The casted is attatched to the proxy
(define ((make-build-caster/coercions
          #:apply-coercion-uid [apply-coercion-uid : Uid]
          #:compose-coercions-uid [compose-coercions-uid : Uid]
          #:compose-coercions  [compose-coercions : Compose-Coercions-Type]
          #:id-coercion-huh    [id-coercion-huh : Id-Coercion-Huh-Type])
         [arity : Nat])
  : (Values Uid CoC3-Code)
  (define casting-code-name (next-uid! (format "fn-cast-~a" arity)))
  ;; This function generates a series of if expressions
  ;; equivalent to an or of the list of input expressions. 
  (: or-help : (Listof CoC3-Expr) -> CoC3-Expr)
  (define (or-help a)
    (cond
      [(null? a) (Quote #t)]
      [(null? (cdr a)) (car a)]
      [else (If (car a) (or-help (cdr a)) (Quote #f))]))
  ;; This function generate a series of a list of checks
  ;; for identity coercions?
  (: id-c*? : Uid* -> CoC3-Expr*)
  (define (id-c*? x*)
    (map (lambda ([x : Uid]) (id-coercion-huh (Var x))) x*))
  ;; new fn-crcn generates the result if a new coercion is needed
  (: new-fn-crcn : Uid Uid Nat Uid* Uid -> CoC3-Expr)
  (define (new-fn-crcn t1 t2 arity args ret)
    (Fn-Coercion (map (inst Var Uid) args) (Var ret)))
  ;; compose-return generate a bindining that composes the return
  ;; of two hyper-coercions if present
  (: compose-return (Uid (Var Uid) (Var Uid) -> CoC3-Bnd))
  (define (compose-return ret old-crcn new-crcn)
    `[,ret . ,(compose-coercions (Fn-Coercion-Return old-crcn)
                                 (Fn-Coercion-Return new-crcn))])
  ;; compose-arg generates a binding that composes the ith
  ;; fn-coercion of the two input coercions. 
  (: compose-arg (Uid (Var Uid) (Var Uid) (Quote Index) -> CoC3-Bnd))
  (define (compose-arg arg old-crcn new-crcn i)
    `[,arg . ,(compose-coercions (Fn-Coercion-Arg new-crcn i)
                                 (Fn-Coercion-Arg old-crcn i))])
  ;; the ids to build a new code
  (define u-clos (next-uid! "unknown_closure"))
  (define new-crcn (next-uid! "new_fn_coercion"))
  (define arg*
    (for/list : (Listof Uid) ([i (in-range arity)])
      (next-uid! "arg_coercion")))
  ;; TODO remove this sanity check once we know the code works
  (unless (= (length arg*) arity)
    (error 'sanity-check "argument list / arity mismatch: (length ~a) != ~a"
           arg* arity))
  (define ret  (next-uid! "ret_coercion"))
  (define old-crcn (next-uid! "old_fn_coercion"))
  (define r-clos (next-uid! "raw_closure"))
  (define t1 (next-uid! "from_type"))
  (define t2 (next-uid! "to_type"))
  
  ;; generate all the compositions
  (: composed-coercions-bindings : (Var Uid) (Var Uid) -> CoC3-Bnd*)
  (define (composed-coercions-bindings old-crcn new-crcn) : CoC3-Bnd*
    (for/fold ([b* : CoC3-Bnd* `(,(compose-return ret old-crcn new-crcn))])
              ([a : Uid (reverse arg*)]
               [i : Integer (in-range (- arity 1) -1 -1)])
      (unless (index? i)
        (error 'lower-function-casts "bad index made"))
      (cons (compose-arg a old-crcn new-crcn (Quote i)) b*)))
  ;; an expression that checks if all composes resulted in ID
  (define all-result-in-id-huh : CoC3-Expr
    (or-help (id-c*? (cons ret arg*))))
  
  ;; Body of build-fn-caster stitch together the AST of the
  ;; code that forms a fn-cast for a particular arity. 
  (define casting-code
    (let ()
      ;; Type racket forgets this if it isn't in the define
      (unless (index? arity)
        (error 'lower-function-cast/build-fn-cast-with-coercion
               "arity grew too large to be a valid index"))
      (code$ (fun crcn)
        (cast-profile/inc-function-casts$
         ;; Is the closure we are casting a hybrid proxy
         (If (Fn-Proxy-Huh fun)
             ;; If so we have to compose the old and new fn-coercions
             ;; First get the old fn-coercion
             ;; i.e. destructure old proxy
             (let$ ([old-crcn (Fn-Proxy-Coercion fun)]
                    [raw-clos (Fn-Proxy-Closure  fun)])
               ;; Then compose each sub coercion
               ;; this loop reverses the list arguments hence the
               ;; reverse that is used in the argument list
               (Let (composed-coercions-bindings old-crcn crcn)
                 ;; Check if all Composes resulted in Id Coercions
                 (If all-result-in-id-huh
                     ;; If so the original closure is the correct type
                     raw-clos
                     ;; Otherwise a new proxy is needed
                     (Fn-Proxy (list #{arity :: Index} apply-coercion-uid compose-coercions-uid)
                               raw-clos
                               (new-fn-crcn t1 t2 arity arg* ret)))))
             ;; Closure is unproxied --> just make a new proxy
             (Fn-Proxy (list #{arity :: Index} apply-coercion-uid compose-coercions-uid) fun crcn))))))
  (values casting-code-name casting-code))

(: make-fn-cast-helpers : (Nat -> (Values Uid CoC3-Code)) -> (Nat -> Uid))
(define (make-fn-cast-helpers build-caster!)
  ;; A map from arity to the binding of the code that handles that arity
  ;; at runtime.
  ;; This 
  (define fn-cast-uid-map : (HashTable Nat Uid) (make-hasheq))
    ;; Get the uid of fn-cast that can handle
    (: get-fn-cast! : Nat -> Uid)
    (define (get-fn-cast! arity)
      (let ([uid? (hash-ref fn-cast-uid-map arity #f)])
        (cond
          [uid? uid?]
          [else
           (define-values (uid code) (build-caster! arity))
           (add-cast-runtime-binding! uid code)
           (hash-set! fn-cast-uid-map arity uid)
           uid])))
    get-fn-cast!)

(: make-proxied-reference/coercions-code-gen-helpers
       (->* (#:apply-coercion Apply-Coercion-Type)
            (Values PBox-Ref-Type PBox-Set-Type
                    PVec-Ref-Type PVec-Set-Type PVec-Len-Type)))
(define (make-proxied-reference/coercions-code-gen-helpers #:apply-coercion apply-coercion)
  ;; Proxied References implementation
  ;; In general each operation has to check if the reference is proxied.
  ;; Then depending on whether it is a read or write operation cast the read
  ;; or written value using the read or write coercion in the reference coercion
  ;; contained by the proxy. 
  ;; Because hyper-coercions are space-efficient we know that at most
  ;; one layer of proxying will be present. This is ensured by the reference
  ;; casting operation which check for proxies on values before casting them. 
  ;; This is very similar to the coercions implementation
  ;; TODO: Abstract the common code between coercions and hyper-coercions
  (: code-gen-pbox-ref : PBox-Ref-Type)
  (define (code-gen-pbox-ref e-gref)
    (cast-profile/max-ref-chain$
     (let$ ([v e-gref])
       (If (Guarded-Proxy-Huh v)
           (cast-profile/inc-ref-proxies-accessed$
            (let$ ([u (Guarded-Proxy-Ref v)]
                   [c (Guarded-Proxy-Coercion v)])
              (apply-coercion (Unguarded-Box-Ref u) (Ref-Coercion-Read c))))
           (Unguarded-Box-Ref v)))))
  
  (: code-gen-pbox-set! PBox-Set-Type)
  (define (code-gen-pbox-set! e-gref w-val)
    (cast-profile/max-ref-chain$
     (let$ ([v e-gref][w w-val])
       (If (Guarded-Proxy-Huh v)
           (cast-profile/inc-ref-proxies-accessed$
            (let$ ([u (Guarded-Proxy-Ref v)]
                   [c (Ref-Coercion-Write (Guarded-Proxy-Coercion v))])
              (Unguarded-Box-Set! u (apply-coercion w c))))
           (Unguarded-Box-Set! v w)))))

  (: code-gen-pvec-ref PVec-Ref-Type)
  (define (code-gen-pvec-ref e-gref i-index)
    (cast-profile/max-vector-chain$
     (let$ ([v e-gref][i i-index])
       (If (Guarded-Proxy-Huh v)
           (cast-profile/inc-vector-proxies-accessed$
            (let$ ([u (Unguarded-Vect-Ref (Guarded-Proxy-Ref v) i)]
                   [c (Ref-Coercion-Read (Guarded-Proxy-Coercion v))])
              (apply-coercion u c)))
           (Unguarded-Vect-Ref v i)))))

  (: code-gen-pvec-set! PVec-Set-Type)
  (define (code-gen-pvec-set! e-gref i-index w-val)
    (cast-profile/max-vector-chain$
     (let$ ([v e-gref][i i-index][w w-val])
       (If (Guarded-Proxy-Huh v)
           (cast-profile/inc-vector-proxies-accessed$
            (let$ ([u (Guarded-Proxy-Ref v)]
                   [w (apply-coercion w (Ref-Coercion-Write (Guarded-Proxy-Coercion v)))])
              (Unguarded-Vect-Set! u i w)))
           (Unguarded-Vect-Set! v i w)))))
  
  (: code-gen-pvec-len PVec-Len-Type)
  (define (code-gen-pvec-len e-gvec)
    (cast-profile/max-vector-chain$
     (let$ ([v e-gvec])
       (If (Guarded-Proxy-Huh v)
           (cast-profile/inc-vector-proxies-accessed$
            (Unguarded-Vect-length (Guarded-Proxy-Ref v)))
           (Unguarded-Vect-length v)))))

  (values code-gen-pbox-ref
          code-gen-pbox-set!
          code-gen-pvec-ref
          code-gen-pvec-set!
          code-gen-pvec-len))

(: apply-coercion/make-coercion->compile-cast :
   Apply-Coercion-Type Make-Coercion-Type -> Cast-Type)
(define ((apply-coercion/make-coercion->compile-cast
          apply-coercion make-coercion)
         v t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
  ;; TODO help compile coercion
  (apply-coercion v (make-coercion t1 t2 l) suspend-monotonic-heap-casts?))

(define monotonic-blame (Quote "Monotonic references do not currently track blame"))

(define (make-compile-mref-state-reduction
         #:interp-cast interp-cast
         #:greatest-lower-bound greatest-lower-bound)
  (define (code-gen-mref-state-reduction)
    (while$ (op$ mref-cast-queue-not-empty?)
            (let*$ ([address (op$ mref-cast-queue-peek-address)]
                    [t1 (Mbox-rtti-ref address)]                 
                    [t2 (op$ mref-cast-queue-peek-type)]
                    [t3 (app-code$ greatest-lower-bound t1 t2)])
              (op$ mref-cast-queue-dequeue)
              (when$ (not$ (op=? t1 t3))
                     (let*$ ([vi (Mbox-val-ref address)]
                             [cvi (interp-cast vi t1 t3 monotonic-blame suspend-monotonic-heap-casts)])
                       (Mbox-rtti-set! address t3)
                       (Mbox-val-set! address cvi))))))
  (define (interp-mref-state-reduction)
    (apply-code (mref-state-reduction-uid?)))
  (define (make-code!)
    (define-track-next-uid!$ mref-state-reduction)
    (define mref-state-reduction-uid mref-state-reduction)
    (add-cast-runtime-binding!
     mref-state-reduction
     (code$ ()
       (begin$
         (code-gen-mref-state-reduction)
         ZERO-EXPR)))
    (mref-state-reduction-uid? mref-state-reduction-uid))
  (define (mref-state-reduction)
    (if (monotonic-cast-inline-without-types?)
        (begin$ (code-gen-mref-state-reduction) ZERO-EXPR)
        (let ([uid? (mref-state-reduction-uid?)])
          (or uid? (make-code!))
          (interp-mref-state-reduction))))
  mref-state-reduction)

(define (make-compile-mvect-state-reduction
         #:interp-cast interp-cast
         #:greatest-lower-bound greatest-lower-bound)
  (define (code-gen-mvect-state-reduction)
    (while$ (op$ mvect-cast-queue-not-empty?)
            (let*$ ([address (op$ mvect-cast-queue-peek-address)]
                    [t1 (Mvector-rtti-ref address)]                 
                    [t2 (op$ mvect-cast-queue-peek-type)]
                    [t3 (app-code$ greatest-lower-bound t1 t2)])
              (op$ mvect-cast-queue-dequeue)
              (when$ (not$ (op=? t1 t3))
                     (let$ ([len (Mvector-length address)])
                       (repeat$ (i ZERO-EXPR len) ()
                         (let*$ ([vi (Mvector-val-ref address i 'no-check-bounds)]
                                 [cvi (interp-cast vi t1 t3 monotonic-blame suspend-monotonic-heap-casts)]
                                 [t4 (Mvector-rtti-ref address)])
                           (Mvector-val-set! address i cvi 'no-check-bounds)))
                       (Mvector-rtti-set! address t3))))))
  (define (interp-mvect-state-reduction)
    (apply-code (mvect-state-reduction-uid?)))
  (define (make-code!)
    (define-track-next-uid!$ mvect-state-reduction)
    (define mvect-state-reduction-uid mvect-state-reduction)
    (add-cast-runtime-binding!
     mvect-state-reduction
     (code$ ()
       (begin$
         (code-gen-mvect-state-reduction)
         ZERO-EXPR)))
    (mvect-state-reduction-uid? mvect-state-reduction-uid))
  (define (mvect-state-reduction)
    (if (monotonic-cast-inline-without-types?)
        (begin$ (code-gen-mvect-state-reduction) ZERO-EXPR)
        (let ([uid? (mvect-state-reduction-uid?)])
          (or uid? (make-code!))
          (interp-mvect-state-reduction))))
  mvect-state-reduction)

(: make-monotonic-helpers
   (->* (#:interp-cast Cast-Type
         #:mref-state-reduction State-Reduction-Type
         #:mvect-state-reduction State-Reduction-Type)
        (#:compile-cast   Compile-Cast-Type
         #:apply-coercion Apply-Coercion-Type
         #:make-coercion  Make-Coercion-Type) 
        (Values MBox-Ref-Type MBox-Set-Type
                MVec-Ref-Type MVec-Set-Type)))
(define (make-monotonic-helpers
         #:interp-cast interp-cast
         #:mref-state-reduction mref-state-reduction
         #:mvect-state-reduction mvect-state-reduction
         #:compile-cast   [compile-cast : (Option Compile-Cast-Type) #f] 
         #:apply-coercion [apply-coercion : (Option Apply-Coercion-Type) #f]
         #:make-coercion  [make-coercion : (Option Make-Coercion-Type) #f])
  ;; Monotonic Reference Types Implementation
  ;; TODO add switch that allows providing both and systematically enable
  ;; one or the other definition.
  (define cast : Cast-Type
    (cond
      [compile-cast compile-cast]
      [(and apply-coercion make-coercion)
       (apply-coercion/make-coercion->compile-cast apply-coercion
                                                   make-coercion)]
      [else
       (error 'make-monotonic-helpers "no implementation of casts provided")]))

  (: code-gen-mbox-ref MBox-Ref-Type)
  (define (code-gen-mbox-ref mref t2)
    (let*$ ([mref mref])
      (cast (Mbox-val-ref mref) (Mbox-rtti-ref mref) t2 (Quote "Monotonic ref read error"))))
  
  (: bnd-t1-if-not-type : CoC3-Expr (CoC3-Expr -> CoC3-Expr) -> CoC3-Expr)
  (define (bnd-t1-if-not-type t1 type->expr)
    (match t1
      [(Type _) (type->expr t1)]
      [_ (let$ ([t1 t1]) (type->expr t1))]))
  
  (: code-gen-mbox-set! MBox-Set-Type)
  (define (code-gen-mbox-set! mref val t1)
    (bnd-t1-if-not-type
     t1
     (lambda ([t1 : CoC3-Expr]) : CoC3-Expr
       (let*$ ([address mref]
               [val val]
               [t2 (Mbox-rtti-ref address)]
               [cv (cast val t1 t2 (Quote "Monotonic ref write error") suspend-monotonic-heap-casts)])
         (Mbox-val-set! address cv)
         (mref-state-reduction)))))

  (: code-gen-mvec-ref MVec-Ref-Type)
  (define (code-gen-mvec-ref mvec i t2)
    (let*$ ([v mvec])
      (cast (Mvector-val-ref v i 'check-bounds) (Mvector-rtti-ref mvec) t2 (Quote "Monotonic vect read error"))))
  
  (: code-gen-mvec-set! MVec-Set-Type)
  (define (code-gen-mvec-set! mvec i val t1)
    (bnd-t1-if-not-type
     t1
     (lambda ([t1 : CoC3-Expr])
       : CoC3-Expr
       (let*$ ([address mvec] [i i] [val val] 
               [t2 (Mvector-rtti-ref address)]
               [cvi (cast val t1 t2 (Quote "Monotonic vect write error") suspend-monotonic-heap-casts)])
         (Mvector-val-set! address i cvi 'check-bounds)
         (mvect-state-reduction)))))

  (cond
    [(inline-monotonic-branch?)
     (values code-gen-mbox-ref code-gen-mbox-set!
             code-gen-mvec-ref code-gen-mvec-set!)]
    [else
     (define mbox-ref-uid  (next-uid! "mbox-ref"))
     (define mbox-set!-uid (next-uid! "mbox-set!")) 
     (define mvec-ref-uid  (next-uid! "mvec-ref"))
     (define mvec-set!-uid (next-uid! "mvec-set!"))
     (add-cast-runtime-binding!
      mbox-ref-uid
      (code$ (mbox t2) (code-gen-mbox-ref mbox t2)))
     (add-cast-runtime-binding!
      mbox-set!-uid
      (code$ (mbox v t1) (code-gen-mbox-set! mbox v t1)))
     (add-cast-runtime-binding!
      mvec-ref-uid
      (code$ (mvec i t2) (code-gen-mvec-ref mvec i t2)))
     (add-cast-runtime-binding!
      mvec-set!-uid
      (code$ (mbox i v t1) (code-gen-mvec-set! mbox i v t1)))
     (values (apply-code-curry mbox-ref-uid) (apply-code-curry mbox-set!-uid)
             (apply-code-curry mvec-ref-uid) (apply-code-curry mvec-set!-uid))]))

(: make-dynamic-operations-helpers
   (->* (#:compile-app App-Type
         #:pbox-ref PBox-Ref-Type
         #:pbox-set PBox-Set-Type
         #:pvec-ref PVec-Ref-Type
         #:pvec-set PVec-Set-Type
         #:pvec-len PVec-Len-Type
         #:mbox-ref MBox-Ref-Type
         #:mbox-set MBox-Set-Type
         #:mvec-ref MVec-Ref-Type
         #:mvec-set MVec-Set-Type
         #:compile-cast Compile-Cast-Type) 
        (Values Dyn-PBox-Ref-Type Dyn-PBox-Set-Type
                Dyn-PVec-Ref-Type Dyn-PVec-Set-Type Dyn-PVec-Len-Type
                Dyn-MBox-Ref-Type Dyn-MBox-Set-Type
                Dyn-MVec-Ref-Type Dyn-MVec-Set-Type
                Dyn-Fn-App-Type Dyn-Tup-Prj-Type)))
(define (make-dynamic-operations-helpers
         #:compile-app compile-app
         #:pbox-ref pbox-ref #:pbox-set pbox-set!
         #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
         #:mbox-ref mbox-ref #:mbox-set mbox-set!
         #:mvec-ref mvec-ref #:mvec-set mvec-set!
         #:compile-cast compile-cast)
  
  (: code-gen-dyn-pbox-ref Dyn-PBox-Ref-Type)
  (define (code-gen-dyn-pbox-ref dyn lbl)
    (let*$ ([v dyn] [l lbl])
      (If (Dyn-Immediate-Tag=Huh v PBOX-DYN-EXPR)
          (let*$ ([val (Dyn-Box-Value v)]
                  [ty  (Dyn-Box-Type v)]
                  [ty  (unfold-possible-mu$ ty)])
            (If (Type-GRef-Huh ty)
                (let*$ ([tyof (Type-GRef-Of ty)]
                        [read-val (pbox-ref val)])
                  (compile-cast read-val tyof DYN-EXPR l))
                (Blame l)))
          (Blame l))))
  (: code-gen-dyn-pbox-set! Dyn-PBox-Set-Type)
  (define (code-gen-dyn-pbox-set! dyn-gbox wrt-val1 t2 lbl)
    (let*$ ([dyn-gbox dyn-gbox]
            [wrt-val1 wrt-val1]
            [t2 t2]
            [lbl lbl])
      (If (ann (Dyn-Immediate-Tag=Huh dyn-gbox PBOX-DYN-EXPR) CoC3-Expr)
          (let*$ ([gbox (ann (Dyn-Box-Value dyn-gbox) CoC3-Expr)]
                  [ty   (ann (Dyn-Box-Type dyn-gbox) CoC3-Expr)]
                  [ty   (unfold-possible-mu$ ty)])
                 (If (Type-GRef-Huh ty)
                (let$ ([tyof (Type-GRef-Of ty)])
                  (cond$
                   [(op=? tyof t2)
                    (pbox-set! gbox wrt-val1)]
                   [else
                    (let$ ([wrt-val2 (compile-cast wrt-val1 t2 tyof lbl)])
                      (pbox-set! gbox wrt-val2))])
                  (compile-cast (Quote '()) UNIT-EXPR DYN-EXPR lbl))
                (Blame lbl)))
          (Blame lbl))))
  
  (: code-gen-dyn-pvec-ref Dyn-PVec-Ref-Type)
  (define (code-gen-dyn-pvec-ref dyn ind lbl)
    (let*$ ([dyn dyn][ind ind][lbl lbl])
      (cond
        [(Dyn-Immediate-Tag=Huh dyn PBOX-DYN-EXPR)
         (let*$ ([maybe-pvec-val (Dyn-Box-Value dyn)]
                 [ty             (Dyn-Box-Type dyn)]
                 [maybe-pvec-ty  (unfold-possible-mu$ ty)])
           (cond$
            [(Type-GVect-Huh maybe-pvec-ty)
             (let$ ([elem-ty (Type-GVect-Of maybe-pvec-ty)]
                    [elem-val (pvec-ref maybe-pvec-val ind)])
               (compile-cast elem-val elem-ty DYN-EXPR lbl))]
            [else (Blame lbl)]))])))
  
  (: code-gen-dyn-pvec-set! Dyn-PVec-Set-Type)
  (define (code-gen-dyn-pvec-set! dyn-gvec ind wrt-val1 t2 lbl) 
    (let*$ ([dyn-gvec dyn-gvec]
            [ind ind]
            [wrt-val1 wrt-val1]
            [t2 t2]
            [lbl lbl])
      (cond$
       [(Dyn-Immediate-Tag=Huh dyn-gvec PBOX-DYN-EXPR)
        (let*$ ([maybe-vec      (Dyn-Box-Value dyn-gvec)]
                [ty             (Dyn-Box-Type  dyn-gvec)]
                [maybe-vec-type (unfold-possible-mu$ ty)])
          (cond$ 
           [(Type-GVect-Huh maybe-vec-type)
            (let*$ ([elem-type (Type-GVect-Of maybe-vec-type)]
                    [new-elem (If (op=? elem-type t2)
                                  wrt-val1
                                  (compile-cast wrt-val1 t2 elem-type lbl))])
              (pvec-set! maybe-vec ind new-elem)
              (compile-cast (Quote '()) UNIT-EXPR DYN-EXPR lbl))]
           [else (Blame lbl)]))]
       [else (Blame lbl)])))
  
  (: code-gen-dyn-pvec-len Dyn-PVec-Len-Type)
  (define (code-gen-dyn-pvec-len expr lbl)
    (let*$ ([v expr]
            [l lbl])
      (cond$
       [(Dyn-Immediate-Tag=Huh v PVEC-DYN-EXPR)
        (let*$ ([val (Dyn-Box-Value v)]
                [ty  (Dyn-Box-Type v)]
                [ty  (unfold-possible-mu$ ty)])
          (cond$
           [(Type-GVect-Huh ty) (pvec-len val)]
           [else (Blame l)]))]
       [else (Blame l)])))
  
  (: code-gen-dyn-mbox-ref Dyn-MBox-Ref-Type)
  (define (code-gen-dyn-mbox-ref dyn lbl)
    (let$ ([dyn dyn] [lbl lbl])
      (cond$
       [(Dyn-Immediate-Tag=Huh dyn MBOX-DYN-EXPR)
        (let*$ ([val (Dyn-Box-Value dyn)]
                [ty  (Dyn-Box-Type dyn)]
                [ty  (unfold-possible-mu$ ty)])
          (cond$
           [(Type-MRef-Huh ty) (mbox-ref val DYN-EXPR)]
           [else (Blame lbl)]))]
        [else (Blame lbl)])))
  
  (: code-gen-dyn-mbox-set! Dyn-MBox-Set-Type)
  (define (code-gen-dyn-mbox-set! dyn-mbox wrt-val1 t2 lbl)
    (let$ ([dyn dyn-mbox] [val wrt-val1] [t2 t2] [lbl lbl])
      (cond$
       [(Dyn-Immediate-Tag=Huh dyn MBOX-DYN-EXPR)
        (let*$ ([mbox (Dyn-Box-Value dyn)]
                [t1 (Dyn-Box-Type dyn)]
                [t1 (unfold-possible-mu$ t1)])
          (cond$
           [(Type-MRef-Huh t1)
            (let$ ([tyof (Type-MRef-Of t1)])
              (If (Type-Dyn-Huh tyof)
                  (compile-cast (mbox-set! mbox wrt-val1 t2) UNIT-EXPR DYN-EXPR lbl)
                  (mbox-set! mbox wrt-val1 t2)))]
           [else (Blame lbl)]))]
       [else (Blame lbl)])))
  
  (: code-gen-dyn-mvec-ref Dyn-MVec-Ref-Type)
  (define (code-gen-dyn-mvec-ref dyn ind lbl)
    (let$ ([dyn dyn] [ind ind] [lbl lbl])
      (cond$
       [(Dyn-Immediate-Tag=Huh dyn MVEC-DYN-EXPR)
        (let*$ ([ty (Dyn-Box-Type dyn)]
                [ty (unfold-possible-mu$ ty)])
          (cond$
           [(Type-MVect-Huh ty)
            (mvec-ref (Dyn-Box-Value dyn) ind DYN-EXPR)]
           [else (Blame lbl)]))]
       [else (Blame lbl)])))
  
  (: code-gen-dyn-mvec-set! Dyn-MVec-Set-Type)
  (define (code-gen-dyn-mvec-set! dyn-mvec ind wrt-val1 t2 lbl) 
    (let$ ([dyn dyn-mvec] [ind ind] [vale wrt-val1] [t2 t2] [lbl lbl])
      (cond$
       [(Dyn-Immediate-Tag=Huh dyn MVEC-DYN-EXPR)
        (let*$ ([val (Dyn-Box-Value dyn-mvec)]
                [ty  (Dyn-Box-Type dyn-mvec)]
                [ty  (unfold-possible-mu$ ty)])
          (cond$
           [(Type-MVect-Huh ty)
            (let$ ([tyof (Type-MVect-Of ty)])
              (cond$
               [(Type-Dyn-Huh tyof)
                (compile-cast (mvec-set! val ind wrt-val1 t2) UNIT-EXPR DYN-EXPR lbl)]
               [else (mvec-set! val ind wrt-val1 t2)]))]
           [else (Blame lbl)]))]
       [else (Blame lbl)])))
  
  (: code-gen-dyn-fn-app Dyn-Fn-App-Type)
  (define (code-gen-dyn-fn-app e e* t* le)
    (define bnd (inst cons Uid CoC3-Expr))
    (unless (= (length e*) (length t*))
      (error 'interpret-casts-with-hyper-coercions/make-fn-app
             "expected types to be same length as arguments"))
    (define app-arity (length e*))
    (define fn-of-dyns-type
      (Type (Fn app-arity (make-list app-arity DYN-TYPE) DYN-TYPE)))
    (let$ ([dyn-fn-blame-info le] [dyn-v e])
      (let-values
          ;; I got tired of trying to make this discriptive name fit
          ([(l) dyn-fn-blame-info]
           [(u* v*) (for/lists ([u* : Uid*]
                                [v* : (Listof (Var Uid))])
                               ([_ e*])
                      (define u (next-uid! "dyn_fn_arg"))
                      (values u (Var u)))])
        (Let (map bnd u* e*)
          (cond$
           ;; This condition asks if this is a boxed dynamic value
           [(Dyn-Immediate-Tag=Huh dyn-v FN-DYN-DYN-EXPR)
            (let*$ ([unboxed-value (Dyn-Box-Value dyn-v)]
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
                  (Let (map bnd cu* ce*)
                    (let$ ([ret-val (compile-app unboxed-value cv*)]
                           [ret-ty  (Type-Fn-return src-type)])
                      (compile-cast ret-val ret-ty DYN-EXPR l))))]
               [else (Blame l)]))]
          [else (Blame l)])))))
  
  (: code-gen-dyn-tup-prj Dyn-Tup-Prj-Type)
  (define (code-gen-dyn-tup-prj e ie le)
    (let$ ([v e] [i ie] [l le])
      (cond$
       [(ann (Dyn-Immediate-Tag=Huh v TUPLE-DYN-EXPR) CoC3-Expr)
        (let*$ ([u  (ann (Dyn-Box-Value v) CoC3-Expr)]
                [t0 (ann (Dyn-Box-Type v) CoC3-Expr)]
                [ty (unfold-possible-mu$ t0)])
          (cond$
           [(ann (and$ (Type-Tuple-Huh ty)
                       (Op '> (list (Type-Tuple-num ty) i)))
                 CoC3-Expr)
            (let$ ([prj-val (ann (Tuple-proj u i) CoC3-Expr)]
                   [prj-ty  (ann (Type-Tuple-item ty i) CoC3-Expr)])
              (ann (compile-cast prj-val prj-ty DYN-EXPR l) CoC3-Expr))]
           [else (Blame l)]))]
       [else (Blame l)])))
  (case (dynamic-operations?)
    ;; In the case that dynamic operations specialization isn't
    ;; enabled we should raise an error if any of the helpers
    ;; are ever invoked.
    [(#f)
     (define ((th-error [sym : Symbol]) . a)
       (error sym "dynamic-operation? = #f but present in AST"))
     (values
      (th-error 'interpret-cast-with-coercions/dyn-gbox-ref)
      (th-error 'interpret-cast-with-coercions/dyn-gbox-set!)
      (th-error 'interpret-cast-with-coercions/dyn-gvec-ref)
      (th-error 'interpret-cast-with-coercions/dyn-gvec-set!)
      (th-error 'interpret-cast-with-coercions/dyn-gvec-len)
      (th-error 'interpret-cast-with-coercions/dyn-mbox-ref)
      (th-error 'interpret-cast-with-coercions/dyn-mbox-set!)
      (th-error 'interpret-cast-with-coercions/dyn-mvec-ref)
      (th-error 'interpret-cast-with-coercions/dyn-mvec-set!)
      (th-error 'interpret-cast-with-coercions/dyn-fn-app)
      (th-error 'interpret-cast-with-hyper-coercions/dyn-tup-prj))]
    [(inline)
     ;; In the case that dynamic operations are inlined we should
     ;; generate the code in-place and therefore do not need any
     ;; runtime support for dynamic-operations.
     ;; no code bindings returned.
     (values
      code-gen-dyn-pbox-ref
      code-gen-dyn-pbox-set!
      code-gen-dyn-pvec-ref
      code-gen-dyn-pvec-set!
      code-gen-dyn-pvec-len
      code-gen-dyn-mbox-ref
      code-gen-dyn-mbox-set!
      code-gen-dyn-mvec-ref
      code-gen-dyn-mvec-set!
      code-gen-dyn-fn-app
      code-gen-dyn-tup-prj)]
    [else
     (define dpbr   (next-uid! "rt_dyn_pbox_ref"))
     (define dpbs   (next-uid! "rt_dyn_pbox_set"))
     (define dpvr   (next-uid! "rt_dyn_pvec_ref"))
     (define dpvs   (next-uid! "rt_dyn_pvec_set"))
     (define dpvl   (next-uid! "rt_dyn_pvec_len"))
     (define dmbr   (next-uid! "rt_dyn_mbox_ref"))
     (define dmbs   (next-uid! "rt_dyn_mbox_set"))
     (define dmvr   (next-uid! "rt_dyn_mvec_ref"))
     (define dmvs   (next-uid! "rt_dyn_mvec_set"))
     (define dtp    (next-uid! "rt_dyn_tuple_project"))
     (add-cast-runtime-binding!
      dpbs (code$ (ref val ty lbl)
             (code-gen-dyn-pbox-set! ref val ty lbl)))
     (add-cast-runtime-binding!
      dpvr (code$ (vec ind lbl)
             (code-gen-dyn-pvec-ref vec ind lbl)))
     (add-cast-runtime-binding!
      dpvs (code$ (vec ind val ty lbl)
             (code-gen-dyn-pvec-set! vec ind val ty lbl)))
     (add-cast-runtime-binding!
      dpvl (code$ (vec lbl)
             (code-gen-dyn-pvec-len vec lbl)))
     (add-cast-runtime-binding!
      dmbr (code$ (ref lbl)
             (code-gen-dyn-mbox-ref ref lbl)))
     (add-cast-runtime-binding!
      dmbs (code$ (ref val ty lbl)
             (code-gen-dyn-mbox-set! ref val ty lbl)))
     (add-cast-runtime-binding!
      dmvr (code$ (vec ind lbl)
             (code-gen-dyn-mvec-ref vec ind lbl)))
     (add-cast-runtime-binding!
      dmvs (code$ (vec ind val ty lbl)
             (code-gen-dyn-mvec-set! vec ind val ty lbl)))
     (add-cast-runtime-binding!
      dtp (code$ (tup ind lbl)
            (code-gen-dyn-tup-prj tup ind lbl)))
     (add-cast-runtime-binding!
      dpbr (code$ (ref lbl)
             (code-gen-dyn-pbox-ref ref lbl)))
     (values 
      (apply-code-curry dpbr)
      (apply-code-curry dpbs)
      (apply-code-curry dpvr)
      (apply-code-curry dpvs)
      (apply-code-curry dpvl)
      (apply-code-curry dmbr)
      (apply-code-curry dmbs)
      (apply-code-curry dmvr)
      (apply-code-curry dmvs)
      ;; Always inline since code is arity dependent
      ;; We should set up the alternative where we use an arity based
      ;; non-parametrically pollymorphic approach.
      code-gen-dyn-fn-app
      (apply-code-curry dtp))]))

(define-type Compile-Med-Cast-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
       (CoC3-Expr #:know-not-eq? Boolean #:interpret? Boolean)
       CoC3-Expr))

(: code-gen-entire-med-cast
   (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr
                   #:fn-cast     Fn-Cast-Type
                   #:tuple-cast  Tuple-Cast-Type
                   #:pref-cast   Proxied-Cast-Type
                   #:pvec-cast   Proxied-Cast-Type
                   #:mbox-cast   Monotonic-Cast-Type
                   #:mvec-cast   Monotonic-Cast-Type
                   #:interp-med-cast Cast-Type)
        (CoC3-Expr)
       CoC3-Expr))

;; This is needed in case we want to manually inline 
(define (code-gen-entire-med-cast v t1 t2 l
                                  #:fn-cast    compile-fn-cast
                                  #:tuple-cast compile-tuple-cast
                                  #:pref-cast  compile-pref-cast
                                  #:pvec-cast  compile-pvec-cast
                                  #:mbox-cast  compile-mbox-cast
                                  #:mvec-cast  compile-mvec-cast
                                  #:interp-med-cast interp-med-cast
                                  [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
  ;; Assumes it will always be given values
  (precondition$ (and$ (not$ (op=? t1 t2))
                       (not$ (Type-Dyn-Huh t1))
                       (not$ (Type-Dyn-Huh t2)))
    (cond$
     ;; All the base cases are taken care of by (= t1 t2) in compile-med-cast
     ;; compile-med-cast should do a check to make sure (=/= t1 t2) and can be
     [(and$ (type-fn?$ t1)
            (type-fn?$ t2)
            (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
      (compile-fn-cast v t1 t2 l)]
     [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
            (op<=? (Type-Tuple-num t2) (Type-Tuple-num t1)))
      (compile-tuple-cast v t1 t2 l suspend-monotonic-heap-casts?)]
     [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
      (compile-pref-cast v (Type-GRef-Of t1) (Type-GRef-Of t2) l)]
     [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
      (compile-pvec-cast v (Type-GVect-Of t1) (Type-GVect-Of t2) l)]
     [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
      (compile-mbox-cast v (Type-MRef-Of t2) suspend-monotonic-heap-casts?)]
     [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
      (compile-mvec-cast v (Type-MVect-Of t2) suspend-monotonic-heap-casts?)]
     [(Type-Mu-Huh t1)
      (let$ ([t1-mu-body (Type-Mu-Body t1)])
        (cond$
         [(op=? t1-mu-body t2) v]
         [else (interp-med-cast v t1-mu-body t2 l suspend-monotonic-heap-casts?)]))]
     [(Type-Mu-Huh t2)
      (let$ ([t2-mu-body (Type-Mu-Body t2)])
        (cond$
         [(op=? t2-mu-body t1) v]
         [else (interp-med-cast v t1 t2-mu-body l suspend-monotonic-heap-casts?)]))]
     [else (Blame l)])))

(: make-interp-med-cast-runtime!
   (->* (#:fn-cast     Fn-Cast-Type
         #:tuple-cast  Tuple-Cast-Type
         #:pref-cast   Proxied-Cast-Type
         #:pvec-cast   Proxied-Cast-Type
         #:mbox-cast   Monotonic-Cast-Type
         #:mvec-cast   Monotonic-Cast-Type)
        Cast-Type))

(define (make-interp-med-cast-runtime!
         #:fn-cast    compile-fn-cast
         #:tuple-cast compile-tuple-cast
         #:pref-cast  compile-pref-cast
         #:pvec-cast  compile-pvec-cast
         #:mbox-cast  compile-mbox-cast
         #:mvec-cast  compile-mvec-cast)

  (define med-cast-uid (next-uid! "interp-med-cast"))

  (: interp-med-cast Cast-Type)
  (define (interp-med-cast v t1 t2 l suspend-monotonic-heap-casts?)
    (apply-code med-cast-uid v t1 t2 l suspend-monotonic-heap-casts?))

  (add-cast-runtime-binding!
   med-cast-uid
   (code$ (v t1 t2 l suspend-monotonic-heap-casts?)
     (code-gen-entire-med-cast
      v t1 t2 l
      #:fn-cast    compile-fn-cast
      #:tuple-cast compile-tuple-cast
      #:pref-cast  compile-pref-cast
      #:pvec-cast  compile-pvec-cast
      #:mbox-cast  compile-mbox-cast
      #:mvec-cast  compile-mvec-cast
      #:interp-med-cast interp-med-cast
      suspend-monotonic-heap-casts?)))
  
  interp-med-cast)


(define *mu-casts* : (Parameterof Nat) (make-parameter 0))
(define *mu-casts-limit* : (Parameterof Nat) (make-parameter 2))

(: make-compile-med-cast
   (->* (#:fn-cast     Fn-Cast-Type
         #:tuple-cast  Tuple-Cast-Type
         #:pref-cast   Proxied-Cast-Type
         #:pvec-cast   Proxied-Cast-Type
         #:mbox-cast   Monotonic-Cast-Type
         #:mvec-cast   Monotonic-Cast-Type
         #:interp-med-cast Cast-Type)
        Compile-Med-Cast-Type))

(define (make-compile-med-cast
         #:fn-cast    compile-fn-cast
         #:tuple-cast compile-tuple-cast
         #:pref-cast  compile-pref-cast
         #:pvec-cast  compile-pvec-cast
         #:mbox-cast  compile-mbox-cast
         #:mvec-cast  compile-mvec-cast
         #:interp-med-cast interp-med-cast)

  (define limit (*mu-casts-limit*))
  (: aux :
     CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (aux v t1 t2 l suspend-monotonic-heap-casts?)
    (match* (t1 t2)
      ;; TODO add tests that specifically target each of these cases
      [((Type t1-t) (Type t2-t))
       (match* (t1-t t2-t)
         ;; While this extra case may seem supperflous it allows us to
         ;; Check that (Unfold (Mu s2)) == (Mu s1) this isn't as simple
         ;; if we eliminate this case
         [((Mu s) _)
          (define mcs (*mu-casts*)) 
          (define interp? (< mcs limit))
          (define t3-t (grift-type-instantiate s t1-t))
          (parameterize ([*mu-casts* (+ 1 mcs)])
            (compile-med-cast v (Type t3-t) t2 l #:interpret? interp?))]
         [(_ (Mu s))
          (define mcs (*mu-casts*)) 
          (define interp? (< mcs limit))
          (define t3-t (grift-type-instantiate s t2-t))
          (parameterize ([*mu-casts* (+ 1 mcs)])
            (compile-med-cast v t1 (Type t3-t) l #:interpret? interp?))]         
         [((Fn a _ _) (Fn a _ _))
          (compile-fn-cast v t1 t2 l)]
         [((GRef t1) (GRef t2))
          (compile-pref-cast v (Type t1) (Type t2) l)]
         [((GVect t1) (GVect t2))
          (compile-pvec-cast v (Type t1) (Type t2) l)]
         [((MRef t1) (MRef t2))
          (compile-mbox-cast v #:t1 (Type t1) (Type t2))]
         [((MVect t1) (MVect t2))
          (compile-mvec-cast v #:t1 (Type t1) (Type t2))]
         [((STuple n _) (STuple m _)) #:when (<= m n)
          (compile-tuple-cast v t1 t2 l)]
         [(_ _) (Blame l)])]
      [((Type t1-t) t2) 
       (match t1-t
         [(Mu s)
          (define mcs (*mu-casts*)) 
          (define interp? (< mcs limit))
          (define t3-t (grift-type-instantiate s t1-t))
          (parameterize ([*mu-casts* (+ 1 mcs)])
            (compile-med-cast v (Type t3-t) t2 l #:interpret? interp?))]
         [_
          (let$ ([t2 (cond$
                      [(Type-Mu-Huh t2) (Type-Mu-Body t2)]
                      [else t2])])
            (match t1-t
              [(Fn a _ _)
               (If (and$ (Type-Fn-Huh t2) (op=? (Quote a) (Type-Fn-arity t2)))
                   (compile-fn-cast v t1 t2 l)
                   (Blame l))]
              [(GRef t1-t)
               (If (Type-GRef-Huh t2)
                   (compile-pref-cast v (Type t1-t) (Type-GRef-Of t2) l)
                   (Blame l))]
              [(GVect t1-t)
               (If (Type-GVect-Huh t2)
                   (compile-pvec-cast v (Type t1-t) (Type-GVect-Of t2) l)
                   (Blame l))]
              [(MRef t1-t)
               (If (Type-MRef-Huh t2)
                   (compile-mbox-cast v #:t1 (Type t1-t) (Type-MRef-Of t2))
                   (Blame l))]
              [(MVect t1-t)
               (If (Type-MVect-Huh t2)
                   (compile-mvec-cast v #:t1 (Type t1-t) (Type-MVect-Of t2))
                   (Blame l))]
              [(STuple n _)
               (If (and$ (Type-Tuple-Huh t2) (op<=? (Type-Tuple-num t2) (Quote n)))
                   (compile-tuple-cast v t1 t2 l)
                   (Blame l))]
              [_ (Blame l)]))])]
      [(t1 (Type t2-t))
       (match t2-t
         [(Mu s)
          (define mcs (*mu-casts*)) 
          (define interp? (< mcs limit))
          (define t3-t (grift-type-instantiate s t2-t))
          (parameterize ([*mu-casts* (+ 1 mcs)])
            (compile-med-cast
             v t1 (Type t3-t) l #:interpret? interp?))]
         [_
          (let$ ([t1 (cond$
                      [(Type-Mu-Huh t1) (Type-Mu-Body t1)]
                      [else t1])])
            (match t2-t
              [(Fn a _ _)
               (If (and$ (Type-Fn-Huh t1) (op=? (Quote a) (Type-Fn-arity t1)))
                   (compile-fn-cast v t1 t2 l)
                   (Blame l))]
              [(GRef t2-t)
               (If (Type-GRef-Huh t1)
                   (compile-pref-cast v (Type-GRef-Of t1) (Type t2-t) l)
                   (Blame l))]
              [(GVect t2-t)
               (If (Type-GVect-Huh t1)
                   (compile-pvec-cast v (Type-GVect-Of t1) (Type t2-t) l)
                   (Blame l))]
              [(MRef t2-t)
               (If (Type-MRef-Huh t1)
                   (compile-mbox-cast v (Type t2-t))
                   (Blame l))]
              [(MVect t2-t)
               (If (Type-MVect-Huh t1)
                   (compile-mvec-cast v (Type t2-t))
                   (Blame l))]
              [(STuple n _)
               (If (and$ (Type-Tuple-Huh t1) (op<=? (Quote n) (Type-Tuple-num t1)))
                   (compile-tuple-cast v t1 t2 l)
                   (Blame l))]
              [_ (Blame l)]))])]
      [(t1 t2)
       (cond
         ;; This is super hacky we can do better
         [(med-cast-inline-without-types?)
          (code-gen-entire-med-cast
           v t1 t2 l
           #:fn-cast    compile-fn-cast
           #:tuple-cast compile-tuple-cast
           #:pref-cast  compile-pref-cast
           #:pvec-cast  compile-pvec-cast
           #:mbox-cast  compile-mbox-cast
           #:mvec-cast  compile-mvec-cast
           #:interp-med-cast interp-med-cast
           suspend-monotonic-heap-casts?)]
         [else (interp-med-cast v t1 t2 l suspend-monotonic-heap-casts?)])]))
  (: compile-med-cast Compile-Med-Cast-Type)
  (define (compile-med-cast
           v t1 t2 l
           [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts]
           #:know-not-eq? [know-not-eq? : Boolean #f]
           #:interpret? [interpret? : Boolean #f])
    (bind-value$
     ([v v] [t1 t1] [t2 t2] [l l])
     (let ([cast
            (lambda ()
              (if interpret?
                  (interp-med-cast
                   v t1 t2 l suspend-monotonic-heap-casts?)
                  (aux v t1 t2 l suspend-monotonic-heap-casts?)))])
       (match* (t1 t2)
         ;; Check some invarients
         [((Type (Dyn)) _)
          (error 'interp-cast/code-gen-med-cast "t1 = Dyn, precondition false")]
         [(_ (Type (Dyn)))
          (error 'interp-cast/code-gen-med-cast "t2 = Dyn, precondition false")]
         ;; Determine if there is enough type information
         ;; or context to elide the eq check.
         ;; TODO
         ;; Now that equality isn't trivial we should check for type-equality
         ;; instead of syntactic equality 
         ;; [((Type t) (Type g)) #:when (type-eqv? t g) v]
         [(t t) v]
         [((Type _) (Type _)) (cast)]
         [(_ _) #:when know-not-eq? (cast)]
         [(_ _) (If (op=? t1 t2) v (cast))]))))
  compile-med-cast)

(define interp-cast-project/inject-inline? (make-parameter #f))
(define interp-cast-med-cast-inline? (make-parameter #f))
(: make-interp-cast-runtime!
   (->* (#:interp-cast-uid  Uid
         #:project          Project-Type
         #:inject           Inject-Type
         #:compile-med-cast Compile-Med-Cast-Type)
        Void))
(define (make-interp-cast-runtime!
         #:interp-cast-uid interp-cast-uid
         #:project compile-project
         #:inject  compile-inject
         #:compile-med-cast compile-med-cast)
  (add-cast-runtime-binding!
   interp-cast-uid
   (code$ (v t1 t2 l suspend-monotonic-heap-casts?)
     (cond$
      [(op=? t1 t2) v]
      [(Type-Dyn-Huh t1)
       (parameterize ([project-inline-without-types? (interp-cast-project/inject-inline?)])
         (compile-project v t2 l suspend-monotonic-heap-casts?))]
      [(Type-Dyn-Huh t2)
       (parameterize ([inject-inline-without-types? (interp-cast-project/inject-inline?)])
         (compile-inject v t1))]
      [else
       (parameterize ([med-cast-inline-without-types? (interp-cast-med-cast-inline?)])
         (compile-med-cast v t1 t2 l suspend-monotonic-heap-casts? #:know-not-eq? #t))]))))

(: make-compile-cast
   (->* (#:interp-cast  Cast-Type
         #:project      Project-Type
         #:inject       Inject-Type
         #:compile-med-cast Compile-Med-Cast-Type)
        Compile-Cast-Type))
(define (make-compile-cast
         #:interp-cast interp-cast
         #:project compile-project
         #:inject  compile-inject
         #:compile-med-cast compile-med-cast)
  ;; TODO this code seems to omit the eq check before
  ;; compile-med-cast (which does the eq check)
  ;; we could make this code slightly better by inlining
  ;; this eq check here?
  (: compile-cast Compile-Cast-Type)
  (define (compile-cast v t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts]
                        #:t1-not-dyn [t1-not-dyn : Boolean #f]
                        #:t2-not-dyn [t2-not-dyn : Boolean #f])
    (debug 'compile-cast v t1 t2)
    (match* (t1 t2)
      [(t t) v]
      [((Type (Dyn)) (Type _))
       ;; t2 not dyn because of (t t) case
       (compile-project v t2 l suspend-monotonic-heap-casts?)]
      [((Type (Dyn)) t2)  #:when t2-not-dyn
       (compile-project v t2 l suspend-monotonic-heap-casts?)]
      [((Type (Dyn)) t2) ;; t2 not Type (2 above)
       (bind-value$
        ([v v] [t2 t2] [l l])
        (If (Type-Dyn-Huh t2)
            v
            (compile-project v t2 l suspend-monotonic-heap-casts?)))]
      [((Type _) (Type (Dyn)))
       ;; t1 not dyn because of (t t) case
       (compile-inject v t1)]
      [(t2 (Type (Dyn))) #:when t1-not-dyn
       (compile-inject v t1)]
      [(t1 (Type (Dyn)))
       (bind-value$
        ([v v] [t1 t1] [l l])
        (If (Type-Dyn-Huh t1)
            v
            (compile-inject v t1)))]
      [((Type (or (Int) (Bool) (Character) (Float) (Unit)))
        (Type (or (Int) (Bool) (Character) (Float) (Unit))))
       ;; Non-Equal Base types
       ;; This shouldn't come up in static code generation? (I think ... -akuhlens)
       ;; (It would mean the program wasn't consistent 
       (Blame l)]
      [((Type _) (Type _))
       (compile-med-cast v t1 t2 l suspend-monotonic-heap-casts?)]
      [(t1 (Type _)) #:when t1-not-dyn
       (compile-med-cast v t1 t2 l suspend-monotonic-heap-casts?)]
      [((Type _) t2) #:when t2-not-dyn
       (compile-med-cast v t1 t2 l suspend-monotonic-heap-casts?)]
      [(t1 t2) (interp-cast v t1 t2 l suspend-monotonic-heap-casts?)]))

  ;; This dumb compilation strategy is simply used
  ;; for benchmarking purposes
  (: compile-cast/interp Compile-Cast-Type)
  (define (compile-cast/interp v t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts]
                               #:t1-not-dyn [t1-not-dyn : Boolean #f]
                               #:t2-not-dyn [t2-not-dyn : Boolean #f])
    (interp-cast v t1 t2 l suspend-monotonic-heap-casts?))
  (cond
    [(specialize-cast-code-generation?) compile-cast]
    [else compile-cast/interp]))

(: make-compile-project
   (->* (#:compile-med-cast Compile-Med-Cast-Type)
        Project-Type))
(define (make-compile-project #:compile-med-cast compile-med-cast)
  (: code-gen-full-project Project-Type)
  (define (code-gen-full-project v t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
    (: help Project-Type)
    (define (help v t2 l suspend-monotonic-heap-casts?)
      (precondition$
       (not$ (Type-Dyn-Huh t2))
       (let$
        ([u  (Dyn-Value v)]
         [t1 (Dyn-Type v)]) 
        (compile-med-cast u t1 t2 l suspend-monotonic-heap-casts?))))
    (bind-value$
     ([t2 t2])
     (help v t2 l suspend-monotonic-heap-casts?)))

  
  (define project-code
    (code$ (e t l suspend-monotonic-heap-casts?)
      (code-gen-full-project e t l suspend-monotonic-heap-casts?)))
  
  (define get-uid!
    (make-lazy-add-cast-runtime-binding! "project" project-code))

  (: interp-project Project-Type)
  (define (interp-project v t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
    (apply-code (get-uid!) v t2 l suspend-monotonic-heap-casts?))
  
  ;; Generates code that will project a dynamic value to a static type t2
  ;; or fail and blame l.
  ;; Notes:
  ;; T2 cannot be dynamic; It doesn't make sense to project to dyn
  ;; The second and third cases are specializations based on knowing what
  ;; representations the dynamic value will have have with specific types.
  ;; Failure in these cases is represented by calling the generic version of
  ;; projection (the fourth case) which could in theory be used to add conversion
  ;; casts. Currently this code generically projectes the value and casts
  ;; using the runtime cast interpreter. 
  ;; TODO let med-cast take care of the eq check
  (: compile-project Project-Type)
  (define (compile-project e t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
    (bind-value$
     ([v e] [t2 t2] [l l] [suspend-monotonic-heap-casts? suspend-monotonic-heap-casts?])
     (cast-profile/inc-projects-casts$
       (match t2
         [(Type (Dyn))
          (error 'grift/make-code-gen-project "Called with t2 = Dyn")]
         [(Type (or (Int) (Character) (Unit) (Bool)))
          (If (Dyn-Immediate-Tag=Huh v t2)
              (Dyn-Immediate-Value v)
              (interp-project v t2 l suspend-monotonic-heap-casts?))]
         [(Type _) 
          (If (Dyn-Immediate-Tag=Huh v t2)
              (let$
               ([u  (Dyn-Box-Value v)]
                [t1 (Dyn-Box-Type v)])
               (compile-med-cast
                u t1 t2 l suspend-monotonic-heap-casts?))
              (interp-project
               v t2 l suspend-monotonic-heap-casts?))]
         [_
          (if (project-inline-without-types?)
              (code-gen-full-project v t2 l suspend-monotonic-heap-casts?)
              (interp-project v t2 l suspend-monotonic-heap-casts?))]))))
  compile-project)

(: make-compile-inject : -> (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (make-compile-inject)
  (define inject-code
    (code$ (e t)
      (Dyn-Object e t)))
  (define get-uid!
    (make-lazy-add-cast-runtime-binding! "inject" inject-code))
  (: compile-inject Inject-Type)
  (define (compile-inject e t)
    (cast-profile/inc-injects-casts$
     (cond
       ;; Dyn-Object does specialization if the type is a type literal
       [(or (Type? t) (inject-inline-without-types?))
        (Dyn-Object e t)]
       [else (apply-code (get-uid!) e t)])))

  compile-inject)

(: make-compile-apply-fn-coercion
   (->* (#:get-fn-cast! (Nat -> Uid))
        Coerce-Proxied-Type))
(define (make-compile-apply-fn-coercion #:get-fn-cast! get-fn-cast!)
  
  (define dfc? (direct-fn-cast-optimization?))

  (: compile-apply-fn-coercion Coerce-Proxied-Type)
  (define (compile-apply-fn-coercion e m)
    (match m
      [(Coercion (Fn n _ _)) #:when dfc?
       (apply-code (get-fn-cast! n) e m)]
      [_
       ;; Let binding to provent expression duplication
       (let*$ ([v e] [m m])
         ;; TODO!!! consider
         ;; (App-Code (Fn-Caster v) (list v m))
         ;; Benifit: no double-memory indirect
         ;;          cast code can be specific
         ;;          two branches eliminated
         ;; Cost   : Proxies must have a caster slot
         ;; (App-Code (Global-Cast-Table-Ref (Fn-Coercion-arity m))
         ;;           (list v m))
         ;; Benifit: Functions no longer have cast code cell
         ;; Neg    : Another double memory indirect
         ;;        : Another branch on the otherside cast code
         (If (Fn-Proxy-Huh v)
             (App-Code (Fn-Caster (Fn-Proxy-Closure v)) (list v m))
             (App-Code (Fn-Caster v) (list v m))))]))
  compile-apply-fn-coercion)

(define (make-fn-coercion-error t1 t2 l)
  (error
   'interp-cast/make-compile-fn-cast
   "make-fn-coercion called without providing an implementation"))

(: make-compile-fn-cast/coercions
   (->* (#:get-fn-cast! (Nat -> Uid)
         #:make-fn-coercion Make-Coercion-Type) 
        Fn-Cast-Type))
(define (make-compile-fn-cast/coercions
         #:get-fn-cast! get-fn-cast! 
         #:make-fn-coercion make-fn-coercion)

  (define dfc? (direct-fn-cast-optimization?))
  (: compile-fn-cast Fn-Cast-Type)
  (define (compile-fn-cast e t1 t2 l)
    (bind-value$
     ([v e] [t1 t1] [t2 t2] [l l] [c (make-fn-coercion t1 t2 l)])
     (begin
       (define (codegen-dynamic-dispatch v)
         ;; TODO!!! consider
         ;; (App-Code (Fn-Caster v) (list v m))
         ;; Benifit: no double-memory indirect
         ;;          cast code can be specific
         ;;          two branches eliminated
         ;; Cost   : Proxies must have a caster slot
         ;; (App-Code (Global-Cast-Table-Ref (Fn-Coercion-arity m))
         ;;           (list v m))
         ;; Benifit: Functions no longer have cast code cell
         ;; Neg    : Another double memory indirect
         ;;        : Another branch on the otherside cast code
         (If (Fn-Proxy-Huh v)
             (App-Code (Fn-Caster (Fn-Proxy-Closure v)) (list v c))
             (App-Code (Fn-Caster v) (list v c))))
       (match c
         [(Quote-Coercion (Identity)) v]
         [(Quote-Coercion (Fn n _ _))
          #:when dfc?
          (apply-code (get-fn-cast! n) v c)]
         [(Quote-Coercion (Fn _ _ _))
          ;; No need for id check you would get in the next case
          (codegen-dynamic-dispatch v)]       
         [c ;; Non-coercion-literal
          (cond$
           [(Id-Coercion-Huh c) v]
           [else
            (match* (t1 t2)
              [((Type (Fn n _ _)) _)
               #:when dfc?
               (apply-code (get-fn-cast! n) v c)]
              [(_ (Type (Fn n _ _)))
               #:when dfc?
               (apply-code (get-fn-cast! n) v c)]
              [(_ _) (codegen-dynamic-dispatch v)])])]))))
  compile-fn-cast)

(: make-compile-fn-cast
   (->* (#:get-fn-cast! (Nat -> Uid))
        Fn-Cast-Type))
(define (make-compile-fn-cast #:get-fn-cast! get-fn-cast!)
  (define dfc? (direct-fn-cast-optimization?))
  (: compile-fn-cast Fn-Cast-Type)
  (define (compile-fn-cast v t1 t2 l)
    (match* (t1 t2)
      [((Type (Fn n _ _)) _) #:when dfc?
       (apply-code (get-fn-cast! n) v t1 t2 l)]
      [(_ (Type (Fn n _ _))) #:when dfc?
       (apply-code (get-fn-cast! n) v t1 t2 l)]
      [(_ _)
       (let$ ([v v])
         (App-Code (Fn-Caster v) (list v t1 t2 l)))]))
  compile-fn-cast)

;; TODO introduce assignments that are unboxed as long as they do not
;; escape. Implement tup-cast here instead of at specify representation
;; worrying about the overhead of allocation here is too much.
(: make-compile-apply-tuple-coercion
   (->* (#:apply-coercion-uid Uid) Apply-Coercion-Type))
(define ((make-compile-apply-tuple-coercion
          #:apply-coercion-uid apply-coercion-uid)
         [e : CoC3-Expr] [m : CoC3-Expr] [suspend-monotonic-heap-casts? : CoC3-Expr do-not-suspend-monotonic-heap-casts])
  (let$ ([v e][m m])
    (Coerce-Tuple apply-coercion-uid v m suspend-monotonic-heap-casts?)))

(: make-compile-cast-tuple/coercions
   (->* (#:apply-coercion-uid Uid
         #:make-med-coercion Make-Med-Coercion-Type)
        Cast-Tuple-Type))
(define (make-compile-cast-tuple/coercions
         #:apply-coercion-uid apply-coercion-uid
         #:make-med-coercion make-med-coercion)

  (define compile-apply-tuple-coercion
    (make-compile-apply-tuple-coercion
     #:apply-coercion-uid apply-coercion-uid))
  
  (: compile-cast-tuple Cast-Tuple-Type)
  (define (compile-cast-tuple e t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
    (let$ ([c (make-med-coercion t1 t2 l)])
      (precondition$ (and$ (tup-coercion?$ c)
                           (not$ (op=? t1 t2)))
        (compile-apply-tuple-coercion e c suspend-monotonic-heap-casts?))))

  compile-cast-tuple)


(: make-compile-cast-tuple 
   (->* (#:interp-cast-uid Uid) Cast-Tuple-Type))
(define (make-compile-cast-tuple #:interp-cast-uid cast-uid)

  (: compile-cast-tuple Cast-Tuple-Type)
  (define (compile-cast-tuple e t1 t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
    (cast-profile/inc-tuple-casts$
     (ann (let$ ([v e] [t1 t1] [t2 t2] [l l])
            (Cast-Tuple cast-uid v t1 t2 l suspend-monotonic-heap-casts?))
          CoC3-Expr)))

  compile-cast-tuple)

;; Note that the compile-cast-proxy-ref functions expects t1 and t2 to be
;; the sub-component of the GRef or GVect type so that the proxying
;; code can be the same for both.

(: compile-cast-proxied/type-based Proxied-Cast-Type)
(define (compile-cast-proxied/type-based e t1 t2 l)
  (Guarded-Proxy e (Twosome t1 t2 l)))

(: compile-cast-pref/type-based Proxied-Cast-Type)
(define (compile-cast-pref/type-based e t1 t2 l)
  (cast-profile/inc-ref-casts$ (compile-cast-proxied/type-based e t1 t2 l)))

(: compile-cast-pvec/type-based Proxied-Cast-Type)
(define (compile-cast-pvec/type-based e t1 t2 l)
  (cast-profile/inc-vector-casts$ (compile-cast-proxied/type-based e t1 t2 l)))

(: make-compile-cast-pref/coercions
   (->* (#:make-coercion Compile-Make-Coercion-Type
         #:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Proxied-Cast-Type))
(define ((make-compile-cast-pref/coercions
          #:make-coercion make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion?) e t1 t2 lbl)
  (cast-profile/inc-ref-casts$
   ((make-compile-cast-proxied/coercions
     #:make-coercion make-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? id-coercion?
     'GRef)
    e t1 t2 lbl)))

(: make-compile-cast-pvec/coercions
   (->* (#:make-coercion Compile-Make-Coercion-Type
         #:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Proxied-Cast-Type))
(define ((make-compile-cast-pvec/coercions
          #:make-coercion make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion?) e t1 t2 lbl)
  (cast-profile/inc-vector-casts$
   ((make-compile-cast-proxied/coercions
     #:make-coercion make-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? id-coercion?
     'GVect)
    e t1 t2 lbl)))

(: make-compile-cast-proxied/coercions
   (->* (#:make-coercion Compile-Make-Coercion-Type
         #:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type
         Proxied-Symbol)
        Proxied-Cast-Type))
(define (make-compile-cast-proxied/coercions
         #:make-coercion make-coercion
         #:compose-coercions compose-coercions
         #:id-coercion? id-coercion?
         #:apply-proxied-ref-coercion
         [apply-ref-coercion
          (make-compile-apply-proxied-coercion
           #:compose-coercions compose-coercions
           #:id-coercion? id-coercion?)]
         proxy-kind)
  
  (: code-gen-cast-proxied Proxied-Cast-Type)
  (define (code-gen-cast-proxied e t1 t2 l)
    (bind-value$
     ;; Let literals through while let binding expressions
     ([t1 t1] [t2 t2] [l l])
     (begin
       (define r (make-coercion t1 t2 l #:top-level? #f))
       (define w (make-coercion t2 t1 l #:top-level? #f))
       (apply-ref-coercion
        e
        (match* (r w)
          [((Quote-Coercion r) (Quote-Coercion w))
           (Quote-Coercion (Ref r w proxy-kind))]
          [(r w)
           (Ref-Coercion
            r w
            (case proxy-kind
              [(GRef)  COERCION-REF-REF-FLAG]
              [(GVect) COERCION-REF-VEC-FLAG]))])))))

  ;; This is only really abstracted because it should eventually
  ;; offer the option of close-coding this code generation.
  code-gen-cast-proxied)

(: make-compile-apply-pref-coercion
   (->* (#:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Coerce-Proxied-Type))
(define ((make-compile-apply-pref-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion-huh)
         e m)
  (cast-profile/inc-ref-casts$
   ((make-compile-apply-proxied-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? id-coercion-huh) e m)))

(: make-compile-apply-pvec-coercion
   (->* (#:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Coerce-Proxied-Type))
(define ((make-compile-apply-pvec-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion-huh)
         e m)
  (cast-profile/inc-vector-casts$
   ((make-compile-apply-proxied-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? id-coercion-huh) e m)))

(: make-compile-apply-proxied-coercion
   (->* (#:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Coerce-Proxied-Type))
(define (make-compile-apply-proxied-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion-huh)
  (define (aux e m m-read m-write)
    : CoC3-Expr
    (bind-value$
     ([v e])
     (cond$
      [(Guarded-Proxy-Huh v)
       (precondition$
        (Ref-Coercion-Huh (Guarded-Proxy-Coercion v))
        (let*$
         ([old-v (Guarded-Proxy-Ref v)]
          [old-m (Guarded-Proxy-Coercion v)]
          [o-write (Ref-Coercion-Write old-m)] 
          [r-write (compose-coercions m-write o-write)]
          [o-read (Ref-Coercion-Read old-m)]
          [r-read (compose-coercions o-read m-read)])
         (cond$
          [(and$ (id-coercion-huh r-read) (id-coercion-huh r-write))
           old-v]
          [else
           (Guarded-Proxy
            old-v
            (Coercion
             (Ref-Coercion
              r-read
              r-write
              (Ref-Coercion-Ref-Huh old-m))))])))]
      [else (Guarded-Proxy v (Coercion m))])))
  (lambda (e m)
    ;; This amounts to a manual value bind I am not sure it is
    ;; worth the code. 
    (match m
      [(Quote-Coercion (Ref readc writec _))
       (aux e m (Quote-Coercion readc) (Quote-Coercion writec))]
      [m
       (let$
        ([m m])
        (aux e m (Ref-Coercion-Read m) (Ref-Coercion-Write m)))])))

(define monotonic-cast-inline-without-types? : (Parameterof Boolean)
  (make-parameter #f))
(define monotonic-cast-close-code-specialization? : (Parameterof Boolean)
  (make-parameter #t))

(: make-compile-mbox-cast
   (->* (#:interp-cast Cast-Type
         #:greatest-lower-bound (Code-Label Uid)
         #:mref-state-reduction State-Reduction-Type)
        Monotonic-Cast-Type))
(define (make-compile-mbox-cast
         #:interp-cast interp-cast
         #:greatest-lower-bound greatest-lower-bound
         #:mref-state-reduction mref-state-reduction)

  (: code-gen-full-mbox-cast : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-full-mbox-cast e t2 l suspend-monotonic-heap-casts?)
    (let*$ ([address e][t2 t2])
      (cond$
       [(Type-Dyn-Huh t2) address]
       [else (code-gen-mbox-cast/no-dyn address t2 l suspend-monotonic-heap-casts?)])))

  (: code-gen-mbox-cast/no-dyn : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-mbox-cast/no-dyn address t2 l suspend-monotonic-heap-casts?)
    (begin$
      (If suspend-monotonic-heap-casts?
          (op$ mref-cast-queue-enqueue address t2)
          (let*$ ([t1 (Mbox-rtti-ref address)]                 
                  [t3 (app-code$ greatest-lower-bound t1 t2)])
            (when$ (not$ (op=? t1 t3))
                   (let*$ ([vi (Mbox-val-ref address)]
                           [cvi (interp-cast vi t1 t3 monotonic-blame suspend-monotonic-heap-casts)])
                     (Mbox-rtti-set! address t3)
                     (Mbox-val-set! address cvi)
                     (mref-state-reduction)))))
      address))

  (define interp-mbox-cast
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mbox-cast"
                 (code$ (e t2 l suspend-monotonic-heap-casts?)
                   (code-gen-full-mbox-cast e t2 l suspend-monotonic-heap-casts?)))])
      (lambda ([mref : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr]
               [suspend-monotonic-heap-casts? : CoC3-Expr])
        : CoC3-Expr
        (apply-code (uid!) mref t2 l suspend-monotonic-heap-casts?))))

  (define interp-mbox-cast/no-dyn
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mbox-cast/no-dyn"
                 (code$ (e t2 l suspend-monotonic-heap-casts?)
                   (code-gen-mbox-cast/no-dyn e t2 l suspend-monotonic-heap-casts?)))])
      (lambda ([mref : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr]
               [suspend-monotonic-heap-casts? : CoC3-Expr])
        : CoC3-Expr
        (apply-code (uid!) mref t2 l suspend-monotonic-heap-casts?))))

  (: compile-mbox-cast Monotonic-Cast-Type)
  (define  (compile-mbox-cast e t2 [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts] [l (Quote "Monotonic")]
                              ;; The t1 optional type allows compiling
                              ;; away some of the code if it is provided
                              ;; t1 is only used at compile time
                              #:t1 [t1 : (Option CoC3-Expr) #f])
    (cast-profile/inc-ref-casts$
     (match* (t1 t2)
       [(_ (Type (Dyn))) e]
       [((Type (not (Dyn))) _)
        (if (monotonic-cast-close-code-specialization?)
            (interp-mbox-cast/no-dyn e t2 l suspend-monotonic-heap-casts?)
            (let$ ([address e])
              (code-gen-mbox-cast/no-dyn address t2 l suspend-monotonic-heap-casts?)))]
       [(_ _)
        (if (monotonic-cast-inline-without-types?)
            (code-gen-full-mbox-cast e t2 l suspend-monotonic-heap-casts?)
            (interp-mbox-cast e t2 l suspend-monotonic-heap-casts?))])))
  compile-mbox-cast)

(: make-compile-mvec-cast
   (->* (#:interp-cast Cast-Type
         #:greatest-lower-bound (Code-Label Uid)
         #:mvect-state-reduction State-Reduction-Type)
        Monotonic-Cast-Type))
(define (make-compile-mvec-cast
         #:interp-cast interp-cast
         #:greatest-lower-bound greatest-lower-bound
         #:mvect-state-reduction mvect-state-reduction)
  (: code-gen-mvec-cast (->* (CoC3-Expr CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
  (define (code-gen-mvec-cast e t2 l [suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts])
    (let*$ ([address e] [t2 t2] [l l])
      (cond$
       [(type-dyn?$ t2) address]
       [else (code-gen-mvec-cast/no-dyn address t2 l suspend-monotonic-heap-casts?)])))

  (: code-gen-mvec-cast/no-dyn : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-mvec-cast/no-dyn address t2 l suspend-monotonic-heap-casts?)
    (begin$
      (If suspend-monotonic-heap-casts?
          (op$ mvect-cast-queue-enqueue address t2)
          (let*$ ([t1 (Mvector-rtti-ref address)]
                  [t3 (app-code$ greatest-lower-bound t1 t2)])
            (when$ (not$ (op=? t1 t3))
                   (let$ ([len (Mvector-length address)])
                     (repeat$ (i ZERO-EXPR len) ()
                       (let*$ ([vi (Mvector-val-ref address i 'no-check-bounds)]
                               [cvi (interp-cast vi t1 t3 monotonic-blame suspend-monotonic-heap-casts)]
                               [t4 (Mvector-rtti-ref address)])
                         (Mvector-val-set! address i cvi 'no-check-bounds)))
                     (Mvector-rtti-set! address t3)
                     (mvect-state-reduction)))))
      address))

  (define interp-mvec-cast
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mvec-cast"
                 (code$ (v t2 l suspend-monotonic-heap-casts?)
                   (code-gen-mvec-cast v t2 l suspend-monotonic-heap-casts?)))])
      (lambda ([v : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr] [suspend-monotonic-heap-casts? : CoC3-Expr])
        (apply-code (uid!) v t2 l suspend-monotonic-heap-casts?))))

  (define interp-mvec-cast/no-dyn
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mvec-cast/no-dyn"
                 (code$ (v t2 l suspend-monotonic-heap-casts?)
                   (code-gen-mvec-cast/no-dyn v t2 l suspend-monotonic-heap-casts?)))])
      (lambda ([v : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr] [suspend-monotonic-heap-casts? : CoC3-Expr])
        (apply-code (uid!) v t2 l suspend-monotonic-heap-casts?))))
  
  (: compile-mvec-cast Monotonic-Cast-Type)
  (define (compile-mvec-cast
           ;; This blame label is fabricated from nothing because
           ;; monotonic references are not completely implemented.
           [e : CoC3-Expr] [t2 : CoC3-Expr]
           [suspend-monotonic-heap-casts? : CoC3-Expr do-not-suspend-monotonic-heap-casts]
           [l : CoC3-Expr (Quote "Monotonic")]
           #:t1 [t1 : (Option CoC3-Expr) #f])
    : CoC3-Expr 
    (cast-profile/inc-vector-casts$
     (match* (t1 t2)
       [(_ (Type (Dyn))) e]
       [((Type (not (Dyn))) _)
        (if (monotonic-cast-close-code-specialization?)
            (interp-mvec-cast/no-dyn e t2 l suspend-monotonic-heap-casts?)
            (code-gen-mvec-cast/no-dyn e t2 l suspend-monotonic-heap-casts?))]
       [(_ _)
        (if (monotonic-cast-inline-without-types?)
            (code-gen-mvec-cast e t2 l suspend-monotonic-heap-casts?)
            (interp-mvec-cast e t2 l suspend-monotonic-heap-casts?))])))
  compile-mvec-cast)

(: make-compose-fn-coercions
   (->* (#:id-coercion? (CoC3-Expr -> CoC3-Expr)
         #:compose-coercions Compose-Coercions-Type)
        Compose-Coercions-Type))
;; TODO make this iterative
(define (make-compose-fn-coercions #:id-coercion? id-coercion?
                                   #:compose-coercions compose-coercions)
  (define compose-fn-coercions-uid (next-uid! "compose-fn-coercions"))
  (define (compose-fn-coercions [c1 : CoC3-Expr] [c2 : CoC3-Expr]
                                [i : CoC3-Expr ZERO-EXPR]
                                [a : (Option CoC3-Expr) #f]
                                [was-id : CoC3-Expr (Quote #t)])
    (cond
      [a (apply-code compose-fn-coercions-uid c1 c2 i a was-id)]
      [else (let*$ ([c1 c1]
                    [arity (fn-coercion-arity$ c1)]) 
              (apply-code compose-fn-coercions-uid c1 c2 i arity was-id))]))
  (add-cast-runtime-binding!
   compose-fn-coercions-uid
   (code$ (c1 c2 i a was-id)
     (cond$
      [(op=? i a)
       (let*$ ([r1 (Fn-Coercion-Return c1)]
               [r2 (Fn-Coercion-Return c2)]
               [cr (compose-coercions r1 r2)])
         (cond$
          [(and$ was-id (id-coercion? cr)) ID-EXPR]
          [else 
           (let$ ([fnc (Id-Fn-Coercion a)])
             (Fn-Coercion-Return-Set! fnc cr)
             fnc)]))]
      [else
       (let*$ ([i2 (Fn-Coercion-Arg c2 i)]
               [i1 (Fn-Coercion-Arg c1 i)]
               [ca (compose-coercions i2 i1)]
               [is-id (and$ was-id (id-coercion? ca))]
               [next-i (op$ + i (Quote 1))]
               [m  (compose-fn-coercions c1 c2 next-i a is-id)])
         (cond$
          [(id-coercion? m) m]
          [else
           (Fn-Coercion-Arg-Set! m i ca)
           m]))])))
  compose-fn-coercions)

(: make-compose-tup-coercions
   (->* (#:id-coercion? (CoC3-Expr -> CoC3-Expr)
         #:compose-coercions Compose-Coercions-Type)
        Compose-Coercions-Type))
;; TODO make this iterative 
(define (make-compose-tup-coercions #:id-coercion? id-coercion?
                                    #:compose-coercions compose-coercions)
  (define compose-tup-coercions-uid (next-uid! "compose-tuple-coercions"))
  (define (compose-tup-coercions [c1 : CoC3-Expr] [c2 : CoC3-Expr]
                                 [i : CoC3-Expr ZERO-EXPR]
                                 [a : (Option CoC3-Expr) #f]
                                 [was-id : CoC3-Expr (Quote #t)])
    (cond
      [a (apply-code compose-tup-coercions-uid c1 c2 i a was-id)]
      [else (let*$ ([c1 c1]
                    [arity (tup-coercion-arity$ c1)]) 
              (apply-code compose-tup-coercions-uid c1 c2 i arity was-id))]))
  (add-cast-runtime-binding!
   compose-tup-coercions-uid
   (code$ (c1 c2 i a was-id)
     (cond$
      [(Op '= `(,i ,a))
       (cond$
        [was-id ID-EXPR]
        [else (Id-Tuple-Coercion a)])]
      [else
       (let*$ ([e1 (Tuple-Coercion-Item c1 i)]
               [e2 (Tuple-Coercion-Item c2 i)]
               [ce (compose-coercions e1 e2)]
               [is-id (and$ was-id (id-coercion? ce))]
               [new-i (Op '+ `(,(Quote 1) ,i))]
               [m  (compose-tup-coercions c1 c2 new-i a is-id)])
         (cond$
          [(id-coercion? m) m]
          [else (Tuple-Coercion-Item-Set! m i ce)
                m]))])))
  compose-tup-coercions)
