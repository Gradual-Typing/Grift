#lang typed/racket/base
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
 racket/format
 racket/list
 "../language/forms.rkt"
 "../language/syntax.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/cast0.rkt"
;; "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../unique-identifiers.rkt"
 "../language/cast-or-coerce3.rkt"
 (submod "../logging.rkt" typed))


(provide (all-defined-out))

(define-type Function-Proxy-Rep (U 'Data 'Hybrid 'Functional))

(: function-cast-representation (Parameterof Function-Proxy-Rep))
(define function-cast-representation
  (make-parameter 'Hybrid))


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

;; (: make-lazy-add-cast-runtime-binding-interp :
;;    String ((->* () #:rest CoC3-Code CoC3-Code) -> CoC3-Code) -> (->* () #:rest CoC3-Code CoC3-Code))
;; (define (make-lazy-add-cast-runtime-binding-interp name code)
;;   (define boxed-interp? : (Boxof (Option (->* () #:rest CoC3-Code CoC3-Code))) (box #f))
;;   (: apply-interp (->* () #:rest CoC3-Code CoC3-Code))
;;   (define (apply-interp . a)
;;     (define interp? (unbox boxed-interp?))
;;     (cond
;;       [interp? (apply interp? a)]
;;       [else
;;        (define uid (next-uid! name))
;;        (define interp (apply-code-curry uid))
;;        ((inst set-box! (Option (->* () #:rest CoC3-Code CoC3-Code))) boxed-interp? interp)
;;        (add-cast-runtime-binding! uid (code interp))
;;        (apply interp a)]))
;;   apply-interp)


(define-type Monotonic-Cast-Type
  (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr #:t1 CoC3-Expr) CoC3-Expr))
(define-type Fn-Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Tuple-Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Ref-Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Project-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Inject-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Apply-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Apply-Med-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr)
       (CoC3-Expr #:know-not-eq? Boolean) CoC3-Expr))

(define-type Id-Coercion-Huh-Type (CoC3-Expr -> CoC3-Expr))
(define-type Compile-Make-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr)
       (#:top-level? Boolean #:know-not-eq? Boolean)
       CoC3-Expr))
(define-type Make-Med-Coercion-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr) (#:know-not-eq? Boolean) CoC3-Expr))

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
  (CoC3-Expr CoC3-Expr* Schml-Type* CoC3-Expr -> CoC3-Expr))
(define-type Dyn-Tup-Prj-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MBox-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MBox-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MVec-Ref-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MVec-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

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

;; This next section of code are the procedures that build all
;; of the runtime code for hyper coercions based casting.

(define-type Compose-Coercions-Type
  (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Compile-Cast-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
       (CoC3-Expr #:t1-not-dyn Boolean #:t2-not-dyn Boolean)
       CoC3-Expr))
(define-type Cast-Type (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Cast-Tuple-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
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
         #:mbox         [mbox : (CoC3-Expr Schml-Type -> CoC3-Expr) Mbox]
         #:stc-mbox-ref [stc-mbox-ref : (CoC3-Expr -> CoC3-Expr) Mbox-val-ref]
         #:stc-mbox-set [stc-mbox-set! : (CoC3-Expr CoC3-Expr -> CoC3-Expr) Mbox-val-set!]
         #:mvec         [mvec : (CoC3-Expr CoC3-Expr Schml-Type -> CoC3-Expr) Mvector]
         #:stc-mvec-ref [stc-mvec-ref : (CoC3-Expr CoC3-Expr -> CoC3-Expr) Mvector-val-ref]
         #:stc-mvec-set [stc-mvec-set! : (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) Mvector-val-set!]
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
      [(Lambda f* (app recur exp)) (compile-lambda f* exp)]
      ;; Applications get turned into an application that "checks for the
      ;; the presence of proxies" This eventually gets optimized aways
      ;; into a functional proxy representation. 
      [(App (app recur e) (app recur* e*)) (compile-app e e*)]
      ;; Use make coercion to implement dynamic application without
      ;; allocating a one time use coercion.
      [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
       (dyn-fn-app e e* t* (Quote l))]
      ;; Transformation to lower guarded reference types
      ;; Guarded Operations on Guarded values are turned into
      ;; calls/inlinings of the runtime proceedures that perform
      ;; proxied reads and writes.
      ;;-------------------------------------------------------------------
      ;; Every Guarded Reference Starts As an Unguarded box
      [(Gbox (app recur e)) (Unguarded-Box e)]
      [(Gvector (app recur n) (app recur init)) (Unguarded-Vect n init)]
      ;; Unboxing calls off to the helpers we have defined
      [(Gvector-ref (app recur v) (app recur i))
       (pvec-ref v i)]
      [(Gunbox (app recur b))
       (pbox-ref b)]
      [(Dyn-GRef-Ref (app recur e) l)
       (dyn-pbox-ref e (Quote l))]
      [(Dyn-GVector-Ref (app recur e) (app recur i) l)
       (dyn-pvec-ref e i (Quote l))]
      ;; Setting a Gaurded reference results in iteratively applying
      ;; all the guarding casts to the value to be written.
      ;; Most of the work is already done but the code requires values
      ;; so there is a little repetative to let bind any values that
      ;; haven't been evaluated.
      [(Gbox-set! (app recur b) (app recur w))
       (pbox-set! b w)]
      [(Gvector-set! (app recur v) (app recur i) (app recur w))
       (pvec-set! v i w)]
      [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
       (dyn-pbox-set! e1 e2 (Type t) (Quote l))]
      [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (dyn-pvec-set! e1 i e2 (Type t) (Quote l))]
      ;; TODO add tests for dyn-gvector-len
      [(Dyn-GVector-Len (app recur e) (app recur l))
       (dyn-pvec-len e l)]
      [(Gvector-length (app recur e))
       (pvec-len e)]
      [(MBoxCastedRef (app recur e) t)
       (mbox-ref e (Type t))]
      [(MBoxCastedSet! (app recur e1) (app recur e2) t)
       (mbox-set! e1 e2 (Type t))]
      [(MVectCastedRef (app recur e) (app recur i) t)
       (mvec-ref e i (Type t))]
      [(MVectCastedSet! (app recur e1) (app recur i) (app recur e2) t)
       (mvec-set! e1 i e2 (Type t))]
      [(Dyn-MRef-Ref (app recur e) l)
       (dyn-mbox-ref e (Quote l))]
      [(Dyn-MRef-Set! (app recur e1) (app recur e2) t l)
       (dyn-mbox-set! e1 e2 (Type t) (Quote l))]
      [(Dyn-MVector-Ref (app recur e) (app recur i) l)
       (dyn-mvec-ref e i (Quote l))]
      [(Dyn-MVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (dyn-mvec-set! e1 i e2 (Type t) (Quote l))]
      ;; Completely statically typed monotonic need no runtime proceedures.
      ;; They are only kept different seperate from unguarded ops because
      ;; there layout is slightly different latter. 
      ;; Long-Term TODO: Why does the name ove these change?
      ;; Does their semantics change?
      [(Mvector-ref (app recur e1) (app recur e2))
       (stc-mvec-ref e1 e2)]
      [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
       (stc-mvec-set! e1 e2 e3)]
      [(Munbox (app recur e))
       (stc-mbox-ref e)]
      [(Mbox-set! (app recur e1) (app recur e2))
       (stc-mbox-set! e1 e2)]
      [(Mvector-length (app recur e)) 
       (mvec-len e)]
      [(Mbox (app recur e) t)
       (mbox e t)]
      [(Mvector (app recur e1) (app recur e2) t)
       (mvec e1 e2 t)]      
      ;; While tuples don't get any special attention in this pass
      ;; dynamic tuple projection needs to get dusugared
      [(Create-tuple e*) (Create-tuple (recur* e*))]
      [(Tuple-proj e i) (Tuple-proj (recur e) (Quote i))]
      [(Dyn-Tuple-Proj (app recur e) (app recur i) (app recur l))
       (dyn-tup-prj e i l)]
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
    (error 'interp-casts-with-hc/build-caster/sanity-check))
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
                    (Fn-Proxy (list #{arity :: Index} apply-coercion-uid)
                              raw-clos
                              (new-fn-crcn t1 t2 arity arg* ret)))))
            ;; Closure is unproxied --> just make a new proxy
            (Fn-Proxy (list #{arity :: Index} apply-coercion-uid) fun crcn)))))
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
    (let$ ([v e-gref])
      (If (Guarded-Proxy-Huh v)
          (let$ ([u (Guarded-Proxy-Ref v)]
                 [c (Guarded-Proxy-Coercion v)])
            (apply-coercion (Unguarded-Box-Ref u) (Ref-Coercion-Read c)))
          (Unguarded-Box-Ref v))))
  
  (: code-gen-pbox-set! PBox-Set-Type)
  (define (code-gen-pbox-set! e-gref w-val)
    (let$ ([v e-gref][w w-val])
      (If (Guarded-Proxy-Huh v)
          (let$ ([u (Guarded-Proxy-Ref v)]
                 [c (Ref-Coercion-Write (Guarded-Proxy-Coercion v))])
            (Unguarded-Box-Set! u (apply-coercion w c)))
          (Unguarded-Box-Set! v w))))

  (: code-gen-pvec-ref PVec-Ref-Type)
  (define (code-gen-pvec-ref e-gref i-index)
    (let$ ([v e-gref][i i-index])
      (If (Guarded-Proxy-Huh v)
          (let$ ([u (Unguarded-Vect-Ref (Guarded-Proxy-Ref v) i)]
                 [c (Ref-Coercion-Read (Guarded-Proxy-Coercion v))])
            (apply-coercion u c))
          (Unguarded-Vect-Ref v i))))

  (: code-gen-pvec-set! PVec-Set-Type)
  (define (code-gen-pvec-set! e-gref i-index w-val)
    (let$ ([v e-gref][i i-index][w w-val])
      (If (Guarded-Proxy-Huh v)
          (let$ ([u (Guarded-Proxy-Ref v)]
                 [w (apply-coercion w (Ref-Coercion-Write (Guarded-Proxy-Coercion v)))])
            (Unguarded-Vect-Set! u i w))
          (Unguarded-Vect-Set! v i w))))
  
  (: code-gen-pvec-len PVec-Len-Type)
  (define (code-gen-pvec-len e-gvec)
    (let$ ([v e-gvec])
      (If (Guarded-Proxy-Huh v)
          (Unguarded-Vect-length (Guarded-Proxy-Ref v))
          (Unguarded-Vect-length v))))

  (values code-gen-pbox-ref
          code-gen-pbox-set!
          code-gen-pvec-ref
          code-gen-pvec-set!
          code-gen-pvec-len))

(: make-proxied-reference/coercions-compile-helpers
   (->* (#:apply-coercion Apply-Coercion-Type)
        (Values PBox-Ref-Type PBox-Set-Type
                PVec-Ref-Type PVec-Set-Type PVec-Len-Type
                CoC3-Bnd-Code*)))
(define (make-proxied-reference/coercions-compile-helpers #:apply-coercion apply-coercion)
  (define-values (code-gen-pbox-ref code-gen-pbox-set!
                                    code-gen-pvec-ref code-gen-pvec-set! code-gen-pvec-len)
    (make-proxied-reference/coercions-code-gen-helpers
     #:apply-coercion apply-coercion))
  (cond
    [(inline-guarded-branch?)
     ;; we just hand back the code generators to build
     ;; inline code everywhere.
     (values code-gen-pbox-ref code-gen-pbox-set!
             code-gen-pvec-ref code-gen-pvec-set! code-gen-pvec-len
             '())]
    [else
     ;; If they are not inlined then the compiler we generate
     ;; the runtime binding and returns procedures that builds
     ;; invokations of this runtime code.
     (let ([pbr   (next-uid! "rt_pbox_ref")] 
           [pbs   (next-uid! "rt_pbox_set")]
           [pvr   (next-uid! "rt_pvec_ref")]
           [pvs   (next-uid! "rt_pvec_set")]
           [pvl   (next-uid! "rt_pvec_len")])
       (values
        (apply-code-curry pbr) (apply-code-curry pbs)
        (apply-code-curry pvr) (apply-code-curry pvs) (apply-code-curry pvl)
        `([,pbr . ,(code$ (pbox)
                     (code-gen-pbox-ref pbox))]
          [,pbs . ,(code$ (pbox value)
                     (code-gen-pbox-set! pbox value))]
          [,pvr . ,(code$ (pvec index)
                     (code-gen-pvec-ref pvec index))]
          [,pvs . ,(code$ (pvec index value)
                     (code-gen-pvec-set! pvec index value))]
          [,pvl . ,(code$ (pvec)
                     (code-gen-pvec-len pvec))])))]))

(: apply-coercion/make-coercion->compile-cast :
   Apply-Coercion-Type Make-Coercion-Type -> Cast-Type)
(define ((apply-coercion/make-coercion->compile-cast
          apply-coercion make-coercion)
         v t1 t2 l [mt : CoC3-Expr ZERO-EXPR])
  ;; TODO help compile coercion
  (apply-coercion v (make-coercion t1 t2 l) mt))


(: make-monotonic-helpers
   (->* ()
        (#:compile-cast   Compile-Cast-Type
         #:apply-coercion Apply-Coercion-Type
         #:make-coercion  Make-Coercion-Type) 
        (Values MBox-Ref-Type MBox-Set-Type
                MVec-Ref-Type MVec-Set-Type)))
(define (make-monotonic-helpers
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
     (lambda ([t1 : CoC3-Expr])
       : CoC3-Expr
       (let*$ ([mref mref]
               [val val] 
               [t2 (Mbox-rtti-ref mref)]
               [cv (cond$
                    [(and$ (type-tup?$ t1) (type-tup?$ t2))
                     (let*$ ([n (Type-Tuple-num t2)]
                             [ctv (Copy-Tuple n val)])
                       (begin$
                         (Mbox-val-set! mref ctv)
                         ctv))]
                    [else val])]
               [ccv (cast cv t1 t2 (Quote "Monotonic ref write error") mref)])
         ;; Unclear: I don't understand why there is a conditional
         ;; write here. Perhaps it is due to the side channel mref in
         ;; cast above.  I think the only use of this is in the
         ;; tuple-casting-in-place code.  Are we slowing down
         ;; non-tuple code to optimize tuple code?  If this is the
         ;; case then knowing the static type (t1) would allow us to
         ;; conservatively apply this optimization only when there is
         ;; a static guarentee that a tuple will be present without
         ;; slowing other code at all.
         ;; TODO: Investigate, Document
         (If (op=? t2 (Mbox-rtti-ref mref))
             (Mbox-val-set! mref ccv)
             (Quote '()))))))
  
  (: code-gen-mvec-ref MVec-Ref-Type)
  (define (code-gen-mvec-ref mvec i t2)
    (let*$ ([v mvec])
      (cast (Mvector-val-ref v i) (Mvector-rtti-ref mvec) t2 (Quote "Monotonic vect read error"))))
  
  (: code-gen-mvec-set! MVec-Set-Type)
  (define (code-gen-mvec-set! mvec i val t1)
    (bnd-t1-if-not-type
     t1
     (lambda ([t1 : CoC3-Expr])
       : CoC3-Expr
       (let*$ ([mvec mvec] [i i] [val val] 
               [t2 (Mvector-rtti-ref mvec)]
               [cvi (cond$
                     [(and$ (type-tup?$ t1) (type-tup?$ t2))
                      (let*$ ([n (Type-Tuple-num t2)]
                              [cvi (Copy-Tuple n val)])
                        (Mvector-val-set! mvec i cvi)
                        cvi)]
                     [else val])]
               [ccvi (cast cvi t1 t2 (Quote "Monotonic vect write error") mvec)])
         (If (op=? t2 (Mvector-rtti-ref mvec))
             (Mvector-val-set! mvec i ccvi)
             (Quote '()))))))
  
  (cond
    ;; TODO this shouldn't be the switch that decides if monotonic is inlined
    [(inline-guarded-branch?)
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
             (apply-code-curry mvec-ref-uid) (apply-code-curry mbox-set!-uid))]))

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
  
  #;
  (define cast : Cast-Type
    (cond
      [compile-cast compile-cast]
      [(and apply-coercion make-coercion)
       (apply-coercion/make-coercion->compile-cast apply-coercion
                                                   make-coercion)]
      [else (error 'make-dynamic-operations-helpers
                   "expected compile-cast/apply-coercion-make-coercion")]))
  
  (: code-gen-dyn-pbox-ref Dyn-PBox-Ref-Type)
  (define (code-gen-dyn-pbox-ref dyn lbl)
    (let*$ ([v dyn] [l lbl])
      (If (dyn-immediate-tag=?$ v PBOX-DYN-EXPR)
          (let*$ ([val (dyn-box-value$ v)]
                  [ty  (dyn-box-type$ v)])
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
      (If (ann (dyn-immediate-tag=?$ dyn-gbox PBOX-DYN-EXPR) CoC3-Expr)
          (let$ ([gbox (ann (dyn-box-value$ dyn-gbox) CoC3-Expr)]
                 [ty   (ann (dyn-box-type$ dyn-gbox) CoC3-Expr)])
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
        [(dyn-immediate-tag=?$ dyn PBOX-DYN-EXPR)
         (let$ ([maybe-pvec-val (dyn-box-value$ dyn)]
                [maybe-pvec-ty  (dyn-box-type$ dyn)])
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
       [(dyn-immediate-tag=?$ dyn-gvec PBOX-DYN-EXPR)
        (let$ ([maybe-vec      (dyn-box-value$ dyn-gvec)]
               [maybe-vec-type (dyn-box-type$  dyn-gvec)])
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
    (let*$ ([v expr] [l lbl])
      (cond$
       [(and$ (dyn-immediate-tag=?$ v PVEC-DYN-EXPR)
              (Type-GVect-Huh (dyn-box-type$ v)))
        (pvec-len (dyn-box-value$ v))]
       [else (Blame l)])))
  
  (: code-gen-dyn-mbox-ref Dyn-MBox-Ref-Type)
  (define (code-gen-dyn-mbox-ref dyn lbl)
    (let$ ([dyn dyn] [lbl lbl])
      (cond$
       [(and$ (dyn-immediate-tag=?$ dyn MBOX-DYN-EXPR)
              (Type-MRef-Huh (dyn-box-type$ dyn)))
        (mbox-ref (dyn-box-value$ dyn) DYN-EXPR)]
       [else (Blame lbl)])))
  
  (: code-gen-dyn-mbox-set! Dyn-MBox-Set-Type)
  (define (code-gen-dyn-mbox-set! dyn-mbox wrt-val1 t2 lbl)
    (let$ ([dyn dyn-mbox] [val wrt-val1] [t2 t2] [lbl lbl])
      (cond$
       [(dyn-immediate-tag=?$ dyn MBOX-DYN-EXPR)
        (let$ ([mbox (dyn-box-value$ dyn)]
               [t1 (dyn-box-type$ dyn)])
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
       [(and$ (dyn-immediate-tag=?$ dyn MVEC-DYN-EXPR)
              (Type-MVect-Huh (dyn-box-type$ dyn)))
        (mvec-ref (dyn-box-value$ dyn) ind DYN-EXPR)]
       [else (Blame lbl)])))
  
  (: code-gen-dyn-mvec-set! Dyn-MVec-Set-Type)
  (define (code-gen-dyn-mvec-set! dyn-mvec ind wrt-val1 t2 lbl) 
    (let$ ([dyn dyn-mvec] [ind ind] [vale wrt-val1] [t2 t2] [lbl lbl])
      (cond$
       [(dyn-immediate-tag=?$ dyn MVEC-DYN-EXPR)
        (let$ ([val (dyn-box-value$ dyn-mvec)]
               [ty  (dyn-box-type$ dyn-mvec)])
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
    ;; Variable are created here instead of using let$
    ;; because they appear in sub-expressions that
    ;; are programmatically generated.
    (define lu  (next-uid! "dyn-fn-blame-info"))
    (define l (Var lu))
    (define tyu (next-uid! "dyn_fn_ty"))
    (define ty  (Var tyu))
    (define vu (next-uid! "dyn-fn"))
    (define v  (Var vu))
    (define uu (next-uid! "dyn-fn-val"))
    (define u  (Var uu))
    (define-values (vu* v*)
      (for/lists ([vu* : Uid*] [v* : CoC3-Expr*]) ([e : CoC3-Expr e*])
        (define u (next-uid! "dyn_fn_arg"))
        (values u (Var u))))
    (unless (= (length v*) (length t*))
      (error 'interpret-casts-with-hyper-coercions/make-fn-app
             "expected types to be same length as arguments"))
    (define arg-casts : CoC3-Expr*
      (for/list : (Listof CoC3-Expr)
                ([v : CoC3-Expr v*]
                 [t : Schml-Type t*]
                 [i (in-naturals)])
        (let$ ([dyn-fn-arg-type (Type-Fn-arg ty (Quote i))])
          (compile-cast v (Type t) dyn-fn-arg-type l))))
    (define casts-apply : CoC3-Expr (compile-app u arg-casts))

    (define ret 
      (Let `([,lu . ,le]
             [,vu . ,e]
             . ,(map (inst cons Uid CoC3-Expr) vu* e*))
        (cond$
         [(dyn-immediate-tag=?$ v FN-DYN-DYN-EXPR)
          (Let `([,uu . ,(dyn-box-value$ v)]
                 [,tyu . ,(dyn-box-type$ v)])
            (cond$
             [(Type-Fn-Huh ty)
              (let$ ([ret-val casts-apply]
                     [ret-ty  (Type-Fn-return ty)])
                (compile-cast ret-val ret-ty DYN-EXPR l))]
             [else (Blame l)]))]
         [else (Blame l)])))
    (define who 'code-gen-dyn-fn-app)
    (debug off who e e* t* le ret))
  
  (: code-gen-dyn-tup-prj Dyn-Tup-Prj-Type)
  (define (code-gen-dyn-tup-prj e ie le)
    (let$ ([v e] [i ie] [l le])
      (cond$
       [(ann (dyn-immediate-tag=?$ v TUPLE-DYN-EXPR) CoC3-Expr)
        (let$ ([u  (ann (dyn-box-value$ v) CoC3-Expr)]
               [ty (ann (dyn-box-type$ v) CoC3-Expr)])
          (cond$
           [(ann (and$ (Type-Tuple-Huh ty) (Op '> (list (Type-Tuple-num ty) i)))
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
       (CoC3-Expr #:know-not-eq? Boolean)
       CoC3-Expr))

(: code-gen-entire-med-cast
   (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr
                   #:fn-cast     Fn-Cast-Type
                   #:tuple-cast  Tuple-Cast-Type
                   #:ref-cast    Ref-Cast-Type
                   #:mbox-cast   Monotonic-Cast-Type
                   #:mvec-cast   Monotonic-Cast-Type)
       CoC3-Expr))

;; This is needed in case we want to manually inline 
(define (code-gen-entire-med-cast v t1 t2 l mt 
                                  #:fn-cast    compile-fn-cast
                                  #:tuple-cast compile-tuple-cast
                                  #:ref-cast   compile-ref-cast
                                  #:mbox-cast  compile-mbox-cast
                                  #:mvec-cast  compile-mvec-cast)
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
      (compile-tuple-cast v t1 t2 l mt)]
     [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
      (compile-ref-cast v (Type-GRef-Of t1) (Type-GRef-Of t2) l)]
     [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
      (compile-ref-cast v (Type-GVect-Of t1) (Type-GVect-Of t2) l)]
     [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
      (compile-mbox-cast v (Type-MRef-Of t2))]
     [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
      (compile-mvec-cast v (Type-MVect-Of t2))] 
     [else (Blame l)])))


(: make-interp-med-cast-runtime!
   (->* (#:fn-cast     Fn-Cast-Type
         #:tuple-cast  Tuple-Cast-Type
         #:ref-cast    Ref-Cast-Type
         #:mbox-cast   Monotonic-Cast-Type
         #:mvec-cast   Monotonic-Cast-Type)
        Cast-Type))

(define (make-interp-med-cast-runtime!
         #:fn-cast    compile-fn-cast
         #:tuple-cast compile-tuple-cast
         #:ref-cast   compile-ref-cast
         #:mbox-cast  compile-mbox-cast
         #:mvec-cast  compile-mvec-cast)

  (define med-cast-uid (next-uid! "interp-med-cast"))

  (: interp-med-cast Cast-Type)
  (define (interp-med-cast v t1 t2 l [mt : CoC3-Expr ZERO-EXPR])
    (apply-code med-cast-uid v t1 t2 l mt))
  
  (add-cast-runtime-binding!
   med-cast-uid
   (code$ (v t1 t2 l mt)
     (code-gen-entire-med-cast
      v t1 t2 l mt
      #:fn-cast    compile-fn-cast
      #:tuple-cast compile-tuple-cast
      #:ref-cast   compile-ref-cast
      #:mbox-cast  compile-mbox-cast
      #:mvec-cast  compile-mvec-cast)))
  
  interp-med-cast)

(: make-compile-med-cast
   (->* (#:fn-cast     Fn-Cast-Type
         #:tuple-cast  Tuple-Cast-Type
         #:ref-cast    Ref-Cast-Type
         #:mbox-cast   Monotonic-Cast-Type
         #:mvec-cast   Monotonic-Cast-Type
         #:interp-med-cast Cast-Type)
        Compile-Med-Cast-Type))

(define ((make-compile-med-cast
          #:fn-cast    compile-fn-cast
          #:tuple-cast compile-tuple-cast
          #:ref-cast   compile-ref-cast
          #:mbox-cast  compile-mbox-cast
          #:mvec-cast  compile-mvec-cast
          #:interp-med-cast interp-med-cast)
         v t1 t2 l [mt : CoC3-Expr ZERO-EXPR]
         #:know-not-eq? [know-not-eq? : Boolean #f])
  
   (: aux : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (aux v t1 t2 l mt)
      (match* (t1 t2)
        ;; TODO add tests that specifically target each of these cases
        [((Type t1-t) (Type t2-t))
         (match* (t1-t t2-t)
           [((Fn a _ _) (Fn a _ _))
            (compile-fn-cast v t1 t2 l)]
           [((GRef t1) (GRef t2))
            (compile-ref-cast v (Type t1) (Type t2) l)]
           [((GVect t1) (GVect t2))
            (compile-ref-cast v (Type t1) (Type t2) l)]
           [((MRef t1) (MRef t2))
            (compile-mbox-cast v #:t1 (Type t1) (Type t2))]
           [((MVect t1) (MVect t2))
            (compile-mvec-cast v #:t1 (Type t1) (Type t2))]
           [((STuple n _) (STuple m _)) #:when (<= m n)
            (compile-tuple-cast v t1 t2 l mt)]
           [(_ _) #;base-types (Blame l)])]           
        [((Type t1-t) t2) 
         (match t1-t
           [(Fn a _ _)
            (If (and$ (Type-Fn-Huh t2) (op=? (Quote a) (Type-Fn-arity t2)))
                (compile-fn-cast v t1 t2 l)
                (Blame l))]
           [(GRef t1-t)
            (If (Type-GRef-Huh t2)
                (compile-ref-cast v (Type t1-t) (Type-GRef-Of t2) l)
                (Blame l))]
           [(GVect t1-t)
            (If (Type-GVect-Huh t2)
                (compile-ref-cast v (Type t1-t) (Type-GVect-Of t2) l)
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
                (compile-tuple-cast v t1 t2 l mt)
                (Blame l))]
           [_ #; Base-Cases (Blame l)])]
        [(t1 (Type t2-t))
         (match t2-t
           [(Fn a _ _)
            (If (and$ (Type-Fn-Huh t1) (op=? (Quote a) (Type-Fn-arity t1)))
                (compile-fn-cast v t1 t2 l)
                (Blame l))]
           [(GRef t2-t)
            (If (Type-GRef-Huh t1)
                (compile-ref-cast v (Type-GRef-Of t1) (Type t2-t) l)
                (Blame l))]
           [(GVect t2-t)
            (If (Type-GVect-Huh t1)
                (compile-ref-cast v (Type-GVect-Of t1) (Type t2-t) l)
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
                (compile-tuple-cast v t1 t2 l mt)
                (Blame l))]
           [_ #;base-cases (Blame l)])] 
        [(t1 t2)
         (cond
           ;; This is super hacky we can do better
           [(med-cast-inline-without-types?)
            (code-gen-entire-med-cast
             v t1 t2 l mt
             #:fn-cast    compile-fn-cast
             #:tuple-cast compile-tuple-cast
             #:ref-cast   compile-ref-cast
             #:mbox-cast  compile-mbox-cast
             #:mvec-cast  compile-mvec-cast)]
           [else (interp-med-cast v t1 t2 l mt)])]))
    (bind-value$
     ([v v] [t1 t1] [t2 t2] [l l] [mt mt])
     (match* (t1 t2)
      [((Type (Dyn)) _)
       (error 'interp-cast/code-gen-med-cast "t1 = Dyn, precondition false")]
      [(_ (Type (Dyn)))
       (error 'interp-cast/code-gen-med-cast "t2 = Dyn, precondition false")]
      [(t t) v]
      [((Type _) (Type _)) (aux v t1 t2 l mt)]
      [(_ _) #:when know-not-eq? (aux v t1 t2 l mt)]
      [(_ _) (If (op=? t1 t2) v (aux v t1 t2 l mt))])))


(define interp-cast-project/inject-inline? (make-parameter #f))
(define interp-cast-med-cast-inline? (make-parameter #f))
(: make-interp-cast-runtime!
   (->* (#:interp-cast-uid  Uid
         #:project      Project-Type
         #:inject       Inject-Type
         #:compile-med-cast Compile-Med-Cast-Type)
        Void))
(define (make-interp-cast-runtime!
         #:interp-cast-uid interp-cast-uid
         #:project compile-project
         #:inject  compile-inject
         #:compile-med-cast compile-med-cast)
  (add-cast-runtime-binding!
   interp-cast-uid
   (code$ (v t1 t2 l mt)
     (cond$
      [(op=? t1 t2) v]
      [(Type-Dyn-Huh t1)
       (parameterize ([project-inline-without-types? (interp-cast-project/inject-inline?)])
         (compile-project v t2 l mt))]
      [(Type-Dyn-Huh t2)
       (parameterize ([inject-inline-without-types? (interp-cast-project/inject-inline?)])
         (compile-inject v t1))]
      [else
       (parameterize ([med-cast-inline-without-types? (interp-cast-med-cast-inline?)])
         (compile-med-cast v t1 t2 l mt #:know-not-eq? #t))]))))

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
  (define (compile-cast v t1 t2 l [mt : CoC3-Expr ZERO-EXPR]
                        #:t1-not-dyn [t1-not-dyn : Boolean #f]
                        #:t2-not-dyn [t2-not-dyn : Boolean #f]) 
    (match* (t1 t2)
      [(t t) v]
      [((Type (Dyn)) (Type _))
       ;; t2 not dyn because of (t t) case
       (compile-project v t2 l mt)]
      [((Type (Dyn)) t2)  #:when t2-not-dyn
       (compile-project v t2 l mt)]
      [((Type (Dyn)) t2) ;; t2 not Type (2 above)
       (bind-value$
        ([v v] [t2 t2] [l l])
        (If (Type-Dyn-Huh t2)
            v
            (compile-project v t2 l mt)))]
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
       (compile-med-cast v t1 t2 l mt)]
      [(t1 (Type _)) #:when t1-not-dyn
       (compile-med-cast v t1 t2 l mt)]
      [((Type _) t2) #:when t2-not-dyn
       (compile-med-cast v t1 t2 l mt)]
      [(t1 t2) (interp-cast v t1 t2 l mt)]))

  ;; This dumb compilation strategy is simply used
  ;; for benchmarking purposes
  (: compile-cast/interp Compile-Cast-Type)
  (define (compile-cast/interp v t1 t2 l [mt : CoC3-Expr ZERO-EXPR]
                               #:t1-not-dyn [t1-not-dyn : Boolean #f]
                               #:t2-not-dyn [t2-not-dyn : Boolean #f])
    (interp-cast v t1 t2 l mt))
  (cond
    [(specialize-cast-code-generation?) compile-cast]
    [else compile-cast/interp]))


(: make-compile-project
   (->* (#:compile-med-cast Compile-Med-Cast-Type)
        Project-Type))
(define (make-compile-project #:compile-med-cast compile-med-cast)
  (: code-gen-full-project Project-Type)
  (define (code-gen-full-project v t2 l mt)
    (: help Project-Type)
    (define (help v t2 l mt)
      (precondition$ (not$ (Type-Dyn-Huh t2))
        (let*$ ([u  (dyn-value$ v)]
                [t1 (dyn-type$ v)]) 
          (compile-med-cast u t1 t2 l mt))))
    (match t2
      [(Type _) (help v t2 l mt)]
      [_ (let$ ([t2 t2]) (help v t2 l mt))]))
  
  (define project-code
    (code$ (e t l mt)
      (code-gen-full-project e t l mt)))
  
  (define get-uid!
    (make-lazy-add-cast-runtime-binding! "project" project-code))

  (: interp-project Project-Type)
  (define (interp-project v t2 l mt)
    (apply-code (get-uid!) v t2 l mt))
  
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
  (define (compile-project e t2 l mt)
    (let*$ ([v e] [l l] [mt mt])
      (match t2
        [(Type (Dyn))
         (error 'grift/make-code-gen-project "Called with t2 = Dyn")]
        [(Type (or (Float) (Int) (Character) (Unit) (Bool)))
         (If (dyn-immediate-tag=?$ v t2)
             (dyn-immediate-value$ v)
             (interp-project v t2 l mt))]
        [(Type _) 
         (If (dyn-immediate-tag=?$ v t2)
             (let*$ ([u  (dyn-box-value$ v)]
                     [t1 (dyn-box-type$ v)])
               (compile-med-cast u t1 t2 l mt))
             (interp-project v t2 l mt))]
        [_
         (if (project-inline-without-types?)
             (code-gen-full-project v t2 l mt)
             (interp-project v t2 l mt))])))
  compile-project)



(: make-compile-inject : -> (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (make-compile-inject)
  (define inject-code
    (code$ (e t)
      (dyn-make$ e t)))
  (define get-uid!
    (make-lazy-add-cast-runtime-binding! "inject" inject-code))
  (: compile-inject Inject-Type)
  (define (compile-inject e t)
    (cond
      ;; make-dyn$ does specialization if the type is a type literal
      [(or (Type? t) (inject-inline-without-types?))
       (dyn-make$ e t)]
      [else (apply-code (get-uid!) e t)]))

  compile-inject)

(: make-compile-apply-fn-coercion
   (->* (#:get-fn-cast! (Nat -> Uid))
        Apply-Coercion-Type))
(define (make-compile-apply-fn-coercion #:get-fn-cast! get-fn-cast!)
  
  (define dfc? (direct-fn-cast-optimization?))

  (: compile-apply-fn-coercion Apply-Coercion-Type)
  (define (compile-apply-fn-coercion e m [mt ZERO-EXPR])
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
  (define (compile-fn-cast v t1 t2 l)
    (define c (make-fn-coercion t1 t2 l))
    (match* (t1 t2)
      [((Type (Fn n _ _)) _) #:when dfc?
       (apply-code (get-fn-cast! n) v c)]
      [(_ (Type (Fn n _ _))) #:when dfc?
       (apply-code (get-fn-cast! n) v c)]
      [(_ _)
       (let$ ([v v] [c c])
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
             (App-Code (Fn-Caster v) (list v c))))]))
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
(define ((make-compile-apply-tuple-coercion #:apply-coercion-uid apply-coercion-uid)
         [e : CoC3-Expr] [m : CoC3-Expr] [mt : CoC3-Expr ZERO-EXPR])
  (match mt
    [(Quote 0) (Coerce-Tuple apply-coercion-uid e m)]
    [_
     (let$ ([v e][m m][mt mt])
       (If (Op '= (list (Quote 0) mt))
           (Coerce-Tuple apply-coercion-uid v m)
           (Coerce-Tuple-In-Place apply-coercion-uid v m mt)))]))


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
  (define (compile-cast-tuple e t1 t2 l mt)
    (let$ ([c (make-med-coercion t1 t2 l)])
      (precondition$ (and$ (tup-coercion?$ c)
                           (not$ (op=? t1 t2)))
        (compile-apply-tuple-coercion e c mt))))

  compile-cast-tuple)


(: make-compile-cast-tuple 
   (->* (#:interp-cast-uid Uid) Cast-Tuple-Type))
(define (make-compile-cast-tuple #:interp-cast-uid cast-uid)
  (: compile-cast-tuple Cast-Tuple-Type)
  (define (compile-cast-tuple e t1 t2 l mt)
    (match mt
      ;; Todo make specializing on the arity, and sub types
      [(Quote 0)
       (ann (Cast-Tuple cast-uid e t1 t2 l) CoC3-Expr)]
      [_ 
       (ann (let$ ([v e] [t1 t1] [t2 t2] [l l] [mt mt])
              (If (op=? mt ZERO-EXPR)
                  (Cast-Tuple cast-uid v t1 t2 l)
                  (Cast-Tuple-In-Place cast-uid v t1 t2 l mt)))
            CoC3-Expr)]))
  compile-cast-tuple)



;; Note that the compile-cast-proxy-ref functions expects t1 and t2 to be
;; the sub-component of the GRef or GVect type so that the proxying
;; code can be the same for both.

(: compile-cast-pref/type-based Ref-Cast-Type)
(define (compile-cast-pref/type-based e t1 t2 l)
  (Guarded-Proxy e (Twosome t1 t2 l)))

(: make-compile-cast-pref/coercions
   (->* (#:make-coercion Compile-Make-Coercion-Type
         #:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Ref-Cast-Type))
(define (make-compile-cast-pref/coercions
          #:make-coercion make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion?)

  (: code-gen-cast-pref Ref-Cast-Type)
  (define (code-gen-cast-pref e t1 t2 l)
    (debug off t1 t2)
    (define ret
    (bind-value$
     ;; Let literals through while let binding expressions
   ([v e] [t1 t1] [t2 t2] [l l])
   ;; There is a small amount of specialization here because
   ;; we know precisely which case of inter-compose-med will
   ;; match...
   ;; TODO add an option to hoist this to a runtime procedure
   ;; TODO simplify this code
   (begin
     (define (aux [m : CoC3-Expr] [m-read : CoC3-Expr] [m-write : CoC3-Expr])
       : CoC3-Expr
       (cond$
        [(Guarded-Proxy-Huh v)
         (precondition$ (Ref-Coercion-Huh (Guarded-Proxy-Coercion v))
           (let*$ ([old-v (Guarded-Proxy-Ref v)]
                    [old-m (Guarded-Proxy-Coercion v)]
                    [o-write (Ref-Coercion-Write old-m)] 
                    [r-write (compose-coercions m-write o-write)]
                    [o-read (Ref-Coercion-Read old-m)]
                    [r-read (compose-coercions o-read m-read)])
             (cond$
              [(and$ (id-coercion? r-read) (id-coercion? r-write))
                old-v]
              [else
               (Guarded-Proxy old-v (Coercion (Ref-Coercion r-read r-write)))])))]
        [else (Guarded-Proxy v (Coercion m))]))
     (define c1 (make-coercion t1 t2 l #:top-level? #f))
     (define c2 (make-coercion t2 t1 l #:top-level? #f))
     (debug off c1 c2)
     (match* (c1 c2)
       [((and m-read (Quote-Coercion readc))
         (and m-write (Quote-Coercion writec)))
        ;; This is the optimal case because m should be statically allocated
        (aux (Quote-Coercion (Ref readc writec)) m-read m-write)]
       [(readc writec)
        (let$ ([m-read  readc]
               [m-write writec])
          (aux (Ref-Coercion m-read m-write) m-read m-write))]))))
    (debug off t1 t2 ret)
    ret)
  ;; TODO add switch to allow this to be close coded
  (cond
    [#t code-gen-cast-pref]
    [else
     (define uid (next-uid! "cast-proxied-reference/coercion"))
     (add-cast-runtime-binding!
      uid
      (code$ (v t1 t2 l mt) (code-gen-cast-pref v t1 t2 l mt)))
     (: build-call Ref-Cast-Type)
     (define (build-call v t1 t2 mt)
       (apply-code uid v t1 t2 mt))
     build-call]))

(: make-compile-apply-ref-coercion
   (->* (#:compose-coercions Compose-Coercions-Type
         #:id-coercion? Id-Coercion-Huh-Type)
        Apply-Coercion-Type))
(define ((make-compile-apply-ref-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? id-coercion-huh) e m [mt (Quote 0)])
  ;; The mt is ignored here
  (let*$ ([v e] [m m])
    ;; There is a small amount of specialization here because
    ;; we know precisely which case of inter-compose-med will
    ;; match...
    (cond$
     [(Guarded-Proxy-Huh v)
      (precondition$
          (Ref-Coercion-Huh (Guarded-Proxy-Coercion v))
        (let*$ ([old-v  (Guarded-Proxy-Ref v)]
                [old-m  (Guarded-Proxy-Coercion v)]
                [o-write (Ref-Coercion-Write old-m)]
                [m-write (Ref-Coercion-Write m)]
                [r-write (compose-coercions m-write o-write)]
                [o-read  (Ref-Coercion-Read old-m)]
                [m-read  (Ref-Coercion-Read m)]
                [r-read  (compose-coercions o-read m-read)])
          (cond$
           [(and$ (id-coercion-huh r-read) (id-coercion-huh r-write))
            old-v]
           [else
            (Guarded-Proxy old-v (Coercion (Ref-Coercion r-read r-write)))])))]
     [else (Guarded-Proxy v (Coercion m))])))



(define monotonic-cast-inline-without-types? : (Parameterof Boolean)
  (make-parameter #f))
(define monotonic-cast-close-code-specialization? : (Parameterof Boolean)
  (make-parameter #t))

(: make-compile-mbox-cast
   (->* (#:interp-cast Cast-Type
         #:greatest-lower-bound (Code-Label Uid))
        Monotonic-Cast-Type))
(define (make-compile-mbox-cast
         #:interp-cast interp-cast
         #:greatest-lower-bound greatest-lower-bound)

  (: copy-mbox-value-if-tuple : CoC3-Expr -> CoC3-Expr)
  (define (copy-mbox-value-if-tuple mref)
    (let*$ ([mref mref] [t (Mbox-rtti-ref mref)] [v (Mbox-val-ref mref)])
      (cond$
       [(type-tup?$ t)
        (let$ ([cv (Copy-Tuple (Type-Tuple-num t) v)])
          (Mbox-val-set! mref cv)
          cv)] 
       [else v])))

  (: code-gen-full-mbox-cast : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-full-mbox-cast e t2 l)
    (let*$ ([v e][t2 t2])
      (cond$
       [(Type-Dyn-Huh t2) v]
       [else
        (let*$ ([t1 (Mbox-rtti-ref v)]
                [t3 (app-code$ greatest-lower-bound t1 t2)])
          (cond$
           [(op=? t1 t3) v]
           [else
            (Mbox-rtti-set! v t3)
            (let*$ ([v-copy (copy-mbox-value-if-tuple v)]
                    [new-v (interp-cast v-copy t1 t3 l v)]
                    [t4 (Mbox-rtti-ref v)])
              (cond$
               [(op=? t3 t4) (Mbox-val-set! v new-v) v]
               [else v]))]))])))

  (: code-gen-mbox-cast/tuple : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-mbox-cast/tuple e t2 n l)
    (let*$ ([v e]
            [t1 (Mbox-rtti-ref v)]
            [t3 (app-code$ greatest-lower-bound t1 t2)])
      (cond$
       [(op=? t1 t3) v]
       [else
        (Mbox-rtti-set! v t3)
        (let*$ ([v-copy (Copy-Tuple n (Mbox-val-ref v))]
                [_      (Mbox-val-set! v v-copy)]
                ;; TODO we know that t1 is a tuple type and
                ;; t2 is a tuple type this should build a cast tuple-in-place node
                [new-v (interp-cast v-copy t1 t3 l v)]
                [t4 (Mbox-rtti-ref v)])
          (cond$
           [(op=? t3 t4) (Mbox-val-set! v new-v) v]
           [else v]))])))
  
  (: code-gen-mbox-cast/non-tuple : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-mbox-cast/non-tuple e t2 l)
    (let*$ ([v e]
            [t1 (Mbox-rtti-ref v)]
            [t3 (app-code$ greatest-lower-bound t1 t2)])
      (cond$
       [(op=? t1 t3) v]
       [else
        (Mbox-rtti-set! v t3)
        (let*$ ([val   (Mbox-val-ref v)]
                [new-v (interp-cast val t1 t3 l v)]
                [t4 (Mbox-rtti-ref v)])
          (cond$
           [(op=? t3 t4) (Mbox-val-set! v new-v) v]
           [else v]))])))

  (define interp-mbox-cast
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mbox-cast"
                 (code$ (e t2 l) (code-gen-full-mbox-cast e t2 l)))])
      (lambda ([mref : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr])
        : CoC3-Expr
        (apply-code (uid!) mref t2 l))))
  
  (define interp-mbox-cast/tuple
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mbox-cast/tuple"
                 (code$ (e t2 n l) (code-gen-mbox-cast/tuple e t2 n l)))])
      (lambda ([mref : CoC3-Expr] [t2 : CoC3-Expr] [n : CoC3-Expr] [l : CoC3-Expr])
        : CoC3-Expr
        (apply-code (uid!) mref t2 n l))))

  (define interp-mbox-cast/non-tuple
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mbox-cast/non-tuple"
                 (code$ (e t2 l) (code-gen-mbox-cast/non-tuple e t2 l)))])
      (lambda ([mref : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr])
        : CoC3-Expr
        (apply-code (uid!) mref t2 l))))

  (: compile-mbox-cast Monotonic-Cast-Type)
  (define  (compile-mbox-cast e t2 [l (Quote "Monotonic")]
                              ;; The t1 optional type allows compiling
                              ;; away some of the code if it is provided
                              ;; t1 is only used at compile time
                              #:t1 [t1 : (Option CoC3-Expr) #f])
    (match* (t1 t2)
      [(_ (Type (Dyn))) e]
      [((Type (STuple a _)) _)
       (define n (Quote a))
       (if (monotonic-cast-close-code-specialization?)
           (interp-mbox-cast/tuple e t2 n l)
           (code-gen-mbox-cast/tuple e t2 n l))]
      [((Type (not (Dyn))) _)
       (if (monotonic-cast-close-code-specialization?)
           (interp-mbox-cast/non-tuple e t2 l)
           (code-gen-mbox-cast/non-tuple e t2 l))]
      [(_ _)
       (if (monotonic-cast-inline-without-types?)
           (code-gen-full-mbox-cast e t2 l)
           (interp-mbox-cast e t2 l))]))
  compile-mbox-cast)


(: make-compile-mvec-cast
   (->* (#:interp-cast Cast-Type
         #:greatest-lower-bound (Code-Label Uid))
        Monotonic-Cast-Type))
(define (make-compile-mvec-cast
         #:interp-cast interp-cast
         #:greatest-lower-bound greatest-lower-bound)
  (: code-gen-full-mvec-cast : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-full-mvec-cast e t2 l)
    (let*$ ([v e] [t2 t2] [l l])
      (cond$
       [(type-dyn?$ t2) v]
       [else
        (let*$ ([t1 (Mvector-rtti-ref v)]
                [t3 (app-code$ greatest-lower-bound t1 t2)])
          (cond$
           [(op=? t1 t3) v]
           [else
            (Mvector-rtti-set! v t3)
            (let$ ([len (Mvector-length v)])
              (cond$
               [(Type-Tuple-Huh t3)
                (let*$ ([n (Type-Tuple-num t3)])
                  (repeat$ (i (Quote 0) len) ()
                    (let*$ ([vi (Mvector-val-ref v i)]
                            [cvi (Copy-Tuple n vi)])
                      (Mvector-val-set! v i cvi)
                      (let*$ ([ccvi (interp-cast cvi t1 t3 l v)]
                              [t4 (Mvector-rtti-ref v)])
                        (If (op=? t3 t4)
                            (Mvector-val-set! v i ccvi)
                            (Break-Repeat))))))]
               [else
                (repeat$ (i (Quote 0) len) ()
                  (let*$ ([vi (Mvector-val-ref v i)]
                          [cvi (interp-cast vi t1 t3 l v)]
                          [t4 (Mvector-rtti-ref v)])
                    (If (op=? t3 t4)
                        (Mvector-val-set! v i cvi)
                        (Break-Repeat))))]))
            v]))])))
  (: code-gen-mvec-cast/tuple : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-mvec-cast/tuple e t2 n l)
    (let*$ ([v e] [t2 t2] [n n] [l l]
            [t1 (Mvector-rtti-ref v)]
            [t3 (app-code$ greatest-lower-bound t1 t2)])
      (cond$
       [(op=? t1 t3) v]
       [else
        (Mvector-rtti-set! v t3)
        (let$ ([len (Mvector-length v)])
          (repeat$ (i (Quote 0) len) ()
            (let*$ ([vi (Mvector-val-ref v i)]
                    [cvi (Copy-Tuple n vi)])
              (Mvector-val-set! v i cvi)
              (let*$ ([ccvi (interp-cast cvi t1 t3 l v)]
                      [t4 (Mvector-rtti-ref v)])
                (If (op=? t3 t4)
                    (Mvector-val-set! v i ccvi)
                    (Break-Repeat))))))
        v])))
  (: code-gen-mvec-cast/non-tuple : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define (code-gen-mvec-cast/non-tuple e t2 l)
    (let*$ ([v e]
            [t1 (Mvector-rtti-ref v)]
            [t3 (app-code$ greatest-lower-bound t1 t2)])
      (cond$
       [(op=? t1 t3) v]
       [else
        (Mvector-rtti-set! v t3)
        (let$ ([len (Mvector-length v)])
          (repeat$ (i (Quote 0) len) ()
            (let*$ ([vi (Mvector-val-ref v i)]
                    [cvi (interp-cast vi t1 t3 l v)]
                    [t4 (Mvector-rtti-ref v)])
              (If (op=? t3 t4)
                  (Mvector-val-set! v i cvi)
                  (Break-Repeat)))))
        v])))
  
  (define interp-mvec-cast
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mvec-cast"
                 (code$ (v t2 l)
                   (code-gen-full-mvec-cast v t2 l)))])
      (lambda ([v : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr])
        (apply-code (uid!) v t2 l))))

  (define interp-mvec-cast/tuple
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mvec-cast/tuple"
                 (code$ (v t2 n l)
                   (code-gen-mvec-cast/tuple v t2 n l)))])
      (lambda ([v : CoC3-Expr] [t2 : CoC3-Expr] [n : CoC3-Expr] [l : CoC3-Expr])
        (apply-code (uid!) v t2 n l))))

  (define interp-mvec-cast/non-tuple
    (let ([uid! (make-lazy-add-cast-runtime-binding!
                 "mvec-cast/non-tuple"
                 (code$ (v t2 l)
                   (code-gen-mvec-cast/non-tuple v t2 l)))])
      (lambda ([v : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr])
        (apply-code (uid!) v t2 l))))
  
  (: compile-mvec-cast Monotonic-Cast-Type)
  (define (compile-mvec-cast
           ;; This blame label is fabricated from nothing because
           ;; monotonic references are not completely implemented.
           [e : CoC3-Expr] [t2 : CoC3-Expr]
           [l : CoC3-Expr (Quote "Monotonic")]
           #:t1 [t1 : (Option CoC3-Expr) #f])
    : CoC3-Expr 
    (match* (t1 t2)
      [(_ (Type (Dyn))) e]
      [((Type (STuple a _)) _)
       (define n (Quote a))
       (if (monotonic-cast-close-code-specialization?)
           (interp-mvec-cast/tuple e t2 n l)
           (code-gen-mvec-cast/tuple e t2 n l))]
      [((Type (not (Dyn))) _)
       (if (monotonic-cast-close-code-specialization?)
           (interp-mvec-cast/non-tuple e t2 l)
           (code-gen-mvec-cast/non-tuple e t2 l))]
      [(_ _)
       (if (monotonic-cast-inline-without-types?)
           (code-gen-full-mvec-cast e t2 l)
           (interp-mvec-cast e t2 l))]))
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
          [else (Tuple-Coercion-Item-Set! m i ce)]))])))
  compose-tup-coercions)
