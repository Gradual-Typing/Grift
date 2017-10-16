#lang typed/racket/base
#|
Pass: interpret casts with hyper-coercions
Used By: impose-cast-semantics (./impose-cast-semantics.rkt)
Cliff Notes: Implement cast samantics via a runtime data structure
currently know as hyper-coercions (minmally bushy version of 
coercions). We hope that these lead to significantly better performance
due to better locality, space consumption, and code organization.
|#
(provide interpret-casts/hyper-coercions)
(require "../language/cast-or-coerce3.rkt"
         "../language/cast0.rkt"
         "../language/syntax.rkt"
         "../configuration.rkt"
         "./interpret-casts-common.rkt"
         (submod "../logging.rkt" typed)
         racket/match
         racket/list
         (for-syntax syntax/parse typed/racket/base))

#|
Compile the cast in the program to calls to a cast interpreter 
that applies runtime cast object (hyper-coercions) to the
expression being casted. This cast interpreter is the runtime
function know as "apply_hyper_coercion", it relies on several
other runtime functions that are also generated and added
to the program via this code.

This code breaks the task down into several distinct subtasks
that can be located throughout this file:
1) Convert type-based casts to hyper-coercion plus call to
   runtime interpreter.
2) Generate code for the runtime that implements
3) Manually optimize operations on dynamic values to prevent
   obvious one-time use allocations.
|#
(: interpret-casts/hyper-coercions : -> (C0-Expr -> CoC3-Expr))
(define (interpret-casts/hyper-coercions)

  (define-values (compile-make-coercion compile-make-med-coercion)
    (make-compile-make-coercion/compile-make-med-coercion))

  (define greatest-lower-bound : (Code-Label Uid)
    (make-compile-types-greatest-lower-bound))
  
  (define compose-coercions
    (make-compose-coercions
     #:make-med-coercion compile-make-med-coercion
     #:greatest-lower-bound greatest-lower-bound))

  (define compile-inject (make-compile-inject))
  
  (define apply-coercion-uid
    (next-uid! "apply_hyper_coercion"))
  (define apply-med-coercion-uid
    (next-uid! "apply_mediating_coercion"))  
  (: apply-coercion : Apply-Coercion-Type)
  (define (apply-coercion e c [m (Quote 0)])
    (apply-code apply-coercion-uid e c m))
  (: apply-med-coercion : Apply-Coercion-Type)
  (define (apply-med-coercion e c [m (Quote 0)])
    (apply-code apply-med-coercion-uid e c m))

  (: get-fn-cast! : Nat -> Uid)
  (define get-fn-cast!
    (make-fn-cast-helpers
     (make-build-caster/coercions
      #:apply-coercion-uid apply-coercion-uid
      #:compose-coercions  compose-coercions
      #:id-coercion-huh    HC-Identity-Huh)))
  
  (: compile-lambda Lambda-Type)
  (define (compile-lambda f* expr)
    (let ([caster (get-fn-cast! (length f*))])
      (Lambda f* (Castable caster expr))))
  
  (: compile-app App-Type)
  (define (compile-app e e*)
    (App-Fn-or-Proxy apply-coercion-uid e e*))

  (define compile-apply-fn-coercion
    (make-compile-apply-fn-coercion
     #:get-fn-cast! get-fn-cast!))

  (define compile-fn-cast/coercions
    (make-compile-fn-cast/coercions
     #:get-fn-cast! get-fn-cast!
     #:make-fn-coercion compile-make-med-coercion))
  
  (define compile-apply-tup-coercion
    (make-compile-apply-tuple-coercion
     #:apply-coercion-uid apply-coercion-uid))

  (define compile-apply-ref-coercion
    (make-compile-apply-ref-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? HC-Identity-Huh))
  
  (define-values (interp-cast compile-cast)
    (cond
      [(hybrid-cast/coercion-runtime?)
       ;; interp-cast-refers to running code which uses type-based
       ;; reasoning instead of coercions to cast values. Higher-Order
       ;; casts are residuallized as coercions lazily
       (define interp-cast-uid (next-uid! "interp-cast"))
       
       (: interp-cast Cast-Type)
       (define (interp-cast v t1 t2 l [mt : CoC3-Expr ZERO-EXPR])
         (apply-code interp-cast-uid v t1 t2 l mt))
       
       ;; This first section builds the cast interpreter that falls
       ;; back to make-coercion when a higher-order cast is applied
       (define compile-tuple-cast/type-based
         (make-compile-cast-tuple #:interp-cast-uid interp-cast-uid))
         
       (define compile-pref-cast/coercions
         (make-compile-cast-pref/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? HC-Identity-Huh))
       
       (define compile-mbox-cast/type-based
         (make-compile-mbox-cast
          #:interp-cast interp-cast
          #:greatest-lower-bound greatest-lower-bound))
       
       (define compile-mvec-cast/type-based
         (make-compile-mvec-cast
          #:interp-cast interp-cast
          #:greatest-lower-bound greatest-lower-bound))
       
       (define interp-med-cast
         (make-interp-med-cast-runtime!
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/type-based
          #:ref-cast   compile-pref-cast/coercions
          #:mbox-cast  compile-mbox-cast/type-based
          #:mvec-cast  compile-mvec-cast/type-based))
         
       (define compile-med-cast
         (make-compile-med-cast
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/type-based
          #:ref-cast   compile-pref-cast/coercions
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

       (make-apply-med-coercion-runtime!
        #:apply-med-coercion-uid apply-med-coercion-uid 
        #:apply-fn-coercion compile-apply-fn-coercion
        #:apply-tup-coercion compile-apply-tup-coercion
        #:apply-ref-coercion compile-apply-ref-coercion
        #:mbox-cast compile-mbox-cast/type-based
        #:mvec-cast compile-mvec-cast/type-based)

       (make-apply-coercion-runtime!
        #:apply-coercion-uid apply-coercion-uid
        #:project compile-project/type-based
        #:inject  compile-inject
        #:apply-med-coercion apply-med-coercion)

       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast
          #:project compile-project/type-based
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast))

       (values interp-cast compile-cast)]
      [else
       (: interp-cast/coercions Cast-Type)
       (define (interp-cast/coercions e t1 t2 l [mt : CoC3-Expr ZERO-EXPR])
         (apply-coercion e (compile-make-coercion t1 t2 l) mt))
       (: interp-med-cast/coercions Cast-Type)
       (define (interp-med-cast/coercions
                e t1 t2 l
                [mt : CoC3-Expr ZERO-EXPR]
                #:know-not-eq? [know-not-eq? : Boolean #f])
         (apply-med-coercion e (compile-make-med-coercion t1 t2 l) mt))

       (define compile-tuple-cast/coercions
         (make-compile-cast-tuple/coercions
          #:apply-coercion-uid apply-coercion-uid
          #:make-med-coercion compile-make-med-coercion))
         
       (define compile-pref-cast/coercions
         (make-compile-cast-pref/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? HC-Identity-Huh))
       
       (define compile-mbox-cast/coercions
         (make-compile-mbox-cast
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound greatest-lower-bound))

       (define compile-mvec-cast/coercions
         (make-compile-mvec-cast
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound greatest-lower-bound))
       
       (make-apply-med-coercion-runtime!
        #:apply-med-coercion-uid apply-med-coercion-uid 
        #:apply-fn-coercion compile-apply-fn-coercion
        #:apply-tup-coercion compile-apply-tup-coercion
        #:apply-ref-coercion compile-apply-ref-coercion
        #:mbox-cast compile-mbox-cast/coercions
        #:mvec-cast compile-mvec-cast/coercions)

       (define compile-med-cast/coercions
         (make-compile-med-cast
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/coercions
          #:ref-cast   compile-pref-cast/coercions
          #:mbox-cast  compile-mbox-cast/coercions
          #:mvec-cast  compile-mvec-cast/coercions
          #:interp-med-cast interp-med-cast/coercions))

       (define compile-project/coercions
         (make-compile-project
          #:compile-med-cast compile-med-cast/coercions))
       
       (make-apply-coercion-runtime!
        #:apply-coercion-uid apply-coercion-uid
        #:project compile-project/coercions
        #:inject  compile-inject
        #:apply-med-coercion apply-med-coercion)

       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast/coercions
          #:project compile-project/coercions
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast/coercions))
       
       (values interp-cast/coercions compile-cast)]))
    
  (define-values (pbox-ref pbox-set! pvec-ref pvec-set! pvec-len)
    (make-proxied-reference/coercions-code-gen-helpers
     #:apply-coercion apply-coercion))
  
  (define-values (mbox-ref mbox-set! mvec-ref mvec-set!)
    (make-monotonic-helpers
     #:apply-coercion apply-coercion
     #:make-coercion  compile-make-coercion))
  
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
     #:compile-app compile-app
     #:pbox-ref pbox-ref #:pbox-set pbox-set!
     #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
     #:mbox-ref mbox-ref #:mbox-set mbox-set!
     #:mvec-ref mvec-ref #:mvec-set mvec-set!
     #:compile-cast compile-cast))
  
  (define-type Get-Cast-Code-Bindings-Type (-> CoC3-Bnd-Code*))
  
  ;; instantiate map-expr and return it so that the
  ;; pass driver uses it to run the pass
  (make-map-expr
   #:compile-cast compile-cast
   #:compile-lambda compile-lambda
   #:compile-app compile-app
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
   #:dyn-fn-app dyn-fn-app
   #:dyn-tup-prj dyn-tup-prj))

(: make-apply-coercion-runtime!
   (->* (#:apply-coercion-uid Uid
         #:project Project-Type
         #:inject  Inject-Type
         #:apply-med-coercion Apply-Coercion-Type)
        Void))
(define (make-apply-coercion-runtime!
         #:apply-coercion-uid apply-coercion-uid
         #:project project
         #:inject  inject
         #:apply-med-coercion apply-med-coercion)

  (add-cast-runtime-binding!
   apply-coercion-uid
   (code$ (v c mt)
     (let*$ ([v1 (cond$
                  [(HC-Project-Huh c)
                   (project v (HC-T1 c) (HC-Label c) mt)] 
                  [else v])]
             [m (HC-Med c)]
             [v2 (If (Id-Coercion-Huh m)
                     v1
                     (apply-med-coercion v1 m mt))])
       (If (HC-Inject-Huh c)
           (If (and$ (Id-Coercion-Huh m) (HC-Project-Huh c))
               v
               (inject v2 (HC-T2 c)))
           v2)))))

(: make-apply-med-coercion-runtime!
   (->* (#:apply-med-coercion-uid Uid
         #:apply-fn-coercion  Apply-Coercion-Type
         #:apply-tup-coercion Apply-Coercion-Type
         #:apply-ref-coercion Apply-Coercion-Type
         #:mbox-cast Monotonic-Cast-Type
         #:mvec-cast Monotonic-Cast-Type)
        Void))
(define (make-apply-med-coercion-runtime!
         #:apply-med-coercion-uid apply-med-coercion-uid
         #:apply-fn-coercion apply-fn-coercion
         #:apply-tup-coercion apply-tup-coercion
         #:apply-ref-coercion apply-ref-coercion
         #:mbox-cast mbox-cast
         #:mvec-cast mvec-cast)

  (add-cast-runtime-binding!
   apply-med-coercion-uid
   (code$ (v m mt)
     (precondition$
         (not$ (Id-Coercion-Huh m))
       (cond$
        ;; Todo Make mediating-coercion? precisely corespond
        ;; to the hyper-coercion notion of mediating-coercions?
        [(Mediating-Coercion-Huh m)
         (cond$
          [(Fn-Coercion-Huh m)    (apply-fn-coercion v m)]
          [(Tuple-Coercion-Huh m) (apply-tup-coercion v m mt)]
          [(Ref-Coercion-Huh m)   (apply-ref-coercion v m)]
          [(MRef-Coercion-Huh m)
           (mbox-cast v (MRef-Coercion-Type m))]
          [(MVect-Coercion-Huh m)
           (mvec-cast v (MVect-Coercion-Type m))]
          [else
           (Blame
            (Quote "Internal Error: hyper-coercions/apply-med-coercion 1"))])]
        ;; This isn't needed with the current implementation
        ;; but I want to include it because the name indicates that it should
        ;; work, and including it only slows down the failure-
        [(Id-Coercion-Huh m) v]
        [(Failed-Coercion-Huh m) (Blame (Failed-Coercion-Label m))] 
        [else
         (Blame
          (Quote "Internal Error: hyper-coercions/apply-med-coercion 2"))])))))

(: make-compile-make-coercion/compile-make-med-coercion :
   -> (Values Compile-Make-Coercion-Type Make-Med-Coercion-Type))
(define (make-compile-make-coercion/compile-make-med-coercion)
  ;; Cunstruct a hyper-coercions expression from the type and blame
  ;; information contained in a type-based cast.
  ;; This coercion literal is eligable to be hoisted in a later
  ;; pass so that it only ever is creates a value once. 
  (: make-coercion : Schml-Type Schml-Type Blame-Label -> CoC3-Expr)
  (define (make-coercion t1 t2 l)
    (: recur : Schml-Type Schml-Type -> Hyper-Coercion)
    (define (recur t1 t2)
      (match* (t1 t2)
        [(t        t) (HC #f t #f #f t IDENTITY)]
        ;; These two lines create an invarient that the only
        ;; time t1 or t2 is Dyn the entire hyper-coercion is an identity 
        [((Dyn)    t) (HC #t t l  #f t IDENTITY)]
        [(t    (Dyn)) (HC #f t #f #t t IDENTITY)]
        [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
         ;; The arity check here means that all coercions have
         ;; the correct arity under the static type system.
         ;; Notice that the argument types are reversed
         ;; because of contravarience of functions.
         (HC #f t1 #f #f t2 (Fn n1 (map recur a2* a1*) (recur r1 r2)))]
        [((STuple n1 t1*) (STuple n2 t2*)) #:when (= n1 n2)
         (HC #f t1 #f #f t2 (CTuple n1 (map recur t1* t2*)))]
        [((GRef t1) (GRef t2))
         (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
        [((GVect t1) (GVect t2))
         (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
        [((MRef _) (MRef t2))
         (HC #f t1 #f #f t2 (MonoRef t2))]
        [((MVect _) (MVect t2))
         (HC #f t1 #f #f t2 (MonoVect t2))]
        [(t1 t2)
         (HC #f t1 #f #f t2 (Failed l))]))
    (Quote-HCoercion (recur t1 t2)))
  (define interp-make-coercion-uid
    (next-uid! "make_hyper_coercion"))
  (define interp-make-med-coercion-uid
    (next-uid! "make_med_coercion"))
  (define interp-make-coercion : Make-Coercion-Type
    (apply-code-curry interp-make-coercion-uid))
  (define interp-make-med-coercion : Make-Coercion-Type
    (apply-code-curry interp-make-med-coercion-uid))

  (add-cast-runtime-binding!
   interp-make-coercion-uid
   (code$ (t1 t2 l)
     (cond$
      [(op=? t1 t2)
       (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t1 ID-EXPR) CoC3-Expr)]
      ;; This is absolutly necisarry
      ;; While Injections and Projections are never made by
      ;; source code coercions composition can create new
      ;; projections and injections.
      [(Type-Dyn-Huh t1)
       (ann (HC (Quote #t) t2 l (Quote #f) t2 ID-EXPR) CoC3-Expr)]
      [(Type-Dyn-Huh t2)
       (ann (HC (Quote #f) t1 (Quote #f) (Quote #t) t1 ID-EXPR) CoC3-Expr)]
      [else
       (let$ ([med (interp-make-med-coercion t1 t2 l)]) 
         (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t2 med) CoC3-Expr))])))
  (add-cast-runtime-binding!
   interp-make-med-coercion-uid
   (code$ (t1 t2 l)
     (precondition$ (not$ (and$ (op=? t1 t2) (Type-Dyn-Huh t1) (Type-Dyn-Huh t2)))
       (cond$
        [(and$ (Type-Fn-Huh t1) (Type-Fn-Huh t2) 
               (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
         ;; This line is a little tricky because unless we have actual
         ;; types for this at compile time we have to generate code that
         ;; can handle arbitry fn-arity.  We delegate this task to specify
         ;; representation because it involves safely allocating an object
         ;; whos size cannot be determined until run-time.
         (Make-Fn-Coercion interp-make-coercion-uid t1 t2 l)]
        [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
               (op<=? (type-tup-arity$ t2) (type-tup-arity$ t1)))
         (Make-Tuple-Coercion interp-make-coercion-uid t1 t2 l)]
        [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
         (let*$ ([gv1_of (Type-GVect-Of t1)]
                 [gv2_of (Type-GVect-Of t2)]
                 [write_crcn (interp-make-coercion gv2_of gv1_of l)]
                 [read_crcn  (interp-make-coercion gv1_of gv2_of l)])
           (Ref-Coercion read_crcn write_crcn))]
        [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
         (let*$ ([gr1_of (Type-GRef-Of t1)]
                 [gr2_of (Type-GRef-Of t2)]
                 [write_crcn (interp-make-coercion gr2_of gr1_of l)]
                 [read_crcn  (interp-make-coercion gr1_of gr2_of l)])
           (Ref-Coercion read_crcn write_crcn))]
        ;; TODO should these two line be (glb t1 t2)?
        [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
         (MRef-Coercion (type-mbox-of$ t2))]
        [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
         (MVect-Coercion (type-mvec-of$ t2))]
        [else (Failed-Coercion l)]))))

  (: compile-make-coercion Compile-Make-Coercion-Type)
  ;; TODO : can this be any better?
  (define (compile-make-coercion t1 t2 l
                                 #:top-level? [top-level? #f]
                                 #:know-not-eq? [know-not-eq? #f])
    (match* (t1 t2 l)
      [((Type t1) (Type t2) (Quote (? blame-label? l)))
       (make-coercion t1 t2 l)]
      [(t1 t2 l) (interp-make-coercion t1 t2 l)]))

  (: compile-make-med-coercion Make-Med-Coercion-Type)
  (define (compile-make-med-coercion t1 t2 l #:know-not-eq? [know-not-eq? #f])
    (match* (t1 t2 l)
      [((Type t1) (Type t2) (Quote (? blame-label? l)))
       (match (make-coercion t1 t2 l)
         [(Quote-HCoercion (HC _ _ _ _ _ m))
          (Quote-HCoercion m)]
         [other (error 'compile-make-coercion)])]
      [(t1 t2 l) #:when know-not-eq?
       (interp-make-med-coercion t1 t2 l)]
      [(t1 t2 l)
       (let$ ([t1 t1][t2 t2][l l])
         (If (op=? t1 t2) ID-EXPR (interp-make-med-coercion t1 t2 l)))]))

  (values compile-make-coercion compile-make-med-coercion))

(: make-compose-coercions
   (->* (#:make-med-coercion Make-Med-Coercion-Type
         #:greatest-lower-bound (Code-Label Uid))
        Compose-Coercions-Type))
(define (make-compose-coercions #:make-med-coercion make-med-coercion
                                #:greatest-lower-bound greatest-lower-bound)
  (define interp-compose-coercions-uid
    (next-uid! "compose_coercions"))
  (define interp-compose-med-coercions-uid
    (next-uid! "compose_med_coercions"))
  (define interp-compose-coercions : Compose-Coercions-Type
    (apply-code-curry interp-compose-coercions-uid))

  (define interp-compose-fn-coercions
    (make-compose-fn-coercions
     #:id-coercion? HC-Identity-Huh
     #:compose-coercions interp-compose-coercions))

  (define interp-compose-tuple-coercions
    (make-compose-tup-coercions
     #:id-coercion? HC-Identity-Huh
     #:compose-coercions interp-compose-coercions))

  (define interp-compose-med-coercions : Compose-Coercions-Type
    (apply-code-curry interp-compose-med-coercions-uid))

  (add-cast-runtime-binding!
   interp-compose-coercions-uid
   (code$ (fst snd)
     (precondition$
         ;; all sequences of coercions to compose are well typed because:
         ;; either the inner types are well-typed or the coercions are
         ;; some combination injections projections and dynamic
         ;; identities that are well typed
         (or$ (op=? (HC-T2 fst) (HC-T1 snd))
              (and$ (or$ (Type-Dyn-Huh (HC-T2 fst)) (HC-Inject-Huh fst))
                    (or$ (Type-Dyn-Huh (HC-T1 snd)) (HC-Project-Huh snd))))
       (let*$ ([fst-t2 (HC-T2 fst)]
               [snd-t1 (HC-T1 snd)])
         (cond$
          ;; These first two cases rule out dynamic identity casts
          ;; ie (HC #f Dyn #f #f Dyn ID)
          ;; These cannot be composed under the following rules because
          ;; the usual invarient of all code following is that Dyn
          ;; isn't present as t1 or t2 in any coercion.
          [(HC-Identity-Huh fst) snd]
          [(HC-Identity-Huh snd) fst]
          [else
           (let*$ ([fst-med (HC-Med fst)]
                   [mid-med
                    (cond$
                     [(and$ (HC-Inject-Huh fst)
                            (HC-Project-Huh snd) 
                            (not$ (op=? fst-t2 snd-t1)))
                      (let*$ ([mid (make-med-coercion
                                    fst-t2 snd-t1 (HC-Label snd)
                                    #:know-not-eq? #t)])
                        ;; we know these won't be Id
                        (interp-compose-med-coercions fst-med mid))]
                     [else fst-med])]
                   [snd-med (HC-Med snd)]
                   [fnl-med
                    ;; consider trying the id-checks here
                    #;(cond$
                       [(Id-Coercion-Huh fst-med) snd-med]
                       [(Id-Coercion-Huh snd-med) fst-med]
                       [else (interp-compose-med-coercions mid-med snd-med)])
                    (interp-compose-med-coercions mid-med snd-med)])
             (HC (HC-Project-Huh fst) (HC-T1 fst) (HC-Label fst) 
                 (HC-Inject-Huh snd) (HC-T2 snd)
                 fnl-med))])))))
  (add-cast-runtime-binding!
   interp-compose-med-coercions-uid
   (code$ (fst snd)
     (cond$
      ;; TODO consider specializing this code
      ;; by moving the Id-coercion-huh calls to
      ;; before each call to compose-med.
      [(Id-Coercion-Huh fst) snd]
      [(Id-Coercion-Huh snd) fst]
      [(and$ (Mediating-Coercion-Huh fst) (Mediating-Coercion-Huh snd))
       (cond$
        ;; Ditching the second check doesn't actually decrease the
        ;; number of checks once we move the failure casts into
        ;; mediating casts. 
        [(and$ (Fn-Coercion-Huh fst) (Fn-Coercion-Huh snd))
         (interp-compose-fn-coercions fst snd)]
        [(and$ (Tuple-Coercion-Huh fst) (Tuple-Coercion-Huh snd))
         (interp-compose-tuple-coercions fst snd)]
        [(and$ (Ref-Coercion-Huh fst) (Ref-Coercion-Huh snd))
         (let*$ ([fst-write (Ref-Coercion-Write fst)]
                 [snd-write (Ref-Coercion-Write snd)]
                 [com-write (interp-compose-coercions snd-write fst-write)]
                 [fst-read  (Ref-Coercion-Read fst)]
                 [snd-read  (Ref-Coercion-Read snd)]
                 [com-read  (interp-compose-coercions fst-read snd-read)])
           (If (and$ (HC-Identity-Huh com-read)
                     (HC-Identity-Huh com-write))
               (Quote-Coercion IDENTITY)
               (Ref-Coercion com-read com-write)))]
        [(and$ (MRef-Coercion-Huh fst) (MRef-Coercion-Huh snd))
         (let*$ ([fst_type  (MRef-Coercion-Type fst)]
                 [snd_type  (MRef-Coercion-Type snd)]
                 [glb       (app-code$ greatest-lower-bound fst_type snd_type)])
           (MRef-Coercion glb))]
        [(and$ (MVect-Coercion-Huh fst) (MVect-Coercion-Huh snd))
         (let*$ ([fst_type  (MVect-Coercion-Type fst)]
                 [snd_type  (MVect-Coercion-Type snd)]
                 [glb       (app-code$ greatest-lower-bound fst_type snd_type)])
           (MVect-Coercion glb))]
        [else
         (Blame (Quote "Internal Error: compose-mediating-coercions 1"))])]
      [(Failed-Coercion-Huh fst)
       (If (Failed-Coercion-Huh snd)
           fst ;; merge blame info for bidirectional behavior
           fst)]
      [(Failed-Coercion-Huh snd) snd]
      [else
       (Blame (Quote "Internal Error: compose-mediating-coercion 2"))])))  
  interp-compose-coercions)
