#lang typed/racket/base
#|
Pass: interpret casts with hyper-coercions
Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                            
Used By: impose-cast-semantics (./impose-cast-semantics.rkt)
Cliff Notes: Implement cast samantics via a runtime data structure
currently know as hyper-coercions (minmally bushy version of 
coercions). We hope that these lead to significantly better performance
due to better locality, space consumption, and code organization.

TODO: abstract all common code between coercions and hyper-coercions
TODO: come up with iterative code that implements tuple and function cast
      in the abstracted common code
TODO: implement optimizer.
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

  (define greatest-lower-bound : Greatest-Lower-Bound-Type
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
         
         (values interp-cast apply-coercion compile-cast
                 ;;compile-cast/type-based ;; I think this will generate better code 
                 #;compile-cast/coercions
                 compile-lambda compile-app)

       (values interp-cast compile-cast)]
      [else
       (: interp-cast/coercions Cast-Type)
       (define (interp-cast/coercions e t1 t2 l [mt : CoC3-Expr ZERO-EXPR])
         (apply-coercion e (compile-make-coercion t1 t2 l) mt))
       (: interp-med-cast/coercions Cast-Type)
       (define (interp-med-cast/coercions e t1 t2 l
                                [mt : CoC3-Expr ZERO-EXPR]
                                #:know-not-eq? [know-not-eq? : Boolean #f])
         (apply-med-coercion e (compile-make-med-coercion t1 t2 l) mt))

       (define compile-tuple-cast/coercions
         (make-compile-cast-tuple/coercions
          ;; This seems suspect
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
    
  (: pbox-ref  PBox-Ref-Type)
  (: pbox-set! PBox-Set-Type)
  (: pvec-ref  PVec-Ref-Type)
  (: pvec-set! PVec-Set-Type)
  (: pvec-len  PVec-Len-Type)
  #;(: proxied-operations-bindings CoC3-Bnd-Code*)
  (define-values (pbox-ref pbox-set!
                           pvec-ref pvec-set! pvec-len
                           #;proxied-operations-bindings)
    (make-proxied-reference/coercions-code-gen-helpers
     #:apply-coercion apply-coercion)
    #;(make-proxied-reference/coercions-compile-helpers
     #:apply-coercion apply-coercion))
  
  (: mbox-set! MBox-Set-Type)
  (: mbox-ref MBox-Ref-Type)
  (: mvec-ref MVec-Ref-Type)
  (: mvec-set! MVec-Set-Type)
  #;(: monotonic-operations-bindings CoC3-Bnd-Code*)
  (define-values (mbox-ref mbox-set! mvec-ref mvec-set! #;monotonic-operations-bindings)
    (make-monotonic-helpers
     #:apply-coercion apply-coercion
     #:make-coercion  compile-make-coercion)
    #;(make-monotonic-reference/coercions-compile-helpers
       #:apply-coercion apply-coercion
       #:make-coercion  compile-make-coercion))
  
  (: dyn-pbox-ref Dyn-PBox-Ref-Type)
  (: dyn-pbox-set! Dyn-PBox-Set-Type)
  (: dyn-pvec-ref Dyn-PVec-Ref-Type)
  (: dyn-pvec-set! Dyn-PVec-Set-Type)
  (: dyn-pvec-len Dyn-PVec-Len-Type)
  (: dyn-mbox-ref Dyn-MBox-Ref-Type)
  (: dyn-mbox-set! Dyn-MBox-Set-Type)
  (: dyn-mvec-ref Dyn-MVec-Ref-Type)
  (: dyn-mvec-set! Dyn-MVec-Set-Type)
  (: dyn-fn-app Dyn-Fn-App-Type)
  (: dyn-tup-prj Dyn-Tup-Prj-Type)
  #;(: dynamic-operations-bindings CoC3-Bnd-Code*)
  (define-values (dyn-pbox-ref dyn-pbox-set!
                               dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
                               dyn-mbox-ref dyn-mbox-set!
                               dyn-mvec-ref dyn-mvec-set!
                               dyn-fn-app dyn-tup-prj
                               #;dynamic-operations-bindings)
    (make-dynamic-operations-helpers
     #:compile-app compile-app
     #:pbox-ref pbox-ref #:pbox-set pbox-set!
     #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
     #:mbox-ref mbox-ref #:mbox-set mbox-set!
     #:mvec-ref mvec-ref #:mvec-set mvec-set!
     #:compile-cast compile-cast)
    #;(make-dynamic-operations/coercions-compile-helpers
     #:cast compile-cast #:compile-app compile-app
     #:pbox-ref pbox-ref #:pbox-set pbox-set!
     #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
     #:mbox-ref mbox-ref #:mbox-set mbox-set!
     #:mvec-ref mvec-ref #:mvec-set mvec-set!))
  
  (define-type Get-Cast-Code-Bindings-Type (-> CoC3-Bnd-Code*))
  
  #;(: hack! : CoC3-Bnd-Code* -> Void)
  #;(define (hack! b*)
    (for ([b b*]) (add-cast-runtime-binding! (car b) (cdr b))))

  #;(: get-cast-code-bindings! Get-Cast-Code-Bindings-Type)
  #;(define (get-cast-code-bindings!)
    (append
     #;(get-fn-cast-bindings!)
     #;proxied-operations-bindings
     #;monotonic-operations-bindings
     #;dynamic-operations-bindings
     `(#;[,interp-projection-uid . ,interp-projection-code]
       #;[,apply-coercion-uid . ,apply-coercion-code]
       #;[,apply-med-coercion-uid . ,apply-med-coercion-code]
       #;[,interp-make-coercion-uid . ,interp-make-coercion-code]
       #;[,interp-make-med-coercion-uid . ,interp-make-med-coercion-code]
       #;[,interp-compose-coercions-uid . ,interp-compose-coercions-code]
       #;[,interp-compose-med-coercions-uid
          . ,interp-compose-med-coercions-code]
       #;[,interp-compose-fn-coercions-uid
        . ,interp-compose-fn-coercions-code]
       #;[,interp-compose-tuple-coercions-uid
        . ,interp-compose-tuple-coercions-code]
       #;[,copy-mref-uid . ,copy-mref-code])))

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
         #:greatest-lower-bound Greatest-Lower-Bound-Type)
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
                 [glb       (greatest-lower-bound fst_type snd_type)])
           (MRef-Coercion glb))]
        [(and$ (MVect-Coercion-Huh fst) (MVect-Coercion-Huh snd))
         (let*$ ([fst_type  (MVect-Coercion-Type fst)]
                 [snd_type  (MVect-Coercion-Type snd)]
                 [glb       (greatest-lower-bound fst_type snd_type)])
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
  #;(add-cast-runtime-binding!
   interp-compose-fn-coercions-uid
       (code$ (c1 c2 i a was-id)
      (cond$
       [(Op '= `(,i ,a))
        (let*$ ([r1 (Fn-Coercion-Return c1)]
                [r2 (Fn-Coercion-Return c2)]
                [cr (interp-compose-coercions r1 r2)])
          (cond$
           [(and$ was-id (HC-Identity-Huh cr)) (Quote-Coercion (Identity))]
           [else 
            (let$ ([fnc (Id-Fn-Coercion a)])
              (Fn-Coercion-Return-Set! fnc cr)
              fnc)]))]
       [else
        (let*$ ([i2 (Fn-Coercion-Arg c2 i)]
                [i1 (Fn-Coercion-Arg c1 i)]
                [ca (interp-compose-coercions i2 i1)]
                [is-id (and$ was-id (HC-Identity-Huh ca))]
                [next-i (Op '+ `(,i ,(Quote 1)))]
                [m  (interp-compose-fn-coercions c1 c2 next-i a is-id)])
          (cond$
           [(Id-Coercion-Huh m) m]
           [else (Fn-Coercion-Arg-Set! m i ca) m]))])))
  #;(add-cast-runtime-binding!
     interp-compose-tuple-coercions-uid
     (code$ (c1 c2 i a was-id)
      (cond$
       [(Op '= `(,i ,a))
        (cond$
         [was-id (Quote-Coercion (Identity))]
         [else (Id-Tuple-Coercion a)])]
       [else
        (let*$ ([e1 (Tuple-Coercion-Item c1 i)]
                [e2 (Tuple-Coercion-Item c2 i)]
                [ce (interp-compose-coercions e1 e2)]
                [is-id (and$ was-id (HC-Identity-Huh ce))]
                [new-i (Op '+ `(,(Quote 1) ,i))]
                [m  (interp-compose-tuple-coercions c1 c2 new-i a is-id)])
          (cond$
           [(Id-Coercion-Huh m) m]
           [else (Tuple-Coercion-Item-Set! m i ce)]))])))
  
  interp-compose-coercions)


  



;; build-caster! generates a piece of code that performs a fn-cast
;; for a specific arity. 

;; This could be done as 3 seperate casters one that handles both
;; casted and uncasted closures and another each seperately.
;; The general caster is used in static code
;; The uncasted is attatched to the original closure
;; The casted is attatched to the proxy
#;(define ((make-build-caster/coercions
          #:apply-coercion-uid [apply-coercion-uid : Uid]
          #:compose-coercions  [compose-coercions : Compose-Coercions-Type]
          #:id-coercion-huh    [id-coercion-huh : Id-Coercion-Huh-Type])
         [arity : Nat])
  : CoC3-Code
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
    (error 'apply-coercions-with-hc/build-caster/sanity-check))
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
  
  (unless (index? arity)
    (error 'lower-function-cast/build-fn-cast-with-coercion
           "arity grew too large to be a valid index"))
  ;; Body of build-fn-caster stitch together the AST of the
  ;; code that forms a fn-cast for a particular arity. 
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
        (Fn-Proxy (list #{arity :: Index} apply-coercion-uid) fun crcn))))




#;(: make-fn-cast-helpers : (Nat -> CoC3-Code) -> (Values (Nat -> Uid) (-> CoC3-Bnd-Code*)))
#;(define (make-fn-cast-helpers build-caster!)
  ;; A map from arity to the binding of the code that handles that arity
  ;; at runtime.
  ;; This 
  (define fn-cast-binding-map : (HashTable Nat CoC3-Bnd-Code) (make-hasheq))
    ;; Get the uid of fn-cast that can handle
    (: get-fn-cast! : Nat -> Uid)
    (define (get-fn-cast! arity)
      (let ([bnd? (hash-ref fn-cast-binding-map arity #f)])
        (cond
          [bnd? (car bnd?)]
          [else
           (define name (string-append "fn_cast_" (number->string arity)))
           (define caster-uid  (next-uid! name))
           (define caster-code (build-caster! arity))
           (define caster-bnd  (cons caster-uid caster-code))
           (hash-set! fn-cast-binding-map arity caster-bnd)
           caster-uid])))
    (: get-fn-cast-bindings! : -> CoC3-Bnd-Code*)
    (define (get-fn-cast-bindings!)
      (hash-values fn-cast-binding-map))
    (values get-fn-cast! get-fn-cast-bindings!))

#;(: make-proxied-reference/coercions-code-gen-helpers
       (->* (#:apply-coercion Apply-Coercion-Type)
            (Values PBox-Ref-Type PBox-Set-Type
                    PVec-Ref-Type PVec-Set-Type PVec-Len-Type)))
#;(define (make-proxied-reference/coercions-code-gen-helpers #:apply-coercion apply-coercion)
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

#;(: make-proxied-reference/coercions-compile-helpers
   (->* (#:apply-coercion Apply-Coercion-Type)
        (Values PBox-Ref-Type PBox-Set-Type
                PVec-Ref-Type PVec-Set-Type PVec-Len-Type
                CoC3-Bnd-Code*)))
#;(define (make-proxied-reference/coercions-compile-helpers #:apply-coercion apply-coercion)
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


#;(: make-monotonic-reference/coercions-compile-helpers
   (->* (#:apply-coercion Apply-Coercion-Type
         #:make-coercion  Make-Coercion-Type)
        (Values MBox-Ref-Type MBox-Set-Type
                MVec-Ref-Type MVec-Set-Type
                CoC3-Bnd-Code*)))
#;(define (make-monotonic-reference/coercions-compile-helpers
         #:apply-coercion apply-coercion
         #:make-coercion  make-coercion)
  
  ;; Monotonic Reference Types Implementation
  (: code-gen-mbox-ref MBox-Ref-Type)
  (define (code-gen-mbox-ref mref t2)
    (let*$ ([mref mref] 
            [t1 (Mbox-rtti-ref mref)]
            [crcn (make-coercion t1 t2 (Quote "Blame Monotonic"))])
      (apply-coercion (Mbox-val-ref mref) crcn)))
  
  (: code-gen-mbox-set!  MBox-Set-Type)
  (define (code-gen-mbox-set! mref val t1)
    (let*$ ([mref mref] [val  val] [t1 t1]
            [t2 (Mbox-rtti-ref mref)]
            [c (make-coercion t1 t2 (Quote "Monotonic"))]
            [cv (cond$
                 [(and$ (type-tup?$ t1) (type-tup?$ t2))
                  (let*$ ([n (Type-Tuple-num t2)]
                          [ctv (Copy-Tuple n val)])
                    (begin$
                      (Mbox-val-set! mref ctv)
                      ctv))]
                 [else val])]
            [ccv (apply-coercion cv c mref)]
            [t2-new (Mbox-rtti-ref mref)])
      (If (op=? t2 t2-new)
          (Mbox-val-set! mref ccv)
          (Quote '()))))
  (: code-gen-mvec-ref  MVec-Ref-Type)
  (define (code-gen-mvec-ref mvec i t2)
    (let*$ ([mvec mvec] [i i] [t2 t2]
            [t1 (Mvector-rtti-ref mvec)]
            [crcn (make-coercion t1 t2 (Quote "Monotonic"))])
      (apply-coercion (Mvector-val-ref mvec i) crcn)))
  
  (: code-gen-mvec-set! MVec-Set-Type)
  (define (code-gen-mvec-set! mvec i val t1)
    (let*$ ([mvec mvec]
            [i i]
            [val val]
            [t1 t1]
            [t2 (Mvector-rtti-ref mvec)]
            [c (make-coercion t1 t2 (Quote "Monotonic"))])
      (cond$
       [(and$ (type-tup?$ t1) (type-tup?$ t2))
        (let*$ ([n (Type-Tuple-num t2)]
                [cvi (Copy-Tuple n val)])
          (begin$
            (Mvector-val-set! mvec i cvi)
            (let*$ ([ccvi (apply-coercion cvi c mvec)]
                    [t2-new (Mvector-rtti-ref mvec)])
              (If (op=? t2 t2-new)
                  (Mvector-val-set! mvec i ccvi)
                  (Quote '())))))]
       [else
        (let*$ ([cvi (apply-coercion val c mvec)]
                [t2-new (Mvector-rtti-ref mvec)])
          (If (op=? t2 t2-new)
              (Mvector-val-set! mvec i cvi)
              (Quote '())))])))

  (cond
    [(inline-guarded-branch?)
     (values code-gen-mbox-ref
             code-gen-mbox-set!
             code-gen-mvec-ref
             code-gen-mvec-set!
             '())]
    [else
     ;; If they are not inlined then the compiler we generate
     ;; the runtime binding and returns procedures that builds
     ;; invokations of this runtime code.
     (let ([mbr   (next-uid! "rt_mbox_ref")]
           [mbs   (next-uid! "rt_mbox_set")]
           [mvr   (next-uid! "rt_mvec_ref")]
           [mvs   (next-uid! "rt_mvec_set")])
       (values
        (apply-code-curry mbr) (apply-code-curry mbs)
        (apply-code-curry mvr) (apply-code-curry mvs)
        `([,mbr . ,(code$ (mbox type)
                     (code-gen-mbox-ref mbox type))]
          [,mbs . ,(code$ (mbox value type)
                     (code-gen-mbox-set! mbox value type))]
          [,mvr . ,(code$ (mvec index type)
                     (code-gen-mvec-ref mvec index type))]
          [,mvs . ,(code$ (mvec index value type)
                     (code-gen-mvec-set! mvec index value type))])))]))

#;(: make-dynamic-operations/coercions-compile-helpers
   (->* (#:cast Compile-Cast-Type
         #:compile-app App-Type
         #:pbox-ref PBox-Ref-Type
         #:pbox-set PBox-Set-Type
         #:pvec-ref PVec-Ref-Type
         #:pvec-set PVec-Set-Type
         #:pvec-len PVec-Len-Type
         #:mbox-ref MBox-Ref-Type
         #:mbox-set MBox-Set-Type
         #:mvec-ref MVec-Ref-Type
         #:mvec-set MVec-Set-Type)
        (Values Dyn-PBox-Ref-Type Dyn-PBox-Set-Type
                Dyn-PVec-Ref-Type Dyn-PVec-Set-Type Dyn-PVec-Len-Type
                Dyn-MBox-Ref-Type Dyn-MBox-Set-Type
                Dyn-MVec-Ref-Type Dyn-MVec-Set-Type
                Dyn-Fn-App-Type
                Dyn-Tup-Prj-Type
                CoC3-Bnd-Code*)))
#;(define (make-dynamic-operations/coercions-compile-helpers
         #:cast cast #:compile-app compile-app
         #:pbox-ref pbox-ref #:pbox-set pbox-set!
         #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
         #:mbox-ref mbox-ref #:mbox-set mbox-set!
         #:mvec-ref mvec-ref #:mvec-set mvec-set!)
  ;; Dynamic Operation Specialization
  (: code-gen-dyn-pbox-ref Dyn-PBox-Ref-Type)
  (define (code-gen-dyn-pbox-ref dyn lbl)
    (let*$ ([v dyn] [l lbl])
      (If (dyn-immediate-tag=?$ v PBOX-DYN-EXPR)
          (let*$ ([val (dyn-box-value$ v)]
                  [ty  (dyn-box-type$ v)])
            (If (Type-GRef-Huh ty)
                (let*$ ([tyof (Type-GRef-Of ty)]
                        [read-val (pbox-ref val)])
                  (cast read-val tyof DYN-EXPR l))
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
                    (let$ ([wrt-val2 (cast wrt-val1 t2 tyof lbl)])
                      (pbox-set! gbox wrt-val2))])
                  (cast (Quote '()) UNIT-EXPR DYN-EXPR lbl))
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
               (cast elem-val elem-ty DYN-EXPR lbl))]
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
                                  (cast wrt-val1 t2 elem-type lbl))])
              (pvec-set! maybe-vec ind new-elem)
              (cast (Quote '()) UNIT-EXPR DYN-EXPR lbl))]
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
                  (cast (mbox-set! mbox wrt-val1 t2) UNIT-EXPR DYN-EXPR lbl)
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
                (cast (mvec-set! val ind wrt-val1 t2) UNIT-EXPR DYN-EXPR lbl)]
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
    (define uu (next-uid! "prj-fn"))
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
          (cast v (Type t) dyn-fn-arg-type l))))
    (define casts-apply : CoC3-Expr (compile-app u arg-casts))

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
              (cast ret-val ret-ty DYN-EXPR l))]
           [else (Blame l)]))]
       [else (Blame l)])))
  
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
              (ann (cast prj-val prj-ty DYN-EXPR l) CoC3-Expr))]
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
      (th-error 'interpret-cast-with-hyper-coercions/dyn-tup-prj)
      '())]
    ;; In the case that dynamic operations are inlined we should
    ;; generate the code in-place and therefore do not need any
    ;; runtime support for dynamic-operations.
    ;; no code bindings returned.
    [(inline)
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
      code-gen-dyn-tup-prj
      '())]
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
      code-gen-dyn-fn-app ;; Always inline since code is arity dependent
      (apply-code-curry dtp)
      `([,dpbr . ,(code$ (ref lbl)
                    (code-gen-dyn-pbox-ref ref lbl))]
        [,dpbs . ,(code$ (ref val ty lbl)
                    (code-gen-dyn-pbox-set! ref val ty lbl))]
        [,dpvr . ,(code$ (vec ind lbl)
                    (code-gen-dyn-pvec-ref vec ind lbl))]
        [,dpvs . ,(code$ (vec ind val ty lbl)
                    (code-gen-dyn-pvec-set! vec ind val ty lbl))]
        [,dpvl . ,(code$ (vec lbl)
                    (code-gen-dyn-pvec-len vec lbl))]
        [,dmbr . ,(code$ (ref lbl)
                    (code-gen-dyn-mbox-ref ref lbl))]
        [,dmbs . ,(code$ (ref val ty lbl)
                    (code-gen-dyn-mbox-set! ref val ty lbl))]
        [,dmvr . ,(code$ (vec ind lbl)
                    (code-gen-dyn-mvec-ref vec ind lbl))]
        [,dmvs . ,(code$ (vec ind val ty lbl)
                    (code-gen-dyn-mvec-set! vec ind val ty lbl))]
        [,dtp . ,(code$ (tup ind lbl)
                   (code-gen-dyn-tup-prj tup ind lbl))]))]))

#;(define-type CopyValueInMonoRef-Type (CoC3-Expr -> CoC3-Expr))

#;(: gen-copy-value-in-monoref-code : CopyValueInMonoRef-Type)
#;(define (gen-copy-value-in-monoref-code a-var)
  (match a-var
    [(Var a) (let$ ([t (Mbox-rtti-ref a-var)]
                    [v (Mbox-val-ref a-var)])
               (cond$
                [(type-tup?$ t)
                 (let*$ ([n (Type-Tuple-num t)]
                         [cv (Copy-Tuple n v)])
                   (Begin
                     (list
                      (Mbox-val-set! a-var cv))
                     cv))]
                [else v]))]
    [other (error 'copy-value-in-monoref "unmatched value ~a" other)]))


  #;(define (compile-apply-fn-coercion e m)
    ;; Let binding to provent expression duplication
    (let*$ ([v e])
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
          (App-Code (Fn-Caster v) (list v m)))))

  
  ;; This is where I suspect we need to start branching
  

  #;(define greatest-lower-bound-uid
      (next-uid! "greatest_lower_bound"))
  #;(define copy-mref-uid
    (next-uid! "copy_mref_value"))
  #;(define interp-projection-uid
      (next-uid! "project_dynamic"))
  ;; Generate calls to runtime procedures
  
  #;(define copy-mref : Copy-Mref-Type
    (apply-code-curry copy-mref-uid))

 
    
  ;; This is needs to be updated


  #;(: interp-projection : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  #;(define (interp-projection e t2 l mt)
      (apply-code interp-projection-uid e t2 l mt))
  
  ;; Code Generators for the various casts
  #;(: compile-project : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  #;(define (compile-project e t2 l mt)
    ;; incoming expression let-bound to prevent expression duplication      
    (let* ([v e] [l l])
      (match t2
        [(Type (or (Int) (Character) (Unit) (Bool)))
         (If (dyn-immediate-tag=?$ v t2)
             (dyn-immediate-value$ v)
             (interp-projection v t2 l mt))]
        [(Type _) 
         (If (dyn-immediate-tag=?$ v t2)
             (let*$ ([u  (dyn-box-value$ v)]
                     [t1 (dyn-box-type$ v)])
               (If (op=? t1 t2)
                   u
                   ;; TODO: compile-med-cast should exists

                   (apply-med-coercion u (make-med-coercion t1 t2 l) mt)))
             (interp-projection v t2 l mt))]
        [otherwise
         (let$ ([t2 t2])
           (let*$ ([dv (dyn-value$ v)]
                   [t1 (dyn-type$ v)])
             (If (op=? t1 t2)
                 dv
                 ;; t1 != t2 -> < t1 =>^l t2> != Id
                 ;; therefore no need to make Id case fast 
                 (apply-med-coercion dv (make-med-coercion t1 t2 l) mt))))])))
  #;(: interp-projection-code  (Code Uid* CoC3-Expr))
  #;(define interp-projection-code
    ;; Using variables result in generating the generic code that is constant but large
    (code$ (v t2 l mt) (compile-project v t2 l mt)))
  
  #;(: compile-mref-cast : CoC3-Expr CoC3-Expr -> CoC3-Expr)
  #;(define (compile-mref-cast e t2)
    (let*$ ([v e][t2 t2])
      (cond$
       [(Type-Dyn-Huh t2) v]
       [else
        (let*$ ([t1 (Mbox-rtti-ref v)]
                [t3 (greatest-lower-bound t1 t2)])
          (cond$
           [(op=? t1 t3) v]
           [else
            (Mbox-rtti-set! v t3)
            (let*$ ([v-copy (copy-mref v)]
                    ;; This call should be replace with a specialize
                    ;; procedure for casting without creating this
                    ;; intermediate coercion. 
                    [c  (make-coercion t1 t3 (Quote "Monotonic"))]
                    [new-v (apply-coercion v-copy c v)]
                    [t4 (Mbox-rtti-ref v)])
              (cond$
               [(op=? t3 t4) (Mbox-val-set! v new-v) v]
               [else v]))]))])))

  #;(define (compile-mvec-cast e t2)
    (let*$ ([v e][t2 t2])
      (cond$
       [(Type-Dyn-Huh t2) v]
       [else
        (let*$ ([t1 (Mvector-rtti-ref v)]
                [t3 (greatest-lower-bound t1 t2)])
          (cond$
           [(op=? t1 t3) v]
           [else
            (Mvector-rtti-set! v t3)
            ;; This blame label is fabricated from nothing because
            ;; monotonic references are not completely implemented.
            (let$ ([c   (make-coercion t1 t3 (Quote "Monotonic"))]
                   [len (Mvector-length v)])
              (cond$
               [(Type-Tuple-Huh t3)
                (let*$ ([n (Type-Tuple-num t3)])
                  (repeat$ (i (Quote 0) len) ()
                    (let*$ ([vi (Mvector-val-ref v i)]
                            [cvi (Copy-Tuple n vi)])
                      (Mvector-val-set! v i cvi)
                      (let*$ ([ccvi (apply-coercion cvi c v)]
                              [t4 (Mvector-rtti-ref v)])
                        (If (op=? t3 t4)
                            (Mvector-val-set! v i ccvi)
                            (Break-Repeat))))))]
               [else
                (repeat$ (i (Quote 0) len) ()
                  (let*$ ([vi (Mvector-val-ref v i)]
                          [cvi (apply-coercion vi c v)]
                          [t4 (Mvector-rtti-ref v)])
                    (If (op=? t3 t4)
                        (Mvector-val-set! v i cvi)
                        (Break-Repeat))))]))
            v]))])))

  
  #;(define greatest-lower-bound-code
      (code$ (t1 t2)
        ((gen-greatest-lower-bound-type-code
          next-uid!
          greatest-lower-bound
          greatest-lower-bound-uid)
         t1 t2)))
  
  #;(define copy-mref-code
    (code$ (mref)
      (gen-copy-value-in-monoref-code mref)))

  #;(: compile-apply-ref-coercion Apply-Coercion-Type)
  #;(define (compile-apply-ref-coercion e m [mt : CoC3-Expr ZERO-EXPR])
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
             [(and$ (HC-Identity-Huh r-read) (HC-Identity-Huh r-write))
              old-v]
             [else
              (Guarded-Proxy old-v (Coercion (Ref-Coercion r-read r-write)))])))]
       [else (Guarded-Proxy v (Coercion m))])))

       #;(: compile-cast Compile-Cast-Type)
       #;(define (compile-cast e t1 t2 l [mt : CoC3-Expr ZERO-EXPR]
                             #:t1-not-dyn [t1-not-dyn : Boolean #f]
                             #:t2-not-dyn [t2-not-dyn : Boolean #f])
         (let*$ ([v e])
           (match* (t1 t2)
             [((Type t) (Type t)) v]
             ;; We don't have to check not Dyn here because t1 <> t2
             [((Type (Dyn)) (Type _))
              (compile-project v t2 l mt)]
             [((Type (Dyn)) t2)
              (cond
                [t2-not-dyn (compile-project v t2 l mt)]
                [else
                 (If (Type-Dyn-Huh t2)
                     v
                     (compile-project v t2 l mt))])]
             ;; We don't have to check not dyn here because t1 <> t2
             [((Type _) (Type (Dyn))) (compile-inject v t1)]
             [(t1 (Type (Dyn)))
              (cond
                [t1-not-dyn (compile-inject v t1)]
                [else
                 (If (Type-Dyn-Huh t1)
                     v
                     (compile-inject v t1))])]
             [(t1 t2)
              (define hc : CoC3-Expr (compile-make-coercion t1 t2 l))
              (debug 'compile-cast/hyper-coercions hc)
              (match hc
                [hc #:when (not (optimize-first-order-coercions?))
                    (apply-coercion v hc)]
                ;; The next cases should never occur because the previous
                ;; code handles them without a call to make-coercion.
                [(Quote-HCoercion (HC #f _ #f #f _ (Identity)))
                 e]
                ;; Hyper-Coercion is Projection 
                [(Quote-HCoercion (HC #t t1 (? blame-label? l) #f _ (Identity)))
                 (compile-project v (Type t1) (Quote l) mt)] 
                ;; Hyper-Coercion is Injection
                [(Quote-HCoercion (HC #f _ #f #t t2 (Identity)))
                 (compile-inject v (Type t2))]
                ;; Hyper-Coercion is a Funtion Coercion
                [(Quote-HCoercion (HC #f _ #f #f _ (and m (Fn a _ _))))
                 #:when (direct-fn-cast-optimization?)
                 ;; Fn-casts use specialize code for their arity
                 ;; Since we are generating it here it is trivial to
                 ;; directly call that code.
                 (let ([caster (get-fn-cast! a)])
                   (App-Code (Code-Label caster) (list v (Quote-HCoercion m))))]
                [(Quote-HCoercion (HC #f _ #f #f _ (and m (Ref c1 c2))))
                 (compile-apply-ref-coercion v (Quote-HCoercion m))]
                [(Quote-HCoercion (HC #f _ #f #f _ (and m (CTuple n c*))))
                 (compile-apply-tup-coercion v (Quote-HCoercion m) mt)]
                [(Quote-HCoercion (HC #f _ #f #f _ (MonoRef t2)))
                 (compile-mbox-cast/coercions v (Type t2))]
                [(Quote-HCoercion (HC #f _ #f #f _ (MonoVect t2)))
                 (compile-mvec-cast/coercions v (Type t2))]
                [(Quote-HCoercion (HC #f _ #f #f _ m))
                 (apply-med-coercion v (Quote-HCoercion m))]
                [hc (apply-coercion v hc mt)])])))

  #;(lambda ([e : C0-Expr])
    (define ret
      ((make-map-expr
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
       #:dyn-tup-prj dyn-tup-prj)
       e))
    (hack! (get-cast-code-bindings!))
    ret)
