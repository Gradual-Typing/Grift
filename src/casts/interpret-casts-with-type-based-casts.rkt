#lang typed/racket/base
;; - This pass creates a cast-interpreter that can cast arbitrary values at
;;   runtime.
;; - Compiles casts to either calls to the cast interpereter or a specialization
;;   of the cast interpreter that can handle that specific cast base on the
;;   value of cast-specialization variable.

(require
 racket/match
 racket/list
 "../configuration.rkt"
 "../language/cast0.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/data-representation.rkt"
 "../language/syntax.rkt"
 "interpret-casts-common.rkt")

(provide interpret-casts/type-based-casts)


(: interpret-casts/type-based-casts : -> (C0-Expr -> CoC3-Expr))
(define (interpret-casts/type-based-casts)

  ;; Interp-Cast Builds a call to a runtime function
  ;; that casts based on types.
  (define interp-cast-uid (next-uid! "interp-cast"))
  (define (interp-cast [v : CoC3-Expr] [t1 : CoC3-Expr]
                       [t2 : CoC3-Expr] [l : CoC3-Expr]
                       [mt : CoC3-Expr ZERO-EXPR])
    (apply-code interp-cast-uid v t1 t2 l mt))
  

  ;; Compiling Function Casts
  (: build-fn-caster : Nat -> (Values Uid CoC3-Code))
  (define (build-fn-caster ary)
    (define name (next-uid! (string-append "fn-cast_" (number->string ary))))
    (match-define (and caster-fmls (list fn t1 t2 lbl))
      (map next-uid! '("f" "t1" "t2" "lbl")))
    (match-define (list fn-var t1-var t2-var lbl-var)
      (map #{Var @ Uid} #{caster-fmls :: Uid*}))
    (define uid* (map next-uid! (make-list ary "v")))
    (define args
      (for/list : (Listof CoC3-Expr)
                ([u uid*]
                 [i (in-naturals)])
        (let* ([i  : CoC3-Expr (Quote i)]
               [t1 : CoC3-Expr (Type-Fn-arg t1-var i)]
               [t2 : CoC3-Expr (Type-Fn-arg t2-var i)])
          (interp-cast (Var u) t2 t1 lbl-var (Quote 0)))))
    (define t1-ret (Type-Fn-return t1-var))
    (define t2-ret (Type-Fn-return t2-var))
    (define call (App-Fn fn-var args))
    (define cast-call (interp-cast call t1-ret t2-ret lbl-var (Quote 0)))
    (define then-cast (Lambda uid* (Castable name cast-call)))
    (values name (Code caster-fmls then-cast)))

  (: compile-app App-Type)
  (define compile-app App-Fn)

  (define get-fn-cast!
    (make-fn-cast-helpers build-fn-caster))
  
  (define compile-fn-cast
    (make-compile-fn-cast #:get-fn-cast! get-fn-cast!))

  (: compile-lambda Lambda-Type)
  (define (compile-lambda fml* e)
    (define ctr (get-fn-cast! (length fml*)))
    (Lambda fml* (Castable ctr e)))

  (define compile-tuple-cast
    (make-compile-cast-tuple #:interp-cast-uid interp-cast-uid))

  (define compile-types-greatest-lower-bound
    (make-compile-types-greatest-lower-bound))
  
  (define compile-mbox-cast
    (make-compile-mbox-cast
     #:interp-cast interp-cast
     #:greatest-lower-bound compile-types-greatest-lower-bound))

  (define compile-mvec-cast
    (make-compile-mvec-cast
     #:interp-cast interp-cast
     #:greatest-lower-bound compile-types-greatest-lower-bound))

  (define interp-med-cast
    (make-interp-med-cast-runtime!
     #:fn-cast    compile-fn-cast
     #:tuple-cast compile-tuple-cast
     #:pref-cast   compile-cast-pref/type-based
     #:pvec-cast  compile-cast-pvec/type-based
     #:mbox-cast  compile-mbox-cast
     #:mvec-cast  compile-mvec-cast))
  
  (define compile-med-cast
    (make-compile-med-cast
     #:fn-cast    compile-fn-cast
     #:tuple-cast compile-tuple-cast
     #:pref-cast  compile-cast-pref/type-based
     #:pvec-cast  compile-cast-pvec/type-based
     #:mbox-cast  compile-mbox-cast
     #:mvec-cast  compile-mvec-cast
     #:interp-med-cast interp-med-cast))

  (define compile-project
    (make-compile-project #:compile-med-cast compile-med-cast))

  (define compile-inject (make-compile-inject))

  (make-interp-cast-runtime!
   #:interp-cast-uid interp-cast-uid 
   #:project compile-project
   #:inject  compile-inject
   #:compile-med-cast compile-med-cast)

  (define compile-cast : Compile-Cast-Type
    (make-compile-cast
     #:interp-cast interp-cast 
     #:project compile-project
     #:inject  compile-inject
     #:compile-med-cast compile-med-cast))

  (define-values (pbox-ref pbox-set! pvec-ref pvec-set! pvec-len)
    (make-proxied-reference-helpers/type-based-casts
     #:cast interp-cast))
  
  (define-values (mbox-ref mbox-set! mvec-ref mvec-set!)
    (make-monotonic-helpers #:compile-cast compile-cast))
    
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
   #:dyn-tup-prj     dyn-tup-prj))


;; Functions for use sites of guarded references with coercions
(: make-proxied-reference-helpers/type-based-casts
   (->* (#:cast Cast-Type)
        (Values PBox-Ref-Type PBox-Set-Type
                PVec-Ref-Type PVec-Set-Type PVec-Len-Type)))
(define (make-proxied-reference-helpers/type-based-casts #:cast cast)
  (define pbox-ref-uid (next-uid! "pbox-ref"))
  (: interp-pbox-ref PBox-Ref-Type)
  (define (interp-pbox-ref b) (apply-code pbox-ref-uid b))
  (: code-gen-pbox-ref PBox-Ref-Type)
  (define (code-gen-pbox-ref b)
    (let$ ([b b])
      (If (Guarded-Proxy-Huh b)
          (let*$ ([u  (Guarded-Proxy-Ref b)]
                  [t1 (Guarded-Proxy-Source b)]
                  [t2 (Guarded-Proxy-Target b)]
                  [l  (Guarded-Proxy-Blames b)]
                  [v  (interp-pbox-ref u)])
            (cast v t1 t2 l))
          (Unguarded-Box-Ref b))))
  (add-cast-runtime-binding!
   pbox-ref-uid
   (code$ (b) (code-gen-pbox-ref b)))
  (define pbox-set!-uid (next-uid! "pbox-set!"))
  (: interp-pbox-set! PBox-Set-Type)
  (define (interp-pbox-set! b v)
    (apply-code pbox-set!-uid b v))
  (: code-gen-pbox-set! PBox-Set-Type)
  (define (code-gen-pbox-set! b v)
    (let$ ([b b][v v])
      (If (Guarded-Proxy-Huh b)
          (let*$ ([u  (Guarded-Proxy-Ref b)]
                  [t1 (Guarded-Proxy-Source b)]
                  [t2 (Guarded-Proxy-Target b)]
                  [l  (Guarded-Proxy-Blames b)]
                  [v  (cast v t2 t1 l)])
            (interp-pbox-set! u v))
          (Unguarded-Box-Set! b v))))
  (add-cast-runtime-binding!
   pbox-set!-uid
   (code$ (b v) (code-gen-pbox-set! b v)))
  (define pvec-ref-uid (next-uid! "pvec-ref"))
  (: interp-pvec-ref PVec-Ref-Type)
  (define (interp-pvec-ref v i)
    (apply-code pvec-ref-uid v i))
  (: code-gen-pvec-ref PVec-Ref-Type)
  (define (code-gen-pvec-ref v i)
    (let$ ([v v] [i i])
      (If (Guarded-Proxy-Huh v)
          (let*$ ([u  (Guarded-Proxy-Ref v)]
                  [t1 (Guarded-Proxy-Source v)]
                  [t2 (Guarded-Proxy-Target v)]
                  [l  (Guarded-Proxy-Blames v)]
                  [r  (interp-pvec-ref u i)])
            (cast r t1 t2 l))
          (Unguarded-Vect-Ref v i))))
  (add-cast-runtime-binding!
   pvec-ref-uid
   (code$ (v i) (code-gen-pvec-ref v i)))
  (define pvec-set!-uid (next-uid! "pvec-set!"))
  (: interp-pvec-set! PVec-Set-Type)
  (define (interp-pvec-set! v i w)
    (apply-code pvec-set!-uid v i w))
  (: code-gen-pvec-set! PVec-Set-Type)
  (define (code-gen-pvec-set! v i w)
    (If (Guarded-Proxy-Huh v)
        (let*$ ([u (Guarded-Proxy-Ref v)]
                [t1 (Guarded-Proxy-Source v)]
                [t2 (Guarded-Proxy-Target v)]
                [l  (Guarded-Proxy-Blames v)]
                [w  (cast w t2 t1 i)])
          (interp-pvec-set! u i w))
        (Unguarded-Vect-Set! v i w)))
  (add-cast-runtime-binding!
   pvec-set!-uid
   (code$ (v i w) (code-gen-pvec-set! v i w)))
  (define pvec-len-uid (next-uid! "pvec-length"))
  (: interp-pvec-len PVec-Len-Type)
  (define (interp-pvec-len v)
    (apply-code pvec-len-uid v))
  (: code-gen-pvec-len PVec-Len-Type)
  (define (code-gen-pvec-len v)
    (let$ ([v v])
      (If (Guarded-Proxy-Huh v)
          (interp-pvec-len (Guarded-Proxy-Ref v))
          (Unguarded-Vect-length v))))
  (add-cast-runtime-binding!
   pvec-len-uid
   (code$ (v) (code-gen-pvec-len v)))
  (cond
    [(inline-guarded-branch?)
     (values code-gen-pbox-ref code-gen-pbox-set!
             code-gen-pvec-ref code-gen-pvec-set! code-gen-pvec-len)]
    [else
     (values interp-pbox-ref interp-pbox-set!
             interp-pvec-ref interp-pvec-set! interp-pvec-len)]))
