#lang typed/racket/base
;; - This pass creates a cast-interpreter that can cast arbitrary values at
;;   runtime.
;; - Compiles casts to either calls to the cast interpereter or a specialization
;;   of the cast interpreter that can handle that specific cast base on the
;;   value of cast-specialization variable.

(require
 scribble/srcdoc
 racket/match
 racket/format
 racket/list
 racket/set
 (for-syntax racket/base)
 (except-in  "../helpers.rkt" logging)
 (submod "../logging.rkt" typed)
 "../unique-identifiers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast0.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/data-representation.rkt"
 "../language/syntax.rkt"
 "interpret-casts-common.rkt"
 #;
 (except-in "./interpret-casts-help.rkt"
            let$* cond$ op=? and$ or$))

(provide
 interpret-casts/type-based-casts
 (all-from-out
  "../language/cast0.rkt"
  "../language/cast-or-coerce3.rkt"))




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

  ;; TODO Consider moving this to common and instntiating mbox and mvec at the same time
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
     #:ref-cast   compile-cast-pref/type-based
     #:mbox-cast  compile-mbox-cast
     #:mvec-cast  compile-mvec-cast))
  
  (define compile-med-cast
    (make-compile-med-cast
     #:fn-cast    compile-fn-cast
     #:tuple-cast compile-tuple-cast
     #:ref-cast   compile-cast-pref/type-based
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
  ;; End of Casting Runtime Instantiation

  (: pbox-ref  PBox-Ref-Type)
  (: pbox-set! PBox-Set-Type)
  (: pvec-ref  PVec-Ref-Type)
  (: pvec-set! PVec-Set-Type)
  (: pvec-len  PVec-Len-Type)
  (define-values (pbox-ref pbox-set!
                  pvec-ref pvec-set! pvec-len)
    (make-proxied-reference-helpers/type-based-casts
     #:cast interp-cast))
  
  (: mbox-set! MBox-Set-Type)
  (: mbox-ref MBox-Ref-Type)
  (: mvec-ref MVec-Ref-Type)
  (: mvec-set! MVec-Set-Type) 
  (define-values (mbox-ref mbox-set! mvec-ref mvec-set!)
    (make-monotonic-helpers #:compile-cast compile-cast))
    
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
  (define-values (dyn-pbox-ref dyn-pbox-set!
                  dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
                  dyn-mbox-ref dyn-mbox-set!
                  dyn-mvec-ref dyn-mvec-set!
                  dyn-fn-app dyn-tup-prj)
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

;; (: use-simple-cast-interp? (Parameterof Boolean))
;; (define use-simple-cast-interp? (make-parameter #f))



                
;; (: interpret-casts/twosomes : Cast0-Lang ->  Cast-or-Coerce3-Lang)
#;
(define (interpret-casts/twosomes prgm)
  ;; Desugaring the input program into its constituents 
  (match-define (Prog (list prgm-name prgm-next prgm-type) prgm-exp)
    prgm)
  (define unique-counter (make-unique-counter prgm-next))
  (define fn-casts : (HashTable Nat CoC3-Bnd-Code)
    (make-hasheq))
  
  (parameterize ([current-unique-counter unique-counter])

    ;; First we generate names for everything in the runtime
    ;; and procedure that will either generate code to perform
    ;; and action or call off to one of the runtime functions
    ;; we are generating.

    ;; Configuration options that determine how code is generated
    (define-type inlinable-casts
      (U 'cast 'inject 'project 'higher-order
         'cast-ground 'cast-undyned 'cast-dyn
         'cast-fn 'cast-gbox 'cast-gvec
         'cast-mvec))

    (define inlined-casts : (Setof inlinable-casts)
      (if (specialize-cast-code-generation?) 
          (set 'inject 'project 'higher-order
               'cast-ground 'cast-undyned 'cast-dyn
               'cast-fn 'cast-gbox 'cast-gvec)
          (set)))
    
    ;; The runtime label for the runtime coercion interpreter
    (define-syntax (inline-or-bnd-cast stx)
      (syntax-case stx ()
        [(_ bnd* f-sym f-name cg-exp ty-arg arg ...)
         (with-syntax ([(p ...) (generate-temporaries #'(arg ...))]
                       [((ta _) ...) #'([ty-arg arg] ...)])
           (syntax/loc stx
             (let ([gen cg-exp])
               (cond
                 [(set-member? inlined-casts f-sym)
                  (values bnd* gen)]
                 [else
                  (define f : Uid (if (Uid? f-name) f-name (next-uid! f-name)))
                  (define p : Uid (next-uid! arg)) ...
                  (define bnd : CoC3-Bnd-Code
                    (cons f (Code (list p ...) (gen (Var p) ...))))
                  (define (call [p : ta] ...) : CoC3-Expr
                    (App-Code (Code-Label f) (list p ...)))
                  (values (cons bnd bnd*) call)]))))]))

    
    (: bindings-needed-for-casts CoC3-Bnd-Code*)
    (: cast : Cast-With-MAddr-Type)
    (: cast-u : Uid)
    (define-values (bindings-needed-for-casts cast cast-u)
      (begin
        (error 'stub)
        (cond
          #;
          [(use-simple-cast-interp?)
           (let*-values
               ([(cast-uid) (next-uid! "interp_cast")]
                ;; This line prevents the code generation
                ;; from infinitely looping calls to cast in
                ;; code for casting makes a recursive call. 
                [(cast) (ann (apply-code cast-uid) Cast-With-MAddr-Type)]
                [(b* ho-cast)
                 (inline-or-bnd-cast
                  '() 'higher-order "higher_order_cast"
                  (make-higher-order-cast-code cast-uid cast)
                  CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
                [(b* inject)
                 (inline-or-bnd-cast
                  b* 'inject "inject_cast"
                  make-inject-code
                  CoC3-Expr "value" "type")]
                [(b* project)
                 (inline-or-bnd-cast
                  b* 'project "project_cast"
                  (make-project-code cast)
                  CoC3-Expr "value" "type" "blame_info" "mono_address")]
                [(b* cast)
                 (inline-or-bnd-cast
                  b* 'cast cast-uid
                  (make-cast-code inject project ho-cast)
                  CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")])
             (values b* cast cast-uid))]
          [else
           (let*-values
               ([(b* cast-fn)
                 (inline-or-bnd-cast
                  '() 'cast-function "cast_function"
                  make-cast-fn-code
                  CoC3-Expr "value" "type1" "type2" "blame_info")]
                [(b* cast-gbox)
                 (inline-or-bnd-cast
                  b* 'cast-gbox "cast_gbox"
                  make-cast-gbox-code
                  CoC3-Expr "value" "type1" "type2" "blame_info")]
                [(glbt-uid) (next-uid! "greatest-lower-bound")]
                [(glbt) (ann (apply-code glbt-uid) GreatestLowerBound-Type)]
                [(b* make-glbt)
                 (error 'todo)
                 #;
                 (inline-or-bnd-cast
                  b* 'glbt glbt-uid
                  (gen-greatest-lower-bound-type-code next-uid! glbt glbt-uid)
                  CoC3-Expr "type1" "type2")]
                [(cast-uid) (next-uid! "interp_cast")]
                [(cast) (ann (apply-code cast-uid) Cast-With-MAddr-Type)]
                [(cv-uid) (next-uid! "copy_value_in_monoref")]
                [(cv) (ann (apply-code cv-uid) CopyValueInMonoRef-Type)]
                [(b* make-cv)
                 (error 'todo)
                 #;
                 (inline-or-bnd-cast
                  b* 'cv cv-uid
                  (gen-copy-value-in-monoref-code next-uid!)
                  CoC3-Expr "address")]
                [(b* cast-mbox) 
                 (inline-or-bnd-cast
                  b* 'cast-mbox "cast_mbox"
                  (make-cast-mbox-code cast glbt cv)
                  CoC3-Expr "value" "type1" "type2" "blame_info")]
                [(b* cast-mvect)
                 (inline-or-bnd-cast
                  b* 'cast-mvect "cast_mvect"
                  (make-cast-mvect-code cast glbt)
                  CoC3-Expr "value" "type1" "type2" "blame_info")]
                [(b* cast-gvec)
                 (inline-or-bnd-cast
                  b* 'cast-gvec "cast_gvec"
                  make-cast-gvect-code
                  CoC3-Expr "value" "type1" "type2" "blame_info")]
                ;; [(b* cast-mvec)
                ;;  (inline-or-bnd-cast
                ;;   b* 'cast-mvec "cast_mvec"
                ;;   (make-cast-mvect-code next-uid! glbt)
                ;;   CoC3-Expr "value" "type1" "type2" "blame_info")]
                [(b* cast-tuple) 
                 (inline-or-bnd-cast
                  b* 'cast-tuple "cast_tuple"
                  (make-cast-tuple-code cast-uid cast)
                  CoC3-Expr "value" "type1" "type2" "blame_info")]
                [(b* cast-ground) 
                 (inline-or-bnd-cast
                  b* 'cast-ground "cast_ground"
                  (make-cast-ground-code cast-uid cast-fn cast-gbox
                                         cast-gvec cast-tuple cast-mbox cast-mvect)
                  CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
                [(b* cast-undyned) 
                 (inline-or-bnd-cast
                  b* 'cast-undyned "cast_undyned"
                  (make-cast-undyned-code cast-ground)
                  CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
                [(b* cast-dyn)
                 (inline-or-bnd-cast
                  b* 'cast-dyn "cast_dyn"
                  (make-cast-dyn-code cast-undyned)
                  CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
                [(b* cast)
                 (let ([gen (make-cast-any-code cast-dyn cast-ground)])
                   (define v  : Uid (next-uid! "value")) 
                   (define t1 : Uid (next-uid! "type1"))
                   (define t2 : Uid (next-uid! "type2"))
                   (define l  : Uid (next-uid! "blame_info"))
                   (define a  : Uid (next-uid! "mono_address"))
                   (define bnd : CoC3-Bnd-Code
                     (cons cast-uid (Code (list v t1 t2 l a) (gen (Var v) (Var t1) (Var t2) (Var l) (Var a)))))
                   (define (call [v : CoC3-Expr] [t1 : CoC3-Expr] [t2 : CoC3-Expr] [l : CoC3-Expr] [a : CoC3-Expr])
                     : CoC3-Expr
                     (App-Code (Code-Label cast-uid) (list v t1 t2 l a)))
                   (values (cons bnd b*)
                           (if (set-member? inlined-casts 'cast)
                               gen
                               call)))])
             (values b* cast cast-uid))])))

    (define get-fn-cast! (make-get-fn-cast fn-casts "fn_cast_" cast))

    ;; ;; Initialize all the state for guarded operations
    (: gbox-ref : GBox-Ref-Type)
    (: gbox-set! : GBox-Set-Type)
    (: gvec-ref : GVec-Ref-Type)
    (: gvec-set! : GVec-Set-Type)
    
    (: bindings-needed-for-guarded : CoC3-Bnd-Code*)
    (define-values (gbox-ref gbox-set! gvec-ref gvec-set! gvec-length
                             bindings-needed-for-guarded)
      ;; First we create initialize the code generators
      (let* ([gbox-ref-uid    (next-uid! "gbox_ref")]
             [gbox-set-uid    (next-uid! "gbox_set")]
             [gvec-ref-uid    (next-uid! "gvec_ref")]
             [gvec-set-uid    (next-uid! "gvec_set")]
             [gvec-length-uid (next-uid! "gvec_length")]
             [gbox-ref     : GBox-Ref-Type (apply-code gbox-ref-uid)]
             [gbox-set     : GBox-Set-Type (apply-code gbox-set-uid)]
             [gvec-ref     : GVec-Ref-Type (apply-code gvec-ref-uid)]
             [gvec-set     : GVec-Set-Type (apply-code gvec-set-uid)]
             [gvec-length  : GVec-Length-Type (apply-code gvec-length-uid)]
             [gen-gbox-ref    (make-gbox-ref-code cast gbox-ref)]
             [gen-gbox-set    (make-gbox-set!-code cast gbox-set)]
             [gen-gvec-ref    (make-gvec-ref-code cast gvec-ref)]
             [gen-gvec-set    (make-gvec-set!-code cast gvec-set)]
             [gen-gvec-length (make-gvec-length-code cast gvec-length)]
             [gbr-b (next-uid! "box")]
             [gbs-b (next-uid! "box")]
             [gbs-v (next-uid! "value")] 
             [gvr-r (next-uid! "vec")]
             [gvr-i (next-uid! "index")] 
             [gvs-r (next-uid! "vec")]
             [gvs-i (next-uid! "index")]
             [gvs-v (next-uid! "value")]
             [gvl-r (next-uid! "vec")]
             [bnd*
              `([,gbox-ref-uid
                 . ,(Code (list gbr-b) (gen-gbox-ref (Var gbr-b)))]
                [,gbox-set-uid
                 . ,(Code (list gbs-b gbs-v)
                      (gen-gbox-set (Var gbs-b) (Var gbs-v)))]
                [,gvec-ref-uid
                 . ,(Code (list gvr-r gvr-i)
                      (gen-gvec-ref (Var gvr-r) (Var gvr-i)))]
                [,gvec-set-uid
                 . ,(Code (list gvs-r gvs-i gvs-v)
                      (gen-gvec-set (Var gvs-r) (Var gvs-i)
                                    (Var gvs-v)))]
                [,gvec-length-uid
                 . ,(Code (list gvl-r)
                      (gen-gvec-length (Var gvl-r)))])])
        (cond
          [(inline-guarded-branch?)
           (values gen-gbox-ref gen-gbox-set gen-gvec-ref gen-gvec-set gen-gvec-length bnd*)]
          [else (values gbox-ref gbox-set gvec-ref gvec-set gvec-length bnd*)])))

    (: mbox-ref  : Mbox-refT)
    (: mbox-set! : Mbox-setT)
    (: mvec-ref  : Mvec-refT)
    (: mvec-set! : Mvec-setT)
    (: bindings-needed-for-monotonic-refs : CoC3-Bnd-Code*)
    (define-values (mbox-ref mbox-set! mvec-ref mvec-set!
                             bindings-needed-for-monotonic-refs)
      (let ([gen-mbox-ref-code (make-mbox-ref-code cast)]
            [gen-mbox-set!-code (make-mbox-set!-code cast)]
            [gen-mvec-ref-code (make-mvec-ref-code cast)]
            [gen-mvec-set!-code (make-mvec-set!-code cast)])
        ;; TODO: move glbt and copy-mono here
        (cond
          [(inline-guarded-branch?)
           (values gen-mbox-ref-code gen-mbox-set!-code
                   gen-mvec-ref-code gen-mvec-set!-code
                   `())]
          [else (let* ([mbr   (next-uid! "rt_mbox_ref")]
                       [mbr-b (next-uid! "box")]
                       [mbr-rt (next-uid! "ref_type")]
                       [mbs   (next-uid! "rt_mbox_set")]
                       [mbs-b (next-uid! "box")]
                       [mbs-v (next-uid! "write_val")]
                       [mbs-rt (next-uid! "ref_type")]
                       [mvr   (next-uid! "rt_mvec_ref")]
                       [mvr-r (next-uid! "vec")]
                       [mvr-i (next-uid! "ind")]
                       [mvr-rt (next-uid! "ref_type")]
                       [mvs   (next-uid! "rt_mvec_set")]
                       [mvs-r (next-uid! "vec")]
                       [mvs-i (next-uid! "ind")]
                       [mvs-v (next-uid! "val")]
                       [mvs-rt (next-uid! "ref_type")])
                  (values
                   (apply-code mbr) (apply-code mbs)
                   (apply-code mvr) (apply-code mvs)
                   `([,mbr
                      . ,(Code (list mbr-b mbr-rt) (gen-mbox-ref-code (Var mbr-b) (Var mbr-rt)))]
                     [,mbs
                      . ,(Code (list mbs-b mbs-v mbs-rt)
                           (gen-mbox-set!-code (Var mbs-b) (Var mbs-v) (Var mbs-rt)))]
                     [,mvr
                      . ,(Code (list mvr-r mvr-i mvr-rt)
                           (gen-mvec-ref-code (Var mvr-r) (Var mvr-i) (Var mvr-rt)))]
                     [,mvs
                      . ,(Code (list mvs-r mvs-i mvs-v mvs-rt)
                           (gen-mvec-set!-code (Var mvs-r) (Var mvs-i)
                                               (Var mvs-v) (Var mvs-rt)))])))])))

    (: dyn-fn-app : Dyn-Fn-App-Type)
    (: bindings-needed-for-fn-dynamic-operations : CoC3-Bnd-Code*)
    (define-values (dyn-fn-app
                    bindings-needed-for-fn-dynamic-operations)
      (let ([gen-dyn-fn-app (make-dyn-fn-app-code cast)])
        (define ((th-error [sym : Symbol]) . a)
          (error sym "dynamic-operation? = #f but present in AST"))
        (case (dynamic-operations?)
          [(#f)
           (values
            (th-error 'interpret-cast-with-twosomes/dyn-fn-app)
            '())]
          [else
           (values
            gen-dyn-fn-app
            `())])))
    
    ;; TODO if the simple casting interface ends up being competitive
    ;; use project and inject calls in the dynamic operation specialization.
    (: dyn-gbox-ref : Dyn-GBox-Ref-Type)
    (: dyn-gbox-set! : Dyn-GBox-Set-Type)
    (: dyn-gvec-ref  : Dyn-GVec-Ref-Type)
    (: dyn-gvec-set! : Dyn-GVec-Set-Type)
    (: bindings-needed-for-guarded-dynamic-operations : CoC3-Bnd-Code*)
    (define-values (dyn-gbox-ref dyn-gbox-set!
                                 dyn-gvec-ref dyn-gvec-set!
                                 bindings-needed-for-guarded-dynamic-operations)
      (let* ([gen-dyn-gvec-set!
              (make-dyn-gvect-set!-code gvec-set! cast)]
             [gen-dyn-gvec-ref
              (make-dyn-gvect-ref-code gvec-ref cast)]
             [gen-dyn-gbox-set!
              (make-dyn-gbox-set!-code gbox-set! cast)]
             [gen-dyn-gbox-ref 
              (make-dyn-gbox-ref-code gbox-ref cast)])
        (define ((th-error [sym : Symbol]) . a)
          (error sym "dynamic-operation? = #f but present in AST"))
        (case (dynamic-operations?)
          [(#f)
           (values
            (th-error 'interpret-cast-with-twosomes/dyn-gbox-ref)
            (th-error 'interpret-cast-with-twosomes/dyn-gbox-set!)
            (th-error 'interpret-cast-with-twosomes/dyn-gvec-ref)
            (th-error 'interpret-cast-with-twosomes/dyn-gvec-set!)
            '())]
          [(inline)
           (values
            gen-dyn-gbox-ref gen-dyn-gbox-set!
            gen-dyn-gvec-ref gen-dyn-gvec-set!
            '())]
          [else
           (define-values (gbr gbr-b gbr-l)
             (values (next-uid! "dyn_gbox_ref")
                     (next-uid! "box")
                     (next-uid! "blame_info")))
           (define-values (gbs gbs-b gbs-v gbs-t gbs-l)
             (values (next-uid! "dyn_gbox_set")
                     (next-uid! "box")
                     (next-uid! "write_val")
                     (next-uid! "val_type")
                     (next-uid! "blame_info")))
           (define-values (gvr gvr-r gvr-i gvr-l)
             (values (next-uid! "dyn_gvec_ref")
                     (next-uid! "vector")
                     (next-uid! "index") 
                     (next-uid! "blame_info")))
           (define-values (gvs gvs-r gvs-i gvs-v gvs-t gvs-l)
             (values (next-uid! "dyn_gvec_set")
                     (next-uid! "vector")
                     (next-uid! "index")
                     (next-uid! "write_val")
                     (next-uid! "val_type")
                     (next-uid! "blame_info")))
           (values
            (apply-code gbr) (apply-code gbs)
            (apply-code gvr) (apply-code gvs)
            `([,gbr
               . ,(Code (list gbr-b gbr-l)
                    (gen-dyn-gbox-ref (Var gbr-b) (Var gbr-l)))]
              [,gbs
               . ,(Code (list gbs-b gbs-v gbs-t gbs-l)
                    (gen-dyn-gbox-set! (Var gbs-b) (Var gbs-v)
                                       (Var gbs-t) (Var gbs-l)))]
              [,gvr
               . ,(Code (list gvr-r gvr-i gvr-l)
                    (gen-dyn-gvec-ref (Var gvr-r) (Var gvr-i) (Var gvr-l)))]
              [,gvs
               . ,(Code (list gvs-r gvs-i gvs-v gvs-t gvs-l)
                    (gen-dyn-gvec-set! (Var gvs-r) (Var gvs-i)
                                       (Var gvs-v) (Var gvs-t)
                                       (Var gvs-l)))]))])))

    (: dyn-mbox-ref : Dyn-Mbox-refT)
    (: dyn-mbox-set! : Dyn-Mbox-setT)
    (: dyn-mvec-ref  : Dyn-Mvec-refT)
    (: dyn-mvec-set! : Dyn-Mvec-setT)
    (: bindings-needed-for-mono-dynamic-operations : CoC3-Bnd-Code*)
    (define-values (dyn-mbox-ref dyn-mbox-set!
                                 dyn-mvec-ref dyn-mvec-set!
                                 bindings-needed-for-mono-dynamic-operations)
      (let* ([gen-dyn-mvec-set!
              (make-dyn-mvect-set!-code mvec-set! cast)]
             [gen-dyn-mvec-ref
              (make-dyn-mvect-ref-code mvec-ref)]
             [gen-dyn-mbox-set!
              (make-dyn-mbox-set!-code mbox-set! cast)]
             [gen-dyn-mbox-ref 
              (make-dyn-mbox-ref-code mbox-ref)])
        (define ((th-error [sym : Symbol]) . a)
          (error sym "dynamic-operation? = #f but present in AST"))
        (case (dynamic-operations?)
          [(#f)
           (values
            (th-error 'interpret-cast-with-twosomes/dyn-mbox-ref)
            (th-error 'interpret-cast-with-twosomes/dyn-mbox-set!)
            (th-error 'interpret-cast-with-twosomes/dyn-mvec-ref)
            (th-error 'interpret-cast-with-twosomes/dyn-mvec-set!)
            '())]
          [(inline)
           (values
            gen-dyn-mbox-ref gen-dyn-mbox-set!
            gen-dyn-mvec-ref gen-dyn-mvec-set!
            '())]
          [else
           (define-values (mbr mbr-b mbr-l)
             (values (next-uid! "dyn_mbox_ref")
                     (next-uid! "box")
                     (next-uid! "blame_info")))
           (define-values (mbs mbs-b mbs-v mbs-t mbs-l)
             (values (next-uid! "dyn_mbox_set")
                     (next-uid! "box")
                     (next-uid! "write_val")
                     (next-uid! "val_type")
                     (next-uid! "blame_info")))
           (define-values (mvr mvr-r mvr-i mvr-l)
             (values (next-uid! "dyn_mvec_ref")
                     (next-uid! "vector")
                     (next-uid! "index") 
                     (next-uid! "blame_info")))
           (define-values (mvs mvs-r mvs-i mvs-v mvs-t mvs-l)
             (values (next-uid! "dyn_mvec_set")
                     (next-uid! "vector")
                     (next-uid! "index")
                     (next-uid! "write_val")
                     (next-uid! "val_type")
                     (next-uid! "blame_info")))
           (values
            (apply-code mbr) (apply-code mbs)
            (apply-code mvr) (apply-code mvs)
            `([,mbr
               . ,(Code (list mbr-b mbr-l)
                    (gen-dyn-mbox-ref (Var mbr-b) (Var mbr-l)))]
              [,mbs
               . ,(Code (list mbs-b mbs-v mbs-t mbs-l)
                    (gen-dyn-mbox-set! (Var mbs-b) (Var mbs-v)
                                       (Var mbs-t) (Var mbs-l)))]
              [,mvr
               . ,(Code (list mvr-r mvr-i mvr-l)
                    (gen-dyn-mvec-ref (Var mvr-r) (Var mvr-i) (Var mvr-l)))]
              [,mvs
               . ,(Code (list mvs-r mvs-i mvs-v mvs-t mvs-l)
                    (gen-dyn-mvec-set! (Var mvs-r) (Var mvs-i)
                                       (Var mvs-v) (Var mvs-t)
                                       (Var mvs-l)))]))])))

    
    ;; ;; Next we generate the bindings for code that needs to be
    ;; ;; included as the runtime.
    (define gradual-runtime-bindings : CoC3-Bnd-Code*
      (append
       bindings-needed-for-fn-dynamic-operations
       bindings-needed-for-monotonic-refs
       bindings-needed-for-mono-dynamic-operations
       bindings-needed-for-guarded
       bindings-needed-for-guarded-dynamic-operations
       bindings-needed-for-casts))

    (define exp-with-lowered-gradual-operations
      (interpret-casts-in-expr
       #;interp-cast-uid cast
       get-fn-cast!
       gbox-ref gbox-set! gvec-ref gvec-set! gvec-length
       mbox-ref mbox-set! mvec-ref mvec-set!
       dyn-gbox-ref dyn-gbox-set! dyn-gvec-ref dyn-gvec-set!
       dyn-mbox-ref dyn-mbox-set! dyn-mvec-ref dyn-mvec-set!
       dyn-fn-app
       prgm-exp))

    (Prog (list prgm-name (unique-counter-next! unique-counter) prgm-type)
      (Labels (append (hash-values fn-casts) gradual-runtime-bindings)
        exp-with-lowered-gradual-operations))
    #;
    (let* ([exp (ic-expr interp-uid interp compose prgm-exp)]
           [next (unbox next-unique-number)]
           [bnd* (list mk-coercion-binding interp-binding)]
           ;; compose-binding? is correlates space-efficient?
           ;; but typed-racket wants to know the binding
           ;; isn't false.
           [bnd* (if compose-binding?
                     (cons compose-binding? bnd*)
                     bnd*)])
      (Prog (list prgm-name next prgm-type)
        (Labels bnd* (Observe exp prgm-type))))
    
    ;; (define dyn-operations-bnd* : CoC3-Bnd-Code*
    ;;   (cond
    ;;     [(inline-dynamic-operations?) '()]
    ;;     [else
    ;;      (parameterize ([inline-dynamic-operations? #t])
    ;;        (define-values (gunbox-b gunbox-l)
    ;;          (values (next-uid! "box") (next-uid! "blame")))
    ;;        (define gunbox-bnd : CoC3-Bnd-Code
    ;;          (cons dyn-gunbox-uid
    ;;                (Code (list gunbox-b gunbox-l)
    ;;                  (dyn-gunbox (Var gunbox-b) (Var gunbox-l)))))
    ;;        (define-values (setbox-b setbox-v setbox-t setbox-l)
    ;;          (values (next-uid! "box") (next-uid! "value")
    ;;                  (next-uid! "type") (next-uid! "blame")))
    ;;        (define setbox-bnd : CoC3-Bnd-Code
    ;;          (cons dyn-setbox-uid
    ;;                (Code (list setbox-b setbox-v setbox-t setbox-l)
    ;;                  (dyn-setbox (Var setbox-b) (Var setbox-v)
    ;;                              (Var setbox-t) (Var setbox-l)))))
    ;;        (list gunbox-bnd setbox-bnd))]))

    
    ;; body of interpret-casts


    #;(error 'todo)))

;; These templates are used to build the code that performs
;; casting at runtime. The templates are a little over
;; parameterized currently in hopes that it make specialization
;; and customization easier in the future.

;;(define-type Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

;;(define-type Cast-With-MAddr-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

;; These templates are used to build the code that performs
;; casting at runtime.
;; In my opinion they currently do too much manual specialization
;; and we should make them simpler but implement a specialization
;; pass that derives code with similar efficiency. AMK 2015-12-17

;; How to cast any type to some other type
;; (: make-cast-any-code :  Cast-With-MAddr-Type Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
;; (define ((make-cast-any-code cast-dyn cast-ground) v t1 t2 lbl mono-address)
;;   (let*$ ([type1 t1] [type2 t2])
;;     (cond$
;;      [(op=? type1 type2) v]
;;      [(op=? type1 (Type DYN-TYPE)) (cast-dyn v type1 type2 lbl mono-address)]
;;      [else (cast-ground v type1 type2 lbl mono-address)])))

;; (: make-cast-ground-code : Uid Cast-Type Cast-Type Cast-Type Cast-Type Cast-Type Cast-Type -> Cast-With-MAddr-Type)
;; (define ((make-cast-ground-code cast-u cast-fn cast-gref
;;                                 cast-gvect cast-tuple
;;                                 cast-mref cast-mvect) v t1 t2 lbl mono-address)
;;   (error 'todo)
;;   #;(let*$ ([value v] [type1 t1] [type2 t2])
;;       ;; TODO use a switch here or come up with a better
;;       ;; dynamic injection behavior
;;       (cond$
;;        [(op=? type1 (Type INT-TYPE))
;;         (if$ (op=? (Type DYN-TYPE) type2)
;;              (dyn-make$ value (Type INT-TYPE))
;;              (Blame lbl))]
;;        [(op=? type1 (Type BOOL-TYPE))
;;         (if$ (op=? (Type DYN-TYPE) type2)
;;              (dyn-make$ value (Type BOOL-TYPE))
;;              (Blame lbl))]
;;        [(op=? type1 (Type FLOAT-TYPE))
;;         (if$ (op=? (Type DYN-TYPE) type2)
;;              (dyn-make$ value (Type FLOAT-TYPE))
;;              (Blame lbl))]
;;        [(op=? type1 (Type CHAR-TYPE))
;;         (if$ (op=? (Type DYN-TYPE) type2)
;;              (dyn-make$ value (Type CHAR-TYPE))
;;              (Blame lbl))]
;;        [(op=? type1 (Type UNIT-TYPE))
;;         (if$ (op=? (Type DYN-TYPE) type2)
;;              (dyn-make$ value (Type UNIT-TYPE))
;;              (Blame lbl))]
;;        [else
;;         (let*$ ([tag1 (type-tag type1)])
;;           (cond$
;;            [(op=? (Tag 'Fn) tag1)
;;             (cast-fn value type1 type2 lbl)]
;;            [(op=? (Tag 'GRef) tag1)
;;             (cast-gref value type1 type2 lbl)]
;;            [(op=? (Tag 'GVect) tag1)
;;             (cast-gvect value type1 type2 lbl)]
;;            [(op=? (Tag 'MRef) tag1)
;;             (cast-mref value type1 type2 lbl)]
;;            [(op=? (Tag 'MVect) tag1)
;;             (cast-mvect value type1 type2 lbl)]
;;            [(op=? (Tag 'STuple) tag1)
;;             (If (Op '= (list (Quote 0) mono-address))
;;                 (cast-tuple value type1 type2 lbl)
;;                 (Cast-Tuple-In-Place cast-u value type1 type2 lbl mono-address))]
;;            [else (Blame (Quote "Unexpected Type1 in cast tree"))]))])))

;; ;; How to cast a Guarded Reference to some other type
;; (: make-cast-gbox-code Cast-Type)
;; (define (make-cast-gbox-code v t1 t2 lbl)
;;   (: gref-arg (CoC3-Expr -> CoC3-Expr))
;;   (define (gref-arg t)
;;     (if (Type? t)
;;         (let ([t (Type-type t)])
;;           (if (GRef? t)
;;               (Type (GRef-arg t))
;;               (error 'interpret-casts "unexpected type in gref-arg")))
;;         (Type-GRef-Of t)))
;;   (: proxy-gref (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
;;   (define (proxy-gref val type1 type2 lbl)
;;     (let*$ ([tag_gref (type-tag type2)])
;;       (if$ (op=? tag_gref (Tag 'GRef))
;;            (let*$ ([g1 (gref-arg type1)]
;;                    [g2 (gref-arg type2)])
;;              (Guarded-Proxy val (Twosome g1 g2 lbl)))
;;            (Blame lbl))))
;;   (let*$ ([val v] [type1 t1] [type2 t2] [label lbl])
;;     (if$ (op=? (Type DYN-TYPE) type2)
;;          (dyn-make$ val type1)
;;          (proxy-gref val type1 type2 label))))

;; ;; how to Cast a Guarded Vector to some other type
;; (: make-cast-gvect-code Cast-Type)
;; (define (make-cast-gvect-code v t1 t2 lbl)
;;   ;; Get the vector type argument either by compile time reflection
;;   ;; or generating runtime code.
;;   (: gvect-arg (CoC3-Expr -> CoC3-Expr))
;;   (define (gvect-arg t)
;;     (if (Type? t)
;;         (let ([t (Type-type t)])
;;           (if (GVect? t)
;;               (Type (GVect-arg t))
;;               (error 'interpret-casts "unexpected type in gvect-arg")))
;;         (Type-GVect-Of t)))
;;   ;; Generate the code to create a proxy if the target type is
;;   ;; actually a Gvector
;;   (: proxy-gvect (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
;;   (define (proxy-gvect val type1 type2 lbl)
;;     (let*$ ([tag_gvect (type-tag type2)])
;;       (if$ (op=? tag_gvect (Tag 'GVect))
;;            (let*$ ([g1 (gvect-arg type1)]
;;                    [g2 (gvect-arg type2)])
;;              (Guarded-Proxy val (Twosome g1 g2 lbl)))
;;            (Blame lbl))))
;;   ;; Check to see if we are casting to dyn, if so box the value.
;;   ;; Otherwise either cast to a gvector type or blame the label.
;;   (let*$ ([val v] [type1 t1] [type2 t2] [label lbl])
;;     (if$ (op=? (Type DYN-TYPE) type2)
;;          (dyn-make$ val type1)
;;          (proxy-gvect val type1 type2 label))))

;; ;; How to cast a Monotonic Reference to some other type
;; (: make-cast-mbox-code : 
;;    Cast-With-MAddr-Type
;;    GreatestLowerBound-Type
;;    CopyValueInMonoRef-Type -> Cast-Type)
;; (define ((make-cast-mbox-code cast glbt copy-val-monoref) v t1 t2 lbl)
;;   (let*$ ([val v] [type1 t1] [type2 t2] [tag_mref (type-tag type2)])
;;     (if$ (op=? (Type DYN-TYPE) type2)
;;          (dyn-make$ val type1)
;;          (if$ (op=? tag_mref (Tag 'MRef))
;;               (match val
;;                 [(Var a)
;;                  (let*$ ([t2 (mref-of$ type2)])
;;                    (if$ (dyn?$ t2)
;;                         val
;;                         (let*$ ([t1 (Mbox-rtti-ref val)]
;;                                 [t3 (glbt t1 t2)])
;;                           (if$ (op=? t1 t3)
;;                                val
;;                                (Begin
;;                                  (list
;;                                   (Mbox-rtti-set! val t3))
;;                                  (let*$ ([vv (copy-val-monoref val)]
;;                                          [cv (cast vv t1 t3 (Quote "") val)]
;;                                          [t4 (Mbox-rtti-ref val)])
;;                                    (if$ (op=? t3 t4)
;;                                         (Begin
;;                                           (list (Mbox-val-set! val cv))
;;                                           val)
;;                                         val)))))))]
;;                 [other (error 'interp-cast/mref "unmatched value ~a" other)])
;;               (Blame lbl)))))

;; (: make-cast-mvect-code : 
;;    Cast-With-MAddr-Type
;;    GreatestLowerBound-Type -> Cast-Type)
;; (define ((make-cast-mvect-code cast glbt) v t1 t2 lbl)
;;   (let*$ ([val v] [type1 t1] [type2 t2] [tag_mvect (type-tag type2)])
;;     (if$ (op=? (Type DYN-TYPE) type2)
;;          (dyn-make$ val type1)
;;          (if$ (op=? tag_mvect (Tag 'MVect))
;;               (match val
;;                 [(Var a)
;;                  (let*$ ([t2 (mvect-of$ type2)])
;;                    (if$ (dyn?$ t2)
;;                         val
;;                         (let*$ ([t1 (Mvector-rtti-ref val)]
;;                                 [t3 (glbt t1 t2)])
;;                           (if$ (op=? t1 t3)
;;                                val
;;                                (Begin
;;                                  (list
;;                                   (Mvector-rtti-set! val t3)
;;                                   (let* ([i-u (next-uid! "index")]
;;                                          [i (Var i-u)]
;;                                          [x (next-uid! "_")])
;;                                     (let*$ ([vn (Mvector-length val)])
;;                                       (cond$
;;                                        [(tupleT?$ t3)
;;                                         (let*$ ([n (Type-Tuple-num t3)])
;;                                           (Repeat i-u (Quote 0) vn x UNIT-IMDT
;;                                                   (let*$ ([vi (Mvector-val-ref val i)]
;;                                                           [cvi (Copy-Tuple n vi)])
;;                                                     (Begin
;;                                                       (list
;;                                                        (Mvector-val-set! val i cvi))
;;                                                       (let*$ ([ccvi (cast cvi t1 t3 (Quote "") val)]
;;                                                               [t4 (Mvector-rtti-ref val)])
;;                                                         (if$ (op=? t3 t4)
;;                                                              (Mvector-val-set! val i ccvi)
;;                                                              (Break-Repeat)))))))]
;;                                        [else
;;                                         ;; TODO: checking if t3=t4 is unneeded in this case, prove it!
;;                                         (Repeat i-u (Quote 0) vn x UNIT-IMDT
;;                                                 (let*$ ([vi (Mvector-val-ref val i)]
;;                                                         [cvi (cast vi t1 t3 (Quote "") val)]
;;                                                         [t4 (Mvector-rtti-ref val)])
;;                                                   (if$ (op=? t3 t4)
;;                                                        (Mvector-val-set! val i cvi)
;;                                                        (Break-Repeat))))]))))
;;                                  val)))))]
;;                 [other (error 'interp-cast/mvect "unmatched value ~a" other)])
;;               (Blame lbl)))))

;; ;; How to Cast a Function to some other type
;; (: make-cast-fn-code Cast-Type)
;; (define (make-cast-fn-code v t1 t2 lbl)
;;   (let*$ ([value v] [type1 t1] [type2 t2])
;;     (if$ (op=? (Type DYN-TYPE) type2)
;;          (dyn-make$ value type1)
;;          (let*$ ([tag2 (type-tag type2)])
;;            (if$ (op=? tag2 (Tag 'Fn))
;;                 (let*$ ([type1_arity (type-fn-arity type1)]
;;                         [type2_arity (type-fn-arity type2)])
;;                   (if$ (op=? type1_arity type2_arity)
;;                        (App-Code (Fn-Caster value)
;;                                  (list value t1 type2 lbl))
;;                        (Blame lbl)))
;;                 (Blame lbl))))))

;; (: make-cast-tuple-code :  Uid Cast-With-MAddr-Type -> Cast-Type)
;; (define ((make-cast-tuple-code cast-u interpret-cast) v t1 t2 lbl)
;;   (let*$ ([value v] [type1 t1] [type2 t2] [label lbl])
;;     (if$ (op=? (Type DYN-TYPE) type2)
;;          (dyn-make$ value type1)
;;          ;; Todo Reformat this code
;;          (if #f #;(and (Type? t1) (Type? t2))
;;              (let ([t1t (Type-type t1)]
;;                    [t2t (Type-type t2)])
;;                (if (and (STuple? t1t) (STuple? t2t))
;;                    (let-values ([(bnd* arg*)
;;                                  (for/fold ([b* : CoC3-Bnd* '()]
;;                                             [arg* : Uid* '()])
;;                                            ([i (in-range (STuple-num t2t))])
;;                                    (unless (index? i)
;;                                      (error 'make-cast-tuple-code "bad index"))
;;                                    (let ([a (next-uid! "item_type")])
;;                                      (values (cons (cons a (let*$ ([item (Tuple-proj v (Quote i))]
;;                                                                    [new-t1t (tuple-type-arg$ t1 i)]
;;                                                                    [new-t2t (tuple-type-arg$ t2 i)])
;;                                                              (interpret-cast item new-t1t new-t2t lbl (Quote 0)))) b*)
;;                                              (cons a arg*))))])
;;                      (Let bnd* (Create-tuple (map (inst Var Uid) arg*))))
;;                    (error 'make-cast-tuple-code)))
;;              (let*$ ([tag2 (type-tag type2)])
;;                (if$ (op=? tag2 (Tag 'STuple))
;;                     (let*$ ([type1_num (type-tuple-num type1)]
;;                             [type2_num (type-tuple-num type2)])
;;                       (if$ (op<=? type2_num type1_num)
;;                            (Cast-Tuple cast-u v t1 t2 lbl)
;;                            (Blame type2_num)))
;;                     (Blame lbl)))))))

;; ;; How to extract a dynamic value
;; (: make-cast-dyn-code :  Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
;; (define ((make-cast-dyn-code cast-undyned)  v t1 t2 lbl mono-address)
;;   #| This should be:
;;   (typecase v
;;   [n : t1 (cast-undyned v^ t1^ t2 lbl)])
;;   ==> 
;;   (case v
;;   [(Dynamic v^ t1^) ...])
;;   ==>
;;   (
;;   Efficient Implementation will require remove-let before
;;   specify representation. TODO
;;   For now I am going to implement this
;;   (dyn-destruct v (v^ t1)
;;   (cast-undyned v^ t1^ t2 lbl))
;;   |#
;;   (let*$ ([val v] [tag (dyn-immediate-tag$ val)])
;;     (cond$
;;      [(op=? (Tag 'Int) tag)
;;       (cast-undyned (dyn-immediate-value$ val) (Type INT-TYPE) t2 lbl mono-address)]
;;      [(op=? (Tag 'Bool) tag)
;;       (cast-undyned (dyn-immediate-value$ val) (Type BOOL-TYPE) t2 lbl mono-address)]
;;      [(op=? (Tag 'Unit) tag)
;;       (cast-undyned (Quote '()) (Type UNIT-TYPE) t2 lbl mono-address)]
;;      [(op=? (Tag 'Char) tag)
;;       (cast-undyned (dyn-immediate-value$ val) (Type CHAR-TYPE) t2 lbl mono-address)]
;;      [(op=? (Tag 'Boxed) tag)
;;       (cast-undyned (dyn-value$ val) (dyn-type$ val) t2 lbl mono-address)]
;;      [else (Blame (Quote "Unexpected value in cast tree"))])))

;; ;; How to cast a non dynamic value to another type
;; ;; Notice this is used in conjuction with interpret cast based
;; ;; on the setting of the parameter recursive-dyn-cast
;; ;; when set to #t the cast used is a call to the interpret
;; ;; cast runtime routine instead of unfolding the cast tree even
;; ;; I am pretty sure this is the desired behavior
;; ;; There is an invarient that there will only ever be one level of dyn
;; (: make-cast-undyned-code :  Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
;; (define ((make-cast-undyned-code cast-ground) v t1 t2 l mono-address)
;;   (let*$ ([value v] [type1 t1] [type2 t2] [label l])
;;     ;; If unboxed at the exact type then just return the value
;;     (cond$
;;      [(op=? type1 type2) value]
;;      ;; Otherwise it was either unboxed at the wrong type
;;      ;; or it is an inter structured type cast.
;;      ;; Either way we build a cast tree specific to this type.
;;      [else (cast-ground value type1 type2 label mono-address)])))

;; ;; We should test this version it is much more straight forward and
;; ;; if it is faster that would be great.

;; (: make-cast-code :  Inject-Type Project-Type Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
;; (define ((make-cast-code inject project ho-cast) v t1 t2 lbl mono-address)
;;   (let*$ ([type1 t1] [type2 t2])
;;     (cond$
;;      [(op=? type1 type2) v]
;;      [(op=? type1 (Type DYN-TYPE)) (project v type2 lbl mono-address)]
;;      [(op=? type2 (Type DYN-TYPE)) (inject v type1)]
;;      [else (ho-cast v type1 type2 lbl mono-address)])))

;; (define-type Inject-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
;; (: make-inject-code : Inject-Type)
;; (define (make-inject-code v t)
;;   (dyn-make$ v t))

;; (define-type Project-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
;; (: make-project-code : Cast-With-MAddr-Type -> Project-Type)
;; (define ((make-project-code cast) v t l mono-address)
;;   (cast (dyn-value$ v) (dyn-type$ v) t l mono-address))

;; ;; TODO: why we pass in cast?
;; (: make-higher-order-cast-code :  Uid Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
;; (define ((make-higher-order-cast-code cast-u cast) v t1 t2 l mono-address)
;;   (let*$ ([value v] [type1 t1] [type2 t2] [label l])
;;     (cond$
;;      [(and$ (Type-Fn-Huh type1)
;;             (and$ (Type-Fn-Huh type2)
;;                   (op=? (Type-Fn-arity type1) (Type-Fn-arity type2))))
;;       ;; We could include a check to see if t1 == t2 but it should
;;       ;; have already been done in make cast
;;       (App-Code (Fn-Caster value) (list value type1 type2 label))]
;;      [(and$ (Type-GVect-Huh type1) (Type-GVect-Huh type2))
;;       (Guarded-Proxy value
;;                      (Twosome (Type-GVect-Of type1) (Type-GVect-Of type2) label))]
;;      [(and$ (Type-GRef-Huh type1) (Type-GRef-Huh type2))
;;       (Guarded-Proxy value
;;                      (Twosome (Type-GRef-Of type1) (Type-GRef-Of type2) label))]
;;      [(and$ (Type-MRef-Huh type1) (Type-MRef-Huh type2))
;;       (error "support mrefs in the simple interpreter")]
;;      [(and$ (Type-MVect-Huh type1) (Type-MVect-Huh type2))
;;       (error "support mvects in the simple interpreter")]
;;      [(and$ (Type-Tuple-Huh type1)
;;             (and$ (Type-Tuple-Huh type2)
;;                   (op<=? (type-tuple-num type2)
;;                          (type-tuple-num type1))))
;;       (If (Op '= (list (Quote 0) mono-address))
;;           (Cast-Tuple cast-u value type1 type2 label)
;;           (Cast-Tuple-In-Place cast-u value type1 type2 l mono-address))]
;;      [else (Blame label)])))

;; ;; ic-expr maps over the expressions lowering function cast
;; ;; into calls to the runtime cast interpreter and direct manipulation
;; ;; of the representation for each type and value.
;; (: interpret-casts-in-expr :
   
;;    Cast-With-MAddr-Type
;;    (Index -> Uid)
;;    GBox-Ref-Type GBox-Set-Type GVec-Ref-Type GVec-Set-Type
;;    GVec-Length-Type
;;    Mbox-refT Mbox-setT Mvec-refT Mvec-setT
;;    Dyn-GBox-Ref-Type Dyn-GBox-Set-Type
;;    Dyn-GVec-Ref-Type Dyn-GVec-Set-Type
;;    Dyn-Mbox-refT Dyn-Mbox-setT
;;    Dyn-Mvec-refT Dyn-Mvec-setT
;;    Dyn-Fn-App-Type
;;    C0-Expr
;;    ->
;;    CoC3-Expr)
;; (define (interpret-casts-in-expr
;;          interp-cast 
;;          get-fn-cast!
;;          gbox-ref gbox-set! gvect-ref gvect-set!
;;          gvect-length
;;          mbox-ref mbox-set! mvect-ref mvect-set!
;;          dyn-gbox-ref dyn-gbox-set!
;;          dyn-gvec-ref dyn-gvec-set!
;;          dyn-mbox-ref dyn-mbox-set!
;;          dyn-mvec-ref dyn-mvec-set!
;;          dyn-fn-app
;;          exp)

;;   ;; map through a data binding 
;;   (: recur-through-bnd* : C0-Bnd -> CoC3-Bnd)
;;   (define (recur-through-bnd* b)
;;     (cons (car b) (recur (cdr b))))

;;   ;; map through a list of expressions
;;   (: recur* : C0-Expr* -> CoC3-Expr*)
;;   (define (recur* exps) (map recur exps))
  
;;   (: recur : C0-Expr -> CoC3-Expr)
;;   (define (recur exp)
;;     ;;(printf "ic: ~a\n" exp) (flush-output (current-output-port))
;;     (match exp
;;       ;; Interesting Cases -----------------------------------------------
;;       ;; Transformations for Casting runtime
;;       [(Cast (app recur e) (Twosome t1 t2 l))
;;        (match* (t1 t2)
;;          [((Fn a _ _) (Fn a _ _)) #:when (direct-fn-cast-optimization?)
;;           (App-Code (Code-Label (get-fn-cast! a)) (list e (Type t1) (Type t2) (Quote l)))]
;;          [(t1 t2) (interp-cast e (Type t1) (Type t2) (Quote l) (Quote 0))])]
;;       [(Lambda f* (app recur exp))
;;        (let ([caster (get-fn-cast! (length f*))])
;;          (Lambda f* (Castable caster exp)))]
;;       [(App (app recur exp) (app recur* exp*))
;;        (build-apply/type-based-cast exp exp*)]
;;       ;; Transformation to lower guarded reference types
;;       ;;-------------------------------------------------------------------
;;       ;; Every Guarded Reference Starts As an Unguarded box
;;       [(Gbox (app recur e)) (Unguarded-Box e)]
;;       ;; Unboxing calls off to the helpers we have defined
;;       [(Gunbox (app recur b))
;;        (if (Var? b)
;;            (gbox-ref b)
;;            (let ([u (next-uid! "gbox")])
;;              (Let (list (cons u b)) (gbox-ref (Var u)))))]
;;       ;; Setting a Gaurded reference results in iteratively applying
;;       ;; all the guarding casts to the value to be written.
;;       ;; Most of the work is already done but the code requires values
;;       ;; so there is a little repetative to let bind any values that
;;       ;; haven't been evaluated.
;;       [(Gbox-set! (app recur b) (app recur w))
;;        (match-define-values (b* (list b^ w^))
;;          (bnd-non-vars next-uid! (list b w) #:names '("gbox" "write_val")))
;;        (if (null? b*)
;;            (gbox-set! b^ w^)
;;            (Let b* (gbox-set! b^ w^)))]
;;       [(Gvector (app recur size) (app recur init)) (Unguarded-Vect size init)]
;;       [(Gvector-ref (app recur v) (app recur i))
;;        (match-define-values (b* (list v^ i^))
;;          (bnd-non-vars next-uid! (list v i) #:names '("gvec" "index")))
;;        (cond
;;          [(null? b*) (gvect-ref v^ i^)]
;;          [else (Let b* (gvect-ref v^ i^))])]
;;       [(Gvector-set! (app recur v) (app recur i) (app recur w))
;;        (match-define-values (b* (list v^ i^ w^))
;;          (bnd-non-vars next-uid! (list v i w)
;;                        #:names '("gvec" "index" "write_val")))
;;        (if (null? b*)
;;            (gvect-set! v^ i^ w^)
;;            (Let b* (gvect-set! v^ i^ w^)))]
;;       [(Gvector-length e)
;;        (if (Var? e)
;;            (gvect-length e)
;;            (let ([u (next-uid! "gvect")])
;;              (Let (list (cons u (recur e))) (gvect-length (Var u)))))]
;;       [(Mbox (app recur e) t) (Mbox e t)]
;;       [(Munbox (app recur e)) (Mbox-val-ref e)]
;;       [(Mbox-set! (app recur e1) (app recur e2))
;;        (Mbox-val-set! e1 e2)]
;;       [(MBoxCastedRef addr t)
;;        (match-define-values (b* (list t^))
;;          (bnd-non-vars next-uid! (list (Type t)) #:names '("type")))
;;        (if (null? b*)
;;            (mbox-ref (Var addr) t^)
;;            (Let b* (mbox-ref (Var addr) t^)))]
;;       [(MBoxCastedSet! addr (app recur e) t)
;;        (match-define-values (b* (list e^ t^))
;;          (bnd-non-vars next-uid! (list e (Type t)) #:names '("write_val" "type")))
;;        (if (null? b*)
;;            (mbox-set! (Var addr) e^ t^)
;;            (Let b* (mbox-set! (Var addr) e^ t^)))]
;;       [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
;;       [(Mvector-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
;;       [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
;;        (Mvector-val-set! e1 e2 e3)]
;;       [(MVectCastedRef addr (app recur i) t)
;;        (match-define-values (b* (list i^ t^))
;;          (bnd-non-vars next-uid! (list i (Type t)) #:names '("index" "type")))
;;        (if (null? b*)
;;            (mvect-ref (Var addr) i^ t^)
;;            (Let b* (mvect-ref (Var addr) i^ t^)))]
;;       [(MVectCastedSet! addr (app recur i) (app recur e) t)
;;        (match-define-values (b* (list i^ e^ t^))
;;          (bnd-non-vars next-uid! (list i e (Type t)) #:names '("index" "write_val" "type")))
;;        (if (null? b*)
;;            (mvect-set! (Var addr) i^ e^ t^)
;;            (Let b* (mvect-set! (Var addr) i^ e^ t^)))]
;;       [(Mvector-length e) (Mvector-length (recur e))]
      
;;       ;; The translation of the dynamic operation 
;;       [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
;;        (define arg-names
;;          `("blame_info" "dyn_fn" . ,(make-list (length e*) "dyn_fn_arg")))
;;        (match-define-values (b* (cons lbl (cons v v*)))
;;          #;(bnd-non-vars next-uid! (cons e (cons l e*)) #:names arg-names)
;;          (bnd-non-vars next-uid! (cons (Quote l) (cons e e*)) #:names arg-names))
;;        #;(printf "b*=~a\nlbl=~a\nv=~a\nv*=~a\n\n" b* lbl v v*)
;;        (define r (Let b* (dyn-fn-app v v* t* lbl)))
;;        #;(printf "r=~a" r)
;;        r]
;;       [(Dyn-Tuple-Proj (app recur e) (app recur i) (app recur l))
;;        (let$ ([v e] [i i] [l l])
;;          (let$ ([u  (ann (dyn-value$ v) CoC3-Expr)]
;;                 [ty (ann (dyn-type$ v) CoC3-Expr)])
;;             (cond$
;;              [(ann (and$ (Type-Tuple-Huh ty) (Op '> (list (Type-Tuple-num ty) i)))
;;                    CoC3-Expr)
;;               (let$ ([prj-val (ann (Tuple-proj u i) CoC3-Expr)]
;;                      [prj-ty  (ann (Type-Tuple-item ty i) CoC3-Expr)])
;;                 (ann (interp-cast prj-val prj-ty DYN-EXPR l (Quote 0)) CoC3-Expr))]
;;              [else (Blame l)])))       ]
;;       [(Dyn-GRef-Ref (app recur e) l)
;;        (match-define-values (b* (list e^ l^))
;;          (bnd-non-vars next-uid! (list e (Quote l))
;;                        #:names (list "dyn_gbox" "blame_info")))
;;        (Let b* (dyn-gbox-ref e^ l^))]
;;       [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
;;        (match-define-values (b* (list e1^ e2^ l^))
;;          (bnd-non-vars next-uid! (list e1 e2 (Quote l))
;;                        #:names (list "dyn_gbox" "write_val" "blame_info")))
;;        (Let b* (dyn-gbox-set! e1^ e2^ (Type t) l^))]
;;       [(Dyn-GVector-Ref (app recur e) (app recur i) l)
;;        (match-define-values (b* (list e^ i^ l^))
;;          (bnd-non-vars next-uid! (list e i (Quote l))
;;                        #:names (list "dyn_gvec" "index" "blame_info")))
;;        (define r (Let b* (dyn-gvec-ref e^ i^ l^)))
;;        #;(printf "r=~a\n\n" r)
;;        r]
;;       [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
;;        (match-define-values (b* (list e1^ i^ e2^ l^))
;;          (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
;;                        #:names (list "dyn_gvec" "index" "write_val" "blame_info")))
;;        (Let b* (dyn-gvec-set! e1^ i^ e2^ (Type t) l^))]
;;       [(Dyn-GVector-Len (app recur e) (app recur l))
;;        (let*$ ([v e] [l l])
;;          (let*$ ([u (dyn-value$ v)]
;;                  [t (dyn-type$ v)])
;;            (If (Type-GVect-Huh t)
;;                (gvect-length u)
;;                (Blame l))))]
;;       [(Dyn-MRef-Ref (app recur e) l)
;;        (match-define-values (b* (list e^ l^))
;;          (bnd-non-vars next-uid! (list e (Quote l))
;;                        #:names (list "dyn_mbox" "blame_info")))
;;        (Let b* (dyn-mbox-ref e^ l^))]
;;       [(Dyn-MRef-Set! (app recur e1) (app recur e2) t l)
;;        (match-define-values (b* (list e1^ e2^ l^))
;;          (bnd-non-vars next-uid! (list e1 e2 (Quote l))
;;                        #:names (list "dyn_mbox" "write_val" "blame_info")))
;;        (Let b* (dyn-mbox-set! e1^ e2^ (Type t) l^))]
;;       [(Dyn-MVector-Ref (app recur e) (app recur i) l)
;;        (match-define-values (b* (list e^ i^ l^))
;;          (bnd-non-vars next-uid! (list e i (Quote l))
;;                        #:names (list "dyn_mvec" "index" "blame_info")))
;;        (Let b* (dyn-mvec-ref e^ i^ l^))]
;;       [(Dyn-MVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
;;        (match-define-values (b* (list e1^ i^ e2^ l^))
;;          (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
;;                        #:names (list "dyn_mvec" "index" "write_val" "blame_info")))
;;        (Let b* (dyn-mvec-set! e1^ i^ e2^ (Type t) l^))]
;;       ;; We should consider desugaring the data structure rep
;;       ;; here if we are using it. That translation makes more
;;       ;; sense but is weird because we defer hybrid until
;;       ;; closure conversion. 
;;       ;; If we aim to support a data representation of casts
;;       ;; then we will need to add these back
;;       ;; [(Fn-Proxy i e1 e2)
;;       ;;  (Fn-Proxy (list i interp-uid) (recur e1) (recur e2))]
;;       ;; [(App/Fn-Proxy-Huh e e*)
;;       ;;  (App-Fn-or-Proxy interp-uid (recur e) (map recur e*))]
;;       ;; [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (recur e))]
;;       ;; [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (recur e))]
;;       ;; In theory it may be better to implement the tranformation
;;       ;; for apply here but it is currently postponed until
;;       ;; closure conversion.
;;       [(Observe e t) (Observe (recur e) t)]
;;       [(and noop (No-Op)) noop]
;;       [(Code-Label u)
;;        (Code-Label u)]
;;       [(Lambda f* (Castable ctr e))
;;        (Lambda f* (Castable ctr (recur e)))]
;;       [(Letrec b* e)
;;        (Letrec (map recur-through-bnd* b*) (recur e))]
;;       [(Let b* e)
;;        (Let (map recur-through-bnd* b*) (recur e))]
;;       [(Op p e*)
;;        (Op p (map recur e*))]
;;       [(If tst csq alt)
;;        (If (recur tst) (recur csq) (recur alt))]
;;       [(Switch e c* d)
;;        (: recur-case : (Switch-Case C0-Expr) -> (Switch-Case CoC3-Expr))
;;        (define/match (recur-case c)
;;          [((cons l r)) (cons l (recur r))])
;;        (Switch (recur e) (map recur-case c*) (recur d))]
;;       [(Var i) (Var i)]
;;       [(Type t) (Type t)]
;;       [(Quote k) (Quote k)]
;;       [(Begin e* e)
;;        (Begin (map recur e*) (recur e))]
;;       [(Repeat i e1 e2 a e3 e4)
;;        (Repeat i (recur e1) (recur e2) a (recur e3) (recur e4))]
;;       ;; Proxies for functions
;;       [(Fn-Caster e)
;;        (Fn-Caster (recur e))]
;;       [(Create-tuple e*)
;;        (Create-tuple (map recur e*))]
;;       [(Tuple-proj e i) (Tuple-proj (recur e) (Quote i))]
;;       [other (error 'interpret-casts "umatched ~a" other)]))
;;   (recur exp))






;; ;;--------------------------------------------------------------------

;; (define-type Mbox-refT ((Var Uid) (Var Uid) -> CoC3-Expr))
;; (define-type Mbox-setT ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; (define-type Mvec-refT ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; (define-type Mvec-setT ((Var Uid) (Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

;; (: make-mbox-ref-code ( Cast-With-MAddr-Type -> Mbox-refT))
;; (define ((make-mbox-ref-code cast) mref t2)
;;   (let ([t1 (next-uid! "t1")])
;;     (Let (list (cons t1 (Mbox-rtti-ref mref)))
;;       (cast (Mbox-val-ref mref) (Var t1) t2 (Quote "") (Quote 0)))))

;; (: make-mbox-set!-code ( Cast-With-MAddr-Type -> Mbox-setT))
;; (define ((make-mbox-set!-code cast) mref val t1)
;;   (let*$ ([t2 (Mbox-rtti-ref mref)]
;;           [cv (cond$
;;                [(and$ (tupleT?$ t1) (tupleT?$ t2))
;;                 (let*$ ([n (Type-Tuple-num t2)]
;;                         [ctv (Copy-Tuple n val)])
;;                   (Begin
;;                     (list
;;                      (Mbox-val-set! mref ctv))
;;                     ctv))]
;;                [else val])]
;;           [ccv (cast cv t1 t2 (Quote "") mref)]
;;           [t2-new (Mbox-rtti-ref mref)])
;;     (begin
;;       (if$ (op=? t2 t2-new)
;;            (Mbox-val-set! mref ccv)
;;            (Quote 0)))))

;; (: make-mvec-ref-code ( Cast-With-MAddr-Type -> Mvec-refT))
;; (define ((make-mvec-ref-code cast) mvect i t2)
;;   (let ([t1 (next-uid! "t1")])
;;     (Let (list (cons t1 (Mvector-rtti-ref mvect)))
;;       (cast (Mvector-val-ref mvect i) (Var t1) t2 (Quote "") (Quote 0)))))

;; (: make-mvec-set!-code ( Cast-With-MAddr-Type -> Mvec-setT))
;; (define ((make-mvec-set!-code cast) mvect i val t1)
;;   (let*$ ([t2 (Mvector-rtti-ref mvect)])
;;     (cond$
;;      [(and$ (tupleT?$ t1) (tupleT?$ t2))
;;       (let*$ ([n (Type-Tuple-num t2)]
;;               [cvi (Copy-Tuple n val)])
;;         (Begin
;;           (list
;;            (Mvector-val-set! mvect i cvi))
;;           (let*$ ([ccvi (cast cvi t1 t2 (Quote "") mvect)]
;;                   [t2-new (Mvector-rtti-ref mvect)])
;;             (if$ (op=? t2 t2-new)
;;                  (Mvector-val-set! mvect i ccvi)
;;                  (Quote 0)))))]
;;      [else
;;       (let*$ ([cvi (cast val t1 t2 (Quote "") mvect)]
;;               [t2-new (Mvector-rtti-ref mvect)])
;;         (if$ (op=? t2 t2-new)
;;              (Mvector-val-set! mvect i cvi)
;;              (Quote 0)))])))

;; (define-type Dyn-GBox-Ref-Type
;;   ((Var Uid) (Var Uid) -> CoC3-Expr))
;; (define-type Dyn-GBox-Set-Type
;;   ((Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
;; (define-type Dyn-GVec-Ref-Type
;;   ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; (define-type Dyn-GVec-Set-Type
;;   ((Var Uid) (Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid)
;;    -> CoC3-Expr))
;; (define-type Dyn-Fn-App-Type
;;   ((Var Uid) (Listof (Var Uid)) Schml-Type* (Var Uid) -> CoC3-Expr))

;; (: make-dyn-gbox-ref-code :
   
;;    GBox-Ref-Type
;;    Cast-With-MAddr-Type
;;    -> Dyn-GBox-Ref-Type)
;; (define ((make-dyn-gbox-ref-code gbox-ref cast) dyn lbl)
;;   (define-values (val ty tyof read-val)
;;     (values (next-uid! "dyn_unbox_val")
;;             (next-uid! "dyn_unbox_ty")
;;             (next-uid! "dyn_unbox_tyof")
;;             (next-uid! "dyn_unbox_read_val")))
;;   (define-values (var-val var-ty var-tyof var-read-val)
;;     (values (Var val) (Var ty) (Var tyof) (Var read-val)))
;;   (Let `((,val . ,(dyn-value$ dyn))
;;          (,ty . ,(dyn-type$ dyn)))
;;     (If (Type-GRef-Huh var-ty)
;;         (Let `([,tyof . ,(Type-GRef-Of var-ty)]
;;                [,read-val . ,(gbox-ref var-val)])
;;           (cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
;;         (Blame lbl))))

;; (: make-dyn-gbox-set!-code :
   
;;    GBox-Set-Type
;;    Cast-With-MAddr-Type
;;    -> Dyn-GBox-Set-Type)
;; (define ((make-dyn-gbox-set!-code gb-set! cast) dyn-gbox wrt-val1 t2 info)
;;   (define-values (gbox ty tyof wrt-val2 wrt-val3)
;;     (values (next-uid! "dyn_setbox_gbox")
;;             (next-uid! "dyn_setbox_ty")
;;             (next-uid! "dyn_setbox_tyof")
;;             (next-uid! "dyn_setbox_wrt_val2")
;;             (next-uid! "dyn_setbox_wrt_val3")))
;;   (define-values (gbox-v ty-v tyof-v wrt-val2-v wrt-val3-v)
;;     (values (Var gbox) (Var ty) (Var tyof) (Var wrt-val2) (Var wrt-val3)))
;;   (Let `((,gbox . ,(dyn-value$ dyn-gbox))
;;          (,ty   . ,(dyn-type$ dyn-gbox)))
;;     (If (Type-GRef-Huh ty-v)
;;         (Let `([,tyof . ,(Type-GRef-Of ty-v)])
;;           (Let `([,wrt-val3 . ,(cast wrt-val1 t2 tyof-v info (Quote 0))])
;;             (cast (gb-set! gbox-v wrt-val3-v) tyof-v (Type DYN-TYPE) info (Quote 0))))
;;         (Blame info))))

;; (: make-dyn-gvect-ref-code :
;;      GVec-Ref-Type Cast-With-MAddr-Type
;;    -> Dyn-GVec-Ref-Type)
;; (define ((make-dyn-gvect-ref-code gv-ref cast) dyn ind lbl)
;;   (define-values (val ty tyof read-val)
;;     (values (next-uid! "dyn_gvec_ref_val")
;;             (next-uid! "dyn_gvec_ref_ty")
;;             (next-uid! "dyn_gvec_ref_tyof")
;;             (next-uid! "dyn_gvec_ref_read_val")))
;;   (define-values (var-val var-ty var-tyof var-read-val)
;;     (values (Var val) (Var ty) (Var tyof) (Var read-val)))
;;   (Let `((,val . ,(dyn-value$ dyn))
;;          (,ty . ,(dyn-type$ dyn)))
;;     (If (Type-GVect-Huh var-ty)
;;         (Let `([,tyof . ,(Type-GVect-Of var-ty)]
;;                [,read-val . ,(gv-ref var-val ind)])
;;           (cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
;;         (Blame lbl))))

;; (: make-dyn-gvect-set!-code :
;;    GVec-Set-Type
;;    Cast-With-MAddr-Type
;;    -> Dyn-GVec-Set-Type)
;; (define ((make-dyn-gvect-set!-code gv-set! cast) dyn-gvec ind wrt-val1 t2 info)
;;   (let$ ([v dyn-gvec][ind ind] [wrt-val1 wrt-val1] [t2 t2] [l info])
;;     (let*$ ([u (dyn-value$ v)]
;;             [t1 (dyn-type$ v)])
;;       (If (Type-GVect-Huh t1)
;;           (let*$ ([tyof (Type-GVect-Of t1)]
;;                   [wrt-val3 (cast wrt-val1 t2 tyof l (Quote 0))])
;;             (cast (gv-set! u ind wrt-val3) tyof (Type DYN-TYPE) l (Quote 0)))
;;           (Blame l)))))

;; (define-type Dyn-Mbox-refT
;;   ((Var Uid) (Var Uid) -> CoC3-Expr))
;; (define-type Dyn-Mbox-setT
;;   ((Var Uid) (Var Uid)
;;    (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
;; (define-type Dyn-Mvec-refT
;;   ((Var Uid) (Var Uid) (Var Uid)
;;    -> CoC3-Expr))
;; (define-type Dyn-Mvec-setT
;;   ((Var Uid) (Var Uid) (Var Uid)
;;    (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))

;; (: make-dyn-mbox-ref-code :
   
;;    Mbox-refT
;;    -> Dyn-Mbox-refT)
;; (define ((make-dyn-mbox-ref-code mb-ref) dyn lbl)
;;   (define-values (val ty dynty)
;;     (values (next-uid! "dyn_unbox_val")
;;             (next-uid! "dyn_unbox_ty")
;;             (next-uid! "dyn_type")))
;;   (define-values (var-val var-ty var-dynty)
;;     (values (Var val) (Var ty) (Var dynty)))
;;   (Let `((,val . ,(dyn-value$ dyn))
;;          (,ty . ,(dyn-type$ dyn)))
;;     (If (Type-MRef-Huh var-ty)
;;         (Let `([,dynty . ,(Type DYN-TYPE)])
;;           (mb-ref var-val var-dynty))
;;         (Blame lbl))))

;; (: make-dyn-mbox-set!-code :
   
;;    Mbox-setT
;;    Cast-With-MAddr-Type
;;    -> Dyn-Mbox-setT)
;; (define ((make-dyn-mbox-set!-code mb-set! $cast) dyn-mbox wrt-val1 t2 info)
;;   (define-values (mbox ty tyof t2u)
;;     (values (next-uid! "dyn_setbox_mbox")
;;             (next-uid! "dyn_setbox_ty")
;;             (next-uid! "dyn_setbox_tyof")
;;             (next-uid! "t2_type")))
;;   (define-values (mbox-v ty-v tyof-v t2u-v)
;;     (values (Var mbox) (Var ty) (Var tyof) (Var t2u)))
;;   (Let `([,mbox . ,(dyn-value$ dyn-mbox)]
;;          [,ty   . ,(dyn-type$ dyn-mbox)])
;;     (If (Type-MRef-Huh ty-v)
;;         (Let `([,tyof . ,(Type-MRef-Of ty-v)]
;;                [,t2u .  ,t2])
;;           (If (Type-Dyn-Huh tyof-v)
;;               ($cast (mb-set! mbox-v wrt-val1 t2u-v)
;;                      (Type UNIT-TYPE) (Type DYN-TYPE) info
;;                      (Quote 0))
;;               (mb-set! mbox-v wrt-val1 t2u-v)))
;;         (Blame info))))

;; (: make-dyn-mvect-ref-code :
;;      Mvec-refT
;;    -> Dyn-Mvec-refT)
;; (define ((make-dyn-mvect-ref-code mv-ref) dyn ind lbl)
;;   (define-values (val ty dynty)
;;     (values (next-uid! "dyn_mvec_ref_val")
;;             (next-uid! "dyn_mvec_ref_ty")
;;             (next-uid! "dyn_type")))
;;   (define-values (var-val var-ty var-dynty)
;;     (values (Var val) (Var ty) (Var dynty)))
;;   (Let `((,val . ,(dyn-value$ dyn))
;;          (,ty . ,(dyn-type$ dyn)))
;;     (If (Type-MVect-Huh var-ty)
;;         (Let `([,dynty . ,(Type DYN-TYPE)])
;;           (mv-ref var-val ind var-dynty))
;;         (Blame lbl))))

;; (: make-dyn-mvect-set!-code :
   
;;    Mvec-setT
;;    Cast-With-MAddr-Type
;;    -> Dyn-Mvec-setT)
;; (define ((make-dyn-mvect-set!-code mv-set! $cast)
;;          dyn-mvec ind wrt-val1 t2 info)
;;   (define-values (val ty tyof t2u)
;;     (values (next-uid! "dyn_setbox_mvect_value")
;;             (next-uid! "dyn_setbox_ty")
;;             (next-uid! "dyn_setbox_tyof")
;;             (next-uid! "t2_type")))
;;   (define-values (val-v ty-v tyof-v t2u-v)
;;     (values (Var val) (Var ty) (Var tyof) (Var t2u)))
;;   (Let `((,val . ,(dyn-value$ dyn-mvec))
;;          (,ty . ,(dyn-type$ dyn-mvec)))
;;     (If (Type-MVect-Huh ty-v)
;;         (Let `([,tyof . ,(Type-MVect-Of ty-v)]
;;                [,t2u .  ,t2])
;;           (If (Type-Dyn-Huh tyof-v)
;;               ($cast (mv-set! val-v ind wrt-val1 t2u-v)
;;                      (Type UNIT-TYPE) (Type DYN-TYPE) info (Quote 0))
;;               (mv-set! val-v ind wrt-val1 t2u-v)))
;;         (Blame info))))


;; (: make-dyn-fn-app-code : Cast-With-MAddr-Type -> Dyn-Fn-App-Type)
;; (define ((make-dyn-fn-app-code cast) v v* t* l)
;;   (define-values (val ty ret-val ret-ty arity)
;;     (values (next-uid! "dyn_fn_val")
;;             (next-uid! "dyn_fn_ty")
;;             (next-uid! "dyn_fn_ret_val")
;;             (next-uid! "dyn_fn_ret_ty")
;;             (length v*)))
;;   (define arg-casts : CoC3-Expr*
;;     (for/list : (Listof CoC3-Expr)
;;               ([v : CoC3-Expr v*]
;;                [t : Schml-Type t*]
;;                [i (in-naturals)])
;;       (define tyi (next-uid! (string-append "dyn_fn_ty" (number->string i))))
;;       (Let `([,tyi . ,(Type-Fn-arg (Var ty) (Quote i))])
;;         (cast v (Type t) (Var tyi) l (Quote 0)))))
;;   (define casts-apply : CoC3-Expr
;;     (case (function-cast-representation)
;;       [(Hybrid) (App-Fn (Var val) arg-casts)]
;;       [(Data) (error 'todo "implement coercions data representation")]
;;       [(Functional) (error 'todo "incompatible with coercions")]
;;       [else (error 'interp-cast-with-coercions/dyn-fn-app "unexpected value")]))
;;   (Let `([,val . ,(dyn-value$ v)]
;;          [,ty . ,(dyn-type$ v)])
;;     (If (If (Type-Fn-Huh (Var ty))
;;             (Op '= (list (Type-Fn-arity (Var ty)) (Quote arity)))
;;             (Quote #f))
;;         (Let `([,ret-val . ,casts-apply]
;;                [,ret-ty . ,(Type-Fn-return (Var ty))])
;;           (cast (Var ret-val) (Var ret-ty) (Type DYN-TYPE) l (Quote 0)))
;;         (Blame l))))

;; ;; TODO (andre) consider this tranform instead
;; ;; (dyn-app f (42) (Int) L1)
;; ;; =>
;; ;; (let ([v (dyn-value f)] [t (dyn-type f)])
;; ;;   (if (eq? t (Int -> Dyn))
;; ;;       (v 42)
;; ;;       (if (and (fn-type? t) (= (fn-type-arity t) 1))
;; ;;           (let ([a (cast 42 Int (fn-type-arg t 0) L1)])
;; ;;             (cast (v t) (fn-type-return t) L1))
;; ;;           (blame L1))))



;; #|------------------------------------------------------------------------------+
;; |Pass: src/casts/lower-function-casts                                           |
;; +-------------------------------------------------------------------------------+
;; |Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
;; +-------------------------------------------------------------------------------+
;;  Description:
;;  This pass adds the castable form which extends all lambdas that
;;  are castable with a free variable which represents the function that is
;;  necessary in order to cast the closure created by this lambda. It can now be
;;  extracted from a closure value using the fn-cast form. Note that lambdas
;;  created in the process of creating these casting functions are not castable.
;;  Though their return values are.

;;  Casts of the form (runtime-cast value-exp type1-exp type2-exp label-exp) are
;;  used in each of the higher order casting functions because the values needed
;;  to perform the cast are not available until runtime.

;;  This pass also substitutes all casts of the form (cast v Fn1 Fn2 l) with the
;;  equivalent form (fn-cast v Fn1 Fn2 l), in order to show that the cast specific
;;  to the closure value is being used.

;;  Furthermore all types not found in casts are removed.

;; Notes on the move to supporting coercions

;; (Coerce coercion E)  ==  E <coercion> in the literature

;; What are function coercions and how do we make them hybrid?

;; (Fun-Proxy? E)

;; (Let ([a (Coerce (Proxy-Fn (Inject Int) (Project Int "foo")) (Lambda (x) x))])
;;  (a 1))

;; == (Fn (Inject Int) (Project Int "foo")) ;; (Fn (Project Int "foo2") (Inject Int))
;; --> apply_coercion (Fn (Identity Dyn) (Indentity Dyn)) (lambda (x) x)

;; (Let ([tmp (lambda (x) x)]
;;       [a (lambda (x) (Coerce (Project Int "foo") (tmp (Inject Int))))]
;;   (a 1)))


;; But this isn't composable in a space efficent manner for instance
;; (Let ([tmp (lambda (x) x)]
;;       [olda   (lambda (x) (Coerce (Project Int "foo") (tmp (Inject Int))))]
;;       [a    (Coerce (Proxy-Fn (Project Int "foo2") (Inject Int)) olda)]
;;   (a 1)))

;; ==> should step to
;; (Let ([tmp (lambda (x) x)]
;;       [olda ?????]
;;       [a   tmp])
;;   (a 1))

;; ;; So in this example every call to dynamic gets the original closure
;; ;; and every call to static gets a closure that is proxying with only
;; ;; a single layer of indirection.

;; ;; source
;; (letrec ((dynamic : ((Dyn -> Dyn) -> Unit)
;;           (lambda (f) (static f)))
;;          (static : ((Int -> Int) -> Unit)
;;           (lambda (f) (dynamic f))))
;;   (dynamic (lambda (x) x)))

;; ==> Insert coercions

;; (letrec ((dynamic : ((Dyn -> Dyn) -> Unit)
;;           (lambda (f)
;;             (dynamic
;;              (coerce (Fn (Inject Int) (Project Int "l1")) f))))
;;          (static : ((Int -> Int) -> Unit)
;;           (lambda (f)
;;             (dynamic
;;              (coerce (Fn (Project Int "l2") (Inject Int)) f)))))
;;   (dynamic (lambda (x) x)))

;; ==> w/hybrid proxies

;; (label ([coerce-fun1 ;; (Coercion Closure -> Closure)
;;          (code (crc fun)
;;           (if (not (hybrid-proxy? fun)) 
;;               (proxy-closure crcn fun)
;;               (let ([crcn^ (rt-compose-coercion crcn (hybrid-proxy-coercion fun))]
;;                     [fun^  (hybrid-proxy-closure fun)])
;;                     (if (identity-coercion? crcn)
;;                         fun^
;;                         ;; might need to handle failed coercion here also ;
;;                         (hybrid-proxy crcn fun^))) ))])
;;  (letrec ((dynamic : ((Dyn -> Dyn) -> Unit)
;;            (lambda (f)
;;              (static
;;               (coerce-fun1 (Fn (Inject Int) (Project Int "l1")) f))))
;;           (static : ((Int -> Int) -> Unit)
;;            (lambda (f)
;;              (dynamic
;;               (coerce-fun1 (Fn (Project Int "l2") (Inject Int)) f)))))
;;   (dynamic (lambda (x) x))))

;; ;;;;;;; extra code needed for hybrid-proxy

;; (labels ([;; the code pointer for a hybrid-proxy-closure
;;           coerce-and-apply1 ;; ((Ptr -> Ptr) arg -> Ptr)
;;           (code (self arg0)
;;            (let ([crc (hybrid-proxy-coercion self)]
;;                  [fun (hybrid-proxy-closure  self)])
;;              (interp-coerce (Proxy-Fn-return crc)
;;                             (fun (interp-coerce (Proxy-Fn-argument 0 crc)
;;                                                 arg0)))))]
;;          [interp-coerce    ;; (Coecion Ptr -> Ptr)
;;           (code (crc val)
;;            (cond
;;              [(Identity? crc) val]
;;              [(Inject? crc) #| make dynamic value |#]
;;              [(Project? crc)
;;               #|-- extract from dynamic
;;                 -- make new coercion between extracted type and projected type|#]
;;              [(Proxy-Fn? crc) #|dispatch to correct arrity of coerce-fun |#]))]))

;; ;;;;;;;; representation of hybrid proxy closures
;; (hybrid-proxy coercion fun) ==> (tag #b001 #(coerce-apply-coerce coercion fun))
;; (hybrid-proxy? closure) ==> (= #b001 (get-tag closure))
;; (hybrid-proxy-closure  proxy) ==> (array-ref (untag proxy) 2)
;; (hybrid-proxy-coercion proxy) ==> (array-ref (untag proxy) 1)
;; (closure-code closure)        ==> (array-ref closure 0)
;; (general-apply f a ...) ==> (closure-apply (untag c) a ...)
;; (closure-apply f a ...) ==> (let ([c f])
;;                               (label-apply (closure-code c) c a ...))

;; ;; Invarients accessors must only be called on hybrid proxy
;; ;; Hybrid-proxies and only have root level failed or function coercions

;; ;; T! & Id_?

;; ;; Space efficient coercions in lazy ud
;; s, t ::= Id_? | (G? ; i) | i
;; i    ::= (g ; G!) | g | Fail
;; g    ::= Id_i | (s -> t)


;; ;; Values
;; u :: k | λx. e
;; v :: (u : c)

;; ;; Expressions
;; e ::= (s e) | (e e) | v |  

;; ;; (Space efficient ?) coercions in lazy-d
;; c    ::= T?  | g
;; g    ::= T!  | (s -> t)   ;; Coercions that will appear in the source code
;; s, t ::= T?ˡ | (T? ; i) | i | T! ;; Coecions that may result from composition
;; i    ::= (r ; T!) | r | ⊥ˡ 
;; r    ::= Id_t | (s -> t)

;; ;; Evaluation
;; E[c u] -> (u : c)
;; E[c (u : g)] -> E[(u : g & c)]
;; E[((λx.e : (s -> t)) v)] -> t (λx.e (s v))


;; <v c> -> v
;; <u c> -> (u : c)
;; <u ⊥ˡ> -> (raise l)
;; <u : T!> -> (u : T!)
;; <u : (s -> t)> -> (u : (s -> t))
;; <(u : g) c> -> <u (c & g)>

;; compose source level coercion
;; (&) :: g -> c -> s 
;; T! & T?  -> Id_T
;; T! & G?ˡ -> <<T =>ˡ G>>
;; (s -> t) & (s' -> t') -> [s' $ s -> t $ t']

;; compose intermediate coercion
;; ($) :: s -> t -> s (must be smaller or equal size)
;; T?l $ Id_? = T?ˡ
;; T?l $ T!   = Id_?
;; T?l $ (T? ; i) = Can't happen?
;; T?l $ (_  ; )  = what here
;; (T? ; i) $ 


;; ;; Smart rebuild
;; [s -> t] -> s
;; [id_T -> id_G] -> Id_(T -> G)
;; [s -> t] -> (s -> t)
 

;; +-------------------------------------------------------------------------------+
;; |Input Grammar Cast-Or-Coercion-Language                                        |
;; |Output Grammar Cast2-Language                                                  |
;; +------------------------------------------------------------------------------|#




;; ;; The entry point for this pass it is called by impose-casting semantics

;; ;; (define (lower-function-casts prgm)
;; ;;   (match-define (Prog (list name next type) exp) prgm)

;; ;;   (define ucount (make-unique-counter next))

;; ;;   )
  
;;   ;; (match 
;;   ;;   ['Type-Based
;;   ;;    (match-define-values (e (naive-fn-state n c*))
;;   ;;      (run-state (lfc-expr exp) (naive-fn-state next (hasheq))))
;;   ;;    (Prog (list name n type) (Labels (hash-values c*) e))]
;;   ;;   ['Coercions
;;   ;;    (match-define-values (e (hybrid-fn-state n a* c*))
;;   ;;      (run-state (lfc-expr exp) (hybrid-fn-state next '() (hasheq))))
;;   ;;    (Prog (list name n type) (Labels (append (hash-values c*) a*) e))]))





;; #;(: put-bnd (Integer C0-Bnd-Code -> (State LFC-State Null)))
;; #;(define ((put-apply i b) s)
;;   (match
;;     [(hybrid-fn-state n a c) (hybrid-fn
;;   (match-let ([(cons n h) s])
;;     (values '() (cons n (hash-set h i b)))))]))

;; #;(: get-bnd (Integer -> (State LFC-State (Option C0-Bnd-Code))))
;; #;(define ((get-bnd i) s)
;;   (match-let ([(cons n h) s])
;;     (values (hash-ref h i #f) s)))

;; (: make-get-fn-cast : (HashTable Nat CoC3-Bnd-Code) String Cast-With-MAddr-Type -> (Index -> Uid))
;; (define ((make-get-fn-cast fn-casts name-base cast) arity)
;;   (define bnd? (hash-ref fn-casts arity #f))
;;   (cond
;;     [bnd? (car bnd?)]
;;     [else
;;      (define name (string-append name-base (number->string arity)))
;;      (define caster-uid  (next-uid! name))
;;      (define caster-code (build-fn-cast cast arity caster-uid))
;;      (define caster-bnd  (cons caster-uid caster-code))
;;      (hash-set! fn-casts arity caster-bnd)
;;      caster-uid]))





  

