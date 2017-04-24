#lang typed/racket/base
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
+------------------------------------------------------------------------------+
|Input Grammar Cast1-Language                                                  |
|Output Grammar Cast3-Language                                                 |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 scribble/srcdoc
 racket/match
 racket/format
 racket/list
 racket/set
 (except-in  "../helpers.rkt" logging)
 (submod "../logging.rkt" typed)
 "../unique-identifiers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast-or-coerce1.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/data-representation.rkt"
 "./interpret-casts-help.rkt")

(provide
 interpret-casts/coercions
 (all-from-out
  "../language/cast-or-coerce1.rkt"
  "../language/cast-or-coerce3.rkt"))

(: space-efficient? (Parameterof Boolean))
(define space-efficient? (make-parameter #t))

(: interpret-casts/coercions : Cast-or-Coerce1-Lang ->  Cast-or-Coerce3-Lang)
(define (interpret-casts/coercions prgm)
  ;; Desugaring the input program into its constituents 
  (match-define (Prog (list prgm-name prgm-next prgm-type) prgm-exp)
    prgm)

  (define unique-counter (make-unique-counter prgm-next))
  (define next-uid! (make-next-uid! unique-counter))

  (define open-coded? : (Symbol -> Boolean)
    (if (specialize-cast-code-generation?)
        (let ([open-coded-set  (set 'make-coercion 'interp-cast)])
          (lambda ([x : Symbol])
            (set-member? open-coded-set x)))
        (lambda (x) #f)))
  
  ;; First we generate names for everything in the runtime
  ;; and procedure that will either generate code to perform
  ;; and action or call off to one of the runtime functions
  ;; we are generating.
  
  ;; The runtime label for the runtime coercion interpreter
  (define interp-coercion-uid (next-uid! "interp_coercion"))

  (: interp-coercion-call : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define interp-coercion-call (apply-code interp-coercion-uid))

  ;; The runtime label for the runtime casts interpreter
  (define interp-cast-uid (next-uid! "interp_cast"))

  (: interp-cast-call : Cast-With-MAddr-Type)
  (define interp-cast-call (apply-code interp-cast-uid))

  ;; The runtime label for the compose interpreter
  (define compose-coercions-uid (next-uid! "compose_coercions"))

  (define compose-coercions-call : Compose-Coercions-Type
    (cond
      [(space-efficient?) (apply-code compose-coercions-uid)]
      [else (lambda (c1 c2) (Sequence-Coercion c1 c2))]))
  
  ;; The runtime label for the make coercion operation
  (define make-coercion-uid (next-uid! "make_coercion"))
  
  (: make-coercion-call : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
  (define make-coercion-call (apply-code make-coercion-uid))
  
  (define gen-make-coercion-code : Make-Coercion-Type
    (make-make-coercion-code next-uid! make-coercion-call make-coercion-uid))
  
  (define make-coercion : Make-Coercion-Type
    (cond
      [(open-coded? 'make-coercion) gen-make-coercion-code]
      [else make-coercion-call]))

  ;; The runtime label for the greatest lower bound
  (define greatest-lower-bound-type-uid (next-uid! "greatest_lower_bound"))
  ;; a call to the runtime greatest lower bound
  (: greatest-lower-bound-type-call GreatestLowerBound-Type)
  (define (greatest-lower-bound-type-call t1 t2)
    (App-Code (Code-Label greatest-lower-bound-type-uid) (list t1 t2)))

  ;; The runtime label for the runtime value copier
  (define copy-value-in-monoref-uid (next-uid! "copy_value_in_monoref"))
  
  (: copy-value-in-monoref-call : CoC3-Expr -> CoC3-Expr)
  (define copy-value-in-monoref-call (apply-code copy-value-in-monoref-uid))

  (define gen-copy-value-in-monoref : CopyValueInMonoRef-Type
    (gen-copy-value-in-monoref-code next-uid!))
  
  (define copy-value-in-monoref : CopyValueInMonoRef-Type
    (cond
      [(open-coded? 'copy-value-in-monoref) gen-copy-value-in-monoref]
      [else copy-value-in-monoref-call]))

  (define gen-greatest-lower-bound-type : GreatestLowerBound-Type
    (gen-greatest-lower-bound-type-code next-uid! greatest-lower-bound-type-call
                                        greatest-lower-bound-type-uid))

  (define greatest-lower-bound-type : GreatestLowerBound-Type
    (cond
      [(open-coded? 'greatest-lower-bound-type) gen-greatest-lower-bound-type]
      [else greatest-lower-bound-type-call]))

  (define gen-compose-coercions-code : Compose-Coercions-Type
    (make-compose-coercions-code next-uid! compose-coercions-call
                                 compose-coercions-uid make-coercion
                                 greatest-lower-bound-type))

  (define compose-coercions : Compose-Coercions-Type
    (cond
      [(open-coded? 'compose-coercions) gen-compose-coercions-code]
      [else compose-coercions-call]))
  
  ;; Code generators for the coercion casting runtime
  (define gen-interp-coercion-code
    (make-coerce-code next-uid!
                      interp-coercion-call interp-coercion-uid
                      make-coercion compose-coercions
                      interp-cast-call interp-cast-uid
                      greatest-lower-bound-type
                      copy-value-in-monoref))

  (define interp-coercion
    (cond
      [(open-coded? 'interp-coercion) gen-interp-coercion-code]
      [else interp-coercion-call]))

  ;; Code generators for the cast interpreter runtime
  (define gen-interp-cast-code
    (make-cast-code next-uid!
                    interp-cast-call interp-cast-uid
                    make-coercion
                    greatest-lower-bound-type
                    copy-value-in-monoref))

  (define interp-cast
    (cond
      [(open-coded? 'interp-cast) gen-interp-cast-code]
      [else interp-cast-call]))
  
  (define bindings-needed-for-interp-coercion
    (let ([interp-v (next-uid! "value")]
          [interp-c (next-uid! "coercion")]
          [interp-a (next-uid! "mono-address")]
          [mc-t1    (next-uid! "type1")]
          [mc-t2    (next-uid! "type2")]
          [mc-lbl   (next-uid! "blame_info")])
      `([,interp-coercion-uid
         . ,(Code (list interp-v interp-c interp-a)
              (gen-interp-coercion-code (Var interp-v) (Var interp-c) (Var interp-a)))]
        [,make-coercion-uid
         . ,(Code (list mc-t1 mc-t2 mc-lbl)
              (gen-make-coercion-code (Var mc-t1) (Var mc-t2) (Var mc-lbl)))])))

  (define bindings-needed-for-space-efficiency
    (cond
      [(not (space-efficient?)) '()]
      [else
       (define-values (c1 c2)
         (values (next-uid! "coercion1")
                 (next-uid! "coercion2")))
       `([,compose-coercions-uid
          . ,(Code (list c1 c2)
               (gen-compose-coercions-code (Var c1) (Var c2)))])]))
  
  ;; Initialize all the state for guarded operations
  ;; TODO standardize type names here
  (: gbox-ref : Gbox-refT)
  (: gbox-set! : Gbox-setT)
  (: gvec-ref : Gvec-refT)
  (: gvec-set! : Gvec-setT)
  (: gvec-length : Gvec-LengthT)
  (: bindings-needed-for-guarded : CoC3-Bnd-Code*)
  (define-values (gbox-ref gbox-set! gvec-ref gvec-set! gvec-length
                           bindings-needed-for-guarded)
    ;; First we create initialize the code generators
    (let ([gen-gbox-ref-code (make-gbox-ref-code interp-coercion-call)]
          [gen-gbox-set!-code (make-gbox-set!-code interp-coercion-call)]
          [gen-gvec-ref-code (make-gvect-ref-code interp-coercion-call)]
          [gen-gvec-set!-code (make-gvect-set!-code interp-coercion-call)]
          [gen-gvec-length-code (make-gvect-length-code)])
      (cond
        [(inline-guarded-branch?)
         ;; we just hand back the code generators to build
         ;; inline code everywhere.
         (values gen-gbox-ref-code gen-gbox-set!-code
                 gen-gvec-ref-code gen-gvec-set!-code
                 gen-gvec-length-code
                 '())]
        [else
         ;; If they are not inlined then the compiler generates
         ;; the runtime binding and returns procedures that builds
         ;; invokations of this runtime code.
         (let ([gbr   (next-uid! "rt_gbox_ref")]
               [gbr-b (next-uid! "box")]
               [gbs   (next-uid! "rt_gbox_set")]
               [gbs-b (next-uid! "box")]
               [gbs-v (next-uid! "write_val")]
               [gvr   (next-uid! "rt_gvec_ref")]
               [gvr-r (next-uid! "vec")]
               [gvr-i (next-uid! "ind")]
               [gvs   (next-uid! "rt_gvec_set")]
               [gvs-r (next-uid! "vec")]
               [gvs-i (next-uid! "ind")]
               [gvs-v (next-uid! "val")]
               [gvl   (next-uid! "rt_gvec_len")]
               [gvl-r (next-uid! "vec")])
           (values
            (apply-code gbr) (apply-code gbs)
            (apply-code gvr) (apply-code gvs)
            (apply-code gvl)
            `([,gbr
               . ,(Code (list gbr-b) (gen-gbox-ref-code (Var gbr-b)))]
              [,gbs
               . ,(Code (list gbs-b gbs-v)
                    (gen-gbox-set!-code (Var gbs-b) (Var gbs-v)))]
              [,gvr
               . ,(Code (list gvr-r gvr-i)
                    (gen-gvec-ref-code (Var gvr-r) (Var gvr-i)))]
              [,gvs
               . ,(Code (list gvs-r gvs-i gvs-v)
                    (gen-gvec-set!-code (Var gvs-r) (Var gvs-i)
                                        (Var gvs-v)))]
              [,gvl
               . ,(Code (list gvl-r)
                    (gen-gvec-length-code (Var gvl-r)))])))])))
  
  (: mbox-ref : Mbox-refT)
  (: mbox-set! : Mbox-setT)
  (: mvec-ref : Mvec-refT)
  (: mvec-set! : Mvec-setT)
  (: bindings-needed-for-monotonic-refs : CoC3-Bnd-Code*)
  (define-values (mbox-ref mbox-set! mvec-ref mvec-set!
                  bindings-needed-for-monotonic-refs)
    (let ([gen-mbox-ref-code (make-mbox-ref-code next-uid! interp-cast-call)]
          [gen-mbox-set!-code (make-mbox-set!-code next-uid! interp-cast-call)]
          [gen-mvec-ref-code (make-mvect-ref-code next-uid! interp-cast-call)]
          [gen-mvec-set!-code (make-mvect-set!-code next-uid! interp-cast-call)]
          [interp-v (next-uid! "value")]
          [interp-t1 (next-uid! "t1")]
          [interp-t2 (next-uid! "t2")]
          [interp-a (next-uid! "mono-address")]
          [interp-lbl (next-uid! "lbl")]
          [glbt-t1    (next-uid! "type1")]
          [glbt-t2    (next-uid! "type2")]
          [glbt-a          (next-uid! "mono-address")])
      (define shared-bnd* : CoC3-Bnd-Code*
        `([,interp-cast-uid
         . ,(Code (list interp-v interp-t1 interp-t2 interp-lbl interp-a)
              (gen-interp-cast-code (Var interp-v) (Var interp-t1) (Var interp-t2) (Var interp-lbl) (Var interp-a)))]
          [,greatest-lower-bound-type-uid
           . ,(Code (list glbt-t1 glbt-t2)
                (gen-greatest-lower-bound-type (Var glbt-t1) (Var glbt-t2)))]
          [,copy-value-in-monoref-uid
           . ,(Code (list glbt-a)
                (gen-copy-value-in-monoref (Var glbt-a)))]))
      (cond
        ;; FIXME: I guess this flag should be generalized to all ref implementations
        [(inline-guarded-branch?)
         (values
          gen-mbox-ref-code gen-mbox-set!-code
          gen-mvec-ref-code gen-mvec-set!-code
          shared-bnd*)]
        [else
         ;; If they are not inlined then the compiler generates
         ;; the runtime binding and returns procedures that builds
         ;; invokations of this runtime code.
         (let ([mbr   (next-uid! "rt_mbox_ref")]
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
            (append
             shared-bnd*
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
                                         (Var mvs-v) (Var mvs-rt)))]))))])))


  (: dyn-fn-app : Dyn-Fn-AppT)
  (: bindings-needed-for-fn-dynamic-operations : CoC3-Bnd-Code*)
  (define-values (dyn-fn-app
                  bindings-needed-for-fn-dynamic-operations)
    (let* ([smart-cast
            (make-smart-cast next-uid! interp-coercion-call make-coercion)]
           [gen-dyn-fn-app
            (make-dyn-fn-app-code next-uid! smart-cast)])
      (define ((th-error [sym : Symbol]) . a)
        (error sym "dynamic-operation? = #f but present in AST"))
      (case (dynamic-operations?)
        [(#f)
         (values
          (th-error 'interpret-cast-with-coercions/dyn-fn-app)
          '())]
        [else
         (values
          gen-dyn-fn-app
          `())])))

  
  (: dyn-gbox-ref : Dyn-Gbox-refT)
  (: dyn-gbox-set! : Dyn-Gbox-setT)
  (: dyn-gvec-ref  : Dyn-Gvec-refT)
  (: dyn-gvec-set! : Dyn-Gvec-setT)
  (: bindings-needed-for-guarded-dynamic-operations : CoC3-Bnd-Code*)
  (define-values (dyn-gbox-ref dyn-gbox-set!
                               dyn-gvec-ref dyn-gvec-set!
                               bindings-needed-for-guarded-dynamic-operations)
    (let* ([smart-cast
            (make-smart-cast next-uid! interp-coercion-call make-coercion)]
           [gen-dyn-gvec-set!
            (make-dyn-gvect-set!-code next-uid! gvec-set! smart-cast)]
           [gen-dyn-gvec-ref
            (make-dyn-gvect-ref-code next-uid! gvec-ref smart-cast)]
           [gen-dyn-gbox-set!
            (make-dyn-gbox-set!-code next-uid! gbox-set! smart-cast)]
           [gen-dyn-gbox-ref 
            (make-dyn-gbox-ref-code next-uid! gbox-ref smart-cast)])
      (define ((th-error [sym : Symbol]) . a)
        (error sym "dynamic-operation? = #f but present in AST"))
      (case (dynamic-operations?)
        [(#f)
         (values
          (th-error 'interpret-cast-with-coercions/dyn-gbox-ref)
          (th-error 'interpret-cast-with-coercions/dyn-gbox-set!)
          (th-error 'interpret-cast-with-coercions/dyn-gvec-ref)
          (th-error 'interpret-cast-with-coercions/dyn-gvec-set!)
          '())]
        [(inline)
         (values
          gen-dyn-gbox-ref gen-dyn-gbox-set!
          gen-dyn-gvec-ref gen-dyn-gvec-set!
          '())]
        [else
         ;; TODO come up with a solution that doesn't manually
         ;; require all the variable initializations
         (let ([gbr   (next-uid! "rt_dyn_gbox_ref")]
               [gbr-b (next-uid! "gbox")]
               [gbr-l (next-uid! "blame_info")]
               [gbs   (next-uid! "rt_dyn_gbox_set")]
               [gbs-b (next-uid! "gbox")]
               [gbs-v (next-uid! "write_val")]
               [gbs-t (next-uid! "val_type")]
               [gbs-l (next-uid! "blame_info")]
               [gvr   (next-uid! "rt_dyn_gvec_ref")]
               [gvr-r (next-uid! "gvec")]
               [gvr-i (next-uid! "ind")]
               [gvr-l (next-uid! "blame_info")]
               [gvs   (next-uid! "rt_dyn_gvec_set")]
               [gvs-r (next-uid! "gvec")]
               [gvs-i (next-uid! "ind")]
               [gvs-v (next-uid! "val")]
               [gvs-t (next-uid! "val_type")]
               [gvs-l (next-uid! "blame_info")])
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
                                       (Var gvs-l)))])))])))
  
  (: dyn-mbox-ref  : Dyn-Mbox-refT)
  (: dyn-mbox-set! : Dyn-Mbox-setT)
  (: dyn-mvec-ref  : Dyn-Mvec-refT)
  (: dyn-mvec-set! : Dyn-Mvec-setT)
  (: bindings-needed-for-mono-dynamic-operations : CoC3-Bnd-Code*)
  (define-values (dyn-mbox-ref dyn-mbox-set!
                               dyn-mvec-ref dyn-mvec-set!
                               bindings-needed-for-mono-dynamic-operations)
    (let* ([smart-cast
            (make-smart-cast next-uid! interp-coercion-call make-coercion)]
           [gen-dyn-mvec-set!
            (make-dyn-mvect-set!-code next-uid! mvec-set! smart-cast)]
           [gen-dyn-mvec-ref
            (make-dyn-mvect-ref-code next-uid! mvec-ref)]
           [gen-dyn-mbox-set!
            (make-dyn-mbox-set!-code next-uid! mbox-set! smart-cast)]
           [gen-dyn-mbox-ref
            (make-dyn-mbox-ref-code next-uid! mbox-ref)])
      (define ((th-error [sym : Symbol]) . a)
        (error sym "dynamic-operation? = #f but present in AST"))
      (case (dynamic-operations?)
        [(#f)
         (values
          (th-error 'interpret-cast-with-coercions/dyn-mbox-ref)
          (th-error 'interpret-cast-with-coercions/dyn-mbox-set!)
          (th-error 'interpret-cast-with-coercions/dyn-mvec-ref)
          (th-error 'interpret-cast-with-coercions/dyn-mvec-set!)
          '())]
        [(inline)
         (values
          gen-dyn-mbox-ref gen-dyn-mbox-set!
          gen-dyn-mvec-ref gen-dyn-mvec-set!
          '())]
        [else
         (let ([mbr   (next-uid! "rt_dyn_mbox_ref")]
               [mbr-b (next-uid! "mbox")]
               [mbr-l (next-uid! "blame_info")]
               [mbs   (next-uid! "rt_dyn_mbox_set")]
               [mbs-b (next-uid! "mbox")]
               [mbs-v (next-uid! "write_val")]
               [mbs-t (next-uid! "val_type")]
               [mbs-l (next-uid! "blame_info")]
               [mvr   (next-uid! "rt_dyn_mvec_ref")]
               [mvr-r (next-uid! "mvec")]
               [mvr-i (next-uid! "ind")]
               [mvr-l (next-uid! "blame_info")]
               [mvs   (next-uid! "rt_dyn_mvec_set")]
               [mvs-r (next-uid! "mvec")]
               [mvs-i (next-uid! "ind")]
               [mvs-v (next-uid! "val")]
               [mvs-t (next-uid! "val_type")]
               [mvs-l (next-uid! "blame_info")])
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
                    (gen-dyn-mvec-ref (Var mvr-r) (Var mvr-i)
                                      (Var mvr-l)))]
              [,mvs
               . ,(Code (list mvs-r mvs-i mvs-v mvs-t mvs-l)
                    (gen-dyn-mvec-set! (Var mvs-r) (Var mvs-i)
                                       (Var mvs-v) (Var mvs-t)
                                       (Var mvs-l)))])))])))
  
  ;; Next we generate the bindings for code that needs to be
  ;; included as the runtime.
  (define gradual-runtime-bindings : CoC3-Bnd-Code*
    (append
     bindings-needed-for-fn-dynamic-operations
     bindings-needed-for-monotonic-refs
     bindings-needed-for-mono-dynamic-operations
     bindings-needed-for-guarded
     bindings-needed-for-guarded-dynamic-operations
     bindings-needed-for-space-efficiency
     bindings-needed-for-interp-coercion))

  (define exp-with-lowered-gradual-operations
    (interpret-casts-in-expr
     next-uid!
     interp-coercion-uid interp-coercion compose-coercions make-coercion
     gbox-ref gbox-set! gvec-ref gvec-set! gvec-length
     mbox-ref mbox-set! mvec-ref mvec-set!
     dyn-gbox-ref dyn-gbox-set! dyn-gvec-ref dyn-gvec-set!
     dyn-mbox-ref  dyn-mbox-set! dyn-mvec-ref dyn-mvec-set!
     dyn-fn-app
     prgm-exp))
  
  (Prog (list prgm-name (unique-counter-next! unique-counter) prgm-type)
    (Labels gradual-runtime-bindings exp-with-lowered-gradual-operations)))

;; These templates are used to build the code that performs
;; casting at runtime. The templates are a little over
;; parameterized currently in hopes that it make specialization
;; and customization easier in the future.
(define-type Coerce-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(: make-coerce-code :
   (String -> Uid)
   Coerce-Type Uid
   Make-Coercion-Type Compose-Coercions-Type
   Cast-With-MAddr-Type Uid
   GreatestLowerBound-Type CopyValueInMonoRef-Type
   ->
   Coerce-Type)
(define ((make-coerce-code
          next-uid!
          coerce coerce-u
          mk-crcn comp-crcn
          cast cast-u
          glbt copy-val-monoref) v c mono_type)
  (define who 'make-coerce-code)
  (debug who v c)

  (define-syntax-let$* let$* next-uid!)
  ;; apply-cast is normally specified in terms of inspecting the
  ;; value. But in a world were value reflection on values is limited
  ;; the coercions can drive the casting process.
  (let$* ([val v] [crcn c])
    (cond$
     [(id?$ crcn) val]
     [(seq?$ crcn)
      ;; I think that something better could be done here
      ;; why recur there are only so many things that the underlying
      ;; sequence could be?
      
      ;; This may be getting in the way of simple specializations
      ;; May want come up with smaller versions of the cast/function
      ;; For the specialized sub grammers.
      (let$* ([seq_fst (seq-fst$ crcn)]
              [seq_snd (seq-snd$ crcn)]
              [fst_cast_value (coerce val seq_fst mono_type)])
        (coerce fst_cast_value seq_snd mono_type))]
     ;; By the typing rules of coercions we know v must be a dyn value
     [(prj?$ crcn)
      (let$* ([prj_type  (prj-type$ crcn)]
              [prj_label (prj-label$ crcn)]
              ;; The current implementation of the dyn interface is not a good one
              ;; this line is going to have to recover the type from dyn twice
              [dyn_value (Dyn-value val)]
              [dyn_type  (Dyn-type val)])
        ;; Maybe this line should be a call to compose?
        ;; which would do the same thing but wouldn't duplicate
        ;; this line across two functions
        ;; I did it this way because I know this code won't build
        ;; the injection coercion
        (let$* ([projected_type (mk-crcn dyn_type prj_type prj_label)])
          (coerce dyn_value projected_type mono_type)))]
     [(inj?$ crcn) (Dyn-make val (inj-type$ crcn))]
     [(mediating-crcn?$ crcn)
      (cond$
       [(fnC?$ crcn)
        (If (Fn-Proxy-Huh val)
            (App-Code (Fn-Caster (Fn-Proxy-Closure val)) (list val crcn))
            (App-Code (Fn-Caster val) (list val crcn)))]
       [(ref?$ crcn)
        (if$ (Guarded-Proxy-Huh val)
             (let$* ([old_val  (Guarded-Proxy-Ref val)]
                     [old_crcn (Guarded-Proxy-Coercion val)])
               (let$* ([composed_crcn (comp-crcn old_crcn crcn)])
                 (if$ (id?$ composed_crcn)
                      old_val
                      (Guarded-Proxy
                       old_val 
                       (Coercion composed_crcn)))))
             (Guarded-Proxy val (Coercion crcn)))]
       [(mrefC?$ crcn)
        (match val
          [(Var a)
           (let$* ([t2 (mrefC-type$ crcn)])
             (if$ (dyn?$ t2)
                  val
                  (let$* ([t1 (Mbox-rtti-ref val)]
                          [t3 (glbt t1 t2)])
                    (if$ (op=? t1 t3)
                         val
                         (Begin
                           (list
                            (Mbox-rtti-set! val t3))
                           (let$* ([vv (copy-val-monoref val)]
                                   [cv (cast
                                        vv t1 t3
                                        (Quote "Monotonic references currently does not track blame")
                                        val)]
                                   [t4 (Mbox-rtti-ref val)])
                             (if$ (op=? t3 t4)
                                  (Begin
                                    (list (Mbox-val-set! val cv))
                                    val)
                                  val)))))))]
          [other (error 'interp-coercion/mrefC "unmatched value ~a" other)])]
       [(mvectC?$ crcn)
        (match val
          [(Var a)
           (let$* ([t2 (mvectC-type$ crcn)])
             (if$ (dyn?$ t2)
                  val
                  (let$* ([t1 (Mvector-rtti-ref val)]
                          [t3 (glbt t1 t2)])
                    (if$ (op=? t1 t3)
                         val
                         (Begin
                           (list
                            (Mvector-rtti-set! val t3)
                            (let$* ([vn (Mvector-length val)])
                              (let* ([i-u (next-uid! "index")]
                                     [i (Var i-u)]
                                     [x (next-uid! "_")])
                                (cond$
                                 [(tupleT?$ t3)
                                  (let$* ([n (Type-Tuple-num t3)])
                                    (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                            (let$* ([vi (Mvector-val-ref val i)]
                                                    [cvi (Copy-Tuple n vi)])
                                              (Begin
                                                (list
                                                 (Mvector-val-set! val i cvi))
                                                (let$* ([ccvi (cast
                                                               cvi t1 t3
                                                               (Quote "Monotonic references currently does not track blame")
                                                               val)]
                                                        [t4 (Mvector-rtti-ref val)])
                                                  (if$ (op=? t3 t4)
                                                       (Mvector-val-set! val i ccvi)
                                                       (Break-Repeat)))))))]
                                 [else
                                  (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                          (let$* ([vi (Mvector-val-ref val i)]
                                                  [cvi (cast
                                                        vi t1 t3
                                                        (Quote "Monotonic references currently does not track blame")
                                                        val)]
                                                  [t4 (Mvector-rtti-ref val)])
                                            (if$ (op=? t3 t4)
                                                 (Mvector-val-set! val i cvi)
                                                 (Break-Repeat))))]))))
                           val)))))]
          [other (error 'interp-coercion/mvectC "unmatched value ~a" other)])]
       [(tuple?$ crcn)
        (match crcn
          [(not (Quote-Coercion _)) (If (Op '= (list (Quote 0) mono_type))
                                        (Coerce-Tuple coerce-u val crcn)
                                        (Coerce-Tuple-In-Place coerce-u val crcn mono_type))]
          [(Quote-Coercion (CTuple n c*))
           (define-values (bnd* var*)
             (for/lists ([bnd* : CoC3-Bnd*] [var* : CoC3-Expr*])
                        ([i (in-range n)] [c (in-list c*)])
               (cond
                 [(index? i)
                  (define tmp (next-uid! "element"))
                  (values
                   (cons tmp (coerce (Tuple-proj val (Quote i)) (Quote-Coercion c) (Quote 0)))
                   (Var tmp))]
                 [else (error 'interpret-casts-with-coercions "bad index")])))
           (Let bnd* (Create-tuple var*))]
          [other (error 'cast/coercion "thats impossible! ... I think")])]
       [else (Blame (Quote "bad implemention of mediating coercions"))])]
     ;; the coercion must be failure
     [else (Blame (fail-label$ crcn))])))


(define-type Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Cast-With-MAddr-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(: make-cast-code :
   (String -> Uid)
   Cast-With-MAddr-Type Uid
   Make-Coercion-Type
   GreatestLowerBound-Type CopyValueInMonoRef-Type
   ->
   Cast-With-MAddr-Type)
(define ((make-cast-code
          next-uid!
          cast cast-u
          mk-crcn
          glbt copy-val-monoref) v type1 type2 lbl mono-address)

  (define-syntax-let$* let$* next-uid!)

  (: cast-dyn : Cast-With-MAddr-Type)
  (define (cast-dyn  v t1 t2 lbl mono-address)
    (let$* ([val v] [tag (Dyn-tag val)])
      (cond$
       [(op=? (Tag 'Int) tag)
        (cast-undyned (Dyn-immediate val) (Type INT-TYPE) t2 lbl mono-address)]
       [(op=? (Tag 'Bool) tag)
        (cast-undyned (Dyn-immediate val) (Type BOOL-TYPE) t2 lbl mono-address)]
       [(op=? (Tag 'Unit) tag)
        (cast-undyned (Quote '()) (Type UNIT-TYPE) t2 lbl mono-address)]
       [(op=? (Tag 'Char) tag)
        (cast-undyned (Dyn-immediate val) (Type CHAR-TYPE) t2 lbl mono-address)]
       [(op=? (Tag 'Boxed) tag)
        (cast-undyned (Dyn-value val) (Dyn-type val) t2 lbl mono-address)]
       [else (Blame (Quote "Unexpected value in cast tree"))])))

  (: cast-undyned : Cast-With-MAddr-Type)
  (define (cast-undyned v t1 t2 l mono-address)
    (let$* ([value v] [type1 t1] [type2 t2] [label l])
      (cond$
       [(op=? type1 type2) value]
       [else (cast-ground value type1 type2 label mono-address)])))

  (: cast-ground : Cast-With-MAddr-Type)
  (define (cast-ground v t1 t2 lbl mono-address)
    (let$* ([value v] [type1 t1] [type2 t2])
      (cond$
       [(op=? type1 (Type INT-TYPE))
        (if$ (op=? (Type DYN-TYPE) type2)
             (Dyn-make value (Type INT-TYPE))
             (Blame lbl))]
       [(op=? type1 (Type BOOL-TYPE))
        (if$ (op=? (Type DYN-TYPE) type2)
             (Dyn-make value (Type BOOL-TYPE))
             (Blame lbl))]
       [(op=? type1 (Type FLOAT-TYPE))
        (if$ (op=? (Type DYN-TYPE) type2)
             (Dyn-make value (Type FLOAT-TYPE))
             (Blame lbl))]
       [(op=? type1 (Type CHAR-TYPE))
        (if$ (op=? (Type DYN-TYPE) type2)
             (Dyn-make value (Type CHAR-TYPE))
             (Blame lbl))]
       [(op=? type1 (Type UNIT-TYPE))
        (if$ (op=? (Type DYN-TYPE) type2)
             (Dyn-make value (Type UNIT-TYPE))
             (Blame lbl))]
       [else
        (let$* ([tag1 (type-tag type1)])
          (cond$
           [(op=? (Tag 'Fn) tag1)
            (cast-fn value type1 type2 lbl)]
           [(op=? (Tag 'MRef) tag1)
            (cast-mbox value type1 type2 lbl)]
           [(op=? (Tag 'MVect) tag1)
            (cast-mvect value type1 type2 lbl)]
           [(op=? (Tag 'STuple) tag1)
            (If (Op '= (list (Quote 0) mono-address))
                (cast-tuple value type1 type2 lbl)
                (Cast-Tuple-In-Place cast-u value type1 type2 lbl mono-address))]
           [else (Blame (Quote "Unexpected Type1 in cast tree"))]))])))
  
  (: cast-fn : Cast-Type)
  (define (cast-fn v t1 t2 lbl)
    (let$* ([value v] [type1 t1] [type2 t2] [l lbl])
      (if$ (op=? (Type DYN-TYPE) type2)
           (Dyn-make value type1)
           (let$* ([crcn (mk-crcn type1 type2 l)])
             (If (Fn-Proxy-Huh value)
                 (App-Code (Fn-Caster (Fn-Proxy-Closure value)) (list value crcn))
                 (App-Code (Fn-Caster value) (list value crcn)))))))

  (: cast-mbox : Cast-Type)
  (define (cast-mbox v t1 t2 lbl)
    (let$* ([val v] [type1 t1] [type2 t2] [tag_mref (type-tag type2)])
      (if$ (op=? (Type DYN-TYPE) type2)
           (Dyn-make val type1)
           (if$ (op=? tag_mref (Tag 'MRef))
                (match val
                  [(Var a)
                   (let$* ([t2 (mref-of$ type2)])
                     (if$ (dyn?$ t2)
                          val
                          (let$* ([t1 (Mbox-rtti-ref val)]
                                  [t3 (glbt t1 t2)])
                            (if$ (op=? t1 t3)
                                 val
                                 (Begin
                                   (list
                                    (Mbox-rtti-set! val t3))
                                   (let$* ([vv (copy-val-monoref val)]
                                           [cv (cast vv t1 t3
                                                     (Quote "Monotonic references currently does not track blame")
                                                     val)]
                                           [t4 (Mbox-rtti-ref val)])
                                     (if$ (op=? t3 t4)
                                          (Begin
                                            (list (Mbox-val-set! val cv))
                                            val)
                                          val)))))))]
                  [other (error 'interp-cast-with-coercions/cast/mref "unmatched value ~a" other)])
                (Blame lbl)))))

  (: cast-mvect : Cast-Type)
  (define (cast-mvect v t1 t2 lbl)
    (let$* ([val v] [type1 t1] [type2 t2] [tag_mvect (type-tag type2)])
      (if$ (op=? (Type DYN-TYPE) type2)
           (Dyn-make val type1)
           (if$ (op=? tag_mvect (Tag 'MVect))
                (match val
                  [(Var a)
                   (let$* ([t2 (mvect-of$ type2)])
                     (if$ (dyn?$ t2)
                          val
                          (let$* ([t1 (Mvector-rtti-ref val)]
                                  [t3 (glbt t1 t2)])
                            (if$ (op=? t1 t3)
                                 val
                                 (Begin
                                   (list
                                    (Mvector-rtti-set! val t3)
                                    (let* ([i-u (next-uid! "index")]
                                           [i (Var i-u)]
                                           [x (next-uid! "_")])
                                      (let$* ([vn (Mvector-length val)])
                                        (cond$
                                         [(tupleT?$ t3)
                                          (let$* ([n (Type-Tuple-num t3)])
                                            (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                                    (let$* ([vi (Mvector-val-ref val i)]
                                                            [cvi (Copy-Tuple n vi)])
                                                      (Begin
                                                        (list
                                                         (Mvector-val-set! val i cvi))
                                                        (let$* ([ccvi (cast cvi t1 t3
                                                                            (Quote "Monotonic references currently does not track blame")
                                                                            val)]
                                                                [t4 (Mvector-rtti-ref val)])
                                                          (if$ (op=? t3 t4)
                                                               (Mvector-val-set! val i ccvi)
                                                               (Break-Repeat)))))))]
                                         [else
                                          ;; TODO: checking if t3=t4 is unneeded in this case, prove it!
                                          (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                                  (let$* ([vi (Mvector-val-ref val i)]
                                                          [cvi (cast vi t1 t3
                                                                     (Quote "Monotonic references currently does not track blame")
                                                                     val)]
                                                          [t4 (Mvector-rtti-ref val)])
                                                    (if$ (op=? t3 t4)
                                                         (Mvector-val-set! val i cvi)
                                                         (Break-Repeat))))]))))
                                   val)))))]
                  [other (error 'interp-casts-with-coercions/cast/mvect "unmatched value ~a" other)])
                (Blame lbl)))))

  (: cast-tuple : Cast-Type)
  (define (cast-tuple v t1 t2 lbl)
    (let$* ([value v] [type1 t1] [type2 t2] [label lbl])
      (if$ (op=? (Type DYN-TYPE) type2)
           (Dyn-make value type1)
           ;; Todo Reformat this code
           (if #f #;(and (Type? t1) (Type? t2))
               (let ([t1t (Type-type t1)]
                     [t2t (Type-type t2)])
                 (if (and (STuple? t1t) (STuple? t2t))
                     (let-values ([(bnd* arg*)
                                   (for/fold ([b* : CoC3-Bnd* '()]
                                              [arg* : Uid* '()])
                                             ([i (in-range (STuple-num t2t))])
                                     (unless (index? i)
                                       (error 'make-cast-tuple-code "bad index"))
                                     (let ([a (next-uid! "item_type")])
                                       (values (cons (cons a (let$* ([item (Tuple-proj v (Quote i))]
                                                                     [new-t1t (tuple-type-arg$ t1 i)]
                                                                     [new-t2t (tuple-type-arg$ t2 i)])
                                                               (cast item new-t1t new-t2t lbl (Quote 0)))) b*)
                                               (cons a arg*))))])
                       (Let bnd* (Create-tuple (map (inst Var Uid) arg*))))
                     (error 'make-cast-tuple-code)))
               (let$* ([tag2 (type-tag type2)])
                 (if$ (op=? tag2 (Tag 'STuple))
                      (let$* ([type1_num (type-tuple-num type1)]
                              [type2_num (type-tuple-num type2)])
                        (if$ (op<=? type2_num type1_num)
                             (Cast-Tuple cast-u v t1 t2 lbl)
                             (Blame type2_num)))
                      (Blame lbl)))))))
  
  (let$* ([val v] [t1 type1] [t2 type2])
    (cond$
     [(op=? type1 type2) v]
     [(op=? type1 (Type DYN-TYPE)) (cast-dyn v type1 type2 lbl mono-address)]
     [else (cast-ground v type1 type2 lbl mono-address)])))

(define-type Make-Coercion-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(: make-make-coercion-code :
   (String -> Uid) Make-Coercion-Type Uid -> Make-Coercion-Type)
(define ((make-make-coercion-code next-uid! mk-crcn mk-crcn-uid) t1 t2 lbl)
  (define who 'make-make-coercion-code)
  (debug who t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
  (: help-comp :
     CoC3-Trivial -> (Schml-Type Schml-Type -> Schml-Coercion))
  (define ((help-comp lbl) t g)
    (let ([c : CoC3-Expr (mk-crcn (Type t) (Type g) lbl)])
      (unless (Quote-Coercion? c)
        (error 'interpret-casts-with-coercions/mk-fn-crcn$/help-comp
               "This should always be possible if all the code is correct"))
      (Quote-Coercion-const c)))
  (: make-make-fn-crcn-code Make-Coercion-Type)
  (define (make-make-fn-crcn-code t1 t2 lbl)
    (cond
      [(and (Type? t1) (Type? t2))
       (define-values (t1^ t2^)
         (values (Type-type t1) (Type-type t2)))
       (unless (and (Fn? t1^) (Fn? t2^))
         (error 'interpret-casts-with-coercions/mk-fn-crcn$
                "That's is impossible!!!!!"))
       (match-define-values
           ((Type (Fn _ t1-args t1-ret))
            (Type (Fn _ t2-args t2-ret)))
         (values t1^ t2^))
       (define arg* (map (help-comp lbl) t2-args t1-args))
       (define ret  ((help-comp lbl) t1-ret t2-ret))
       (Quote-Coercion (Fn (Fn-arity t1) arg* ret))]
      ;; In the two cases below we could speculatively generate
      ;; the code for a coercion with known arity
      ;;[(Type? t1)]
      ;;[(Type? t2)]
      ;; The Make-Fn-Coercion node require the ability
      ;; to create and initialize objects with unkown size
      ;; at runtime. Since this is we are currently impoverished
      ;; by our language we choose to implement this during
      ;; specify representation were the language is a little
      ;; more flexible.
      [else (Make-Fn-Coercion mk-crcn-uid t1 t2 lbl)]))
  (let$* ([t1 t1] [t2 t2] [lbl lbl])
    (cond$
     [(op=? t1 t2) (Quote-Coercion (Identity))]
     [(and$ (fnT?$ t1) (fnT?$ t2))
      ;; This line is a little tricky because
      ;; unless we have actual types for this at
      ;; compile time we have to generate code
      ;; that can handle arbitry fn-arity.
      ;; We delegate this task to specify representation because
      ;; it involves safely allocating an object whos size cannot
      ;; be determined until run-time.
      (if$ (op=? (fnT-arity$ t1) (fnT-arity$ t2))
           (make-make-fn-crcn-code t1 t2 lbl)
           (Blame lbl))]
     [(and$ (gvect?$ t1) (gvect?$ t2))
      (let$* ([gv1_of (gvect-of$ t1)]
              [gv2_of (gvect-of$ t2)]
              [read_crcn  (mk-crcn gv1_of gv2_of lbl)]
              [write_crcn (mk-crcn gv2_of gv1_of lbl)])
        (ref$ read_crcn write_crcn))]
     [(and$ (gref?$ t1) (gref?$ t2))
      (let$* ([gr1_of (gref-of$ t1)]
              [gr2_of (gref-of$ t2)]
              [read_crcn  (mk-crcn gr1_of gr2_of lbl)]
              [write_crcn (mk-crcn gr2_of gr1_of lbl)])
        (ref$ read_crcn write_crcn))]
     [(and$ (mref?$ t1) (mref?$ t2))
      (let$* ([t (mref-of$ t2)])
        (mrefC$ t))]
     [(and$ (mvect?$ t1) (mvect?$ t2))
      (let$* ([t (mvect-of$ t2)])
        (mvectC$ t))]
     [(and$ (tupleT?$ t1) (tupleT?$ t2))
      (if$ (op<=? (tupleT-num$ t2) (tupleT-num$ t1))
           (if (and (Type? t1) (Type? t2))
               (let* ([t1 (Type-type t1)] [t2 (Type-type t2)])
                 (if (and (STuple? t1) (STuple? t2))
                     (let* ([t1-args (STuple-items t1)]
                            [t2-args (STuple-items t2)]
                            [item* (map (help-comp lbl) t1-args t2-args)])
                       (Quote-Coercion (CTuple (STuple-num t2) item*)))
                     (error 'make-coercion)))
               (Make-Tuple-Coercion mk-crcn-uid t1 t2 lbl))
           (Blame lbl))]
     ;; This is absolutly necisarry
     ;; While Injections and Projections are never made by
     ;; source code coercions composition can create new
     ;; projections and injections.
     [(dyn?$ t1) (seq$ (prj$ t2 lbl) (Quote-Coercion (Identity)))]
     [(dyn?$ t2) (seq$ (Quote-Coercion (Identity)) (inj$ t1))]
     [else (fail$ lbl)])))

(define-type Compose-Coercions-Type
  (CoC3-Expr CoC3-Expr -> CoC3-Expr))

(: make-compose-coercions-code :
   (String -> Uid)
   Compose-Coercions-Type
   Uid
   Make-Coercion-Type
   GreatestLowerBound-Type
   -> Compose-Coercions-Type)
(define ((make-compose-coercions-code next-uid! compose compose-uid mk-crcn mk-glbt) c1 c2)
  (define-syntax-let$* let$* next-uid!)
  (: help-comp : Schml-Coercion Schml-Coercion -> Schml-Coercion)
  (define (help-comp s t)
    (let ([c (compose (Quote-Coercion s) (Quote-Coercion t))])
      (unless (Quote-Coercion? c)
        (error 'interpret-casts-with-coercions/help-comp))
      (Quote-Coercion-const c)))
  (cond$
   ;; Eliminating the Identities cuts down on the number of branches
   ;; and should be fast
   [(id?$ c1) c2]
   [(id?$ c2) c1]
   ;; We could elminate failure on the left next, but we choose to make
   ;; success as fast as possible even if it introduces more branches overall.
   [(seq?$ c1)
    (let$* ([seq_fst (seq-fst$ c1)]
            [seq_snd (seq-snd$ c1)])
      (cond$
       [(prj?$ seq_fst)
        (let$* ([comp_seq_snd (compose seq_snd c2)])
          (seq$ seq_fst comp_seq_snd))]
       ;; We have to priority failure on the right over injection
       [(fail?$ c2) c2]
       ;; Because of the typeing rule for coercions we know that
       ;; c2 must be a (I?;i) aka projection sequence because
       ;; it must have the type dyn => I.
       [else ;;  (inj?$ seq_snd) only thing that is left in normal form
        (let$* ([proj  (seq-fst$ c2)]
                [final (seq-snd$ c2)]
                [t1    (inj-type$ seq_snd)]
                [t2    (prj-type$ proj)]
                [lbl   (prj-label$ proj)]
                [comp_prj_inj (mk-crcn t1 t2 lbl)]
                [comp_fst_pi (compose seq_fst comp_prj_inj)])
          (compose comp_fst_pi final))]))]
   [(seq?$ c2)
    (cond$
     [(fail?$ c1) c1]
     [else
      ;; must be c1 & (g;I?)
      (let$* ([seq_fst (seq-fst$ c2)]
              [seq_snd (seq-snd$ c2)]
              [comp_c1_final (compose c1 seq_fst)])
        (seq$ comp_c1_final seq_snd))])]
   ;; All Branches from here out will have to check if c2 is failure
   ;; so we do it once to eliminate the possibility
   [(fail?$ c2) (if$ (fail?$ c1) c1 c2)]
   [(mediating-crcn?$ c1)
    (cond$
     [(fnC?$ c1) ;; c2 must be a Function Coercion
      (cond
        [(and (Quote-Coercion? c1) (Quote-Coercion? c2))
         (let* ([c1 (Quote-Coercion-const c1)]
                [c2 (Quote-Coercion-const c2)])
           (unless (and (Fn? c1) (Fn? c2))
             (error 'compose-fn-crcn$ "given ~a ~a" c1 c2))
           (match-define-values ((Fn _ c1-args c1-ret) (Fn _ c2-args c2-ret))
             (values c1 c2))
           (define arg* (map help-comp c2-args c1-args))
           (define ret  (help-comp c1-ret c2-ret))
           (Quote-Coercion (Fn (Fn-arity c1) arg* ret)))]
        [else (Compose-Fn-Coercion compose-uid c1 c2)])]
     [(ref?$ c1) ;; c2 must also be a reference coercion
      (let$* ([ref1_read  (ref-read$  c1)]
              [ref1_write (ref-write$ c1)]
              [ref2_read  (ref-read$  c2)]
              [ref2_write (ref-write$ c2)]
              [read  (compose ref1_read  ref2_read)]
              [write (compose ref2_write ref1_write)])
        (if$ (and$ (id?$ read) (id?$ write))
             (Quote-Coercion (Identity))
             (ref$ read write)))]
     [(tuple?$ c1)
      (if (and (Quote-Coercion? c1) (Quote-Coercion? c2))
          (let* ([c1 (Quote-Coercion-const c1)]
                 [c2 (Quote-Coercion-const c2)])
            (if (and (CTuple? c1) (CTuple? c2))
                (let* ([c1-args (CTuple-items c1)]
                       [c2-args (CTuple-items c2)]
                       [arg*    (map help-comp c1-args c2-args)])
                  (Quote-Coercion (CTuple (CTuple-num c2) arg*)))
                (error 'compose-coercions "given ~a ~a" c1 c2)))
          (Compose-Tuple-Coercion compose-uid c1 c2))]
     [(mrefC?$ c1)
      (let$* ([ref1_type  (mrefC-type$ c1)]
              [ref2_type  (mrefC-type$ c2)]
              [type3  (mk-glbt ref1_type  ref2_type)])
        (mrefC$ type3))]
     [(mvectC?$ c1)
      (let$* ([ref1_type  (mvectC-type$ c1)]
              [ref2_type  (mvectC-type$ c2)]
              [type3  (mk-glbt ref1_type  ref2_type)])
        (mvectC$ type3))]
     [else (Blame (Quote "bad implemention of mediating coercions"))])]
   ;; C1 must be a failed coercion
   [else (Blame (fail-label$ c1))]))

;; ic-expr maps over the expressions lowering function cast
;; into calls to the runtime cast interpreter and direct manipulation
;; of the representation for each type and value.
(: interpret-casts-in-expr :
   (String -> Uid)
   Uid
   Coerce-Type
   Compose-Coercions-Type
   Make-Coercion-Type
   Gbox-refT Gbox-setT Gvec-refT Gvec-setT Gvec-LengthT
   Mbox-refT Mbox-setT Mvec-refT Mvec-setT
   Dyn-Gbox-refT Dyn-Gbox-setT 
   Dyn-Gvec-refT Dyn-Gvec-setT
   Dyn-Mbox-refT Dyn-Mbox-setT
   Dyn-Mvec-refT Dyn-Mvec-setT
   Dyn-Fn-AppT
   CoC1-Expr
   ->
   CoC3-Expr)
(define (interpret-casts-in-expr
         next-uid!
         interp-uid interp-coercion interp-compose mk-coercion
         gbox-ref gbox-set! gvect-ref gvect-set! gvect-length
         mbox-ref mbox-set! mvect-ref mvect-set!
         dyn-gbox-ref dyn-gbox-set! 
         dyn-gvec-ref dyn-gvec-set!
         dyn-mbox-ref dyn-mbox-set!
         dyn-mvec-ref dyn-mvec-set!
         dyn-fn-app
         exp)
  ;; map through a code binding 
  (: recur-through-bndc* : CoC1-Bnd-Code -> CoC3-Bnd-Code)
  (define (recur-through-bndc* b)
    (match-let ([(cons u (Code u* e)) b])
      (cons u (Code u* (recur e)))))
  ;; map through a data binding 
  (: recur-through-bnd* : CoC1-Bnd -> CoC3-Bnd)
  (define (recur-through-bnd* b)
    (cons (car b) (recur (cdr b))))

  ;; map through a list of expressions
  (: recur* : CoC1-Expr* -> CoC3-Expr*)
  (define (recur* exps) (map recur exps))
  
  (: recur : CoC1-Expr -> CoC3-Expr)
  (define (recur exp)
    (define who 'recur)
    (debug who exp)
    (match exp
      ;; Interesting Cases -----------------------------------------------
      ;; Transformations for Casting runtime
      [(Interpreted-Cast (app recur v) (Coercion (app recur c)))
       (interp-coercion v c (Quote 0))]
      [(Cast (app recur e) (Coercion c))
       (interp-coercion e (Quote-Coercion c) (Quote 0))]
      [(Cast e (Coercion (Ref r w))) #:when #f
       ;; TODO (Andre) delete this branch
       ;; Here this specialization of reference coercions
       ;; was originally in the lower-reference-casts pass.
       ;; This seems redundant when placed next to the
       ;; specialization that occurs in this pass.
       ;; I am leaving it for now but we should delete this
       ;; code once we have confirmed doing so won't break
       ;; anything.
       (define e^ (recur e))
       (define-values (u o r^ w^)
         (values (next-uid! "ref_coercion")
                 (next-uid! "ref_old_ref")
                 (next-uid! "ref_read")
                 (next-uid! "ref_write")))
       (: do-ref-coercion-inline : (Var Uid) -> CoC3-Expr)
       (define (do-ref-coercion-inline v)
         (If (Guarded-Proxy-Huh v)
             (Let (list (cons u (Guarded-Proxy-Coercion v))
                        (cons o (Guarded-Proxy-Ref v)))
               (Let (list (cons r^ (interp-compose
                                    (Ref-Coercion-Read (Var u))
                                    (Quote-Coercion r)))
                          (cons w^ (interp-compose
                                    (Quote-Coercion w)
                                    (Ref-Coercion-Write (Var u)))))
                 (If (If (Id-Coercion-Huh (Var w^)) 
                         (Id-Coercion-Huh (Var r^))
                         (Quote #f))
                     (Var o)
                     (Guarded-Proxy (Var o)
                                    (Coercion (Ref-Coercion (Var r^) (Var w^)))))))
             (Guarded-Proxy v (Coercion (Quote-Coercion (Ref r w))))))
       (cond
         [(Var? e^) (do-ref-coercion-inline e^)]
         [else
          (define u (next-uid! "guarded-ref"))
          (Let (list (cons u e^)) (do-ref-coercion-inline (Var u)))])]
      [(Compose-Coercions c1 c2)
       (interp-compose (recur c1) (recur c2))]
      ;; Transformation to lower guarded reference types
      ;;-------------------------------------------------------------------
      ;; Every Guarded Reference Starts As an Unguarded box
      [(Gbox (app recur e)) (Unguarded-Box e)]
      ;; Unboxing calls off to the helpers we have defined
      [(Gunbox (app recur b))
       (if (Var? b)
           (gbox-ref b)
           (let ([u (next-uid! "gbox")])
             (Let (list (cons u b)) (gbox-ref (Var u)))))]
      ;; Setting a Gaurded reference results in iteratively applying
      ;; all the guarding casts to the value to be written.
      ;; Most of the work is already done but the code requires values
      ;; so there is a little repetative to let bind any values that
      ;; haven't been evaluated.
      [(Gbox-set! (app recur b) (app recur w))
       (match-define-values (b* (list b^ w^))
         (bnd-non-vars next-uid! (list b w) #:names '("gbox" "write_val")))
       (if (null? b*)
           (gbox-set! b^ w^)
           (Let b* (gbox-set! b^ w^)))]
      [(Gvector (app recur size) (app recur init)) (Unguarded-Vect size init)]
      [(Gvector-ref (app recur v) (app recur i))
       (match-define-values (b* (list v^ i^))
         (bnd-non-vars next-uid! (list v i) #:names '("gvec" "index")))
       (cond
         [(null? b*) (gvect-ref v^ i^)]
         [else (Let b* (gvect-ref v^ i^))])]
      [(Gvector-set! (app recur v) (app recur i) (app recur w))
       (match-define-values (b* (list v^ i^ w^))
         (bnd-non-vars next-uid! (list v i w)
                       #:names '("gvec" "index" "write_val")))
       (if (null? b*)
           (gvect-set! v^ i^ w^)
           (Let b* (gvect-set! v^ i^ w^)))]
      [(Gvector-length e)
       (if (Var? e)
           (gvect-length e)
           (let ([u (next-uid! "gvect")])
             (Let (list (cons u (recur e))) (gvect-length (Var u)))))]
      [(Mbox (app recur e) t) (Mbox e t)]
      [(Munbox (app recur e)) (Mbox-val-ref e)]
      [(Mbox-set! (app recur e1) (app recur e2))
       (Mbox-val-set! e1 e2)]
      [(MBoxCastedRef addr t)
       (match-define-values (b* (list t^))
         (bnd-non-vars next-uid! (list (Type t)) #:names '("type")))
       (if (null? b*)
           (mbox-ref (Var addr) t^)
           (Let b* (mbox-ref (Var addr) t^)))]
      [(MBoxCastedSet! addr (app recur e) t)
       (match-define-values (b* (list e^ t^))
         (bnd-non-vars next-uid! (list e (Type t)) #:names '("write_val" "type")))
       (if (null? b*)
           (mbox-set! (Var addr) e^ t^)
           (Let b* (mbox-set! (Var addr) e^ t^)))]
      [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
      [(Mvector-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
      [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
       (Mvector-val-set! e1 e2 e3)]
      [(MVectCastedRef addr (app recur i) t)
       (match-define-values (b* (list i^ t^))
         (bnd-non-vars next-uid! (list i (Type t)) #:names '("index" "type")))
       (if (null? b*)
           (mvect-ref (Var addr) i^ t^)
           (Let b* (mvect-ref (Var addr) i^ t^)))]
      [(MVectCastedSet! addr (app recur i) (app recur e) t)
       (match-define-values (b* (list i^ e^ t^))
         (bnd-non-vars next-uid! (list i e (Type t)) #:names '("index" "write_val" "type")))
       (if (null? b*)
           (mvect-set! (Var addr) i^ e^ t^)
           (Let b* (mvect-set! (Var addr) i^ e^ t^)))]
      [(Mvector-length e) (Mvector-length (recur e))]
      
      ;; The translation of the dynamic operation 
      [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
       (define arg-names
         `("blame_info" "dyn_fn" . ,(make-list (length e*) "dyn_fn_arg")))
       (match-define-values (b* (cons lbl (cons v v*)))
         #;(bnd-non-vars next-uid! (cons e (cons l e*)) #:names arg-names)
         (bnd-non-vars next-uid! (cons (Quote l) (cons e e*)) #:names arg-names))
       #;(printf "b*=~a\nlbl=~a\nv=~a\nv*=~a\n\n" b* lbl v v*)
       (define r (Let b* (dyn-fn-app v v* t* lbl)))
       #;(printf "r=~a" r)
       r]
      [(Dyn-GRef-Ref (app recur e) l)
       (match-define-values (b* (list e^ l^))
         (bnd-non-vars next-uid! (list e (Quote l))
                       #:names (list "dyn_gbox" "blame_info")))
       (Let b* (dyn-gbox-ref e^ l^))]
      [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
       (match-define-values (b* (list e1^ e2^ l^))
         (bnd-non-vars next-uid! (list e1 e2 (Quote l))
                       #:names (list "dyn_gbox" "write_val" "blame_info")))
       (Let b* (dyn-gbox-set! e1^ e2^ (Type t) l^))]
      [(Dyn-GVector-Ref (app recur e) (app recur i) l)
       (match-define-values (b* (list e^ i^ l^))
         (bnd-non-vars next-uid! (list e i (Quote l))
                       #:names (list "dyn_gvec" "index" "blame_info")))
       (define r (Let b* (dyn-gvec-ref e^ i^ l^)))
       #;(printf "r=~a\n\n" r)
       r]
      [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (match-define-values (b* (list e1^ i^ e2^ l^))
         (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
                       #:names (list "dyn_gvec" "index" "write_val" "blame_info")))
       (Let b* (dyn-gvec-set! e1^ i^ e2^ (Type t) l^))]
      [(Dyn-MRef-Ref (app recur e) l)
       (match-define-values (b* (list e^ l^))
         (bnd-non-vars next-uid! (list e (Quote l))
                       #:names (list "dyn_mbox" "blame_info")))
       (Let b* (dyn-mbox-ref e^ l^))]
      [(Dyn-MRef-Set! (app recur e1) (app recur e2) t l)
       (match-define-values (b* (list e1^ e2^ l^))
         (bnd-non-vars next-uid! (list e1 e2 (Quote l))
                       #:names (list "dyn_mbox" "write_val" "blame_info")))
       (Let b* (dyn-mbox-set! e1^ e2^ (Type t) l^))]
      [(Dyn-MVector-Ref (app recur e) (app recur i) l)
       (match-define-values (b* (list e^ i^ l^))
         (bnd-non-vars next-uid! (list e i (Quote l))
                       #:names (list "dyn_mvec" "index" "blame_info")))
       (Let b* (dyn-mvec-ref e^ i^ l^))]
      [(Dyn-MVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (match-define-values (b* (list e1^ i^ e2^ l^))
         (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
                       #:names (list "dyn_mvec" "index" "write_val" "blame_info")))
       (Let b* (dyn-mvec-set! e1^ i^ e2^ (Type t) l^))]
      ;; We should consider desugaring the data structure rep
      ;; here if we are using it. That translation makes more
      ;; sense but is weird because we defer hybrid until
      ;; closure conversion. 
      [(Fn-Proxy i e1 e2)
       (Fn-Proxy (list i interp-uid) (recur e1) (recur e2))]
      [(App/Fn-Proxy-Huh e e*)
       (App-Fn-or-Proxy interp-uid (recur e) (map recur e*))]
      [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (recur e))]
      [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (recur e))]
      ;; In theory it may be better to implement the tranformation
      ;; for apply here but it is currently postponed until
      ;; closure conversion.
      [(App-Fn e e*) (App-Fn (recur e) (map recur e*))]
      ;; Uniteresting Recursion till the end
      [(Id-Coercion-Huh e)
       (Id-Coercion-Huh (recur e))]
      [(Fn-Coercion e* e)
       (Fn-Coercion (map recur e*) (recur e))]
      [(Fn-Coercion-Arg e1 e2)
       (Fn-Coercion-Arg (recur e1) (recur e2))]
      [(Fn-Coercion-Return e)
       (Fn-Coercion-Return (recur e))]
      [(Ref-Coercion e1 e2)
       (Ref-Coercion (recur e1) (recur e2))]
      [(Ref-Coercion-Read e)
       (Ref-Coercion-Read (recur e))]
      [(Ref-Coercion-Write e)
       (Ref-Coercion-Write (recur e))]
      [(Quote-Coercion c)
       (Quote-Coercion c)]
      [(Observe e t) (Observe (recur e) t)]
      [(and noop (No-Op)) noop]
      [(Code-Label u)
       (Code-Label u)]
      [(Labels c* e)
       (Labels (map recur-through-bndc* c*) (recur e))]
      [(App-Code e e*)
       (App-Code (recur e) (map recur e*)) ]
      [(Lambda f* (Castable ctr e))
       (Lambda f* (Castable ctr (recur e)))]
      [(Letrec b* e)
       (Letrec (map recur-through-bnd* b*) (recur e))]
      [(Let b* e)
       (Let (map recur-through-bnd* b*) (recur e))]
      [(Op p e*)
       (Op p (map recur e*))]
      [(Fn-Caster e)
       (Fn-Caster (recur e))]
      ;; Type manipulation
      [(Type-Fn-arg e i)
       (Type-Fn-arg (recur e) (recur i))]
      [(Type-Fn-return e)
       (Type-Fn-return (recur e))]
      [(Type-Fn-arity e)
       (Type-Fn-arity (recur e))]
      [(Blame e)
       (Blame (recur e))]
      [(If tst csq alt)
       (If (recur tst) (recur csq) (recur alt))]
      [(Switch e c* d)
       (: recur-case : (Switch-Case CoC1-Expr) -> (Switch-Case CoC3-Expr))
       (define/match (recur-case c)
         [((cons l r)) (cons l (recur r))])
       (Switch (recur e) (map recur-case c*) (recur d))]
      [(Var i) (Var i)]
      [(Type t) (Type t)]
      [(Quote k) (Quote k)]
      [(Begin e* e)
       (Begin (map recur e*) (recur e))]
      [(Repeat i e1 e2 a e3 e4)
       (Repeat i (recur e1) (recur e2) a (recur e3) (recur e4))]
      ;; Proxies for functions
      [(Fn-Proxy-Coercion e)
       (Fn-Proxy-Coercion (recur e))]
      [(Create-tuple e*)
       (Create-tuple (map recur e*))]
      [(Tuple-proj e i) (Tuple-proj (recur e) (Quote i))]
      ;; Coercions manipulation
      [(or (Guarded-Proxy-Source _) (Guarded-Proxy-Target _)
           (Guarded-Proxy-Blames _))
       (error 'interpret-casts "twosome code in coercion pass: ~a" exp)]
      [other (error 'interpret-casts "umatched ~a" other)]))
  (recur exp))


;; TODO: fix smart cast to take the mono rt

(define-type Smart-Cast-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
       (#:t1-not-dyn Boolean #:t2-not-dyn Boolean)
       CoC3-Expr))
(: make-smart-cast :
   (String -> Uid)
   Coerce-Type
   Make-Coercion-Type
   -> Smart-Cast-Type)
(define ((make-smart-cast next-uid! cast mk-coercion)
         val t1 t2 lbl mono_type
         #:t1-not-dyn [t1-not-dyn #f]
         #:t2-not-dyn [t2-not-dyn #f])
  (match* (val t1 t2)
    [(val (Type t) (Type t)) val]
    [(val (Type (Dyn)) t2) #:when t2-not-dyn
     (match val
       [(or (Var _) (Quote _)) (cast val (Project-Coercion t2 lbl) mono_type)]
       [other
        (define u (next-uid! "tmp"))
        (Let (list (cons u val))
          (cast (Var u) (Project-Coercion t2 lbl) mono_type))])]
    [((or (Var _) (Quote _)) (Type (Dyn)) t2)
     (If (Type-Dyn-Huh t2) val (cast val (Project-Coercion t2 lbl) mono_type))]
    [(val t1 (Type (Dyn))) #:when t1-not-dyn
     (cast val (Inject-Coercion t1) mono_type)]
    [((Var _) t1 (Type (Dyn)))
     (If (Type-Dyn-Huh t1) val (cast val (Inject-Coercion t1) mono_type))]
    [(val t1 t2)
     (cast val (mk-coercion t1 t2 lbl) mono_type)]))

(define-type Dyn-Gbox-refT
  ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-Gbox-setT
  ((Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
(define-type Dyn-Gvec-refT
  ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-Gvec-setT
  ((Var Uid) (Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid)
   -> CoC3-Expr))
(define-type Dyn-Fn-AppT
  ((Var Uid) (Listof (Var Uid)) Schml-Type* (Var Uid) -> CoC3-Expr))

(: make-dyn-gbox-ref-code :
   (String -> Uid)
   Gbox-refT
   Smart-Cast-Type
   -> Dyn-Gbox-refT)
(define ((make-dyn-gbox-ref-code next-uid! gb-ref $cast) dyn lbl)
  (define-values (val ty tyof read-val)
    (values (next-uid! "dyn_unbox_val")
            (next-uid! "dyn_unbox_ty")
            (next-uid! "dyn_unbox_tyof")
            (next-uid! "dyn_unbox_read_val")))
  (define-values (var-val var-ty var-tyof var-read-val)
    (values (Var val) (Var ty) (Var tyof) (Var read-val)))
  (Let `((,val . ,(Dyn-value dyn))
         (,ty . ,(Dyn-type dyn)))
    (If (Type-GRef-Huh var-ty)
        (Let `([,tyof . ,(Type-GRef-Of var-ty)]
               [,read-val . ,(gb-ref var-val)])
          ($cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
        (Blame lbl))))

(: make-dyn-gbox-set!-code :
   (String -> Uid)
   Gbox-setT
   Smart-Cast-Type
   -> Dyn-Gbox-setT)
(define ((make-dyn-gbox-set!-code next-uid! gb-set! $cast) dyn-gbox wrt-val1 t2 info)
  (define-values (gbox ty tyof wrt-val2 wrt-val3)
    (values (next-uid! "dyn_setbox_gbox")
            (next-uid! "dyn_setbox_ty")
            (next-uid! "dyn_setbox_tyof")
            (next-uid! "dyn_setbox_wrt_val2")
            (next-uid! "dyn_setbox_wrt_val3")))
  (define-values (gbox-v ty-v tyof-v wrt-val2-v wrt-val3-v)
    (values (Var gbox) (Var ty) (Var tyof) (Var wrt-val2) (Var wrt-val3)))
  (Let `((,gbox . ,(Dyn-value dyn-gbox))
         (,ty   . ,(Dyn-type dyn-gbox)))
    (If (Type-GRef-Huh ty-v)
        (Let `([,tyof . ,(Type-GRef-Of ty-v)])
          (If (Type-Dyn-Huh tyof-v)
              (Let `([,wrt-val2 . ,($cast wrt-val1 t2 (Type DYN-TYPE) info (Quote 0))])
                (gb-set! gbox-v wrt-val2-v))
              (Let `([,wrt-val3 . ,($cast wrt-val1 t2 tyof-v info (Quote 0) #:t2-not-dyn #t)])
                ($cast (gb-set! gbox-v wrt-val3-v)
                       tyof-v (Type DYN-TYPE) info (Quote 0)
                       #:t1-not-dyn #t))))
        (Blame info))))

(: make-dyn-gvect-ref-code :
   (String -> Uid)  Gvec-refT Smart-Cast-Type
   -> Dyn-Gvec-refT)
(define ((make-dyn-gvect-ref-code next-uid! gv-ref $cast) dyn ind lbl)
  (define-values (val ty tyof read-val)
    (values (next-uid! "dyn_gvec_ref_val")
            (next-uid! "dyn_gvec_ref_ty")
            (next-uid! "dyn_gvec_ref_tyof")
            (next-uid! "dyn_gvec_ref_read_val")))
  (define-values (var-val var-ty var-tyof var-read-val)
    (values (Var val) (Var ty) (Var tyof) (Var read-val)))
  (Let `((,val . ,(Dyn-value dyn))
         (,ty . ,(Dyn-type dyn)))
    (If (Type-GVect-Huh var-ty)
        (Let `([,tyof . ,(Type-GVect-Of var-ty)]
               [,read-val . ,(gv-ref var-val ind)])
          ($cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
        (Blame lbl))))

(: make-dyn-gvect-set!-code :
   (String -> Uid)
   Gvec-setT
   Smart-Cast-Type
   -> Dyn-Gvec-setT)
(define ((make-dyn-gvect-set!-code next-uid! gv-set! $cast)
         dyn-gvec ind wrt-val1 t2 info)
  (define-values (val ty tyof wrt-val2 wrt-val3)
    (values (next-uid! "dyn_setbox_gvect_value")
            (next-uid! "dyn_setbox_ty")
            (next-uid! "dyn_setbox_tyof")
            (next-uid! "dyn_setbox_write_value")
            (next-uid! "dyn_setbox_write_value3")))
  (define-values (val-v ty-v tyof-v wrt-val2-v wrt-val3-v)
    (values (Var val) (Var ty) (Var tyof) (Var wrt-val2) (Var wrt-val3)))
  (Let `((,val . ,(Dyn-value dyn-gvec))
         (,ty . ,(Dyn-type dyn-gvec)))
    (If (Type-GVect-Huh ty-v)
        (Let `([,tyof . ,(Type-GVect-Of ty-v)])
          (If (Type-Dyn-Huh tyof-v)
              (Let `([,wrt-val2 . ,($cast wrt-val1 t2 (Type DYN-TYPE)
                                          info (Quote 0))])
                (gv-set! val-v ind wrt-val2-v))
              (Let `([,wrt-val3 . ,($cast wrt-val1 t2 tyof-v
                                          info (Quote 0) #:t2-not-dyn #t)])
                ($cast (gv-set! val-v ind wrt-val3-v) 
                       tyof-v (Type DYN-TYPE) info
                       (Quote 0) #:t1-not-dyn #t))))
        (Blame info))))


(define-type Dyn-Mbox-refT
  ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-Mbox-setT
  ((Var Uid) (Var Uid)
   (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
(define-type Dyn-Mvec-refT
  ((Var Uid) (Var Uid) (Var Uid)
   -> CoC3-Expr))
(define-type Dyn-Mvec-setT
  ((Var Uid) (Var Uid) (Var Uid)
   (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))

(: make-dyn-mbox-ref-code :
   (String -> Uid)
   Mbox-refT
   -> Dyn-Mbox-refT)
(define ((make-dyn-mbox-ref-code next-uid! mb-ref) dyn lbl)
  (define-values (val ty dynty)
    (values (next-uid! "dyn_unbox_val")
            (next-uid! "dyn_unbox_ty")
            (next-uid! "dyn_type")))
  (define-values (var-val var-ty var-dynty)
    (values (Var val) (Var ty) (Var dynty)))
  (Let `((,val . ,(Dyn-value dyn))
         (,ty . ,(Dyn-type dyn)))
    (If (Type-MRef-Huh var-ty)
        (Let `([,dynty . ,(Type DYN-TYPE)])
          (mb-ref var-val var-dynty))
        (Blame lbl))))

(: make-dyn-mbox-set!-code :
   (String -> Uid)
   Mbox-setT
   Smart-Cast-Type
   -> Dyn-Mbox-setT)
(define ((make-dyn-mbox-set!-code next-uid! mb-set! $cast) dyn-mbox wrt-val1 t2 info)
  (define-values (mbox ty tyof t2u)
    (values (next-uid! "dyn_setbox_mbox")
            (next-uid! "dyn_setbox_ty")
            (next-uid! "dyn_setbox_tyof")
            (next-uid! "t2_type")))
  (define-values (mbox-v ty-v tyof-v t2u-v)
    (values (Var mbox) (Var ty) (Var tyof) (Var t2u)))
  (Let `([,mbox . ,(Dyn-value dyn-mbox)]
         [,ty   . ,(Dyn-type dyn-mbox)])
    (If (Type-MRef-Huh ty-v)
        (Let `([,tyof . ,(Type-MRef-Of ty-v)]
               [,t2u .  ,t2])
          (If (Type-Dyn-Huh tyof-v)
              ($cast (mb-set! mbox-v wrt-val1 t2u-v)
                     (Type UNIT-TYPE) (Type DYN-TYPE) info
                     (Quote 0))
              (mb-set! mbox-v wrt-val1 t2u-v)))
        (Blame info))))

(: make-dyn-mvect-ref-code :
   (String -> Uid)  Mvec-refT
   -> Dyn-Mvec-refT)
(define ((make-dyn-mvect-ref-code next-uid! mv-ref) dyn ind lbl)
  (define-values (val ty dynty)
    (values (next-uid! "dyn_mvec_ref_val")
            (next-uid! "dyn_mvec_ref_ty")
            (next-uid! "dyn_type")))
  (define-values (var-val var-ty var-dynty)
    (values (Var val) (Var ty) (Var dynty)))
  (Let `((,val . ,(Dyn-value dyn))
         (,ty . ,(Dyn-type dyn)))
    (If (Type-MVect-Huh var-ty)
        (Let `([,dynty . ,(Type DYN-TYPE)])
          (mv-ref var-val ind var-dynty))
        (Blame lbl))))

(: make-dyn-mvect-set!-code :
   (String -> Uid)
   Mvec-setT
   Smart-Cast-Type
   -> Dyn-Mvec-setT)
(define ((make-dyn-mvect-set!-code next-uid! mv-set! $cast)
         dyn-mvec ind wrt-val1 t2 info)
  (define-values (val ty tyof t2u)
    (values (next-uid! "dyn_setbox_mvect_value")
            (next-uid! "dyn_setbox_ty")
            (next-uid! "dyn_setbox_tyof")
            (next-uid! "t2_type")))
  (define-values (val-v ty-v tyof-v t2u-v)
    (values (Var val) (Var ty) (Var tyof) (Var t2u)))
  (Let `((,val . ,(Dyn-value dyn-mvec))
         (,ty . ,(Dyn-type dyn-mvec)))
    (If (Type-MVect-Huh ty-v)
        (Let `([,tyof . ,(Type-MVect-Of ty-v)]
               [,t2u .  ,t2])
          (If (Type-Dyn-Huh tyof-v)
              ($cast (mv-set! val-v ind wrt-val1 t2u-v)
                     (Type UNIT-TYPE) (Type DYN-TYPE) info (Quote 0))
              (mv-set! val-v ind wrt-val1 t2u-v)))
        (Blame info))))


(: make-dyn-fn-app-code : (String -> Uid) Smart-Cast-Type -> Dyn-Fn-AppT)
(define ((make-dyn-fn-app-code next-uid! $cast) v v* t* l)
  (define-values (val ty ret-val ret-ty)
    (values (next-uid! "dyn_fn_val")
            (next-uid! "dyn_fn_ty")
            (next-uid! "dyn_fn_ret_val")
            (next-uid! "dyn_fn_ret_ty")))
  (define arg-casts : CoC3-Expr*
    (for/list : (Listof CoC3-Expr)
              ([v : CoC3-Expr v*]
               [t : Schml-Type t*]
               [i (in-naturals)])
      (define tyi (next-uid! (string-append "dyn_fn_ty" (number->string i))))
      (Let `([,tyi . ,(Type-Fn-arg (Var ty) (Quote i))])
        ($cast v (Type t) (Var tyi) l (Quote 0)))))
  (define casts-apply : CoC3-Expr
    (case (function-cast-representation)
      [(Hybrid) (App-Fn (Var val) arg-casts)]
      [(Data) (error 'todo "implement coercions data representation")]
      [(Functional) (error 'todo "incompatible with coercions")]
      [else (error 'interp-cast-with-coercions/dyn-fn-app "unexpected value")]))
  (Let `([,val . ,(Dyn-value v)]
         [,ty . ,(Dyn-type v)])
    (If (Type-Fn-Huh (Var ty))
        (Let `([,ret-val . ,casts-apply]
               [,ret-ty . ,(Type-Fn-return (Var ty))])
          ($cast (Var ret-val) (Var ret-ty) (Type DYN-TYPE) l
                 (Quote 0)))
        (Blame l))))


;; Functions for use sites of guarded references with coercions
(define-type Gbox-refT    ((Var Uid) -> CoC3-Expr))
(define-type Gbox-setT    ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Gvec-refT    ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Gvec-setT    ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Gvec-LengthT ((Var Uid) -> CoC3-Expr))

(: make-gbox-ref-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gbox-refT))
(define ((make-gbox-ref-code cast) gref)
  (If (Guarded-Proxy-Huh gref)
      (cast
       (Unguarded-Box-Ref (Guarded-Proxy-Ref gref))
       (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))
       (Quote 0))
      (Unguarded-Box-Ref gref)))

(: make-gbox-set!-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gbox-setT))
(define ((make-gbox-set!-code cast) gref val)
  (If (Guarded-Proxy-Huh gref)
      (Unguarded-Box-Set!
       (Guarded-Proxy-Ref gref)
       (cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)) (Quote 0)))
      (Unguarded-Box-Set! gref val)))

(: make-gvect-ref-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gvec-refT))
(define ((make-gvect-ref-code cast) gref index)
  (If (Guarded-Proxy-Huh gref)
      (cast (Unguarded-Vect-Ref (Guarded-Proxy-Ref gref) index)
            (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))
            (Quote 0))
      (Unguarded-Vect-Ref gref index)))

(: make-gvect-length-code (-> Gvec-LengthT))
(define ((make-gvect-length-code) gvect)
  (If (Guarded-Proxy-Huh gvect)
      (Unguarded-Vect-length (Guarded-Proxy-Ref gvect))
      (Unguarded-Vect-length gvect)))

(: make-gvect-set!-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gvec-setT))
(define ((make-gvect-set!-code cast) gref index val)
  (: index-exp CoC3-Expr)
  (define index-exp (if (integer? index) (Quote index) index))
  (If (Guarded-Proxy-Huh gref)
      (Unguarded-Vect-Set!
       (Guarded-Proxy-Ref gref)
       index-exp
       (cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)) (Quote 0)))
      (Unguarded-Vect-Set! gref index-exp val)))

(define-type Mbox-refT    ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Mbox-setT    ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Mvec-refT    ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Mvec-setT    ((Var Uid) (Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

(: make-mbox-ref-code
   ((String -> Uid)
    Cast-With-MAddr-Type
    -> Mbox-refT))
(define ((make-mbox-ref-code next-uid! cast) mref t2)
  (let ([t1 (next-uid! "t1")]
        [c  (next-uid! "crcn")])
    (Let (list (cons t1 (Mbox-rtti-ref mref)))
      (cast (Mbox-val-ref mref) (Var t1) t2 (Quote "") (Quote 0)))))

(: make-mbox-set!-code
   ((String -> Uid)
    Cast-With-MAddr-Type
    -> Mbox-setT))
(define ((make-mbox-set!-code next-uid! cast) mref val t1)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([t2 (Mbox-rtti-ref mref)]
          [cv (cond$
               [(and$ (tupleT?$ t1) (tupleT?$ t2))
                (let$* ([n (Type-Tuple-num t2)]
                        [ctv (Copy-Tuple n val)])
                  (Begin
                    (list
                     (Mbox-val-set! mref ctv))
                    ctv))]
               [else val])]
          [ccv (cast cv t1 t2 (Quote "monotonic references currently does not track blame") mref)]
          [t2-new (Mbox-rtti-ref mref)])
    (begin
      (if$ (op=? t2 t2-new)
           (Mbox-val-set! mref ccv)
           (Quote 0)))))

(: make-mvect-ref-code
   ((String -> Uid)
    Cast-With-MAddr-Type
    -> Mvec-refT))
(define ((make-mvect-ref-code next-uid! cast) mvect i t2)
  (let ([t1 (next-uid! "t1")]
        [c  (next-uid! "crcn")])
    (Let (list (cons t1 (Mvector-rtti-ref mvect)))
      (cast (Mvector-val-ref mvect i) (Var t1) t2 (Quote "") (Quote 0)))))

(: make-mvect-set!-code
   ((String -> Uid)
    Cast-With-MAddr-Type
    -> Mvec-setT))
(define ((make-mvect-set!-code next-uid! cast) mvect i val t1)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([t2 (Mvector-rtti-ref mvect)])
    (cond$
     [(and$ (tupleT?$ t1) (tupleT?$ t2))
      (let$* ([n (Type-Tuple-num t2)]
              [cvi (Copy-Tuple n val)])
        (Begin
          (list
           (Mvector-val-set! mvect i cvi))
          (let$* ([ccvi (cast
                         cvi t1 t2
                         (Quote "monotonic references currently does not track blame")
                         mvect)]
                  [t2-new (Mvector-rtti-ref mvect)])
            (if$ (op=? t2 t2-new)
                 (Mvector-val-set! mvect i ccvi)
                 (Quote 0)))))]
     [else
      (let$* ([cvi (cast
                    val t1 t2
                    (Quote "monotonic references currently does not track blame")
                    mvect)]
              [t2-new (Mvector-rtti-ref mvect)])
        (if$ (op=? t2 t2-new)
             (Mvector-val-set! mvect i cvi)
             (Quote 0)))])))
