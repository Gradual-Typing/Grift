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
  (define interp-cast-uid (next-uid! "interp_coercion"))

  (: interp-cast-call : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
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


  (define gen-compose-coercions-code : Compose-Coercions-Type
    (make-compose-coercions-code next-uid! compose-coercions-call
                                 compose-coercions-uid make-coercion))

  (define compose-coercions : Compose-Coercions-Type
    (cond
      [(open-coded? 'compose-coercions) gen-compose-coercions-code]
      [else compose-coercions-call]))

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
  
  ;; Code generators for the coercion casting runtime
  (define gen-interp-cast-code
    (make-cast-code next-uid! interp-cast-call interp-cast-uid
                    make-coercion compose-coercions
                    greatest-lower-bound-type
                    copy-value-in-monoref))

  (define interp-cast
    (cond
      [(open-coded? 'interp-cast) gen-interp-cast-code]
      [else interp-cast-call]))
  
  (define bindings-needed-for-interp-cast
    (let ([interp-v (next-uid! "value")]
          [interp-c (next-uid! "coercion")]
          [interp-a (next-uid! "mono-address")]
          [mc-t1    (next-uid! "type1")]
          [mc-t2    (next-uid! "type2")]
          [mc-lbl   (next-uid! "blame_info")])
      `([,interp-cast-uid
         . ,(Code (list interp-v interp-c interp-a)
              (gen-interp-cast-code (Var interp-v) (Var interp-c) (Var interp-a)))]
        [,make-coercion-uid
         . ,(Code (list mc-t1 mc-t2 mc-lbl)
              (gen-make-coercion-code (Var mc-t1) (Var mc-t2) (Var mc-lbl)))])))
  
  (define bindings-needed-for-monotonic-refs
    (let ([glbt-t1    (next-uid! "type1")]
          [glbt-t2    (next-uid! "type2")]
          [a          (next-uid! "mono-address")])
      `([,greatest-lower-bound-type-uid
         . ,(Code (list glbt-t1 glbt-t2)
              (gen-greatest-lower-bound-type (Var glbt-t1) (Var glbt-t2)))]
        [,copy-value-in-monoref-uid
         . ,(Code (list a)
              (gen-copy-value-in-monoref (Var a)))])))

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
  (: bindings-needed-for-guarded : CoC3-Bnd-Code*)
  (define-values (gbox-ref gbox-set! gvec-ref gvec-set!
                           bindings-needed-for-guarded)
    ;; First we create initialize the code generators
    (let ([gen-gbox-ref-code (make-gbox-ref-code interp-cast-call)]
          [gen-gbox-set!-code (make-gbox-set!-code interp-cast-call)]
          [gen-gvec-ref-code (make-gvect-ref-code interp-cast-call)]
          [gen-gvec-set!-code (make-gvect-set!-code interp-cast-call)])
      (cond
        [(inline-guarded-branch?)
         ;; we just hand back the code generators to build
         ;; inline code everywhere.
         (values gen-gbox-ref-code gen-gbox-set!-code
                 gen-gvec-ref-code gen-gvec-set!-code
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
               [gvs-v (next-uid! "val")])
           (values
            (apply-code gbr) (apply-code gbs)
            (apply-code gvr) (apply-code gvs)
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
                                        (Var gvs-v)))])))])))


  (: dyn-fn-app : Dyn-Fn-AppT)
  (: dyn-gbox-ref : Dyn-Gbox-refT)
  (: dyn-gbox-set! : Dyn-Gbox-setT)
  (: dyn-gvec-ref  : Dyn-Gvec-refT)
  (: dyn-gvec-set! : Dyn-Gvec-setT)
  (: bindings-needed-for-dynamic-operations : CoC3-Bnd-Code*)
  (define-values (dyn-fn-app
                  dyn-gbox-ref dyn-gbox-set!
                  dyn-gvec-ref dyn-gvec-set!
                  bindings-needed-for-dynamic-operations)
    (let* ([smart-cast
            (make-smart-cast next-uid! interp-cast-call make-coercion)]
           [gen-dyn-fn-app
            (make-dyn-fn-app-code next-uid! smart-cast)]
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
          (th-error 'interpret-cast-with-coercions/dyn-fn-app)
          (th-error 'interpret-cast-with-coercions/dyn-gbox-ref)
          (th-error 'interpret-cast-with-coercions/dyn-gbox-set!)
          (th-error 'interpret-cast-with-coercions/dyn-gvec-ref)
          (th-error 'interpret-cast-with-coercions/dyn-gvec-set!)
          '())]
        [(inline)
         (values
          gen-dyn-fn-app
          gen-dyn-gbox-ref gen-dyn-gbox-set!
          gen-dyn-gvec-ref gen-dyn-gvec-set!
          '())]
        [else
         ;; TODO come up with a solution that doesn't manually
         ;; require all the variable initializations
         (let ([gbr   (next-uid! "rt_dyn_gbox_ref")]
               [gbr-b (next-uid! "box")]
               [gbr-l (next-uid! "blame_info")]
               [gbs   (next-uid! "rt_dyn_gbox_set")]
               [gbs-b (next-uid! "box")]
               [gbs-v (next-uid! "write_val")]
               [gbs-t (next-uid! "val_type")]
               [gbs-l (next-uid! "blame_info")]
               [gvr   (next-uid! "rt_dyn_gvec_ref")]
               [gvr-r (next-uid! "vec")]
               [gvr-i (next-uid! "ind")]
               [gvr-l (next-uid! "blame_info")]
               [gvs   (next-uid! "rt_dyn_gvec_set")]
               [gvs-r (next-uid! "vec")]
               [gvs-i (next-uid! "ind")]
               [gvs-v (next-uid! "val")]
               [gvs-t (next-uid! "val_type")]
               [gvs-l (next-uid! "blame_info")])
           (values
            gen-dyn-fn-app
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

  
  ;; Next we generate the bindings for code that needs to be
  ;; included as the runtime.
  (define gradual-runtime-bindings : CoC3-Bnd-Code*
    (append
     bindings-needed-for-guarded
     bindings-needed-for-dynamic-operations
     bindings-needed-for-space-efficiency
     bindings-needed-for-monotonic-refs
     bindings-needed-for-interp-cast))

  (define exp-with-lowered-gradual-operations
    (interpret-casts-in-expr
     next-uid!
     interp-cast-uid interp-cast compose-coercions make-coercion
     gbox-set! gbox-ref gvec-set! gvec-ref
     dyn-gbox-set! dyn-gbox-ref dyn-gvec-set! dyn-gvec-ref dyn-fn-app
     prgm-exp))
  
  (Prog (list prgm-name (unique-counter-next! unique-counter) prgm-type)
    (Labels gradual-runtime-bindings
      (Observe  exp-with-lowered-gradual-operations prgm-type))))

;; These templates are used to build the code that performs
;; casting at runtime. The templates are a little over
;; parameterized currently in hopes that it make specialization
;; and customization easier in the future.
(define-type Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(: make-cast-code :
   (String -> Uid) Cast-Type Uid Make-Coercion-Type
   Compose-Coercions-Type GreatestLowerBound-Type
   CopyValueInMonoRef-Type
   ->
   Cast-Type)
(define ((make-cast-code next-uid! cast cast-u mk-crcn comp-crcn glbt copy-val-monoref) v c mono_type)
  (define who 'make-cast-code)
  (debug who v c)
  ;; schml: /home/deyaa/mono/Schml/src/casts/interpret-casts-with-coercions.rkt:354:2:
  ;; who='make-cast-code
  ;; v=(Var (Uid "r0" 0))
  ;; c=(Quote-Coercion (MonoVect (STuple 3 (list #0=(Int) #1=(Dyn) (MVect (STuple 3 (list #0# #0# #1#)))))))

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
              [fst_cast_value (cast val seq_fst mono_type)])
        (cast fst_cast_value seq_snd mono_type))]
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
          (cast dyn_value projected_type mono_type)))]
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
                  (let$* ([t1 (Mbox-rtti-ref a)]
                          [t3 (glbt t1 t2)])
                    (if$ (op=? t1 t3)
                         val
                         (Begin
                           (list
                            (Mbox-rtti-set! a t3))
                           (let$* ([vv (copy-val-monoref val)]
                                   [c (mk-crcn t1 t3 (Quote ""))]
                                   [cv (cast vv c val)]
                                   [t4 (Mbox-rtti-ref a)])
                             (if$ (op=? t3 t4)
                                  (Begin
                                    (list (Mbox-val-set! val cv))
                                    val)
                                  val)))))))]
          [other (error 'interp-cast/mrefC "unmatched value ~a" other)])]
       [(mvectC?$ crcn)
        (match val
          [(Var a)
           (let$* ([t2 (mvectC-type$ crcn)])
             (if$ (dyn?$ t2)
                  val
                  (let$* ([t1 (Mvector-rtti-ref a)]
                          [t3 (glbt t1 t2)])
                    (if$ (op=? t1 t3)
                         val
                         (Begin
                           (list
                            (Mvector-rtti-set! a t3)
                            (let$* ([c (mk-crcn t1 t3 (Quote ""))]
                                    [vn (Mvector-size val)])
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
                                                (let$* ([ccvi (cast cvi c val)]
                                                        [t4 (Mvector-rtti-ref a)])
                                                  (if$ (op=? t3 t4)
                                                       (Mvector-val-set! val i ccvi)
                                                       (Break-Repeat)))))))]
                                 [else
                                  (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                          (let$* ([vi (Mvector-val-ref val i)]
                                                  [cvi (cast vi c val)]
                                                  [t4 (Mvector-rtti-ref a)])
                                            (if$ (op=? t3 t4)
                                                 (Mvector-val-set! val i cvi)
                                                 (Break-Repeat))))]))))
                           val)))))]
          [other (error 'interp-cast/mvectC "unmatched value ~a" other)])]
       [(tuple?$ crcn)
        (match crcn
          [(not (Quote-Coercion _)) (If (Op '= (list (Quote 0) mono_type))
                                        (Coerce-Tuple cast-u val crcn)
                                        (Coerce-Tuple-In-Place cast-u val crcn mono_type))]
          [(Quote-Coercion (CTuple n c*))
           (define-values (bnd* var*)
             (for/lists ([bnd* : CoC3-Bnd*] [var* : CoC3-Expr*])
                        ([i (in-range n)] [c (in-list c*)])
               (cond
                 [(index? i)
                  (define tmp (next-uid! "element"))
                  (values
                   (cons tmp (cast (Tuple-proj val i) (Quote-Coercion c) (Quote 0)))
                   (Var tmp))]
                 [else (error 'interpret-casts-with-coercions "bad index")])))
           (Let bnd* (Create-tuple var*))]
          [other (error 'cast/coercion "thats impossible! ... I think")])]
       [else (Blame (Quote "bad implemention of mediating coercions"))])]
     ;; the coercion must be failure
     [else (Blame (fail-label$ crcn))])))

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
   -> Compose-Coercions-Type)
(define ((make-compose-coercions-code next-uid! compose compose-uid mk-crcn) c1 c2)
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
     [else (Blame (Quote "bad implemention of mediating coercions"))])]
   ;; C1 must be a failed coercion
   [else (Blame (fail-label$ c1))]))

;; ic-expr maps over the expressions lowering function cast
;; into calls to the runtime cast interpreter and direct manipulation
;; of the representation for each type and value.
(: interpret-casts-in-expr :
   (String -> Uid)
   Uid
   (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
   (CoC3-Expr CoC3-Expr -> CoC3-Expr)
   (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
   Gbox-setT Gbox-refT Gvec-setT Gvec-refT
   Dyn-Gbox-setT Dyn-Gbox-refT
   Dyn-Gvec-setT Dyn-Gvec-refT
   Dyn-Fn-AppT
   CoC1-Expr
   ->
   CoC3-Expr)
(define (interpret-casts-in-expr next-uid!
                                 interp-uid interp-cast interp-compose mk-coercion
                                 gbox-set! gbox-ref gvect-set! gvect-ref
                                 dyn-gbox-set! dyn-gbox-ref
                                 dyn-gvec-set! dyn-gvec-ref
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
       (interp-cast v c (Quote 0))]
      [(Cast (app recur e) (Coercion c))
       (interp-cast e (Quote-Coercion c) (Quote 0))]
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
      [(Mbox (app recur e) t) (Mbox e t)]
      [(Munbox (app recur e)) (Mbox-val-ref e)]
      [(Mbox-set! (app recur e1) (app recur e2))
       (Mbox-val-set! e1 e2)]
      [(MBoxCastedRef addr t)
       (let ([t1 (next-uid! "t1")]
             [t2 (next-uid! "t2")]
             [c  (next-uid! "crcn")])
         (Let (list
               (cons t1 (Mbox-rtti-ref addr))
               (cons t2 (Type t)))
           (Let (list (cons c (mk-coercion (Var t1) (Var t2) (Quote ""))))
             (interp-cast (Mbox-val-ref (Var addr)) (Var c) (Quote 0)))))]
      [(MBoxCastedSet! addr (app recur e) t)
       (: mbox-set! (Uid
                     (U (Quote Cast-Literal) (Var Uid))
                     (Var Uid) ->
                     CoC3-Expr))
       (define (mbox-set! addr val type)
         (define-syntax-let$* let$* next-uid!)
         (let$* ([t1 (Mbox-rtti-ref addr)]
                 [c (mk-coercion type t1 (Quote ""))]
                 ;; TODO: copy the value first
                 [cv (cond$
                      [(tupleT?$ t1)
                       (let$* ([n (Type-Tuple-num t1)]
                               [ctv (Copy-Tuple n val)])
                         (Begin
                           (list
                            (Mbox-val-set! (Var addr) ctv))
                           ctv))]
                      [else val])]
                 [ccv (interp-cast cv c (Var addr))]
                 [t2 (Mbox-rtti-ref addr)])
           (begin
             (if$ (op=? t1 t2)
                  (Mbox-val-set! (Var addr) ccv)
                  (Quote 0)))))
       (let ([t1 (next-uid! "t1")])
         (Let (list (cons t1 (Type t)))
           (if (or (Var? e) (Quote? e))
               (mbox-set! addr e (Var t1))
               (let ([val (next-uid! "write_cv")])
                 (Let (list (cons val e))
                   (mbox-set! addr (Var val) (Var t1)))))))]
      [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
      [(Mvector-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
      [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
       (Mvector-val-set! e1 e2 e3)]
      [(MVectCastedRef addr (app recur i) t)
       (let ([t1 (next-uid! "t1")]
             [t2 (next-uid! "t2")]
             [c  (next-uid! "crcn")])
         (Let (list
               (cons t1 (Mvector-rtti-ref addr))
               (cons t2 (Type t)))
           (Let (list (cons c (mk-coercion (Var t1) (Var t2) (Quote ""))))
             (interp-cast (Mvector-val-ref (Var addr) i) (Var c) (Quote 0)))))]
      [(MVectCastedSet! addr (app recur i) (app recur e) t)
       (: mvect-set! (Uid
                      CoC3-Expr
                      (U (Quote Cast-Literal) (Var Uid))
                      (Var Uid) ->
                      CoC3-Expr))
       (define (mvect-set! addr i val type)
         (define-syntax-let$* let$* next-uid!)
         (let$* ([t1 (Mvector-rtti-ref addr)]
                 [c (mk-coercion type t1 (Quote ""))]
                 ;; TODO: copy the values first
                 [cv (interp-cast val c (Var addr))]
                 [t2 (Mvector-rtti-ref addr)])
           (begin
             (if$ (op=? t1 t2)
                  (Mvector-val-set! (Var addr) i cv)
                  (Quote 0)))))
       (let ([t1 (next-uid! "t1")])
         (Let (list (cons t1 (Type t)))
           (if (or (Var? e) (Quote? e))
               (mvect-set! addr i e (Var t1))
               (let ([val (next-uid! "write_cv")])
                 (Let (list (cons val e))
                   (mvect-set! addr i (Var val) (Var t1)))))))]
      
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
      [(Tuple-proj e i) (Tuple-proj (recur e) i)]
      ;; Coercions manipulation
      [(or (Guarded-Proxy-Source _) (Guarded-Proxy-Target _)
           (Guarded-Proxy-Blames _))
       (error 'interpret-casts "twosome code in coercion pass: ~a" exp)]
      [other (error 'interpret-casts "umatched ~a" other)]))
  (recur exp))


(define-type Smart-Cast-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
       (#:t1-not-dyn Boolean #:t2-not-dyn Boolean)
       CoC3-Expr))
(: make-smart-cast :
   (String -> Uid)
   (CoC3-Expr CoC3-Expr CoC3-Expr  -> CoC3-Expr)
   (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
   -> Smart-Cast-Type)
(define ((make-smart-cast next-uid! cast mk-coercion)
         val t1 t2 lbl
         #:t1-not-dyn [t1-not-dyn #f]
         #:t2-not-dyn [t2-not-dyn #f])
  (match* (val t1 t2)
    [(val (Type t) (Type t)) val]
    [(val (Type (Dyn)) t2) #:when t2-not-dyn
     (match val
       [(or (Var _) (Quote _)) (cast val (Project-Coercion t2 lbl) (Quote 0))]
       [other
        (define u (next-uid! "tmp"))
        (Let (list (cons u val))
          (cast (Var u) (Project-Coercion t2 lbl) (Quote 0)))])]
    [((or (Var _) (Quote _)) (Type (Dyn)) t2)
     (If (Type-Dyn-Huh t2) val (cast val (Project-Coercion t2 lbl) (Quote 0)))]
    [(val t1 (Type (Dyn))) #:when t1-not-dyn
     (cast val (Inject-Coercion t1) (Quote 0))]
    [((Var _) t1 (Type (Dyn)))
     (If (Type-Dyn-Huh t1) val (cast val (Inject-Coercion t1) (Quote 0)))]
    [(val t1 t2)
     (cast val (mk-coercion t1 t2 lbl) (Quote 0))]))

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
          ($cast var-read-val var-tyof (Type DYN-TYPE) lbl))
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
              (Let `([,wrt-val2 . ,($cast wrt-val1 t2 (Type DYN-TYPE) info)])
                (gb-set! gbox-v wrt-val2-v))
              (Let `([,wrt-val3 . ,($cast wrt-val1 t2 tyof-v info #:t2-not-dyn #t)])
                ($cast (gb-set! gbox-v wrt-val3-v)
                       tyof-v (Type DYN-TYPE) info #:t1-not-dyn #t))))
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
          ($cast var-read-val var-tyof (Type DYN-TYPE) lbl))
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
              (Let `([,wrt-val2 . ,($cast wrt-val1 t2 (Type DYN-TYPE) info)])
                (gv-set! val-v ind wrt-val2-v))
              (Let `([,wrt-val3 . ,($cast wrt-val1 t2 tyof-v info #:t2-not-dyn #t)])
                ($cast (gv-set! val-v ind wrt-val3-v) 
                       tyof-v (Type DYN-TYPE) info #:t1-not-dyn #t))))
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
        ($cast v (Type t) (Var tyi) l))))
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
          ($cast (Var ret-val) (Var ret-ty) (Type DYN-TYPE) l))
        (Blame l))))


;; Functions for use sites of guarded references with coercions
(define-type Gbox-refT ((Var Uid) -> CoC3-Expr))
(define-type Gbox-setT ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Gvec-refT ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Gvec-setT ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

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
