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
|Input Grammar Cast2-Language                                                  |
|Output Grammar Cast3-Language                                                 |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
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
 "../language/cast-or-coerce1.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/data-representation.rkt"
 "./interpret-casts-help.rkt")

(provide
 interpret-casts/twosomes
 (all-from-out
  "../language/cast-or-coerce1.rkt"
  "../language/cast-or-coerce3.rkt"))

(: use-simple-cast-interp? (Parameterof Boolean))
(define use-simple-cast-interp? (make-parameter #f))

(: interpret-casts/twosomes : Cast-or-Coerce1-Lang  ->  Cast-or-Coerce3-Lang)
(define (interpret-casts/twosomes prgm)
  ;; Desugaring the input program into its constituents 
  (match-define (Prog (list prgm-name prgm-next prgm-type) prgm-exp)
    prgm)
  (define unique-counter (make-unique-counter prgm-next))
  (define next-uid! (make-next-uid! unique-counter))
  
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
        (set 'cast 'inject 'project 'higher-order
             'cast-ground 'cast-undyned 'cast-dyn
             'cast-fn 'cast-gbox 'cast-gvec)
        (set)))
  
  ;; The runtime label for the runtime coercion interpreter
  (define-syntax (inline-or-bnd-cast stx)
    (syntax-case stx ()
      [(_ bnd* f-sym f-name cg-exp ty-arg arg ...)
       (with-syntax ([(p ...) (generate-temporaries #'(arg ...))]
                     [((ta _) ...) #'([ty-arg arg] ...)])
         #'(let ([gen cg-exp])
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
                (values (cons bnd bnd*) call)])))]))

  
  (: bindings-needed-for-casts CoC3-Bnd-Code*)
  (: cast : Cast-With-MAddr-Type)
  (: cast-u : Uid)
  (define-values (bindings-needed-for-casts cast cast-u)
    (cond
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
              (make-higher-order-cast-code next-uid! cast-uid cast)
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
              (make-cast-code next-uid! inject project ho-cast)
              CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")])
         (values b* cast cast-uid))]
      [else
       (let*-values
           ([(b* cast-fn)
             (inline-or-bnd-cast
              '() 'cast-function "cast_function"
              (make-cast-fn-code next-uid!)
              CoC3-Expr "value" "type1" "type2" "blame_info")]
            [(b* cast-gbox)
             (inline-or-bnd-cast
              b* 'cast-gbox "cast_gbox"
              (make-cast-gbox-code next-uid!)
              CoC3-Expr "value" "type1" "type2" "blame_info")]
            [(glbt-uid) (next-uid! "greatest-lower-bound")]
            [(glbt) (ann (apply-code glbt-uid) GreatestLowerBound-Type)]
            [(b* make-glbt)
             (inline-or-bnd-cast
              b* 'glbt glbt-uid
              (gen-greatest-lower-bound-type-code next-uid! glbt glbt-uid)
              CoC3-Expr "type1" "type2")]
            [(cast-uid) (next-uid! "interp_cast")]
            [(cast) (ann (apply-code cast-uid) Cast-With-MAddr-Type)]
            [(cv-uid) (next-uid! "copy_value_in_monoref")]
            [(cv) (ann (apply-code cv-uid) CopyValueInMonoRef-Type)]
            [(b* make-cv)
             (inline-or-bnd-cast
              b* 'cv cv-uid
              (gen-copy-value-in-monoref-code next-uid!)
              CoC3-Expr "address")]
            [(b* cast-mbox)
             (inline-or-bnd-cast
              b* 'cast-mbox "cast_mbox"
              (make-cast-mbox-code next-uid! cast glbt cv)
              CoC3-Expr "value" "type1" "type2" "blame_info")]
            [(b* cast-mvect)
             (inline-or-bnd-cast
              b* 'cast-mvect "cast_mvect"
              (make-cast-mvect-code next-uid! cast glbt)
              CoC3-Expr "value" "type1" "type2" "blame_info")]
            [(b* cast-gvec)
             (inline-or-bnd-cast
              b* 'cast-gvec "cast_gvec"
              (make-cast-gvect-code next-uid!)
              CoC3-Expr "value" "type1" "type2" "blame_info")]
            ;; [(b* cast-mvec)
            ;;  (inline-or-bnd-cast
            ;;   b* 'cast-mvec "cast_mvec"
            ;;   (make-cast-mvect-code next-uid! glbt)
            ;;   CoC3-Expr "value" "type1" "type2" "blame_info")]
            [(b* cast-tuple)
             (inline-or-bnd-cast
              b* 'cast-tuple "cast_tuple"
              (make-cast-tuple-code next-uid! cast-uid cast)
              CoC3-Expr "value" "type1" "type2" "blame_info")]
            [(b* cast-ground)
             (inline-or-bnd-cast
              b* 'cast-ground "cast_ground"
              (make-cast-ground-code next-uid! cast-uid cast-fn cast-gbox
                                     cast-gvec cast-tuple cast-mbox cast-mvect)
              CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
            [(b* cast-undyned)
             (inline-or-bnd-cast
              b* 'cast-undyned "cast_undyned"
              (make-cast-undyned-code next-uid! cast-ground)
              CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
            [(b* cast-dyn)
             (inline-or-bnd-cast
              b* 'cast-dyn "cast_dyn"
              (make-cast-dyn-code next-uid! cast-undyned)
              CoC3-Expr "value" "type1" "type2" "blame_info" "mono_address")]
            [(b* cast)
             (let ([gen (make-cast-any-code next-uid! cast-dyn cast-ground)])
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
         (values b* cast cast-uid))]))

  ;; ;; Initialize all the state for guarded operations
  (: gbox-ref : GBox-Ref-Type)
  (: gbox-set! : GBox-Set-Type)
  (: gvec-ref : GVec-Ref-Type)
  (: gvec-set! : GVec-Set-Type)
  
  (: bindings-needed-for-guarded : CoC3-Bnd-Code*)
  (define-values (gbox-ref gbox-set! gvec-ref gvec-set!
                           bindings-needed-for-guarded)
    ;; First we create initialize the code generators
    (let* ([gbox-ref-uid (next-uid! "gbox_ref")]
           [gbox-set-uid (next-uid! "gbox_set")]
           [gvec-ref-uid (next-uid! "gvec_ref")]
           [gvec-set-uid (next-uid! "gvec_set")]
           [gbox-ref     : GBox-Ref-Type (apply-code gbox-ref-uid)]
           [gbox-set     : GBox-Set-Type (apply-code gbox-set-uid)]
           [gvec-ref     : GVec-Ref-Type (apply-code gvec-ref-uid)]
           [gvec-set     : GVec-Set-Type (apply-code gvec-set-uid)]
           [gen-gbox-ref (make-gbox-ref-code next-uid! cast gbox-ref)]
           [gen-gbox-set (make-gbox-set!-code next-uid! cast gbox-set)]
           [gen-gvec-ref (make-gvec-ref-code next-uid! cast gvec-ref)]
           [gen-gvec-set (make-gvec-set!-code next-uid! cast gvec-set)]
           [gbr-b (next-uid! "box")]
           [gbs-b (next-uid! "box")]
           [gbs-v (next-uid! "value")] 
           [gvr-r (next-uid! "vec")]
           [gvr-i (next-uid! "index")] 
           [gvs-r (next-uid! "vec")]
           [gvs-i (next-uid! "index")]
           [gvs-v (next-uid! "value")]
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
                                  (Var gvs-v)))])])
      (cond
        [(inline-guarded-branch?)
         (values gen-gbox-ref gen-gbox-set gen-gvec-ref gen-gvec-set bnd*)]
        [else (values gbox-ref gbox-set gvec-ref gvec-set bnd*)])))
  
  ;; TODO if the simple casting interface ends up being competitive
  ;; use project and inject calls in the dynamic operation specialization.
  (: dyn-fn-app : Dyn-Fn-App-Type)
  (: dyn-gbox-ref : Dyn-GBox-Ref-Type)
  (: dyn-gbox-set! : Dyn-GBox-Set-Type)
  (: dyn-gvec-ref  : Dyn-GVec-Ref-Type)
  (: dyn-gvec-set! : Dyn-GVec-Set-Type)
  (: bindings-needed-for-dynamic-operations : CoC3-Bnd-Code*)
  (define-values (dyn-fn-app
                  dyn-gbox-ref dyn-gbox-set!
                  dyn-gvec-ref dyn-gvec-set!
                  bindings-needed-for-dynamic-operations)
    (let* ([gen-dyn-fn-app
            (make-dyn-fn-app-code next-uid! cast)]
           [gen-dyn-gvec-set!
            (make-dyn-gvect-set!-code next-uid! gvec-set! cast)]
           [gen-dyn-gvec-ref
            (make-dyn-gvect-ref-code next-uid! gvec-ref cast)]
           [gen-dyn-gbox-set!
            (make-dyn-gbox-set!-code next-uid! gbox-set! cast)]
           [gen-dyn-gbox-ref 
            (make-dyn-gbox-ref-code next-uid! gbox-ref cast)])
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
                                     (Var gvs-l)))]))])))

  
  ;; ;; Next we generate the bindings for code that needs to be
  ;; ;; included as the runtime.
  (define gradual-runtime-bindings : CoC3-Bnd-Code*
    (append bindings-needed-for-dynamic-operations
            bindings-needed-for-casts
            bindings-needed-for-guarded))


  
  (define exp-with-lowered-gradual-operations
    (interpret-casts-in-expr
     next-uid!
     #;interp-cast-uid cast 
     gbox-set! gbox-ref gvec-set! gvec-ref
     dyn-gbox-set! dyn-gbox-ref dyn-gvec-set! dyn-gvec-ref dyn-fn-app
     prgm-exp))

  (Prog (list prgm-name (unique-counter-next! unique-counter) prgm-type)
    (Labels gradual-runtime-bindings exp-with-lowered-gradual-operations))
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


  #;(error 'todo))

;; These templates are used to build the code that performs
;; casting at runtime. The templates are a little over
;; parameterized currently in hopes that it make specialization
;; and customization easier in the future.

(define-type Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Cast-With-MAddr-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

;; These templates are used to build the code that performs
;; casting at runtime.
;; In my opinion they currently do too much manual specialization
;; and we should make them simpler but implement a specialization
;; pass that derives code with similar efficiency. AMK 2015-12-17

;; How to cast any type to some other type
(: make-cast-any-code : (String -> Uid) Cast-With-MAddr-Type Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
(define ((make-cast-any-code next-uid! cast-dyn cast-ground) v t1 t2 lbl mono-address)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([type1 t1] [type2 t2])
    (cond$
     [(op=? type1 type2) v]
     [(op=? type1 (Type DYN-TYPE)) (cast-dyn v type1 type2 lbl mono-address)]
     [else (cast-ground v type1 type2 lbl mono-address)])))

(: make-cast-ground-code :
   (String -> Uid) Uid Cast-Type Cast-Type Cast-Type Cast-Type Cast-Type Cast-Type -> Cast-With-MAddr-Type)
(define ((make-cast-ground-code next-uid! cast-u cast-fn cast-gref
                                cast-gvect cast-tuple
                                cast-mref cast-mvect) v t1 t2 lbl mono-address)
  (define-syntax-let$* let$* next-uid!)
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
     [(op=? type1 (Type UNIT-TYPE))
      (if$ (op=? (Type DYN-TYPE) type2)
           (Dyn-make value (Type UNIT-TYPE))
           (Blame lbl))]
     [else
      (let$* ([tag1 (type-tag type1)])
        (cond$
         [(op=? (Tag 'Fn) tag1)
          (cast-fn value type1 type2 lbl)]
         [(op=? (Tag 'GRef) tag1)
          (cast-gref value type1 type2 lbl)]
         [(op=? (Tag 'GVect) tag1)
          (cast-gvect value type1 type2 lbl)]
         [(op=? (Tag 'MRef) tag1)
          (cast-mref value type1 type2 lbl)]
         [(op=? (Tag 'MVect) tag1)
          (cast-mvect value type1 type2 lbl)]
         [(op=? (Tag 'STuple) tag1)
          (If (Op '= (list (Quote 0) mono-address))
              (cast-tuple value type1 type2 lbl)
              (Cast-Tuple-In-Place cast-u value type1 type2 lbl mono-address))]
         [else (Blame (Quote "Unexpected Type1 in cast tree"))]))])))

;; How to cast a Guarded Reference to some other type
(: make-cast-gbox-code : (String -> Uid) -> Cast-Type)
(define ((make-cast-gbox-code next-uid!) v t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
  (: gref-arg (CoC3-Expr -> CoC3-Expr))
  (define (gref-arg t)
    (if (Type? t)
        (let ([t (Type-type t)])
          (if (GRef? t)
              (Type (GRef-arg t))
              (error 'interpret-casts "unexpected type in gref-arg")))
        (Type-GRef-Of t)))
  (: proxy-gref (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (proxy-gref val type1 type2 lbl)
    (let$* ([tag_gref (type-tag type2)])
      (if$ (op=? tag_gref (Tag 'GRef))
           (let$* ([g1 (gref-arg type1)]
                   [g2 (gref-arg type2)])
             (Guarded-Proxy val (Twosome g1 g2 lbl)))
           (Blame lbl))))
  (let$* ([val v] [type1 t1] [type2 t2] [label lbl])
    (if$ (op=? (Type DYN-TYPE) type2)
         (Dyn-make val type1)
         (proxy-gref val type1 type2 label))))

;; How to Cast a Guarded Vector to some other type
(: make-cast-gvect-code :
   (String -> Uid) -> (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define ((make-cast-gvect-code next-uid!) v t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
  ;; Get the vector type argument either by compile time reflection
  ;; or generating runtime code.
  (: gvect-arg (CoC3-Expr -> CoC3-Expr))
  (define (gvect-arg t)
    (if (Type? t)
        (let ([t (Type-type t)])
          (if (GVect? t)
              (Type (GVect-arg t))
              (error 'interpret-casts "unexpected type in gvect-arg")))
        (Type-GVect-Of t)))
  ;; Generate the code to create a proxy if the target type is
  ;; actually a Gvector
  (: proxy-gvect (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (proxy-gvect val type1 type2 lbl)
    (let$* ([tag_gvect (type-tag type2)])
      (if$ (op=? tag_gvect (Tag 'GVect))
           (let$* ([g1 (gvect-arg type1)]
                   [g2 (gvect-arg type2)])
             (Guarded-Proxy val (Twosome g1 g2 lbl)))
           (Blame lbl))))
  ;; Check to see if we are casting to dyn, if so box the value.
  ;; Otherwise either cast to a gvector type or blame the label.
  (let$* ([val v] [type1 t1] [type2 t2] [label lbl])
    (if$ (op=? (Type DYN-TYPE) type2)
         (Dyn-make val type1)
         (proxy-gvect val type1 type2 label))))

;; How to cast a Monotonic Reference to some other type
(: make-cast-mbox-code : (String -> Uid)
   Cast-With-MAddr-Type
   GreatestLowerBound-Type
   CopyValueInMonoRef-Type -> Cast-Type)
(define ((make-cast-mbox-code next-uid! cast glbt copy-val-monoref) v t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([val v] [type1 t1] [type2 t2] [tag_mref (type-tag type2)])
    (if$ (op=? (Type DYN-TYPE) type2)
         (Dyn-make val type1)
         (if$ (op=? tag_mref (Tag 'MRef))
              (match val
                [(Var a)
                 (let$* ([t2 (mref-of$ type2)])
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
                                         [cv (cast vv t1 t3 (Quote "") val)]
                                         [t4 (Mbox-rtti-ref a)])
                                   (if$ (op=? t3 t4)
                                        (Begin
                                          (list (Mbox-val-set! val cv))
                                          val)
                                        val)))))))]
                [other (error 'interp-cast/mref "unmatched value ~a" other)])
              (Blame lbl)))))

(: make-cast-mvect-code : (String -> Uid)
   Cast-With-MAddr-Type
   GreatestLowerBound-Type -> Cast-Type)
(define ((make-cast-mvect-code next-uid! cast glbt) v t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([val v] [type1 t1] [type2 t2] [tag_mvect (type-tag type2)])
    (if$ (op=? (Type DYN-TYPE) type2)
         (Dyn-make val type1)
         (if$ (op=? tag_mvect (Tag 'MVect))
              (match val
                [(Var a)
                 (let$* ([t2 (mvect-of$ type2)])
                   (if$ (dyn?$ t2)
                        val
                        (let$* ([t1 (Mvector-rtti-ref a)]
                                [t3 (glbt t1 t2)])
                          (if$ (op=? t1 t3)
                               val
                               (Begin
                                 (list
                                  (Mvector-rtti-set! a t3)
                                  (let* ([i-u (next-uid! "index")]
                                         [i (Var i-u)]
                                         [x (next-uid! "_")])
                                    (let$* ([vn (Mvector-size val)])
                                      (cond$
                                       [(tupleT?$ t3)
                                        (let$* ([n (Type-Tuple-num t3)])
                                          (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                                  (let$* ([vi (Mvector-val-ref val i)]
                                                          [cvi (Copy-Tuple n vi)])
                                                    (Begin
                                                      (list
                                                       (Mvector-val-set! val i cvi))
                                                      (let$* ([ccvi (cast cvi t1 t3 (Quote "") val)]
                                                              [t4 (Mvector-rtti-ref a)])
                                                        (if$ (op=? t3 t4)
                                                             (Mvector-val-set! val i ccvi)
                                                             (Break-Repeat)))))))]
                                       [else
                                        ;; TODO: checking if t3=t4 is unneeded in this case, prove it!
                                        (Repeat i-u (Quote 0) vn x UNIT-IMDT
                                                (let$* ([vi (Mvector-val-ref val i)]
                                                        [cvi (cast vi t1 t3 (Quote "") val)]
                                                        [t4 (Mvector-rtti-ref a)])
                                                  (if$ (op=? t3 t4)
                                                       (Mvector-val-set! val i cvi)
                                                       (Break-Repeat))))]))))
                                 val)))))]
                [other (error 'interp-cast/mvect "unmatched value ~a" other)])
              (Blame lbl)))))

;; How to Cast a Function to some other type
(: make-cast-fn-code : (String -> Uid) -> Cast-Type)
(define ((make-cast-fn-code next-uid!) v t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([value v] [type1 t1] [type2 t2])
    (if$ (op=? (Type DYN-TYPE) type2)
         (Dyn-make value type1)
         (let$* ([tag2 (type-tag type2)])
           (if$ (op=? tag2 (Tag 'Fn))
                (let$* ([type1_arity (type-fn-arity type1)]
                        [type2_arity (type-fn-arity type2)])
                  (if$ (op=? type1_arity type2_arity)
                       (App-Code (Fn-Caster value)
                                 (list value t1 type2 lbl))
                       (Blame lbl)))
                (Blame lbl))))))

(: make-cast-tuple-code : (String -> Uid) Uid Cast-With-MAddr-Type -> Cast-Type)
(define ((make-cast-tuple-code next-uid! cast-u interpret-cast) v t1 t2 lbl)
  (define-syntax-let$* let$* next-uid!)
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
                                     (values (cons (cons a (let$* ([item (Tuple-proj v i)]
                                                                   [new-t1t (tuple-type-arg$ t1 i)]
                                                                   [new-t2t (tuple-type-arg$ t2 i)])
                                                             (interpret-cast item new-t1t new-t2t lbl (Quote 0)))) b*)
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

;; How to extract a dynamic value
(: make-cast-dyn-code : (String -> Uid) Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
(define ((make-cast-dyn-code next-uid! cast-undyned)  v t1 t2 lbl mono-address)
  (define-syntax-let$* let$* next-uid!)
  #| This should be:
  (typecase v
  [n : t1 (cast-undyned v^ t1^ t2 lbl)])
  ==> 
  (case v
  [(Dynamic v^ t1^) ...])
  ==>
  (
  Efficient Implementation will require remove-let before
  specify representation. TODO
  For now I am going to implement this
  (dyn-destruct v (v^ t1)
  (cast-undyned v^ t1^ t2 lbl))
  |#
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

;; How to cast a non dynamic value to another type
;; Notice this is used in conjuction with interpret cast based
;; on the setting of the parameter recursive-dyn-cast
;; when set to #t the cast used is a call to the interpret
;; cast runtime routine instead of unfolding the cast tree even
;; I am pretty sure this is the desired behavior
;; There is an invarient that there will only ever be one level of dyn
(: make-cast-undyned-code : (String -> Uid) Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
(define ((make-cast-undyned-code next-uid! cast-ground) v t1 t2 l mono-address)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([value v] [type1 t1] [type2 t2] [label l])
    ;; If unboxed at the exact type then just return the value
    (cond$
     [(op=? type1 type2) value]
     ;; Otherwise it was either unboxed at the wrong type
     ;; or it is an inter structured type cast.
     ;; Either way we build a cast tree specific to this type.
     [else (cast-ground value type1 type2 label mono-address)])))

;; We should test this version it is much more straight forward and
;; if it is faster that would be great.

(: make-cast-code : (String -> Uid) Inject-Type Project-Type Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
(define ((make-cast-code next-uid! inject project ho-cast) v t1 t2 lbl mono-address)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([type1 t1] [type2 t2])
    (cond$
     [(op=? type1 type2) v]
     [(op=? type1 (Type DYN-TYPE)) (project v type2 lbl mono-address)]
     [(op=? type2 (Type DYN-TYPE)) (inject v type1)]
     [else (ho-cast v type1 type2 lbl mono-address)])))

(define-type Inject-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(: make-inject-code : Inject-Type)
(define (make-inject-code v t)
  (Dyn-make v t))

(define-type Project-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(: make-project-code : Cast-With-MAddr-Type -> Project-Type)
(define ((make-project-code cast) v t l mono-address)
  (cast (Dyn-value v) (Dyn-type v) t l mono-address))

;; TODO: why we pass in cast?
(: make-higher-order-cast-code : (String -> Uid) Uid Cast-With-MAddr-Type -> Cast-With-MAddr-Type)
(define ((make-higher-order-cast-code next-uid! cast-u cast) v t1 t2 l mono-address)
  (define-syntax-let$* let$* next-uid!)
  (let$* ([value v] [type1 t1] [type2 t2] [label l])
    (cond$
     [(and$ (Type-Fn-Huh type1)
            (and$ (Type-Fn-Huh type2)
                  (op=? (Type-Fn-arity type1) (Type-Fn-arity type2))))
      ;; We could include a check to see if t1 == t2 but it should
      ;; have already been done in make cast
      (App-Code (Fn-Caster value) (list value type1 type2 label))]
     [(and$ (Type-GVect-Huh type1) (Type-GVect-Huh type2))
      (Guarded-Proxy value
                     (Twosome (Type-GVect-Of type1) (Type-GVect-Of type2) label))]
     [(and$ (Type-GRef-Huh type1) (Type-GRef-Huh type2))
      (Guarded-Proxy value
                     (Twosome (Type-GRef-Of type1) (Type-GRef-Of type2) label))]
     [(and$ (Type-MRef-Huh type1) (Type-MRef-Huh type2))
      (error "support mrefs in the simple interpreter")]
     [(and$ (Type-MVect-Huh type1) (Type-MVect-Huh type2))
      (error "support mvects in the simple interpreter")]
     [(and$ (Type-Tuple-Huh type1)
            (and$ (Type-Tuple-Huh type2)
                  (op<=? (type-tuple-num type2)
                         (type-tuple-num type1))))
      (If (Op '= (list (Quote 0) mono-address))
          (Cast-Tuple cast-u value type1 type2 label)
          (Cast-Tuple-In-Place cast-u value type1 type2 l mono-address))]
     [else (Blame label)])))

;; ic-expr maps over the expressions lowering function cast
;; into calls to the runtime cast interpreter and direct manipulation
;; of the representation for each type and value.
(: interpret-casts-in-expr :
   (String -> Uid)
   #;Uid
   Cast-With-MAddr-Type
   GBox-Set-Type GBox-Ref-Type GVec-Set-Type GVec-Ref-Type
   Dyn-GBox-Set-Type Dyn-GBox-Ref-Type
   Dyn-GVec-Set-Type Dyn-GVec-Ref-Type
   Dyn-Fn-App-Type
   CoC1-Expr
   ->
   CoC3-Expr)
(define (interpret-casts-in-expr next-uid!
                                 interp-cast 
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
    ;;(printf "ic: ~a\n" exp) (flush-output (current-output-port))
    (match exp
      ;; Interesting Cases -----------------------------------------------
      ;; Transformations for Casting runtime
      [(Interpreted-Cast (app recur v) (Twosome t1 t2 l))
       (interp-cast v (recur t1) (recur t2) (recur l) (Quote 0))]
      [(Cast (app recur e) (Twosome t1 t2 l))
       (interp-cast e (Type t1) (Type t2) (Quote l) (Quote 0))]

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
             [t2 (next-uid! "t2")])
         (Let (list
               (cons t1 (Mbox-rtti-ref addr))
               (cons t2 (Type t)))
           (interp-cast (Mbox-val-ref (Var addr)) (Var t1) (Var t2) (Quote "") (Quote 0))))]
      [(MBoxCastedSet! addr (app recur e) t)
       (: mbox-set! (Uid
                     (U (Quote Cast-Literal) (Var Uid))
                     (Var Uid) ->
                     CoC3-Expr))
       (define (mbox-set! addr val type)
         (define-syntax-let$* let$* next-uid!)
         (let$* ([t1 (Mbox-rtti-ref addr)]
                 [cv (cond$
                      [(tupleT?$ t1)
                       (let$* ([n (Type-Tuple-num t1)]
                               [ctv (Copy-Tuple n val)])
                         (Begin
                           (list
                            (Mbox-val-set! (Var addr) ctv))
                           ctv))]
                      [else val])]
                 [ccv (interp-cast cv type t1 (Quote "") (Var addr))]
                 [t2 (Mbox-rtti-ref addr)])
           (if$ (op=? t1 t2)
                (Mbox-val-set! (Var addr) ccv)
                (Quote 0))))
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
             [t2 (next-uid! "t2")])
         (Let (list
               (cons t1 (Mvector-rtti-ref addr))
               (cons t2 (Type t)))
           (interp-cast (Mvector-val-ref (Var addr) i) (Var t1) (Var t2) (Quote "") (Quote 0))))]
      [(MVectCastedSet! addr (app recur i) (app recur e) t)
       (: mvect-set! (Uid
                      CoC3-Expr
                      (U (Quote Cast-Literal) (Var Uid))
                      (Var Uid) ->
                      CoC3-Expr))
       (define (mvect-set! addr i val type)
         (define-syntax-let$* let$* next-uid!)
         (let$* ([t1 (Mvector-rtti-ref addr)])
           (cond$
            [(tupleT?$ t1)
             (let$* ([n (Type-Tuple-num t1)]
                     [cvi (Copy-Tuple n val)])
               (Begin
                 (list
                  (Mvector-val-set! (Var addr) i cvi))
                 (let$* ([ccvi (interp-cast cvi type t1 (Quote "") (Var addr))]
                         [t2 (Mvector-rtti-ref addr)])
                   (if$ (op=? t1 t2)
                        (Mvector-val-set! (Var addr) i ccvi)
                        (Quote 0)))))]
            [else
             (let$* ([cvi (interp-cast val type t1 (Quote "") (Var addr))]
                     [t2 (Mvector-rtti-ref addr)])
               (if$ (op=? t1 t2)
                    (Mvector-val-set! (Var addr) i cvi)
                    (Quote 0)))])))
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
      ;; If we aim to support a data representation of casts
      ;; then we will need to add these back
      ;; [(Fn-Proxy i e1 e2)
      ;;  (Fn-Proxy (list i interp-uid) (recur e1) (recur e2))]
      ;; [(App/Fn-Proxy-Huh e e*)
      ;;  (App-Fn-or-Proxy interp-uid (recur e) (map recur e*))]
      ;; [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (recur e))]
      ;; [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (recur e))]
      ;; In theory it may be better to implement the tranformation
      ;; for apply here but it is currently postponed until
      ;; closure conversion.
      [(App-Fn e e*) (App-Fn (recur e) (map recur e*))]
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
      [(Fn-Caster e)
       (Fn-Caster (recur e))]
      [(Create-tuple e*)
       (Create-tuple (map recur e*))]
      [(Tuple-proj e i) (Tuple-proj (recur e) i)]
      ;; Coercions manipulation
      [(or (Fn-Proxy-Coercion _) 
           (Compose-Coercions _ _)
           (Id-Coercion-Huh _)
           (Fn-Coercion _ _)
           (Fn-Coercion-Arg _ _)
           (Fn-Coercion-Return _)
           (Quote-Coercion _))
       (error 'interpret-casts "twosome code in coercion pass: ~a" exp)]
      [other (error 'interpret-casts "umatched ~a" other)]))
  (recur exp))


;; Functions for use sites of guarded references with coercions
(define-type GBox-Ref-Type ((Var Uid) -> CoC3-Expr))
(define-type GBox-Set-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type GVec-Ref-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type GVec-Set-Type ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

(define-type MBox-Ref-Type (Uid (Var Uid) -> CoC3-Expr))
(define-type MBox-set-Type (Uid (U (Quote Cast-Literal) (Var Uid)) (Var Uid) ->
                                CoC3-Expr))

(: make-gbox-ref-code ((String -> Uid) Cast-With-MAddr-Type GBox-Ref-Type -> GBox-Ref-Type))
(define ((make-gbox-ref-code next-uid! cast gbox-ref) b)
  (define-values (b-u s-u t-u i-u r-u)
    (values (next-uid! "gbox_ref_proxied_box")
            (next-uid! "gbox_ref_proxied_source_type")
            (next-uid! "gbox_ref_proxied_target_type")
            (next-uid! "gbox_ref_proxied_blame_info")
            (next-uid! "gbox_ref_read_value")))
  (If (Guarded-Proxy-Huh b)
      (Let `([,b-u . ,(Guarded-Proxy-Ref b)]
             [,s-u . ,(Guarded-Proxy-Source b)]
             [,t-u . ,(Guarded-Proxy-Target b)]
             [,i-u . ,(Guarded-Proxy-Blames b)])
        (Let `([,r-u . ,(gbox-ref (Var b-u))])
          (cast (Var r-u) (Var s-u) (Var t-u) (Var i-u) (Quote 0))))
      (Unguarded-Box-Ref b)))

(: make-gbox-set!-code ((String -> Uid) Cast-With-MAddr-Type GBox-Set-Type -> GBox-Set-Type))
(define ((make-gbox-set!-code next-uid! cast gbox-set!) b v)
  (define-values (b-u s-u t-u i-u w-u)
    (values (next-uid! "gbox_set_proxied_box")
            (next-uid! "gbox_set_proxied_source_type")
            (next-uid! "gbox_set_proxied_target_type")
            (next-uid! "gbox_set_proxied_blame_info")
            (next-uid! "gbox_set_write_value")))
  (If (Guarded-Proxy-Huh b)
      (Let `([,b-u . ,(Guarded-Proxy-Ref b)]
             [,s-u . ,(Guarded-Proxy-Source b)]
             [,t-u . ,(Guarded-Proxy-Target b)]
             [,i-u . ,(Guarded-Proxy-Blames b)])
        (Let `([,w-u . ,(cast v (Var t-u) (Var s-u) (Var i-u) (Quote 0))])
          (gbox-set! (Var b-u) (Var w-u))))
      (Unguarded-Box-Set! b v)))

(: make-gvec-ref-code ((String -> Uid) Cast-With-MAddr-Type GVec-Ref-Type -> GVec-Ref-Type))
(define ((make-gvec-ref-code next-uid! cast gvec-ref) v i)
  (define-values (v-u s-u t-u i-u r-u)
    (values (next-uid! "gvec_ref_proxied_vector")
            (next-uid! "gvec_ref_proxied_source_type")
            (next-uid! "gvec_ref_proxied_target_type")
            (next-uid! "gvec_ref_proxied_blame_info")
            (next-uid! "gvec_ref_read_value")))
  (If (Guarded-Proxy-Huh v)
      (Let `([,v-u . ,(Guarded-Proxy-Ref v)]
             [,s-u . ,(Guarded-Proxy-Source v)]
             [,t-u . ,(Guarded-Proxy-Target v)]
             [,i-u . ,(Guarded-Proxy-Blames v)])
        (Let `([,r-u . ,(gvec-ref (Var v-u) i)])
          (cast (Var r-u) (Var s-u) (Var t-u) (Var i-u) (Quote 0))))
      (Unguarded-Vect-Ref v i)))

(: make-gvec-set!-code ((String -> Uid) Cast-With-MAddr-Type GVec-Set-Type -> GVec-Set-Type))
(define ((make-gvec-set!-code next-uid! cast gvec-set!) v i w)
  (define-values (v-u s-u t-u i-u w-u)
    (values (next-uid! "gvec_set_proxied_vector")
            (next-uid! "gvec_set_proxied_source_type")
            (next-uid! "gvec_set_proxied_target_type")
            (next-uid! "gvec_set_proxied_blame_info")
            (next-uid! "gvec_set_write_value")))
  (If (Guarded-Proxy-Huh v)
      (Let `([,v-u . ,(Guarded-Proxy-Ref v)]
             [,s-u . ,(Guarded-Proxy-Source v)]
             [,t-u . ,(Guarded-Proxy-Target v)]
             [,i-u . ,(Guarded-Proxy-Blames v)])
        (Let `([,w-u . ,(cast w (Var t-u) (Var s-u) (Var i-u) (Quote 0))])
          (gvec-set! (Var v-u) i (Var w-u))))
      (Unguarded-Vect-Set! v i w)))

(define-type Dyn-GBox-Ref-Type
  ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-GBox-Set-Type
  ((Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
(define-type Dyn-GVec-Ref-Type
  ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-GVec-Set-Type
  ((Var Uid) (Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid)
   -> CoC3-Expr))
(define-type Dyn-Fn-App-Type
  ((Var Uid) (Listof (Var Uid)) Schml-Type* (Var Uid) -> CoC3-Expr))

(: make-dyn-gbox-ref-code :
   (String -> Uid)
   GBox-Ref-Type
   Cast-With-MAddr-Type
   -> Dyn-GBox-Ref-Type)
(define ((make-dyn-gbox-ref-code next-uid! gbox-ref cast) dyn lbl)
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
               [,read-val . ,(gbox-ref var-val)])
          (cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
        (Blame lbl))))

(: make-dyn-gbox-set!-code :
   (String -> Uid)
   GBox-Set-Type
   Cast-With-MAddr-Type
   -> Dyn-GBox-Set-Type)
(define ((make-dyn-gbox-set!-code next-uid! gb-set! cast) dyn-gbox wrt-val1 t2 info)
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
          (Let `([,wrt-val3 . ,(cast wrt-val1 t2 tyof-v info (Quote 0))])
            (cast (gb-set! gbox-v wrt-val3-v) tyof-v (Type DYN-TYPE) info (Quote 0))))
        (Blame info))))

(: make-dyn-gvect-ref-code :
   (String -> Uid)  GVec-Ref-Type Cast-With-MAddr-Type
   -> Dyn-GVec-Ref-Type)
(define ((make-dyn-gvect-ref-code next-uid! gv-ref cast) dyn ind lbl)
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
          (cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
        (Blame lbl))))

(: make-dyn-gvect-set!-code :
   (String -> Uid)
   GVec-Set-Type
   Cast-With-MAddr-Type
   -> Dyn-GVec-Set-Type)
(define ((make-dyn-gvect-set!-code next-uid! gv-set! cast)
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
          (Let `([,wrt-val3 . ,(cast wrt-val1 t2 tyof-v info (Quote 0))])
            (cast (gv-set! val-v ind wrt-val3-v) tyof-v (Type DYN-TYPE) info (Quote 0))))
        (Blame info))))

(: make-dyn-fn-app-code : (String -> Uid) Cast-With-MAddr-Type -> Dyn-Fn-App-Type)
(define ((make-dyn-fn-app-code next-uid! cast) v v* t* l)
  (define-values (val ty ret-val ret-ty arity)
    (values (next-uid! "dyn_fn_val")
            (next-uid! "dyn_fn_ty")
            (next-uid! "dyn_fn_ret_val")
            (next-uid! "dyn_fn_ret_ty")
            (length v*)))
  (define arg-casts : CoC3-Expr*
    (for/list : (Listof CoC3-Expr)
              ([v : CoC3-Expr v*]
               [t : Schml-Type t*]
               [i (in-naturals)])
      (define tyi (next-uid! (string-append "dyn_fn_ty" (number->string i))))
      (Let `([,tyi . ,(Type-Fn-arg (Var ty) (Quote i))])
        (cast v (Type t) (Var tyi) l (Quote 0)))))
  (define casts-apply : CoC3-Expr
    (case (function-cast-representation)
      [(Hybrid) (App-Fn (Var val) arg-casts)]
      [(Data) (error 'todo "implement coercions data representation")]
      [(Functional) (error 'todo "incompatible with coercions")]
      [else (error 'interp-cast-with-coercions/dyn-fn-app "unexpected value")]))
  (Let `([,val . ,(Dyn-value v)]
         [,ty . ,(Dyn-type v)])
    (If (If (Type-Fn-Huh (Var ty))
            (Op '= (list (Type-Fn-arity (Var ty)) (Quote arity)))
            (Quote #f))
        (Let `([,ret-val . ,casts-apply]
               [,ret-ty . ,(Type-Fn-return (Var ty))])
          (cast (Var ret-val) (Var ret-ty) (Type DYN-TYPE) l (Quote 0)))
        (Blame l))))

;; TODO (andre) consider this tranform instead
;; (dyn-app f (42) (Int) L1)
;; =>
;; (let ([v (dyn-value f)] [t (dyn-type f)])
;;   (if (eq? t (Int -> Dyn))
;;       (v 42)
;;       (if (and (fn-type? t) (= (fn-type-arity t) 1))
;;           (let ([a (cast 42 Int (fn-type-arg t 0) L1)])
;;             (cast (v t) (fn-type-return t) L1))
;;           (blame L1))))



