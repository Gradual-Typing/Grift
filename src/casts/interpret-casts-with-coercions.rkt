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
 racket/match
 racket/format
 "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast-or-coerce2.rkt"
 "../language/cast-or-coerce3.rkt"
 "./interpret-casts-help.rkt")

(provide
 interpret-casts/coercions
 (all-from-out
  "../language/cast-or-coerce2.rkt"
  "../language/cast-or-coerce3.rkt"))

(: interpret-casts/coercions
   (Cast-or-Coerce2-Lang Config  ->  Cast-or-Coerce3-Lang))
(trace-define (interpret-casts/coercions prgm config)
              ;; Configuration options that determine how code is generated
              (: cast-rep Cast-Representation)
              (define cast-rep (Config-cast-rep config))
              (: specialize-casts? (Parameterof Boolean))
              (define specialize-casts? (make-parameter #f))
              (: recursive-dyn-cast? (Parameterof Boolean))
              (define recursive-dyn-cast? (make-parameter #t))
              (: space-efficient? Boolean)
              (define space-efficient? #f)
              
              ;; Desugaring the input program into its constituents 
              (match-define (Prog (list prgm-name prgm-next prgm-type) prgm-exp)
                prgm)

              ;; Just a tad of mutable state to generate unique identifiers
              (define next-unique-number : (Boxof Nat) (box prgm-next))
              ;; And syntax for introducing bindings when needed
              (define-syntax-let$* let$* next-unique-number)
              ;; And unconditional binding introduction
              (: next-uid! (String -> Uid))
              (define (next-uid! x)
                (let ([n (unbox next-unique-number)])
                  (set-box! next-unique-number (add1 n))
                  (Uid x n)))

              ;; The runtime label for the runtime coercion interpreter
              (define interp-uid (next-uid! "interp_coercion"))
              ;; Make a call to the runtime coercion interpreter
              (: interp (CoC3-Expr CoC3-Expr -> CoC3-Expr))
              (define (interp v c)
                (App-Code (Code-Label interp-uid) (list v c)))

              ;; The runtime label for the compose interpreter
              (define compose-uid (next-uid! "compose_coercions"))
              ;; Make a call to the runtime coercion composer
              (: compose (CoC3-Expr CoC3-Expr -> CoC3-Expr))
              (define compose
                (if space-efficient?
                    (lambda (c1 c2)
                      (App-Code (Code-Label compose-uid) (list c1 c2)))
                    (lambda (c1 c2)
                      (Sequence-Coercion c1 c2))))

              ;; The runtime label for the make coercion operation
              (define mk-coercion-uid (next-uid! "make_coercion"))
              ;; Make a call to the runtime coercion creator
              (: mk-coercion (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
              (define (mk-coercion t1 t2 l)
                (App-Code (Code-Label mk-coercion-uid) (list t1 t2 l)))

              ;; The runtime label for the greatest lower bound
              (define glb-uid (next-uid! "greatest_lower_bound"))
              ;; Make a call to the runtime greatest lower bound
              (: mk-glb-call (CoC3-Expr CoC3-Expr -> CoC3-Expr))
              (define (mk-glb-call t1 t2)
                (App-Code (Code-Label glb-uid) (list t1 t2)))

              (: greatest-lower-bound$ (GreatestLowerBound-Type Uid -> GreatestLowerBound-Type))
              (define ((greatest-lower-bound$ mk-glb mk-glb-uid) t1 t2)
                (cond$
                 [(op=? t1 t2) t1]
                 [(dyn?$ t1) t2]
                 [(dyn?$ t2) t1]
                 [(and$ (fnT?$ t1) (fnT?$ t2))
                  (if$ (op=? (fnT-arity$ t1) (fnT-arity$ t2))
                       ((mk-fn-type$ mk-glb mk-glb-uid) t1 t2)
                       (Error (Quote "can not compute the greatest lower bound for function types with mismatch arities")))]
                 [(and$ (gref?$ t1) (gref?$ t2))
                  (let$* ([gr1_of (gref-of$ t1)]
                          [gr2_of (gref-of$ t2)]
                          [t  (mk-glb gr1_of gr2_of)])
                         (gref$ t))]
                 [(and$ (gvect?$ t1) (gvect?$ t2))
                  (let$* ([gr1_of (gvect-of$ t1)]
                          [gr2_of (gvect-of$ t2)]
                          [t  (mk-glb gr1_of gr2_of)])
                         (gvect$ t))]
                 [else (Error (Quote "inconsistent types"))]))

              (: mk-fn-type$ (GreatestLowerBound-Type Uid -> GreatestLowerBound-Type))
              (define ((mk-fn-type$  mk-glb uid) t1 t2)
                (: help-glb (GreatestLowerBound-Type ->
                                                      (Schml-Type Schml-Type -> Schml-Type)))
                (define ((help-glb mk-glb) t1 t2)
                  (let ([t : CoC3-Expr (mk-glb (Type t1) (Type t2))])
                    (if (Type? t)
                        (Type-type t)
                        ;; This should always be possible if all the code is correct
                        (error 'mk-fn-type$/help-glb))))
                ;; This is suppose to be outside of the specializing code
                ;; because we are defining the meta make function coercion
                (cond
                  [(and (Type? t1) (Type? t2))
                   (let* ([t1 (Type-type t1)] [t2 (Type-type t2)])
                     (unless (and (Fn? t1) (Fn? t2))
                       (error 'mk-fn-type$))
                     (let* ([t1-args (Fn-fmls t1)]
                            [t2-args (Fn-fmls t2)]
                            [t1-ret  (Fn-ret  t1)]
                            [t2-ret  (Fn-ret  t2)]
                            [arg* (map (help-glb mk-glb) t2-args t1-args)]
                            [ret  ((help-glb mk-glb) t1-ret t2-ret)])
                       (Type (Fn (Fn-arity t1) arg* ret))))]
                  [else (Make-Fn-Type uid t1 t2)]))
              
              ;; These templates are used to build the code that performs
              ;; casting at runtime. The templates are a little over
              ;; parameterized currently in hopes that it make specialization
              ;; and customization easier in the future.
              (define-type Cast/Coercion-Type
                (CoC3-Trivial CoC3-Trivial -> CoC3-Expr))

              (: cast/coercion (Cast/Coercion-Type
                                Make-Coercion-Type
                                Compose-Coercions-Type ->
                                Cast/Coercion-Type))
              (define ((cast/coercion cast mk-crcn comp-crcn) v c)
                ;; apply-cast is normally specified in terms of inspecting the
                ;; value. But in a world were value reflection on values is limited
                ;; the coercions can drive the casting process.
                (cond$
                 [(id?$ c) v]
                 [(seq?$ c)
                  ;; I think that something better could be done here
                  ;; why recur there are only so many things that the underlying
                  ;; sequence could be?
                  (let$* ([seq_fst (seq-fst$ c)]
                          [seq_snd (seq-snd$ c)]
                          [fst_cast_value (cast v seq_fst)])
                         (cast fst_cast_value seq_snd))]
                 ;; By the typing rules of coercions we know v must be a dyn value
                 [(prj?$ c)
                  (let$* ([prj_type  (prj-type$ c)]
                          [prj_label (prj-label$ c)]
                          ;; The current implementation of the dyn interface is not a good one
                          ;; this line is going to have to recover the type from dyn twice
                          [dyn_value (Dyn-value v)]
                          [dyn_type  (Dyn-type v)])
                         ;; Maybe this line should be a call to compose?
                         ;; which would do the same thing but wouldn't duplicate
                         ;; this line across two functions
                         ;; I did it this way because I know this code won't build
                         ;; the injection coercion
                         (let$* ([projected_type (mk-crcn dyn_type prj_type prj_label)])
                                (cast dyn_value projected_type)))]
                 [(inj?$ c) (Dyn-make v (inj-type$ c))]
                 [(fnC?$ c)
                  (If (Fn-Proxy-Huh v)
                      (App-Code (Fn-Caster (Fn-Proxy-Closure v)) (list v c))
                      (App-Code (Fn-Caster v) (list v c)))]
                 [(ref?$ c)
                  (if$ (Guarded-Proxy-Huh v)
                       (let$* ([old_crcn (Guarded-Proxy-Coercion v)])
                              (let$* ([composed_crcn (comp-crcn old_crcn c)])
                                     (if$ (id?$ composed_crcn)
                                          v
                                          (Guarded-Proxy
                                           (Guarded-Proxy-Ref v)
                                           (Coercion composed_crcn)))))
                       (Guarded-Proxy v (Coercion c)))]
                 [(mrefC?$ c)
                  (match-let ([(Var a) v])
                    (let$* ([t2 (mrefC-type$ c)])
                           (if$ (dyn?$ t2)
                                v
                                (let$* ([t1 (Mbox-rtti-ref a)]
                                        [t3 (mk-glb-call t1 t2)])
                                       (if$ (op=? t1 t3)
                                            v
                                            (let$* ([c (mk-crcn t1 t3 (Quote ""))]
                                                    [cv (Mbox-val-ref v)]
                                                    [cv_ (CastedValue cv (Coercion c))])
                                                   (Begin
                                                     (list
                                                      (Mbox-val-set! v cv_)
                                                      (Mbox-rtti-set! a t3))
                                                     v)))))))]
                 ;; the coercion must be failure
                 [else (Blame (fail-label$ c))]))

              (define-type Make-Coercion-Type
                (CoC3-Trivial CoC3-Trivial CoC3-Trivial -> CoC3-Expr))

              (: make-coercion (Make-Coercion-Type Uid -> Make-Coercion-Type))
              (define ((make-coercion mk-crcn mk-crcn-uid) t1 t2 lbl)
                (cond$
                 [(op=? t1 t2) (Quote-Coercion (Identity))]
                 [(and$ (fnT?$ t1) (fnT?$ t2))
                  ;; This line is a little tricky because
                  ;; unless we have actual types for this at
                  ;; compile time we have to generate code
                  ;; that can handle arbitry fn-arity.
                  ;; We delegate this task to specify representation because
                  ;; it involves safely allocating a object whos size cannot
                  ;; be determined until run-time.
                  (if$ (op=? (fnT-arity$ t1) (fnT-arity$ t2))
                       ((mk-fn-crcn$ mk-crcn mk-crcn-uid) t1 t2 lbl)
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
                 ;; This is absolutly necisarry
                 ;; While Injections and Projections are never made by
                 ;; source code coercions composition can create new
                 ;; projections and injections.
                 [(dyn?$ t1) (seq$ (prj$ t2 lbl) (Quote-Coercion (Identity)))]
                 [(dyn?$ t2) (seq$ (Quote-Coercion (Identity)) (inj$ t1))]
                 [else (fail$ lbl)]))

              (: mk-fn-crcn$ (Make-Coercion-Type Uid -> Make-Coercion-Type))
              (define ((mk-fn-crcn$  mk-crcn uid) t1 t2 lbl)
                (: help-comp (Make-Coercion-Type CoC3-Trivial ->
                                                 (Schml-Type Schml-Type -> Schml-Coercion)))
                (define ((help-comp mk-crcn lbl) t g)
                  (let ([c : CoC3-Expr (mk-crcn (Type t) (Type g) lbl)])
                    (if (Quote-Coercion? c)
                        (Quote-Coercion-const c)
                        ;; This should always be possible if all the code is correct
                        (error 'mk-fn-crcn$/help-comp))))
                ;; This is suppose to be outside of the specializing code
                ;; because we are defining the meta make function coercion
                (cond
                  [(and (Type? t1) (Type? t2))
                   (let* ([t1 (Type-type t1)] [t2 (Type-type t2)])
                     (unless (and (Fn? t1) (Fn? t2))
                       (error 'mk-fn-crcn$))
                     (let* ([t1-args (Fn-fmls t1)]
                            [t2-args (Fn-fmls t2)]
                            [t1-ret  (Fn-ret  t1)]
                            [t2-ret  (Fn-ret  t2)]
                            [arg* (map (help-comp mk-crcn lbl) t2-args t1-args)]
                            [ret  ((help-comp mk-crcn lbl) t1-ret t2-ret)])
                       (Quote-Coercion (Fn (Fn-arity t1) arg* ret))))]
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
                  [else (Make-Fn-Coercion uid t1 t2 lbl)]))

              (define-type Compose-Coercions-Type
                (CoC3-Trivial CoC3-Trivial -> CoC3-Expr))
              (: compose-coercions (Compose-Coercions-Type
                                    Uid
                                    Make-Coercion-Type ->
                                    Compose-Coercions-Type))
              (define ((compose-coercions compose compose-uid mk-crcn) c1 c2)
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
                   [(fail?$ c1) c2]
                   [else
                    ;; must be c1 & (g;I?)
                    (let$* ([seq_fst (seq-fst$ c2)]
                            [seq_snd (seq-snd$ c2)]
                            [comp_c1_final (compose c1 seq_fst)])
                           (seq$ comp_c1_final seq_snd))])]
                 ;; All Branches from here out will have to check if c2 is failure
                 ;; so we do it once to eliminate the possibility
                 [(fail?$ c2) (if$ (fail?$ c1) c1 c2)]
                 [(fnC?$ c1) ;; c2 must be a Function Coercion
                  ((compose-fn-crcn$
                    (compose-coercions compose compose-uid mk-crcn)
                    compose-uid)
                   c1 c2)]
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
                 
                 ;; note that there is no case for MonoRef coercion since each
                 ;; coercion is applied instantly to the heap cell, addresses are
                 ;; the only value form in monotonic.

                 ;; C1 must be a failed coercion
                 [else (Blame (fail-label$ c1))]))

              (: compose-fn-crcn$ (Compose-Coercions-Type Uid -> Compose-Coercions-Type))
              (define ((compose-fn-crcn$ compose compose-uid) c1 c2)
                (: help-comp (Compose-Coercions-Type
                              -> (Schml-Coercion Schml-Coercion
                                                 -> Schml-Coercion)))
                (define ((help-comp compose) s t)
                  (let ([c (compose (Quote-Coercion s) (Quote-Coercion t))])
                    (if (Quote-Coercion? c)
                        (Quote-Coercion-const c)
                        (error 'compose-fn-coercn/help))))
                (cond
                  [(and (Quote-Coercion? c1) (Quote-Coercion? c2))
                   (let* ([c1 (Quote-Coercion-const c1)]
                          [c2 (Quote-Coercion-const c2)])
                     (unless (and (Fn? c1) (Fn? c2))
                       (error 'compose-fn-crcn$ "given ~a ~a" c1 c2))
                     (let* ([c1-args (Fn-fmls c1)]
                            [c2-args (Fn-fmls c2)]
                            [c1-ret  (Fn-ret  c1)]
                            [c2-ret  (Fn-ret  c2)]
                            [arg*    (map (help-comp compose) c2-args c1-args)]
                            [ret     ((help-comp compose) c1-ret c2-ret)])
                       (Quote-Coercion (Fn (Fn-arity c1) arg* ret))))]
                  ;; TODO Consider the case were we have one but not the other type
                  [else (Compose-Fn-Coercion compose-uid c1 c2)]))

              (define interp-binding : CoC3-Bnd-Code
                (let* ([v  (next-uid! "value")]
                       [c (next-uid! "coercion")])
                  `(,interp-uid
                    .
                    ,(Code (list v c)
                           ((cast/coercion interp mk-coercion compose)
                            (Var v) (Var c))))))

              (define compose-binding? : (Option CoC3-Bnd-Code)
                (if (not space-efficient?)
                    #f
                    (let* ([c1 (next-uid! "coercion1")]
                           [c2 (next-uid! "coercion2")])
                      `(,compose-uid
                        .
                        ,(Code (list c1 c2)
                               ((compose-coercions compose compose-uid mk-coercion)
                                (Var c1) (Var c2)))))))

              (define mk-coercion-binding
                (let ([t1 (next-uid! "type1")]
                      [t2 (next-uid! "type2")]
                      [l  (next-uid! "label")])
                  `(,mk-coercion-uid
                    .
                    ,(Code (list t1 t2 l)
                           ((make-coercion mk-coercion mk-coercion-uid)
                            (Var t1) (Var t2) (Var l))))))

              (define glb-binding
                (let ([t1 (next-uid! "type1")]
                      [t2 (next-uid! "type2")])
                  `(,glb-uid
                    .
                    ,(Code (list t1 t2)
                           ((greatest-lower-bound$ mk-glb-call glb-uid) (Var t1) (Var t2))))))
              
              ;; ic-expr maps over the expressions lowering function cast
              ;; into calls to the runtime cast interpreter and direct manipulation
              ;; of the representation for each type and value.
              (: ic-expr (CoC2-Expr -> CoC3-Expr))
              (define (ic-expr exp)
                (match exp
                  ;; Interesting Transformations
                  [(Interpreted-Cast (app ic-expr v) (Coercion (app ic-expr c)))
                   (if (and (specialize-casts?) (Quote-Coercion? c))
                       (interp v c)
                       (interp v c))]
                  [(Cast (app ic-expr v) (Coercion c))
                   (if (specialize-casts?)
                       (interp v (Quote-Coercion c))
                       (interp v (Quote-Coercion c)))]
                  [(Compose-Coercions c1 c2)
                   (compose (ic-expr c1) (ic-expr c2))]
                  [(Fn-Proxy i e1 e2)
                   (Fn-Proxy (list i interp-uid) (ic-expr e1) (ic-expr e2))]
                  ;; We should consider desugaring the data structure rep
                  ;; here if we are using it. That translation makes more
                  ;; sense but is weird because we defer hybrid until
                  ;; closure conversion. 
                  [(App/Fn-Proxy-Huh e e*)
                   (App-Fn-or-Proxy interp-uid (ic-expr e) (map ic-expr e*))]
                  [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (ic-expr e))]
                  [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (ic-expr e))]
                  ;; In theory it may be better to implement the tranformation
                  ;; for apply here but it is currently postponed until
                  ;; closure conversion.
                  [(App-Fn e e*) (App-Fn (ic-expr e) (map ic-expr e*))]
                  ;; Uniteresting Recursion till the end
                  [(Id-Coercion-Huh e)
                   (Id-Coercion-Huh (ic-expr e))]
                  [(Fn-Coercion e* e)
                   (Fn-Coercion (map ic-expr e*) (ic-expr e))]
                  [(Fn-Coercion-Arg e1 e2)
                   (Fn-Coercion-Arg (ic-expr e1) (ic-expr e2))]
                  [(Fn-Coercion-Return e)
                   (Fn-Coercion-Return (ic-expr e))]
                  [(Ref-Coercion e1 e2)
                   (Ref-Coercion (ic-expr e1) (ic-expr e2))]
                  [(Ref-Coercion-Read e)
                   (Ref-Coercion-Read (ic-expr e))]
                  [(Ref-Coercion-Write e)
                   (Ref-Coercion-Write (ic-expr e))]
                  [(Quote-Coercion c)
                   (Quote-Coercion c)]
                  [(Code-Label u)
                   (Code-Label u)]
                  [(Labels c* e)
                   (Labels (map ic-bndc c*) (ic-expr e))]
                  [(App-Code e e*)
                   (App-Code (ic-expr e) (map ic-expr e*)) ]
                  [(Lambda f* (Castable ctr e))
                   (Lambda f* (Castable ctr (ic-expr e)))]
                  [(Letrec b* e)
                   (Letrec (map ic-bnd b*) (ic-expr e))]
                  [(Let b* e)
                   (Let (map ic-bnd b*) (ic-expr e))]
                  [(Op p e*)
                   (Op p (map ic-expr e*))]
                  [(Fn-Caster e)
                   (Fn-Caster (ic-expr e))]
                  ;; Type manipulation
                  [(Type-Fn-arg e i)
                   (Type-Fn-arg (ic-expr e) (ic-expr i))]
                  [(Type-Fn-return e)
                   (Type-Fn-return (ic-expr e))]
                  [(Type-Fn-arity e)
                   (Type-Fn-arity (ic-expr e))]
                  [(Blame e)
                   (Blame (ic-expr e))]
                  [(If tst csq alt)
                   (If (ic-expr tst) (ic-expr csq) (ic-expr alt))]
                  [(Var i) (Var i)]
                  [(Type t) (Type t)]
                  [(Quote k) (Quote k)]
                  [(Begin e* e)
                   (Begin (map ic-expr e*) (ic-expr e))]
                  [(Repeat i e1 e2 e3)
                   (Repeat i (ic-expr e1) (ic-expr e2) (ic-expr e3))]
                  ;; Proxies for functions
                  [(Fn-Proxy-Coercion e)
                   (Fn-Proxy-Coercion (ic-expr e))]
                  ;; Unguarded references
                  [(Unguarded-Box e)
                   (Unguarded-Box (ic-expr e))]
                  [(Unguarded-Box-Ref e)
                   (Unguarded-Box-Ref (ic-expr e))]
                  [(Unguarded-Box-Set! e1 e2)
                   (Unguarded-Box-Set! (ic-expr e1) (ic-expr e2))]
                  [(Unguarded-Vect e1 e2)
                   (Unguarded-Vect (ic-expr e1) (ic-expr e2))]
                  [(Unguarded-Vect-Ref e1 e2)
                   (Unguarded-Vect-Ref (ic-expr e1) (ic-expr e2))]
                  [(Unguarded-Vect-Set! e1 e2 e3)
                   (Unguarded-Vect-Set! (ic-expr e1) (ic-expr e2) (ic-expr e3))]
                  ;; Proxies for references
                  [(Guarded-Proxy-Huh e)
                   (Guarded-Proxy-Huh (ic-expr e))]
                  [(Guarded-Proxy e (Coercion c))
                   (Guarded-Proxy (ic-expr e) (Coercion (ic-expr c)))]
                  [(Guarded-Proxy-Ref e)
                   (Guarded-Proxy-Ref (ic-expr e))]
                  [(Guarded-Proxy-Coercion e)
                   (Guarded-Proxy-Coercion (ic-expr e))]
                  ;; Casted Values
                  [(CastedValue-Huh e)
                   (CastedValue-Huh (ic-expr e))]
                  [(CastedValue e (Coercion c))
                   (CastedValue (ic-expr e) (Coercion (ic-expr c)))]
                  [(CastedValue-Value e)
                   (CastedValue-Value (ic-expr e))]
                  [(CastedValue-Coercion e)
                   (CastedValue-Coercion (ic-expr e))]
                  [(Mbox e t) (Mbox (ic-expr e) t)]
                  [(Mbox-val-set! e1 e2) (Mbox-val-set! (ic-expr e1) (ic-expr e2))]
                  [(Mbox-val-ref e) (Mbox-val-ref (ic-expr e))]
                  [(Mbox-rtti-set! u e) (Mbox-rtti-set! u (ic-expr e))]
                  [(Mbox-rtti-ref u) (Mbox-rtti-ref u)]
                  [(Mvector e1 e2 t) (Mvector (ic-expr e1) (ic-expr e2) t)]
                  [(Mvector-val-set! e1 e2 e3)
                   (Mvector-val-set! (ic-expr e1) (ic-expr e2) (ic-expr e3))]
                  [(Mvector-val-ref e1 e2)
                   (Mvector-val-ref (ic-expr e1) (ic-expr e2))]
                  [(Mvector-rtti-set! u e) (Mvector-rtti-set! u (ic-expr e))]
                  [(Mvector-rtti-ref u) (Mvector-rtti-ref u)]
                  [(Make-Coercion e1 e2) (mk-coercion (ic-expr e1) (ic-expr e2) (Quote ""))]
                  ;; Coercions manipulation
                  [other
                   (if (or (Guarded-Proxy-Source? other)
                           (Guarded-Proxy-Target? other)
                           (Guarded-Proxy-Blames? other)
                           (CastedValue-Source? other)
                           (CastedValue-Target? other)
                           (CastedValue-Blames? other)
                           )
                       (error 'ic-expr "twosome code in coercion pass: ~a" other)
                       (error 'ic-expr "umatched ~a" other))]))

              ;; map through a code binding 
              (: ic-bndc (CoC2-Bnd-Code -> CoC3-Bnd-Code))
              (define (ic-bndc b)
                (match-let ([(cons u (Code u* e)) b])
                  (cons u (Code u* (ic-expr e)))))

              ;; map through a data binding 
              (: ic-bnd (CoC2-Bnd -> CoC3-Bnd))
              (define (ic-bnd b)
                (cons (car b) (ic-expr (cdr b))))

              ;; body of interpret-casts
              (let* ([exp (ic-expr prgm-exp)]
                     [next (unbox next-unique-number)]
                     [bnd* (list mk-coercion-binding interp-binding glb-binding)]
                     ;; compose-binding? is correlates space-efficient?
                     ;; but typed-racket wants to know the binding
                     ;; isn't false.
                     [bnd* (if compose-binding?
                               (cons compose-binding? bnd*)
                               bnd*)])
                (Prog (list prgm-name next prgm-type)
                      (Labels bnd* (Observe exp prgm-type)))))
