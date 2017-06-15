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
         (except-in "./interpret-casts-help.rkt"
                    let$* cond$ op=? and$ or$)
         (submod "../logging.rkt" typed)
         racket/match
         racket/list
         (for-syntax syntax/parse typed/racket/base))


(: interpret-casts/hyper-coercions
   : Cast0.5-Lang -> Cast-or-Coerce3-Lang)
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


(define (interpret-casts/hyper-coercions prgm)
  (match-define (Prog (list name next type) e) prgm)
  
  ;; All the implicit state of this program
  (define next-unique (make-unique-counter next))
  
  (parameterize ([current-unique-counter next-unique])
    ;; The ids for everything that is manditory
    (define interp-cast-uid
      (next-uid! "apply_hyper_coercion"))
    (define interp-med-cast-uid
      (next-uid! "apply_mediating_coercion"))
    (define interp-compose-coercions-uid
      (next-uid! "compose_coercions"))
    (define interp-compose-med-coercions-uid
      (next-uid! "compose_med_coercions"))
    (define interp-compose-fn-coercions-uid
      (next-uid! "compose-fn-coercions"))
    (define interp-compose-tuple-coercions-uid
      (next-uid! "compose-tuple-coercions"))
    (define interp-make-coercion-uid
      (next-uid! "make_hyper_coercion"))
    (define interp-make-med-coercion-uid
      (next-uid! "make_med_coercion"))
    (define greatest-lower-bound-uid
      (next-uid! "greatest_lower_bound"))
    (define copy-mref-uid
      (next-uid! "copy_mref_value"))
    (define interp-projection-uid
      (next-uid! "project_dynamic"))
    ;; Generate calls to runtime procedures
    (define interp-compose-coercions : Interp-Compose-Coercions-Type
      (apply-code interp-compose-coercions-uid))
    (define interp-compose-fn-coercions
      : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
      (apply-code interp-compose-fn-coercions-uid))
    (define interp-compose-tuple-coercions
      : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
      (apply-code interp-compose-tuple-coercions-uid))
    (define interp-compose-med-coercions : Interp-Compose-Coercions-Type
      (apply-code interp-compose-med-coercions-uid))
    (define interp-make-coercion : Interp-Make-Coercion-Type
      (apply-code interp-make-coercion-uid))
    (define interp-make-med-coercion : Interp-Make-Coercion-Type
      (apply-code interp-make-med-coercion-uid))
    (define greatest-lower-bound : Greatest-Lower-Bound-Type
      (apply-code greatest-lower-bound-uid))
    (define copy-mref : Copy-Mref-Type
      (apply-code copy-mref-uid))
    (define Id (Quote-Coercion IDENTITY))
    (: interp-cast : Interp-Cast-Type)
    (define (interp-cast e c [m (Quote 0)])
      ;; I am only using (Quote 0) because that was what the coercions
      ;; version of this pass used. -- Andre
      (((inst apply-code CoC3-Expr) interp-cast-uid) e c m))
    (: interp-med-cast : Interp-Cast-Type)
    (define (interp-med-cast e c [m (Quote 0)])
      (((inst apply-code CoC3-Expr) interp-med-cast-uid) e c m))
    (: interp-projection : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (interp-projection e t2 l)
      (((inst apply-code CoC3-Expr) interp-projection-uid) e t2 l))
    ;; Code Generators for the various casts
    (: compile-projection : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (compile-projection e t2 l)
      ;; incoming expression let-bound to prevent expression duplication      
      (let* ([v e] [l l])
        (match t2
          [(Type (or (Int) (Character) (Unit) (Bool)))
           (If (dyn-immediate-tag=?$ v t2)
               (dyn-immediate-value$ v)
               (interp-projection v t2 l))]
          [(Type _) 
           (If (dyn-immediate-tag=?$ v t2)
               (let*$ ([u  (dyn-box-value$ v)]
                       [t1 (dyn-box-type$ v)])
                 (If (op=? t1 t2)
                     u
                     ;; TODO: compile-med-cast should exists
                     (interp-med-cast u (interp-make-med-coercion t1 t2 l))))
               (interp-projection v t2 l))]
        [otherwise
         (let$ ([t2 t2])
           (let*$ ([dv (dyn-value$ v)]
                   [t1 (dyn-type$ v)])
             (If (op=? t1 t2)
                 dv
                 ;; t1 != t2 -> < t1 =>^l t2> != Id
                 ;; therefore no need to make Id case fast 
                 (interp-med-cast dv (interp-make-med-coercion t1 t2 l)))))])))
    (: interp-projection-code  (Code Uid* CoC3-Expr))
    (define interp-projection-code
      ;; Using variables result in generating the generic code that is constant but large
      (code$ (v t2 l) (compile-projection v t2 l)))
    
    (: compile-injection : CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (compile-injection e t)
      ;; Dyn make generates specialized code when t is known
      (dyn-make$ e t))

    (: compile-fn-cast : CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (compile-fn-cast e m)
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
    
    (: compile-tup-cast (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
    (define (compile-tup-cast e m [mt (Quote 0)])
      (match mt
        [(Quote 0) (Coerce-Tuple interp-cast-uid e m)]
        [_
         (let$ ([v e][m m][mt mt])
           (If (Op '= (list (Quote 0) mt))
               (Coerce-Tuple interp-cast-uid v m)
               (Coerce-Tuple-In-Place interp-cast-uid v m mt)))]))

    (: compile-ref-cast : CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (compile-ref-cast e m)
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
                    [r-write (interp-compose-coercions m-write o-write)]
                    [o-read  (Ref-Coercion-Read old-m)]
                    [m-read  (Ref-Coercion-Read m)]
                    [r-read  (interp-compose-coercions o-read m-read)])
              (cond$
               [(and$ (HC-Identity-Huh r-read) (HC-Identity-Huh r-write))
                old-v]
               [else
                (Guarded-Proxy old-v (Coercion (Ref-Coercion r-read r-write)))])))]
         [else (Guarded-Proxy v (Coercion m))])))
    (: compile-mref-cast : CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (compile-mref-cast e t2)
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
                      [c  (interp-make-coercion t1 t3 (Quote ""))]
                      [new-v (interp-cast v-copy c v)]
                      [t4 (Mbox-rtti-ref v)])
                (cond$
                 [(op=? t3 t4) (Mbox-val-set! v new-v) v]
                 [else v]))]))])))
    (: compile-mvec-cast : CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (define (compile-mvec-cast e t2)
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
              (let$ ([c   (interp-make-coercion t1 t3 (Quote ""))]
                     [len (Mvector-length v)])
                (cond$
                 [(Type-Tuple-Huh t3)
                  (let*$ ([n (Type-Tuple-num t3)])
                    (repeat$ (i (Quote 0) len) ()
                      (let*$ ([vi (Mvector-val-ref v i)]
                              [cvi (Copy-Tuple n vi)])
                        (Mvector-val-set! v i cvi)
                        (let*$ ([ccvi (interp-cast cvi c v)]
                                [t4 (Mvector-rtti-ref v)])
                          (if$ (op=? t3 t4)
                               (Mvector-val-set! v i ccvi)
                               (Break-Repeat))))))]
                 [else
                  (repeat$ (i (Quote 0) len) ()
                    (let*$ ([vi (Mvector-val-ref v i)]
                            [cvi (interp-cast vi c v)]
                            [t4 (Mvector-rtti-ref v)])
                      (If (op=? t3 t4)
                          (Mvector-val-set! v i cvi)
                          (Break-Repeat))))]))
              v]))])))
    (: interp-cast-code (Code Uid* CoC3-Expr))
    (define interp-cast-code
      (code$ (v c mt)
        (let*$ ([v1 (cond$
                     [(HC-Project-Huh c)
                      (compile-projection v (HC-T1 c) (HC-Label c))] 
                     [else v])]
                [m (HC-Med c)]
                [v2 (If (Id-Coercion-Huh m)
                        v1
                        (interp-med-cast v1 m mt))])
          (If (HC-Inject-Huh c)
              (If (and$ (Id-Coercion-Huh m) (HC-Project-Huh c))
                  v
                  (compile-injection v2 (HC-T2 c)))
              v2))))
    (: interp-med-cast-code (Code Uid* CoC3-Expr))
    (define interp-med-cast-code 
      (code$ (v m mt)
        (precondition$
            (not$ (Id-Coercion-Huh m))
          (cond$
           ;; Todo Make mediating-coercion? precisely corespond
           ;; to the hyper-coercion notion of mediating-coercions?
           [(Mediating-Coercion-Huh m)
            (cond$
             [(Fn-Coercion-Huh m)    (compile-fn-cast v m)]
             [(Tuple-Coercion-Huh m) (compile-tup-cast v m mt)]
             [(Ref-Coercion-Huh m)   (compile-ref-cast v m)]
             [(MRef-Coercion-Huh m)
              (compile-mref-cast v (MRef-Coercion-Type m))]
             [(MVect-Coercion-Huh m)
              (compile-mvec-cast v (MVect-Coercion-Type m))]
             [else
              (Blame
               (Quote "Internal Error: hyper-coercions/interp-med-cast 1"))])]
           ;; This isn't needed with the current implementation
           ;; but I want to include it because the name indicates that it should
           ;; work, and including it only slows down the failure-
           [(Id-Coercion-Huh m) v]
           [(Failed-Coercion-Huh m) (Blame (Failed-Coercion-Label m))] 
           [else
            (Blame
             (Quote "Internal Error: hyper-coercions/interp-med-cast 2"))]))))
    ;; Cunstruct a hyper-coercions expression from the type and blame
    ;; information contained in a type-based cast.
    ;; This coercion literal is eligable to be hoisted in a later
    ;; pass so that it only ever is creates a value once. 
    (: make-coercion : Make-Coercion-Type)
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
    (: interp-make-coercion-code (Code Uid* CoC3-Expr))
    (define interp-make-coercion-code
      #;(define-values (t1u t2u lu)
          (values (next-uid! "t1") (next-uid! "t2") (next-uid! "blame_info")))
      #;(define-values (t1 t2 l)
          (values (Var t1u) (Var t2u) (Var lu)))
      (code$ (t1 t2 l)
        (cond$
         [(op=? t1 t2)
          (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t1 Id) CoC3-Expr)]
         ;; This is absolutly necisarry
         ;; While Injections and Projections are never made by
         ;; source code coercions composition can create new
         ;; projections and injections.
         [(Type-Dyn-Huh t1)
          (ann (HC (Quote #t) t2 l (Quote #f) t2 Id) CoC3-Expr)]
         [(Type-Dyn-Huh t2)
          (ann (HC (Quote #f) t1 (Quote #f) (Quote #t) t1 Id) CoC3-Expr)]
         [else
          (let$ ([med (interp-make-med-coercion t1 t2 l)]) 
            (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t2 med) CoC3-Expr))])))
    (: interp-make-med-coercion-code
       (Code Uid* CoC3-Expr))
    (define interp-make-med-coercion-code
      ;; precondition: t1 != Dyn /\ t2 != Dyn /\ t1 != t2
      ;; Should be called make_non_id_med_coercion
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
                  (op<=? (tupleT-num$ t2) (tupleT-num$ t1)))
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
            (MRef-Coercion (mref-of$ t2))]
           [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
            (MVect-Coercion (mvect-of$ t2))]
           [else (Failed-Coercion l)]))))
    (: interp-compose-coercions-code (Code Uid* CoC3-Expr))
    (define interp-compose-coercions-code
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
                               ;; interp-make-med-coercion assumes that fst-t2 != snd-t1
                               (not$ (op=? fst-t2 snd-t1)))
                         (let*$ ([mid (interp-make-med-coercion
                                       fst-t2 snd-t1 (HC-Label snd))])
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
    (: interp-compose-med-coercions-code (Code Uid* CoC3-Expr))
    (define interp-compose-med-coercions-code
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
            (interp-compose-fn-coercions
             fst snd (Quote 0) (Fn-Coercion-Arity fst) (Quote #t))]
           [(and$ (Tuple-Coercion-Huh fst) (Tuple-Coercion-Huh snd))
            (interp-compose-tuple-coercions
             fst snd (Quote 0) (Tuple-Coercion-Num fst) (Quote #t))]
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
    (: interp-compose-fn-coercions-code (Code Uid* CoC3-Expr))
    (define interp-compose-fn-coercions-code
      ;; TODO Implement Fn-coercion-set! Make-Fn-Coercion
      ;; TODO make this iterative
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
    (: interp-compose-tuple-coercions-code (Code Uid* CoC3-Expr))
    (define interp-compose-tuple-coercions-code
      ;; TODO make this iterative 
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
    (define greatest-lower-bound-code
      (code$ (t1 t2)
        ((gen-greatest-lower-bound-type-code
          next-uid!
          greatest-lower-bound
          greatest-lower-bound-uid)
         t1 t2)))
    (define copy-mref-code
      (code$ (mref)
        ((gen-copy-value-in-monoref-code next-uid!) mref)))

    (: compile-make-coercion Compile-Make-Coercion-Type)
    ;; TODO : can this be any better?
    (define (compile-make-coercion t1 t2 l)
      (match* (t1 t2 l)
        [((Type t1) (Type t2) (Quote (? blame-label? l)))
         (make-coercion t1 t2 l)]
        [(t1 t2 l) (interp-make-coercion t1 t2 l)]))

    (: compile-cast Compile-Cast-Type)
    (define (compile-cast e t1 t2 l
                          #:t1-not-dyn [t1-not-dyn : Boolean #f]
                          #:t2-not-dyn [t2-not-dyn : Boolean #f])
      (let*$ ([v e])
        (match* (t1 t2)
          [((Type t) (Type t)) v]
          ;; We don't have to check not Dyn here because t1 <> t2
          [((Type (Dyn)) (Type _))
           (compile-projection v t2 l)]
          [((Type (Dyn)) t2)
           (cond
             [t2-not-dyn (compile-projection v t2 l)]
             [else
              (If (Type-Dyn-Huh t2)
                  v
                  (compile-projection v t2 l))])]
          ;; We don't have to check not dyn here because t1 <> t2
          [((Type _) (Type (Dyn))) (compile-injection v t1)]
          [(t1 (Type (Dyn)))
           (cond
             [t1-not-dyn (compile-injection v t1)]
             [else
              (If (Type-Dyn-Huh t1)
                  v
                  (compile-injection v t1))])]
          [(t1 t2)
           (define hc : CoC3-Expr (compile-make-coercion t1 t2 l))
           (debug 'compile-cast/hyper-coercions hc)
           (match hc
             [hc #:when (not (optimize-first-order-coercions?))
                 (interp-cast v hc)]
             ;; The next cases should never occur because the previous
             ;; code handles them without a call to make-coercion.
             [(Quote-HCoercion (HC #f _ #f #f _ (Identity)))
              e]
             ;; Hyper-Coercion is Projection 
             [(Quote-HCoercion (HC #t t1 (? blame-label? l) #f _ (Identity)))
              (compile-projection v (Type t1) (Quote l))] 
             ;; Hyper-Coercion is Injection
             [(Quote-HCoercion (HC #f _ #f #t t2 (Identity)))
              (compile-injection v (Type t2))]
             ;; Hyper-Coercion is a Funtion Coercion
             [(Quote-HCoercion (HC #f _ #f #f _ (and m (Fn a _ _))))
              #:when (direct-fn-cast-optimization?)
              ;; Fn-casts use specialize code for their arity
              ;; Since we are generating it here it is trivial to
              ;; directly call that code.
              (let ([caster (get-fn-cast! a)])
                (App-Code (Code-Label caster)
                          (list v (Quote-HCoercion m))))]
             [(Quote-HCoercion (HC #f _ #f #f _ (and m (Ref c1 c2))))
              (compile-ref-cast v (Quote-HCoercion m))]
             [(Quote-HCoercion (HC #f _ #f #f _ (and m (CTuple n c*))))
              (compile-tup-cast v (Quote-HCoercion m))]
             [(Quote-HCoercion (HC #f _ #f #f _ (MonoRef t2)))
              (compile-mref-cast v (Type t2))]
             [(Quote-HCoercion (HC #f _ #f #f _ (MonoVect t2)))
              (compile-mvec-cast v (Type t2))]
             [(Quote-HCoercion (HC #f _ #f #f _ m))
              (interp-med-cast v (Quote-HCoercion m))]
             [hc (interp-cast v hc)])])))
    ;; The function cast machinery
    ;; Internal State: map of arity specific casters
    (define fn-casts : (HashTable Nat CoC3-Bnd-Code)
      (make-hasheq))
    (define name-base : String "fn_cast_code")
    ;; build-caster! generates a piece of code that performs a fn-cast
    ;; for a specific arity. 

    ;; This could be done as 3 seperate casters one that handles both
    ;; casted and uncasted closures and another each seperately.
    ;; The general caster is used in static code
    ;; The uncasted is attatched to the original closure
    ;; The casted is attatched to the proxy
    (: build-caster! : Nat Uid -> (Code Uid* CoC3-Expr))
    (define (build-caster! arity name)
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
        (map (lambda ([x : Uid]) (HC-Identity-Huh (Var x))) x*))
      ;; new fn-crcn generates the result if a new coercion is needed
      (: new-fn-crcn : Uid Uid Nat Uid* Uid -> CoC3-Expr)
      (define (new-fn-crcn t1 t2 arity args ret)
        (Fn-Coercion (map (inst Var Uid) args) (Var ret)))
      ;; compose-return generate a bindining that composes the return
      ;; of two hyper-coercions if present
      (: compose-return (Uid (Var Uid) (Var Uid) -> CoC3-Bnd))
      (define (compose-return ret old-crcn new-crcn)
        `[,ret . ,(interp-compose-coercions
                   (Fn-Coercion-Return old-crcn)
                   (Fn-Coercion-Return new-crcn))])
      ;; compose-arg generates a binding that composes the ith
      ;; fn-coercion of the two input coercions. 
      (: compose-arg (Uid (Var Uid) (Var Uid) (Quote Index) -> CoC3-Bnd))
      (define (compose-arg arg old-crcn new-crcn i)
        `[,arg . ,(interp-compose-coercions
                   (Fn-Coercion-Arg new-crcn i)
                   (Fn-Coercion-Arg old-crcn i))])
      ;; the ids to build a new code
      (define u-clos (next-uid! "unknown_closure"))
      (define new-crcn (next-uid! "new_fn_coercion"))
      (define arg* (for/list : (Listof Uid) ([i (in-range arity)])
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
                    (Fn-Proxy (list #{arity :: Index} interp-cast-uid)
                              raw-clos
                              (new-fn-crcn t1 t2 arity arg* ret)))))
            ;; Closure is unproxied --> just make a new proxy
            (Fn-Proxy (list #{arity :: Index} interp-cast-uid) fun crcn))))
    ;; Get the uid of fn-cast that can handle 
    (: get-fn-cast! : Nat -> Uid)
    (define (get-fn-cast! arity)
      (let ([bnd? (hash-ref fn-casts arity #f)])
        (cond
          [bnd? (car bnd?)]
          [else
           (define name (string-append name-base (number->string arity)))
           (define caster-uid  (next-uid! name))
           (define caster-code (build-caster! arity caster-uid))
           (define caster-bnd  (cons caster-uid caster-code))
           (hash-set! fn-casts arity caster-bnd)
           caster-uid])))

    (: compile-lambda Compile-Lambda-Type)
    (define (compile-lambda f* expr)
      (let ([caster (get-fn-cast! (length f*))])
        (Lambda f* (Castable caster expr))))
    (: compile-app Compile-App-Type)
    (define (compile-app e e*)
      (App-Fn-or-Proxy interp-cast-uid e e*))


    ;; Proxied References Implementation
    (: make-gbox-ref-code PBox-Ref-Type)
    (define (make-gbox-ref-code gref)
      (If (Guarded-Proxy-Huh gref)
          (interp-cast
           (Unguarded-Box-Ref (Guarded-Proxy-Ref gref))
           (Ref-Coercion-Read (Guarded-Proxy-Coercion gref)))
          (Unguarded-Box-Ref gref)))

    (: make-gbox-set!-code  PBox-Set-Type)
    (define (make-gbox-set!-code gref val)
      (If (Guarded-Proxy-Huh gref)
          (Unguarded-Box-Set!
           (Guarded-Proxy-Ref gref)
           (interp-cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref))))
          (Unguarded-Box-Set! gref val)))

    (: make-gvect-ref-code  PVec-Ref-Type)
    (define (make-gvect-ref-code gref index)
      (If (Guarded-Proxy-Huh gref)
          (interp-cast (Unguarded-Vect-Ref (Guarded-Proxy-Ref gref) index)
                       (Ref-Coercion-Read (Guarded-Proxy-Coercion gref)))
          (Unguarded-Vect-Ref gref index)))

    (: make-gvect-length-code PVec-Len-Type)
    (define (make-gvect-length-code gvect)
      (If (Guarded-Proxy-Huh gvect)
          (Unguarded-Vect-length (Guarded-Proxy-Ref gvect))
          (Unguarded-Vect-length gvect)))

    (: make-gvect-set!-code PVec-Set-Type)
    (define (make-gvect-set!-code gref index val)
      (: index-exp CoC3-Expr)
      (define index-exp (if (integer? index) (Quote index) index))
      (If (Guarded-Proxy-Huh gref)
          (Unguarded-Vect-Set!
           (Guarded-Proxy-Ref gref)
           index-exp
           (interp-cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref))))
          (Unguarded-Vect-Set! gref index-exp val)))

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
    (: pbox-ref  PBox-Ref-Type)
    (: pbox-set! PBox-Set-Type)
    (: pvec-ref  PVec-Ref-Type)
    (: pvec-set! PVec-Set-Type)
    (: pvec-len  PVec-Len-Type)
    (: proxied-operations-bindings CoC3-Bnd-Code*)
    (define-values (pbox-ref pbox-set!
                             pvec-ref pvec-set! pvec-len
                             proxied-operations-bindings)
      (cond
        [(inline-guarded-branch?)
         ;; we just hand back the code generators to build
         ;; inline code everywhere.
         (values make-gbox-ref-code
                 make-gbox-set!-code
                 make-gvect-ref-code
                 make-gvect-set!-code
                 make-gvect-length-code
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
            (apply-code pbr) (apply-code pbs)
            (apply-code pvr) (apply-code pvs) (apply-code pvl)
            `([,pbr . ,(code$ (pbox) (make-gbox-ref-code pbox))]
              [,pbs . ,(code$ (pbox value) (make-gbox-set!-code pbox value))]
              [,pvr . ,(code$ (pvec index) (make-gvect-ref-code pvec index))]
              [,pvs . ,(code$ (pvec index value)
                         (make-gvect-set!-code pvec index value))]
              [,pvl . ,(code$ (pvec) (make-gvect-length-code pvec))])))]))

    ;; Monotonic Reference Types Implementation
    (: make-mbox-ref-code MBox-Ref-Type)
    (define (make-mbox-ref-code mref t2)
      (let*$ ([mref mref] 
              [t1 (Mbox-rtti-ref mref)]
              [crcn (interp-make-coercion t1 t2 (Quote ""))])
        (interp-cast (Mbox-val-ref mref) crcn)))

    
    (: make-mbox-set!-code  MBox-Set-Type)
    (define (make-mbox-set!-code mref val t1)
      (let*$ ([mref mref] [val  val] [t1 t1]
              [t2 (Mbox-rtti-ref mref)]
              [c (interp-make-coercion t1 t2 (Quote ""))]
              [cv (cond$
                   [(and$ (tupleT?$ t1) (tupleT?$ t2))
                    (let*$ ([n (Type-Tuple-num t2)]
                            [ctv (Copy-Tuple n val)])
                      (begin$
                        (Mbox-val-set! mref ctv)
                        ctv))]
                   [else val])]
              [ccv (interp-cast cv c mref)]
              [t2-new (Mbox-rtti-ref mref)])
        (If (op=? t2 t2-new)
            (Mbox-val-set! mref ccv)
            (Quote '()))))
    (: make-mvect-ref-code  MVec-Ref-Type)
    (define (make-mvect-ref-code mvect i t2)
      (let*$ ([mvect mvect] [i i] [t2 t2]
              [t1 (Mvector-rtti-ref mvect)]
              [crcn (interp-make-coercion t1 t2 (Quote ""))])
        (interp-cast (Mvector-val-ref mvect i) crcn)))
    
    (: make-mvect-set!-code MVec-Set-Type)
    (define (make-mvect-set!-code mvect i val t1)
      (let*$ ([mvect mvect]
              [i i]
              [val val]
              [t1 t1]
              [t2 (Mvector-rtti-ref mvect)]
              [c (interp-make-coercion t1 t2 (Quote ""))])
        (cond$
         [(and$ (tupleT?$ t1) (tupleT?$ t2))
          (let*$ ([n (Type-Tuple-num t2)]
                  [cvi (Copy-Tuple n val)])
            (begin$
              (Mvector-val-set! mvect i cvi)
              (let*$ ([ccvi (interp-cast cvi c mvect)]
                      [t2-new (Mvector-rtti-ref mvect)])
                (If (op=? t2 t2-new)
                    (Mvector-val-set! mvect i ccvi)
                    (Quote 0)))))]
         [else
          (let*$ ([cvi (interp-cast val c mvect)]
                  [t2-new (Mvector-rtti-ref mvect)])
            (If (op=? t2 t2-new)
                (Mvector-val-set! mvect i cvi)
                (Quote 0)))])))
    
    (: mbox-set! MBox-Set-Type)
    (: mbox-ref MBox-Ref-Type)
    (: mvec-ref MVec-Ref-Type)
    (: mvec-set! MVec-Set-Type)
    (: monotonic-operations-bindings CoC3-Bnd-Code*)
    (define-values (mbox-ref mbox-set! mvec-ref mvec-set! monotonic-operations-bindings)
      (cond
        [(inline-guarded-branch?)
         (values make-mbox-ref-code
                 make-mbox-set!-code
                 make-mvect-ref-code
                 make-mvect-set!-code
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
            (apply-code mbr) (apply-code mbs)
            (apply-code mvr) (apply-code mvs)
            `([,mbr . ,(code$ (mbox type) (make-mbox-ref-code mbox type))]
              [,mbs . ,(code$ (mbox value type) (make-mbox-set!-code mbox value type))]
              [,mvr . ,(code$ (mvec index type) (make-mvect-ref-code mvec index type))]
              [,mvs . ,(code$ (mvec index value type)
                         (make-mvect-set!-code mvec index value type))])))]))
    
    
    ;; Dynamic Operation Specialization
    (: make-dyn-pbox-ref-code Dyn-PBox-Ref-Type)
    (define (make-dyn-pbox-ref-code dyn lbl)
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
    (: make-dyn-pbox-set!-code Dyn-PBox-Set-Type)
    (define (make-dyn-pbox-set!-code dyn-gbox wrt-val1 t2 lbl)
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
    
    (: make-dyn-pvec-ref-code Dyn-PVec-Ref-Type)
    (define (make-dyn-pvec-ref-code dyn ind lbl)
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
    
    (: make-dyn-pvec-set!-code Dyn-PVec-Set-Type)
    (define (make-dyn-pvec-set!-code dyn-gvec ind wrt-val1 t2 lbl) 
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
    
    (: make-dyn-pvec-len-code Dyn-PVec-Len-Type)
    (define (make-dyn-pvec-len-code dyn lbl)
      (let*$ ([dyn dyn] [lbl lbl])
        (cond$
         [(and$ (dyn-immediate-tag=?$ dyn PVEC-DYN-EXPR)
                (Type-GVect-Huh (dyn-box-type$ dyn)))
          (pvec-len (dyn-box-value$ dyn))]
         [else (Blame lbl)])))

    (: make-dyn-mbox-ref-code Dyn-MBox-Ref-Type)
    (define (make-dyn-mbox-ref-code dyn lbl)
      (let$ ([dyn dyn] [lbl lbl])
        (cond$
         [(and$ (dyn-immediate-tag=?$ dyn MBOX-DYN-EXPR)
                (Type-MRef-Huh (dyn-box-type$ dyn)))
          (mbox-ref (dyn-box-value$ dyn) DYN-EXPR)]
         [else (Blame lbl)])))
    
    (: make-dyn-mbox-set!-code Dyn-MBox-Set-Type)
    (define (make-dyn-mbox-set!-code dyn-mbox wrt-val1 t2 lbl)
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
    
    (: make-dyn-mvec-ref-code Dyn-MVec-Ref-Type)
    (define (make-dyn-mvec-ref-code dyn ind lbl)
      (let$ ([dyn dyn] [ind ind] [lbl lbl])
        (cond$
         [(and$ (dyn-immediate-tag=?$ dyn MVEC-DYN-EXPR)
                (Type-MVect-Huh (dyn-box-type$ dyn)))
          (mvec-ref (dyn-box-value$ dyn) ind DYN-EXPR)]
         [else (Blame lbl)])))
    
    (: make-dyn-mvec-set!-code Dyn-MVec-Set-Type)
    (define (make-dyn-mvec-set!-code dyn-mvec ind wrt-val1 t2 lbl) 
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
    
    (: make-dyn-fn-app-code Dyn-Fn-App-Type)
    (define (make-dyn-fn-app-code e e* t* le)
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
        (error 'interpret-casts-with-hyper-coercions/make-fn-app-code
               "expected types to be same length as arguments"))
      (define arg-casts : CoC3-Expr*
        (for/list : (Listof CoC3-Expr)
                  ([v : CoC3-Expr v*]
                   [t : Schml-Type t*]
                   [i (in-naturals)])
          (let$ ([dyn-fn-arg-type (Type-Fn-arg ty (Quote i))])
            (compile-cast v (Type t) dyn-fn-arg-type l))))
      (define casts-apply : CoC3-Expr
        (case (function-cast-representation)
          [(Hybrid) (App-Fn-or-Proxy interp-cast-uid u arg-casts)]
          [(Data)   (error 'todo "implement coercions data representation")]
          [(Functional) (error 'todo "incompatible with coercions")]
          [else (error 'interp-cast-with-coercions/dyn-fn-app "unexpected value")]))
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
    
    (: make-dyn-tup-prj-code Dyn-Tup-Prj-Type)
    (define (make-dyn-tup-prj-code e ie le)
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
    (: dynamic-operations-bindings CoC3-Bnd-Code*)
    (define-values (dyn-pbox-ref dyn-pbox-set!
                                 dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
                                 dyn-mbox-ref dyn-mbox-set!
                                 dyn-mvec-ref dyn-mvec-set!
                                 dyn-fn-app dyn-tup-prj
                                 dynamic-operations-bindings)
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
          make-dyn-pbox-ref-code
          make-dyn-pbox-set!-code
          make-dyn-pvec-ref-code
          make-dyn-pvec-set!-code
          make-dyn-pvec-len-code
          make-dyn-mbox-ref-code
          make-dyn-mbox-set!-code
          make-dyn-mvec-ref-code
          make-dyn-mvec-set!-code
          make-dyn-fn-app-code
          make-dyn-tup-prj-code
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
          (apply-code dpbr)
          (apply-code dpbs)
          (apply-code dpvr)
          (apply-code dpvs)
          (apply-code dpvl)
          (apply-code dmbr)
          (apply-code dmbs)
          (apply-code dmvr)
          (apply-code dmvs)
          make-dyn-fn-app-code ;; Always inline since code is arity dependent
          (apply-code dtp)
          `([,dpbr . ,(code$ (ref lbl)
                        (make-dyn-pbox-ref-code ref lbl))]
            [,dpbs . ,(code$ (ref val ty lbl)
                        (make-dyn-pbox-set!-code ref val ty lbl))]
            [,dpvr . ,(code$ (vec ind lbl)
                        (make-dyn-pvec-ref-code vec ind lbl))]
            [,dpvs . ,(code$ (vec ind val ty lbl)
                        (make-dyn-pvec-set!-code vec ind val ty lbl))]
            [,dpvl . ,(code$ (vec lbl) (make-dyn-pvec-len-code vec lbl))]
            [,dmbr . ,(code$ (ref lbl)
                        (make-dyn-mbox-ref-code ref lbl))]
            [,dmbs . ,(code$ (ref val ty lbl)
                        (make-dyn-mbox-set!-code ref val ty lbl))]
            [,dmvr . ,(code$ (vec ind lbl)
                        (make-dyn-mvec-ref-code vec ind lbl))]
            [,dmvs . ,(code$ (vec ind val ty lbl)
                        (make-dyn-mvec-set!-code vec ind val ty lbl))]
            [,dtp . ,(code$ (tup ind lbl)
                       (make-dyn-tup-prj-code tup ind lbl))]))]))
    

    
    (define-type Get-Cast-Code-Bindings-Type (-> CoC3-Bnd-Code*))
    (: get-cast-code-bindings! Get-Cast-Code-Bindings-Type)
    (define (get-cast-code-bindings!)
      (append
       (hash-values fn-casts)
       proxied-operations-bindings
       monotonic-operations-bindings
       dynamic-operations-bindings
       `([,interp-projection-uid . ,interp-projection-code]
         [,interp-cast-uid . ,interp-cast-code]
         [,interp-med-cast-uid . ,interp-med-cast-code]
         [,interp-make-coercion-uid . ,interp-make-coercion-code]
         [,interp-make-med-coercion-uid . ,interp-make-med-coercion-code]
         [,interp-compose-coercions-uid . ,interp-compose-coercions-code]
         [,interp-compose-med-coercions-uid
          . ,interp-compose-med-coercions-code]
         [,interp-compose-fn-coercions-uid
          . ,interp-compose-fn-coercions-code]
         [,interp-compose-tuple-coercions-uid
          . ,interp-compose-tuple-coercions-code]
         [,greatest-lower-bound-uid . ,greatest-lower-bound-code]
         [,copy-mref-uid . ,copy-mref-code])))
    (define map-expr!
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

    ;; This map-expr! call must occur before next-unique is
    ;; extracted because new unique variable are allocated in
    ;; this pass. Additionally calls to get-fn-caster! will
    ;; cause additional code to be generated for each arity
    ;; of function that exists; this code is extracted from
    ;; fn-casts.
    (define new-e (map-expr! e))

    ;; Reconstruct the entire program
    (Prog (list name (unique-counter-next! next-unique) type)
      (Labels (get-cast-code-bindings!) new-e))))


(define (make-map-expr
         #:compile-cast    [compile-cast : Compile-Cast-Type]
         #:compile-lambda  [compile-lambda : Compile-Lambda-Type]
         #:compile-app     [compile-app : Compile-App-Type]
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
         #:dyn-tup-prj  [dyn-tup-prj   : Dyn-Tup-Prj-Type])
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
    (debug 'interpret-cast-with-hyper-coercions/map-expr e)
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
      [(Dyn-GVector-Len (app recur e) (app recur l))
       (dyn-pvec-len e l)]
      [(Gvector-length (app recur e))
       (pvec-len e)]
      [(MBoxCastedRef addr t)
       (mbox-ref (Var addr) (Type t))]
      [(MBoxCastedSet! addr (app recur e) t)
       (mbox-set! (Var addr) e (Type t))]
      [(MVectCastedRef addr (app recur i) t)
       (mvec-ref (Var addr) i (Type t))]
      [(MVectCastedSet! addr (app recur i) (app recur e) t)
       (mvec-set! (Var addr) i e (Type t))]
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
       (Mvector-val-ref e1 e2)]
      [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
       (Mvector-val-set! e1 e2 e3)]
      [(Munbox (app recur e))
       (Mbox-val-ref e)]
      [(Mbox-set! (app recur e1) (app recur e2))
       (Mbox-val-set! e1 e2)]
      [(Mvector-length e) (Mvector-length (recur e))]
      [(Mbox (app recur e) t) (Mbox e t)]
      [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]      
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
      [(and noop (No-Op)) noop]      
      [other
       (error 'cast/interp-casts-with-hyper-coercion "unmatched ~a" other)]))
  ;; Body of map-expr
  ;; just return the recursion procedure
  recur)


