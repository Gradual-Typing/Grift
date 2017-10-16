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

(require
 scribble/srcdoc
 racket/match
 racket/format
 racket/list
 racket/set
 (submod "../logging.rkt" typed)
 "../unique-identifiers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/syntax.rkt"
 "../language/cast0.rkt"
 "../language/cast-or-coerce3.rkt"
 "./interpret-casts-common.rkt")

(provide
 interpret-casts/coercions
 (all-from-out
  "../language/cast0.rkt"
  "../language/cast-or-coerce3.rkt"))


(: make-coercion
   (->* (Schml-Type Schml-Type Blame-Label)
        (#:top-level? Boolean)
        Schml-Coercion))
(define (make-coercion t1 t2 lbl
                       #:top-level? [top-level? #f])
  (: r : Schml-Type Schml-Type -> Schml-Coercion)
  (define (r t1 t2) (make-coercion t1 t2 lbl #:top-level? #f))
  (match* (t1 t2)
    [(t t) IDENTITY]
    [((Dyn) _) 
     (if (and top-level? (optimize-first-order-coercions?))
         (Project t2 lbl)
         (Sequence (Project t2 lbl) IDENTITY))]
    [(_ (Dyn))
     (if (and top-level? (optimize-first-order-coercions?))
         (Inject t1)
         (Sequence IDENTITY (Inject t1)))]
    [((Fn n t1* t1) (Fn n t2* t2)) (Fn n (map r t2* t1*) (r t1 t2))]
    [((STuple n t1*) (STuple n t2*)) (CTuple n (map r t1* t2*))]
    [((GRef t1)(GRef t2))    (Ref (r t1 t2) (r t2 t1))]
    [((GVect t1) (GVect t2)) (Ref (r t1 t2) (r t2 t1))]
    [((MRef t1) (MRef t2))   (MonoRef t2)]
    [((MVect t1) (MVect t2)) (MonoVect t2)]
    [(_ _) (Failed lbl)]))

(define monolithic-make-coercion? : (Parameterof Boolean)
  (make-parameter #t))
(define make-coercion-inline/both-vars? : (Parameterof Boolean)
  (make-parameter #f))

(define-type CoC3-Value (U (Quote Schml-Literal)
                           (Quote-Coercion Coercion)
                           (Type Schml-Type)
                           (Var Uid)))

(define (make-compile-make-coercion/make-med-coercion)
  : (Values Compile-Make-Coercion-Type Make-Med-Coercion-Type)
  (define make-coercion-uid (next-uid! "make-coercion"))
  (define make-med-coercion-uid (next-uid! "make-med-coercion"))
  (: interp-make-coercion Make-Coercion-Type)
  (define (interp-make-coercion t1 t2 lbl)
    (apply-code make-coercion-uid t1 t2 lbl))
  (: interp-make-med-coercion Make-Coercion-Type)
  (define (interp-make-med-coercion t1 t2 lbl)
    (apply-code make-med-coercion-uid t1 t2 lbl))
  ;; Invariant: t1 <> t2
  ;; The compile function takes care of this case
  (: code-gen-make-med-coercion Make-Coercion-Type)
  (define (code-gen-make-med-coercion t1 t2 lbl)
    (let$ ([t1 t1] [t2 t2] [lbl lbl])
      (ann
       (cond$ 
        [(and$ (type-fn?$ t1) (type-fn?$ t2))
         ;; This line is a little tricky because
         ;; unless we have actual types for this at
         ;; compile time we have to generate code
         ;; that can handle arbitry fn-arity.
         ;; We delegate this task to specify representation because
         ;; it involves safely allocating an object whos size cannot
         ;; be determined until run-time.
         ;; TODO insert primitives that would allow this functions creation here
         (ann (If (op=? (type-fn-arity$ t1) (type-fn-arity$ t2))
                  (Make-Fn-Coercion make-coercion-uid t1 t2 lbl)
                  (Blame lbl))
              CoC3-Expr)]
        [(and$ (type-tup?$ t1)
               (type-tup?$ t2)
               (op<=? (type-tup-arity$ t2) (type-tup-arity$ t1)))
         (ann (Make-Tuple-Coercion make-coercion-uid t1 t2 lbl)
              CoC3-Expr)]
        [(and$ (type-pvec?$ t1) (type-pvec?$ t2))
         (ann (let*$ ([pvof1 (type-pvec-of$ t1)]
                      [pvof2 (type-pvec-of$ t2)]
                      [read_crcn  (interp-make-coercion pvof1 pvof2 lbl)]
                      [write_crcn (interp-make-coercion pvof2 pvof1 lbl)])
                (Ref-Coercion read_crcn write_crcn))
              CoC3-Expr)]
        [(and$ (type-pbox?$ t1) (type-pbox?$ t2))
         (ann (let*$ ([pbof1 (type-pbox-of$ t1)]
                      [pbof2 (type-pbox-of$ t2)]
                      [read_crcn  (interp-make-coercion pbof1 pbof2 lbl)]
                      [write_crcn (interp-make-coercion pbof2 pbof1 lbl)])
                (Ref-Coercion read_crcn write_crcn))
              CoC3-Expr)]
        [(and$ (type-mvec?$ t1) (type-mvec?$ t2))
         (ann (let$ ([t (type-mvec-of$ t2)])
                (MVect-Coercion t))
              CoC3-Expr)]
        [(and$ (type-mbox?$ t1) (type-mbox?$ t2))
         (ann (let$ ([t (type-mbox-of$ t2)])
                (MRef-Coercion t))
              CoC3-Expr)]
        [else (ann (Failed-Coercion lbl) CoC3-Expr)])
       CoC3-Expr)))

  (: compile-make-med-coercion Make-Med-Coercion-Type)
  (define (compile-make-med-coercion t1 t2 lbl #:know-not-eq? [know-not-eq? #f])
    (: r : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    ;; The real inspection but it expects all expressions to be literals
    ;; or variables 
    (define (r t1 t2 lbl)
      (match* (t1 t2)
        [((Type t1-t) (Type t2-t))
         ;; TODO This code should either never run or be generating
         ;; coercion literals
         (match* (t1-t t2-t)
           [(t t) ID-EXPR]
           [((Fn n _ _) (Fn n _ _)) 
            (Make-Fn-Coercion make-coercion-uid t1 t2 lbl)]
           [((STuple n _) (STuple m _)) #:when (<= n m)
            ;; Todo this should allocate a new coercion without the
            ;; doing the map at runtime be we currently 
            (Make-Tuple-Coercion make-coercion-uid t1 t2 lbl)]
           [((GRef t1-t) (GRef t2-t))
            (define t1 (Type t1-t))
            (define t2 (Type t2-t))
            (Ref-Coercion (interp-make-coercion t1 t2 lbl)
                          (interp-make-coercion t2 t1 lbl))]
           [((GVect t1-t) (GVect t2-t))
            (define t1 (Type t1-t))
            (define t2 (Type t2-t))
            (Ref-Coercion (interp-make-coercion t1 t2 lbl)
                          (interp-make-coercion t2 t1 lbl))]
           [((MRef _) (MRef t2))
            (Quote-Coercion (MonoRef t2))]
           [((MVect _) (MVect t2))
            (Quote-Coercion (MonoVect t2))]
           [(_ _) (Failed-Coercion lbl)])]
        [((Type t1-t) t2)
         (match t1-t
           [(Fn n _ _)
            (If (and$ (type-fn?$ t2) (op=? (type-fn-arity$ t2) (Quote n)))
                (Make-Fn-Coercion make-coercion-uid t1 t2 lbl)
                (Failed-Coercion lbl))]
           [(STuple n _)
            (If (and$ (type-tup?$ t2) (op=? (type-tup-arity$ t2) (Quote n)))
                (Make-Tuple-Coercion make-coercion-uid t1 t2 lbl)
                (Failed-Coercion lbl))]
           [(GRef t1-t)
            (define t1 (Type t1-t))
            (If (type-pbox?$ t2)
                (let$ ([t2 (type-pbox-of$ t2)])
                  (Ref-Coercion (interp-make-coercion t1 t2 lbl)
                                (interp-make-coercion t2 t1 lbl)))
                (Failed-Coercion lbl))]
           [(GVect t1-t)
            (define t1 (Type t1-t))
            (If (type-pvec?$ t2)
                (let$ ([t2 (type-pvec-of$ t2)])
                  (Ref-Coercion (interp-make-coercion t1 t2 lbl)
                                (interp-make-coercion t2 t1 lbl)))
                (Failed-Coercion lbl))]
           [(MRef _)
            (If (type-mbox?$ t2)
                (MRef-Coercion (type-mbox-of$ t2))
                (Failed-Coercion lbl))]
           [(MVect _)
            (If (type-mvec?$ t2)
                (MVect-Coercion (type-mvec-of$ t2))
                (Failed-Coercion lbl))]
           [_ (Failed-Coercion lbl)])]
        [(t1 (Type t2-t))
         (match t2-t
           [(Fn n _ _)
            (If (and$ (type-fn?$ t1) (op=? (type-fn-arity$ t1) (Quote n)))
                (Make-Fn-Coercion make-coercion-uid t1 t2 lbl)
                (Failed-Coercion lbl))]
           [(STuple n _)
            (If (and$ (type-tup?$ t1) (op=? (type-tup-arity$ t1) (Quote n)))
                (Make-Tuple-Coercion make-coercion-uid t1 t2 lbl)
                (Failed-Coercion lbl))]
           [(GRef t2-t)
            (define t2 (Type t2-t))
            (If (type-pbox?$ t1)
                (let$ ([t1 (type-pbox-of$ t1)])
                  (Ref-Coercion (interp-make-coercion t1 t2 lbl)
                                (interp-make-coercion t2 t1 lbl)))
                (Failed-Coercion lbl))]
           [(GVect t2-t)
            (define t2 (Type t2-t))
            (If (type-pvec?$ t1)
                (let$ ([t1 (type-pvec-of$ t1)])
                  (Ref-Coercion (interp-make-coercion t1 t2 lbl)
                                (interp-make-coercion t2 t1 lbl)))
                (Failed-Coercion lbl))]
           [(MRef t2-t)
            (If (type-mbox?$ t1)
                (MRef-Coercion (Type t2-t))
                (Failed-Coercion lbl))]
           [(MVect t2-t)
            (If (type-mvec?$ t1)
                (MVect-Coercion (Type t2-t))
                (Failed-Coercion lbl))]
           [_ (Failed-Coercion lbl)])]
        [(t1 t2) (interp-make-med-coercion t1 t2 lbl)]))
    ;; Bind non-values and decide to be eq? check 
    (bind-value$
     ([t1 t1] [t2 t2] [lbl lbl])
     (match* (t1 t2)
       [(t t) ID-EXPR]
       [((Type t1) (Type t2)) #:when (Quote? lbl)
        (match-define (Quote lit) lbl)
        (unless (string? lit)
          (error 'compile-make-med-coercion "expected a string got: ~v" lit))
        (Quote-Coercion (make-coercion t1 t2 lit #:top-level? #f))]
       [((Type _) (Type _)) (r t1 t2 lbl)]
       [(_ _) #:when know-not-eq? (r t1 t2 lbl)]
       [(_ _) (If (op=? t1 t2) ID-EXPR (r t1 t2 lbl))])
     ))
  
  (add-cast-runtime-binding!
   make-med-coercion-uid
   (code$ (t1 t2 lbl)
     (code-gen-make-med-coercion t1 t2 lbl)))
  
  (add-cast-runtime-binding!
   make-coercion-uid
   (code$ (t1 t2 lbl)
     (cond$
      [(op=? t1 t2) (Quote-Coercion (Identity))]
      ;; This is absolutly necisarry
      ;; While Injections and Projections are never made by
      ;; source code coercions composition can create new
      ;; projections and injections
      [(type-dyn?$ t1)
       (Sequence-Coercion (Project-Coercion t2 lbl)
                          (Quote-Coercion (Identity)))]
      [(type-dyn?$ t2)
       (Sequence-Coercion (Quote-Coercion (Identity))
                          (Inject-Coercion t1))]
      [else
       (cond
         [(monolithic-make-coercion?) (code-gen-make-med-coercion t1 t2 lbl)]
         [else (interp-make-med-coercion t1 t2 lbl)])])))
  
  ;; This is only applied to source level make-coercions at the top level
  ;; ie there are know recursive calls to compile-make-coercion therefore
  ;; Projects and Injects returned from compile make-coercion do not
  ;; have to be in space-efficient normal-form.
  (: compile-make-coercion Compile-Make-Coercion-Type)
  (define (compile-make-coercion t1 t2 lbl
                                 #:top-level? [top-level? #t]
                                 #:know-not-eq? [know-not-eq? #f])
    (: r : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    ;; r expects t1 t2 lbl to be either literals or vars
    (define (r t1 t2 lbl)
      (match* (t1 t2)
        [(t t) (Quote-Coercion (Identity))]
        [((Type (Dyn)) _)
         (let ([prj (Project-Coercion t2 lbl)])
           (if (and top-level? (optimize-first-order-coercions?))
               prj
               (Sequence-Coercion prj ID-EXPR)))]
        [(_ (Type (Dyn)))
         (let ([inj (Inject-Coercion t1)])
           (if (and top-level? (optimize-first-order-coercions?))
               inj
               (Sequence-Coercion ID-EXPR inj)))]
        ;; From here on any use of the type pattern is used to rule out
        ;; that variable from being dynamic
        [((Type _) (Type _))
         (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)]
        [((Var _) (Type _))
         (cond$
          [(type-dyn?$ t1) (r DYN-EXPR t2 lbl)]
          [else (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)])]
        [((Type _) (Var _))
         (cond$
          [(type-dyn?$ t2) (r t1 DYN-EXPR lbl)]
          [else (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)])]
        [((Var _) (Var _))
         (cond
           [(make-coercion-inline/both-vars?) 
            (cond$
             [(type-dyn?$ t1) (r DYN-EXPR t2 lbl)]
             [(type-dyn?$ t2) (r t1 DYN-EXPR lbl)]
             ;; There is no more information to specialize on
             [else (interp-make-med-coercion t1 t2 lbl)])]
           [else (interp-make-coercion t1 t2 lbl)])]))
    ;; bind all non-literals and decide to check for eq?
    (define ret
      (bind-value$
       ([t1 t1] [t2 t2] [lbl lbl])
       (match* (t1 t2)
         [(t t) ID-EXPR]
         [((Type t1) (Type t2)) #:when (Quote? lbl)
          (match-define (Quote lit) lbl)
          (unless (string? lit)
            (error 'compile-make-med-coercion "expected a string got: ~v" lit))
          (Quote-Coercion (make-coercion t1 t2 lit #:top-level? top-level?))]
         [((Type _) (Type _)) ;; lbl is an expression
          ;; Two types that are not equal do not need an eq check
          (r t1 t2 lbl)]
         [((Var _) (Var _)) (interp-make-coercion t1 t2 lbl)]
         [(_ _) #:when know-not-eq? (r t1 t2 lbl)]
         [(_ _) (If (op=? t1 t2) ID-EXPR (r t1 t2 lbl))])))
    ret)
  (values compile-make-coercion compile-make-med-coercion))

(: make-apply-coercion-runtime!
   (->* (#:apply-coercion-uid Uid
         #:project Project-Type
         #:inject  Inject-Type
         #:apply-fn-coercion  Apply-Coercion-Type
         #:apply-tup-coercion Apply-Coercion-Type
         #:apply-ref-coercion Apply-Coercion-Type
         #:mbox-cast Monotonic-Cast-Type
         #:mvec-cast Monotonic-Cast-Type)
        Void))
(define (make-apply-coercion-runtime!
         #:apply-coercion-uid apply-coercion-uid
         #:project project
         #:inject  inject
         #:apply-fn-coercion apply-fn-coercion
         #:apply-tup-coercion apply-tup-coercion
         #:apply-ref-coercion apply-ref-coercion
         #:mbox-cast mbox-cast
         #:mvec-cast mvec-cast)

  (add-cast-runtime-binding!
   apply-coercion-uid
   (code$ (v c mt)
     (cond$
      [(Id-Coercion-Huh c) v]
      [(Sequence-Coercion-Huh c)
       (let*$ ([seq-fst (Sequence-Coercion-Fst c)]
               [seq-snd (Sequence-Coercion-Snd c)])
         ;; This is a racket conditional
         (cond
           ;; TODO make sure the original code works then move to this
           #;
           [(coercions-are-space-efficient?)
            (precondition (or$ (Project-Coercion-Huh seq-fst)
                               (Inject-Coercion-Huh seq-snd)))
            (cond$
             [(Project-Coercion-Huh seq-fst)
              (let$ ([v (project v
                                 (Project-Coercion-Type seq-fst)
                                 (Project-Coercion-Label seq-fst) mt)])
                (cond$
                 [(Id-Coercion-Huh seq-snd) v]
                 [else (apply-code apply-coercion-uid v seq-snd mt)]))]
             [else ;; Must be inject coercion
              (let$ ([v (cond$
                         [(Id-Coercion-Huh seq-fst) v]
                         [else (apply-code apply-coercion-uid v seq-fst mt)])])
                (inject v (Inject-Coercion-Type c)))])]
           [else
            (let$ ([v (apply-code apply-coercion-uid v seq-fst mt)])
              (apply-code apply-coercion-uid v seq-snd mt))]))]
      [(Project-Coercion-Huh c)
       (project v (Project-Coercion-Type c) (Project-Coercion-Label c) mt)]
      [(Inject-Coercion-Huh c)
       (inject v (Inject-Coercion-Type c))]
      [(Mediating-Coercion-Huh c)
       (cond$
        [(Fn-Coercion-Huh c) (apply-fn-coercion v c)]
        [(Tuple-Coercion-Huh c) (apply-tup-coercion v c mt)]
        [(Ref-Coercion-Huh c) (apply-ref-coercion v c)] 
        [(MRef-Coercion-Huh c) (mbox-cast v (MRef-Coercion-Type c))]
        [(MVect-Coercion-Huh c) (mvec-cast v (MRef-Coercion-Type c))]
        [else (Blame (Quote "bad implemention of mediating coercions"))])]
      ;; the coercion must be failure
      [else
       (precondition$ (Failed-Coercion-Huh c)
         (Blame (Failed-Coercion-Label c)))]))))

(: make-compile-apply-coercion
   (->* (#:apply-coercion-uid Uid
         #:apply-coercion Apply-Coercion-Type
         #:project Project-Type
         #:inject  Inject-Type
         #:apply-fn-coercion  Apply-Coercion-Type
         #:apply-tup-coercion Apply-Coercion-Type
         #:apply-ref-coercion Apply-Coercion-Type
         #:mbox-cast Monotonic-Cast-Type
         #:mvec-cast Monotonic-Cast-Type)
        Apply-Coercion-Type))

(define (make-compile-apply-coercion
         #:apply-coercion-uid apply-coercion-uid
         #:apply-coercion apply-coercion
         #:project project
         #:inject  inject
         #:apply-fn-coercion apply-fn-coercion
         #:apply-tup-coercion apply-tup-coercion
         #:apply-ref-coercion apply-ref-coercion
         #:mbox-cast mbox-cast
         #:mvec-cast mvec-cast)
  
  (: compile-apply-coercion Apply-Coercion-Type)
  (define (compile-apply-coercion v c [mt ZERO-EXPR])
    (match c
      [(Quote-Coercion (Identity)) v]
      [(Quote-Coercion (Sequence c1 c2))
       (let$ ([v (compile-apply-coercion v (Quote-Coercion c1))])
         (compile-apply-coercion v (Quote-Coercion c2)))]
      [(Quote-Coercion (Project t2 l)) (project v (Type t2) (Quote l) mt)]
      [(Quote-Coercion (Inject t1))    (inject v (Type t1))]
      [(Quote-Coercion (Fn _ _ _))     (apply-fn-coercion v c)]
      [(Quote-Coercion (CTuple _ _))   (apply-tup-coercion v c mt)]
      [(Quote-Coercion (Ref _ _))      (apply-ref-coercion v c)]
      [(Quote-Coercion (MonoRef t2))   (mbox-cast v (Type t2))]
      [(Quote-Coercion (MonoVect t2))  (mvec-cast v (Type t2))]
      [(Quote-Coercion (Failed l))     (Blame (Quote l))]
      [non-literal-coercion
       (apply-coercion v c)]))
  compile-apply-coercion)


(: make-compose-coercions
   (->* (#:make-coercion Make-Coercion-Type
         #:greatest-lower-bound (Code-Label Uid))
        (Values Uid Compose-Coercions-Type)))
(define (make-compose-coercions #:make-coercion make-coercion
                                #:greatest-lower-bound greatest-lower-bound)
  (define compose-coercions-uid (next-uid! "compose-coercions"))
  (: compose-coercions Compose-Coercions-Type)
  (define (compose-coercions c1 c2)
    (apply-code compose-coercions-uid c1 c2))
  (define compose-fn-coercions
    (make-compose-fn-coercions #:id-coercion? Id-Coercion-Huh
                               #:compose-coercions compose-coercions))
  (define compose-tup-coercions
    (make-compose-tup-coercions #:id-coercion? Id-Coercion-Huh
                                #:compose-coercions compose-coercions))
  (add-cast-runtime-binding!
   compose-coercions-uid
   (code$ (c1 c2)
     (cond$
      ;; Eliminating the Identities cuts down on the number of branches
      ;; and should be fast
      [(id-coercion?$ c1) c2]
      [(id-coercion?$ c2) c1]
      ;; We could elminate failure on the left next, but we choose to make
      ;; success as fast as possible even if it introduces more branches overall.
      [(seq-coercion?$ c1)
       (let*$ ([seq_fst (seq-coercion-fst$ c1)]
               [seq_snd (seq-coercion-snd$ c1)])
         (cond$
          [(prj-coercion?$ seq_fst)
           (let*$ ([comp_seq_snd (compose-coercions seq_snd c2)])
             (seq-coercion$ seq_fst comp_seq_snd))]
          ;; We have to priority failure on the right over injection
          [(failed-coercion?$ c2) c2]
          ;; Because of the typeing rule for coercions we know that
          ;; c2 must be a (I?;i) aka projection sequence because
          ;; it must have the type dyn => I.
          [else ;;  (inj?$ seq_snd) only thing that is left in normal form
           (let*$ ([proj  (seq-coercion-fst$ c2)]
                   [final (seq-coercion-snd$ c2)]
                   [t1    (inj-coercion-type$ seq_snd)]
                   [t2    (prj-coercion-type$ proj)]
                   [lbl   (prj-coercion-label$ proj)]
                   [comp_prj_inj (make-coercion t1 t2 lbl)]
                   [comp_fst_pi (compose-coercions seq_fst comp_prj_inj)])
             (compose-coercions comp_fst_pi final))]))]
      [(seq-coercion?$ c2)
       (cond$
        [(failed-coercion?$ c1) c1]
        [else
         ;; must be c1 & (g;I?)
         (let*$ ([seq_fst (seq-coercion-fst$ c2)]
                 [seq_snd (seq-coercion-snd$ c2)]
                 [comp_c1_final (compose-coercions c1 seq_fst)])
           (seq-coercion$ comp_c1_final seq_snd))])]
      ;; All Branches from here out will have to check if c2 is failure
      ;; so we do it once to eliminate the possibility
      [(failed-coercion?$ c2)
       (If (failed-coercion?$ c1)
           c1
           c2)]
      [(med-coercion?$ c1)
       (cond$
        [(fn-coercion?$ c1) ;; c2 must be a Function Coercion
         ;; TODO this could be implemented in code here
         (compose-fn-coercions c1 c2)]
        [(ref-coercion?$ c1) ;; c2 must also be a reference coercion
         (let*$ ([ref1_read  (ref-coercion-read$  c1)]
                 [ref1_write (ref-coercion-write$ c1)]
                 [ref2_read  (ref-coercion-read$  c2)]
                 [ref2_write (ref-coercion-write$ c2)]
                 [read  (compose-coercions ref1_read  ref2_read)]
                 [write (compose-coercions ref2_write ref1_write)])
           (If (and$ (id-coercion?$ read)
                     (id-coercion?$ write))
               ID-EXPR
               (ref-coercion$ read write)))]
        [(tup-coercion?$ c1)
         (compose-tup-coercions c1 c2)]
        [(mbox-coercion?$ c1)
         (let*$ ([ref1_type  (mbox-coercion-type$ c1)]
                 [ref2_type  (mbox-coercion-type$ c2)]
                 [type3  (app-code$ greatest-lower-bound ref1_type ref2_type)])
           (mbox-coercion$ type3))]
        [(mvec-coercion?$ c1)
         (let*$ ([ref1_type  (mvec-coercion-type$ c1)]
                 [ref2_type  (mvec-coercion-type$ c2)]
                 [type3  (app-code$ greatest-lower-bound ref1_type ref2_type)])
           (mvec-coercion$ type3))]
        [else (Blame (Quote "bad implemention of mediating coercions"))])]
      ;; C1 must be a failed coercion
      [else (Blame (failed-coercion-label$ c1))])))
  ;; For now we are not even trying to be good at compiling coercion composition
  (values compose-coercions-uid compose-coercions))

(: interpret-casts/coercions : -> (C0-Expr -> CoC3-Expr))
(define (interpret-casts/coercions)
  (define greatest-lower-bound (make-compile-types-greatest-lower-bound))
  (define-values (compile-make-coercion compile-make-med-coercion)
    (make-compile-make-coercion/make-med-coercion))
  (define-values (compose-coercions-uid compose-coercions)
    (make-compose-coercions #:make-coercion compile-make-coercion
                            #:greatest-lower-bound greatest-lower-bound))
  (define apply-coercion-uid (next-uid! "apply-coercion"))
  (define (apply-coercion [v : CoC3-Expr] [c : CoC3-Expr]
                          [mt : CoC3-Expr ZERO-EXPR])
    : CoC3-Expr
    (apply-code apply-coercion-uid v c mt))

  (define get-fn-cast!
    (make-fn-cast-helpers
     (make-build-caster/coercions
      #:apply-coercion-uid apply-coercion-uid
      #:compose-coercions compose-coercions
      #:id-coercion-huh   Id-Coercion-Huh)))
  
  (: compile-lambda Lambda-Type)
  (define (compile-lambda fml* e)
    (define ctr (get-fn-cast! (length fml*)))
    (Lambda fml* (Castable ctr e)))
  
  (: compile-app App-Type)
  (define (compile-app e e*)
    (App-Fn-or-Proxy apply-coercion-uid e e*))
  
  (define compile-fn-cast/coercions
    (make-compile-fn-cast/coercions
     #:get-fn-cast! get-fn-cast!
     #:make-fn-coercion compile-make-coercion))

  (define compile-types-greatest-lower-bound
    (make-compile-types-greatest-lower-bound))
  
  (define compile-apply-fn-coercion
    (make-compile-apply-fn-coercion #:get-fn-cast! get-fn-cast!))
  
  (define compile-apply-tup-coercion
    (make-compile-apply-tuple-coercion
     #:apply-coercion-uid apply-coercion-uid))
  
  (define compile-apply-ref-coercion
    (make-compile-apply-ref-coercion
     #:compose-coercions compose-coercions
     #:id-coercion? Id-Coercion-Huh))
  
  (define compile-inject (make-compile-inject))

  ;; Interp-Cast Builds a call to a runtime function
  ;; that casts based on types.
  ;; Compile cast specializes based on types
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
          #:id-coercion? Id-Coercion-Huh))

       (define compile-mbox-cast/type-based
         (make-compile-mbox-cast
          #:interp-cast interp-cast
          #:greatest-lower-bound compile-types-greatest-lower-bound))
       
       (define compile-mvec-cast/type-based
         (make-compile-mvec-cast
          #:interp-cast interp-cast
          #:greatest-lower-bound compile-types-greatest-lower-bound))

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

       (make-apply-coercion-runtime!
        #:apply-coercion-uid apply-coercion-uid
        #:project compile-project/type-based
        #:inject  compile-inject
        #:apply-fn-coercion compile-apply-fn-coercion
        #:apply-tup-coercion compile-apply-tup-coercion
        #:apply-ref-coercion compile-apply-ref-coercion
        #:mbox-cast compile-mbox-cast/type-based
        #:mvec-cast compile-mvec-cast/type-based)
       
       (define compile-cast
         (make-compile-cast
          #:interp-cast interp-cast
          #:project compile-project/type-based
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast))
       
       (values interp-cast compile-cast)]
      [else
       (define interp-cast-uid (next-uid! "interp-cast"))
       
       (: interp-cast/coercions Cast-Type)
       (define (interp-cast/coercions v t1 t2 l [mt : CoC3-Expr ZERO-EXPR]) 
         (apply-coercion v (compile-make-coercion t1 t2 l #:top-level? #t) mt))

       (: interp-med-cast/coercions Cast-Type)
       (define (interp-med-cast/coercions v t1 t2 l [mt : CoC3-Expr ZERO-EXPR])
         (apply-coercion v (compile-make-med-coercion t1 t2 l) mt))
       
       ;; This first section builds the cast interpreter that falls
       ;; back to make-coercion when a higher-order cast is applied
       (define compile-tuple-cast/coercions
         (make-compile-cast-tuple/coercions
          #:apply-coercion-uid apply-coercion-uid
          #:make-med-coercion compile-make-med-coercion))
       
       (define compile-pref-cast/coercions
         (make-compile-cast-pref/coercions
          #:make-coercion compile-make-coercion
          #:compose-coercions compose-coercions
          #:id-coercion? Id-Coercion-Huh))

       (define compile-mbox-cast/coercions
         (make-compile-mbox-cast
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound compile-types-greatest-lower-bound))
       
       (define compile-mvec-cast/coercions
         (make-compile-mvec-cast
          #:interp-cast interp-cast/coercions
          #:greatest-lower-bound compile-types-greatest-lower-bound))
       
       (define compile-med-cast/coercions
         (make-compile-med-cast
          #:fn-cast    compile-fn-cast/coercions
          #:tuple-cast compile-tuple-cast/coercions
          #:ref-cast   compile-pref-cast/coercions
          #:mbox-cast  compile-mbox-cast/coercions
          #:mvec-cast  compile-mvec-cast/coercions
          #:interp-med-cast interp-med-cast/coercions))

       (define compile-project/coercions
         (make-compile-project #:compile-med-cast compile-med-cast/coercions))

       (make-apply-coercion-runtime!
        #:apply-coercion-uid apply-coercion-uid
        #:project compile-project/coercions
        #:inject  compile-inject
        #:apply-fn-coercion compile-apply-fn-coercion
        #:apply-tup-coercion compile-apply-tup-coercion
        #:apply-ref-coercion compile-apply-ref-coercion
        #:mbox-cast compile-mbox-cast/coercions
        #:mvec-cast compile-mvec-cast/coercions)
       
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
