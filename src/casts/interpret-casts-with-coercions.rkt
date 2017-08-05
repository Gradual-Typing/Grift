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
 (submod "../logging.rkt" typed)
 "../unique-identifiers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/syntax.rkt"
 "../language/cast0.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/data-representation.rkt"
 "./interpret-casts-common.rkt")

(provide
 interpret-casts/coercions
 (all-from-out
  "../language/cast0.rkt"
  "../language/cast-or-coerce3.rkt"))


#|
cast-interp
project  (recur)
inject   (N/A)
Fun       (make-coercion)
PRef/PVec (make-coercion)
MRef      (interp-cast)
MVec      (either)
Tup       (either)
|#

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

  #;
  (add-cast-runtime-binding!
   apply-coercion-uid
   (code$ (v c mt)
     (cond$
      [(Id-Coercion-Huh c) v]
      [(Sequence-Coercion-Huh c)
       (let*$ ([seq_fst (Sequence-Coercion-Fst c)]
               [seq_snd (Sequence-Coercion-Snd c)]
               [v (apply-coercion v seq_fst mt)])
         (apply-coercion v seq_snd mt))]
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
      [else (precondition$ (Failed-Coercion-Huh c)
              (Blame (Failed-Coercion-Label c)))])))
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

#;
(define (make-compile-cast/coercions)
  (define (compile-cast t1 t2 lbl
                        #:know-not-eq? [know-not-eq? #f])
    (: r : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    ;; r expects t1 t2 lbl to be either literals or vars
    (define (r v t1 t2 lbl)
      (match* (t1 t2)
        [(t t) v]
        [((Type (Dyn)) _) (project v t2 lbl)]
        [(_ (Type (Dyn))) (inject v t1)]
        ;; From here on any use of the type pattern is used to rule out
        ;; that variable from being dynamic
        [((Type _) (Type _)) (compile-med-cast v t1 t2 lbl #:know-not-eq? #t)]
        [((Var _) (Type _))
         (cond$
          [(type-dyn?$ t1) (r v DYN-EXPR t2 lbl)]
          [else (compile-med-cast v t1 t2 lbl #:know-not-eq? #t)])]
        [((Type _) (Var _))
         (cond$
          [(type-dyn?$ t2) (r v t1 DYN-EXPR lbl)]
          [else (compile-make-med-coercion t1 t2 lbl #:know-not-eq? #t)])]
        [((Var _) (Var _)) (interp-cast t1 t2 lbl)
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
    ret))


(: make-compose-coercions
   (->* (#:make-coercion Make-Coercion-Type
         #:greatest-lower-bound Greatest-Lower-Bound-Type)
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
                 [type3  (greatest-lower-bound ref1_type ref2_type)])
           (mbox-coercion$ type3))]
        [(mvec-coercion?$ c1)
         (let*$ ([ref1_type  (mvec-coercion-type$ c1)]
                 [ref2_type  (mvec-coercion-type$ c2)]
                 [type3  (greatest-lower-bound ref1_type  ref2_type)])
           (mvec-coercion$ type3))]
        [else (Blame (Quote "bad implemention of mediating coercions"))])]
      ;; C1 must be a failed coercion
      [else (Blame (failed-coercion-label$ c1))])))
  ;; For now we are not even trying to be good at compiling coercion composition
  (values compose-coercions-uid compose-coercions))

(: interpret-casts/coercions : -> (C0-Expr -> CoC3-Expr))
(define (interpret-casts/coercions)
  
  (define greatest-lower-bound (make-compile-types-greatest-lower-bound))
  ;; Interp-Cast Builds a call to a runtime function
  ;; that casts based on types.
  ;; This define is here because the body returns either
  ;; an interpretation of casts that uses coercion only
  ;; or a hybrid of type-based cast and coercions
  ;; TODO can we abstract compile-{app, lambda}
  (define-values (interp-cast apply-coercion compile-cast compile-lambda compile-app)
    (let ()
      ;; This code is invarient to hybrid runtime
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
         #;
         (define compile-cast/type-based : Compile-Cast-Type
           (make-compile-cast
            #:interp-cast interp-cast
            #:project compile-project
            #:inject  compile-inject
            #:compile-med-cast compile-med-cast))
         
         ;; This second part builds the coercion runtime which
         ;; falls back on to the cast interpreter instead of calling
         ;; make-coercion then applying the coercion
         
         #;
         (define compile-apply-coercion : Apply-Coercion-Type
           (make-compile-apply-coercion
            #:apply-coercion-uid apply-coercion-uid
            #:apply-coercion apply-coercion
            #:project compile-project
            #:inject  compile-inject
            #:apply-fn-coercion  compile-apply-fn-coercion
            #:apply-tup-coercion compile-apply-tup-coercion
            #:apply-ref-coercion compile-apply-ref-coercion
            #:mbox-cast compile-mbox-cast
            #:mvec-cast compile-mvec-cast))
         
         #;(: compile-cast/coercions Compile-Cast-Type)
         #;
         (define (compile-cast/coercions v t1 t2 l
                                         [mt ZERO-EXPR]
                                         #:t1-not-dyn [t1-not-dyn? #f]
                                         #:t2-not-dyn [t2-not-dyn? #f])
           (match* (t1 t2)
             ;; I think there is still room for improvement here
             [((MRef t1) (MRef t2))
              (compile-mbox-cast v #:t1 (Type t1) (Type t2) l)]
             [((MVect t1) (MVect t2))
              (compile-mvec-cast v #:t1 (Type t1) (Type t2) l)]
             [(other wise)
              (define c (compile-make-coercion t1 t2 l #:top-level? #t))
              (define e (compile-apply-coercion v c))
              (debug off c e)]))

         (define compile-cast
           (make-compile-cast
            #:interp-cast interp-cast
            #:project compile-project/type-based
            #:inject  compile-inject
            #:compile-med-cast compile-med-cast))
         
         (values interp-cast apply-coercion compile-cast
                 ;;compile-cast/type-based ;; I think this will generate better code 
                 #;compile-cast/coercions
                 compile-lambda compile-app)]
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

         #;
         (make-interp-cast-runtime!
          #:interp-cast-uid interp-cast-uid 
          #:project compile-project/coercions
          #:inject  compile-inject
          #:compile-med-cast compile-med-cast)

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
         
         (values interp-cast/coercions
                 apply-coercion
                 compile-cast
                 ;;compile-cast/type-based ;; I think this will generate better code 
                 #;compile-cast/coercions
                 compile-lambda compile-app)
         
         ;; ;; Otherwise interp-cast is a call to make-coercion
         ;; ;; Then applying that coercion
         ;; (: interp-cast/coercion Cast-Type)
         ;; (define (interp-cast [v : CoC3-Expr] [t1 : CoC3-Expr]
         ;;                      [t2 : CoC3-Expr] [l : CoC3-Expr]
         ;;                      [mt : CoC3-Expr ZERO-EXPR])
         ;;   ;; Should this be smarter?
         ;;   (apply-coercion v (compile-make-coercion t1 t2 l #:top-level? #f) mt))

         ;; ;; This need to be merged with the type-based-compile-med-cast
         ;; (: interp-med-cast Compile-Med-Cast-Type)
         ;; (define (interp-med-cast [v : CoC3-Expr] [t1 : CoC3-Expr]
         ;;                          [t2 : CoC3-Expr] [l : CoC3-Expr]
         ;;                          [mt : CoC3-Expr ZERO-EXPR]
         ;;                          #:know-not-eq? [know-not-eq? : Boolean #f])
         ;;   (apply-coercion v (compile-make-med-coercion t1 t2 l #:know-not-eq? know-not-eq?)))
         
         ;; ;; TODO Consider moving this to common and instntiating mbox and mvec at the same time
         
         ;; (define compile-mbox-cast
         ;;   (make-compile-mbox-cast
         ;;    #:interp-cast interp-cast
         ;;    #:greatest-lower-bound compile-types-greatest-lower-bound))
         
         ;; (define compile-mvec-cast
         ;;   (make-compile-mvec-cast
         ;;    #:interp-cast interp-cast
         ;;    #:greatest-lower-bound compile-types-greatest-lower-bound))

         ;; (define compile-project
         ;;   (make-compile-project #:compile-med-cast interp-med-cast))
         
         ;; (define compile-apply-coercion : Apply-Coercion-Type
         ;;   (make-compile-apply-coercion
         ;;    #:apply-coercion-uid apply-coercion-uid
         ;;    #:apply-coercion apply-coercion
         ;;    #:project compile-project
         ;;    #:inject  compile-inject
         ;;    #:apply-fn-coercion  compile-apply-fn-coercion
         ;;    #:apply-tup-coercion compile-apply-tup-coercion
         ;;    #:apply-ref-coercion compile-apply-ref-coercion
         ;;    #:mbox-cast compile-mbox-cast
         ;;    #:mvec-cast compile-mvec-cast))
         
         ;; (: compile-cast/coercions Compile-Cast-Type)
         ;; (define (compile-cast/coercions
         ;;          v t1 t2 l
         ;;          [mt ZERO-EXPR]
         ;;          #:t1-not-dyn [t1-not-dyn? #f]
         ;;          #:t2-not-dyn [t2-not-dyn? #f])
         ;;   (error 'todo)
         ;;   #;
         ;;   (bind-value$
         ;;    ([v v] [t1 t1] [t2 t2] [l l] [mt mt])
         ;;    (match* (t1 t2)
         ;;      [((Type t) (Type t)) v]
         ;;      ;; We don't have to check not Dyn here because t1 <> t2
         ;;      [((Type (Dyn)) (Type _))
         ;;       (project v t2 l)]
         ;;      [((Type (Dyn)) t2)
         ;;       (cond
         ;;         [t2-not-dyn (project v t2 l)]
         ;;         [else
         ;;          (If (Type-Dyn-Huh t2)
         ;;              v
         ;;              (project v t2 l))])]
         ;;      ;; We don't have to check not dyn here because t1 <> t2
         ;;      [((Type _) (Type (Dyn))) (inject v t1)]
         ;;      [(t1 (Type (Dyn)))
         ;;       (cond
         ;;         [t1-not-dyn (inject v t1)]
         ;;         [else
         ;;          (If (Type-Dyn-Huh t1)
         ;;              v
         ;;              (inject v t1))])]
         ;;      [((Type _) _) 
         ;;       (define hc : CoC3-Expr (compile-make-coercion t1 t2 l))
         ;;       (debug 'compile-cast/hyper-coercions hc)
         ;;       (match hc
         ;;         [hc #:when (not (optimize-first-order-coercions?))
         ;;             (interp-cast v hc)]
         ;;         ;; The next cases should never occur because the previous
         ;;         ;; code handles them without a call to make-coercion.
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ (Identity)))
         ;;          e]
         ;;         ;; Hyper-Coercion is Projection 
         ;;         [(Quote-HCoercion (HC #t t1 (? blame-label? l) #f _ (Identity)))
         ;;          (project v (Type t1) (Quote l))] 
         ;;         ;; Hyper-Coercion is Injection
         ;;         [(Quote-HCoercion (HC #f _ #f #t t2 (Identity)))
         ;;          (inject v (Type t2))]
         ;;         ;; Hyper-Coercion is a Funtion Coercion
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ (and m (Fn a _ _))))
         ;;          #:when (direct-fn-cast-optimization?)
         ;;          ;; Fn-casts use specialize code for their arity
         ;;          ;; Since we are generating it here it is trivial to
         ;;          ;; directly call that code.
         ;;          (let ([caster (get-fn-cast! a)])
         ;;            (App-Code (Code-Label caster) (list v (Quote-HCoercion m))))]
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ (and m (Ref c1 c2))))
         ;;          (compile-ref-cast v (Quote-HCoercion m))]
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ (and m (CTuple n c*))))
         ;;          (compile-tup-cast v (Quote-HCoercion m))]
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ (MonoRef t2)))
         ;;          (compile-mref-cast v (Type t2))]
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ (MonoVect t2)))
         ;;          (compile-mvec-cast v (Type t2))]
         ;;         [(Quote-HCoercion (HC #f _ #f #f _ m))
         ;;          (interp-med-cast v (Quote-HCoercion m))]
         ;;         [hc (interp-cast v hc)])]))
         ;;   (match* (t1 t2)
         ;;     ;; I think there is still room for improvement here
         ;;     [((MRef t1) (MRef t2))
         ;;      (compile-mbox-cast v #:t1 (Type t1) (Type t2) l)]
         ;;     [((MVect t1) (MVect t2))
         ;;      (compile-mvec-cast v #:t1 (Type t1) (Type t2) l)]
         ;;     [(other wise)
         ;;      (define c (compile-make-coercion t1 t2 l #:top-level? #t))
         ;;      (define e (compile-apply-coercion v c))
         ;;      (debug off c e)]))

         ;; (values interp-cast apply-coercion
         ;;         compile-cast/coercions
         ;;         compile-lambda compile-app)
         ])))



  
  (: pbox-ref  PBox-Ref-Type)
  (: pbox-set! PBox-Set-Type)
  (: pvec-ref  PVec-Ref-Type)
  (: pvec-set! PVec-Set-Type)
  (: pvec-len  PVec-Len-Type)
  (define-values (pbox-ref pbox-set!
                  pvec-ref pvec-set! pvec-len)
    (make-proxied-reference/coercions-code-gen-helpers
     #:apply-coercion apply-coercion))
  
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


;; TODO move to where they belong

;; (define-type Make-Coercion-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
;; (define-type hybrid-cast/coercion-runtime-Type
;;   (Parameterof (Setof (U 'project 'mref 'mvec))))
;; (define hybrid-cast/coercion-runtime
;;   (make-parameter (list->set '(project mref mvec))))

;; (define cast-runtime-code-bindings : (Parameterof (Option CoC3-Bnd-Code*))
;;   (make-parameter #f))

;; (define (add-cast-runtime-binding! uid code)
;;   (define prev (cast-runtime-code-bindings))
;;   (unless prev
;;     (error 'add-cast-runtime-binding! "used outside the dynamic scope of interprete-cast"))
;;   (cast-runtime-code-bindings (cons (cons uid code) prev)))

;; (define (interpret-casts prgm)
;;     (match-define (Prog (list name next type) e) prgm)
;;   ;; All the implicit state of this program
;;   (define next-unique (make-unique-counter next))
  
;;   (parameterize ([cast-runtime-code-bindings '()]
;;                  [current-unique-counter next-unique])
;;     (define ic-expr!
;;       (case (cast-representation) 
;;         [(|Type-Based Casts|) (interpret-casts/type-based-casts!)]
;;         [(Coercions) (interpret-casts/coercions!)]
;;         [(Hyper-Coercions) (interpret-casts/hyper-coercions!)]
;;         [(Static) (error 'todo)]))

;;     (define new-e (ic-expr! e))
    
;;     ;; Reconstruct the entire program
;;     (Prog (list name (unique-counter-next! next-unique) type)
;;       (Labels (cast-runtime-code-bindings!) new-e))

    
;;     ;; The ids for everything that is manditory
    
;;     ;; (define apply-med-coercion-uid
;;     ;;   (next-uid! "apply_mediating_coercion"))
    

;;     ;; (define compose-med-coercions-uid
;;     ;;   (next-uid! "compose_med_coercions"))
;;     ;; (define compose-fn-coercions-uid
;;     ;;   (next-uid! "compose-fn-coercions"))
;;     ;; (define compose-tuple-coercions-uid
;;     ;;   (next-uid! "compose-tuple-coercions"))
;;     ;; (define make-coercion-uid
;;     ;;   (next-uid! "make_coercion"))
;;     ;; (define make-med-coercion-uid
;;     ;;   (next-uid! "make_med_coercion"))
;;     ;; (define greatest-lower-bound-uid
;;     ;;   (next-uid! "greatest_lower_bound"))
;;     ;; (define copy-mref-uid
;;     ;;   (next-uid! "copy_mref_value"))
;;     ;; (define inject-uid
;;     ;;   (next-uid "inject"))
;;     ;; (define med-cast-uid
;;     ;;   (next-uid "med-cast"))
;;     ;; ;; Generate calls to runtime procedures
    
;;     ;; (define interp-compose-fn-coercions
;;     ;;   : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;;   (apply-code compose-fn-coercions-uid))
;;     ;; (define interp-compose-tuple-coercions
;;     ;;   : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;;   (apply-code compose-tuple-coercions-uid))
;;     ;; (define interp-compose-med-coercions : Compose-Coercions-Type
;;     ;;   (apply-code compose-med-coercions-uid))
;;     ;; (define interp-make-coercion : Interp-Make-Coercion-Type
;;     ;;   (apply-code make-coercion-uid))
;;     ;; (define interp-make-med-coercion : Interp-Make-Coercion-Type
;;     ;;   (apply-code make-med-coercion-uid))
;;     ;; (define interp-greatest-lower-bound : Greatest-Lower-Bound-Type
;;     ;;   (apply-code greatest-lower-bound-uid))
;;     ;; (define interp-copy-mref : Copy-Mref-Type
;;     ;;   (apply-code copy-mref-uid))
    
;;     ;; (: interp-apply-med-coercion : Interp-Cast-Type)
;;     ;; (define (interp-med-coercion e c [m (Quote 0)])
;;     ;;   (((inst apply-code CoC3-Expr) apply-med-coercion-uid) e c m))

;;     ;; (: interp-inject : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;; (define (interp-inject e t)
;;     ;;   (((inst apply-code CoC3-Expr) inject-uid) e t))


;;     ;; Combine two coercions (c1 : A => B) (c2 : B => C) by calling a runtime function
;;     (define compose-coercions-uid (next-uid! "compose_coercions"))
;;     (define interp-compose-coercions : Compose-Coercions-Type
;;       (apply-code compose-coercions-uid))
    
;;     ;; Cast by calling a function that casts by interpreting the types until
;;     ;; a coercion has to be residualized for a higher-order cast (fn or pref)
;;     (define cast-uid (next-uid "cast"))
;;     (: interp-cast Cast-Type)
;;     (define (interp-cast e t1 t2 l [mt ZERO-EXPR])
;;       (((inst apply-code CoC3-Expr) cast-uid) e t1 t2 l mt))

;;     ;; Cast by calling a function that interprets a coercion this function
;;     ;; could possibly call the "interp-cast" runtime function based on the
;;     ;; parameter hybrid-cast/coercion-interp
;;     (define apply-coercion-uid (next-uid! "apply_coercion"))
;;     (: interp-apply-coercion Apply-Coercion-Type)
;;     (define (interp-apply-coercion e c [m (Quote 0)])
;;       ;; TODO: Quote 0 is implicitly the null value later on this is a little hacky
;;       (((inst apply-code CoC3-Expr) apply-coercion-uid) e c m))

;;     (define project-uid (next-uid! "project"))
;;     (: interp-project Project-Type)
;;     (define (interp-project e t2 l)
;;       (((inst apply-code CoC3-Expr) project-uid) e t2 l))
    
;;     ;; (: interp-med-cast : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;; (define (interp-med-cast e t1 t2 l [m (Quote 0)])
;;     ;;   (((inst apply-code CoC3-Expr) med-cast-uid) e c m))
    
    
;;     ;; (define (compile-make-coercion t1 t2 l)
;;     ;;   (cond
;;     ;;     [(and (Type? t1) (Type? t2) (Quote? l) (String? (Quote-datum l)))
;;     ;;      (Quote-Coercion ((make-coercion (Quote-datum l)) (Type-type t1) (Type-type t2)))]
;;     ;;     [else (interp-make-coercion t1 t2 l)]))

    
    
   
    
;;     ;; ;; Cast Based Code Generators
;;     ;; ;; Casting Runtime that interprets types unless a coercion has to be made
;;     ;; ;; fn-casts and proxied reference casts result in a call to make-coercion
;;     ;; ;; This code is entered from projections and monotonic coercion application
;;     ;; ;; essentially whenever a coercions application results in a call to make coercion
;;     ;; ;; followed by an immediate apply-coercion.
;;     ;; (define cast-based-project-uid (next-uid "project-with-cast"))
;;     ;; (define (interp-cast-based-project e t2 l)
;;     ;;   (((inst apply-code CoC3-Expr) interp-cast-based-project-uid) e t2 l))
;;     ;; (define code-gen-cast-based-project
;;     ;;   (make-code-gen-project
;;     ;;    #:interp-project interp-cast-based-project
;;     ;;    #:interp-med-cast interp-med-cast))
;;     ;; (define (compile-cast-based-project e t2 l)
;;     ;;   (cond
;;     ;;     [(Type? t2) (code-gen-cast-based-project e t2 l)]
;;     ;;     [else (interp-cast-based-project e t2 l)]))
;;     ;; (define interp-cast-based-project-code
;;     ;;   (code$ (e t2 l) (code-gen-cast-based-project e t2 l)))

;;     ;; (define (compile-inject e t1)
;;     ;;   (cond
;;     ;;     [(Type? t1) (dyn-make$ e t1)]
;;     ;;     [else (interp-inject e t1)]))

;;     ;; ;; Fn-Cast and Ref-Cast have to "Fall-Back" to coercions to get space-efficiency
;;     ;; (define (compile-cast-based-fn-cast e t1 t2 l)
;;     ;;   (code-gen-apply-fn-coercion e (compile-make-coercion t1 t2 l)))

;;     ;; (: code-gen-apply-ref-coercion Apply-Coercion-Type)
;;     ;; (define code-gen-apply-ref-coercion
;;     ;;   (make-code-gen-apply-ref-coercion
;;     ;;    #:compose-coercions interp-compose-coercions
;;     ;;    #:id-coercion-huh Id-Coercion-Huh))

;;     ;; (define (compile-cast-based-ref-cast e t1 t2 l)
;;     ;;   (code-gen-apply-ref-coercion e (compile-make-coercion t1 t2 l)))

;;     ;; (: code-gen-mref-cast Monotonic-Cast-Type)
;;     ;; (define code-gen-mref-cast
;;     ;;   (make-code-gen-mref-cast
;;     ;;    #:compile-cast interp-cast
;;     ;;    #:greatest-lower-bound greatest-lower-bound))
    
;;     ;; (: code-gen-mvec-cast Monotonic-Cast-Type)
;;     ;; (define code-gen-mvec-cast
;;     ;;   (make-code-gen-mvec-cast
;;     ;;    #:compile-cast interp-cast
;;     ;;    #:greatest-lower-bound greatest-lower-bound))
    
;;     ;; (define code-gen-tuple-cast
;;     ;;   (make-code-gen-tuple-cast
;;     ;;    #:cast-uid cast-uid))
    
;;     ;; (define-values (code-gen-cast     cast-code     compile-cast
;;     ;;                 code-gen-med-cast med-cast-code compile-med-cast)
;;     ;;   (make-cast-helpers
;;     ;;    #:interp-cast interp-cast
;;     ;;    #:interp-med-cast interp-med-cast
;;     ;;    #:project compile-cast-based-project
;;     ;;    #:inject  compile-inject
;;     ;;    #:fn-cast compile-cast-based-fn-cast
;;     ;;    #:tuple-cast code-gen-tuple-cast
;;     ;;    #:ref-cast   code-gen-ref-cast
;;     ;;    #:mref-cast  code-gen-mref-cast
;;     ;;    #:mvec-cast  code-gen-mvec-cast))

;;     ;; (define code-gen-project
;;     ;;   (make-code-gen-projection
;;     ;;    #:interp-project  interp-project
;;     ;;    #:med-cast        interp-med-cast))
    
;;     ;; (: interp-project-code CoC3-Code)
;;     ;; (define interp-project-code
;;     ;;   ;; Using variables result in generating the generic code that is constant but large
;;     ;;   (code$ (v t2 l)
;;     ;;     (code-gen-project v t2 l)))


;;     ;; (: interp-inject-code CoC3-Code)
;;     ;; (define interp-inject-code
;;     ;;   (code$ (v t1)
;;     ;;     (code-gen-inject v t1)))
        
;;     ;; (: code-gen-apply-tuple-coercion Apply-Tuple-Coercion-Type)
;;     ;; (define code-gen-apply-tuple-coercion
;;     ;;   (make-code-gen-apply-tuple-coercion
;;     ;;    #:apply-coercion-uid apply-coercion-uid))

    


    
;;     ;; (: interp-apply-coercion-code CoC3-Code)
;;     ;; (define interp-apply-coercion-code
;;     ;;   (code$ (v c mt)
;;     ;;     (code-gen-apply-coercion v c mt)))    

    
;;     ;; ;; Cunstruct a hyper-coercions expression from the type and blame
;;     ;; ;; information contained in a type-based cast.
;;     ;; ;; This coercion literal is eligable to be hoisted in a later
;;     ;; ;; pass so that it only ever is creates a value once. 
;;     ;; (: make-coercion : Make-Coercion-Type)
;;     ;; (define (make-coercion t1 t2 l)
;;     ;;   (: recur : Schml-Type Schml-Type -> Hyper-Coercion)
;;     ;;   (define (recur t1 t2)
;;     ;;     (match* (t1 t2)
;;     ;;       [(t        t) (HC #f t #f #f t IDENTITY)]
;;     ;;       ;; These two lines create an invarient that the only
;;     ;;       ;; time t1 or t2 is Dyn the entire hyper-coercion is an identity 
;;     ;;       [((Dyn)    t) (HC #t t l  #f t IDENTITY)]
;;     ;;       [(t    (Dyn)) (HC #f t #f #t t IDENTITY)]
;;     ;;       [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
;;     ;;        ;; The arity check here means that all coercions have
;;     ;;        ;; the correct arity under the static type system.
;;     ;;        ;; Notice that the argument types are reversed
;;     ;;        ;; because of contravarience of functions.
;;     ;;        (HC #f t1 #f #f t2 (Fn n1 (map recur a2* a1*) (recur r1 r2)))]
;;     ;;       [((STuple n1 t1*) (STuple n2 t2*)) #:when (= n1 n2)
;;     ;;        (HC #f t1 #f #f t2 (CTuple n1 (map recur t1* t2*)))]
;;     ;;       [((GRef t1) (GRef t2))
;;     ;;        (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
;;     ;;       [((GVect t1) (GVect t2))
;;     ;;        (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
;;     ;;       [((MRef _) (MRef t2))
;;     ;;        (HC #f t1 #f #f t2 (MonoRef t2))]
;;     ;;       [((MVect _) (MVect t2))
;;     ;;        (HC #f t1 #f #f t2 (MonoVect t2))]
;;     ;;       [(t1 t2)
;;     ;;        (HC #f t1 #f #f t2 (Failed l))]))
;;     ;;   (Quote-HCoercion (recur t1 t2)))
;;     ;; (: interp-make-coercion-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-make-coercion-code
;;     ;;   (define-values (t1u t2u lu)
;;     ;;       (values (next-uid! "t1") (next-uid! "t2") (next-uid! "blame_info")))
;;     ;;   (define-values (t1 t2 l)
;;     ;;       (values (Var t1u) (Var t2u) (Var lu)))
;;     ;;   (code$ (t1 t2 l)
;;     ;;     (cond$
;;     ;;      [(op=? t1 t2)
;;     ;;       (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t1 Id) CoC3-Expr)]
;;     ;;      ;; This is absolutly necisarry
;;     ;;      ;; While Injections and Projections are never made by
;;     ;;      ;; source code coercions composition can create new
;;     ;;      ;; projections and injections.
;;     ;;      [(Type-Dyn-Huh t1)
;;     ;;       (ann (HC (Quote #t) t2 l (Quote #f) t2 Id) CoC3-Expr)]
;;     ;;      [(Type-Dyn-Huh t2)
;;     ;;       (ann (HC (Quote #f) t1 (Quote #f) (Quote #t) t1 Id) CoC3-Expr)]
;;     ;;      [else
;;     ;;       (let$ ([med (interp-make-med-coercion t1 t2 l)]) 
;;     ;;         (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t2 med) CoC3-Expr))])))
;;     ;; (: interp-make-med-coercion-code
;;     ;;    (Code Uid* CoC3-Expr))
;;     ;; (define interp-make-med-coercion-code
;;     ;;   ;; precondition: t1 != Dyn /\ t2 != Dyn /\ t1 != t2
;;     ;;   ;; Should be called make_non_id_med_coercion
;;     ;;   (code$ (t1 t2 l)
;;     ;;     (precondition$ (not$ (and$ (op=? t1 t2) (Type-Dyn-Huh t1) (Type-Dyn-Huh t2)))
;;     ;;       (cond$
;;     ;;        [(and$ (Type-Fn-Huh t1) (Type-Fn-Huh t2) 
;;     ;;               (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
;;     ;;         ;; This line is a little tricky because unless we have actual
;;     ;;         ;; types for this at compile time we have to generate code that
;;     ;;         ;; can handle arbitry fn-arity.  We delegate this task to specify
;;     ;;         ;; representation because it involves safely allocating an object
;;     ;;         ;; whos size cannot be determined until run-time.
;;     ;;         (Make-Fn-Coercion interp-make-coercion-uid t1 t2 l)]
;;     ;;        [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
;;     ;;               (op<=? (tupleT-num$ t2) (tupleT-num$ t1)))
;;     ;;         (Make-Tuple-Coercion interp-make-coercion-uid t1 t2 l)]
;;     ;;        [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
;;     ;;         (let*$ ([gv1_of (Type-GVect-Of t1)]
;;     ;;                 [gv2_of (Type-GVect-Of t2)]
;;     ;;                 [write_crcn (interp-make-coercion gv2_of gv1_of l)]
;;     ;;                 [read_crcn  (interp-make-coercion gv1_of gv2_of l)])
;;     ;;           (Ref-Coercion read_crcn write_crcn))]
;;     ;;        [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
;;     ;;         (let*$ ([gr1_of (Type-GRef-Of t1)]
;;     ;;                 [gr2_of (Type-GRef-Of t2)]
;;     ;;                 [write_crcn (interp-make-coercion gr2_of gr1_of l)]
;;     ;;                 [read_crcn  (interp-make-coercion gr1_of gr2_of l)])
;;     ;;           (Ref-Coercion read_crcn write_crcn))]
;;     ;;        ;; TODO should these two line be (glb t1 t2)? 
;;     ;;        [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
;;     ;;         (MRef-Coercion (mref-of$ t2))]
;;     ;;        [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
;;     ;;         (MVect-Coercion (mvect-of$ t2))]
;;     ;;        [else (Failed-Coercion l)]))))

;;     ;; (: interp-compose-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-coercions-code
;;     ;;   (code$ (fst snd)
;;     ;;     (precondition$
;;     ;;         ;; all sequences of coercions to compose are well typed because:
;;     ;;         ;; either the inner types are well-typed or the coercions are
;;     ;;         ;; some combination injections projections and dynamic
;;     ;;         ;; identities that are well typed
;;     ;;         (or$ (op=? (HC-T2 fst) (HC-T1 snd))
;;     ;;              (and$ (or$ (Type-Dyn-Huh (HC-T2 fst)) (HC-Inject-Huh fst))
;;     ;;                    (or$ (Type-Dyn-Huh (HC-T1 snd)) (HC-Project-Huh snd))))
;;     ;;       (let*$ ([fst-t2 (HC-T2 fst)]
;;     ;;               [snd-t1 (HC-T1 snd)])
;;     ;;         (cond$
;;     ;;          ;; These first two cases rule out dynamic identity casts
;;     ;;          ;; ie (HC #f Dyn #f #f Dyn ID)
;;     ;;          ;; These cannot be composed under the following rules because
;;     ;;          ;; the usual invarient of all code following is that Dyn
;;     ;;          ;; isn't present as t1 or t2 in any coercion.
;;     ;;          [(HC-Identity-Huh fst) snd]
;;     ;;          [(HC-Identity-Huh snd) fst]
;;     ;;          [else
;;     ;;           (let*$ ([fst-med (HC-Med fst)]
;;     ;;                   [mid-med
;;     ;;                    (cond$
;;     ;;                     [(and$ (HC-Inject-Huh fst)
;;     ;;                            (HC-Project-Huh snd)
;;     ;;                            ;; interp-make-med-coercion assumes that fst-t2 != snd-t1
;;     ;;                            (not$ (op=? fst-t2 snd-t1)))
;;     ;;                      (let*$ ([mid (interp-make-med-coercion
;;     ;;                                    fst-t2 snd-t1 (HC-Label snd))])
;;     ;;                        ;; we know these won't be Id
;;     ;;                        (interp-compose-med-coercions fst-med mid))]
;;     ;;                     [else fst-med])]
;;     ;;                   [snd-med (HC-Med snd)]
;;     ;;                   [fnl-med
;;     ;;                    ;; consider trying the id-checks here
;;     ;;                    (cond$
;;     ;;                       [(Id-Coercion-Huh fst-med) snd-med]
;;     ;;                       [(Id-Coercion-Huh snd-med) fst-med]
;;     ;;                       [else (interp-compose-med-coercions mid-med snd-med)])
;;     ;;                    (interp-compose-med-coercions mid-med snd-med)])
;;     ;;             (HC (HC-Project-Huh fst) (HC-T1 fst) (HC-Label fst) 
;;     ;;                 (HC-Inject-Huh snd) (HC-T2 snd)
;;     ;;                 fnl-med))])))))

;;     ;; (: interp-compose-med-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-med-coercions-code
;;     ;;   (code$ (fst snd)
;;     ;;     (cond$
;;     ;;      ;; TODO consider specializing this code
;;     ;;      ;; by moving the Id-coercion-huh calls to
;;     ;;      ;; before each call to compose-med.
;;     ;;      [(Id-Coercion-Huh fst) snd]
;;     ;;      [(Id-Coercion-Huh snd) fst]
;;     ;;      [(and$ (Mediating-Coercion-Huh fst) (Mediating-Coercion-Huh snd))
;;     ;;       (cond$
;;     ;;        ;; Ditching the second check doesn't actually decrease the
;;     ;;        ;; number of checks once we move the failure casts into
;;     ;;        ;; mediating casts. 
;;     ;;        [(and$ (Fn-Coercion-Huh fst) (Fn-Coercion-Huh snd))
;;     ;;         (interp-compose-fn-coercions
;;     ;;          fst snd (Quote 0) (Fn-Coercion-Arity fst) (Quote #t))]
;;     ;;        [(and$ (Tuple-Coercion-Huh fst) (Tuple-Coercion-Huh snd))
;;     ;;         (interp-compose-tuple-coercions
;;     ;;          fst snd (Quote 0) (Tuple-Coercion-Num fst) (Quote #t))]
;;     ;;        [(and$ (Ref-Coercion-Huh fst) (Ref-Coercion-Huh snd))
;;     ;;         (let*$ ([fst-write (Ref-Coercion-Write fst)]
;;     ;;                 [snd-write (Ref-Coercion-Write snd)]
;;     ;;                 [com-write (interp-compose-coercions snd-write fst-write)]
;;     ;;                 [fst-read  (Ref-Coercion-Read fst)]
;;     ;;                 [snd-read  (Ref-Coercion-Read snd)]
;;     ;;                 [com-read  (interp-compose-coercions fst-read snd-read)])
;;     ;;           (If (and$ (HC-Identity-Huh com-read)
;;     ;;                     (HC-Identity-Huh com-write))
;;     ;;               (Quote-Coercion IDENTITY)
;;     ;;               (Ref-Coercion com-read com-write)))]
;;     ;;        [(and$ (MRef-Coercion-Huh fst) (MRef-Coercion-Huh snd))
;;     ;;         (let*$ ([fst_type  (MRef-Coercion-Type fst)]
;;     ;;                 [snd_type  (MRef-Coercion-Type snd)]
;;     ;;                 [glb       (greatest-lower-bound fst_type snd_type)])
;;     ;;           (MRef-Coercion glb))]
;;     ;;        [(and$ (MVect-Coercion-Huh fst) (MVect-Coercion-Huh snd))
;;     ;;         (let*$ ([fst_type  (MVect-Coercion-Type fst)]
;;     ;;                 [snd_type  (MVect-Coercion-Type snd)]
;;     ;;                 [glb       (greatest-lower-bound fst_type snd_type)])
;;     ;;           (MVect-Coercion glb))]
;;     ;;        [else
;;     ;;         (Blame (Quote "Internal Error: compose-mediating-coercions 1"))])]
;;     ;;      [(Failed-Coercion-Huh fst)
;;     ;;       (If (Failed-Coercion-Huh snd)
;;     ;;           fst ;; merge blame info for bidirectional behavior
;;     ;;           fst)]
;;     ;;      [(Failed-Coercion-Huh snd) snd]
;;     ;;      [else
;;     ;;       (Blame (Quote "Internal Error: compose-mediating-coercion 2"))])))

;;     ;; (: interp-compose-fn-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-fn-coercions-code
;;     ;;   ;; TODO Implement Fn-coercion-set! Make-Fn-Coercion
;;     ;;   ;; TODO make this iterative
;;     ;;   (code$ (c1 c2 i a was-id)
;;     ;;     (cond$
;;     ;;      [(Op '= `(,i ,a))
;;     ;;       (let*$ ([r1 (Fn-Coercion-Return c1)]
;;     ;;               [r2 (Fn-Coercion-Return c2)]
;;     ;;               [cr (interp-compose-coercions r1 r2)])
;;     ;;         (cond$
;;     ;;          [(and$ was-id (HC-Identity-Huh cr)) (Quote-Coercion (Identity))]
;;     ;;          [else 
;;     ;;           (let$ ([fnc (Id-Fn-Coercion a)])
;;     ;;             (Fn-Coercion-Return-Set! fnc cr)
;;     ;;             fnc)]))]
;;     ;;      [else
;;     ;;       (let*$ ([i2 (Fn-Coercion-Arg c2 i)]
;;     ;;               [i1 (Fn-Coercion-Arg c1 i)]
;;     ;;               [ca (interp-compose-coercions i2 i1)]
;;     ;;               [is-id (and$ was-id (HC-Identity-Huh ca))]
;;     ;;               [next-i (Op '+ `(,i ,(Quote 1)))]
;;     ;;               [m  (interp-compose-fn-coercions c1 c2 next-i a is-id)])
;;     ;;         (cond$
;;     ;;          [(Id-Coercion-Huh m) m]
;;     ;;          [else (Fn-Coercion-Arg-Set! m i ca) m]))])))

;;     ;; (: interp-compose-tuple-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-tuple-coercions-code
;;     ;;   ;; TODO make this iterative 
;;     ;;   (code$ (c1 c2 i a was-id)
;;     ;;     (cond$
;;     ;;      [(Op '= `(,i ,a))
;;     ;;       (cond$
;;     ;;        [was-id (Quote-Coercion (Identity))]
;;     ;;        [else (Id-Tuple-Coercion a)])]
;;     ;;      [else
;;     ;;       (let*$ ([e1 (Tuple-Coercion-Item c1 i)]
;;     ;;               [e2 (Tuple-Coercion-Item c2 i)]
;;     ;;               [ce (interp-compose-coercions e1 e2)]
;;     ;;               [is-id (and$ was-id (HC-Identity-Huh ce))]
;;     ;;               [new-i (Op '+ `(,(Quote 1) ,i))]
;;     ;;               [m  (interp-compose-tuple-coercions c1 c2 new-i a is-id)])
;;     ;;         (cond$
;;     ;;          [(Id-Coercion-Huh m) m]
;;     ;;          [else (Tuple-Coercion-Item-Set! m i ce)]))])))

;;     ;; (define greatest-lower-bound-code
;;     ;;   (code$ (t1 t2)
;;     ;;     ((gen-greatest-lower-bound-type-code
;;     ;;       next-uid!
;;     ;;       greatest-lower-bound
;;     ;;       greatest-lower-bound-uid)
;;     ;;      t1 t2)))

;;     ;; (define copy-mref-code
;;     ;;   (code$ (mref)
;;     ;;     ((gen-copy-value-in-monoref-code next-uid!) mref)))

;;     ;; (: compile-make-coercion Compile-Make-Coercion-Type)
;;     ;; ;; TODO : can this be any better?
;;     ;; (define (compile-make-coercion t1 t2 l)
;;     ;;   (match* (t1 t2 l)
;;     ;;     [((Type t1) (Type t2) (Quote (? blame-label? l)))
;;     ;;      (make-coercion t1 t2 l)]
;;     ;;     [(t1 t2 l) (interp-make-coercion t1 t2 l)]))

;;     ;; TODO should project always code-gen?
;;     ;; Should have the option on invoking interp-cast / interp-apply-coercion
;;     (define compile-project 
;;       (error 'todo))
;;     (define compile-inject
;;       (error 'todo))
;;     (define compile-fn-cast
;;       (error 'todo))
;;     (define compile-ref-cast
;;       (error 'todo))
;;     (define compile-tup-cast
;;       (error 'todo))
;;     (define compile-mref-cast
;;       (error 'todo))
;;     (define compile-mvec-cast
;;       (error 'todo))
    
;;     ;; Generic specialization of source level casts
;;     (define compile-cast
;;       (make-compile-cast
;;        #:compile-project   compile-project
;;        #:compile-inject    compile-inject
;;        #:compile-fn-cast   compile-fn-cast
;;        #:compile-tup-cast  compile-tup-cast 
;;        #:compile-ref-cast  compile-ref-cast
;;        #:compile-mref-cast compile-mref-cast
;;        #:compile-mvec-cast compile-mvec-cast
;;        ;; There shouldn't be any casts that actually fall into this category.
;;        #:interp-cast       interp-cast))
    
;;     (: get-fn-cast! : Nat -> Uid)
;;     (: get-fn-cast-bindings! : -> CoC3-Bnd-Code*)
;;     (define-values (get-fn-cast! get-fn-cast-bindings!)
;;       (make-fn-cast-helpers
;;        (make-build-caster/coercions
;;         #:apply-coercion-uid cast-uid
;;         #:compose-coercions  interp-compose-coercions
;;         #:id-coercion-huh    Id-Coercion-Huh)))
    
;;     (: compile-lambda Compile-Lambda-Type)
;;     (define (compile-lambda f* expr)
;;       (let ([caster (get-fn-cast! (length f*))])
;;         (Lambda f* (Castable caster expr))))
    
;;     (: compile-app Compile-App-Type)
;;     (define (compile-app e e*)
;;       (App-Fn-or-Proxy apply-coercion-uid e e*))
   
;;     (: pbox-ref  PBox-Ref-Type)
;;     (: pbox-set! PBox-Set-Type)
;;     (: pvec-ref  PVec-Ref-Type)
;;     (: pvec-set! PVec-Set-Type)
;;     (: pvec-len  PVec-Len-Type)
;;     (: proxied-operations-bindings CoC3-Bnd-Code*)
;;     (define-values (pbox-ref pbox-set!
;;                     pvec-ref pvec-set! pvec-len
;;                     proxied-operations-bindings)
;;       (make-proxied-reference/coercions-compile-helpers
;;        #:apply-coercion interp-cast))
    
;;     (: mbox-set! MBox-Set-Type)
;;     (: mbox-ref MBox-Ref-Type)
;;     (: mvec-ref MVec-Ref-Type)
;;     (: mvec-set! MVec-Set-Type)
;;     (: monotonic-operations-bindings CoC3-Bnd-Code*)
;;     (define-values (mbox-ref mbox-set! mvec-ref mvec-set! monotonic-operations-bindings)
;;       (make-monotonic-reference/coercions-compile-helpers
;;        ;; TODO we should be able to handle mvecs and mrefs seperately
;;        ;; because the sharing between mvecs could mean coercions are a win whereas
;;        ;; it seems obvious that we would want to interp-cast for mref
;;        #:interp-cast interp-cast))
    
;;     (: dyn-pbox-ref Dyn-PBox-Ref-Type)
;;     (: dyn-pbox-set! Dyn-PBox-Set-Type)
;;     (: dyn-pvec-ref Dyn-PVec-Ref-Type)
;;     (: dyn-pvec-set! Dyn-PVec-Set-Type)
;;     (: dyn-pvec-len Dyn-PVec-Len-Type)
;;     (: dyn-mbox-ref Dyn-MBox-Ref-Type)
;;     (: dyn-mbox-set! Dyn-MBox-Set-Type)
;;     (: dyn-mvec-ref Dyn-MVec-Ref-Type)
;;     (: dyn-mvec-set! Dyn-MVec-Set-Type)
;;     (: dyn-fn-app Dyn-Fn-App-Type)
;;     (: dyn-tup-prj Dyn-Tup-Prj-Type)
;;     (: dynamic-operations-bindings CoC3-Bnd-Code*)
;;     (define-values (dyn-pbox-ref dyn-pbox-set!
;;                     dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
;;                     dyn-mbox-ref dyn-mbox-set!
;;                     dyn-mvec-ref dyn-mvec-set!
;;                     dyn-fn-app dyn-tup-prj
;;                     dynamic-operations-bindings)
;;       (make-dynamic-operations/coercions-compile-helpers
;;        #:cast compile-cast #:compile-app compile-app
;;        #:pbox-ref pbox-ref #:pbox-set pbox-set!
;;        #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
;;        #:mbox-ref mbox-ref #:mbox-set mbox-set!
;;        #:mvec-ref mvec-ref #:mvec-set mvec-set!))
    
;;     (define-type Get-Cast-Code-Bindings-Type (-> CoC3-Bnd-Code*))
;;     (: get-cast-code-bindings! Get-Cast-Code-Bindings-Type)
;;     (define (get-cast-code-bindings!)
;;       (append
;;        (get-fn-cast-bindings!)
;;        proxied-operations-bindings
;;        monotonic-operations-bindings
;;        dynamic-operations-bindings
;;        `([,interp-projection-uid . ,interp-projection-code]
;;          [,interp-cast-uid . ,interp-cast-code]
;;          [,interp-med-cast-uid . ,interp-med-cast-code]
;;          [,interp-make-coercion-uid . ,interp-make-coercion-code]
;;          [,interp-make-med-coercion-uid . ,interp-make-med-coercion-code]
;;          [,interp-compose-coercions-uid . ,interp-compose-coercions-code]
;;          [,interp-compose-med-coercions-uid
;;           . ,interp-compose-med-coercions-code]
;;          [,interp-compose-fn-coercions-uid
;;           . ,interp-compose-fn-coercions-code]
;;          [,interp-compose-tuple-coercions-uid
;;           . ,interp-compose-tuple-coercions-code]
;;          [,greatest-lower-bound-uid . ,greatest-lower-bound-code]
;;          [,copy-mref-uid . ,copy-mref-code])))


;;     (define map-expr!
;;       (make-map-expr
;;        #:compile-cast compile-cast
;;        #:compile-lambda compile-lambda
;;        #:compile-app compile-app
;;        #:pbox-ref pbox-ref
;;        #:pbox-set pbox-set!
;;        #:pvec-ref pvec-ref
;;        #:pvec-set pvec-set!
;;        #:pvec-len pvec-len
;;        #:mbox-ref mbox-ref
;;        #:mbox-set mbox-set!
;;        #:mvec-ref mvec-ref
;;        #:mvec-set mvec-set!
;;        #:dyn-pbox-ref dyn-pbox-ref
;;        #:dyn-pbox-set dyn-pbox-set!
;;        #:dyn-pvec-ref dyn-pvec-ref
;;        #:dyn-pvec-set dyn-pvec-set!
;;        #:dyn-pvec-len dyn-pvec-len
;;        #:dyn-mbox-ref dyn-mbox-ref
;;        #:dyn-mbox-set dyn-mbox-set!
;;        #:dyn-mvec-ref dyn-mvec-ref
;;        #:dyn-mvec-set dyn-mvec-set!
;;        #:dyn-fn-app dyn-fn-app
;;        #:dyn-tup-prj dyn-tup-prj))

;;     )

;;   )

;; (: make-code-gen-project
;;    (->* (#:interp-project Project-Type
;;          #:med-cast Cast-Type)
;;         Project-Type))
;; (define ((make-code-gen-project #:interp-project interp-project #:med-cast med-cast) e t2 l)
;;   ;; Generates code that will project a dynamic value to a static type t2
;;   ;; or fail and blame l.
;;   ;; Notes:
;;   ;; T2 cannot be dynamic; It doesn't make sense to project to dyn
;;   ;; The second and third cases are specializations based on knowing what
;;   ;; representations the dynamic value will have have with specific types.
;;   ;; Failure in these cases is represented by calling the generic version of
;;   ;; projection (the fourth case) which could in theory be used to add conversion
;;   ;; casts. Currently this code generically projectes the value and casts
;;   ;; using the runtime cast interpreter. 
;;   ;; TODO let med-cast take care of the eq check
;;   (let*$ ([v e] [l l])
;;     (match t2
;;       [(Type (Dyn))
;;        (error 'grift/make-code-gen-project "Called with t2 = Dyn")]
;;       [(Type (or (Int) (Character) (Unit) (Bool)))
;;        (If (dyn-immediate-tag=?$ v t2)
;;            (dyn-immediate-value$ v)
;;            (interp-project v t2 l))]
;;       [(Type _) 
;;        (If (dyn-immediate-tag=?$ v t2)
;;            (let*$ ([u  (dyn-box-value$ v)]
;;                    [t1 (dyn-box-type$ v)])
;;              (If (op=? t1 t2)
;;                  u
;;                  (med-cast u t1 t2 l)))
;;            (interp-project v t2 l))]
;;       [otherwise 
;;        (let$ ([t2 t2])
;;          (precondition$ (not$ (Type-Dyn-Huh t2))
;;            (let*$ ([u  (dyn-value$ v)]
;;                    [t1 (dyn-type$ v)]) 
;;              (If (op=? t1 t2)
;;                  u
;;                  ;; t1 != t2 -> < t1 =>^l t2> != Id
;;                  ;; therefore no need to make Id case fast 
;;                  (med-cast u t1 t2 l #:know-=/= #t)))))])))

;; (: code-gen-inject : CoC3-Expr CoC3-Expr -> CoC3-Expr)
;; (define (code-gen-inject e t)
;;   ;; Dyn make generates specialized code when t is known
;;   (dyn-make$ e t))

;; (define-type Monotonic-Cast-Type
;;   (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
;; (: make-code-gen-mref-cast
;;    (->* (#:compile-cast Compile-Cast-Type
;;          #:type-greatest-lower-bound Greatest-Lower-Bound-Type)
;;         Monotonic-Cast-Type))
;; (define ((make-code-gen-mref-cast
;;           #:compile-cast compile-cast
;;           #:copy-mref copy-mref
;;           #:greatest-lower-bound greatest-lower-bound)
;;          e t2 [l (Quote "Monotonic")])
;;   (let*$ ([v e][t2 t2])
;;     (cond$
;;      [(Type-Dyn-Huh t2) v]
;;      [else
;;       (let*$ ([t1 (Mbox-rtti-ref v)]
;;               [t3 (greatest-lower-bound t1 t2)])
;;         (cond$
;;          [(op=? t1 t3) v]
;;          [else
;;           (Mbox-rtti-set! v t3)
;;           (let*$ ([v-copy (copy-mref v)]
;;                   [new-v (compile-cast v-copy t1 t3 l v)]
;;                   [t4 (Mbox-rtti-ref v)])
;;             (cond$
;;              [(op=? t3 t4) (Mbox-val-set! v new-v) v]
;;              [else v]))]))])))

;; (: make-code-gen-mvec-cast
;;    (->* (#:compile-cast Compile-Cast-Type
;;          #:type-greatest-lower-bound Greatest-Lower-Bound-Type)
;;         Monotonic-Cast-Type))
;; (define ((make-code-gen-mvec-cast
;;           #:compile-cast compile-cast
;;           #:greatest-lower-bound greatest-lower-bound)
;;          ;; This blame label is fabricated from nothing because
;;          ;; monotonic references are not completely implemented.
;;          e t2 [l (Quote "Monotonic")])
;;   (let*$ ([v e] [t2 t2])
;;     (cond$
;;      [(Type-Dyn-Huh t2) v]
;;      [else
;;       (let*$ ([t1 (Mvector-rtti-ref v)]
;;               [t3 (greatest-lower-bound t1 t2)])
;;         (cond$
;;          [(op=? t1 t3) v]
;;          [else
;;           (Mvector-rtti-set! v t3)
;;           (let$ ([len (Mvector-length v)])
;;             (cond$
;;              [(Type-Tuple-Huh t3)
;;               (let*$ ([n (Type-Tuple-num t3)])
;;                 (repeat$ (i (Quote 0) len) ()
;;                   (let*$ ([vi (Mvector-val-ref v i)]
;;                           [cvi (Copy-Tuple n vi)])
;;                     (Mvector-val-set! v i cvi)
;;                     (let*$ ([ccvi (compile-cast cvi t1 t3 l v)]
;;                             [t4 (Mvector-rtti-ref v)])
;;                       (if$ (op=? t3 t4)
;;                            (Mvector-val-set! v i ccvi)
;;                            (Break-Repeat))))))]
;;              [else
;;               (repeat$ (i (Quote 0) len) ()
;;                 (let*$ ([vi (Mvector-val-ref v i)]
;;                         [cvi (compile-cast vi t1 t3 l v)]
;;                         [t4 (Mvector-rtti-ref v)])
;;                   (If (op=? t3 t4)
;;                       (Mvector-val-set! v i cvi)
;;                       (Break-Repeat))))]))
;;           v]))])))



;; (: code-gen-apply-fn-coercion : CoC3-Expr CoC3-Expr -> CoC3-Expr)
;; (define (code-gen-apply-fn-coercion e m)
;;   ;; Let binding to provent expression duplication
;;   (let*$ ([v e] [m m])
;;     ;; TODO!!! consider
;;     ;; (App-Code (Fn-Caster v) (list v m))
;;     ;; Benifit: no double-memory indirect
;;     ;;          cast code can be specific
;;     ;;          two branches eliminated
;;     ;; Cost   : Proxies must have a caster slot
;;     ;; (App-Code (Global-Cast-Table-Ref (Fn-Coercion-arity m))
;;     ;;           (list v m))
;;     ;; Benifit: Functions no longer have cast code cell
;;     ;; Neg    : Another double memory indirect
;;     ;;        : Another branch on the otherside cast code
;;     (If (Fn-Proxy-Huh v)
;;         (App-Code (Fn-Caster (Fn-Proxy-Closure v)) (list v m))
;;         (App-Code (Fn-Caster v) (list v m)))))

;; ;; TODO introduce assignments that are unboxed as long as they do not
;; ;; escape. Implement tup-cast here instead of at specify representation
;; ;; worrying about the overhead of allocation here is too much.
;; (: make-code-gen-apply-tuple-coercion
;;    (->* (#:apply-coercion-uid Uid) Apply-Coercion-Type))
;; (define ((make-code-gen-apply-tuple-coercion #:apply-coercion-uid apply-coercion-uid)
;;          e m mt)
;;   (match mt
;;     [(Quote 0) (Coerce-Tuple apply-coercion-uid e m)]
;;     [_
;;      (let$ ([v e][m m][mt mt])
;;        (If (Op '= (list (Quote 0) mt))
;;            (Coerce-Tuple apply-coercion-uid v m)
;;            (Coerce-Tuple-In-Place apply-coercion-uid v m mt)))]))

;; (: make-code-gen-cast-tuple 
;;    (->* (#:cast-uid Uid) Cast-Type))
;; (define ((make-code-gen-cast-tuple #:cast-uid cast-uid) e t1 t2 l mt)
;;   (match mt
;;     [(Quote 0) (Cast-Tuple cast-uid e t1 t2 l)]
;;     [_
;;      (let$ ([v e] [t1 t1] [t2 t2] [l l] [mt mt])
;;        (If (op=? mt ZERO-EXPR)
;;            (Cast-Tuple cast-uid v t1 t2 l)
;;            (Cast-Tuple-In-Place cast-uid v t1 t2 l mt)))]))

;; (: make-code-gen-apply-ref-coercion
;;    (->* (#:compose-coercions Compose-Coercions-Type
;;          #:id-coercion-huh Id-Coercion-Huh-Type)
;;         Apply-Coercion-Type))
;; (define ((make-code-gen-apply-ref-coercion
;;           #:compose-coercions compose-coercions
;;           #:id-coercion-huh id-coercion-huh) e m [mt (Quote 0)])
;;   ;; The mt is ignored here
;;   (let*$ ([v e] [m m])
;;     ;; There is a small amount of specialization here because
;;     ;; we know precisely which case of inter-compose-med will
;;     ;; match...
;;     (cond$
;;      [(Guarded-Proxy-Huh v)
;;       (precondition$
;;           (Ref-Coercion-Huh (Guarded-Proxy-Coercion v))
;;         (let*$ ([old-v  (Guarded-Proxy-Ref v)]
;;                 [old-m  (Guarded-Proxy-Coercion v)]
;;                 [o-write (Ref-Coercion-Write old-m)]
;;                 [m-write (Ref-Coercion-Write m)]
;;                 [r-write (compose-coercions m-write o-write)]
;;                 [o-read  (Ref-Coercion-Read old-m)]
;;                 [m-read  (Ref-Coercion-Read m)]
;;                 [r-read  (compose-coercions o-read m-read)])
;;           (cond$
;;            [(and$ (id-coercion-huh r-read) (id-coercion-huh r-write))
;;             old-v]
;;            [else
;;             (Guarded-Proxy old-v (Coercion (Ref-Coercion r-read r-write)))])))]
;;      [else (Guarded-Proxy v (Coercion m))])))

;; (: make-cast-helpers
;;    (->* (#:interp-cast Interp-Cast-Type
;;          #:project     Project-Type
;;          #:inject      Inject-Type
;;          #:fn-cast     Fn-Cast-Type
;;          #:tuple-cast  Tuple-Cast-Type
;;          #:ref-cast    Ref-Cast-Type
;;          #:mref-cast   MRef-Cast-Type
;;          #:mvec-cast   MVec-Cast-Type)
;;         (Values Cast-Type CoC3-Code Cast-Type
;;                 Cast-Type CoC3-Code Compile-Med-Cast-Type)))
;; (define (make-cast-helpers
;;          #:interp-cast interp-cast
;;          #:interp-med-cast interp-med-cast
;;          #:project compile-project
;;          #:inject  compile-inject
;;          #:fn-cast compile-fn-cast
;;          #:tuple-cast compile-tuple-cast
;;          #:ref-cast compile-ref-cast
;;          #:mref-cast compile-mref-cast
;;          #:mvec-cast compile-mvec-cast)
  
;;   (define (code-gen-cast v t1 t2 l
;;                          [mt ZERO-EXPR]
;;                          #:t1-not-dyn [t1-not-dyn #f]
;;                          #:t2-not-dyn [t2-not-dyn #f])
;;     (match* (t1 t2)
;;       [(t t) v]
;;       [((Type (Dyn)) (Type _))
;;        ;; t2 not dyn because of (t t) case
;;        (compile-project v t2 l)]
;;       [((Type (Dyn)) t2)  #:when t2-not-dyn
;;        (compile-project v t2 l)]
;;       [((Type (Dyn)) t2) ;; t2 not Type (2 above)
;;        (let$ ([v v] [t2 t2] [l l])
;;          (If (Type-Dyn-Huh t2)
;;              v
;;              (compile-project v t2 l)))]
;;       [((Type _) (Type (Dyn)))
;;        ;; t1 not dyn because of (t t) case
;;        (compile-inject v t1)]
;;       [(t2 (Type (Dyn))) #:when t1-not-dyn
;;        (compile-inject v t1)]
;;       [(t1 (Type (Dyn)))
;;        (let$ ([v v] [t1 t1] [l l])
;;          (If (Type-Dyn-Huh t1)
;;              v
;;              (compile-inject v t1)))]
;;       [((Type (or (Int) (Bool) (Character) (Float) (Unit)))
;;         (Type (or (Int) (Bool) (Character) (Float) (Unit))))
;;        ;; Non-Equal Base types
;;        ;; This shouldn't come up in static code generation? (I think ... -akuhlens)
;;        ;; (It would mean the program wasn't consistent
;;        (error 'interpret-casts/code-gen-interp-cast "This shouldn't happen")]
;;       [((Type _) (Type _))
;;        (code-gen-med-cast v t1 t2 l mt)]
;;       [(t1 (Type _)) #:when t1-not-dyn
;;        (code-gen-med-cast v t1 t2 l mt)]
;;       [((Type _) t2) #:when t2-not-dyn
;;        (code-gen-med-cast v t1 t2 l mt)]
;;       [(t1 t2)
;;        (let$ ([v v] [t1 t1] [t2 t2] [l l] [mt mt])
;;          (cond$
;;           [(op=? t1 t2) v]
;;           [(Type-Dyn-Huh t1) (compile-project v t2 l)]
;;           [(Type-Dyn-Huh t2) (compile-inject v t1 l)]
;;           [else (compile-med-cast v t1 t2 l mt)]))]))

;;   (: code-gen-med-cast Cast-Type)
;;   (define (code-gen-med-cast v t1 t2 l
;;                              [mt ZERO-EXPR]
;;                              ;; These two arguments are ignored
;;                              ;; because the precondition is that
;;                              ;; they are both true
;;                              [t1-not-dyn #t]
;;                              [t2-not-dyn #t])
;;     (match* (t1 t2)
;;       [((Type (Dyn)) _)
;;        (error 'interp-cast/code-gen-med-cast "t1 = Dyn, precondition false")]
;;       [(_ (Type (Dyn)))
;;        (error 'interp-cast/code-gen-med-cast "t2 = Dyn, precondition false")]
;;       [((Type t1-t) (Type t2-t))
;;        (match* (t1-t t2-t)
;;          [((Fn a _ _) (Fn a _ _))
;;           (compile-fn-cast v t1 t2 l)]
;;          [((GRef t1) (GRef t2))
;;           (compile-ref-cast v (Type t1) (Type t2) l)]
;;          [((GVect t1) (GVect t2))
;;           (compile-ref-cast v (Type t1) (Type t2) l)]
;;          [((MRef _) (MRef t2))
;;           (compile-mref-cast v (Type t2))]
;;          [((MVect _) (MVect t2))
;;           (compile-mvec-cast v (Type t2))]
;;          [((CTuple n _) (CTuple m _)) #:when (<= m n)
;;           (compile-tuple-cast v t1 t2 l)]
;;          [(_ _) (begin$ v (Blame l))])]           
;;       [((Type t1-t) t2)
;;        (define lu (next-uid! "l"))
;;        (define l^ (Var lu))
;;        (define tu (next-uid! "t2"))
;;        (define t^ (Var tu))
;;        (define-values (guard cast)
;;          (match t1-t
;;            [(Fn a _ _)
;;             (values (and$ (Type-Fn-Huh t^) (op=? a (Type-Fn-arity t^)))
;;                     (compile-fn-cast v t1 t^ l^))]
;;            [(GRef t1-t)
;;             (values (Type-GRef-Huh t^)
;;                     (compile-ref-cast v (Type t1-t) (Type-GRef-Of t^) l^))]
;;            [(GVect t1-t)
;;             (values (Type-GVect-Huh t^)
;;                     (compile-ref-cast v (Type t1-t) (Type-GVect-Of t^) l^))]
;;            [(MRef _)
;;             (values (Type-MRef-Huh t^)
;;                     (compile-mref-cast v (Type-MRef-Of t^)))]
;;            [(MVect _)
;;             (values (Type-MVect-Huh t^)
;;                     (compile-mvec-cast v (Type-MVect-Of t^)))]
;;            [(CTuple n _)
;;             (values (and$ (Type-Tuple-Huh t^) (op<=? (Type-Tuple-num t^) n))
;;                     (compile-tuple-cast v t1 t^ l^))]))
;;        (Let `([,tu . ,t2] [,lu . ,l])
;;          (If guard cast (Blame lu)))]
;;       [(t1 (Type t2-t))
;;        (define lu (next-uid! "l"))
;;        (define l^ (Var lu))
;;        (define tu (next-uid! "t2"))
;;        (define t^ (Var tu))
;;        (define-values (guard cast)
;;          (match t2-t
;;            [(Fn a _ _)
;;             (values (and$ (Type-Fn-Huh t^) (op=? (Quote a) (Type-Fn-arity t^)))
;;                     (compile-fn-cast v t^ t2 l^))]
;;            [(GRef t1-t)
;;             (values (Type-GRef-Huh t^)
;;                     (compile-ref-cast v (Type-GRef-Of t^) (Type t2-t) l^))]
;;            [(GVect t1-t)
;;             (values (Type-GVect-Huh t^)
;;                     (compile-ref-cast v (Type-GVect-Of t^) (Type t2-t) l^))]
;;            [(MRef t2-t)
;;             (values (Type-MRef-Huh t^)
;;                     (compile-mref-cast v (Type t2-t)))]
;;            [(MVect t2-t)
;;             (values (Type-MVect-Huh t^)
;;                     (compile-mvec-cast v (Type t2-t)))]
;;            [(CTuple n _)
;;             (values (and$ (Type-Tuple-Huh t^) (op<=? n (Type-Tuple-num t^)))
;;                     (compile-tuple-cast v t^ t2 l^ mt))]))
;;        (Let `([,tu . ,t2] [,lu . ,l])
;;          (If guard cast (Blame lu)))]
;;       [(t1 t2) (interp-med-cast v t1 t2 l mt)]))

;;   (define (compile-med-cast v t1 t2 l [mt ZERO-EXPR] #:know-=/= [know-=/= #f]) 
;;     ;; TODO This seems more complicated than it needs to be
;;     ;; Can we use macros to fix this?
;;     (cond
;;       [(not (Var? v))
;;        (let$ ([v v]) (compile-med-cast v t1 t2 l mt #:know-=/= know-=/=))]
;;       [(not (Var? l))
;;        (let$ ([l l]) (compile-med-cast v t1 t2 l mt #:know-=/= know-=/=))]
;;       [(not (or (Quote? mt) (Var? mt)))
;;        (let$ ([mt mt]) (compile-med-cast v t1 t2 l mt #:know-=/= know-=/=))]
;;       [(not (or (Var? t1) (Type? t1)))
;;        (let$ ([t1 t1]) (compile-med-cast v t1 t2 l mt #:know-=/= know-=/=))]
;;       [(not (or (Var? t2) (Type? t2)))
;;        (let$ ([t2 t2]) (compile-med-cast v t1 t2 l mt #:know-=/= know-=/=))]
;;       [(not know-=/=)
;;        (If (op=? t1 t2)
;;            v
;;            (compile-med-cast v t1 t2 l mt #:know-=/= #t))]
;;       [(or (Type? t1) (Type? t2)) (code-gen-med-cast v t1 t2 l mt)]
;;       [else (interp-med-cast v t1 t2 l mt)]))

;;   (define interp-cast-code
;;     (code$ (v t1 t2 l mt)
;;       (precondition$ (and$ (not$ (op=? t1 t2))
;;                            (not$ (Type-Dyn-Huh t1))
;;                            (not$ (Type-Dyn-Huh t2)))
;;         (cond$
;;          ;; All the base cases are taken care of by (= t1 t2) in compile-med-cast
;;          ;; compile-med-cast should do a check to make sure (=/= t1 t2) and can be
;;          [(and$ (Type-Fn-Huh t1)
;;                 (Type-Fn-Huh t2)
;;                 (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
;;           (compile-fn-cast v t1 t2 l)]
;;          [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
;;                 (op<=? (Type-Tuple-num t2) (Type-Tuple-num t1)))
;;           (compile-tuple-cast v t1 t2 l mt)]
;;          [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
;;           (compile-ref-cast v (Type-GRef-Of t1) (Type-GRef-Of t2) l)]
;;          [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
;;           (compile-ref-cast v (Type-GVect-Of t1) (Type-GVect-Of t2) l)]
;;          [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
;;           (compile-mref-cast v (Type-MRef-Of t1) (Type-MRef-Of t2) l)]
;;          [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
;;           (compile-mvec-cast v (Type-MRef-Of t1) (Type-MRef-Of t2) l)] 
;;          [else (Blame l)]))))

;;   (define interp-med-cast-code
;;     (code$ (v t1 t2 l)
;;       (code-gen-med-cast v t1 t2 l)))
  
;;   (values interp-cast-code interp-med-cast-code))

;; (: make-code-gen-make-coercion : Make-Coercion-Type Uid -> Make-Coercion-Type)
;; (define ((make-code-gen-make-coercion mk-crcn make-coercion-uid) t1 t2 lbl)
;;   (: help-comp :
;;      CoC3-Trivial -> (Schml-Type Schml-Type -> Schml-Coercion))
;;   (define ((help-comp lbl) t g)
;;     (let ([c : CoC3-Expr (mk-crcn (Type t) (Type g) lbl)])
;;       (unless (Quote-Coercion? c)
;;         (error 'interpret-casts-with-coercions/mk-fn-crcn$/help-comp
;;                "This should always be possible if all the code is correct"))
;;       (Quote-Coercion-const c)))
;;   (: make-make-fn-crcn-code Make-Coercion-Type)
;;   (define (make-make-fn-crcn-code t1 t2 lbl)
;;     (cond
;;       [(and (Type? t1) (Type? t2))
;;        (define-values (t1^ t2^)
;;          (values (Type-type t1) (Type-type t2)))
;;        (unless (and (Fn? t1^) (Fn? t2^))
;;          (error 'interpret-casts-with-coercions/mk-fn-crcn$
;;                 "That's impossible!!!!!"))
;;        (match-define-values
;;            ((Type (Fn _ t1-args t1-ret))
;;             (Type (Fn _ t2-args t2-ret)))
;;          (values t1^ t2^))
;;        (define arg* (map (help-comp lbl) t2-args t1-args))
;;        (define ret  ((help-comp lbl) t1-ret t2-ret))
;;        (Quote-Coercion (Fn (Fn-arity t1) arg* ret))]
;;       ;; In the two cases below we could speculatively generate
;;       ;; the code for a coercion with known arity
;;       ;;[(Type? t1)]
;;       ;;[(Type? t2)]
;;       ;; The Make-Fn-Coercion node require the ability
;;       ;; to create and initialize objects with unkown size
;;       ;; at runtime. Since this is we are currently impoverished
;;       ;; by our language we choose to implement this during
;;       ;; specify representation were the language is a little
;;       ;; more flexible.
;;       [else (Make-Fn-Coercion make-coercion-uid t1 t2 lbl)]))
;;   (let$ ([t1 t1] [t2 t2] [lbl lbl])
;;     (cond$
;;      [(op=? t1 t2) (Quote-Coercion (Identity))]
;;      [(and$ (fnT?$ t1) (fnT?$ t2))
;;       ;; This line is a little tricky because
;;       ;; unless we have actual types for this at
;;       ;; compile time we have to generate code
;;       ;; that can handle arbitry fn-arity.
;;       ;; We delegate this task to specify representation because
;;       ;; it involves safely allocating an object whos size cannot
;;       ;; be determined until run-time.
;;       (if$ (op=? (fnT-arity$ t1) (fnT-arity$ t2))
;;            (make-make-fn-crcn-code t1 t2 lbl)
;;            (Blame lbl))]
;;      [(and$ (gvect?$ t1) (gvect?$ t2))
;;       (let*$ ([gv1_of (gvect-of$ t1)]
;;               [gv2_of (gvect-of$ t2)]
;;               [read_crcn  (mk-crcn gv1_of gv2_of lbl)]
;;               [write_crcn (mk-crcn gv2_of gv1_of lbl)])
;;         (ref$ read_crcn write_crcn))]
;;      [(and$ (gref?$ t1) (gref?$ t2))
;;       (let*$ ([gr1_of (gref-of$ t1)]
;;               [gr2_of (gref-of$ t2)]
;;               [read_crcn  (mk-crcn gr1_of gr2_of lbl)]
;;               [write_crcn (mk-crcn gr2_of gr1_of lbl)])
;;         (ref$ read_crcn write_crcn))]
;;      [(and$ (mref?$ t1) (mref?$ t2))
;;       (let*$ ([t (mref-of$ t2)])
;;         (mrefC$ t))]
;;      [(and$ (mvect?$ t1) (mvect?$ t2))
;;       (let*$ ([t (mvect-of$ t2)])
;;         (mvectC$ t))]
;;      [(and$ (tupleT?$ t1) (tupleT?$ t2))
;;       (if$ (op<=? (tupleT-num$ t2) (tupleT-num$ t1))
;;            (if (and (Type? t1) (Type? t2))
;;                (let* ([t1 (Type-type t1)] [t2 (Type-type t2)])
;;                  (if (and (STuple? t1) (STuple? t2))
;;                      (let* ([t1-args (STuple-items t1)]
;;                             [t2-args (STuple-items t2)]
;;                             [item* (map (help-comp lbl) t1-args t2-args)])
;;                        (Quote-Coercion (CTuple (STuple-num t2) item*)))
;;                      (error 'make-coercion)))
;;                (Make-Tuple-Coercion make-coercion-uid t1 t2 lbl))
;;            (Blame lbl))]
;;      ;; This is absolutly necisarry
;;      ;; While Injections and Projections are never made by
;;      ;; source code coercions composition can create new
;;      ;; projections and injections.
;;      [(dyn?$ t1) (seq$ (prj$ t2 lbl) (Quote-Coercion (Identity)))]
;;      [(dyn?$ t2) (seq$ (Quote-Coercion (Identity)) (inj$ t1))]
;;      [else (fail$ lbl)])))


;; (: make-coercion (->* (String) (Boolean) (Schml-Type Schml-Type -> Schml-Coercion)))
;; (define ((make-coercion lbl
;;                         [space-efficient-normal-form?
;;                          (and (coercions-are-space-efficient?)
;;                               (not (optimize-first-order-coercions?)))])
;;          t1 t2)
;;   (define recur (make-coercion lbl (coercions-are-space-efficient?)))
;;   ;; (logging make-coercion () "t1 ~a\n\t t2 ~a\n" t1 t2)
;;   (define result : Schml-Coercion
;;     (match* (t1 t2)
;;       [(t        t) IDENTITY]
;;       [((Dyn)    t)
;;        (if space-efficient-normal-form?
;;            (Sequence (Project t lbl) (Identity))
;;            (Project t lbl))]
;;       [(t    (Dyn))
;;        (if space-efficient-normal-form?
;;            (Sequence (Identity) (Inject t))
;;            (Inject t))]
;;       [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
;;        ;; The arity check here means that all coercions have
;;        ;; the correct arrity under the static type system.
;;        ;; Notice that the argument types are reversed
;;        ;; because of contravarience of functions.
;;        (Fn n1 (map recur a2* a1*) (recur r1 r2))]
;;       ;; It seems like we could get by without other coercions
;;       [((GRef t1) (GRef t2))
;;        (Ref (recur t1 t2) (recur t2 t1))]
;;       [((GVect t1) (GVect t2))
;;        (Ref (recur t1 t2) (recur t2 t1))]
;;       [((MRef _) (MRef t2)) (MonoRef t2)]
;;       [((MVect _) (MVect t2)) (MonoVect t2)]
;;       [((STuple n1 t1*) (STuple n2 t2*)) #:when (= n1 n2)
;;        (CTuple n1 (map recur t1* t2*))]
;;       [(_ _) (Failed lbl)]))
;;   ;; (logging make-coercion () "t1 ~a\n\tt2 ~a\n\tresult ~a\n" t1 t2 result)
;;   result)

;; (define-type Compose-Coercions-Type
;;   (CoC3-Expr CoC3-Expr -> CoC3-Expr))




;; (: make-compile-cast
;;    (-> (#:compile-project Project-Type
;;         #:compile-inject  Inject-Type
;;         #:compile-fn-cast Fn-Cast-Type
;;         #:compile-tup-cast Tuple-Cast-Type
;;         #:compile-ref-cast Ref-Cast-Type
;;         #:compile-mref-cast MRef-Cast-Type
;;         #:compile-mvec-cast MVec-Cast-Type
;;         #:interp-cast       Cast-Type)
;;        Compile-Cast-Type))
;; (define ((make-compile-cast
;;           #:compile-project  compile-project
;;           #:compile-inject   compile-inject
;;           #:compile-fn-cast  compile-fn-cast
;;           #:compile-tup-cast compile-tup-cast 
;;           #:compile-ref-cast compile-ref-cast
;;           #:compile-mref-cast compile-mref-cast
;;           #:compile-mvec-cast compile-mvec-cast 
;;           #:interp-cast       interp-cast)
;;          e t1 t2 l
;;          #:t1-not-dyn [t1-not-dyn : Boolean #f]
;;          #:t2-not-dyn [t2-not-dyn : Boolean #f])
;;   (let*$ ([v e])
;;     (match* (t1 t2)
;;       [(t t) v]
;;       ;; We don't have to check not Dyn here because t1 <> t2
;;       [((Type (Dyn)) (Type _))
;;        (compile-project v t2 l)]
;;       [((Type (Dyn)) t2)
;;        (cond
;;          [t2-not-dyn (compile-project v t2 l)]
;;          [else
;;           (If (Type-Dyn-Huh t2)
;;               v
;;               (compile-project v t2 l))])]
;;       ;; We don't have to check not dyn here because t1 <> t2
;;       [((Type _) (Type (Dyn))) (compile-inject v t1)]
;;       [(t1 (Type (Dyn)))
;;        (cond
;;          [t1-not-dyn (compile-inject v t1)]
;;          [else
;;           (If (Type-Dyn-Huh t1)
;;               v
;;                   (compile-inject v t1))])]
;;       [((Type (Fn n _ _)) (Type (Fn n _ _)))
;;        (compile-fn-cast v t1 t2 l)]
;;       [((Type (STuple n _)) (Type (STuple n _)))
;;        (compile-tup-cast v t1 t2 l)]
;;       [((Type (GRef t1)) (Type (GRef t2)))
;;        (compile-ref-cast v (Type t1) (Type t2) l)]
;;       [((Type (GVect t1)) (Type (GVect t2)))
;;        (compile-ref-cast v (Type t1) (Type t2) l)]
;;       [((Type (MRef t1)) (Type (MRef t2)))
;;        (compile-mref-cast v (Type t2) l)]
;;       [((Type (MVect t1)) (Type (MVect t2)))
;;        (compile-mvec-cast v (Type t2) l)]
;;       [(t1 t2) (interp-cast v t1 t2 l)])))




;; (define (interpret-casts/coercions prgm)
;;   (match-define (Prog (list name next type) e) prgm)
;;   ;; All the implicit state of this program
;;   (define next-unique (make-unique-counter next))
  
;;   (parameterize ([current-unique-counter next-unique])
;;     ;; The ids for everything that is manditory
    
;;     ;; (define apply-med-coercion-uid
;;     ;;   (next-uid! "apply_mediating_coercion"))
    

;;     ;; (define compose-med-coercions-uid
;;     ;;   (next-uid! "compose_med_coercions"))
;;     ;; (define compose-fn-coercions-uid
;;     ;;   (next-uid! "compose-fn-coercions"))
;;     ;; (define compose-tuple-coercions-uid
;;     ;;   (next-uid! "compose-tuple-coercions"))
;;     ;; (define make-coercion-uid
;;     ;;   (next-uid! "make_coercion"))
;;     ;; (define make-med-coercion-uid
;;     ;;   (next-uid! "make_med_coercion"))
;;     ;; (define greatest-lower-bound-uid
;;     ;;   (next-uid! "greatest_lower_bound"))
;;     ;; (define copy-mref-uid
;;     ;;   (next-uid! "copy_mref_value"))
;;     ;; (define inject-uid
;;     ;;   (next-uid "inject"))
;;     ;; (define med-cast-uid
;;     ;;   (next-uid "med-cast"))
;;     ;; ;; Generate calls to runtime procedures
    
;;     ;; (define interp-compose-fn-coercions
;;     ;;   : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;;   (apply-code compose-fn-coercions-uid))
;;     ;; (define interp-compose-tuple-coercions
;;     ;;   : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;;   (apply-code compose-tuple-coercions-uid))
;;     ;; (define interp-compose-med-coercions : Compose-Coercions-Type
;;     ;;   (apply-code compose-med-coercions-uid))
;;     ;; (define interp-make-coercion : Interp-Make-Coercion-Type
;;     ;;   (apply-code make-coercion-uid))
;;     ;; (define interp-make-med-coercion : Interp-Make-Coercion-Type
;;     ;;   (apply-code make-med-coercion-uid))
;;     ;; (define interp-greatest-lower-bound : Greatest-Lower-Bound-Type
;;     ;;   (apply-code greatest-lower-bound-uid))
;;     ;; (define interp-copy-mref : Copy-Mref-Type
;;     ;;   (apply-code copy-mref-uid))
    
;;     ;; (: interp-apply-med-coercion : Interp-Cast-Type)
;;     ;; (define (interp-med-coercion e c [m (Quote 0)])
;;     ;;   (((inst apply-code CoC3-Expr) apply-med-coercion-uid) e c m))

;;     ;; (: interp-inject : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;; (define (interp-inject e t)
;;     ;;   (((inst apply-code CoC3-Expr) inject-uid) e t))


;;     ;; Combine two coercions (c1 : A => B) (c2 : B => C) by calling a runtime function
;;     (define compose-coercions-uid (next-uid! "compose_coercions"))
;;     (define interp-compose-coercions : Compose-Coercions-Type
;;       (apply-code compose-coercions-uid))
    
;;     ;; Cast by calling a function that casts by interpreting the types until
;;     ;; a coercion has to be residualized for a higher-order cast (fn or pref)
;;     (define cast-uid (next-uid "cast"))
;;     (: interp-cast Cast-Type)
;;     (define (interp-cast e t1 t2 l [mt ZERO-EXPR])
;;       (((inst apply-code CoC3-Expr) cast-uid) e t1 t2 l mt))

;;     ;; Cast by calling a function that interprets a coercion this function
;;     ;; could possibly call the "interp-cast" runtime function based on the
;;     ;; parameter hybrid-cast/coercion-interp
;;     (define apply-coercion-uid (next-uid! "apply_coercion"))
;;     (: interp-apply-coercion Apply-Coercion-Type)
;;     (define (interp-apply-coercion e c [m (Quote 0)])
;;       ;; TODO: Quote 0 is implicitly the null value later on this is a little hacky
;;       (((inst apply-code CoC3-Expr) apply-coercion-uid) e c m))

;;     (define project-uid (next-uid! "project"))
;;     (: interp-project Project-Type)
;;     (define (interp-project e t2 l)
;;       (((inst apply-code CoC3-Expr) project-uid) e t2 l))
    
;;     ;; (: interp-med-cast : CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     ;; (define (interp-med-cast e t1 t2 l [m (Quote 0)])
;;     ;;   (((inst apply-code CoC3-Expr) med-cast-uid) e c m))
    
    
;;     ;; (define (compile-make-coercion t1 t2 l)
;;     ;;   (cond
;;     ;;     [(and (Type? t1) (Type? t2) (Quote? l) (String? (Quote-datum l)))
;;     ;;      (Quote-Coercion ((make-coercion (Quote-datum l)) (Type-type t1) (Type-type t2)))]
;;     ;;     [else (interp-make-coercion t1 t2 l)]))

    
    
   
    
;;     ;; ;; Cast Based Code Generators
;;     ;; ;; Casting Runtime that interprets types unless a coercion has to be made
;;     ;; ;; fn-casts and proxied reference casts result in a call to make-coercion
;;     ;; ;; This code is entered from projections and monotonic coercion application
;;     ;; ;; essentially whenever a coercions application results in a call to make coercion
;;     ;; ;; followed by an immediate apply-coercion.
;;     ;; (define cast-based-project-uid (next-uid "project-with-cast"))
;;     ;; (define (interp-cast-based-project e t2 l)
;;     ;;   (((inst apply-code CoC3-Expr) interp-cast-based-project-uid) e t2 l))
;;     ;; (define code-gen-cast-based-project
;;     ;;   (make-code-gen-project
;;     ;;    #:interp-project interp-cast-based-project
;;     ;;    #:interp-med-cast interp-med-cast))
;;     ;; (define (compile-cast-based-project e t2 l)
;;     ;;   (cond
;;     ;;     [(Type? t2) (code-gen-cast-based-project e t2 l)]
;;     ;;     [else (interp-cast-based-project e t2 l)]))
;;     ;; (define interp-cast-based-project-code
;;     ;;   (code$ (e t2 l) (code-gen-cast-based-project e t2 l)))

;;     ;; (define (compile-inject e t1)
;;     ;;   (cond
;;     ;;     [(Type? t1) (dyn-make$ e t1)]
;;     ;;     [else (interp-inject e t1)]))

;;     ;; ;; Fn-Cast and Ref-Cast have to "Fall-Back" to coercions to get space-efficiency
;;     ;; (define (compile-cast-based-fn-cast e t1 t2 l)
;;     ;;   (code-gen-apply-fn-coercion e (compile-make-coercion t1 t2 l)))

;;     ;; (: code-gen-apply-ref-coercion Apply-Coercion-Type)
;;     ;; (define code-gen-apply-ref-coercion
;;     ;;   (make-code-gen-apply-ref-coercion
;;     ;;    #:compose-coercions interp-compose-coercions
;;     ;;    #:id-coercion-huh Id-Coercion-Huh))

;;     ;; (define (compile-cast-based-ref-cast e t1 t2 l)
;;     ;;   (code-gen-apply-ref-coercion e (compile-make-coercion t1 t2 l)))

;;     ;; (: code-gen-mref-cast Monotonic-Cast-Type)
;;     ;; (define code-gen-mref-cast
;;     ;;   (make-code-gen-mref-cast
;;     ;;    #:compile-cast interp-cast
;;     ;;    #:greatest-lower-bound greatest-lower-bound))
    
;;     ;; (: code-gen-mvec-cast Monotonic-Cast-Type)
;;     ;; (define code-gen-mvec-cast
;;     ;;   (make-code-gen-mvec-cast
;;     ;;    #:compile-cast interp-cast
;;     ;;    #:greatest-lower-bound greatest-lower-bound))
    
;;     ;; (define code-gen-tuple-cast
;;     ;;   (make-code-gen-tuple-cast
;;     ;;    #:cast-uid cast-uid))
    
;;     ;; (define-values (code-gen-cast     cast-code     compile-cast
;;     ;;                 code-gen-med-cast med-cast-code compile-med-cast)
;;     ;;   (make-cast-helpers
;;     ;;    #:interp-cast interp-cast
;;     ;;    #:interp-med-cast interp-med-cast
;;     ;;    #:project compile-cast-based-project
;;     ;;    #:inject  compile-inject
;;     ;;    #:fn-cast compile-cast-based-fn-cast
;;     ;;    #:tuple-cast code-gen-tuple-cast
;;     ;;    #:ref-cast   code-gen-ref-cast
;;     ;;    #:mref-cast  code-gen-mref-cast
;;     ;;    #:mvec-cast  code-gen-mvec-cast))

;;     ;; (define code-gen-project
;;     ;;   (make-code-gen-projection
;;     ;;    #:interp-project  interp-project
;;     ;;    #:med-cast        interp-med-cast))
    
;;     ;; (: interp-project-code CoC3-Code)
;;     ;; (define interp-project-code
;;     ;;   ;; Using variables result in generating the generic code that is constant but large
;;     ;;   (code$ (v t2 l)
;;     ;;     (code-gen-project v t2 l)))


;;     ;; (: interp-inject-code CoC3-Code)
;;     ;; (define interp-inject-code
;;     ;;   (code$ (v t1)
;;     ;;     (code-gen-inject v t1)))
        
;;     ;; (: code-gen-apply-tuple-coercion Apply-Tuple-Coercion-Type)
;;     ;; (define code-gen-apply-tuple-coercion
;;     ;;   (make-code-gen-apply-tuple-coercion
;;     ;;    #:apply-coercion-uid apply-coercion-uid))

    

;;     ;; (define apply-coercion-code
;;     ;;   ;; I am pretty sure that this code should never exist
;;     ;;   ;; The cast code should be used unless a coercion is needed to save the cast
;;     ;;   
;;     ;;   (match c
;;     ;;     [(Coercion (Identity)) v]
;;     ;;     [(Coercion (Seq c1 c2))
;;     ;;      (let$ ([v (code-gen-apply-coercion v (Coercion c1) mt)])
;;     ;;        (code-gen-apply-coercion v (Coercion c2) mt))]
;;     ;;     [(Coercion (Project t2 l)) (code-gen-project v (Type t2) (Quote l))]
;;     ;;     [(Coercion (Inject t1)) (code-gen-inject v (Type t1))]
;;     ;;     [(Coercion (Fn _ _ _)) (code-gen-apply-fn-coercion v c)]
;;     ;;     [(Coercion (Ref _ _)) (code-gen-apply-ref-coercion v c)]
;;     ;;     [(Coercion (MonoRef t2)) (code-gen-mref-cast v (Type t2))]
;;     ;;     [(Coercion (MonoVect t2)) (code-gen-mvec-cast v (Type t2))]
;;     ;;     [(Coercion (CTuple _ _)) (code-gen-apply-tuple-coercion v c mt)]
;;     ;;     [(Coercion (Failed l)) (Blame (Quote l))]
;;     ;;     [other
;;     ;;      (let$ ([v v] [c c] [mt mt])
;;     ;;        )])
;;     ;;   (code$ (v c mt)
;;     ;;     (cond$
;;     ;;      [(Id-Coercion-Huh c) v]
;;     ;;      [(Sequence-Coercion-Huh c)
;;     ;;       (let*$ ([seq_fst (Sequence-Coercion-Fst c)]
;;     ;;               [seq_snd (Sequence-Coercion-Snd c)]
;;     ;;               [v (interp-apply-coercion v seq_fst mt)])
;;     ;;         (interp-apply-coercion v seq_snd mt))]
;;     ;;      [(Project-Coercion-Huh c)
;;     ;;       (code-gen-project-cast v (Project-Coercion-Type c) (Project-Coercion-Label c))]
;;     ;;      [(Inject-Coercion-Huh c)
;;     ;;       (code-gen-injection v (Inject-Coercion-Type c))]
;;     ;;      [(Mediating-Coercion-Huh c)
;;     ;;       (cond$
;;     ;;        [(Fn-Coercion-Huh c) (code-gen-apply-fn-coercion v c)]
;;     ;;        [(Ref-Coercion-Huh c) (code-gen-apply-ref-coercion v c)]
;;     ;;        [(MRef-Coercion-Huh c) (code-gen-mref-cast v (MRef-Coercion-Type c))]
;;     ;;        [(MVect-Coercion-Huh c) (code-gen-mvec-cast v (MRef-Coercion-Type c))]
;;     ;;        [(Tuple-Coercion-Huh c) (code-gen-apply-tuple-coercion v c mt)]
;;     ;;        [else (Blame (Quote "bad implemention of mediating coercions"))])]
;;     ;;      ;; the coercion must be failure
;;     ;;      [else (precondition$ (Failed-Coercion-Huh c)
;;     ;;              (Blame (Failed-Coercion-Label c)))])))
    
;;     ;; (: interp-apply-coercion-code CoC3-Code)
;;     ;; (define interp-apply-coercion-code
;;     ;;   (code$ (v c mt)
;;     ;;     (code-gen-apply-coercion v c mt)))    

    
;;     ;; ;; Cunstruct a hyper-coercions expression from the type and blame
;;     ;; ;; information contained in a type-based cast.
;;     ;; ;; This coercion literal is eligable to be hoisted in a later
;;     ;; ;; pass so that it only ever is creates a value once. 
;;     ;; (: make-coercion : Make-Coercion-Type)
;;     ;; (define (make-coercion t1 t2 l)
;;     ;;   (: recur : Schml-Type Schml-Type -> Hyper-Coercion)
;;     ;;   (define (recur t1 t2)
;;     ;;     (match* (t1 t2)
;;     ;;       [(t        t) (HC #f t #f #f t IDENTITY)]
;;     ;;       ;; These two lines create an invarient that the only
;;     ;;       ;; time t1 or t2 is Dyn the entire hyper-coercion is an identity 
;;     ;;       [((Dyn)    t) (HC #t t l  #f t IDENTITY)]
;;     ;;       [(t    (Dyn)) (HC #f t #f #t t IDENTITY)]
;;     ;;       [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
;;     ;;        ;; The arity check here means that all coercions have
;;     ;;        ;; the correct arity under the static type system.
;;     ;;        ;; Notice that the argument types are reversed
;;     ;;        ;; because of contravarience of functions.
;;     ;;        (HC #f t1 #f #f t2 (Fn n1 (map recur a2* a1*) (recur r1 r2)))]
;;     ;;       [((STuple n1 t1*) (STuple n2 t2*)) #:when (= n1 n2)
;;     ;;        (HC #f t1 #f #f t2 (CTuple n1 (map recur t1* t2*)))]
;;     ;;       [((GRef t1) (GRef t2))
;;     ;;        (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
;;     ;;       [((GVect t1) (GVect t2))
;;     ;;        (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
;;     ;;       [((MRef _) (MRef t2))
;;     ;;        (HC #f t1 #f #f t2 (MonoRef t2))]
;;     ;;       [((MVect _) (MVect t2))
;;     ;;        (HC #f t1 #f #f t2 (MonoVect t2))]
;;     ;;       [(t1 t2)
;;     ;;        (HC #f t1 #f #f t2 (Failed l))]))
;;     ;;   (Quote-HCoercion (recur t1 t2)))
;;     ;; (: interp-make-coercion-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-make-coercion-code
;;     ;;   (define-values (t1u t2u lu)
;;     ;;       (values (next-uid! "t1") (next-uid! "t2") (next-uid! "blame_info")))
;;     ;;   (define-values (t1 t2 l)
;;     ;;       (values (Var t1u) (Var t2u) (Var lu)))
;;     ;;   (code$ (t1 t2 l)
;;     ;;     (cond$
;;     ;;      [(op=? t1 t2)
;;     ;;       (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t1 Id) CoC3-Expr)]
;;     ;;      ;; This is absolutly necisarry
;;     ;;      ;; While Injections and Projections are never made by
;;     ;;      ;; source code coercions composition can create new
;;     ;;      ;; projections and injections.
;;     ;;      [(Type-Dyn-Huh t1)
;;     ;;       (ann (HC (Quote #t) t2 l (Quote #f) t2 Id) CoC3-Expr)]
;;     ;;      [(Type-Dyn-Huh t2)
;;     ;;       (ann (HC (Quote #f) t1 (Quote #f) (Quote #t) t1 Id) CoC3-Expr)]
;;     ;;      [else
;;     ;;       (let$ ([med (interp-make-med-coercion t1 t2 l)]) 
;;     ;;         (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t2 med) CoC3-Expr))])))
;;     ;; (: interp-make-med-coercion-code
;;     ;;    (Code Uid* CoC3-Expr))
;;     ;; (define interp-make-med-coercion-code
;;     ;;   ;; precondition: t1 != Dyn /\ t2 != Dyn /\ t1 != t2
;;     ;;   ;; Should be called make_non_id_med_coercion
;;     ;;   (code$ (t1 t2 l)
;;     ;;     (precondition$ (not$ (and$ (op=? t1 t2) (Type-Dyn-Huh t1) (Type-Dyn-Huh t2)))
;;     ;;       (cond$
;;     ;;        [(and$ (Type-Fn-Huh t1) (Type-Fn-Huh t2) 
;;     ;;               (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
;;     ;;         ;; This line is a little tricky because unless we have actual
;;     ;;         ;; types for this at compile time we have to generate code that
;;     ;;         ;; can handle arbitry fn-arity.  We delegate this task to specify
;;     ;;         ;; representation because it involves safely allocating an object
;;     ;;         ;; whos size cannot be determined until run-time.
;;     ;;         (Make-Fn-Coercion interp-make-coercion-uid t1 t2 l)]
;;     ;;        [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
;;     ;;               (op<=? (tupleT-num$ t2) (tupleT-num$ t1)))
;;     ;;         (Make-Tuple-Coercion interp-make-coercion-uid t1 t2 l)]
;;     ;;        [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
;;     ;;         (let*$ ([gv1_of (Type-GVect-Of t1)]
;;     ;;                 [gv2_of (Type-GVect-Of t2)]
;;     ;;                 [write_crcn (interp-make-coercion gv2_of gv1_of l)]
;;     ;;                 [read_crcn  (interp-make-coercion gv1_of gv2_of l)])
;;     ;;           (Ref-Coercion read_crcn write_crcn))]
;;     ;;        [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
;;     ;;         (let*$ ([gr1_of (Type-GRef-Of t1)]
;;     ;;                 [gr2_of (Type-GRef-Of t2)]
;;     ;;                 [write_crcn (interp-make-coercion gr2_of gr1_of l)]
;;     ;;                 [read_crcn  (interp-make-coercion gr1_of gr2_of l)])
;;     ;;           (Ref-Coercion read_crcn write_crcn))]
;;     ;;        ;; TODO should these two line be (glb t1 t2)? 
;;     ;;        [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
;;     ;;         (MRef-Coercion (mref-of$ t2))]
;;     ;;        [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
;;     ;;         (MVect-Coercion (mvect-of$ t2))]
;;     ;;        [else (Failed-Coercion l)]))))

;;     ;; (: interp-compose-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-coercions-code
;;     ;;   (code$ (fst snd)
;;     ;;     (precondition$
;;     ;;         ;; all sequences of coercions to compose are well typed because:
;;     ;;         ;; either the inner types are well-typed or the coercions are
;;     ;;         ;; some combination injections projections and dynamic
;;     ;;         ;; identities that are well typed
;;     ;;         (or$ (op=? (HC-T2 fst) (HC-T1 snd))
;;     ;;              (and$ (or$ (Type-Dyn-Huh (HC-T2 fst)) (HC-Inject-Huh fst))
;;     ;;                    (or$ (Type-Dyn-Huh (HC-T1 snd)) (HC-Project-Huh snd))))
;;     ;;       (let*$ ([fst-t2 (HC-T2 fst)]
;;     ;;               [snd-t1 (HC-T1 snd)])
;;     ;;         (cond$
;;     ;;          ;; These first two cases rule out dynamic identity casts
;;     ;;          ;; ie (HC #f Dyn #f #f Dyn ID)
;;     ;;          ;; These cannot be composed under the following rules because
;;     ;;          ;; the usual invarient of all code following is that Dyn
;;     ;;          ;; isn't present as t1 or t2 in any coercion.
;;     ;;          [(HC-Identity-Huh fst) snd]
;;     ;;          [(HC-Identity-Huh snd) fst]
;;     ;;          [else
;;     ;;           (let*$ ([fst-med (HC-Med fst)]
;;     ;;                   [mid-med
;;     ;;                    (cond$
;;     ;;                     [(and$ (HC-Inject-Huh fst)
;;     ;;                            (HC-Project-Huh snd)
;;     ;;                            ;; interp-make-med-coercion assumes that fst-t2 != snd-t1
;;     ;;                            (not$ (op=? fst-t2 snd-t1)))
;;     ;;                      (let*$ ([mid (interp-make-med-coercion
;;     ;;                                    fst-t2 snd-t1 (HC-Label snd))])
;;     ;;                        ;; we know these won't be Id
;;     ;;                        (interp-compose-med-coercions fst-med mid))]
;;     ;;                     [else fst-med])]
;;     ;;                   [snd-med (HC-Med snd)]
;;     ;;                   [fnl-med
;;     ;;                    ;; consider trying the id-checks here
;;     ;;                    (cond$
;;     ;;                       [(Id-Coercion-Huh fst-med) snd-med]
;;     ;;                       [(Id-Coercion-Huh snd-med) fst-med]
;;     ;;                       [else (interp-compose-med-coercions mid-med snd-med)])
;;     ;;                    (interp-compose-med-coercions mid-med snd-med)])
;;     ;;             (HC (HC-Project-Huh fst) (HC-T1 fst) (HC-Label fst) 
;;     ;;                 (HC-Inject-Huh snd) (HC-T2 snd)
;;     ;;                 fnl-med))])))))

;;     ;; (: interp-compose-med-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-med-coercions-code
;;     ;;   (code$ (fst snd)
;;     ;;     (cond$
;;     ;;      ;; TODO consider specializing this code
;;     ;;      ;; by moving the Id-coercion-huh calls to
;;     ;;      ;; before each call to compose-med.
;;     ;;      [(Id-Coercion-Huh fst) snd]
;;     ;;      [(Id-Coercion-Huh snd) fst]
;;     ;;      [(and$ (Mediating-Coercion-Huh fst) (Mediating-Coercion-Huh snd))
;;     ;;       (cond$
;;     ;;        ;; Ditching the second check doesn't actually decrease the
;;     ;;        ;; number of checks once we move the failure casts into
;;     ;;        ;; mediating casts. 
;;     ;;        [(and$ (Fn-Coercion-Huh fst) (Fn-Coercion-Huh snd))
;;     ;;         (interp-compose-fn-coercions
;;     ;;          fst snd (Quote 0) (Fn-Coercion-Arity fst) (Quote #t))]
;;     ;;        [(and$ (Tuple-Coercion-Huh fst) (Tuple-Coercion-Huh snd))
;;     ;;         (interp-compose-tuple-coercions
;;     ;;          fst snd (Quote 0) (Tuple-Coercion-Num fst) (Quote #t))]
;;     ;;        [(and$ (Ref-Coercion-Huh fst) (Ref-Coercion-Huh snd))
;;     ;;         (let*$ ([fst-write (Ref-Coercion-Write fst)]
;;     ;;                 [snd-write (Ref-Coercion-Write snd)]
;;     ;;                 [com-write (interp-compose-coercions snd-write fst-write)]
;;     ;;                 [fst-read  (Ref-Coercion-Read fst)]
;;     ;;                 [snd-read  (Ref-Coercion-Read snd)]
;;     ;;                 [com-read  (interp-compose-coercions fst-read snd-read)])
;;     ;;           (If (and$ (HC-Identity-Huh com-read)
;;     ;;                     (HC-Identity-Huh com-write))
;;     ;;               (Quote-Coercion IDENTITY)
;;     ;;               (Ref-Coercion com-read com-write)))]
;;     ;;        [(and$ (MRef-Coercion-Huh fst) (MRef-Coercion-Huh snd))
;;     ;;         (let*$ ([fst_type  (MRef-Coercion-Type fst)]
;;     ;;                 [snd_type  (MRef-Coercion-Type snd)]
;;     ;;                 [glb       (greatest-lower-bound fst_type snd_type)])
;;     ;;           (MRef-Coercion glb))]
;;     ;;        [(and$ (MVect-Coercion-Huh fst) (MVect-Coercion-Huh snd))
;;     ;;         (let*$ ([fst_type  (MVect-Coercion-Type fst)]
;;     ;;                 [snd_type  (MVect-Coercion-Type snd)]
;;     ;;                 [glb       (greatest-lower-bound fst_type snd_type)])
;;     ;;           (MVect-Coercion glb))]
;;     ;;        [else
;;     ;;         (Blame (Quote "Internal Error: compose-mediating-coercions 1"))])]
;;     ;;      [(Failed-Coercion-Huh fst)
;;     ;;       (If (Failed-Coercion-Huh snd)
;;     ;;           fst ;; merge blame info for bidirectional behavior
;;     ;;           fst)]
;;     ;;      [(Failed-Coercion-Huh snd) snd]
;;     ;;      [else
;;     ;;       (Blame (Quote "Internal Error: compose-mediating-coercion 2"))])))

;;     ;; (: interp-compose-fn-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-fn-coercions-code
;;     ;;   ;; TODO Implement Fn-coercion-set! Make-Fn-Coercion
;;     ;;   ;; TODO make this iterative
;;     ;;   (code$ (c1 c2 i a was-id)
;;     ;;     (cond$
;;     ;;      [(Op '= `(,i ,a))
;;     ;;       (let*$ ([r1 (Fn-Coercion-Return c1)]
;;     ;;               [r2 (Fn-Coercion-Return c2)]
;;     ;;               [cr (interp-compose-coercions r1 r2)])
;;     ;;         (cond$
;;     ;;          [(and$ was-id (HC-Identity-Huh cr)) (Quote-Coercion (Identity))]
;;     ;;          [else 
;;     ;;           (let$ ([fnc (Id-Fn-Coercion a)])
;;     ;;             (Fn-Coercion-Return-Set! fnc cr)
;;     ;;             fnc)]))]
;;     ;;      [else
;;     ;;       (let*$ ([i2 (Fn-Coercion-Arg c2 i)]
;;     ;;               [i1 (Fn-Coercion-Arg c1 i)]
;;     ;;               [ca (interp-compose-coercions i2 i1)]
;;     ;;               [is-id (and$ was-id (HC-Identity-Huh ca))]
;;     ;;               [next-i (Op '+ `(,i ,(Quote 1)))]
;;     ;;               [m  (interp-compose-fn-coercions c1 c2 next-i a is-id)])
;;     ;;         (cond$
;;     ;;          [(Id-Coercion-Huh m) m]
;;     ;;          [else (Fn-Coercion-Arg-Set! m i ca) m]))])))

;;     ;; (: interp-compose-tuple-coercions-code (Code Uid* CoC3-Expr))
;;     ;; (define interp-compose-tuple-coercions-code
;;     ;;   ;; TODO make this iterative 
;;     ;;   (code$ (c1 c2 i a was-id)
;;     ;;     (cond$
;;     ;;      [(Op '= `(,i ,a))
;;     ;;       (cond$
;;     ;;        [was-id (Quote-Coercion (Identity))]
;;     ;;        [else (Id-Tuple-Coercion a)])]
;;     ;;      [else
;;     ;;       (let*$ ([e1 (Tuple-Coercion-Item c1 i)]
;;     ;;               [e2 (Tuple-Coercion-Item c2 i)]
;;     ;;               [ce (interp-compose-coercions e1 e2)]
;;     ;;               [is-id (and$ was-id (HC-Identity-Huh ce))]
;;     ;;               [new-i (Op '+ `(,(Quote 1) ,i))]
;;     ;;               [m  (interp-compose-tuple-coercions c1 c2 new-i a is-id)])
;;     ;;         (cond$
;;     ;;          [(Id-Coercion-Huh m) m]
;;     ;;          [else (Tuple-Coercion-Item-Set! m i ce)]))])))

;;     ;; (define greatest-lower-bound-code
;;     ;;   (code$ (t1 t2)
;;     ;;     ((gen-greatest-lower-bound-type-code
;;     ;;       next-uid!
;;     ;;       greatest-lower-bound
;;     ;;       greatest-lower-bound-uid)
;;     ;;      t1 t2)))

;;     ;; (define copy-mref-code
;;     ;;   (code$ (mref)
;;     ;;     ((gen-copy-value-in-monoref-code next-uid!) mref)))

;;     ;; (: compile-make-coercion Compile-Make-Coercion-Type)
;;     ;; ;; TODO : can this be any better?
;;     ;; (define (compile-make-coercion t1 t2 l)
;;     ;;   (match* (t1 t2 l)
;;     ;;     [((Type t1) (Type t2) (Quote (? blame-label? l)))
;;     ;;      (make-coercion t1 t2 l)]
;;     ;;     [(t1 t2 l) (interp-make-coercion t1 t2 l)]))

;;     ;; TODO should project always code-gen?
;;     ;; Should have the option on invoking interp-cast / interp-apply-coercion
;;     (define compile-project 
;;       (error 'todo))
;;     (define compile-inject
;;       (error 'todo))
;;     (define compile-fn-cast
;;       (error 'todo))
;;     (define compile-ref-cast
;;       (error 'todo))
;;     (define compile-tup-cast
;;       (error 'todo))
;;     (define compile-mref-cast
;;       (error 'todo))
;;     (define compile-mvec-cast
;;       (error 'todo))
    
;;     ;; Generic specialization of source level casts
;;     (define compile-cast
;;       (make-compile-cast
;;        #:compile-project   compile-project
;;        #:compile-inject    compile-inject
;;        #:compile-fn-cast   compile-fn-cast
;;        #:compile-tup-cast  compile-tup-cast 
;;        #:compile-ref-cast  compile-ref-cast
;;        #:compile-mref-cast compile-mref-cast
;;        #:compile-mvec-cast compile-mvec-cast
;;        ;; There shouldn't be any casts that actually fall into this category.
;;        #:interp-cast       interp-cast))
    
;;     (: get-fn-cast! : Nat -> Uid)
;;     (: get-fn-cast-bindings! : -> CoC3-Bnd-Code*)
;;     (define-values (get-fn-cast! get-fn-cast-bindings!)
;;       (make-fn-cast-helpers
;;        (make-build-caster/coercions
;;         #:apply-coercion-uid cast-uid
;;         #:compose-coercions  interp-compose-coercions
;;         #:id-coercion-huh    Id-Coercion-Huh)))
    
;;     (: compile-lambda Compile-Lambda-Type)
;;     (define (compile-lambda f* expr)
;;       (let ([caster (get-fn-cast! (length f*))])
;;         (Lambda f* (Castable caster expr))))
    
;;     (: compile-app Compile-App-Type)
;;     (define (compile-app e e*)
;;       (App-Fn-or-Proxy apply-coercion-uid e e*))
   
;;     (: pbox-ref  PBox-Ref-Type)
;;     (: pbox-set! PBox-Set-Type)
;;     (: pvec-ref  PVec-Ref-Type)
;;     (: pvec-set! PVec-Set-Type)
;;     (: pvec-len  PVec-Len-Type)
;;     (: proxied-operations-bindings CoC3-Bnd-Code*)
;;     (define-values (pbox-ref pbox-set!
;;                     pvec-ref pvec-set! pvec-len
;;                     proxied-operations-bindings)
;;       (make-proxied-reference/coercions-compile-helpers
;;        #:apply-coercion interp-cast))
    
;;     (: mbox-set! MBox-Set-Type)
;;     (: mbox-ref MBox-Ref-Type)
;;     (: mvec-ref MVec-Ref-Type)
;;     (: mvec-set! MVec-Set-Type)
;;     (: monotonic-operations-bindings CoC3-Bnd-Code*)
;;     (define-values (mbox-ref mbox-set! mvec-ref mvec-set! monotonic-operations-bindings)
;;       (make-monotonic-reference/coercions-compile-helpers
;;        ;; TODO we should be able to handle mvecs and mrefs seperately
;;        ;; because the sharing between mvecs could mean coercions are a win whereas
;;        ;; it seems obvious that we would want to interp-cast for mref
;;        #:interp-cast interp-cast))
    
;;     (: dyn-pbox-ref Dyn-PBox-Ref-Type)
;;     (: dyn-pbox-set! Dyn-PBox-Set-Type)
;;     (: dyn-pvec-ref Dyn-PVec-Ref-Type)
;;     (: dyn-pvec-set! Dyn-PVec-Set-Type)
;;     (: dyn-pvec-len Dyn-PVec-Len-Type)
;;     (: dyn-mbox-ref Dyn-MBox-Ref-Type)
;;     (: dyn-mbox-set! Dyn-MBox-Set-Type)
;;     (: dyn-mvec-ref Dyn-MVec-Ref-Type)
;;     (: dyn-mvec-set! Dyn-MVec-Set-Type)
;;     (: dyn-fn-app Dyn-Fn-App-Type)
;;     (: dyn-tup-prj Dyn-Tup-Prj-Type)
;;     (: dynamic-operations-bindings CoC3-Bnd-Code*)
;;     (define-values (dyn-pbox-ref dyn-pbox-set!
;;                     dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
;;                     dyn-mbox-ref dyn-mbox-set!
;;                     dyn-mvec-ref dyn-mvec-set!
;;                     dyn-fn-app dyn-tup-prj
;;                     dynamic-operations-bindings)
;;       (make-dynamic-operations/coercions-compile-helpers
;;        #:cast compile-cast #:compile-app compile-app
;;        #:pbox-ref pbox-ref #:pbox-set pbox-set!
;;        #:pvec-ref pvec-ref #:pvec-set pvec-set! #:pvec-len pvec-len
;;        #:mbox-ref mbox-ref #:mbox-set mbox-set!
;;        #:mvec-ref mvec-ref #:mvec-set mvec-set!))
    
;;     (define-type Get-Cast-Code-Bindings-Type (-> CoC3-Bnd-Code*))
;;     (: get-cast-code-bindings! Get-Cast-Code-Bindings-Type)
;;     (define (get-cast-code-bindings!)
;;       (append
;;        (get-fn-cast-bindings!)
;;        proxied-operations-bindings
;;        monotonic-operations-bindings
;;        dynamic-operations-bindings
;;        `([,interp-projection-uid . ,interp-projection-code]
;;          [,interp-cast-uid . ,interp-cast-code]
;;          [,interp-med-cast-uid . ,interp-med-cast-code]
;;          [,interp-make-coercion-uid . ,interp-make-coercion-code]
;;          [,interp-make-med-coercion-uid . ,interp-make-med-coercion-code]
;;          [,interp-compose-coercions-uid . ,interp-compose-coercions-code]
;;          [,interp-compose-med-coercions-uid
;;           . ,interp-compose-med-coercions-code]
;;          [,interp-compose-fn-coercions-uid
;;           . ,interp-compose-fn-coercions-code]
;;          [,interp-compose-tuple-coercions-uid
;;           . ,interp-compose-tuple-coercions-code]
;;          [,greatest-lower-bound-uid . ,greatest-lower-bound-code]
;;          [,copy-mref-uid . ,copy-mref-code])))


;;     (define map-expr!
;;       (make-map-expr
;;        #:compile-cast compile-cast
;;        #:compile-lambda compile-lambda
;;        #:compile-app compile-app
;;        #:pbox-ref pbox-ref
;;        #:pbox-set pbox-set!
;;        #:pvec-ref pvec-ref
;;        #:pvec-set pvec-set!
;;        #:pvec-len pvec-len
;;        #:mbox-ref mbox-ref
;;        #:mbox-set mbox-set!
;;        #:mvec-ref mvec-ref
;;        #:mvec-set mvec-set!
;;        #:dyn-pbox-ref dyn-pbox-ref
;;        #:dyn-pbox-set dyn-pbox-set!
;;        #:dyn-pvec-ref dyn-pvec-ref
;;        #:dyn-pvec-set dyn-pvec-set!
;;        #:dyn-pvec-len dyn-pvec-len
;;        #:dyn-mbox-ref dyn-mbox-ref
;;        #:dyn-mbox-set dyn-mbox-set!
;;        #:dyn-mvec-ref dyn-mvec-ref
;;        #:dyn-mvec-set dyn-mvec-set!
;;        #:dyn-fn-app dyn-fn-app
;;        #:dyn-tup-prj dyn-tup-prj))

;;     ;; This map-expr! call must occur before next-unique is
;;     ;; extracted because new unique variable are allocated in
;;     ;; this pass. Additionally calls to get-fn-caster! will
;;     ;; cause additional code to be generated for each arity
;;     ;; of function that exists; this code is extracted from
;;     ;; fn-casts.
;;     (define new-e (map-expr! e))

;;     ;; Reconstruct the entire program
;;     (Prog (list name (unique-counter-next! next-unique) type)
;;       (Labels (get-cast-code-bindings!) new-e))))


;; #;(: interpret-casts/coercions : Cast0.5-Lang ->  Cast-or-Coerce3-Lang)
;; #;
;; (define (interpret-casts/coercions prgm)
;;   (define cprgm (casts->coercions prgm))
;;   (define fprgm (lower-function-casts cprgm))
;;   ;; Desugaring the input program into its constituents 
;;   (match-define (Prog (list prgm-name prgm-next prgm-type) prgm-exp)
;;     fprgm)
  
;;   (define unique-counter (make-unique-counter prgm-next))

;;   (parameterize ([current-unique-counter unique-counter])
    
;;     (define open-coded? : (Symbol -> Boolean)
;;       (if (specialize-cast-code-generation?)
;;           (let ([open-coded-set  (set 'make-coercion 'interp-cast)])
;;             (lambda ([x : Symbol])
;;               (set-member? open-coded-set x)))
;;           (lambda (x) #f)))
  
;;     ;; First we generate names for everything in the runtime
;;     ;; and procedure that will either generate code to perform
;;     ;; and action or call off to one of the runtime functions
;;     ;; we are generating.
  
;;     ;; The runtime label for the runtime coercion interpreter
;;     (define interp-coercion-uid (next-uid! "interp_coercion"))

;;     (: interp-coercion-call : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     (define interp-coercion-call (apply-code interp-coercion-uid))

;;     ;; The runtime label for the runtime casts interpreter
;;     (define interp-cast-uid (next-uid! "interp_cast"))

;;     (: interp-cast-call : Cast-With-MAddr-Type)
;;     (define interp-cast-call (apply-code interp-cast-uid))

;;     ;; The runtime label for the compose interpreter
;;     (define compose-coercions-uid (next-uid! "compose_coercions"))

;;     (define compose-coercions-call : Compose-Coercions-Type
;;       (cond
;;         [(space-efficient?) (apply-code compose-coercions-uid)]
;;         [else (lambda (c1 c2) (Sequence-Coercion c1 c2))]))
  
;;     ;; The runtime label for the make coercion operation
;;     (define make-coercion-uid (next-uid! "make_coercion"))
  
;;     (: make-coercion-call : CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
;;     (define make-coercion-call (apply-code make-coercion-uid))
  
;;     (define gen-make-coercion-code : Make-Coercion-Type
;;       (make-make-coercion-code next-uid! make-coercion-call make-coercion-uid))
  
;;     (define make-coercion : Make-Coercion-Type
;;       (cond
;;         [(open-coded? 'make-coercion) gen-make-coercion-code]
;;         [else make-coercion-call]))

;;     ;; The runtime label for the greatest lower bound
;;     (define greatest-lower-bound-type-uid (next-uid! "greatest_lower_bound"))
;;     ;; a call to the runtime greatest lower bound
;;     (: greatest-lower-bound-type-call GreatestLowerBound-Type)
;;     (define (greatest-lower-bound-type-call t1 t2)
;;       (App-Code (Code-Label greatest-lower-bound-type-uid) (list t1 t2)))

;;     ;; The runtime label for the runtime value copier
;;     (define copy-value-in-monoref-uid (next-uid! "copy_value_in_monoref"))
  
;;     (: copy-value-in-monoref-call : CoC3-Expr -> CoC3-Expr)
;;     (define copy-value-in-monoref-call (apply-code copy-value-in-monoref-uid))

;;     (define gen-copy-value-in-monoref : CopyValueInMonoRef-Type
;;       (gen-copy-value-in-monoref-code next-uid!))
  
;;     (define copy-value-in-monoref : CopyValueInMonoRef-Type
;;       (cond
;;         [(open-coded? 'copy-value-in-monoref) gen-copy-value-in-monoref]
;;         [else copy-value-in-monoref-call]))

;;     (define gen-greatest-lower-bound-type : GreatestLowerBound-Type
;;       (gen-greatest-lower-bound-type-code next-uid! greatest-lower-bound-type-call
;;                                           greatest-lower-bound-type-uid))

;;     (define greatest-lower-bound-type : GreatestLowerBound-Type
;;       (cond
;;         [(open-coded? 'greatest-lower-bound-type) gen-greatest-lower-bound-type]
;;         [else greatest-lower-bound-type-call]))

;;     (define gen-compose-coercions-code : Compose-Coercions-Type
;;       (make-compose-coercions-code next-uid! compose-coercions-call
;;                                    compose-coercions-uid make-coercion
;;                                    greatest-lower-bound-type))

;;     (define compose-coercions : Compose-Coercions-Type
;;       (cond
;;         [(open-coded? 'compose-coercions) gen-compose-coercions-code]
;;         [else compose-coercions-call]))
  
;;     ;; Code generators for the coercion casting runtime
;;     (define gen-interp-coercion-code
;;       (make-coerce-code next-uid!
;;                         interp-coercion-call interp-coercion-uid
;;                         make-coercion compose-coercions
;;                         interp-cast-call interp-cast-uid
;;                         greatest-lower-bound-type
;;                         copy-value-in-monoref))

;;     (define interp-coercion
;;       (cond
;;         [(open-coded? 'interp-coercion) gen-interp-coercion-code]
;;         [else interp-coercion-call]))

;;     ;; Code generators for the cast interpreter runtime
;;     (define gen-interp-cast-code
;;       (make-cast-code next-uid!
;;                       interp-cast-call interp-cast-uid
;;                       make-coercion
;;                       greatest-lower-bound-type
;;                       copy-value-in-monoref))

;;     (define interp-cast
;;       (cond
;;         [(open-coded? 'interp-cast) gen-interp-cast-code]
;;         [else interp-cast-call]))
  
;;     (define bindings-needed-for-interp-coercion
;;       (let ([interp-v (next-uid! "value")]
;;             [interp-c (next-uid! "coercion")]
;;             [interp-a (next-uid! "mono-address")]
;;             [mc-t1    (next-uid! "type1")]
;;             [mc-t2    (next-uid! "type2")]
;;             [mc-lbl   (next-uid! "blame_info")])
;;         `([,interp-coercion-uid
;;            . ,(Code (list interp-v interp-c interp-a)
;;                 (gen-interp-coercion-code (Var interp-v) (Var interp-c) (Var interp-a)))]
;;           [,make-coercion-uid
;;            . ,(Code (list mc-t1 mc-t2 mc-lbl)
;;                 (gen-make-coercion-code (Var mc-t1) (Var mc-t2) (Var mc-lbl)))])))

;;     (define bindings-needed-for-space-efficiency
;;       (cond
;;         [(not (space-efficient?)) '()]
;;         [else
;;          (define-values (c1 c2)
;;            (values (next-uid! "coercion1")
;;                    (next-uid! "coercion2")))
;;          `([,compose-coercions-uid
;;             . ,(Code (list c1 c2)
;;                  (gen-compose-coercions-code (Var c1) (Var c2)))])]))
  
;;     ;; Initialize all the state for guarded operations
;;     ;; TODO standardize type names here
;;     (: gbox-ref : Gbox-refT)
;;     (: gbox-set! : Gbox-setT)
;;     (: gvec-ref : Gvec-refT)
;;     (: gvec-set! : Gvec-setT)
;;     (: gvec-length : Gvec-LengthT)
;;     (: bindings-needed-for-guarded : CoC3-Bnd-Code*)
;;     (define-values (gbox-ref gbox-set! gvec-ref gvec-set! gvec-length
;;                              bindings-needed-for-guarded)
;;       ;; First we create initialize the code generators
;;       (let ([gen-gbox-ref-code (make-gbox-ref-code interp-coercion-call)]
;;             [gen-gbox-set!-code (make-gbox-set!-code interp-coercion-call)]
;;             [gen-gvec-ref-code (make-gvect-ref-code interp-coercion-call)]
;;             [gen-gvec-set!-code (make-gvect-set!-code interp-coercion-call)]
;;             [gen-gvec-length-code (make-gvect-length-code)])
;;         (cond
;;           [(inline-guarded-branch?)
;;            ;; we just hand back the code generators to build
;;            ;; inline code everywhere.
;;            (values gen-gbox-ref-code gen-gbox-set!-code
;;                    gen-gvec-ref-code gen-gvec-set!-code
;;                    gen-gvec-length-code
;;                    '())]
;;           [else
;;            ;; If they are not inlined then the compiler generates
;;            ;; the runtime binding and returns procedures that builds
;;            ;; invokations of this runtime code.
;;            (let ([gbr   (next-uid! "rt_gbox_ref")]
;;                  [gbr-b (next-uid! "box")]
;;                  [gbs   (next-uid! "rt_gbox_set")]
;;                  [gbs-b (next-uid! "box")]
;;                  [gbs-v (next-uid! "write_val")]
;;                  [gvr   (next-uid! "rt_gvec_ref")]
;;                  [gvr-r (next-uid! "vec")]
;;                  [gvr-i (next-uid! "ind")]
;;                  [gvs   (next-uid! "rt_gvec_set")]
;;                  [gvs-r (next-uid! "vec")]
;;                  [gvs-i (next-uid! "ind")]
;;                  [gvs-v (next-uid! "val")]
;;                  [gvl   (next-uid! "rt_gvec_len")]
;;                  [gvl-r (next-uid! "vec")])
;;              (values
;;               (apply-code gbr) (apply-code gbs)
;;               (apply-code gvr) (apply-code gvs)
;;               (apply-code gvl)
;;               `([,gbr
;;                  . ,(Code (list gbr-b) (gen-gbox-ref-code (Var gbr-b)))]
;;                 [,gbs
;;                  . ,(Code (list gbs-b gbs-v)
;;                       (gen-gbox-set!-code (Var gbs-b) (Var gbs-v)))]
;;                 [,gvr
;;                  . ,(Code (list gvr-r gvr-i)
;;                       (gen-gvec-ref-code (Var gvr-r) (Var gvr-i)))]
;;                 [,gvs
;;                  . ,(Code (list gvs-r gvs-i gvs-v)
;;                       (gen-gvec-set!-code (Var gvs-r) (Var gvs-i)
;;                                           (Var gvs-v)))]
;;                 [,gvl
;;                  . ,(Code (list gvl-r)
;;                       (gen-gvec-length-code (Var gvl-r)))])))])))
  
;;     (: mbox-ref : Mbox-refT)
;;     (: mbox-set! : Mbox-setT)
;;     (: mvec-ref : Mvec-refT)
;;     (: mvec-set! : Mvec-setT)
;;     (: bindings-needed-for-monotonic-refs : CoC3-Bnd-Code*)
;;     (define-values (mbox-ref mbox-set! mvec-ref mvec-set!
;;                              bindings-needed-for-monotonic-refs)
;;       (let ([gen-mbox-ref-code (make-mbox-ref-code next-uid! interp-cast-call)]
;;             [gen-mbox-set!-code (make-mbox-set!-code next-uid! interp-cast-call)]
;;             [gen-mvec-ref-code (make-mvect-ref-code next-uid! interp-cast-call)]
;;             [gen-mvec-set!-code (make-mvect-set!-code next-uid! interp-cast-call)]
;;             [interp-v (next-uid! "value")]
;;             [interp-t1 (next-uid! "t1")]
;;             [interp-t2 (next-uid! "t2")]
;;             [interp-a (next-uid! "mono-address")]
;;             [interp-lbl (next-uid! "lbl")]
;;             [glbt-t1    (next-uid! "type1")]
;;             [glbt-t2    (next-uid! "type2")]
;;             [glbt-a          (next-uid! "mono-address")])
;;         (define shared-bnd* : CoC3-Bnd-Code*
;;           `([,interp-cast-uid
;;              . ,(Code (list interp-v interp-t1 interp-t2 interp-lbl interp-a)
;;                   (gen-interp-cast-code (Var interp-v) (Var interp-t1) (Var interp-t2) (Var interp-lbl) (Var interp-a)))]
;;             [,greatest-lower-bound-type-uid
;;              . ,(Code (list glbt-t1 glbt-t2)
;;                   (gen-greatest-lower-bound-type (Var glbt-t1) (Var glbt-t2)))]
;;             [,copy-value-in-monoref-uid
;;              . ,(Code (list glbt-a)
;;                   (gen-copy-value-in-monoref (Var glbt-a)))]))
;;         (cond
;;           ;; FIXME: I guess this flag should be generalized to all ref implementations
;;           [(inline-guarded-branch?)
;;            (values
;;             gen-mbox-ref-code gen-mbox-set!-code
;;             gen-mvec-ref-code gen-mvec-set!-code
;;             shared-bnd*)]
;;           [else
;;            ;; If they are not inlined then the compiler generates
;;            ;; the runtime binding and returns procedures that builds
;;            ;; invokations of this runtime code.
;;            (let ([mbr   (next-uid! "rt_mbox_ref")]
;;                  [mbr-b (next-uid! "box")]
;;                  [mbr-rt (next-uid! "ref_type")]
;;                  [mbs   (next-uid! "rt_mbox_set")]
;;                  [mbs-b (next-uid! "box")]
;;                  [mbs-v (next-uid! "write_val")]
;;                  [mbs-rt (next-uid! "ref_type")]
;;                  [mvr   (next-uid! "rt_mvec_ref")]
;;                  [mvr-r (next-uid! "vec")]
;;                  [mvr-i (next-uid! "ind")]
;;                  [mvr-rt (next-uid! "ref_type")]
;;                  [mvs   (next-uid! "rt_mvec_set")]
;;                  [mvs-r (next-uid! "vec")]
;;                  [mvs-i (next-uid! "ind")]
;;                  [mvs-v (next-uid! "val")]
;;                  [mvs-rt (next-uid! "ref_type")])
;;              (values
;;               (apply-code mbr) (apply-code mbs)
;;               (apply-code mvr) (apply-code mvs)
;;               (append
;;                shared-bnd*
;;                `([,mbr
;;                   . ,(Code (list mbr-b mbr-rt) (gen-mbox-ref-code (Var mbr-b) (Var mbr-rt)))]
;;                  [,mbs
;;                   . ,(Code (list mbs-b mbs-v mbs-rt)
;;                        (gen-mbox-set!-code (Var mbs-b) (Var mbs-v) (Var mbs-rt)))]
;;                  [,mvr
;;                   . ,(Code (list mvr-r mvr-i mvr-rt)
;;                        (gen-mvec-ref-code (Var mvr-r) (Var mvr-i) (Var mvr-rt)))]
;;                  [,mvs
;;                   . ,(Code (list mvs-r mvs-i mvs-v mvs-rt)
;;                        (gen-mvec-set!-code (Var mvs-r) (Var mvs-i)
;;                                            (Var mvs-v) (Var mvs-rt)))]))))])))


;;     (: dyn-fn-app : Dyn-Fn-AppT)
;;     (: bindings-needed-for-fn-dynamic-operations : CoC3-Bnd-Code*)
;;     (define-values (dyn-fn-app
;;                     bindings-needed-for-fn-dynamic-operations)
;;       (let* ([smart-cast
;;               (make-smart-cast next-uid! interp-coercion-call make-coercion)]
;;              [gen-dyn-fn-app
;;               (make-dyn-fn-app-code next-uid! smart-cast)])
;;         (define ((th-error [sym : Symbol]) . a)
;;           (error sym "dynamic-operation? = #f but present in AST"))
;;         (case (dynamic-operations?)
;;           [(#f)
;;            (values
;;             (th-error 'interpret-cast-with-coercions/dyn-fn-app)
;;             '())]
;;           [else
;;            (values
;;             gen-dyn-fn-app
;;             `())])))

  
;;     (: dyn-gbox-ref : Dyn-Gbox-refT)
;;     (: dyn-gbox-set! : Dyn-Gbox-setT)
;;     (: dyn-gvec-ref  : Dyn-Gvec-refT)
;;     (: dyn-gvec-set! : Dyn-Gvec-setT)
;;     (: bindings-needed-for-guarded-dynamic-operations : CoC3-Bnd-Code*)
;;     (define-values (dyn-gbox-ref dyn-gbox-set!
;;                                  dyn-gvec-ref dyn-gvec-set!
;;                                  bindings-needed-for-guarded-dynamic-operations)
;;       (let* ([smart-cast
;;               (make-smart-cast next-uid! interp-coercion-call make-coercion)]
;;              [gen-dyn-gvec-set!
;;               (make-dyn-gvect-set!-code next-uid! gvec-set! smart-cast)]
;;              [gen-dyn-gvec-ref
;;               (make-dyn-gvect-ref-code next-uid! gvec-ref smart-cast)]
;;              [gen-dyn-gbox-set!
;;               (make-dyn-gbox-set!-code next-uid! gbox-set! smart-cast)]
;;              [gen-dyn-gbox-ref 
;;               (make-dyn-gbox-ref-code next-uid! gbox-ref smart-cast)])
;;         (define ((th-error [sym : Symbol]) . a)
;;           (error sym "dynamic-operation? = #f but present in AST"))
;;         (case (dynamic-operations?)
;;           [(#f)
;;            (values
;;             (th-error 'interpret-cast-with-coercions/dyn-gbox-ref)
;;             (th-error 'interpret-cast-with-coercions/dyn-gbox-set!)
;;             (th-error 'interpret-cast-with-coercions/dyn-gvec-ref)
;;             (th-error 'interpret-cast-with-coercions/dyn-gvec-set!)
;;             '())]
;;           [(inline)
;;            (values
;;             gen-dyn-gbox-ref gen-dyn-gbox-set!
;;             gen-dyn-gvec-ref gen-dyn-gvec-set!
;;             '())]
;;           [else
;;            ;; TODO come up with a solution that doesn't manually
;;            ;; require all the variable initializations
;;            (let ([gbr   (next-uid! "rt_dyn_gbox_ref")]
;;                  [gbr-b (next-uid! "gbox")]
;;                  [gbr-l (next-uid! "blame_info")]
;;                  [gbs   (next-uid! "rt_dyn_gbox_set")]
;;                  [gbs-b (next-uid! "gbox")]
;;                  [gbs-v (next-uid! "write_val")]
;;                  [gbs-t (next-uid! "val_type")]
;;                  [gbs-l (next-uid! "blame_info")]
;;                  [gvr   (next-uid! "rt_dyn_gvec_ref")]
;;                  [gvr-r (next-uid! "gvec")]
;;                  [gvr-i (next-uid! "ind")]
;;                  [gvr-l (next-uid! "blame_info")]
;;                  [gvs   (next-uid! "rt_dyn_gvec_set")]
;;                  [gvs-r (next-uid! "gvec")]
;;                  [gvs-i (next-uid! "ind")]
;;                  [gvs-v (next-uid! "val")]
;;                  [gvs-t (next-uid! "val_type")]
;;                  [gvs-l (next-uid! "blame_info")])
;;              (values
;;               (apply-code gbr) (apply-code gbs)
;;               (apply-code gvr) (apply-code gvs)
;;               `([,gbr
;;                  . ,(Code (list gbr-b gbr-l)
;;                       (gen-dyn-gbox-ref (Var gbr-b) (Var gbr-l)))]
;;                 [,gbs
;;                  . ,(Code (list gbs-b gbs-v gbs-t gbs-l)
;;                       (gen-dyn-gbox-set! (Var gbs-b) (Var gbs-v)
;;                                          (Var gbs-t) (Var gbs-l)))]
;;                 [,gvr
;;                  . ,(Code (list gvr-r gvr-i gvr-l)
;;                       (gen-dyn-gvec-ref (Var gvr-r) (Var gvr-i) (Var gvr-l)))]
;;                 [,gvs
;;                  . ,(Code (list gvs-r gvs-i gvs-v gvs-t gvs-l)
;;                       (gen-dyn-gvec-set! (Var gvs-r) (Var gvs-i)
;;                                          (Var gvs-v) (Var gvs-t)
;;                                          (Var gvs-l)))])))])))
  
;;     (: dyn-mbox-ref  : Dyn-Mbox-refT)
;;     (: dyn-mbox-set! : Dyn-Mbox-setT)
;;     (: dyn-mvec-ref  : Dyn-Mvec-refT)
;;     (: dyn-mvec-set! : Dyn-Mvec-setT)
;;     (: bindings-needed-for-mono-dynamic-operations : CoC3-Bnd-Code*)
;;     (define-values (dyn-mbox-ref dyn-mbox-set!
;;                                  dyn-mvec-ref dyn-mvec-set!
;;                                  bindings-needed-for-mono-dynamic-operations)
;;       (let* ([smart-cast
;;               (make-smart-cast next-uid! interp-coercion-call make-coercion)]
;;              [gen-dyn-mvec-set!
;;               (make-dyn-mvect-set!-code next-uid! mvec-set! smart-cast)]
;;              [gen-dyn-mvec-ref
;;               (make-dyn-mvect-ref-code next-uid! mvec-ref)]
;;              [gen-dyn-mbox-set!
;;               (make-dyn-mbox-set!-code next-uid! mbox-set! smart-cast)]
;;              [gen-dyn-mbox-ref
;;               (make-dyn-mbox-ref-code next-uid! mbox-ref)])
;;         (define ((th-error [sym : Symbol]) . a)
;;           (error sym "dynamic-operation? = #f but present in AST"))
;;         (case (dynamic-operations?)
;;           [(#f)
;;            (values
;;             (th-error 'interpret-cast-with-coercions/dyn-mbox-ref)
;;             (th-error 'interpret-cast-with-coercions/dyn-mbox-set!)
;;             (th-error 'interpret-cast-with-coercions/dyn-mvec-ref)
;;             (th-error 'interpret-cast-with-coercions/dyn-mvec-set!)
;;             '())]
;;           [(inline)
;;            (values
;;             gen-dyn-mbox-ref gen-dyn-mbox-set!
;;             gen-dyn-mvec-ref gen-dyn-mvec-set!
;;             '())]
;;           [else
;;            (let ([mbr   (next-uid! "rt_dyn_mbox_ref")]
;;                  [mbr-b (next-uid! "mbox")]
;;                  [mbr-l (next-uid! "blame_info")]
;;                  [mbs   (next-uid! "rt_dyn_mbox_set")]
;;                  [mbs-b (next-uid! "mbox")]
;;                  [mbs-v (next-uid! "write_val")]
;;                  [mbs-t (next-uid! "val_type")]
;;                  [mbs-l (next-uid! "blame_info")]
;;                  [mvr   (next-uid! "rt_dyn_mvec_ref")]
;;                  [mvr-r (next-uid! "mvec")]
;;                  [mvr-i (next-uid! "ind")]
;;                  [mvr-l (next-uid! "blame_info")]
;;                  [mvs   (next-uid! "rt_dyn_mvec_set")]
;;                  [mvs-r (next-uid! "mvec")]
;;                  [mvs-i (next-uid! "ind")]
;;                  [mvs-v (next-uid! "val")]
;;                  [mvs-t (next-uid! "val_type")]
;;                  [mvs-l (next-uid! "blame_info")])
;;              (values
;;               (apply-code mbr) (apply-code mbs)
;;               (apply-code mvr) (apply-code mvs)
;;               `([,mbr
;;                  . ,(Code (list mbr-b mbr-l)
;;                       (gen-dyn-mbox-ref (Var mbr-b) (Var mbr-l)))]
;;                 [,mbs
;;                  . ,(Code (list mbs-b mbs-v mbs-t mbs-l)
;;                       (gen-dyn-mbox-set! (Var mbs-b) (Var mbs-v)
;;                                          (Var mbs-t) (Var mbs-l)))]
;;                 [,mvr
;;                  . ,(Code (list mvr-r mvr-i mvr-l)
;;                       (gen-dyn-mvec-ref (Var mvr-r) (Var mvr-i)
;;                                         (Var mvr-l)))]
;;                 [,mvs
;;                  . ,(Code (list mvs-r mvs-i mvs-v mvs-t mvs-l)
;;                       (gen-dyn-mvec-set! (Var mvs-r) (Var mvs-i)
;;                                          (Var mvs-v) (Var mvs-t)
;;                                          (Var mvs-l)))])))])))
  
;;     ;; Next we generate the bindings for code that needs to be
;;     ;; included as the runtime.
;;     (define gradual-runtime-bindings : CoC3-Bnd-Code*
;;       (append
;;        bindings-needed-for-fn-dynamic-operations
;;        bindings-needed-for-monotonic-refs
;;        bindings-needed-for-mono-dynamic-operations
;;        bindings-needed-for-guarded
;;        bindings-needed-for-guarded-dynamic-operations
;;        bindings-needed-for-space-efficiency
;;        bindings-needed-for-interp-coercion))

;;     (define exp-with-lowered-gradual-operations
;;       (interpret-casts-in-expr
;;        next-uid!
;;        interp-coercion-uid interp-coercion compose-coercions make-coercion
;;        gbox-ref gbox-set! gvec-ref gvec-set! gvec-length
;;        mbox-ref mbox-set! mvec-ref mvec-set!
;;        dyn-gbox-ref dyn-gbox-set! dyn-gvec-ref dyn-gvec-set!
;;        dyn-mbox-ref  dyn-mbox-set! dyn-mvec-ref dyn-mvec-set!
;;        dyn-fn-app
;;        prgm-exp))
  
;;     (Prog (list prgm-name (unique-counter-next! unique-counter) prgm-type)
;;       (Labels gradual-runtime-bindings exp-with-lowered-gradual-operations))))

;; ;; These templates are used to build the code that performs
;; ;; casting at runtime. The templates are a little over
;; ;; parameterized currently in hopes that it make specialization
;; ;; and customization easier in the future.
;; #;(define-type Coerce-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

;; #;
;; (: make-coerce-code :
;;    (String -> Uid)
;;    Coerce-Type Uid
;;    Make-Coercion-Type Compose-Coercions-Type
;;    Cast-With-MAddr-Type Uid
;;    GreatestLowerBound-Type CopyValueInMonoRef-Type
;;    ->
;;    Coerce-Type)
;; #;
;; (define ((make-coerce-code
;;           next-uid!
;;           coerce coerce-u
;;           mk-crcn comp-crcn
;;           cast cast-u
;;           glbt copy-val-monoref) v c mono_type)
;;   (define who 'make-coerce-code)
;;   (debug who v c)
;;   ;; apply-cast is normally specified in terms of inspecting the
;;   ;; value. But in a world were value reflection on values is limited
;;   ;; the coercions can drive the casting process.
;;   (let*$ ([val v] [crcn c])
;;     (cond$
;;      [(id?$ crcn) val]
;;      [(seq?$ crcn)
;;       ;; I think that something better could be done here
;;       ;; why recur there are only so many things that the underlying
;;       ;; sequence could be?
      
;;       ;; This may be getting in the way of simple specializations
;;       ;; May want come up with smaller versions of the cast/function
;;       ;; For the specialized sub grammers.
;;       (let*$ ([seq_fst (seq-fst$ crcn)]
;;               [seq_snd (seq-snd$ crcn)]
;;               [fst_cast_value (coerce val seq_fst mono_type)])
;;         (coerce fst_cast_value seq_snd mono_type))]
;;      ;; By the typing rules of coercions we know v must be a dyn value
;;      [(prj?$ crcn)
;;       (let*$ ([prj_type  (prj-type$ crcn)]
;;               [prj_label (prj-label$ crcn)]
;;               ;; The current implementation of the dyn interface is not a good one
;;               ;; this line is going to have to recover the type from dyn twice
;;               [dyn_value (dyn-value$ val)]
;;               [dyn_type  (dyn-type$ val)])
;;         ;; Maybe this line should be a call to compose?
;;         ;; which would do the same thing but wouldn't duplicate
;;         ;; this line across two functions
;;         ;; I did it this way because I know this code won't build
;;         ;; the injection coercion
;;         (let*$ ([projected_type (mk-crcn dyn_type prj_type prj_label)])
;;           (coerce dyn_value projected_type mono_type)))]
;;      [(inj?$ crcn) (dyn-make$ val (inj-type$ crcn))]
;;      [(mediating-crcn?$ crcn)
;;       (cond$
;;        [(fnC?$ crcn)
;;         (If (Fn-Proxy-Huh val)
;;             (App-Code (Fn-Caster (Fn-Proxy-Closure val)) (list val crcn))
;;             (App-Code (Fn-Caster val) (list val crcn)))]
;;        [(ref?$ crcn)
;;         (if$ (Guarded-Proxy-Huh val)
;;              (let*$ ([old_val  (Guarded-Proxy-Ref val)]
;;                      [old_crcn (Guarded-Proxy-Coercion val)])
;;                (let*$ ([composed_crcn (comp-crcn old_crcn crcn)])
;;                  (if$ (id?$ composed_crcn)
;;                       old_val
;;                       (Guarded-Proxy
;;                        old_val 
;;                        (Coercion composed_crcn)))))
;;              (Guarded-Proxy val (Coercion crcn)))]
;;        [(mrefC?$ crcn)
;;         (match val
;;           [(Var a)
;;            (let*$ ([t2 (mrefC-type$ crcn)])
;;              (if$ (dyn?$ t2)
;;                   val
;;                   (let*$ ([t1 (Mbox-rtti-ref val)]
;;                           [t3 (glbt t1 t2)])
;;                     (if$ (op=? t1 t3)
;;                          val
;;                          (Begin
;;                            (list
;;                             (Mbox-rtti-set! val t3))
;;                            (let*$ ([vv (copy-val-monoref val)]
;;                                    [cv (cast
;;                                         vv t1 t3
;;                                         (Quote "Monotonic references currently does not track blame")
;;                                         val)]
;;                                    [t4 (Mbox-rtti-ref val)])
;;                              (if$ (op=? t3 t4)
;;                                   (Begin
;;                                     (list (Mbox-val-set! val cv))
;;                                     val)
;;                                   val)))))))]
;;           [other (error 'interp-coercion/mrefC "unmatched value ~a" other)])]
;;        [(mvectC?$ crcn)
;;         (match val
;;           [(Var a)
;;            (let*$ ([t2 (mvectC-type$ crcn)])
;;              (if$ (dyn?$ t2)
;;                   val
;;                   (let*$ ([t1 (Mvector-rtti-ref val)]
;;                           [t3 (glbt t1 t2)])
;;                     (if$ (op=? t1 t3)
;;                          val
;;                          (Begin
;;                            (list
;;                             (Mvector-rtti-set! val t3)
;;                             (let*$ ([vn (Mvector-length val)])
;;                               (let* ([i-u (next-uid! "index")]
;;                                      [i (Var i-u)]
;;                                      [x (next-uid! "_")])
;;                                 (cond$
;;                                  [(tupleT?$ t3)
;;                                   (let*$ ([n (Type-Tuple-num t3)])
;;                                     (Repeat i-u (Quote 0) vn x UNIT-IMDT
;;                                       (let*$ ([vi (Mvector-val-ref val i)]
;;                                               [cvi (Copy-Tuple n vi)])
;;                                         (Begin
;;                                           (list
;;                                            (Mvector-val-set! val i cvi))
;;                                           (let*$ ([ccvi (cast
;;                                                          cvi t1 t3
;;                                                          (Quote "Monotonic references currently does not track blame")
;;                                                          val)]
;;                                                   [t4 (Mvector-rtti-ref val)])
;;                                             (if$ (op=? t3 t4)
;;                                                  (Mvector-val-set! val i ccvi)
;;                                                  (Break-Repeat)))))))]
;;                                  [else
;;                                   (Repeat i-u (Quote 0) vn x UNIT-IMDT
;;                                     (let*$ ([vi (Mvector-val-ref val i)]
;;                                             [cvi (cast
;;                                                   vi t1 t3
;;                                                   (Quote "Monotonic references currently does not track blame")
;;                                                   val)]
;;                                             [t4 (Mvector-rtti-ref val)])
;;                                       (if$ (op=? t3 t4)
;;                                            (Mvector-val-set! val i cvi)
;;                                            (Break-Repeat))))]))))
;;                            val)))))]
;;           [other (error 'interp-coercion/mvectC "unmatched value ~a" other)])]
;;        [(tuple?$ crcn)
;;         (match crcn
;;           [(not (Quote-Coercion _)) (If (Op '= (list (Quote 0) mono_type))
;;                                         (Coerce-Tuple coerce-u val crcn)
;;                                         (Coerce-Tuple-In-Place coerce-u val crcn mono_type))]
;;           [(Quote-Coercion (CTuple n c*))
;;            (define-values (bnd* var*)
;;              (for/lists ([bnd* : CoC3-Bnd*] [var* : CoC3-Expr*])
;;                         ([i (in-range n)] [c (in-list c*)])
;;                (cond
;;                  [(index? i)
;;                   (define tmp (next-uid! "element"))
;;                   (values
;;                    (cons tmp (coerce (Tuple-proj val (Quote i)) (Quote-Coercion c) (Quote 0)))
;;                    (Var tmp))]
;;                  [else (error 'interpret-casts-with-coercions "bad index")])))
;;            (Let bnd* (Create-tuple var*))]
;;           [other (error 'cast/coercion "thats impossible! ... I think")])]
;;        [else (Blame (Quote "bad implemention of mediating coercions"))])]
;;      ;; the coercion must be failure
;;      [else (Blame (fail-label$ crcn))])))


;; (define-type Cast-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
;; (define-type Cast-With-MAddr-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

;; (: make-cast-code :
;;    (String -> Uid)
;;    Cast-With-MAddr-Type Uid
;;    Make-Coercion-Type
;;    GreatestLowerBound-Type CopyValueInMonoRef-Type
;;    ->
;;    Cast-With-MAddr-Type)
;; (define ((make-cast-code
;;           next-uid!
;;           cast cast-u
;;           mk-crcn
;;           glbt copy-val-monoref) v type1 type2 lbl mono-address)
;;   (: cast-dyn : Cast-With-MAddr-Type)
;;   (define (cast-dyn  v t1 t2 lbl mono-address)
;;     (let*$ ([val v] [tag (dyn-immediate-tag$ val)])
;;       (cond$
;;        [(op=? (Tag 'Int) tag)
;;         (cast-undyned (dyn-immediate-value$ val) (Type INT-TYPE) t2 lbl mono-address)]
;;        [(op=? (Tag 'Bool) tag)
;;         (cast-undyned (dyn-immediate-value$ val) (Type BOOL-TYPE) t2 lbl mono-address)]
;;        [(op=? (Tag 'Unit) tag)
;;         (cast-undyned (Quote '()) (Type UNIT-TYPE) t2 lbl mono-address)]
;;        [(op=? (Tag 'Char) tag)
;;         (cast-undyned (dyn-immediate-value$ val) (Type CHAR-TYPE) t2 lbl mono-address)]
;;        [(op=? (Tag 'Boxed) tag)
;;         (cast-undyned (dyn-value$ val) (dyn-type$ val) t2 lbl mono-address)]
;;        [else (Blame (Quote "Unexpected value in cast tree"))])))

;;   (: cast-undyned : Cast-With-MAddr-Type)
;;   (define (cast-undyned v t1 t2 l mono-address)
;;     (let*$ ([value v] [type1 t1] [type2 t2] [label l])
;;       (cond$
;;        [(op=? type1 type2) value]
;;        [else (cast-ground value type1 type2 label mono-address)])))

;;   (: cast-ground : Cast-With-MAddr-Type)
;;   (define (cast-ground v t1 t2 lbl mono-address)
;;     (let*$ ([value v] [type1 t1] [type2 t2])
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
;;            [(op=? (Tag 'MRef) tag1)
;;             (cast-mbox value type1 type2 lbl)]
;;            [(op=? (Tag 'MVect) tag1)
;;             (cast-mvect value type1 type2 lbl)]
;;            [(op=? (Tag 'STuple) tag1)
;;             (If (Op '= (list (Quote 0) mono-address))
;;                 (cast-tuple value type1 type2 lbl)
;;                 (Cast-Tuple-In-Place cast-u value type1 type2 lbl mono-address))]
;;            [else (Blame (Quote "Unexpected Type1 in cast tree"))]))])))
  
;;   (: cast-fn : Cast-Type)
;;   (define (cast-fn v t1 t2 lbl)
;;     (let*$ ([value v] [type1 t1] [type2 t2] [l lbl])
;;       (if$ (op=? (Type DYN-TYPE) type2)
;;            (dyn-make$ value type1)
;;            (let*$ ([crcn (mk-crcn type1 type2 l)])
;;              (If (Fn-Proxy-Huh value)
;;                  (App-Code (Fn-Caster (Fn-Proxy-Closure value)) (list value crcn))
;;                  (App-Code (Fn-Caster value) (list value crcn)))))))

;;   (: cast-mbox : Cast-Type)
;;   (define (cast-mbox v t1 t2 lbl)
;;     (let*$ ([val v] [type1 t1] [type2 t2] [tag_mref (type-tag type2)])
;;       (if$ (op=? (Type DYN-TYPE) type2)
;;            (dyn-make$ val type1)
;;            (if$ (op=? tag_mref (Tag 'MRef))
;;                 (match val
;;                   [(Var a)
;;                    (let*$ ([t2 (mref-of$ type2)])
;;                      (if$ (dyn?$ t2)
;;                           val
;;                           (let*$ ([t1 (Mbox-rtti-ref val)]
;;                                   [t3 (glbt t1 t2)])
;;                             (if$ (op=? t1 t3)
;;                                  val
;;                                  (Begin
;;                                    (list
;;                                     (Mbox-rtti-set! val t3))
;;                                    (let*$ ([vv (copy-val-monoref val)]
;;                                            [cv (cast vv t1 t3
;;                                                      (Quote "Monotonic references currently does not track blame")
;;                                                      val)]
;;                                            [t4 (Mbox-rtti-ref val)])
;;                                      (if$ (op=? t3 t4)
;;                                           (Begin
;;                                             (list (Mbox-val-set! val cv))
;;                                             val)
;;                                           val)))))))]
;;                   [other (error 'interp-cast-with-coercions/cast/mref "unmatched value ~a" other)])
;;                 (Blame lbl)))))

;;   (: cast-mvect : Cast-Type)
;;   (define (cast-mvect v t1 t2 lbl)
;;     (let*$ ([val v] [type1 t1] [type2 t2] [tag_mvect (type-tag type2)])
;;       (if$ (op=? (Type DYN-TYPE) type2)
;;            (dyn-make$ val type1)
;;            (if$ (op=? tag_mvect (Tag 'MVect))
;;                 (match val
;;                   [(Var a)
;;                    (let*$ ([t2 (mvect-of$ type2)])
;;                      (if$ (dyn?$ t2)
;;                           val
;;                           (let*$ ([t1 (Mvector-rtti-ref val)]
;;                                   [t3 (glbt t1 t2)])
;;                             (if$ (op=? t1 t3)
;;                                  val
;;                                  (Begin
;;                                    (list
;;                                     (Mvector-rtti-set! val t3)
;;                                     (let* ([i-u (next-uid! "index")]
;;                                            [i (Var i-u)]
;;                                            [x (next-uid! "_")])
;;                                       (let*$ ([vn (Mvector-length val)])
;;                                         (cond$
;;                                          [(tupleT?$ t3)
;;                                           (let*$ ([n (Type-Tuple-num t3)])
;;                                             (Repeat i-u (Quote 0) vn x UNIT-IMDT
;;                                                     (let*$ ([vi (Mvector-val-ref val i)]
;;                                                             [cvi (Copy-Tuple n vi)])
;;                                                       (Begin
;;                                                         (list
;;                                                          (Mvector-val-set! val i cvi))
;;                                                         (let*$ ([ccvi (cast cvi t1 t3
;;                                                                             (Quote "Monotonic references currently does not track blame")
;;                                                                             val)]
;;                                                                 [t4 (Mvector-rtti-ref val)])
;;                                                           (if$ (op=? t3 t4)
;;                                                                (Mvector-val-set! val i ccvi)
;;                                                                (Break-Repeat)))))))]
;;                                          [else
;;                                           ;; TODO: checking if t3=t4 is unneeded in this case, prove it!
;;                                           (Repeat i-u (Quote 0) vn x UNIT-IMDT
;;                                                   (let*$ ([vi (Mvector-val-ref val i)]
;;                                                           [cvi (cast vi t1 t3
;;                                                                      (Quote "Monotonic references currently does not track blame")
;;                                                                      val)]
;;                                                           [t4 (Mvector-rtti-ref val)])
;;                                                     (if$ (op=? t3 t4)
;;                                                          (Mvector-val-set! val i cvi)
;;                                                          (Break-Repeat))))]))))
;;                                    val)))))]
;;                   [other (error 'interp-casts-with-coercions/cast/mvect "unmatched value ~a" other)])
;;                 (Blame lbl)))))

;;   (: cast-tuple : Cast-Type)
;;   (define (cast-tuple v t1 t2 lbl)
;;     (let*$ ([value v] [type1 t1] [type2 t2] [label lbl])
;;       (if$ (op=? (Type DYN-TYPE) type2)
;;            (dyn-make$ value type1)
;;            ;; Todo Reformat this code
;;            (if #f #;(and (Type? t1) (Type? t2))
;;                (let ([t1t (Type-type t1)]
;;                      [t2t (Type-type t2)])
;;                  (if (and (STuple? t1t) (STuple? t2t))
;;                      (let-values ([(bnd* arg*)
;;                                    (for/fold ([b* : CoC3-Bnd* '()]
;;                                               [arg* : Uid* '()])
;;                                              ([i (in-range (STuple-num t2t))])
;;                                      (unless (index? i)
;;                                        (error 'make-cast-tuple-code "bad index"))
;;                                      (let ([a (next-uid! "item_type")])
;;                                        (values (cons (cons a (let*$ ([item (Tuple-proj v (Quote i))]
;;                                                                      [new-t1t (tuple-type-arg$ t1 i)]
;;                                                                      [new-t2t (tuple-type-arg$ t2 i)])
;;                                                                (cast item new-t1t new-t2t lbl (Quote 0)))) b*)
;;                                                (cons a arg*))))])
;;                        (Let bnd* (Create-tuple (map (inst Var Uid) arg*))))
;;                      (error 'make-cast-tuple-code)))
;;                (let*$ ([tag2 (type-tag type2)])
;;                  (if$ (op=? tag2 (Tag 'STuple))
;;                       (let*$ ([type1_num (type-tuple-num type1)]
;;                               [type2_num (type-tuple-num type2)])
;;                         (if$ (op<=? type2_num type1_num)
;;                              (Cast-Tuple cast-u v t1 t2 lbl)
;;                              (Blame type2_num)))
;;                       (Blame lbl)))))))
  
;;   (let*$ ([val v] [t1 type1] [t2 type2])
;;     (cond$
;;      [(op=? type1 type2) v]
;;      [(op=? type1 (Type DYN-TYPE)) (cast-dyn v type1 type2 lbl mono-address)]
;;      [else (cast-ground v type1 type2 lbl mono-address)])))





;; ;; ;; ic-expr maps over the expressions lowering function cast
;; ;; ;; into calls to the runtime cast interpreter and direct manipulation
;; ;; ;; of the representation for each type and value.
;; ;; (: interpret-casts-in-expr :
;; ;;    (String -> Uid)
;; ;;    Uid
;; ;;    Coerce-Type
;; ;;    Compose-Coercions-Type
;; ;;    Make-Coercion-Type
;; ;;    Gbox-refT Gbox-setT Gvec-refT Gvec-setT Gvec-LengthT
;; ;;    Mbox-refT Mbox-setT Mvec-refT Mvec-setT
;; ;;    Dyn-Gbox-refT Dyn-Gbox-setT 
;; ;;    Dyn-Gvec-refT Dyn-Gvec-setT
;; ;;    Dyn-Mbox-refT Dyn-Mbox-setT
;; ;;    Dyn-Mvec-refT Dyn-Mvec-setT
;; ;;    Dyn-Fn-AppT
;; ;;    CoC1-Expr
;; ;;    ->
;; ;;    CoC3-Expr)
;; ;; (define (interpret-casts-in-expr
;; ;;          next-uid!
;; ;;          interp-uid interp-coercion interp-compose mk-coercion
;; ;;          gbox-ref gbox-set! gvect-ref gvect-set! gvect-length
;; ;;          mbox-ref mbox-set! mvect-ref mvect-set!
;; ;;          dyn-gbox-ref dyn-gbox-set! 
;; ;;          dyn-gvec-ref dyn-gvec-set!
;; ;;          dyn-mbox-ref dyn-mbox-set!
;; ;;          dyn-mvec-ref dyn-mvec-set!
;; ;;          dyn-fn-app
;; ;;          exp)
;; ;;   ;; map through a code binding 
;; ;;   (: recur-through-bndc* : CoC1-Bnd-Code -> CoC3-Bnd-Code)
;; ;;   (define (recur-through-bndc* b)
;; ;;     (match-let ([(cons u (Code u* e)) b])
;; ;;       (cons u (Code u* (recur e)))))
;; ;;   ;; map through a data binding 
;; ;;   (: recur-through-bnd* : CoC1-Bnd -> CoC3-Bnd)
;; ;;   (define (recur-through-bnd* b)
;; ;;     (cons (car b) (recur (cdr b))))

;; ;;   ;; map through a list of expressions
;; ;;   (: recur* : CoC1-Expr* -> CoC3-Expr*)
;; ;;   (define (recur* exps) (map recur exps))
  
;; ;;   (: recur : CoC1-Expr -> CoC3-Expr)
;; ;;   (define (recur exp)
;; ;;     (define who 'recur)
;; ;;     (debug who exp)
;; ;;     (match exp
;; ;;       ;; Interesting Cases -----------------------------------------------
;; ;;       ;; Transformations for Casting runtime
;; ;;       [(Interpreted-Cast (app recur v) (Coercion (app recur c)))
;; ;;        (interp-coercion v c (Quote 0))]
;; ;;       [(Cast (app recur e) (Coercion c))
;; ;;        (interp-coercion e (Quote-Coercion c) (Quote 0))]
;; ;;       [(Cast e (Coercion (Ref r w))) #:when #f
;; ;;        ;; TODO (Andre) delete this branch
;; ;;        ;; Here this specialization of reference coercions
;; ;;        ;; was originally in the lower-reference-casts pass.
;; ;;        ;; This seems redundant when placed next to the
;; ;;        ;; specialization that occurs in this pass.
;; ;;        ;; I am leaving it for now but we should delete this
;; ;;        ;; code once we have confirmed doing so won't break
;; ;;        ;; anything.
;; ;;        (define e^ (recur e))
;; ;;        (define-values (u o r^ w^)
;; ;;          (values (next-uid! "ref_coercion")
;; ;;                  (next-uid! "ref_old_ref")
;; ;;                  (next-uid! "ref_read")
;; ;;                  (next-uid! "ref_write")))
;; ;;        (: do-ref-coercion-inline : (Var Uid) -> CoC3-Expr)
;; ;;        (define (do-ref-coercion-inline v)
;; ;;          (If (Guarded-Proxy-Huh v)
;; ;;              (Let (list (cons u (Guarded-Proxy-Coercion v))
;; ;;                         (cons o (Guarded-Proxy-Ref v)))
;; ;;                (Let (list (cons r^ (interp-compose
;; ;;                                     (Ref-Coercion-Read (Var u))
;; ;;                                     (Quote-Coercion r)))
;; ;;                           (cons w^ (interp-compose
;; ;;                                     (Quote-Coercion w)
;; ;;                                     (Ref-Coercion-Write (Var u)))))
;; ;;                  (If (If (Id-Coercion-Huh (Var w^)) 
;; ;;                          (Id-Coercion-Huh (Var r^))
;; ;;                          (Quote #f))
;; ;;                      (Var o)
;; ;;                      (Guarded-Proxy (Var o)
;; ;;                                     (Coercion (Ref-Coercion (Var r^) (Var w^)))))))
;; ;;              (Guarded-Proxy v (Coercion (Quote-Coercion (Ref r w))))))
;; ;;        (cond
;; ;;          [(Var? e^) (do-ref-coercion-inline e^)]
;; ;;          [else
;; ;;           (define u (next-uid! "guarded-ref"))
;; ;;           (Let (list (cons u e^)) (do-ref-coercion-inline (Var u)))])]
;; ;;       [(Compose-Coercions c1 c2)
;; ;;        (interp-compose (recur c1) (recur c2))]
;; ;;       ;; Transformation to lower guarded reference types
;; ;;       ;;-------------------------------------------------------------------
;; ;;       ;; Every Guarded Reference Starts As an Unguarded box
;; ;;       [(Gbox (app recur e)) (Unguarded-Box e)]
;; ;;       ;; Unboxing calls off to the helpers we have defined
;; ;;       [(Gunbox (app recur b))
;; ;;        (if (Var? b)
;; ;;            (gbox-ref b)
;; ;;            (let ([u (next-uid! "gbox")])
;; ;;              (Let (list (cons u b)) (gbox-ref (Var u)))))]
;; ;;       ;; Setting a Gaurded reference results in iteratively applying
;; ;;       ;; all the guarding casts to the value to be written.
;; ;;       ;; Most of the work is already done but the code requires values
;; ;;       ;; so there is a little repetative to let bind any values that
;; ;;       ;; haven't been evaluated.
;; ;;       [(Gbox-set! (app recur b) (app recur w))
;; ;;        (match-define-values (b* (list b^ w^))
;; ;;          (bnd-non-vars next-uid! (list b w) #:names '("gbox" "write_val")))
;; ;;        (if (null? b*)
;; ;;            (gbox-set! b^ w^)
;; ;;            (Let b* (gbox-set! b^ w^)))]
;; ;;       [(Gvector (app recur size) (app recur init)) (Unguarded-Vect size init)]
;; ;;       [(Gvector-ref (app recur v) (app recur i))
;; ;;        (match-define-values (b* (list v^ i^))
;; ;;          (bnd-non-vars next-uid! (list v i) #:names '("gvec" "index")))
;; ;;        (cond
;; ;;          [(null? b*) (gvect-ref v^ i^)]
;; ;;          [else (Let b* (gvect-ref v^ i^))])]
;; ;;       [(Gvector-set! (app recur v) (app recur i) (app recur w))
;; ;;        (match-define-values (b* (list v^ i^ w^))
;; ;;          (bnd-non-vars next-uid! (list v i w)
;; ;;                        #:names '("gvec" "index" "write_val")))
;; ;;        (if (null? b*)
;; ;;            (gvect-set! v^ i^ w^)
;; ;;            (Let b* (gvect-set! v^ i^ w^)))]
;; ;;       [(Gvector-length e)
;; ;;        (if (Var? e)
;; ;;            (gvect-length e)
;; ;;            (let ([u (next-uid! "gvect")])
;; ;;              (Let (list (cons u (recur e))) (gvect-length (Var u)))))]
;; ;;       [(Mbox (app recur e) t) (Mbox e t)]
;; ;;       [(Munbox (app recur e)) (Mbox-val-ref e)]
;; ;;       [(Mbox-set! (app recur e1) (app recur e2))
;; ;;        (Mbox-val-set! e1 e2)]
;; ;;       [(MBoxCastedRef addr t)
;; ;;        (match-define-values (b* (list t^))
;; ;;          (bnd-non-vars next-uid! (list (Type t)) #:names '("type")))
;; ;;        (if (null? b*)
;; ;;            (mbox-ref (Var addr) t^)
;; ;;            (Let b* (mbox-ref (Var addr) t^)))]
;; ;;       [(MBoxCastedSet! addr (app recur e) t)
;; ;;        (match-define-values (b* (list e^ t^))
;; ;;          (bnd-non-vars next-uid! (list e (Type t)) #:names '("write_val" "type")))
;; ;;        (if (null? b*)
;; ;;            (mbox-set! (Var addr) e^ t^)
;; ;;            (Let b* (mbox-set! (Var addr) e^ t^)))]
;; ;;       [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
;; ;;       [(Mvector-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
;; ;;       [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
;; ;;        (Mvector-val-set! e1 e2 e3)]
;; ;;       [(MVectCastedRef addr (app recur i) t)
;; ;;        (match-define-values (b* (list i^ t^))
;; ;;          (bnd-non-vars next-uid! (list i (Type t)) #:names '("index" "type")))
;; ;;        (if (null? b*)
;; ;;            (mvect-ref (Var addr) i^ t^)
;; ;;            (Let b* (mvect-ref (Var addr) i^ t^)))]
;; ;;       [(MVectCastedSet! addr (app recur i) (app recur e) t)
;; ;;        (match-define-values (b* (list i^ e^ t^))
;; ;;          (bnd-non-vars next-uid! (list i e (Type t)) #:names '("index" "write_val" "type")))
;; ;;        (if (null? b*)
;; ;;            (mvect-set! (Var addr) i^ e^ t^)
;; ;;            (Let b* (mvect-set! (Var addr) i^ e^ t^)))]
;; ;;       [(Mvector-length e) (Mvector-length (recur e))]
      
;; ;;       ;; The translation of the dynamic operation 
;; ;;       [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
;; ;;        (define arg-names
;; ;;          `("blame_info" "dyn_fn" . ,(make-list (length e*) "dyn_fn_arg")))
;; ;;        (match-define-values (b* (cons lbl (cons v v*)))
;; ;;          #;(bnd-non-vars next-uid! (cons e (cons l e*)) #:names arg-names)
;; ;;          (bnd-non-vars next-uid! (cons (Quote l) (cons e e*)) #:names arg-names))
;; ;;        #;(printf "b*=~a\nlbl=~a\nv=~a\nv*=~a\n\n" b* lbl v v*)
;; ;;        (define r (Let b* (dyn-fn-app v v* t* lbl)))
;; ;;        #;(printf "r=~a" r)
;; ;;        r]
;; ;;       [(Dyn-Tuple-Proj (app recur e) (app recur i) (app recur l))
;; ;;        (let*$ ([v e] [i i] [l l]
;; ;;                [u  (ann (dyn-value$ v) CoC3-Expr)]
;; ;;                [ty (ann (dyn-type$ v) CoC3-Expr)])
;; ;;          (cond$
;; ;;           [(ann (and$ (Type-Tuple-Huh ty) (Op '> (list (Type-Tuple-num ty) i)))
;; ;;                 CoC3-Expr)
;; ;;            (let$ ([prj-val (ann (Tuple-proj u i) CoC3-Expr)]
;; ;;                   [prj-ty  (ann (Type-Tuple-item ty i) CoC3-Expr)])
;; ;;              (ann (interp-coercion prj-val (mk-coercion prj-ty DYN-EXPR l) (Quote 0)) CoC3-Expr))]
;; ;;           [else (Blame l)]))]
;; ;;       [(Dyn-GRef-Ref (app recur e) l)
;; ;;        (match-define-values (b* (list e^ l^))
;; ;;          (bnd-non-vars next-uid! (list e (Quote l))
;; ;;                        #:names (list "dyn_gbox" "blame_info")))
;; ;;        (Let b* (dyn-gbox-ref e^ l^))]
;; ;;       [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
;; ;;        (match-define-values (b* (list e1^ e2^ l^))
;; ;;          (bnd-non-vars next-uid! (list e1 e2 (Quote l))
;; ;;                        #:names (list "dyn_gbox" "write_val" "blame_info")))
;; ;;        (Let b* (dyn-gbox-set! e1^ e2^ (Type t) l^))]
;; ;;       [(Dyn-GVector-Ref (app recur e) (app recur i) l)
;; ;;        (match-define-values (b* (list e^ i^ l^))
;; ;;          (bnd-non-vars next-uid! (list e i (Quote l))
;; ;;                        #:names (list "dyn_gvec" "index" "blame_info")))
;; ;;        (define r (Let b* (dyn-gvec-ref e^ i^ l^)))
;; ;;        #;(printf "r=~a\n\n" r)
;; ;;        r]
;; ;;       [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
;; ;;        (match-define-values (b* (list e1^ i^ e2^ l^))
;; ;;          (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
;; ;;                        #:names (list "dyn_gvec" "index" "write_val" "blame_info")))
;; ;;        (Let b* (dyn-gvec-set! e1^ i^ e2^ (Type t) l^))]
;; ;;       [(Dyn-GVector-Len (app recur e) (app recur l))
;; ;;        (let*$ ([v e] [l l]
;; ;;                [u (dyn-value$ v)]
;; ;;                [t (dyn-type$ v)])
;; ;;          (If (Type-GVect-Huh t)
;; ;;              (gvect-length u)
;; ;;              (Blame l)))]
;; ;;       [(Dyn-MRef-Ref (app recur e) l)
;; ;;        (match-define-values (b* (list e^ l^))
;; ;;          (bnd-non-vars next-uid! (list e (Quote l))
;; ;;                        #:names (list "dyn_mbox" "blame_info")))
;; ;;        (Let b* (dyn-mbox-ref e^ l^))]
;; ;;       [(Dyn-MRef-Set! (app recur e1) (app recur e2) t l)
;; ;;        (match-define-values (b* (list e1^ e2^ l^))
;; ;;          (bnd-non-vars next-uid! (list e1 e2 (Quote l))
;; ;;                        #:names (list "dyn_mbox" "write_val" "blame_info")))
;; ;;        (Let b* (dyn-mbox-set! e1^ e2^ (Type t) l^))]
;; ;;       [(Dyn-MVector-Ref (app recur e) (app recur i) l)
;; ;;        (match-define-values (b* (list e^ i^ l^))
;; ;;          (bnd-non-vars next-uid! (list e i (Quote l))
;; ;;                        #:names (list "dyn_mvec" "index" "blame_info")))
;; ;;        (Let b* (dyn-mvec-ref e^ i^ l^))]
;; ;;       [(Dyn-MVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
;; ;;        (match-define-values (b* (list e1^ i^ e2^ l^))
;; ;;          (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
;; ;;                        #:names (list "dyn_mvec" "index" "write_val" "blame_info")))
;; ;;        (Let b* (dyn-mvec-set! e1^ i^ e2^ (Type t) l^))]
;; ;;       ;; We should consider desugaring the data structure rep
;; ;;       ;; here if we are using it. That translation makes more
;; ;;       ;; sense but is weird because we defer hybrid until
;; ;;       ;; closure conversion. 
;; ;;       [(Fn-Proxy i e1 e2)
;; ;;        (Fn-Proxy (list i interp-uid) (recur e1) (recur e2))]
;; ;;       [(App/Fn-Proxy-Huh e e*)
;; ;;        (App-Fn-or-Proxy interp-uid (recur e) (map recur e*))]
;; ;;       [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (recur e))]
;; ;;       [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (recur e))]
;; ;;       ;; In theory it may be better to implement the tranformation
;; ;;       ;; for apply here but it is currently postponed until
;; ;;       ;; closure conversion.
;; ;;       [(App-Fn e e*) (App-Fn (recur e) (map recur e*))]
;; ;;       ;; Uniteresting Recursion till the end
;; ;;       [(Id-Coercion-Huh e)
;; ;;        (Id-Coercion-Huh (recur e))]
;; ;;       [(Fn-Coercion e* e)
;; ;;        (Fn-Coercion (map recur e*) (recur e))]
;; ;;       [(Fn-Coercion-Arg e1 e2)
;; ;;        (Fn-Coercion-Arg (recur e1) (recur e2))]
;; ;;       [(Fn-Coercion-Return e)
;; ;;        (Fn-Coercion-Return (recur e))]
;; ;;       [(Ref-Coercion e1 e2)
;; ;;        (Ref-Coercion (recur e1) (recur e2))]
;; ;;       [(Ref-Coercion-Read e)
;; ;;        (Ref-Coercion-Read (recur e))]
;; ;;       [(Ref-Coercion-Write e)
;; ;;        (Ref-Coercion-Write (recur e))]
;; ;;       [(Quote-Coercion c)
;; ;;        (Quote-Coercion c)]
;; ;;       [(Observe e t) (Observe (recur e) t)]
;; ;;       [(and noop (No-Op)) noop]
;; ;;       [(Code-Label u)
;; ;;        (Code-Label u)]
;; ;;       [(Labels c* e)
;; ;;        (Labels (map recur-through-bndc* c*) (recur e))]
;; ;;       [(App-Code e e*)
;; ;;        (App-Code (recur e) (map recur e*)) ]
;; ;;       [(Lambda f* (Castable ctr e))
;; ;;        (Lambda f* (Castable ctr (recur e)))]
;; ;;       [(Letrec b* e)
;; ;;        (Letrec (map recur-through-bnd* b*) (recur e))]
;; ;;       [(Let b* e)
;; ;;        (Let (map recur-through-bnd* b*) (recur e))]
;; ;;       [(Op p e*)
;; ;;        (Op p (map recur e*))]
;; ;;       [(Fn-Caster e)
;; ;;        (Fn-Caster (recur e))]
;; ;;       ;; Type manipulation
;; ;;       [(Type-Fn-arg e i)
;; ;;        (Type-Fn-arg (recur e) (recur i))]
;; ;;       [(Type-Fn-return e)
;; ;;        (Type-Fn-return (recur e))]
;; ;;       [(Type-Fn-arity e)
;; ;;        (Type-Fn-arity (recur e))]
;; ;;       [(Blame e)
;; ;;        (Blame (recur e))]
;; ;;       [(If tst csq alt)
;; ;;        (If (recur tst) (recur csq) (recur alt))]
;; ;;       [(Switch e c* d)
;; ;;        (: recur-case : (Switch-Case CoC1-Expr) -> (Switch-Case CoC3-Expr))
;; ;;        (define/match (recur-case c)
;; ;;          [((cons l r)) (cons l (recur r))])
;; ;;        (Switch (recur e) (map recur-case c*) (recur d))]
;; ;;       [(Var i) (Var i)]
;; ;;       [(Type t) (Type t)]
;; ;;       [(Quote k) (Quote k)]
;; ;;       [(Begin e* e)
;; ;;        (Begin (map recur e*) (recur e))]
;; ;;       [(Repeat i e1 e2 a e3 e4)
;; ;;        (Repeat i (recur e1) (recur e2) a (recur e3) (recur e4))]
;; ;;       ;; Proxies for functions
;; ;;       [(Fn-Proxy-Coercion e)
;; ;;        (Fn-Proxy-Coercion (recur e))]
;; ;;       [(Create-tuple e*)
;; ;;        (Create-tuple (map recur e*))]
;; ;;       [(Tuple-proj e i) (Tuple-proj (recur e) (Quote i))]
;; ;;       ;; Coercions manipulation
;; ;;       [(or (Guarded-Proxy-Source _) (Guarded-Proxy-Target _)
;; ;;            (Guarded-Proxy-Blames _))
;; ;;        (error 'interpret-casts "twosome code in coercion pass: ~a" exp)]
;; ;;       [other (error 'interpret-casts "umatched ~a" other)]))
;; ;;   (recur exp))


;; ;; ;; TODO: fix smart cast to take the mono rt

;; ;; (define-type Smart-Cast-Type
;; ;;   (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
;; ;;        (#:t1-not-dyn Boolean #:t2-not-dyn Boolean)
;; ;;        CoC3-Expr))
;; ;; (: make-smart-cast :
;; ;;    (String -> Uid)
;; ;;    Coerce-Type
;; ;;    Make-Coercion-Type
;; ;;    -> Smart-Cast-Type)
;; ;; (define ((make-smart-cast next-uid! cast mk-coercion)
;; ;;          val t1 t2 lbl mono_type
;; ;;          #:t1-not-dyn [t1-not-dyn #f]
;; ;;          #:t2-not-dyn [t2-not-dyn #f])
;; ;;   (match* (val t1 t2)
;; ;;     [(val (Type t) (Type t)) val]
;; ;;     [(val (Type (Dyn)) t2) #:when t2-not-dyn
;; ;;      (match val
;; ;;        [(or (Var _) (Quote _)) (cast val (Project-Coercion t2 lbl) mono_type)]
;; ;;        [other
;; ;;         (define u (next-uid! "tmp"))
;; ;;         (Let (list (cons u val))
;; ;;           (cast (Var u) (Project-Coercion t2 lbl) mono_type))])]
;; ;;     [((or (Var _) (Quote _)) (Type (Dyn)) t2)
;; ;;      (If (Type-Dyn-Huh t2) val (cast val (Project-Coercion t2 lbl) mono_type))]
;; ;;     [(val t1 (Type (Dyn))) #:when t1-not-dyn
;; ;;      (cast val (Inject-Coercion t1) mono_type)]
;; ;;     [((Var _) t1 (Type (Dyn)))
;; ;;      (If (Type-Dyn-Huh t1) val (cast val (Inject-Coercion t1) mono_type))]
;; ;;     [(val t1 t2)
;; ;;      (cast val (mk-coercion t1 t2 lbl) mono_type)]))

;; ;; (define-type Dyn-Gbox-refT
;; ;;   ((Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Dyn-Gbox-setT
;; ;;   ((Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Dyn-Gvec-refT
;; ;;   ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Dyn-Gvec-setT
;; ;;   ((Var Uid) (Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid)
;; ;;    -> CoC3-Expr))
;; ;; (define-type Dyn-Fn-AppT
;; ;;   ((Var Uid) (Listof (Var Uid)) Schml-Type* (Var Uid) -> CoC3-Expr))

;; ;; (: make-dyn-gbox-ref-code :
;; ;;    (String -> Uid)
;; ;;    Gbox-refT
;; ;;    Smart-Cast-Type
;; ;;    -> Dyn-Gbox-refT)
;; ;; (define ((make-dyn-gbox-ref-code next-uid! gb-ref $cast) dyn lbl)
;; ;;   (define-values (val ty tyof read-val)
;; ;;     (values (next-uid! "dyn_unbox_val")
;; ;;             (next-uid! "dyn_unbox_ty")
;; ;;             (next-uid! "dyn_unbox_tyof")
;; ;;             (next-uid! "dyn_unbox_read_val")))
;; ;;   (define-values (var-val var-ty var-tyof var-read-val)
;; ;;     (values (Var val) (Var ty) (Var tyof) (Var read-val)))
;; ;;   (Let `((,val . ,(dyn-value$ dyn))
;; ;;          (,ty . ,(dyn-type$ dyn)))
;; ;;     (If (Type-GRef-Huh var-ty)
;; ;;         (Let `([,tyof . ,(Type-GRef-Of var-ty)]
;; ;;                [,read-val . ,(gb-ref var-val)])
;; ;;           ($cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
;; ;;         (Blame lbl))))

;; ;; (: make-dyn-gbox-set!-code :
;; ;;    (String -> Uid)
;; ;;    Gbox-setT
;; ;;    Smart-Cast-Type
;; ;;    -> Dyn-Gbox-setT)
;; ;; (define ((make-dyn-gbox-set!-code next-uid! gb-set! $cast) dyn-gbox wrt-val1 t2 info)
;; ;;   (define-values (gbox ty tyof wrt-val2 wrt-val3)
;; ;;     (values (next-uid! "dyn_setbox_gbox")
;; ;;             (next-uid! "dyn_setbox_ty")
;; ;;             (next-uid! "dyn_setbox_tyof")
;; ;;             (next-uid! "dyn_setbox_wrt_val2")
;; ;;             (next-uid! "dyn_setbox_wrt_val3")))
;; ;;   (define-values (gbox-v ty-v tyof-v wrt-val2-v wrt-val3-v)
;; ;;     (values (Var gbox) (Var ty) (Var tyof) (Var wrt-val2) (Var wrt-val3)))
;; ;;   (Let `((,gbox . ,(dyn-value$ dyn-gbox))
;; ;;          (,ty   . ,(dyn-type$ dyn-gbox)))
;; ;;     (If (Type-GRef-Huh ty-v)
;; ;;         (Let `([,tyof . ,(Type-GRef-Of ty-v)])
;; ;;           (If (Type-Dyn-Huh tyof-v)
;; ;;               (Let `([,wrt-val2 . ,($cast wrt-val1 t2 (Type DYN-TYPE) info (Quote 0))])
;; ;;                 (gb-set! gbox-v wrt-val2-v))
;; ;;               (Let `([,wrt-val3 . ,($cast wrt-val1 t2 tyof-v info (Quote 0) #:t2-not-dyn #t)])
;; ;;                 ($cast (gb-set! gbox-v wrt-val3-v)
;; ;;                        tyof-v (Type DYN-TYPE) info (Quote 0)
;; ;;                        #:t1-not-dyn #t))))
;; ;;         (Blame info))))

;; ;; (: make-dyn-gvect-ref-code :
;; ;;    (String -> Uid)  Gvec-refT Smart-Cast-Type
;; ;;    -> Dyn-Gvec-refT)
;; ;; (define ((make-dyn-gvect-ref-code next-uid! gv-ref $cast) dyn ind lbl)
;; ;;   (define-values (val ty tyof read-val)
;; ;;     (values (next-uid! "dyn_gvec_ref_val")
;; ;;             (next-uid! "dyn_gvec_ref_ty")
;; ;;             (next-uid! "dyn_gvec_ref_tyof")
;; ;;             (next-uid! "dyn_gvec_ref_read_val")))
;; ;;   (define-values (var-val var-ty var-tyof var-read-val)
;; ;;     (values (Var val) (Var ty) (Var tyof) (Var read-val)))
;; ;;   (Let `((,val . ,(dyn-value$ dyn))
;; ;;          (,ty . ,(dyn-type$ dyn)))
;; ;;     (If (Type-GVect-Huh var-ty)
;; ;;         (Let `([,tyof . ,(Type-GVect-Of var-ty)]
;; ;;                [,read-val . ,(gv-ref var-val ind)])
;; ;;           ($cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
;; ;;         (Blame lbl))))

;; ;; (: make-dyn-gvect-set!-code :
;; ;;    (String -> Uid)
;; ;;    Gvec-setT
;; ;;    Smart-Cast-Type
;; ;;    -> Dyn-Gvec-setT)
;; ;; (define ((make-dyn-gvect-set!-code next-uid! gv-set! $cast)
;; ;;          dyn-gvec ind wrt-val1 t2 info)
;; ;;   (define-values (val ty tyof wrt-val2 wrt-val3)
;; ;;     (values (next-uid! "dyn_setbox_gvect_value")
;; ;;             (next-uid! "dyn_setbox_ty")
;; ;;             (next-uid! "dyn_setbox_tyof")
;; ;;             (next-uid! "dyn_setbox_write_value")
;; ;;             (next-uid! "dyn_setbox_write_value3")))
;; ;;   (define-values (val-v ty-v tyof-v wrt-val2-v wrt-val3-v)
;; ;;     (values (Var val) (Var ty) (Var tyof) (Var wrt-val2) (Var wrt-val3)))
;; ;;   (Let `((,val . ,(dyn-value$ dyn-gvec))
;; ;;          (,ty . ,(dyn-type$ dyn-gvec)))
;; ;;     (If (Type-GVect-Huh ty-v)
;; ;;         (Let `([,tyof . ,(Type-GVect-Of ty-v)])
;; ;;           (If (Type-Dyn-Huh tyof-v)
;; ;;               (Let `([,wrt-val2 . ,($cast wrt-val1 t2 (Type DYN-TYPE)
;; ;;                                           info (Quote 0))])
;; ;;                 (gv-set! val-v ind wrt-val2-v))
;; ;;               (Let `([,wrt-val3 . ,($cast wrt-val1 t2 tyof-v
;; ;;                                           info (Quote 0) #:t2-not-dyn #t)])
;; ;;                 ($cast (gv-set! val-v ind wrt-val3-v) 
;; ;;                        tyof-v (Type DYN-TYPE) info
;; ;;                        (Quote 0) #:t1-not-dyn #t))))
;; ;;         (Blame info))))


;; ;; (define-type Dyn-Mbox-refT
;; ;;   ((Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Dyn-Mbox-setT
;; ;;   ((Var Uid) (Var Uid)
;; ;;    (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Dyn-Mvec-refT
;; ;;   ((Var Uid) (Var Uid) (Var Uid)
;; ;;    -> CoC3-Expr))
;; ;; (define-type Dyn-Mvec-setT
;; ;;   ((Var Uid) (Var Uid) (Var Uid)
;; ;;    (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))

;; ;; (: make-dyn-mbox-ref-code :
;; ;;    (String -> Uid)
;; ;;    Mbox-refT
;; ;;    -> Dyn-Mbox-refT)
;; ;; (define ((make-dyn-mbox-ref-code next-uid! mb-ref) dyn lbl)
;; ;;   (define-values (val ty dynty)
;; ;;     (values (next-uid! "dyn_unbox_val")
;; ;;             (next-uid! "dyn_unbox_ty")
;; ;;             (next-uid! "dyn_type")))
;; ;;   (define-values (var-val var-ty var-dynty)
;; ;;     (values (Var val) (Var ty) (Var dynty)))
;; ;;   (Let `((,val . ,(dyn-value$ dyn))
;; ;;          (,ty . ,(dyn-type$ dyn)))
;; ;;     (If (Type-MRef-Huh var-ty)
;; ;;         (Let `([,dynty . ,(Type DYN-TYPE)])
;; ;;           (mb-ref var-val var-dynty))
;; ;;         (Blame lbl))))

;; ;; (: make-dyn-mbox-set!-code :
;; ;;    (String -> Uid)
;; ;;    Mbox-setT
;; ;;    Smart-Cast-Type
;; ;;    -> Dyn-Mbox-setT)
;; ;; (define ((make-dyn-mbox-set!-code next-uid! mb-set! $cast) dyn-mbox wrt-val1 t2 info)
;; ;;   (define-values (mbox ty tyof t2u)
;; ;;     (values (next-uid! "dyn_setbox_mbox")
;; ;;             (next-uid! "dyn_setbox_ty")
;; ;;             (next-uid! "dyn_setbox_tyof")
;; ;;             (next-uid! "t2_type")))
;; ;;   (define-values (mbox-v ty-v tyof-v t2u-v)
;; ;;     (values (Var mbox) (Var ty) (Var tyof) (Var t2u)))
;; ;;   (Let `([,mbox . ,(dyn-value$ dyn-mbox)]
;; ;;          [,ty   . ,(dyn-type$ dyn-mbox)])
;; ;;     (If (Type-MRef-Huh ty-v)
;; ;;         (Let `([,tyof . ,(Type-MRef-Of ty-v)]
;; ;;                [,t2u .  ,t2])
;; ;;           (If (Type-Dyn-Huh tyof-v)
;; ;;               ($cast (mb-set! mbox-v wrt-val1 t2u-v)
;; ;;                      (Type UNIT-TYPE) (Type DYN-TYPE) info
;; ;;                      (Quote 0))
;; ;;               (mb-set! mbox-v wrt-val1 t2u-v)))
;; ;;         (Blame info))))

;; ;; (: make-dyn-mvect-ref-code :
;; ;;    (String -> Uid)  Mvec-refT
;; ;;    -> Dyn-Mvec-refT)
;; ;; (define ((make-dyn-mvect-ref-code next-uid! mv-ref) dyn ind lbl)
;; ;;   (define-values (val ty dynty)
;; ;;     (values (next-uid! "dyn_mvec_ref_val")
;; ;;             (next-uid! "dyn_mvec_ref_ty")
;; ;;             (next-uid! "dyn_type")))
;; ;;   (define-values (var-val var-ty var-dynty)
;; ;;     (values (Var val) (Var ty) (Var dynty)))
;; ;;   (Let `((,val . ,(dyn-value$ dyn))
;; ;;          (,ty . ,(dyn-type$ dyn)))
;; ;;     (If (Type-MVect-Huh var-ty)
;; ;;         (Let `([,dynty . ,(Type DYN-TYPE)])
;; ;;           (mv-ref var-val ind var-dynty))
;; ;;         (Blame lbl))))

;; ;; (: make-dyn-mvect-set!-code :
;; ;;    (String -> Uid)
;; ;;    Mvec-setT
;; ;;    Smart-Cast-Type
;; ;;    -> Dyn-Mvec-setT)
;; ;; (define ((make-dyn-mvect-set!-code next-uid! mv-set! $cast)
;; ;;          dyn-mvec ind wrt-val1 t2 info)
;; ;;   (define-values (val ty tyof t2u)
;; ;;     (values (next-uid! "dyn_setbox_mvect_value")
;; ;;             (next-uid! "dyn_setbox_ty")
;; ;;             (next-uid! "dyn_setbox_tyof")
;; ;;             (next-uid! "t2_type")))
;; ;;   (define-values (val-v ty-v tyof-v t2u-v)
;; ;;     (values (Var val) (Var ty) (Var tyof) (Var t2u)))
;; ;;   (Let `((,val . ,(dyn-value$ dyn-mvec))
;; ;;          (,ty . ,(dyn-type$ dyn-mvec)))
;; ;;     (If (Type-MVect-Huh ty-v)
;; ;;         (Let `([,tyof . ,(Type-MVect-Of ty-v)]
;; ;;                [,t2u .  ,t2])
;; ;;           (If (Type-Dyn-Huh tyof-v)
;; ;;               ($cast (mv-set! val-v ind wrt-val1 t2u-v)
;; ;;                      (Type UNIT-TYPE) (Type DYN-TYPE) info (Quote 0))
;; ;;               (mv-set! val-v ind wrt-val1 t2u-v)))
;; ;;         (Blame info))))


;; ;; (: make-dyn-fn-app-code : (String -> Uid) Smart-Cast-Type -> Dyn-Fn-AppT)
;; ;; (define ((make-dyn-fn-app-code next-uid! $cast) v v* t* l)
;; ;;   (define-values (val ty ret-val ret-ty)
;; ;;     (values (next-uid! "dyn_fn_val")
;; ;;             (next-uid! "dyn_fn_ty")
;; ;;             (next-uid! "dyn_fn_ret_val")
;; ;;             (next-uid! "dyn_fn_ret_ty")))
;; ;;   (define arg-casts : CoC3-Expr*
;; ;;     (for/list : (Listof CoC3-Expr)
;; ;;               ([v : CoC3-Expr v*]
;; ;;                [t : Schml-Type t*]
;; ;;                [i (in-naturals)])
;; ;;       (define tyi (next-uid! (string-append "dyn_fn_ty" (number->string i))))
;; ;;       (Let `([,tyi . ,(Type-Fn-arg (Var ty) (Quote i))])
;; ;;         ($cast v (Type t) (Var tyi) l (Quote 0)))))
;; ;;   (define casts-apply : CoC3-Expr
;; ;;     (case (function-cast-representation)
;; ;;       [(Hybrid) (App-Fn (Var val) arg-casts)]
;; ;;       [(Data) (error 'todo "implement coercions data representation")]
;; ;;       [(Functional) (error 'todo "incompatible with coercions")]
;; ;;       [else (error 'interp-cast-with-coercions/dyn-fn-app "unexpected value")]))
;; ;;   (Let `([,val . ,(dyn-value$ v)]
;; ;;          [,ty . ,(dyn-type$ v)])
;; ;;     (If (Type-Fn-Huh (Var ty))
;; ;;         (Let `([,ret-val . ,casts-apply]
;; ;;                [,ret-ty . ,(Type-Fn-return (Var ty))])
;; ;;           ($cast (Var ret-val) (Var ret-ty) (Type DYN-TYPE) l
;; ;;                  (Quote 0)))
;; ;;         (Blame l))))


;; ;; ;; Functions for use sites of guarded references with coercions
;; ;; (define-type Gbox-refT    ((Var Uid) -> CoC3-Expr))
;; ;; (define-type Gbox-setT    ((Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Gvec-refT    ((Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Gvec-setT    ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Gvec-LengthT ((Var Uid) -> CoC3-Expr))

;; ;; (: make-gbox-ref-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gbox-refT))
;; ;; (define ((make-gbox-ref-code cast) gref)
;; ;;   (If (Guarded-Proxy-Huh gref)
;; ;;       (cast
;; ;;        (Unguarded-Box-Ref (Guarded-Proxy-Ref gref))
;; ;;        (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))
;; ;;        (Quote 0))
;; ;;       (Unguarded-Box-Ref gref)))

;; ;; (: make-gbox-set!-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gbox-setT))
;; ;; (define ((make-gbox-set!-code cast) gref val)
;; ;;   (If (Guarded-Proxy-Huh gref)
;; ;;       (Unguarded-Box-Set!
;; ;;        (Guarded-Proxy-Ref gref)
;; ;;        (cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)) (Quote 0)))
;; ;;       (Unguarded-Box-Set! gref val)))

;; ;; (: make-gvect-ref-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gvec-refT))
;; ;; (define ((make-gvect-ref-code cast) gref index)
;; ;;   (If (Guarded-Proxy-Huh gref)
;; ;;       (cast (Unguarded-Vect-Ref (Guarded-Proxy-Ref gref) index)
;; ;;             (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))
;; ;;             (Quote 0))
;; ;;       (Unguarded-Vect-Ref gref index)))

;; ;; (: make-gvect-length-code (-> Gvec-LengthT))
;; ;; (define ((make-gvect-length-code) gvect)
;; ;;   (If (Guarded-Proxy-Huh gvect)
;; ;;       (Unguarded-Vect-length (Guarded-Proxy-Ref gvect))
;; ;;       (Unguarded-Vect-length gvect)))

;; ;; (: make-gvect-set!-code ((CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr) -> Gvec-setT))
;; ;; (define ((make-gvect-set!-code cast) gref index val)
;; ;;   (: index-exp CoC3-Expr)
;; ;;   (define index-exp (if (integer? index) (Quote index) index))
;; ;;   (If (Guarded-Proxy-Huh gref)
;; ;;       (Unguarded-Vect-Set!
;; ;;        (Guarded-Proxy-Ref gref)
;; ;;        index-exp
;; ;;        (cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)) (Quote 0)))
;; ;;       (Unguarded-Vect-Set! gref index-exp val)))

;; ;; (define-type Mbox-refT    ((Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Mbox-setT    ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Mvec-refT    ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
;; ;; (define-type Mvec-setT    ((Var Uid) (Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

;; #;
;; (: make-mbox-ref-code
;;    ((String -> Uid)
;;     Cast-With-MAddr-Type
;;     -> Mbox-refT))
;; #;
;; (define ((make-mbox-ref-code next-uid! cast) mref t2)
;;   (let ([t1 (next-uid! "t1")]
;;         [c  (next-uid! "crcn")])
;;     (Let (list (cons t1 (Mbox-rtti-ref mref)))
;;       (cast (Mbox-val-ref mref) (Var t1) t2 (Quote "") (Quote 0)))))

;; #;
;; (: make-mbox-set!-code
;;    ((String -> Uid)
;;     Cast-With-MAddr-Type
;;     -> Mbox-setT))
;; #;
;; (define ((make-mbox-set!-code next-uid! cast) mref val t1)
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
;;           [ccv (cast cv t1 t2 (Quote "monotonic references currently does not track blame") mref)]
;;           [t2-new (Mbox-rtti-ref mref)])
;;     (begin
;;       (if$ (op=? t2 t2-new)
;;            (Mbox-val-set! mref ccv)
;;            (Quote 0)))))

;; #;
;; (: make-mvect-ref-code
;;    ((String -> Uid)
;;     Cast-With-MAddr-Type
;;     -> Mvec-refT))
;; #;
;; (define ((make-mvect-ref-code next-uid! cast) mvect i t2)
;;   (let ([t1 (next-uid! "t1")]
;;         [c  (next-uid! "crcn")])
;;     (Let (list (cons t1 (Mvector-rtti-ref mvect)))
;;       (cast (Mvector-val-ref mvect i) (Var t1) t2 (Quote "") (Quote 0)))))

;; #;
;; (: make-mvect-set!-code
;;    ((String -> Uid)
;;     Cast-With-MAddr-Type
;;     -> Mvec-setT))
;; #;
;; (define ((make-mvect-set!-code next-uid! cast) mvect i val t1)
;;   (let*$ ([t2 (Mvector-rtti-ref mvect)])
;;     (cond$
;;      [(and$ (tupleT?$ t1) (tupleT?$ t2))
;;       (let*$ ([n (Type-Tuple-num t2)]
;;               [cvi (Copy-Tuple n val)])
;;         (Begin
;;           (list
;;            (Mvector-val-set! mvect i cvi))
;;           (let*$ ([ccvi (cast
;;                          cvi t1 t2
;;                          (Quote "monotonic references currently does not track blame")
;;                          mvect)]
;;                   [t2-new (Mvector-rtti-ref mvect)])
;;             (if$ (op=? t2 t2-new)
;;                  (Mvector-val-set! mvect i ccvi)
;;                  (Quote 0)))))]
;;      [else
;;       (let*$ ([cvi (cast
;;                     val t1 t2
;;                     (Quote "monotonic references currently does not track blame")
;;                     mvect)]
;;               [t2-new (Mvector-rtti-ref mvect)])
;;         (if$ (op=? t2 t2-new)
;;              (Mvector-val-set! mvect i cvi)
;;              (Quote 0)))])))
