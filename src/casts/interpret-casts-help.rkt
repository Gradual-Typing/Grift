#lang typed/racket/base
(require
 racket/match
 racket/format
 racket/list
 "../language/cast-or-coerce3.rkt"

 "../language/cast0.rkt"
;; "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../unique-identifiers.rkt"
 "../language/cast-or-coerce3.rkt"
 (submod "../logging.rkt" typed))

(provide (all-defined-out))

;; Configuration that spans multiple passes
(: specialize-casts? (Parameterof Boolean))
(define specialize-casts? (make-parameter #f))

(define-type Function-Proxy-Rep (U 'Data 'Hybrid 'Functional))
(: function-cast-representation (Parameterof Function-Proxy-Rep))
(define function-cast-representation
  (make-parameter 'Hybrid))



;; Expr$ and cond$ will prune branches if the condition is (Quote #t) or (Quote #f)
;; and generates If statements (to construct the cast tree)
(: if/specialization (CoC3-Expr (-> CoC3-Expr) (-> CoC3-Expr) -> CoC3-Expr))
(define (if/specialization t c a)
  (if (Quote? t)
      (if (Quote-literal t) (c) (a))
      (If t (c) (a))))

;; better syntax for calling if/specialization
(define-syntax-rule (if$ t c a)
  (if/specialization t (lambda () c) (lambda () a)))

;; Pure version of the above use for programming in
;; the object language with and/or
(: if$_ (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (if$_ t c a)
  (if (Quote? t)
      (if (Quote-literal t) c a)
      (If t c a)))

;; Warning and$ doesn't evaluate the first expression
;; if the second is the literal false
(: and$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (and$ fst snd)
  (match* (fst snd)
    [((Quote #f) _) fst]
    [(_ (Quote #f)) snd]
    [((Quote #t) _) snd]
    [(_ (Quote #t)) fst]
    [(_ _) (If fst snd (Quote #f))]))

(: or$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (or$ fst snd)
  (match* (fst snd)
    [((Quote #f) _) snd]
    [(_ (Quote #f)) fst]
    [((Quote #t) _) fst]
    [(_ (Quote #t)) snd]
    [(_ _) (If fst snd (Quote #f))]))

(define-syntax cond$
  (syntax-rules (else)
    [(_ (else c)) c]
    [(_ (t c) (t* c*) ...)
     (if$ t c (cond$ (t* c*) ...))]))

;; make sure that t is a trivial expression
(define-type CoC3-Trivial (U (Quote-Coercion Schml-Coercion)
                             (Quote Cast-Literal)
                             (Tag Tag-Symbol)
                             (Type Schml-Type)
                             (Var Uid)))

;; This is meant to help let$* which accumulates the list of
;; bindings backwards. This gives proper let* behavior
(: let$*-help (CoC3-Bnd* CoC3-Expr -> CoC3-Expr))
(define (let$*-help b* b)
  (if (null? b*)
      b
      (let$*-help (cdr b*) (Let (list (car b*)) b))))

(: make-trivialize :
   (String -> Uid)
   -> 
   (String CoC3-Expr (Listof (Pairof Uid CoC3-Expr))
           ->
           (Values CoC3-Bnd* CoC3-Trivial)))
(define ((make-trivialize next-uid!) name expr bnd*)
  (if (or (Quote? expr) (Tag? expr) (Type? expr)
          (Var? expr) (Quote-Coercion? expr))
      (values bnd* expr)
      (let ([uvar (next-uid! name)])
        (values (cons (cons uvar expr) bnd*) (Var uvar)))))

;; createsCoC3-Expr let bindings for non-trivial CoC3-Expr expressions,
;; since non-trivial expressions must be evaluated only once.
;; This has to be a macro because it plays with what value is bound to
;; the t* variable in the code in order to reduce the number of cases
;; that must be handle
(define-syntax-rule (define-syntax-let$* let$* next-uid!)
  (... ;; Quote-ellipsis
   (begin
     (define trivialize (make-trivialize next-uid!))
     (: let$*-help ((Listof (Pairof Uid CoC3-Expr)) CoC3-Expr -> CoC3-Expr))
     (define (let$*-help b* b)
       (if (null? b*)
           b
           (let$*-help (cdr b*) (Let (list (car b*)) b))))
     (define-syntax let$*
       (syntax-rules ()
         [(_ () b) b]
         [(_ ([t* v*] ...) b)
          (let ([b* : (Listof (Pairof Uid CoC3-Expr)) '()])
            (let*-values ([(b* t*) (trivialize (~a 't*) v* b*)] ...)
              (let ([body : CoC3-Expr b])
                (let$*-help b* body))))])))))

;; This uses the dynamically scoped next-uid! which looks at the
;; current-next-unique-counter variable.
;; Old way of defining new syntax each time you want to use
;; let$* blows. TODO get rid of all uses of the old macro so that
;; we can define this more simply.
(define-syntax-let$* let$* next-uid!)

;; performs compile time folding of prim = on literals
;; todo convert this to using eq-huh
;; TODO this should be op=?$
(: op=? (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (op=? o x)
  (cond
    [(and (Quote? o) (Quote? x)) (Quote (eq? (Quote-literal o)
                                             (Quote-literal x)))]
    [(and (Tag? o) (Tag? x)) (Quote (eq? (Tag-bits o) (Tag-bits x)))]
    [(and (Type? o) (Type? x)) (Quote (equal? (Type-type o) (Type-type x)))]
    [else (Op '= (list o x))]))

(: op<=? (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (op<=? o x)
  (cond
    [(and (Quote? o) (Quote? x))
     (let ([n1 (Quote-literal o)]
           [n2 (Quote-literal x)])
       (if (and (integer? n1) (integer? n2))
           (Quote (<= n1 n2))
           (error 'interpret-casts/op<=? "Unexpected ~a ~a" n1 n2)))]
    [else (Op '<= (list o x))]))

;;These functions type to fold predicates in order to allow ifs to
;;generate only checks that are actually needed
(: type-tag (-> CoC3-Expr CoC3-Expr))
(define (type-tag o)
  (if (Type? o)
      (let ([v (Type-type o)])
        (cond
          [(or (Dyn? v) (Int? v) (Bool? v) (Unit? v)
               (Character? v)(Float? v))
           (Tag 'Atomic)]
          [(GRef? v) (Tag 'GRef)]
          [(GVect? v) (Tag 'GVect)]
          [(Fn? v) (Tag 'Fn)]
          [(MRef? v) (Tag 'MRef)]
          [(MVect? v) (Tag 'MVect)]
          [(STuple? v) (Tag 'STuple)]
          [else (error 'interpret-casts/type-tag
                       "Unexpected ~a" v)]))
      (Type-Tag o)))

(: type-fn-arity (CoC3-Expr -> CoC3-Expr))
(define (type-fn-arity o)
  (cond
    [(not (Type? o)) (Type-Fn-arity o)]
    [(Fn? (Type-type o)) (Quote (Fn-arity (Type-type o)))]
    [else (error 'ic/type-fn-arity "bad value?: ~a" o)]))

(: type-tuple-num (CoC3-Expr -> CoC3-Expr))
(define (type-tuple-num o)
  (cond
    [(not (Type? o)) (Type-Tuple-num o)]
    [(STuple? (Type-type o))  (Quote (STuple-num (Type-type o)))]
    [else (error 'ic/type-tuple-num "bad value?: ~a" o)]))


(define-syntax-rule
  (define-smart-coercion? (name compile-time? run-time?) ...)
  (begin
    (: name (CoC3-Expr -> CoC3-Expr)) ...
    (define (name x)
      (if (not (Quote-Coercion? x))
          (run-time? x)
          (let ([x (Quote-Coercion-const x)])
            (if (compile-time? x)
                (Quote #t)
                (Quote #f))))) ...))

(define-smart-coercion?
  (id?$              Identity?       Id-Coercion-Huh)
  (seq?$             Sequence?       Sequence-Coercion-Huh)
  (prj?$             Project?        Project-Coercion-Huh)
  (inj?$             Inject?         Inject-Coercion-Huh)
  (fail?$            Failed?         Failed-Coercion-Huh)
  (fnC?$             Fn?             Fn-Coercion-Huh)
  (ref?$             Ref?            Ref-Coercion-Huh)
  (mrefC?$           MonoRef?        MRef-Coercion-Huh)
  (mvectC?$          MonoVect?       MVect-Coercion-Huh)
  (tuple?$           CTuple?         Tuple-Coercion-Huh)
  (mediating-crcn?$  mediating-crcn? Mediating-Coercion-Huh))

(define (mediating-crcn? x)
  (or (CTuple? x) (Ref? x) (Fn? x) (MonoRef? x) (MonoVect? x)))

(define-syntax-rule
  (define-smart-access (name check kuote compile-time run-time) ...)
  (begin
    (: name (CoC3-Expr -> CoC3-Expr)) ...
    (define (name x)
      (if (not (Quote-Coercion? x))
          (run-time x)
          (let ([x (Quote-Coercion-const x)])
            (if (check x)
                (kuote (compile-time x))
                (error 'interpret-casts "~a applied to ~a" 'name x)))))
    ...))

(define-smart-access
  (seq-fst$     Sequence?  Quote-Coercion Sequence-fst  Sequence-Coercion-Fst)
  (seq-snd$     Sequence?  Quote-Coercion Sequence-snd  Sequence-Coercion-Snd)
  (prj-type$    Project?   Type           Project-type  Project-Coercion-Type)
  (prj-label$   Project?   Quote          Project-label Project-Coercion-Label)
  (inj-type$    Inject?    Type           Inject-type   Inject-Coercion-Type)
  (ref-read$    Ref?       Quote-Coercion Ref-read      Ref-Coercion-Read)
  (ref-write$   Ref?       Quote-Coercion Ref-write     Ref-Coercion-Write)
  (mrefC-type$  MonoRef?   Type           MonoRef-type  MRef-Coercion-Type)
  (mvectC-type$ MonoVect?  Type           MonoVect-type MVect-Coercion-Type)
  (tuple-num$   CTuple?    Quote          CTuple-num    Tuple-Coercion-Num)
  (fail-label$  Failed?    Quote          Failed-label  Failed-Coercion-Label))

(: tuple-crcn-arg$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (tuple-crcn-arg$ x i)
  (if (and (Quote-Coercion? x) (Quote? i))
      (let ([x (Quote-Coercion-const x)]
            [i (Quote-literal i)])
        (if (and (CTuple? x) (integer? i) (index? i))
            (Quote-Coercion (list-ref (CTuple-items x) i))
            (error 'interpret-casts "~a applied to ~a" 'tuple-crcn x)))
      (Tuple-Coercion-Item x i)))

(: tuple-type-arg$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (tuple-type-arg$ x i)
  (if (and (Type? x) (Quote? i))
      (let ([x (Type-type x)]
            [i (Quote-literal i)])
        (if (and (STuple? x) (integer? i) (index? i))
            (Type (list-ref (STuple-items x) i))
            (error 'interpret-casts "~a applied to ~a" 'tuple-type x)))
      (Type-Tuple-item x i)))

(define-syntax-rule
  (define-smart-coercion (name compile-time run-time field ...) ...)
  (begin
    (define (name (field : CoC3-Expr) ...) : CoC3-Expr
      (if (and (Quote-Coercion? field) ...)
          (Quote-Coercion (compile-time (Quote-Coercion-const field) ...))
          (run-time field ...)))
    ...))

(define-smart-coercion
  (seq$  Sequence Sequence-Coercion fst snd)
  (ref$  Ref      Ref-Coercion      read write))

(: mrefC$ (CoC3-Expr -> CoC3-Expr))
(define (mrefC$ type)
  (if (Type? type)
      (let ([t (Type-type type)])
        (Quote-Coercion (MonoRef t)))
      (MRef-Coercion type)))

(: mvectC$ (CoC3-Expr -> CoC3-Expr))
(define (mvectC$ type)
  (if (Type? type)
      (let ([t (Type-type type)])
        (Quote-Coercion (MonoVect t)))
      (MVect-Coercion type)))

(: inj$ (CoC3-Expr -> CoC3-Expr))
(define (inj$ type)
  (if (Type? type)
      (let ([t (Type-type type)])
        (Quote-Coercion (Inject t)))
      (Inject-Coercion type)))

(: prj$ (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define (prj$ type label)
  (if (and (Type? type) (Quote? label))
      (let ([t (Type-type type)]
            [s (Quote-literal label)])
        (if (string? s)
            (Quote-Coercion (Project t s))
            (error 'proj$ "given ~a ~a" t s)))
      (Project-Coercion type label)))

(: fail$ (CoC3-Expr -> CoC3-Expr))
(define (fail$ label)
  (cond
    [(not (Quote? label)) (Failed-Coercion label)]
    [else
     (let ([s (Quote-literal label)])
       (if (string? s)
           (Quote-Coercion (Failed s))
           (error 'fail$ "given ~a" s)))]))

(define-syntax-rule
  (define-smart-type? (name compile-time? run-time?) ...)
  (begin
    (begin
      (: name (CoC3-Expr -> CoC3-Expr))
      (define (name x)
        (if (not (Type? x))
            (run-time? x)
            (let ([x (Type-type x)])
              (Quote (compile-time? x))))))
    ...))

(define-smart-type?
  (dyn?$    Dyn?    Type-Dyn-Huh)
  (fnT?$    Fn?     Type-Fn-Huh)
  (gvect?$  GVect?  Type-GVect-Huh)
  (gref?$   GRef?   Type-GRef-Huh)
  (mref?$   MRef?   Type-MRef-Huh)
  (mvect?$  MVect?  Type-MVect-Huh)
  (tupleT?$ STuple? Type-Tuple-Huh))

(: fnT-arity$ (CoC3-Expr -> CoC3-Expr))
(define (fnT-arity$ x)
  (if (not (Type? x))
      (Type-Fn-arity x)
      (let ([x (Type-type x)])
        (if (Fn? x)
            (Quote (Fn-arity x))
            (error 'tupleT-num$ "given ~a" x)))))

(: tupleT-num$ (CoC3-Expr -> CoC3-Expr))
(define (tupleT-num$ x)
  (if (not (Type? x))
      (Type-Tuple-num x)
      (let ([x (Type-type x)])
        (if (STuple? x)
            (Quote (STuple-num x))
            (error 'tupleT-num$ "given ~a" x)))))

(: gvect-of$ (CoC3-Expr -> CoC3-Expr))
(define (gvect-of$ x)
  (if (not (Type? x))
      (Type-GVect-Of x)
      (let ([x (Type-type x)])
        (if (GVect? x)
            (Type (GVect-arg x))
            (error 'gvect-of$ "given ~a" x)))))

(: gref-of$ (CoC3-Expr -> CoC3-Expr))
(define (gref-of$ x)
  (if (not (Type? x))
      (Type-GRef-Of x)
      (let ([x (Type-type x)])
        (if (GRef? x)
            (Type (GRef-arg x))
            (error 'gref-of$ "given ~a" x)))))

(: mref-of$ (CoC3-Expr -> CoC3-Expr))
(define (mref-of$ x)
  (if (not (Type? x))
      (Type-MRef-Of x)
      (let ([x (Type-type x)])
        (if (MRef? x)
            (Type (MRef-arg x))
            (error 'mref-of$ "given ~a" x)))))

(: mvect-of$ (CoC3-Expr -> CoC3-Expr))
(define (mvect-of$ x)
  (if (not (Type? x))
      (Type-MVect-Of x)
      (let ([x (Type-type x)])
        (if (MVect? x)
            (Type (MVect-arg x))
            (error 'mvect-of$ "given ~a" x)))))

;; building types, I do not think we will ever need to build them in
;; compile time, but monotonic references need to build them in runtime
(define-syntax-rule
  (define-smart-type (name compile-time run-time field ...) ...)
  (begin
    (define (name (field : CoC3-Expr) ...) : CoC3-Expr
      (if (and (Type? field) ...)
          (Type (compile-time (Type-type field) ...))
          (run-time field ...)))
    ...))

(define-smart-type
  (gref$  GRef  Type-GRef  type)
  (gvect$ GVect Type-GVect type)
  (mref$  MRef  Type-MRef  type)
  (mvect$ MVect Type-MVect type))

(: bnd-non-vars
   (((String -> Uid) CoC3-Expr*) (#:names (Option (Listof String)))
    . ->* .
    (Values CoC3-Bnd* (Listof (Var Uid)))))
(define (bnd-non-vars next-uid! e* #:names [names? #f])
  (define names : (Listof String) (or names? (make-list (length e*) "tmp")))
  (define-values (bnd* var*)
    (for/fold ([bnd* : CoC3-Bnd* '()]
               [var* : (Listof (Var Uid)) '()])
              ([e : CoC3-Expr e*]
               [n : String names])
      (cond
        [(Var? e) (values bnd* (cons e var*))]
        [else
         (let ([u (next-uid! n)])
           (values (cons (cons u e) bnd*) (cons (Var u) var*)))])))
  #;(printf "bnd-non-vars:\ne*=~a\nnames=~a\nbnd*=~a\nvar*=~a\n\n"
            e* names? bnd* var*)
  (values bnd* (reverse var*)))


(: apply-code
   (All (A)
     (Uid -> (->* () #:rest A (App-Code (Code-Label Uid) (Listof A))))))
(define ((apply-code u) . a*)
  (App-Code (Code-Label u) a*))

(define-type GreatestLowerBound-Type (CoC3-Trivial CoC3-Trivial -> CoC3-Expr))

(: gen-greatest-lower-bound-type-code : (String -> Uid) GreatestLowerBound-Type Uid -> GreatestLowerBound-Type)
(define ((gen-greatest-lower-bound-type-code next-uid! glbt glbt-uid) t1 t2)
  (define-syntax-let$* let$* next-uid!)
  (cond$
   [(op=? t1 t2) t1]
   [(dyn?$ t1) t2]
   [(dyn?$ t2) t1]
   [(and$ (fnT?$ t1) (fnT?$ t2))
    (if$ (op=? (fnT-arity$ t1) (fnT-arity$ t2))
         (Make-Fn-Type glbt-uid t1 t2)
         (Error (Quote "can not compute the greatest lower bound for function types with mismatch arities")))]
   [(and$ (gref?$ t1) (gref?$ t2))
    (let$* ([gr1_of (gref-of$ t1)]
            [gr2_of (gref-of$ t2)]
            [t  (glbt gr1_of gr2_of)])
      (gref$ t))]
   [(and$ (gvect?$ t1) (gvect?$ t2))
    (let$* ([gr1_of (gvect-of$ t1)]
            [gr2_of (gvect-of$ t2)]
            [t  (glbt gr1_of gr2_of)])
      (gvect$ t))]
   [(and$ (mref?$ t1) (mref?$ t2))
    (let$* ([gr1_of (mref-of$ t1)]
            [gr2_of (mref-of$ t2)]
            [t  (glbt gr1_of gr2_of)])
      (mref$ t))]
   [(and$ (mvect?$ t1) (mvect?$ t2))
    (let$* ([gr1_of (mvect-of$ t1)]
            [gr2_of (mvect-of$ t2)]
            [t  (glbt gr1_of gr2_of)])
      (mvect$ t))]
   [(and$ (tupleT?$ t1) (tupleT?$ t2))
    (if$ (op<=? (tupleT-num$ t2) (tupleT-num$ t1))
         (Make-Tuple-Type glbt-uid t1 t2)
         (Error (Quote "can not compute the greatest lower bound for tuple types with inconsistent sizes")))]
   [else (Error (Quote "inconsistent types"))]))

(define-type CopyValueInMonoRef-Type (CoC3-Expr -> CoC3-Expr))

(: gen-copy-value-in-monoref-code : (String -> Uid) -> CopyValueInMonoRef-Type)
(define ((gen-copy-value-in-monoref-code next-uid!) a-var)
  (define-syntax-let$* let$* next-uid!)
  (match a-var
    [(Var a) (let$* ([t (Mbox-rtti-ref a-var)]
                     [v (Mbox-val-ref a-var)])
               (cond$
                [(tupleT?$ t)
                 (let$* ([n (Type-Tuple-num t)]
                         [cv (Copy-Tuple n v)])
                   (Begin
                     (list
                      (Mbox-val-set! a-var cv))
                     cv))]
                [else v]))]
    [other (error 'copy-value-in-monoref "unmatched value ~a" other)]))


(define-type Compile-Cast-Type
  (->* (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr)
       (#:t1-not-dyn Boolean #:t2-not-dyn Boolean)
       CoC3-Expr))

(define-type Compile-Make-Coercion-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type Compile-Lambda-Type (Uid* CoC3-Expr -> CoC3-Expr)) 
(define-type Compile-App-Type (CoC3-Expr CoC3-Expr* -> CoC3-Expr))

(define-type Dyn-PBox-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PBox-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PVec-Ref-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PVec-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MBox-Ref-Type
  (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MBox-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MVec-Ref-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-MVec-Set-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-PVec-Len-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Dyn-Fn-App-Type
  (CoC3-Expr CoC3-Expr* Schml-Type* CoC3-Expr -> CoC3-Expr))
(define-type Dyn-Tup-Prj-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))

(define-type MBox-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MBox-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MVec-Ref-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type MVec-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))


;; This next section of code are the procedures that build all
;; of the runtime code for hyper coercions based casting.

(define-type Interp-Compose-Coercions-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Interp-Cast-Type (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Interp-Make-Coercion-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Greatest-Lower-Bound-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Copy-Mref-Type (CoC3-Expr -> CoC3-Expr))
(define-type Make-Coercion-Type (Schml-Type Schml-Type Blame-Label -> CoC3-Expr))

;; Functions for use sites of guarded references with coercions
(define-type PBox-Ref-Type (CoC3-Expr -> CoC3-Expr))
(define-type PBox-Set-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type PVec-Ref-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type PVec-Set-Type (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type PVec-Len-Type (CoC3-Expr -> CoC3-Expr))

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
