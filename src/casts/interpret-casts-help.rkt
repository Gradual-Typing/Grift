#lang typed/racket/base
(require
 racket/match
 racket/format
 racket/list
 "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast-or-coerce3.rkt")

(provide (all-defined-out))

;; Configuration that spans multiple passes
(: specialize-casts? (Parameterof Boolean))
(define specialize-casts? (make-parameter #f))



;; inline-guarded-branch
;; Parameter determining if the code generated for gbox-set! and gunbox
;; performs the first check to see if the gref is a unguarded reference
;; or delegates the entire operation to the runtime.
;; This is only used for the twosome representation
(: inline-guarded-branch? (Parameterof Boolean))
(define inline-guarded-branch? (make-parameter #f))

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
#|
;(: trivial? (CoC3-Expr -> Boolean : Trivial))
(define (trivial? x)
  (or (Quote? x)
      (Tag? x)
      (Type? x)
      (Var? x)))

(: trivialize ((Box Nat) String CoC3-Expr CoC3-Bnd* ->
               (Pair CoC3-Bnd* let$-Trivial)))
(define (trivialize next s e b*)
  (if (or (Quote? e) (Tag? e) (Type? e) (Var? e))
      `(,b* . ,e)
      (let ([u (unbox next)])
        (set-box! next (add1 u))
        ())
      (bind-state
       (uid-state s)
       (lambda ([u : Uid])
         : (State Nat (Pair CoC3-Bnd* (Var Uid)))
         (return-state `(((,u . ,e) . ,b*) . ,(Var u)))))))

;; This is meant to help let$* which accumulates the list of
;; bindings backwards. This gives proper let* behavior
(: let$*-help (CoC3-Bnd* CoC3-Expr -> CoC3-Expr))
(define (let$*-help b* b)
  (if (null? b*)
      b
      (let$*-help (cdr b*) (Let (list (car b*)) b))))
|#

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

#|
(do (bind-state : (State Nat CoC3-Expr))
    ((cons b* t*) : (Pair CoC3-Bnd* CoC3-Trivial)
     <- (trivialize (~a 't*) v* b*)) ...
     (b^ : CoC3-Expr <- b)
     (if (null? b*)
         (return-state b^)
         (return-state (let$*-help b* b^))))|#



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
          [(or (Dyn? v) (Int? v) (Bool? v) (Unit? v))
           (Tag 'Atomic)]
          [(GRef? v) (Tag 'GRef)]
          [(GVect? v) (Tag 'GVect)]
          [(Fn? v) (Tag 'Fn)]
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
  (tuple?$           CTuple?         Tuple-Coercion-Huh)
  (mediating-crcn?$  mediating-crcn? Mediating-Coercion-Huh?))

(define (mediating-crcn? x)
  (or (CTuple? x) (Ref? x) (Fn? x)))

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
  (seq-fst$    Sequence?  Quote-Coercion Sequence-fst  Sequence-Coercion-Fst)
  (seq-snd$    Sequence?  Quote-Coercion Sequence-snd  Sequence-Coercion-Snd)
  (prj-type$   Project?   Type           Project-type  Project-Coercion-Type)
  (prj-label$  Project?   Quote          Project-label Project-Coercion-Label)
  (inj-type$   Inject?    Type           Inject-type   Inject-Coercion-Type)
  (ref-read$   Ref?       Quote-Coercion Ref-read      Ref-Coercion-Read)
  (ref-write$  Ref?       Quote-Coercion Ref-write     Ref-Coercion-Write)
  (tuple-num$  CTuple?    Quote          CTuple-num    Tuple-Coercion-Num)
  (fail-label$ Failed?    Quote          Failed-label  Failed-Coercion-Label))

(: tuple-crcn-arg$ (CoC3-Expr Index -> CoC3-Expr))
(define (tuple-crcn-arg$ x i)
  (if (not (Quote-Coercion? x))
      (Tuple-Coercion-Item x i)
      (let ([x (Quote-Coercion-const x)])
        (if (CTuple? x)
            (Quote-Coercion (list-ref (CTuple-items x) i))
            (error 'interpret-casts "~a applied to ~a" 'tuple-crcn x)))))

(: tuple-type-arg$ (CoC3-Expr Index -> CoC3-Expr))
(define (tuple-type-arg$ x i)
  (if (not (Type? x))
      (Type-Tuple-item x i)
      (let ([x (Type-type x)])
        (if (STuple? x)
            (Type (list-ref (STuple-items x) i))
            (error 'interpret-casts "~a applied to ~a" 'tuple-type x)))))

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
            (error 'gvect-of$ "given ~a" x)))))

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
