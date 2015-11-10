#lang racket/base
#|

This interpreter is supose to be able to handle the semantics of
all forms for every language after insert implicit casts. It is
currently implemented in severral files.

|#

(provide interp)

(require racket/fixnum
         racket/match
         "../../src/helpers-untyped.rkt"
         "../../src/errors.rkt"
	 "../../src/language/forms.rkt"
         "../../src/configuration.rkt"
         "../values.rkt"
         ;; The sub-components of the interpreter
         "./primitive.rkt"
         "./errors.rkt"
         "./recursive-environments.rkt"
         "./coercions-ld.rkt"
         "./guarded-references.rkt")

;; Entry point into the interpreter.
;; Sets up the interpreter for each individual program
;; Todo move the interpreter to keeping an explicit state that is threaded through
(trace-define (interp prgm config)
  (initialize-state)
  (define global-lbls (global-env))
  (define-values (cast-rep cast apply) (select-help config global-lbls))
  (define eval
    (interp-expr cast-rep cast apply global-lbls))
  (match-let ([(Prog _ exp) prgm])
    (observe-lazy (lambda () (eval exp (empty-env))))))

;; Select the helper functions that are needed in order to implement a
;; specific configuration
(define (select-help config lbls)
  (match* ((Config-semantics config) (Config-cast-rep config)) 
    [('Lazy-D 'Twosomes)
     (let ([cast (apply-cast-ld lbls)])
       (values 'Twosomes cast (apply-lazy cast)))]
    [('Lazy-D 'Coercions)
     (values 'Coercions apply-coercion-lazy (apply-lazy apply-coercion-lazy))]))

;; Various Representations of Procedures and Closures
(struct Interp-Proc (value caster))
(struct Interp-Twosome (type1 type2 label))
(struct Interp-Dyn (value mark)
  #:transparent)
(struct Interp-Code (apply))

;; Makes the casted value respresentation for the twosome implementation
(define (mk-cast/twosome e t g l)
  (cond
    [(equal? t g) e]
    [else (Interp-Dyn e (Interp-Twosome t g l))]))

;; A small counter used for tracking the recursion depth when logging
(define i-depth (box 0))
(define (inc r) (set-box! r (add1 (unbox r))))
(define (dec r) (set-box! r (sub1 (unbox r))))
(define (get r) (unbox r))

#;(define-type eval-expr-type (-> C0-Expr (Env CL-Value) CL-Value))
#;(: interp-expr (-> Cast-Type apply-type eval-expr-type))
;; TODO lbls here is broken one possible fix is to use a mutable hash table
;; to make the scope of lbls global.
(define (interp-expr cast-rep cst apply lbls)
  (define (recur exp env)
    (define (recur/env e) (recur e env))
    (define (map-curry-recur exp*)
      (map (lambda (exp) (lambda (env) (recur exp env))) exp*))
    ;; (exp env lbls -> value) -> lbls -> bnd-code -> Interp-Code
    (define ((interp-code recur) id code)
      (match-let ([(Code fml* b) code])
        (Interp-Code
         (lambda (v*)
           (unless (= (length fml*) (length v*))
             (error 'interp "code ~a applied to wrong arity: ~a" id v*))
           (recur b (env-extend (empty-env) fml* v*))))))
    (inc i-depth)
    (logging cast/interp-expr (All) "~a> ~v\n\t~v" (get i-depth) exp env)
    #;(: recur/env (-> C0-Expr CL-Value))
    ;; res is a temporary that stores the result for debugging
    (define res
      (match exp
        ;; Binding Forms 
        [(Letrec bnd* body)
         (let ([id*  (map car bnd*)]
               [rhs* (map cdr bnd*)])
           (recur body (env-extend-rec env id* (map-curry-recur rhs*))))]
        [(Labels (list (cons id* code*) ...) body)
         (global-env-extend! lbls id* (map (interp-code recur) id* code*))
         (recur body env)]
        [(Let bnd* body)
         (let ([id*  (map car bnd*)]
               [rhs* (map (lambda (b) (recur/env (cdr b))) bnd*)])
           (recur body (env-extend env id* rhs*)))]
        [(Var id) (env-lookup env id)]
        ;; Branching / Control Flow
        [(If (app recur/env tst) csq alt)
         (if tst (recur/env csq) (recur/env alt))]
        [(Begin e* e) (begin (for-each recur/env e*) (recur/env e))]
        ;; Since repeat is implemented at a very low level
        ;; the representation of unit changes before it disapears
        [(Repeat id start stop body)
         (let ([start-v (recur start env)]
               [stop-v  (recur stop env)])
           (cond
             [(not (integer? start-v))
              (raise-repeat-type-unenforced start-v start)]
             [(not (integer? stop-v))
              (raise-repeat-type-unenforced stop-v stop)]
             [else
              (run-repeat-loop recur env body id start-v stop-v '())]))]
        ;; Primitives
        [(Op p e*) (delta p (map recur/env e*))]
        [(Quote k) k]
        ;; The interface for functions
        [(Lambda id* body)
         (let-values ([(lbl? body)
           (if (Castable? body)
               (values (Castable-caster body)
                       (Castable-body body))
               (values #f body))])
           (Interp-Proc
            (lambda (arg*)
              (recur body (env-extend env id* arg*)))
            lbl?))]
        [(Fn-Caster (app recur/env v))
         (unless (Interp-Proc? v)
           (mismatch Fn-Caster v '(Fn _ _ _)))
         (define lbl? (Interp-Proc-caster v))
         (unless (Uid? lbl?)
           (mismatch Fn-Caster lbl? 'Code-Label))
         (Code-Label lbl?)]
        [(App e e*) (apply (recur/env e) (map recur/env e*))]
        [(App-Fn (app recur/env f) (list (app recur/env v*) ...))
         (unless (Interp-Proc? f)
           (error 'interp/App-Closure))
         ((Interp-Proc-value f) v*)]
        ;; The interface for code 
        [(App-Code (app recur/env c) (list (app recur/env v*) ...))
         (unless (Code-Label? c)
           (error 'interp/App-Code "got ~a" c))
         ((Interp-Code-apply (global-env-lookup lbls (Code-Label-value c))) v*)]
        [(Code-Label id) (Code-Label id)]
        ;; The interface for Fn-Proxies
        ;; The underscored value changes but it is only to pass extra
        ;; information to subsequent passes
        [(Fn-Proxy _ (app recur/env fn-crcn) (app recur/env clos))
         (Interp-Dyn clos fn-crcn)]
        [(Fn-Proxy-Huh (app recur/env v))
         ;; In final product we are relying on this function to only
         ;; distinguish between the cononical forms for function types
         ;; it is a type error to use it otherwise.
         (cond
           [(Interp-Proc? v) #f]
           ;; TODO Fix the name of Fn to be Fn-Coercion
           [(and (Interp-Dyn? v) (Fn? (Interp-Dyn-mark v))) #t]
           [else (error 'interp/Fn-Proxy-Huh "type invalid value ~a" v)])]
        [(Fn-Proxy-Closure (app recur/env v))
         (unless (and (Interp-Dyn? v) (Fn? (Interp-Dyn-mark v)))
           (error 'interp/Fn-Proxy-Closure "type invalid ~a" v))
         (Interp-Dyn-value v)]
        [(Fn-Proxy-Coercion (app recur/env v))
         (unless (and (Interp-Dyn? v) (Fn? (Interp-Dyn-mark v)))
           (error 'interp/Fn-Proxy-Coercion))
         (Interp-Dyn-mark v)]
        [(App/Fn-Proxy-Huh (app recur/env v) (list (app recur/env v*) ...))
         (match v
           [(Interp-Proc f _) (f v*)]
           [(Interp-Dyn (Interp-Proc f _) (Fn _ c* c))
            (cst c (f (map cst c* v*)))]
           [other (mismatch App/Fn-Proxy-Huh v 'Fn/Fn-Proxy)])]
        ;; This is really the same as above but relies on the runtime
        ;; cast code Is Kindof a cheap trick because we want to delay
        ;; the desugaring of this line until closure conversion to
        ;; implement hybrid. But we will need this label at that point
        ;; If we are implementing the data representation
        [(App-Fn-or-Proxy i (app recur/env v) (list (app recur/env v*) ...))
         (match v
           [(Interp-Proc f _) (f v*)]
           [(Interp-Dyn (Interp-Proc f _) (Fn _ c* c))
            (let ([cast (Interp-Code-apply (global-env-lookup lbls i))])
              (cast (list (f (map (lambda (v c) (cast (list v c))) v* c*)) c)))]
           [other (error 'App-Fn-or-Proxy "unmatched ~a" exp)])]
        ;; Types as runtime objects
        ;; TODO Rename this
        [(Type t) t]
        [(Type-Tag (app recur/env t))
         (cond
           [(or (Int? t) (Bool? t) (Dyn? t)) 'Atomic]
           [(Fn? t) 'Fn]
           [(GRef? t) 'GRef]
           [(GVect? t) 'GVect]
           [else (mismatch Type-Tag t 'Type)])]
        [(Type-Dyn-Huh (app recur/env t)) (Dyn? t)]
        [(Type-Fn-Huh (app recur/env t)) (Fn? t)]
        [(Type-Fn-arg (app recur/env v) (app recur/env i))
         (unless (and (Fn? v) (integer? i))
           (error 'interp/Type-Fn-Arg))
         (list-ref (Fn-fmls v) i)]
        ;; TODO Rename this Fn-Return
        [(Type-Fn-return (app recur/env v))
         (unless (Fn? v)
           (error 'interp/Type-Fn-Return))
         ;; TODO Rename this FN-return
         (Fn-ret v)]
        [(Type-Fn-arity (app recur/env v))
         (unless (Fn? v)
           (error 'interp/Type-Fn-Arity))
         (Fn-arity v)]
        [(Type-GRef-Huh (app recur/env t)) (GRef? t)]
        [(Type-GRef-Of (app recur/env t))
         (unless (GRef? t)
           (mismatch Type-GRef-Of t 'GRef))
         (GRef-arg t)]
        [(Type-GVect-Huh (app recur/env t)) (GVect? t)]
        [(Type-GVect-Of (app recur/env t))
         (unless (GVect? t)
           (mismatch Type-GVect-Of t 'GRef))
         (GVect-arg t)]
        ;; Coercions as runtime objects
        [(Quote-Coercion c) c]
        [(Sequence-Coercion-Huh (app recur/env t))
         (Sequence? t)]
        [(Sequence-Coercion (app recur/env f) (app recur/env s))
         (unless (coercion? f)
           (mismatch Sequence-Coercion f 'Coercion))
         (unless (coercion? s)
           (mismatch Sequence-Coercion s 'Coercion))
         (Sequence f s)]
        [(Sequence-Coercion-Fst (app recur/env s))
         (unless (and (coercion? s) (Sequence? s))
           (mismatch Sequence-Coercion-Fst s 'Sequence-Coercion))
         (Sequence-fst s)]
        [(Sequence-Coercion-Snd (app recur/env s))
         (unless (and (coercion? s) (Sequence? s))
           (mismatch Sequence-Coercion-Snd s 'Sequence-Coercion))
         (Sequence-snd s)]
        [(Project-Coercion (app recur/env t) (app recur/env l))
         (unless (schml-type? t)
           (mismatch Project-Coercion t 'Type))
         (unless (string? l)
           (mismatch Project-Coercion l 'String))
         (Project t l)]
        [(Project-Coercion-Huh (app recur/env c)) (Project? c)]
        [(Project-Coercion-Type (app recur/env p))
         (unless (and (coercion? p) (Project? p))
           (mismatch Project-Coercion-Type p 'Project-Coercion))
         (Project-type p)]
        [(Project-Coercion-Label (app recur/env p))
         (unless (and (coercion? p) (Project? p))
           (mismatch Project-Coercion-Label p 'Project-Coercion))
         (Project-label p)]
        [(Inject-Coercion (app recur/env t))
         (unless (schml-type? t)
           (mismatch Inject-Coercion t 'Type))
         (Inject t)]
        [(Inject-Coercion-Huh (app recur/env c)) (Inject? c)]
        [(Inject-Coercion-Type (app recur/env i))
         (unless (and (coercion? i) (Inject? i))
           (mismatch Inject-Coercion-Type i 'Inject-Coercion))
         (Inject-type i)]
        [(Failed-Coercion (app recur/env l))
         (unless (string? l)
           (mismatch Fail-Coercion l 'String))
         (Failed l)]
        [(Failed-Coercion-Huh (app recur/env c)) (Failed? c)]
        [(Failed-Coercion-Label (app recur/env f))
         (unless (and (coercion? f) (Failed? f))
           (mismatch Failed-Coercion-Label f 'Failed-Coercion))
         (Failed-label f)]
        [(Ref-Coercion-Huh (app recur/env c)) (Ref? c)]
        [(Fn-Coercion-Huh (app recur/env c)) (Fn? c)]
        [(Make-Fn-Coercion make-uid
                           (app recur/env t1) (app recur/env t2)
                           (app recur/env l))
         (unless (Fn? t1)
           (error Make-Fn-Coercion t1 'Fn-Type))
         (unless (Fn? t2)
           (error Make-Fn-Coercion t2 'Fn-Type))
         (unless (string? l)
           (error Make-Fn-Coercion l  'String))
         (define ((help fn l) t s) (fn (list t s l)))
         (let* ([fn (Interp-Code-apply (global-env-lookup lbls make-uid))]
               [arg* (map (help fn l) (Fn-fmls t2) (Fn-fmls t1))]
               [ret  ((help fn l) (Fn-ret t1) (Fn-ret t2))])
           (Fn (Fn-arity t1) arg* ret))]
        [(Compose-Fn-Coercion comp-uid (app recur/env c1) (app recur/env c2))
         (unless (Fn? c1)
           (error Compose-Fn-Coercion c1 'Fn-Coercion))
         (unless (Fn? c2)
           (error Compose-Fn-Coercion c2 'Fn-Coercion))
         (define ((help fn) t s) (fn (list t s)))
         (let* ([fn   (Interp-Code-apply (global-env-lookup lbls comp-uid))]
               [arg* (map (help fn) (Fn-fmls c2) (Fn-fmls c1))]
               [ret  ((help fn) (Fn-ret c1) (Fn-ret c2))])
           (if (and (andmap Identity? arg*) (Identity? ret))
               (Identity)
               (Fn (Fn-arity c1) arg* ret)))]
        [(Compose-Coercions (app recur/env c1) (app recur/env c2))
         (compose c1 c2)]
        [(Id-Coercion-Huh (app recur/env c)) (Identity? c)]
        [(Fn-Coercion (list (app recur/env c*) ...) (app recur/env c))
         (Fn (length c*) c* c)]
        [(Fn-Coercion-Arg (app recur/env c) (app recur/env i))
         (list-ref (Fn-fmls c) i)]
        [(Fn-Coercion-Return (app recur/env c))
         (Fn-ret c)]
        [(Ref-Coercion (app recur/env r) (app recur/env w))
         (Ref r w)]
        [(Ref-Coercion-Read (app recur/env c))
         (match c
           [(Ref r _) r]
           [other (error 'Ref-Coercion-Read)])]
        [(Ref-Coercion-Write (app recur/env c))
         (match c
           [(Ref _ w) w]
           [other (error 'Ref-Coercion-Write)])]
        ;; Cast Handling TODO Make it so that there are only two forms here
        ;; Interp-Cast and Cast
        [(Cast (app recur/env val) rep)
         (match rep
           [(Twosome t-exp t-cast label)           
            (cond
              [(eq? cast-rep 'Twosomes)
               (cst val t-exp t-cast label)]
              ;; If we are testing coercions but in a language before
              ;; they are inserted translate cast to coercions
              [(eq? cast-rep 'Coercions)
               (cst ((mk-coercion label) t-exp t-cast) val)]
              [else (error 'interp/Cast)])]
           [(Coercion c)
            (cond
              [(eq? cast-rep 'Coercions) (cst c val)]
              [else (error 'interp/Coerce)])]
           [other (error 'interp/cast "~a" other)])]
        [(Interpreted-Cast (app recur/env v) rep)
         (match rep
           [(Twosome (app recur/env t1)(app recur/env t2) (app recur/env l))
            (cond
              [(eq? cast-rep 'Twosomes) (cst v t1 t2 l)]
              [else (error 'interp/Interpret-Cast)])]
           [(Coercion (app recur/env c))
            (cond
              [(eq? cast-rep 'Coercions) (cst c  v)]
              [else (error 'interp/Interpret-Coerce)])])]
        [(Blame (app recur/env lbl)) (raise-blame lbl)]
        [(Observe (app recur/env v) t)
         (match t
           [(Int)  (raise (format "Int : ~a" v))]
           [(Bool) (raise (format "Bool : ~a" v))]
           [(Unit) (raise "Unit : ()")]
           [(Dyn)  (raise "Dynamic : ?")]
           [(Fn _ _ _) (raise "Function : ?")]
           [(GRef _)   (raise "GReference : ?")]
           [(GVect _)  (raise "GVector : ?")]
           [other (error 'interp/Observe "~a ~a" t v)])]
        ;; Guarded references
        [(Gbox (app recur/env v)) (make-gbox v)]
        [(Gunbox (app recur/env v))
         (read-grep cst v)]
        [(Gbox-set! (app recur/env e1) (app recur/env e2))
         (write-grep cst e1 e2)]
        ;; Guarded vectors
        [(Gvector (app recur/env size) (app recur/env v))
         (make-gvect size v)]
        [(Gvector-ref (app recur/env v) (app recur/env i))
         (read-grep cst v i)]
        [(Gvector-set! (app recur/env v1)
                       (app recur/env i)
                       (app recur/env v2))
         (write-grep cst v1 v2 i)]
        ;; Guarded Runtime Representation
        ;; TODO Make this Guarded-Proxy-Huh
        [(Guarded-Proxy-Huh (app recur/env r))
         (match r
           [(box _) #f]
           [(vector _ ...) #f]
           [(Interp-GProxy _ _ _) #t]
           [other (error 'interp/GRep-Proxied? "~a" other)])]
        ;; TODO Make this (Guarded-Proxy v (Twosome t1 t2 l))
        [(Guarded-Proxy (app recur/env v) rep)
         (match rep
           [(Twosome (app recur/env t1) (app recur/env t2) (app recur/env l))
            (match v
              [(box _) (Interp-GProxy 'Box v (Twosome t1 t2 l))]
              [(vector _ ...) (Interp-GProxy 'Vector v (Twosome t1 t2 l))]
              [(Interp-GProxy t _ _)
               (Interp-GProxy t v (Twosome t1 t2 l))]
              [other (error 'interp/Guarded-Proxy/Twosome "~a" other)])]
           [(Coercion (app recur/env c))
            (if (Identity? c)
                v
                (match v
                  [(box _) (Interp-GProxy 'Box v c)]
                  [(vector _ ...) (Interp-GProxy 'Vector v c)] 
                  [other (error 'interp/Guarded-Proxy/Coercion "~a" other)]))]
           [other (error 'interp/Guarded-Proxy "~a" other)])]
        [(Guarded-Proxy-Ref (app recur/env p))
         (match p
           [(Interp-GProxy _ v _) v]
           [other (error 'interp/Guarded-Proxy-Ref "~a" p)])]
        [(Guarded-Proxy-Source (app recur/env p))
         (match p
           [(Interp-GProxy _ _ (Twosome t1 _ _)) t1]
           [other (error 'interp/Guarded-Proxy-Source "~a" p)])]
        [(Guarded-Proxy-Target (app recur/env p))
         (match p
           [(Interp-GProxy _ _ (Twosome _ t2 _)) t2]
           [other (error 'interp/Guarded-Proxy-Target "~a" p)])]
        [(Guarded-Proxy-Blames (app recur/env p))
         (match p
           [(Interp-GProxy _ _ (Twosome _ _ l)) l]
           [other (error 'interp/Guarded-Proxy-Blames "~a" p)])]
        [(Guarded-Proxy-Coercion (app recur/env p))
         (match p
           [(Interp-GProxy _ _ c) c]
           [other (error 'interp/Guarded-Proxy-Blames "~a" p)])]
        ;; UnGuarded references
        [(Unguarded-Box (app recur/env v)) (box v)]
        [(Unguarded-Box-Set! (app recur/env b) (app recur/env v))
         (unless (and (box? b))
           (error 'interp/Unguarded-Box-Set!))
         (set-box! b v)]
        [(Unguarded-Box-Ref (app recur/env b))
         (unless (box? b)
           (error 'interp/UGbox-ref))
         (unbox b)]
        [(Unguarded-Vect (app recur/env n) (app recur/env v))
         (unless (exact-nonnegative-integer? n)
           ;; TODO This should output the same text runtime error message does
           (error 'interp/UGvect "given invalid size ~a" n))
         (make-vector n v)]
        [(Unguarded-Vect-Set!
          (app recur/env r) (app recur/env i) (app recur/env v))
         (unless (and (vector? r) (exact-nonnegative-integer? i))
           (error 'interp/UGvect-set! "invalid input ~a ~a" r i))
         (vector-set! r i v)]
        [(Unguarded-Vect-Ref (app recur/env r) (app recur/env i))
         (unless (and (vector? r) (exact-nonnegative-integer? i))
           (error 'interp/UGvect-ref "invalid input ~a ~a" r i))
         (vector-ref r i)]
        ;; The Dynamic Representation (Exposed too early for my taste)
        [(Dyn-tag (app recur/env v))
         (match v
           [(Interp-Dyn _ (Int))  'Int]
           [(Interp-Dyn _ (Bool)) 'Bool]
           [(Interp-Dyn _ (Unit)) 'Unit]
           [(Interp-Dyn _ (Fn _ _ _)) 'Boxed]
           [(Interp-Dyn _ (GRef _))   'Boxed]
           [(Interp-Dyn _ (GVect _))  'Boxed]
           [other (unmatched Dyn-tag v)])]
        [(Dyn-immediate (app recur/env v))
         (match v
           [(Interp-Dyn i (or (Int) (Bool) (Unit))) i]
           [other (mismatch Dyn-tag v 'Dynamic-Immediate)])]
        [(Dyn-type (app recur/env v))
         (match v
           [(Interp-Dyn _ t) t]
           [other (mismatch Dyn-type v 'Dynamic-Boxed)])]
        [(Dyn-value (app recur/env v))
         (match v
           [(Interp-Dyn v _) v]
           [other (error 'interp/Dyn-type "~a" other)])]
        [(Dyn-make (app recur/env v) (app recur/env t))
         (unless (schml-type? t)
           (mismatch Dyn-make t 'Type))
         (Interp-Dyn v t)]
        ;; The Type Tag interface (one that shouldn't be expose so early)
        [(Tag k) k]
       
        [e (unmatched interp e)]))
    (logging cast/interp-res (all) "~a > ~v \n\t==> ~v" (get i-depth) exp res)
    (dec i-depth)
    res)
  recur)





;; Implements the functionallity of the repeat construct
(define (run-repeat-loop interp env body id start stop unit)
  (define (loop index stop)
    (if (< index stop)
        (begin
          (interp body (env-cons id index env))
          (loop (+ index 1) stop))
        unit))
  (loop start stop))

;; Another counter that is used to figure out the depth of the recursion
;; when casting
(define c-depth (box 0))

;; Implements cast node functionality
(define ((apply-cast-ld lbls) v1 t1 t2 l1)
  ;;(inc c-depth)
  ;;(logging apply-cast-ld (All) "~a > ~v ~v ~v ~v" (get c-depth) v1 t1 t2 l1)
  (define res
    (if (shallow-consistent? t1 t2)
        (match* (t1 t2)
          [((Dyn) t2)
           (match v1
             [(Interp-Dyn v2 (Interp-Twosome t3 t1 l2))
              ((apply-cast-ld lbls) v2 t3 t2 l1)]
             [other (mismatch apply-cast-ld v1 t1)])]
          [((GRef t1) (GRef t2)) (cast-grep/twosome v1 t1 t2 l1)]
          [((GVect t1) (GVect t2)) (cast-grep/twosome v1 t1 t2 l1)]
          [((Fn n1 t11* t12) (Fn n2 t21* t22))
           (cond
             [(not (= n1 n2)) (raise-blame l1)]
             ;; If there are function casters present then we
             ;; must be at the point were we are using functions
             ;; as the wrappers for casted functions.
             [(and (Interp-Proc? v1) (Interp-Proc-caster v1))
              ;; This check should also be done in the cast interpreter
              (if (equal? t1 t2)
                  v1
                  ((Interp-Code-apply (global-env-lookup lbls (Interp-Proc-caster v1)))
                   (list  v1 t1 t2 l1)))]
             [else (mk-cast/twosome v1 t1 t2 l1)])]
          [(_ _) (mk-cast/twosome v1 t1 t2 l1)])
        (raise-blame l1)))
  ;;(logging apply-cast-res (All) "~a > ~v" (get c-depth) res)
  ;;(dec c-depth)
  res)

#;(define-type apply-type (-> CL-Value CL-Value* CL-Value))

#;(: apply-lazy (-> Cast-Type apply-type))
(define (apply-lazy cast)
  #;(: cast/lbl (-> String (-> CL-Value Schml-Type Schml-Type CL-Value)))
  (define (cast/lbl l)
    (lambda (v t1 t2) ;;: CL-Value
      (cast v t1 t2 l)))
  #;(: recur apply-type)
  (define (recur rator rands)
    (match rator
      [(Interp-Dyn val (Interp-Twosome (Fn ar1 t1* t2) (Fn ar2 t3* t4) lbl))
       (define rands^
         (if (= ar1 ar2)
             (map (cast/lbl lbl) rands t3* t1*)
             (raise (exn:schml:type:dynamic lbl (current-continuation-marks)))))
       (define result (recur val rands^))
       (cast result t2 t4 lbl)]
      [(Interp-Dyn val (Fn ar args res))
       (define rands^ (map apply-coercion-lazy args rands))
       ;; Threats to validity
       (define result (recur val rands^))
       (apply-coercion-lazy res result)]
      [(Interp-Proc rator _) (rator rands)]
      [otherwise 
       (error 'interp "Unexpected value in apply-lazy match ~a" rator)]))
  recur)

(define (mk-cast/coercion v c)
  (match c
    [(Identity) v]
    [(Failed l) (raise-blame l)]
    [(Ref _ _)
     (unless (or (vector? v) (box? v))
       (error 'interp "not as space efficient as we believe"))
     (if (box? v)
         (Interp-GProxy 'Box v c)
         (Interp-GProxy 'Vector v c))]
    [other (Interp-Dyn v c)]))

(define (apply-coercion-lazy c v)
  (match v
    [(Interp-Dyn v^ c^)      (mk-cast/coercion v^ (compose c^ c))]
    [(Interp-GProxy _ v^ c^) (mk-cast/coercion v^ (compose c^ c))]
    [other (mk-cast/coercion v c)]))



#;(: observe-lazy (-> (-> CL-Value) Test-Value))
(define (observe-lazy thunk)
  (with-handlers ([string?
                   (lambda (s) (parse-observable s))]
                  [exn:schml:type:dynamic?
		   (lambda (e)
                     (blame #f (exn-message e)))])
    (match (thunk)
      [(? integer? v) (int v)] 
      [(? boolean? v) (bool v)]
      [(? null?)      (unit)]
      [(box v)                     (gbox)]
      [(Interp-GProxy 'Box _ _)    (gbox)]
      [(Interp-Dyn (box _) (Ref _ _)) (gbox)]
      [(vector _ ...)              (gvect)]
      [(Interp-GProxy 'Vector _ _) (gvect)]
      [(Interp-Dyn (vector _ ...) (Ref _ _)) (gvect)]
      [(Interp-Proc _ _)             (function)]
      [(Interp-Dyn _ (list _ _ (Fn _ _ _))) (function)]
      [(Interp-Dyn _ (Fn _ _ _)) (function)]
      [(Interp-Dyn _ (Interp-Twosome _ (Fn _ _ _) _)) (function)]
      [(Interp-Dyn _ _) (dyn)]
      [else (error "returned unexpected value")])))


