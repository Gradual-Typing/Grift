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
    (logging cast/interp-expr (All) "~a> ~v" (get i-depth) exp)
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
         (let-values ([(lbl? body) (if (Castable? body)
                                          (values (Castable-caster body)
                                                  (Castable-body body))
                                          (values #f body))])
           (Interp-Proc
            (lambda (arg*)
              (recur body (env-extend env id* arg*)))
            lbl?))]
        [(Fn-Caster (app recur/env v))
         (unless (Interp-Proc? v)
           (error 'interp/Fn-Caster "not interpreted function ~a" v))
         (define lbl? (Interp-Proc-caster v))
         (unless (Uid? lbl?)
           (error 'interp/Fn-Caster "not a code label ~a" lbl?))
         (Code-Label lbl?)]
        [(App e e*) (apply (recur/env e) (map recur/env e*))]
        [(App-Closure (app recur/env f) (list (app recur/env v*) ...))
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
        [(Fn-Proxy i (app recur/env fn-crcn) (app recur/env clos))
         (Interp-Dyn clos fn-crcn)]
        [(Fn-Proxy-Huh (app recur/env v))
         ;; In final product we are relying on this function to only
         ;; distinguish between the cononical forms for function types
         ;; it is a type error to use it otherwise.
         (cond
           [(Interp-Proc? v) #f]
           ;; TODO Fix the name of Proxy-Fn to be Fn-Coercion
           [(and (Interp-Dyn? v) (Proxy-Fn? (Interp-Dyn-mark v))) #t]
           [else (error 'interp/Fn-Proxy-Huh "type invalid value ~a" v)])]
        [(Fn-Proxy-Closure (app recur/env v))
         (unless (and (Interp-Dyn? v) (Proxy-Fn? (Interp-Dyn-mark v)))
           (error 'interp/Fn-Proxy-Closure "type invalid ~a" v))
         (Interp-Dyn-value v)]
        [(Fn-Proxy-Coercion (app recur/env v))
         (unless (and (Interp-Dyn? v) (Proxy-Fn? (Interp-Dyn-mark v)))
           (error 'interp/Fn-Proxy-Coercion))
         (Interp-Dyn-mark v)]
        [(App/Fn-Proxy-Huh (app recur/env v) (list (app recur/env v*) ...))
         (cond
           [(Interp-Proc? v) ((Interp-Proc-value v) v*)]
           [(not (and (Interp-Dyn? v) (Proxy-Fn? (Interp-Dyn-mark v))))
            (error 'inter/App/Fn-Proxy-Huh "other value ~a" v)]
           [(not (eq? cast-rep 'Coercions))
            (error 'inter/App/Fn-Proxy-Huh "wrong cast fun ~a" cst)]
           [else
            (let* ([c (Interp-Dyn-mark v)]
                   [f (Interp-Dyn-value v)])
              (unless (Interp-Proc? f)
                (error 'inter/App/Fn-Proxy-Huh "value in proxy not fun: ~a" f))
              (cst (Proxy-Fn-return c)
                   ((Interp-Proc-value f) (map cst (Proxy-Fn-args c) v*))))])]
        ;; Types as runtime objects
        ;; TODO Rename this
        [(Type t) t]
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
        ;; Coercions as runtime objects
        [(Quote-Coercion c) c]
        [(Compose (app recur/env c1) (app recur/env c2))
         (compose c1 c2)]
        [(Id-Coercion-Huh (app recur/env c)) (Identity? c)]
        [(Fn-Coercion (list (app recur/env c*) ...) (app recur/env c))
         (Proxy-Fn (length c*) c* c)]
        [(Fn-Coercion-Arg (app recur/env c) (app recur/env i))
         (list-ref (Proxy-Fn-args c) i)]
        [(Fn-Coercion-Return (app recur/env c))
         (Proxy-Fn-return c)]
        ;; Cast Handling TODO Make it so that there are only two forms here
        ;; Interp-Cast and Cast
        [(Coerce c (app recur/env v))
         (cond
           [(eq? cast-rep 'Coercions) (cst c v)]
           [else (error 'interp/Coerce)])]
        [(Interpreted-Coerce (app recur/env c) (app recur/env v))
         (cond
           [(eq? cast-rep 'Coercions) (cst c  v)]
           [else (error 'interp/Interpret-Coerce)])]
        [(Cast (app recur/env val) t-exp t-cast label)
         ;; If we are testing coercions but in a language before
         ;; they are inserted translate cast to coercions
         (cond
           [(eq? cast-rep 'Twosomes)
            (cst val t-exp t-cast label)]
           [(eq? cast-rep 'Coercions)
            (cst ((mk-coercion label) t-exp t-cast) val)]
           [else (error 'interp/Cast)])]
        [(Interpreted-Cast (app recur/env v)
                           (app recur/env t1)(app recur/env t2)
                           (app recur/env l))
         (cond
           [(eq? cast-rep 'Twosomes) (cst v t1 t2 l)]
           [else (error 'interp/Interpret-Cast)])]
        [(Blame (app recur/env lbl)) (raise-blame lbl)]
        ;; Guarded references
        [(Gbox (app recur/env v)) (make-gbox v)]
        [(Gunbox (app recur/env v))
         (read-grep cst v)]
        [(Gbox-set! (app recur/env e1) (app recur/env e2))
         (write-grep cst e1 e2)]
        ;; guarded arrays
        [(Gvector (app recur/env size) (app recur/env v))
         (make-gvect size v)]
        [(Gvector-ref (app recur/env v) (app recur/env i))
         (read-grep cst v i)]
        [(Gvector-set! (app recur/env v1)
                       (app recur/env i)
                       (app recur/env v2))
         (write-grep cst v1 v2 i)]
        [e (error 'interp "Umatched expression ~a" e)]))
    (logging cast/interp-res (all) "~a > ~v" (get i-depth) res)
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
  (define-syntax-rule (raise-value-type-mismatch v t)
    (error 'cast-lang-interp "value ~a not the connoical form of ~t" v t))
  (define res
    (if (shallow-consistent? t1 t2)
        (match* (t1 t2)
          [((Dyn) t2)
           (match v1
             [(Interp-Dyn v2 (Interp-Twosome t3 t1 l2))
              ((apply-cast-ld lbls) v2 t3 t2 l1)]
             [other (raise-value-type-mismatch v1 t1)])]
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
        (raise (exn:schml:type:dynamic l1 (current-continuation-marks)))))
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
      [(Interp-Dyn val (Proxy-Fn ar args res))
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
    [(Identity _) v]
    [(Failed l) (raise-blame l)]
    [(Proxy-Guarded _ _)
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
  (with-handlers ([exn:schml:type:dynamic?
		   (lambda (e)
                     (blame #f (exn-message e)))])
    (match (thunk)
      [(? integer? v) (int v)] 
      [(? boolean? v) (bool v)]
      [(? null?)      (unit)]
      [(box v)                     (gbox)]
      [(Interp-GProxy 'Box _ _)    (gbox)]
      [(Interp-Dyn (box _) (Proxy-Guarded _ _)) (gbox)]
      [(vector _ ...)              (gvect)]
      [(Interp-GProxy 'Vector _ _) (gvect)]
      [(Interp-Dyn (vector _ ...) (Proxy-Guarded _ _)) (gvect)]
      [(Interp-Proc _ _)             (function)]
      [(Interp-Dyn _ (list _ _ (Fn _ _ _))) (function)]
      [(Interp-Dyn _ (Proxy-Fn _ _ _)) (function)]
      [(Interp-Dyn _ (Interp-Twosome _ (Fn _ _ _) _)) (function)]
      [(Interp-Dyn _ _) (dyn)]
      [else (error "returned unexpected value")])))


