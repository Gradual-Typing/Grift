#lang racket/base
#|

This interpreter is supose to be able to handle the semantics of
all forms for every language after insert implicit casts. It is
currently implemented in several files.

|#

(provide interp)

(require racket/fixnum
         racket/match
         "../../src/helpers-untyped.rkt"
         "../../src/errors.rkt"
	 "../../src/language.rkt"
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
  (define-values (cast apply) (select-help config))
  (define eval
    (interp-expr cast apply))
  (match-let ([(Prog _ exp) prgm])
    (observe-lazy (lambda () (eval exp (env))))))

;; Select the helper functions that are needed in order to implement a
;; specific configuration
(define (select-help config)
  (match* ((Config-semantics config) (Config-cast-rep config)) 
    [('Lazy-D 'Twosomes)  (values apply-cast-ld (apply-lazy apply-cast-ld))]
    [('Lazy-D 'Coercions) (values apply-coercion-lazy (apply-lazy apply-coercion-lazy))]))

;; Various Representations of Procedures and Closures
(struct Interp-Proc (value))
(struct Interp-Twosome (type1 type2 label))
(struct Interp-Dyn (value mark)
  #:transparent)


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
(define (interp-expr cst apply)
  (define (recur exp env)
    (define (recur/env e) (recur e env))
    (define (map-curry-recur exp*)
      (map (lambda (exp) (lambda (env) (recur exp env))) exp*))
    (inc i-depth)
    (logging cast/interp-expr (All) "~a> ~v" (get i-depth) exp)
    #;(: recur/env (-> C0-Expr CL-Value))
    ;; res is a temporary that stores the result for debugging
    (define res
      (match exp
        [(Lambda id* body)
         (Interp-Proc
          (lambda (arg*)
            (recur body (env-extend env id* arg*))))]
        [(Letrec bnd* body)
         (let ([id*  (map car bnd*)]
               [rhs* (map cdr bnd*)])
           (recur body (env-extend-rec env id* (map-curry-recur rhs*))))]
        [(Let bnd* body)
         (let ([id*  (map car bnd*)]
               [rhs* (map (lambda (b) (recur/env (cdr b))) bnd*)])
           (recur body (env-extend env id* rhs*)))]
        [(If (app recur/env tst) csq alt)
         (if tst (recur/env csq) (recur/env alt))]
        [(App e e*) (apply (recur/env e) (map recur/env e*))]
        [(Op p e*) (delta p (map recur/env e*))]
        [(Coerce c (app recur/env v))
         (if (eq? cst apply-coercion-lazy)
             (cst c v)
             (error 'interp "cst not for coercions"))]
        [(Cast (app recur/env val) t-exp t-cast label)
         (if (eq? cst apply-cast-ld)
             (cst val t-exp t-cast label)
             (error 'interp "cst not for twosomes"))]
        [(Var id) (env-lookup env id)]
        [(Quote k) k]
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
        ;; Guarded references
        [(Gbox (app recur/env v)) (make-gbox v)]
        [(Gunbox (app recur/env v))
         (read-grep cst v)]
        [(Gbox-set! (app recur/env e1) e2)
         (write-grep cst e1 (recur/env e2))]
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
(define (apply-cast-ld v1 t1 t2 l1)
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
              (apply-cast-ld v2 t3 t2 l1)]
             [other (raise-value-type-mismatch v1 t1)])]
          [((GRef t1) (GRef t2)) (cast-grep/twosome v1 t1 t2 l1)]
          [((GVect t1) (GVect t2)) (cast-grep/twosome v1 t1 t2 l1)]
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
      [(Interp-Proc rator) (rator rands)]
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
    [(Interp-Dyn v^ c^) (mk-cast/coercion v^ (compose c^ c))]
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
      [(Interp-GProxy 'Box v _)    (gbox)]
      [(Interp-Dyn (box _) (Proxy-Guarded _ _)) (gbox)]
      [(vector _ ...)              (gvect)]
      [(Interp-GProxy 'Vector v _) (gvect)]
      [(Interp-Dyn (vector _ ...) (Proxy-Guarded _ _)) (gvect)]
      [(Interp-Proc v)             (function)]
      [(Interp-Dyn _ (list _ _ (Fn _ _ _))) (function)]
      [(Interp-Dyn _ (Proxy-Fn _ _ _)) (function)]
      [(Interp-Dyn _ _) (dyn)]
      [else (error "returned unexpected value")])))


