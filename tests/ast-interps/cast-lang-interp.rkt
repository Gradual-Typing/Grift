;; when we update to racket 6.2, get rid off racket casts (e.g. for vectors)

#lang typed/racket

(require "../../src/helpers.rkt"
         "../../src/errors.rkt"
	 "../../src/language/cast0.rkt"
         "../../src/configuration.rkt"
	 "../values.rkt"
         racket/fixnum)

(provide cast-lang-interp)

(define-type CL-Value (U Boolean Integer Null CL-Proc CL-Dyn CL-GRef CL-GVect Blame-Label))
(define-type CL-Value* (Listof CL-Value))

;; This is a common idiom because typed-racket has a glitch concerning functions and
;; Unions

(struct CL-Proc ([value : (-> CL-Value* CL-Value)]))

(struct CL-Dyn ([label : String]
                [value : CL-Value]
                [type1 : Schml-Type]
                [type2 : Schml-Type])
  #:transparent)

;; The Guarded ADT Until Specify Representation
(define-type CL-GRef (U CL-GProxy CL-GBox))

(struct CL-GProxy  ([value : CL-GRef]
                    [t1 : Schml-Type]
                    [t2 : Schml-Type]
                    [label : Blame-Label])
  #:transparent)

(struct CL-GBox  ([value : CL-Value])
  #:mutable
  #:transparent)

(define-predicate gref? CL-GRef)

(: write-gref (-> Cast-Type CL-GRef CL-Value CL-Value))
(define (write-gref cast r v)
  (cond
    [(CL-GBox? r) (begin (set-CL-GBox-value! r v) '())]
    [(CL-GProxy? r)
      (write-gref cast
                  (CL-GProxy-value r)
                  (cast v
                        (CL-GProxy-t2 r)
                        (CL-GProxy-t1 r)
                        (CL-GProxy-label r)))]))

(define-type CL-GVect (U CL-GVectProxy (Vectorof CL-Value)))

(struct CL-GVectProxy  ([value : CL-GVect]
                        [t1 : Schml-Type]
                        [t2 : Schml-Type]
                        [label : Blame-Label])
  #:transparent)

(: write-gvect (-> Cast-Type CL-GVect Integer CL-Value CL-Value))
(define (write-gvect cast vect pos val)
  (cond
    [(vector? vect) (begin (vector-set! vect pos val) '())] ;; because vector-set! returns #void not '()
    [(CL-GVectProxy? vect)
     (write-gvect cast
                  (CL-GVectProxy-value vect)
                  pos
                  (cast val
                        (CL-GVectProxy-t2 vect)
                        (CL-GVectProxy-t1 vect)
                        (CL-GVectProxy-label vect)))]))

(: cast-gvect (-> CL-GVect Schml-Type Schml-Type Blame-Label CL-GVect))
(define cast-gvect CL-GVectProxy)

(: read-gref (-> Cast-Type CL-GRef CL-Value))
(define (read-gref cst r)
  (cond
    [(CL-GBox? r) (CL-GBox-value r)]
    [(CL-GProxy? r)
     (cst (read-gref cst (CL-GProxy-value r))
           (CL-GProxy-t1 r)
           (CL-GProxy-t2 r)
           (CL-GProxy-label r))]
    [else (TODO ERROR about vilation of the adt invarient)]))

(: read-gvect (-> Cast-Type Integer CL-GVect CL-Value))
(define (read-gvect cst i r)
  (cond
    [(vector? r) (vector-ref r i)]
    [(CL-GVectProxy? r)
     (cst (read-gvect cst i (CL-GVectProxy-value r))
           (CL-GVectProxy-t1 r)
           (CL-GVectProxy-t2 r)
           (CL-GVectProxy-label r))]
    [else (TODO ERROR about vilation of the adt invarient)]))

(define-type (Env a) (HashTable Uid (Boxof (U 'undefined a))))

(: env-lookup (-> (Env CL-Value) Uid CL-Value))
(define (env-lookup e u)
  (let ((ref (hash-ref e u (lambda () (error 'cfi "Unbound var ~a" u)))))
    (let ([val (unbox ref)])
      (if (eq? 'undefined val)
          (error 'cast-lang-interp-reference-to-undef)
          val))))

(: env-extend* (-> (Env CL-Value) Uid* CL-Value* (Env CL-Value)))
(define (env-extend* env uid* val*)
  (: env-cons (-> Uid CL-Value (Env CL-Value) (Env CL-Value)))
  (define (env-cons uid val env)
    (hash-set env uid (ann (box val) (Boxof (U CL-Value 'undefined)))))
  (foldr env-cons env uid* val*))

(: env-extend/undef* (-> (Env CL-Value) Uid* (Env CL-Value)))
(define (env-extend/undef* env uid*)
  (: env-cons (-> Uid (Env CL-Value) (Env CL-Value)))
  (define (env-cons uid env)
    (hash-set env uid (ann (box 'undefined) (Boxof (U CL-Value 'undefined)))))
  (foldr env-cons env uid*))

(: env-set! (-> (Env CL-Value) Uid CL-Value Void))
(define (env-set! env uid val)
  (let ([box
         : (Boxof (U 'undefined CL-Value))
         (hash-ref env uid (lambda () (error 'cfi "Unbound var ~a" uid)))])
    (set-box! box val)))

(: env-extend-rec* (-> (Env CL-Value) Uid* (Listof (-> (Env CL-Value) CL-Value)) (Env CL-Value)))
(define (env-extend-rec* env uid* f-rhs*)
  (let* ([env^ (env-extend/undef* env uid*)])
    (for-each (lambda ([uid : Uid] [f-rhs : (-> (Env CL-Value) CL-Value)])
                (env-set! env^ uid (f-rhs env^)))
              uid*
              f-rhs*)
    env^))

(define-syntax-rule (empty-env)
  (hash))

(define-type Cast-Type (-> CL-Value Schml-Type Schml-Type Blame-Label CL-Value))

(: mk-cast Cast-Type)
(define (mk-cast e t g l)
  (if (equal? t g)
      e
      (CL-Dyn l e t g)))

(: cast-lang-interp (-> Cast0-Lang Config Test-Value))
(trace-define (cast-lang-interp prgm comp-config)
  (initialize-state)
  (let ([eval (interp-expr apply-cast-ld (apply-lazy apply-cast-ld))]
	[observe observe-lazy])
    (match-let ([(Prog _ exp) prgm])
      (observe (lambda (): CL-Value (eval exp (empty-env)))))))

(define i-depth (box 0))
(define (inc [r : (Boxof Integer)]) (set-box! r (add1 (unbox r))))
(define (dec [r : (Boxof Integer)]) (set-box! r (sub1 (unbox r))))
(define (get [r : (Boxof Integer)]) (unbox r))

(define-type eval-expr-type (-> C0-Expr (Env CL-Value) CL-Value))
(: interp-expr (-> Cast-Type apply-type eval-expr-type))
(define (interp-expr cst apply)
  (: recur eval-expr-type)
  (define (recur exp env)
    (inc i-depth)
    (logging cast/interp-expr (All) "~a> ~v" (get i-depth) exp)
    (: recur/env (-> C0-Expr CL-Value))
    (define (recur/env e) (recur e env))
    (define (map-curry-recur [exp* : C0-Expr*])
      (define (curry-recur [exp : C0-Expr]): (-> (Env CL-Value) CL-Value)
        (lambda ([env : (Env CL-Value)]): CL-Value
                (recur exp env)))
      (map curry-recur exp*))
    (define res : CL-Value
      (match exp
      [(Lambda id* body)
       (CL-Proc (lambda ([arg* : CL-Value*]) (recur body (env-extend* env id* arg*))))]
      [(Letrec bnd* body)
       (let ([id*  (map (inst car Uid C0-Expr) bnd*)]
             [rhs* (map (inst cdr Uid C0-Expr) bnd*)])
         (recur body (env-extend-rec* env id* (map-curry-recur rhs*))))]
      [(Let bnd* body)
       (let ([id*  (map (inst car Uid C0-Expr) bnd*)]
             [rhs* (map (lambda ([b : C0-Bnd]) (recur/env (cdr b))) bnd*)])
         (recur body (env-extend* env id* rhs*)))]
      [(If (app recur/env tst) csq alt)
       (if tst (recur/env csq) (recur/env alt))]
      [(App e e*) (apply (recur/env e) (map recur/env e*))]
      [(Op p e*) (delta p (map recur/env e*))]
      [(Cast (app recur/env val) (Twosome t-exp t-cast label))
       (cst val t-exp t-cast label)]
      [(Var id) (env-lookup env id)]
      [(Quote k) k]
      [(Begin e* e) (begin (for-each recur/env e*) (recur/env e))]
      [(Repeat id start stop eff)
       (let* ([start (recur start env)]
              [stop  (recur stop  env)])
         (letrec ([loop : (Integer Integer -> CL-Value)
                   (lambda ([index : Integer] [stop : Integer]) : CL-Value
                    (if (< index stop)
                        (begin (recur eff (env-extend* env (cons id '()) (cons index '())))
                               (loop (+ index 1) stop))
                        '()))])
               (if (and (integer? start) (integer? stop))
                   (loop start stop)
                   (TODO come up with an error message for start not an integer))))]
      ;; Guarded references
      [(Gbox e)    (CL-GBox (recur/env e))]
      [(Gunbox (app recur/env e))
       (read-gref cst (assert e gref?))]
      [(Gbox-set! (app recur/env e1) e2)
       (if (gref? e1)
           (write-gref cst e1 (recur/env e2))
           (TODO raise an error here about the type system being broken))]
      ;; guarded arrays
      [(Gvector (app recur/env size) (app recur/env e))
       (if (integer? size)
           (make-vector size e)
           (TODO raise an error here))]
      [(Gvector-ref (app recur/env e) (app recur/env i))
       (if (integer? i)
           (read-gvect cst i (cast e CL-GVect))
           (TODO raise an error here))]
      [(Gvector-set! (app recur/env e1) (app recur/env i) (app recur/env e2))
       (write-gvect cst (cast e1 CL-GVect) (assert i integer?) e2)]
      [e (error 'interp "Umatched expression ~a" e)]))
    (logging cast/interp-res (all) "~a > ~v" (get i-depth) res)
    (dec i-depth)
    res)
  recur)

(: delta (-> Symbol CL-Value* CL-Value))
(define (delta p v*)
  (when (trace? 'All)
    (logf "cl-delta:\n~v\n\n" p))
  (case p
    [(+) (tc IxI->I + v*)]
    [(-) (tc IxI->I - v*)]
    [(*) (tc IxI->I * v*)]
    [(=) (tc IxI->B = v*)]
    [(<) (tc IxI->B < v*)]
    [(>) (tc IxI->B > v*)]
    [(>=) (tc IxI->B >= v*)]
    [(<=) (tc IxI->B <= v*)]
    [(%>>) (tc FxF->I fxrshift v*)]
    [(%<<) (tc FxF->I fxlshift v*)]
    [(%/) (tc IxI->I quotient v*)]
    [(binary-and) (tc FxF->I fxand v*)]
    [(binary-or) (tc FxF->I fxior v*)]
    [(timer-start) (tc ->U timer-start v*)]
    [(timer-stop)  (tc ->U timer-stop v*)]
    [(timer-report) (tc ->U timer-report v*)]
    [else (error 'delta "~a" p)]))

(define-syntax tc
  (syntax-rules (IxI->I IxI->B ->U)
    [(_ IxI->I p v) (tc-help p v (integer? (car v)) (integer? (cadr v)))]
    [(_ IxI->B p v) (tc-help p v (integer? (car v)) (integer? (cadr v)))]
    [(_ ->U p v)    (tc-help p v)]
    [(_ FxF->I p v) (tc-help p v (fixnum? (car v)) (fixnum? (cadr v)))]))


(define-syntax (tc-help stx)
  (syntax-case stx ()
    [(_ p v (? e) ...)
     (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
       #'(let ((tmp e) ...)
           (if (and (? tmp) ...)
               (p tmp ...)
               (error 'cfi-delta "type error ~a" `(p ,e ...)))))]))

(define (initialize-state)
  (set! timer-started? #f)
  (set! timer-stopped? #f)
  (set! start-time 0.0)
  (set! stop-time 0.0))

(define start-time : Real 0.0)
(define stop-time  : Real 0.0)

(define timer-started? : Boolean #f)
(define timer-stopped?  : Boolean #f)

(define (timer-start)
  (set! timer-started? #t)
  (set! start-time (current-inexact-milliseconds))
  '())

(define (timer-stop)
  (set! stop-time  (current-inexact-milliseconds))
  (set! timer-stopped? #t)
  '())

(define (timer-report)
  (when (not timer-started?)
    (raise (exn:schml:type:dynamic
            "timer not started"
            (current-continuation-marks))))
  (when (not timer-stopped?)
    (raise (exn:schml:type:dynamic
            "timer not stopped"
            (current-continuation-marks))))
  #;(printf "time (sec): ~a\n" (- stop-time start-time))
  '())

;; The lazy-d parts
(define c-depth (box 0))

(: apply-cast-ld Cast-Type)
(define (apply-cast-ld v1 t1 t2 l1)
  (inc c-depth)
  (logging apply-cast-ld (All) "~a > ~v ~v ~v ~v" (get c-depth) v1 t1 t2 l1)
  (define res
    (if (shallow-consistent? t1 t2)
        (cond
          [(Dyn? t1)
           (match v1
             [(CL-Dyn l2 v2 t3 t1) (apply-cast-ld v2 t3 t2 l1)]
             [o (error 'cast-lang-interp "Unexpected value in apply-cast-ld match ~a" o)])]
          [(and (GRef? t1) (GRef? t2))
           (if (gref? v1)
               (CL-GProxy v1 (GRef-arg t1) (GRef-arg t2) l1)
               (error 'cast-lang-interp "language invarient broken"))]
          [(and (GVect? t1) (GVect? t2))
           (CL-GVectProxy (cast v1 CL-GVect) (GVect-arg t1) (GVect-arg t2) l1)]
          [else (mk-cast v1 t1 t2 l1)])
        (raise (exn:schml:type:dynamic l1 (current-continuation-marks)))))
  (logging apply-cast-res (All) "~a > ~v" (get c-depth) res)
  (dec c-depth)
  res)

(define-type apply-type (-> CL-Value CL-Value* CL-Value))

(: apply-lazy (-> Cast-Type apply-type))
(define (apply-lazy cast)
  (: cast/lbl (-> String (-> CL-Value Schml-Type Schml-Type CL-Value)))
  (define (cast/lbl l)
    (lambda ([v : CL-Value] [t1 : Schml-Type] [t2 : Schml-Type]) : CL-Value
      (cast v t1 t2 l)))
  (: recur apply-type)
  (define (recur rator rands)
    (match rator
      [(CL-Dyn lbl val (Fn ar1 t1* t2) (Fn ar2 t3* t4))
       (let* ((rands^ (if (= ar1 ar2)
                          (map (cast/lbl lbl) rands t3* t1*)
                          (raise (exn:schml:type:dynamic lbl (current-continuation-marks)))))
              (result (recur val rands^)))
         (cast result t2 t4 lbl))]
      [otherwise (if (CL-Proc? rator)
                     ((CL-Proc-value rator) rands)
                     (error 'interp "Unexpected value in apply-lazy match ~a" rator))]))
  recur)

(: observe-lazy (-> (-> CL-Value) Test-Value))
(define (observe-lazy thunk)
  (with-handlers ([exn:schml:type:dynamic?
		   (lambda ([e : exn:schml:type:dynamic])
                     (blame #f (exn-message e)))])
    (let ([v (thunk)])
     (cond
      [(integer? v) (int v)]
      [(boolean? v) (bool v)]
      [(null? v)    (unit)]
      [(gref? v)    (gbox)]
      [(vector? v) (gvect)]
      [(CL-GVectProxy? v) (gvect)]
      [(CL-Proc? v) (function)]
      [(CL-Dyn? v) (if (Fn? (CL-Dyn-type2 v)) (function) (dyn))]
      [else (error "returned unexpected value")]))))
