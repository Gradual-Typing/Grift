#lang typed/racket

(require schml/compiler/helpers
         schml/compiler/errors
	 schml/testing/values
	 schml/compiler/language)

(provide cast-lang-interp)

(define-type CL-Value (U Boolean Integer CL-Proc CL-Dyn))
(define-type CL-Value* (Listof CL-Value))

;; This is a common idiom because typed-racket has a glitch concerning functions and
;; Unions
(struct CL-Proc ([value : (-> CL-Value* CL-Value)]))

(struct CL-Dyn ([label : String]
                 [value : CL-Value]
                 [type1 : Schml-Type]
                 [type2 : Schml-Type]))

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

(define-type cast-type (-> String CL-Value Schml-Type Schml-Type CL-Value))
(: mk-cast cast-type)
(define (mk-cast l e t g)
  (if (equal? t g) 
      e 
      (CL-Dyn l e t g)))

(: cast-lang-interp (-> Cast0-Lang Config Test-Value))
(define (cast-lang-interp prgm comp-config)
  (let ([eval (interp-expr apply-cast-ld (apply-lazy apply-cast-ld))] 
	[observe observe-lazy])
    (match-let ([(Prog _ exp) prgm])
      (observe (lambda (): CL-Value (eval exp (empty-env)))))))

(define-type eval-expr-type (-> C0-Expr (Env CL-Value) CL-Value))
(: interp-expr (-> cast-type apply-type eval-expr-type))
(define (interp-expr cast apply)
  (: recur eval-expr-type)
  (define (recur exp env)
    (: recur/env (-> C0-Expr CL-Value))
    (define (recur/env e) (recur e env))
    (define (map-curry-recur [exp* : C0-Expr*])
      (define (curry-recur [exp : C0-Expr]): (-> (Env CL-Value) CL-Value)
        (lambda ([env : (Env CL-Value)]): CL-Value
                (recur exp env)))
      (map curry-recur exp*))
    (when (trace? 'Vomit)
      (logf "cl-expr:\n~v\n\n" exp))
    (match exp
      [(Lambda fml*  _  body)
       (let ([id* : Uid* (map (inst Fml-identifier Uid Schml-Type) fml*)])
         (CL-Proc (lambda ([arg* : CL-Value*]) (recur body (env-extend* env id* arg*)))))]
      [(Letrec bnd* body) 
       (let ([id*  (map (inst Bnd-identifier Uid Schml-Type C0-Expr) bnd*)]
             [rhs* (map (inst Bnd-expression Uid Schml-Type C0-Expr) bnd*)])
         (recur body (env-extend-rec* env id* (map-curry-recur rhs*))))]
      [(Let bnd* body)
       (let ([id*  (map (inst Bnd-identifier Uid Schml-Type C0-Expr) bnd*)]
             [rhs* (map (lambda ([b : C0-Bnd])
                          (recur/env (Bnd-expression b)))
                        bnd*)])
         (recur body (env-extend* env id* rhs*)))]
      [(If (app recur/env tst) csq alt)
       (if tst (recur/env csq) (recur/env alt))]
      [(App e e*) (apply (recur/env e) (map recur/env e*))]
      [(Op p e*) (delta p (map recur/env e*))]
      [(Cast (app recur/env val) t-exp t-cast label)
       (cast label val t-exp t-cast)]
      [(Var id) (env-lookup env id)]
      [(Quote k) k]
      [e (error 'interp "Umatched expression ~a" e)]))
  recur)

(: delta (-> Symbol CL-Value* CL-Value))
(define (delta p v*)
  (when (trace? 'Vomit)
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
    [else (error 'delta "~a" p)]))

(define-syntax tc
  (syntax-rules (IxI->I IxI->B)
    [(_ IxI->I p v) (tc-help p v (integer? (car v)) (integer? (cadr v)))]
    [(_ IxI->B p v) (tc-help p v (integer? (car v)) (integer? (cadr v)))]))

(define-syntax (tc-help stx)
  (syntax-case stx ()
    [(_ p v (? e) ...)
     (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
       #'(let ((tmp e) ...)
           (if (and (? tmp) ...)
               (p tmp ...) 
               (error 'cfi-delta "type error ~a" `(p ,e ...)))))]))
 

;; The lazy-d parts
(: apply-cast-ld cast-type)
(define (apply-cast-ld l1 v1 t1 t2)
  (if (shallow-consistent? t1 t2)
      (if (Dyn? t1)
	  (match v1
	    [(CL-Dyn l2 v2 t3 t1)
	     (apply-cast-ld l1 v2 t3 t2)]
	    [o (error 'cast-lang-interp "Unexpected value in apply-cast-ld match ~a" o)])
	  (mk-cast l1 v1 t1 t2))
      (raise (exn:schml:type:dynamic l1 (current-continuation-marks)))))

(define-type apply-type (-> CL-Value CL-Value* CL-Value))
(: apply-lazy (-> cast-type apply-type))
(define (apply-lazy cast)
  (: cast/lbl (-> String (-> CL-Value Schml-Type Schml-Type CL-Value)))
  (define (cast/lbl l)
    (lambda ([v : CL-Value] [t1 : Schml-Type] [t2 : Schml-Type]) : CL-Value
      (cast l v t1 t2)))
  (: recur apply-type)
  (define (recur rator rands)
    (match rator
      [(CL-Dyn lbl val (Fn ar1 t1* t2) (Fn ar2 t3* t4))
       (let* ((rands^ (if (= ar1 ar2)
                          (map (cast/lbl lbl) rands t3* t1*)
                          (raise (exn:schml:type:dynamic lbl (current-continuation-marks)))))
              (result (recur val rands^)))
         (cast lbl result t2 t4))]
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
      [(integer? v) (integ v)]
      [(boolean? v) (boole v)]
      [(CL-Proc? v) (function)]
      [else (if (Fn? (CL-Dyn-type2 v)) (function) (dynamic))]))))


