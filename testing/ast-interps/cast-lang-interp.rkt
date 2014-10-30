#lang racket

(require schml/framework/build-compiler
         schml/framework/helpers
         schml/framework/errors
	 schml/testing/values
	 schml/compiler/language)

(provide cast-lang-interp)

(struct cfi-dyn (labal values type1 type2))

(define-syntax-rule (env-lookup e u)
  (let ((ref (hash-ref e u (lambda () (error 'cfi "Unbound var ~a" u)))))
    (let ([val ((unbox ref))])
      (set-box! ref (lambda () val))
      val)))

(define (env-extend* env u* v*)
  (foldr (lambda (uid val env)
	   (hash-set env uid (box (lambda () val))))
	 env u* v*))

(define (env-extend-rec* env uid* rhs*)
  (letrec ([rec-env (foldr (lambda (uid rhs env)
			     (hash-set env uid (box (lambda () 
						      (eval rhs rec-env)))))
			   env uid* rhs*)])
    rec-env))

(define-syntax-rule (empty-env)
  (hash))


(define (mk-cast l e t g)
  (if (equal? t g) 
      e 
      (cfi-dyn l e t g)))


(define (cast-lang-interp prgm comp-config)
  (let ([eval (interp-expr apply-cast-ld (apply-lazy apply-cast-ld))] 
	[observe observe-lazy])
    (match-let ([(Prog _ exp) prgm])
      (observe eval exp (empty-env)))))

(define (interp-expr cast apply)
  (define (recur exp env)
    (define (recur/env e) (recur e env))
    (match exp
	[(Lambda (list (Fml id* _) ...) _  body)
	 (lambda (arg*) (recur body (env-extend* env id* arg*)))]
	[(Letrec (list (Bnd i* _ r*) ...) body) 
	 (recur body (env-extend-rec* env i* r*))]
	[(Let (list (Bnd i* _ (app recur/env v*)) ...) body) 
	 (recur body (env-extend* env i* v*))]
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

(define (delta p v*)
  (define-syntax app
    (syntax-rules (IxI->I IxI->B)
      [(_ IxI->I p) (! p integer? 2 (car v*) (cadr v*))]
      [(_ IxI->B p) (! p integer? 2 (car v*) (cadr v*))]))
  (define-syntax (! stx)
    (syntax-case stx ()
      [(_ p from? ary e ...)
       (with-syntax ([(tmp ...) (generate-temporaries #'(e ...))])
	 #'(let ((tmp e) ...)
	     (if (and (= (length v*) ary) (from? tmp) ...)
		 (p tmp ...) 
		 (error 'cfi-delta "type error ~a" `(p ,e ...)))))]))
  (case p
    [(+) (app IxI->I +)]
    [(-) (app IxI->I -)]
    [(*) (app IxI->I *)]
    [(=) (app IxI->B =)]
    [(<) (app IxI->B <)]
    [(>) (app IxI->B >)]
    [(>=) (app IxI->B >=)]
    [(<=) (app IxI->B <=)]
    [else (error 'delta "~a" p)]))

;; The lazy-d parts
(define (apply-cast-ld l1 v1 t1 t2)
  (if (shallow-consistent? t1 t2)
      (if (Dyn? t1)
	  (match v1
	    [(cfi-dyn l2 v2 t3 t1)
	     (apply-cast-ld l1 v2 t3 t2)]
	    [o (error 'interp "Unexpected value in apply-cast-ld match ~a" o)])
	  (mk-cast l1 v1 t1 t2))
      (raise (exn:schml:type:dynamic l1 (current-continuation-marks)))))

;;(: apply-lazy apply-ctr-type)
(define (apply-lazy cast)

  (define (cast/lbl l) (lambda (v t1 t2) (cast l v t1 t2)))

  (define (recur rator rands)
    (match rator
     [(cfi-dyn lbl val (Fn ar1 t1* t2) (Fn ar2 t3* t4))
      (let* ((rands^ (if (= ar1 ar2)
			 (map (cast/lbl lbl) rands t3* t1*)
			 (raise (exn:schml:type:dynamic lbl 
				  (current-continuation-marks)))))
	     (result (recur val rands^)))
	(cast lbl result t2 t4))]
     [(? procedure? proc) (proc rands)]
     [else (error 'interp "Unexpected value in apply-lazy match ~a" rator)]))
  recur)

(define (observe-lazy interp exp env)
  (with-handlers ([exn:schml:type:dynamic? 
		   (lambda (e) (blame #f (exn-message e)))])
    (let ([v (interp exp env)])
     (cond 
      [(integer? v) (integ v)]
      [(boolean? v) (boole v)]
      [(procedure? v) (function)]
      [else (if (Fn? (cfi-dyn-type2 v)) (function) (dynamic))]))))


